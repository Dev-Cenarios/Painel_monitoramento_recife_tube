# scripts/perfil_ui.R
# -----------------------------------------------------------------------------
# Mede o lado da UI: custo de montar o HTML e tamanho do que trafega.
#
# O perfil_abertura.R mostrou que a camada de dados custa 0,8 s num processo
# quente, mas a tela leva ~6,5 s. A diferenca esta aqui: construcao de UI,
# serializacao dos widgets echarts e o payload que vai pro navegador.
#
# Uso: Rscript scripts/perfil_ui.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(rlang)
  library(sf); library(leaflet); library(echarts4r)
  library(shiny); library(bslib); library(htmltools); library(htmlwidgets); library(later)
})

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
})
SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
CORE_R    <- file.path(SHINY_DIR, "core", "R")

CORE_ENV <- new.env(parent = globalenv())
for (f in c("utils.R", "paths.R", "config.R")) source(file.path(CORE_R, f), local = globalenv(), encoding = "UTF-8")
for (f in c("mod_data.R", "mod_map.R", "mod_charts.R", "mod_kpis.R", "mod_state.R")) {
  source(file.path(CORE_R, "modules", f), local = globalenv(), encoding = "UTF-8")
}
source(file.path(CORE_R, "app_shell.R"), local = globalenv(), encoding = "UTF-8")

APP_DIR <- file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE")
ANO <- 2023L

cfg <- core_get_config(app_dir = APP_DIR, doenca = "TUBERCULOSE", year_min = 2010L, year_max = 2023L)
pack <- local({
  source(file.path(SHINY_DIR, "core", "diseases", "TUBERCULOSE", "disease_pack.R"), local = TRUE, encoding = "UTF-8")
  get_disease_pack()
})

ms <- function(expr) {
  t0 <- as.numeric(Sys.time())
  v <- tryCatch(force(expr), error = function(e) { cat("  erro:", conditionMessage(e), "\n"); NULL })
  list(ms = (as.numeric(Sys.time()) - t0) * 1000, val = v)
}
kb <- function(x) formatC(x / 1024, format = "f", digits = 1)

cat("\n=== 1. CONSTRUCAO DA UI (por sessao) ===\n")
r <- ms(core_build_ui(cfg, pack))
html <- tryCatch(as.character(r$val), error = function(e) "")
cat(sprintf("core_build_ui .................. %8.1f ms\n", r$ms))
cat(sprintf("HTML gerado ................... %8s KB\n", kb(sum(nchar(html, type = "bytes")))))

cat("\n=== 2. BLOCOS ESTATICOS DE CSS/JS EM app_shell.R ===\n")
for (fn in c(".core_shell_css", ".core_shell_js", ".core_pack_css")) {
  if (exists(fn, mode = "function")) {
    v <- tryCatch(as.character(do.call(fn, list(pack = pack))), error = function(e)
         tryCatch(as.character(do.call(fn, list())), error = function(e2) ""))
    cat(sprintf("%-20s ........... %8s KB (regerado a cada sessao)\n", fn, kb(sum(nchar(v, type = "bytes")))))
  }
}

cat("\n=== 3. PAYLOAD DOS WIDGETS (o que vai pelo websocket) ===\n")
cat("Cada grafico echarts vira JSON. Series longas custam banda e parse no browser.\n\n")

det <- .read_tb_recife_detail(cfg)
alvos <- list(
  "serie mensal (2010-2023)" = function() .tb_recife_month_series(cfg, NULL, 2010:ANO),
  "serie anual"              = function() .tb_recife_year_series(cfg, NULL),
  "piramide (Recife, 2023)"  = function() .tb_recife_pyr_evt(cfg, NULL, ANO),
  "composicao (Recife)"      = function() .tb_recife_comp_df(cfg, NULL, ANO),
  "ranking bairros"          = function() .read_tb_recife_bairro_rank(cfg, ANO, 15L)
)
cat(sprintf("%-28s %8s %10s %12s\n", "serie", "ms", "linhas", "JSON KB"))
for (nm in names(alvos)) {
  rr <- ms(alvos[[nm]]())
  d <- rr$val
  n <- if (is.data.frame(d)) nrow(d) else NA_integer_
  js <- tryCatch(nchar(jsonlite::toJSON(d, dataframe = "columns", na = "null"), type = "bytes"),
                 error = function(e) NA_real_)
  cat(sprintf("%-28s %8.1f %10s %12s\n", nm, rr$ms, format(n), if (is.na(js)) "--" else kb(js)))
}

cat("\n=== 4. GEOMETRIA ENVIADA AO LEAFLET ===\n")
lay <- .read_recife_shape_layers(cfg)
g <- lay$bairros
if (!is.null(g)) {
  gj <- tryCatch(sf::st_as_text(sf::st_geometry(g)), error = function(e) character(0))
  vert <- tryCatch(sum(vapply(sf::st_geometry(g), function(x) nrow(sf::st_coordinates(x)), integer(1))), error = function(e) NA)
  cat(sprintf("poligonos ..................... %8d\n", nrow(g)))
  cat(sprintf("vertices totais ............... %8s\n", format(vert)))
  cat(sprintf("WKT aproximado ................ %8s KB\n", kb(sum(nchar(gj, type = "bytes")))))
}

cat("\n=== 5. IMAGENS DO CABECALHO ===\n")
img <- file.path(cfg$ROOT, "data", "Imagens")
if (dir.exists(img)) {
  for (f in list.files(img, full.names = TRUE)) {
    cat(sprintf("%-30s %8s KB\n", basename(f), kb(file.info(f)$size)))
  }
}
cat("\n")
