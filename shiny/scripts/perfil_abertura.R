# scripts/perfil_abertura.R
# -----------------------------------------------------------------------------
# Perfil de amostragem (Rprof) da abertura do painel.
#
# Simula o que uma sessao faz ao abrir e mede ONDE o tempo se vai, por funcao,
# em vez de adivinhar. Roda duas vezes: processo frio (1a sessao) e quente
# (2a sessao em diante), porque os gargalos sao diferentes nos dois casos.
#
# Uso: Rscript scripts/perfil_abertura.R
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

for (f in c("utils.R", "paths.R", "config.R")) {
  source(file.path(CORE_R, f), local = globalenv(), encoding = "UTF-8")
}
for (f in c("mod_data.R", "mod_map.R", "mod_charts.R", "mod_kpis.R")) {
  source(file.path(CORE_R, "modules", f), local = globalenv(), encoding = "UTF-8")
}

APP_DIR <- file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE")
ANO <- 2023L

# O que uma abertura efetivamente pede.
abertura <- function() {
  cfg  <- core_get_config(app_dir = APP_DIR, doenca = "TUBERCULOSE", year_min = 2010L, year_max = 2023L)
  pack <- list(DOENCA = "TUBERCULOSE")
  api  <- mod_data_init(cfg, pack)

  MUN <- cfg$RECIFE_MUN7; UF <- cfg$RECIFE_UF

  # camada de dados
  try(api$get_years_available(level = "MUN"), silent = TRUE)
  try(api$get_mun_metrics(ano = ANO, uf = UF, mun = MUN), silent = TRUE)
  try(api$get_time_series(level = "MUN", uf = UF, mun = MUN), silent = TRUE)
  try(api$get_piramide(ano = ANO, level = "MUN", uf = UF, mun = MUN), silent = TRUE)
  try(api$get_ranking(ano = ANO, level = "MUN", uf = UF, kind = "cases"), silent = TRUE)
  try(api$get_composicao(ano = ANO, level = "MUN", uf = UF, mun = MUN), silent = TRUE)

  # mapa
  try(.read_recife_shape_layers(cfg), silent = TRUE)
  try(.read_tb_recife_points(cfg), silent = TRUE)
  try(.read_tb_recife_cases(cfg), silent = TRUE)

  # graficos (Recife inteiro, que e o estado inicial)
  try(.read_tb_recife_detail(cfg), silent = TRUE)
  try(.read_tb_recife_bairro_rank(cfg, ANO), silent = TRUE)
  try(.tb_recife_pyr_evt(cfg, NULL, ANO), silent = TRUE)
  try(.tb_recife_comp_df(cfg, NULL, ANO), silent = TRUE)
  try(.tb_recife_year_series(cfg, NULL), silent = TRUE)
  try(.tb_recife_month_series(cfg, NULL, 2010:ANO), silent = TRUE)

  invisible(TRUE)
}

relatar <- function(rotulo, arquivo) {
  s <- summaryRprof(arquivo)
  total <- s$sampling.time
  cat("\n########", rotulo, "-- total", formatC(total, digits = 2, format = "f"), "s\n")

  st <- s$by.self
  st <- st[st$self.time > 0, , drop = FALSE]
  st <- utils::head(st[order(-st$self.time), , drop = FALSE], 14)
  cat(sprintf("%-42s %8s %7s\n", "funcao (self)", "seg", "%"))
  for (i in seq_len(nrow(st))) {
    cat(sprintf("%-42s %8.2f %6.1f%%\n", substr(rownames(st)[i], 1, 42), st$self.time[i], st$self.pct[i]))
  }

  tt <- s$by.total
  alvos <- grep("read_tb|charts_read|shape_layers|st_read|st_make_valid|st_simplify|open_dataset|read_parquet|core_get_config|mod_data_init|recife_bairro_key|list.files|list.dirs",
                rownames(tt), value = TRUE)
  tt <- tt[alvos, , drop = FALSE]
  tt <- utils::head(tt[order(-tt$total.time), , drop = FALSE], 14)
  if (nrow(tt) > 0) {
    cat("\n", sprintf("%-42s %8s %7s", "funcao de interesse (total)", "seg", "%"), "\n", sep = "")
    for (i in seq_len(nrow(tt))) {
      cat(sprintf("%-42s %8.2f %6.1f%%\n", substr(rownames(tt)[i], 1, 42), tt$total.time[i], tt$total.pct[i]))
    }
  }
}

f1 <- tempfile(fileext = ".out"); f2 <- tempfile(fileext = ".out")

Rprof(f1, interval = 0.005, line.profiling = FALSE)
abertura()
Rprof(NULL)
relatar("1a SESSAO (processo frio)", f1)

Rprof(f2, interval = 0.005, line.profiling = FALSE)
abertura()
Rprof(NULL)
relatar("2a SESSAO (processo quente) <- o caso comum no servidor", f2)
cat("\n")
