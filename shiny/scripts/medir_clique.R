# scripts/medir_clique.R
# -----------------------------------------------------------------------------
# Mede o que o usuario sente: abertura do painel e clique num bairro.
#
# O medir_performance.R cobre a camada mod_data. Mas os 8-12 s de abertura e os
# 6 s por clique nao estao la -- estao no mapa (mod_map) e nos graficos
# (mod_charts), que tem leitores proprios, separados do mod_data.
#
# Uso: Rscript scripts/medir_clique.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(rlang)
  library(sf); library(leaflet); library(echarts4r)
  library(shiny); library(bslib); library(htmltools); library(htmlwidgets); library(later)
})

SCRIPT_DIR <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
}, error = function(e) getwd())

SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
CORE_R    <- file.path(SHINY_DIR, "core", "R")

source(file.path(CORE_R, "utils.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "paths.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "config.R"), local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "modules", "mod_data.R"),   local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "modules", "mod_map.R"),    local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "modules", "mod_charts.R"), local = globalenv(), encoding = "UTF-8")

cfg <- core_get_config(app_dir = file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE"),
                       doenca = "TUBERCULOSE", year_min = 2010L, year_max = 2023L)
ANO <- 2023L

ms <- function(expr) {
  t0 <- as.numeric(Sys.time())
  ok <- TRUE; erro <- NULL
  val <- tryCatch(force(expr), error = function(e) { ok <<- FALSE; erro <<- conditionMessage(e); NULL })
  list(ms = (as.numeric(Sys.time()) - t0) * 1000, ok = ok, erro = erro, val = val)
}

fmt <- function(x) if (is.null(x) || is.na(x)) "    --  " else formatC(x, format = "f", digits = 1, width = 8)

linha <- function(rotulo, r, extra = "") {
  cat(sprintf("| %-40s | %s | %s\n", rotulo, fmt(r$ms), extra))
  if (!is.null(r$erro)) cat("|   erro:", substr(r$erro, 1, 70), "\n")
}

cat("\n=== ABERTURA (primeira vez no processo: cache frio) ===\n")
cat("| operacao                                 |      ms  |\n")
cat("|------------------------------------------|----------|\n")

r1 <- ms(.read_recife_shape_layers(cfg))
linha("mapa: shapefile bairros (st_read+valid+simplify)", r1,
      if (isTRUE(r1$ok) && !is.null(r1$val$bairros)) sprintf("%d poligonos", nrow(r1$val$bairros)) else "")

r2 <- ms(.read_tb_recife_points(cfg))
linha("mapa: pontos de casos (parquet sinan)", r2,
      if (isTRUE(r2$ok) && !is.null(r2$val)) sprintf("%d pontos", nrow(r2$val)) else "")

r3 <- ms(.charts_read_tb_recife_raw(cfg))
linha("charts: microdado bruto (parquet sinan)", r3,
      if (isTRUE(r3$ok) && is.data.frame(r3$val)) sprintf("%d linhas", nrow(r3$val)) else "")

r4 <- ms(.read_tb_recife_detail(cfg))
linha("charts: .read_tb_recife_detail (1a vez)", r4,
      if (isTRUE(r4$ok) && is.data.frame(r4$val)) sprintf("%d linhas", nrow(r4$val)) else "")

cat("\n=== SEGUNDA VEZ (mostra o que TEM cache e o que NAO tem) ===\n")
cat("| operacao                                 |      ms  |\n")
cat("|------------------------------------------|----------|\n")
linha("mapa: shapefile bairros", ms(.read_recife_shape_layers(cfg)), "<- cacheado")
linha("charts: microdado bruto", ms(.charts_read_tb_recife_raw(cfg)), "<- cacheado")
linha("charts: .read_tb_recife_detail", ms(.read_tb_recife_detail(cfg)), "<- SEM CACHE?")

cat("\n=== CLIQUE NUM BAIRRO ===\n")
det <- .read_tb_recife_detail(cfg)
BK <- if (is.data.frame(det) && nrow(det) > 0) names(sort(table(det$bairro_key), decreasing = TRUE))[[1]] else ""
cat("bairro escolhido (o de maior volume):", BK, "\n")
cat("| operacao                                 |      ms  |\n")
cat("|------------------------------------------|----------|\n")

linha("1. piramide etaria", ms(.tb_recife_pyr_evt(cfg, BK, ANO)))
linha("2. composicao por variavel", ms(.tb_recife_comp_df(cfg, BK, ANO)))
linha("3. serie anual", ms(.tb_recife_year_series(cfg, BK)))
linha("4. serie mensal", ms(.tb_recife_month_series(cfg, BK)))

total <- ms({
  .tb_recife_pyr_evt(cfg, BK, ANO); .tb_recife_comp_df(cfg, BK, ANO)
  .tb_recife_year_series(cfg, BK);  .tb_recife_month_series(cfg, BK)
})
cat("|------------------------------------------|----------|\n")
linha("TOTAL do clique (os 4 juntos)", total)

cat("\n=== ATRIBUICAO: onde o tempo do clique se vai ===\n")
r_det <- ms(.read_tb_recife_detail(cfg))
r_loc <- ms(.tb_recife_local_rows(cfg, BK, ANO))
cat(sprintf("  .read_tb_recife_detail sozinho ......... %s ms\n", fmt(r_det$ms)))
cat(sprintf("  .tb_recife_local_rows (detail+filtro) .. %s ms\n", fmt(r_loc$ms)))
cat(sprintf("  chamadas de local_rows por clique ...... 4\n"))
cat(sprintf("  => custo so de rederivar o detail ...... %s ms\n", fmt(r_det$ms * 4)))
cat("\n")
