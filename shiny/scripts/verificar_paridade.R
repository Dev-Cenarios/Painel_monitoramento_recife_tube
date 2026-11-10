# scripts/verificar_paridade.R
# -----------------------------------------------------------------------------
# Prova que a memoizacao de mod_charts nao mudou nenhum numero.
#
# Roda os mesmos leitores na versao original (shiny/_backup/mod_charts.R.bak) e
# na atual, e compara os resultados com identical(). Performance que muda
# resultado nao e otimizacao, e bug.
#
# Uso: Rscript scripts/verificar_paridade.R original|atual <arquivo_saida.rds>
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(rlang)
  library(sf); library(leaflet); library(echarts4r)
  library(shiny); library(bslib); library(htmltools); library(htmlwidgets); library(later)
})

args <- commandArgs(trailingOnly = TRUE)
VARIANTE <- args[[1]]
SAIDA    <- args[[2]]

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
})
SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
CORE_R    <- file.path(SHINY_DIR, "core", "R")

charts_src <- if (identical(VARIANTE, "original")) {
  tmp <- file.path(tempdir(), "mod_charts_orig.R")
  file.copy(file.path(SHINY_DIR, "_backup", "mod_charts.R.bak"), tmp, overwrite = TRUE)
  tmp
} else file.path(CORE_R, "modules", "mod_charts.R")

source(file.path(CORE_R, "utils.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "paths.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "config.R"), local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "modules", "mod_data.R"), local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "modules", "mod_map.R"),  local = globalenv(), encoding = "UTF-8")
source(charts_src, local = globalenv(), encoding = "UTF-8")

cfg <- core_get_config(app_dir = file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE"),
                       doenca = "TUBERCULOSE", year_min = 2010L, year_max = 2023L)

det <- .read_tb_recife_detail(cfg)
bairros <- names(sort(table(det$bairro_key), decreasing = TRUE))[1:12]
anos <- c(2015L, 2019L, 2021L, 2023L)

res <- list(
  detail_dim   = dim(det),
  detail_ordem = utils::head(det$bairro_key, 200),
  detail_anos  = table(det$ano, useNA = "ifany")
)

for (b in bairros) {
  for (y in anos) {
    tag <- paste0(b, "|", y)
    res[[paste0("rows|",  tag)]] <- .tb_recife_local_rows(cfg, b, y)
    res[[paste0("pyr|",   tag)]] <- .tb_recife_pyr_evt(cfg, b, y)
    res[[paste0("comp|",  tag)]] <- .tb_recife_comp_df(cfg, b, y)
  }
  res[[paste0("ano|",  b)]] <- .tb_recife_year_series(cfg, b)
  res[[paste0("mes|",  b)]] <- .tb_recife_month_series(cfg, b, 2020:2023)
}
# Tambem sem bairro selecionado (Recife inteiro) e sem ano.
res[["rows|TODOS"]] <- .tb_recife_local_rows(cfg, NULL, NULL)
res[["pyr|TODOS"]]  <- .tb_recife_pyr_evt(cfg, NULL, 2023L)

saveRDS(res, SAIDA)
cat(VARIANTE, ": gravados", length(res), "resultados em", SAIDA, "\n")
