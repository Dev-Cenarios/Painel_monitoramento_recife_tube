# etl/run_etl.R
# -----------------------------------------------------------------------------
# ETL do Painel de Monitoramento da Tuberculose (Recife).
#
# Le os insumos brutos de data/ e grava todos os datasets que o painel le, no
# layout de data/parquet/. Por seguranca, grava numa pasta NOVA (padrao:
# data/parquet_novo). Depois de validar (etl/validar.R), troque as pastas.
#
# Uso (da raiz do projeto):
#   Rscript etl/run_etl.R [ano_max] [pasta_saida]
#   Rscript etl/run_etl.R 2023
#   Rscript etl/run_etl.R 2024 data/parquet_2024
#
# Ordem das etapas (cada uma em etl/R/):
#   10_entradas    le SINAN, SIM, populacao SVS, populacao por bairro, etl/ref/
#   20_sinan       sinan/ (microdado) e popsvs/
#   30_agregados   incidence/, _cache_ts/, obitos/, piramides/
#   40_composicao  sinan_landing/, sinan_dict/
#   50_apoio       dim_geo/, meta/
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(arrow) })

args    <- commandArgs(trailingOnly = TRUE)
ANO_MAX <- if (length(args) >= 1) as.integer(args[[1]]) else 2023L
SAIDA   <- if (length(args) >= 2) args[[2]] else NULL

ETL_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else normalizePath("etl", winslash = "/")
})
for (f in sort(list.files(file.path(ETL_DIR, "R"), pattern = "[.]R$", full.names = TRUE))) source(f, encoding = "UTF-8")

cam <- etl_caminhos(raiz = normalizePath(file.path(ETL_DIR, ".."), winslash = "/"), saida = SAIDA)
if (dir.exists(cam$saida) && length(list.files(cam$saida, recursive = TRUE)) > 0) {
  stop("A pasta de saida ja existe e nao esta vazia: ", cam$saida,
       "\nApague-a ou passe outra pasta. O ETL nunca sobrescreve dados.", call. = FALSE)
}
if (normalizePath(cam$saida, winslash = "/", mustWork = FALSE) ==
    normalizePath(file.path(cam$raiz, "data", "parquet"), winslash = "/", mustWork = FALSE)) {
  stop("Nao grave direto em data/parquet: gere em outra pasta, valide e depois troque.", call. = FALSE)
}

t0 <- Sys.time()
msg(sprintf("ETL %s | anos %d-%d | saida: %s", DOENCA, ANO_MIN, ANO_MAX, cam$saida))
anos <- ANO_MIN:ANO_MAX

ref    <- ler_referencias(cam)
sinan  <- ler_sinan(cam, ANO_MAX)
sim    <- ler_sim_tb(cam, ANO_MAX)
pop    <- ler_pop_svs(cam, anos)
pop_b  <- ler_pop_bairro(cam)

anos_sem_caso <- setdiff(anos, unique(sinan$ano))
if (length(anos_sem_caso)) stop("Sem casos no SINAN para: ", paste(anos_sem_caso, collapse = ", "), call. = FALSE)
if (any(!anos %in% ref$br_anual$ano)) {
  msg("AVISO: etl/ref/nacional_*.csv nao tem os anos", paste(setdiff(anos, ref$br_anual$ano), collapse = ", "),
      "-> o nivel Brasil fica sem esses anos (ver README, secao ETL).")
}

dir.create(cam$saida, recursive = TRUE, showWarnings = FALSE)
grava_sinan_microdado(sinan, cam$saida)
grava_pop_svs(pop, cam$saida)

res    <- resumo_recife(sinan, sim, pop, anos)
bairro <- resumo_bairros(sinan, sim, pop_b, ref, anos)
grava_incidence(res, ref, bairro, cam$saida)
grava_series(sinan, res, ref, cam$saida)
grava_obitos(res, ref, cam$saida)
grava_piramides(sinan, pop, anos, cam$saida)

grava_composicao(sinan, ref, cam$saida)

grava_dim_geo(ref, cam$saida)
grava_meta(ANO_MAX, cam$saida)

msg(sprintf("Pronto em %.1f s. Proximo passo: Rscript etl/validar.R %s", as.numeric(difftime(Sys.time(), t0, units = "secs")), cam$saida))
