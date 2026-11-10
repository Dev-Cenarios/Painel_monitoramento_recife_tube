# scripts/verificar_paridade_dados.R
# -----------------------------------------------------------------------------
# Prova que a memoizacao dos leitores (passos 2 e 3) nao mudou nenhum resultado.
#
# O risco especifico do passo 3 e a chave de argumentos: se duas chamadas
# diferentes gerassem a mesma chave, o cache devolveria o dado errado -- em
# silencio. Este script exercita combinacoes que DEVEM diferir entre si.
#
# Uso: Rscript scripts/verificar_paridade_dados.R original|atual saida.rds
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(arrow); library(dplyr); library(rlang) })

args <- commandArgs(trailingOnly = TRUE)
VARIANTE <- args[[1]]; SAIDA <- args[[2]]

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
})
SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
CORE_R    <- file.path(SHINY_DIR, "core", "R")

fonte_dados <- if (identical(VARIANTE, "original")) {
  tmp <- file.path(tempdir(), "mod_data_orig.R")
  file.copy(file.path(SHINY_DIR, "_backup", "mod_data.R.bak"), tmp, overwrite = TRUE)
  tmp
} else file.path(CORE_R, "modules", "mod_data.R")

source(file.path(CORE_R, "utils.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "paths.R"),  local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "config.R"), local = globalenv(), encoding = "UTF-8")
source(fonte_dados, local = globalenv(), encoding = "UTF-8")

cfg <- core_get_config(app_dir = file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE"),
                       doenca = "TUBERCULOSE", year_min = 2010L, year_max = 2023L)
api <- mod_data_init(cfg, list(DOENCA = "TUBERCULOSE"))

MUN <- cfg$RECIFE_MUN7; UF <- cfg$RECIFE_UF
ANOS <- c(2013L, 2017L, 2020L, 2023L)

res <- list()
pega <- function(nome, expr) res[[nome]] <<- tryCatch(expr, error = function(e) paste("ERRO:", conditionMessage(e)))

for (y in ANOS) {
  pega(paste0("mun|", y),   api$get_mun_metrics(ano = y, uf = UF, mun = MUN))
  pega(paste0("uf|", y),    api$get_uf_metrics(ano = y, uf = UF))
  pega(paste0("pyr|", y),   api$get_piramide(ano = y, level = "MUN", uf = UF, mun = MUN))
  pega(paste0("comp|", y),  api$get_composicao(ano = y, level = "MUN", uf = UF, mun = MUN))
  pega(paste0("scope|", y), api$get_scope(ano = y, level = "MUN", uf = UF, mun = MUN))
  # mesmo ano, kinds diferentes: chaves TEM de divergir
  for (k in c("cases", "incid", "obitos")) {
    pega(paste0("rank|", y, "|", k), api$get_ranking(ano = y, level = "MUN", uf = UF, kind = k))
  }
  # mesmo ano, top_n diferentes
  pega(paste0("rank5|", y),  api$get_ranking(ano = y, level = "MUN", uf = UF, kind = "cases", top_n = 5))
  pega(paste0("rank30|", y), api$get_ranking(ano = y, level = "MUN", uf = UF, kind = "cases", top_n = 30))
}

# niveis diferentes com os mesmos demais argumentos
for (lv in c("BR", "UF", "MUN")) {
  pega(paste0("ts|", lv), api$get_time_series(level = lv, uf = UF, mun = MUN))
  pega(paste0("years|", lv), api$get_years_available(level = lv))
}
pega("vars", api$get_variaveis_interesse(ano = 2023L, level = "MUN", uf = UF, mun = MUN))
pega("compvars", api$get_composicao_vars_available(ano = 2023L, level = "MUN", uf = UF, mun = MUN))
pega("cent", api$get_mun_centroids(uf = UF))
pega("choro", api$get_mun_choropleth(ano = 2023L, uf = UF))

# repetir tudo: a 2a passada vem do cache e tem de bater com a 1a
for (y in ANOS) {
  a <- res[[paste0("mun|", y)]]
  b <- api$get_mun_metrics(ano = y, uf = UF, mun = MUN)
  res[[paste0("recheck_mun|", y)]] <- identical(a, b)
}

saveRDS(res, SAIDA)
cat(VARIANTE, ":", length(res), "resultados ->", SAIDA, "\n")
