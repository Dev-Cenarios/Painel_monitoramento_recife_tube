# etl/validar.R
# -----------------------------------------------------------------------------
# Compara uma saida do ETL com a pasta de parquets em uso pelo painel.
# Nao imprime nenhum dado individual: so contagens e diferencas de agregados.
#
# Uso (da raiz do projeto):
#   Rscript etl/validar.R [pasta_nova] [pasta_atual] [ano_max]
#   Rscript etl/validar.R data/parquet_novo data/parquet 2023
#
# Saida: uma linha por dataset (OK / DIFERENTE) e, para os diferentes, as
# colunas e o tamanho da diferenca. Codigo de saida 1 se algo divergir.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(arrow); library(dplyr) })

args    <- commandArgs(trailingOnly = TRUE)
NOVA    <- if (length(args) >= 1) args[[1]] else "data/parquet_novo"
ATUAL   <- if (length(args) >= 2) args[[2]] else "data/parquet"
ANO_MAX <- if (length(args) >= 3) as.integer(args[[3]]) else 2023L

ler <- function(raiz, sub) {
  fs <- list.files(file.path(raiz, sub), pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
  if (!length(fs)) return(NULL)
  bind_rows(lapply(fs, function(f) {
    df <- as.data.frame(read_parquet(f))
    rel <- substring(f, nchar(file.path(raiz, sub)) + 2)
    kv <- regmatches(rel, gregexpr("[A-Za-z_]+=[^/]+", rel))[[1]]
    for (k in kv) { a <- strsplit(k, "=")[[1]]; if (!a[1] %in% names(df)) df[[a[1]]] <- a[2] }
    for (n in names(df)) {
      if (is.factor(df[[n]])) df[[n]] <- as.character(df[[n]])
      if (inherits(df[[n]], "integer64")) df[[n]] <- as.numeric(df[[n]])
      if (is.integer(df[[n]])) df[[n]] <- as.numeric(df[[n]])
    }
    if ("ano" %in% names(df)) df$ano <- as.numeric(as.character(df$ano))
    df
  }))
}

falhas <- 0L
relata <- function(nome, ok, detalhe = NULL) {
  cat(sprintf("%-38s %s\n", nome, if (ok) "OK" else "DIFERENTE"))
  if (!ok) { falhas <<- falhas + 1L; if (length(detalhe)) cat(paste0("    ", detalhe, "\n"), sep = "") }
}

# Compara dois data.frames por chave; numericos com tolerancia relativa.
compara <- function(nome, sub, chaves, colunas = NULL, filtro = NULL) {
  a <- ler(ATUAL, sub); b <- ler(NOVA, sub)
  if (is.null(b)) return(relata(nome, FALSE, "dataset ausente na saida nova"))
  if (!is.null(filtro)) { a <- filtro(a); b <- filtro(b) }
  colunas <- colunas %||% setdiff(intersect(names(a), names(b)), chaves)
  faltam <- setdiff(setdiff(names(a), c(chaves, "doenca", "nivel", "tipo")), names(b))
  ka <- do.call(paste, c(a[chaves], sep = "|")); kb <- do.call(paste, c(b[chaves], sep = "|"))
  det <- character(0)
  if (length(faltam)) det <- c(det, paste("colunas ausentes na nova:", paste(faltam, collapse = ", ")))
  if (anyDuplicated(ka)) det <- c(det, sprintf("chave duplicada na atual (%d)", sum(duplicated(ka))))
  so_a <- setdiff(ka, kb); so_b <- setdiff(kb, ka)
  # Sem exemplos: a chave pode conter texto livre do SINAN.
  if (length(so_a)) det <- c(det, sprintf("%d linhas so na atual", length(so_a)))
  if (length(so_b)) det <- c(det, sprintf("%d linhas so na nova", length(so_b)))
  m <- match(ka, kb); ok <- !is.na(m)
  for (cn in colunas) {
    x <- a[[cn]][ok]; y <- b[[cn]][m[ok]]
    if (is.list(x) || is.list(y)) {
      x <- vapply(x, function(z) paste(sort(z), collapse = ","), ""); y <- vapply(y, function(z) paste(sort(z), collapse = ","), "")
    }
    if (is.numeric(x) && is.numeric(y)) {
      bad <- !((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) <= 1e-6 * pmax(1, abs(x))))
    } else {
      x <- as.character(x); y <- as.character(y)
      bad <- !((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y))
    }
    if (any(bad)) {
      ex <- if (is.numeric(x)) sprintf(" (ex. %s: %s -> %s)", ka[ok][bad][1], format(x[bad][1]), format(y[bad][1])) else ""
      det <- c(det, sprintf("%-22s %5d de %d linhas diferem%s", cn, sum(bad), sum(ok), ex))
    }
  }
  relata(nome, length(det) == 0, det)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
ate <- function(df) df[df$ano <= ANO_MAX, , drop = FALSE]

cat(sprintf("Validando %s contra %s (anos ate %d)\n\n", NOVA, ATUAL, ANO_MAX))

# Microdado: compara por contagem e por assinatura das linhas, sem imprimir conteudo.
local({
  ler_sinan <- function(r) bind_rows(lapply(list.files(file.path(r, "sinan", "TUBERCULOSE"), full.names = TRUE),
                                             function(f) as.data.frame(read_parquet(f))))
  a <- ler_sinan(ATUAL); b <- ler_sinan(NOVA)
  a <- a[as.integer(a$nu_ano) <= ANO_MAX, ]
  det <- character(0)
  if (!identical(sort(names(a)), sort(names(b)))) det <- c(det, "conjunto de colunas diferente")
  na <- table(a$nu_ano); nb <- table(b$nu_ano)
  if (!identical(as.integer(na), as.integer(nb[names(na)]))) det <- c(det, "contagem por ano diferente")
  cols <- intersect(names(a), names(b))
  sa <- sort(do.call(paste, c(a[cols], sep = "\x1f"))); sb <- sort(do.call(paste, c(b[cols], sep = "\x1f")))
  if (length(sa) == length(sb) && !identical(sa, sb)) det <- c(det, sprintf("%d linhas com conteudo diferente", sum(sa != sb)))
  relata(sprintf("sinan/ (%d casos)", nrow(a)), length(det) == 0, det)
})

compara("incidence MUN", "incidence/doenca=TUBERCULOSE/nivel=MUN", c("ano"), filtro = function(d) ate(d[d$cod_mun7 == "2611606", ]))
compara("incidence UF",  "incidence/doenca=TUBERCULOSE/nivel=UF",  c("ano"), filtro = function(d) ate(d[d$uf == "PE", ]))
compara("incidence BR",  "incidence/doenca=TUBERCULOSE/nivel=BR",  c("ano"), filtro = ate)
compara("incidence BAIRRO (sem obitos)", "incidence/doenca=TUBERCULOSE/nivel=BAIRRO", c("ano", "bairro_key"),
        colunas = c("bairro_nome", "casos_total", "casos_M", "casos_F", "casos_cura", "encerramento_total", "abandono",
                    "interrupcao_trat_pct", "hiv_tested", "hiv_pos", "hiv_pos_pct", "pop_total", "incid_100k_total"), filtro = ate)
compara("incidence BAIRRO (obitos)", "incidence/doenca=TUBERCULOSE/nivel=BAIRRO", c("ano", "bairro_key"),
        colunas = c("casos_obitos", "mortalidade_100k_total", "letalidade_pct"), filtro = ate)
compara("_cache_ts", "_cache_ts", c("nivel", "ano", "mes"), filtro = ate)
compara("obitos", "obitos/doenca=TUBERCULOSE", c("nivel", "ano"), filtro = ate)
compara("piramides", "piramides", c("nivel", "tipo", "ano", "sexo", "faixa_ord"), filtro = ate)
compara("sinan_landing", "sinan_landing", c("nivel", "ano", "variavel", "sexo", "valor"), filtro = ate)
compara("sinan_dict", "sinan_dict", c("variavel", "valor"))
compara("dim_geo", "dim_geo/municipios_centroids", c("uf", "cod_mun7"))
compara("meta (dados_ate)", "meta", c("doenca"), colunas = "dados_ate")

cat(sprintf("\n%s\n", if (falhas == 0) "Tudo igual." else sprintf("%d dataset(s) com diferenca.", falhas)))
quit(status = if (falhas == 0) 0L else 1L)
