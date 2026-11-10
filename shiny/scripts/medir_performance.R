# scripts/medir_performance.R
# -----------------------------------------------------------------------------
# Linha de base de performance da camada de dados do painel Shiny.
#
# Espelha o metodo de scripts/medir_performance.py do painel RecifeTB
# (Streamlit): mediana de 5 execucoes, primeira descartada, escopo MUN (Recife),
# ano 2023 -- para que os dois numeros sejam comparaveis.
#
# Uso:
#   Rscript scripts/medir_performance.R depois    # arvore atual
#   Rscript scripts/medir_performance.R antes     # arquivos de _backup/
#
# A variante "antes" existe porque a mudanca principal (cache por processo em
# vez de por sessao) so aparece quando se mede a SEGUNDA sessao. Medir uma
# sessao isolada nao mostra nada.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(rlang)
})

args <- commandArgs(trailingOnly = TRUE)
VARIANTE <- if (length(args) > 0) tolower(trimws(args[[1]])) else "depois"
if (!VARIANTE %in% c("antes", "depois")) {
  stop("Variante invalida: use 'antes' ou 'depois'.")
}

REPETICOES <- 5L
SESSOES    <- 5L
ANO        <- 2023L

SCRIPT_DIR <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
}, error = function(e) getwd())

SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
CORE_R    <- file.path(SHINY_DIR, "core", "R")
BACKUP    <- file.path(SHINY_DIR, "_backup")

# ---------------------------------------------------------------------------
# Selecao dos fontes conforme a variante
# ---------------------------------------------------------------------------
fonte <- function(nome) {
  # nome: "config.R" ou "mod_data.R"
  if (identical(VARIANTE, "antes")) {
    bak <- file.path(BACKUP, paste0(nome, ".bak"))
    if (!file.exists(bak)) {
      stop("Backup ausente para a variante 'antes': ", bak,
           "\nSem ele nao ha com o que comparar.")
    }
    tmp <- file.path(tempdir(), nome)
    file.copy(bak, tmp, overwrite = TRUE)
    return(tmp)
  }
  if (identical(nome, "config.R")) file.path(CORE_R, "config.R")
  else file.path(CORE_R, "modules", "mod_data.R")
}

source(file.path(CORE_R, "utils.R"), local = globalenv(), encoding = "UTF-8")
source(file.path(CORE_R, "paths.R"), local = globalenv(), encoding = "UTF-8")
source(fonte("config.R"),            local = globalenv(), encoding = "UTF-8")
source(fonte("mod_data.R"),          local = globalenv(), encoding = "UTF-8")

APP_DIR <- file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE")

# ---------------------------------------------------------------------------
# Cronometragem
# ---------------------------------------------------------------------------
# proc.time() no Windows tem granularidade de 10 ms, grossa demais para os
# leitores individuais. Sys.time() da resolucao sub-milissegundo.
ms <- function(expr) {
  t0 <- as.numeric(Sys.time())
  ok <- TRUE
  erro <- NULL
  tryCatch(force(expr), error = function(e) {
    ok <<- FALSE
    erro <<- conditionMessage(e)
  })
  list(ms = (as.numeric(Sys.time()) - t0) * 1000, ok = ok, erro = erro)
}

cronometrar <- function(fn, n = REPETICOES, aquecer = TRUE) {
  if (isTRUE(aquecer)) {
    r <- ms(fn())
    if (!isTRUE(r$ok)) return(list(mediana = NA_real_, pior = NA_real_, erro = r$erro))
  }
  tempos <- numeric(0)
  erro <- NULL
  for (i in seq_len(n)) {
    r <- ms(fn())
    if (!isTRUE(r$ok)) { erro <- r$erro; break }
    tempos <- c(tempos, r$ms)
  }
  if (length(tempos) == 0) return(list(mediana = NA_real_, pior = NA_real_, erro = erro))
  list(mediana = stats::median(tempos), pior = max(tempos), erro = erro)
}

fmt <- function(x) if (is.na(x)) "   --  " else formatC(x, format = "f", digits = 1, width = 7)

linha <- function(rotulo, r) {
  cat(sprintf("| %-34s | %s | %s |\n", rotulo, fmt(r$mediana), fmt(r$pior)))
  if (!is.null(r$erro)) cat(sprintf("|   erro: %-64s |\n", substr(r$erro, 1, 64)))
}

# ---------------------------------------------------------------------------
# 1. Startup: descoberta de configuracao
# ---------------------------------------------------------------------------
cat("\n== variante:", VARIANTE, "==\n")
cat("R", as.character(getRversion()), "| arrow", as.character(utils::packageVersion("arrow")), "\n")

caps <- tryCatch(arrow::arrow_info()$capabilities, error = function(e) NULL)
if (!is.null(caps)) {
  cat("arrow capabilities:", paste(names(which(unlist(caps))), collapse = ", "), "\n")
  capv <- unlist(caps)
  if (!("dataset" %in% names(capv)) || !isTRUE(capv[["dataset"]])) {
    cat("\n*** ATENCAO: arrow sem suporte a 'dataset'. Reinstale o arrow ***\n")
  }
}
cat("\n")

cat("| operacao                           | mediana |   pior  |\n")
cat("|------------------------------------|---------|---------|\n")

r_cfg <- cronometrar(function() {
  core_get_config(app_dir = APP_DIR, doenca = "TUBERCULOSE",
                  year_min = 2010L, year_max = 2023L)
}, aquecer = TRUE)
linha("core_get_config (por startup)", r_cfg)

cfg <- core_get_config(app_dir = APP_DIR, doenca = "TUBERCULOSE",
                       year_min = 2010L, year_max = 2023L)
pack <- list(DOENCA = "TUBERCULOSE")

MUN <- cfg$RECIFE_MUN7 %||% "2611606"
UF  <- cfg$RECIFE_UF %||% "PE"

# ---------------------------------------------------------------------------
# 2. A metrica que importa: custo de UMA SESSAO nova
# ---------------------------------------------------------------------------
# Reproduz o que o servidor faz quando um usuario abre o painel: cria a API de
# dados e pede o que a primeira renderizacao pede. Com cache por sessao, toda
# sessao paga o preco cheio; com cache por processo, so a primeira paga.
sessao <- function() {
  api <- mod_data_init(cfg, pack)
  invisible(list(
    tryCatch(api$get_years_available(level = "MUN"), error = function(e) NULL),
    tryCatch(api$get_mun_metrics(ano = ANO, uf = UF, mun = MUN), error = function(e) NULL),
    tryCatch(api$get_time_series(level = "MUN", uf = UF, mun = MUN), error = function(e) NULL),
    tryCatch(api$get_piramide(ano = ANO, level = "MUN", uf = UF, mun = MUN), error = function(e) NULL),
    tryCatch(api$get_ranking(ano = ANO, level = "MUN", uf = UF, kind = "cases"), error = function(e) NULL),
    tryCatch(api$get_composicao(ano = ANO, level = "MUN", uf = UF, mun = MUN), error = function(e) NULL)
  ))
}

cat("|------------------------------------|---------|---------|\n")
tempos_sessao <- numeric(0)
for (i in seq_len(SESSOES)) {
  r <- ms(sessao())
  tempos_sessao <- c(tempos_sessao, if (isTRUE(r$ok)) r$ms else NA_real_)
  cat(sprintf("| sessao #%-2d (abertura completa)      | %s |         |\n", i, fmt(tempos_sessao[[i]])))
}
cat(sprintf("| -> 1a sessao                       | %s |         |\n", fmt(tempos_sessao[[1]])))
cat(sprintf("| -> mediana das seguintes           | %s |         |\n",
            fmt(stats::median(tempos_sessao[-1], na.rm = TRUE))))

# ---------------------------------------------------------------------------
# 3. Leitores individuais, ja quentes (paridade com o script Python)
# ---------------------------------------------------------------------------
api <- mod_data_init(cfg, pack)

cat("|------------------------------------|---------|---------|\n")
linha("get_years_available",  cronometrar(function() api$get_years_available(level = "MUN")))
linha("get_mun_metrics",      cronometrar(function() api$get_mun_metrics(ano = ANO, uf = UF, mun = MUN)))
linha("get_time_series",      cronometrar(function() api$get_time_series(level = "MUN", uf = UF, mun = MUN)))
linha("get_piramide",         cronometrar(function() api$get_piramide(ano = ANO, level = "MUN", uf = UF, mun = MUN)))
linha("get_ranking",          cronometrar(function() api$get_ranking(ano = ANO, level = "MUN", uf = UF, kind = "cases")))
linha("get_composicao",       cronometrar(function() api$get_composicao(ano = ANO, level = "MUN", uf = UF, mun = MUN)))
cat("\n")

if (exists("mod_data_cache_info", mode = "function")) {
  cat("configuracoes em cache no processo:\n")
  print(mod_data_cache_info())
  cat("\n")
}
