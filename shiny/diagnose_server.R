# diagnose_server.R
# Rode NO SERVIDOR LINUX, a partir da pasta do app:
#   cd /caminho/para/core/apps/TUBERCULOSE
#   Rscript ../../../diagnose_server.R
# ou:
#   Rscript /caminho/para/shiny/diagnose_server.R /caminho/para/core/apps/TUBERCULOSE
#
# Ele NÃO sobe o Shiny. Só verifica caminhos, dados e pacotes.

args <- commandArgs(trailingOnly = TRUE)
APP_DIR <- if (length(args) >= 1 && nzchar(args[[1]])) normalizePath(args[[1]], mustWork = FALSE) else getwd()
cat("======================================================\n")
cat("APP_DIR (getwd no Shiny Server):", APP_DIR, "\n")
cat("Sys.getlocale():", Sys.getlocale(), "\n")
cat("R:", R.version.string, "\n")
cat("Usuario:", Sys.info()[["user"]], "\n")
cat("======================================================\n\n")

## 1) Variaveis de ambiente esperadas -------------------------------------------
cat("--- ENV ---\n")
for (v in c("SINAN_ROOT","SINAN_PARQ_ROOT","SINAN_SUPPORT_ROOT","SINAN_IMG_ROOT")) {
  cat(sprintf("  %-20s = %s\n", v, dQuote(Sys.getenv(v, unset = "<vazio>"))))
}
cat("  (.Renviron em APP_DIR existe? ", file.exists(file.path(APP_DIR, ".Renviron")), ")\n\n")

## 2) Descoberta do ROOT (mesma logica de config.R) ----------------------------
find_root <- function(from) {
  er <- Sys.getenv("SINAN_ROOT", unset = "")
  if (nzchar(er) && dir.exists(file.path(er, "data", "parquet"))) return(normalizePath(er))
  p <- normalizePath(from, mustWork = FALSE)
  for (i in 0:15) {
    if (dir.exists(file.path(p, "data", "parquet"))) return(p)
    up <- dirname(p); if (identical(up, p)) break; p <- up
  }
  NA_character_
}
ROOT <- find_root(normalizePath(file.path(APP_DIR, "..", ".."), mustWork = FALSE))
cat("--- ROOT ---\n")
cat("  ROOT detectado:", ROOT, "\n")
if (is.na(ROOT)) {
  cat("  >>> FALHA: nenhuma pasta com data/parquet foi achada subindo 15 niveis.\n")
  cat("  >>> Isso derruba o app inteiro (core_get_config chama stop()).\n")
  cat("  >>> Solucao: deployar a arvore inteira OU definir SINAN_ROOT / SINAN_PARQ_ROOT.\n\n")
} else {
  PARQ <- Sys.getenv("SINAN_PARQ_ROOT", unset = file.path(ROOT, "data", "parquet"))
  cat("  PARQ_ROOT:", PARQ, " existe?", dir.exists(PARQ), "\n")
  npq <- length(list.files(PARQ, pattern = "\\.parquet$", recursive = TRUE))
  cat("  .parquet abaixo de PARQ_ROOT:", npq, "\n")
  inc <- file.path(PARQ, "incidence")
  cat("  incidence/ existe?", dir.exists(inc),
      " | .parquet:", length(list.files(inc, pattern="\\.parquet$", recursive=TRUE)), "\n")
  cat("  subpastas de incidence/: ",
      paste(list.dirs(inc, recursive = FALSE, full.names = FALSE), collapse = " | "), "\n")
  bairro <- file.path(inc, "doenca=TUBERCULOSE", "nivel=BAIRRO")
  bfiles <- list.files(bairro, pattern="\\.parquet$", recursive=TRUE, full.names=TRUE)
  cat("  nivel=BAIRRO existe?", dir.exists(bairro), " | .parquet:", length(bfiles), "\n")

  # tamanho / permissao / leitura real de um parquet
  amostra <- head(list.files(inc, pattern="\\.parquet$", recursive=TRUE, full.names=TRUE), 3)
  if (length(amostra)) {
    cat("\n  amostra de .parquet (tamanho / modo / uid):\n")
    fi <- file.info(amostra)
    uid <- if ("uid" %in% names(fi)) fi$uid else rep(NA, nrow(fi))
    for (i in seq_along(amostra)) {
      cat(sprintf("   %10.0f B   mode=%s   uid=%s   %s\n",
                  as.numeric(fi$size[i]), as.character(fi$mode[i]),
                  as.character(uid[i]), amostra[i]))
    }
    if (any(!is.finite(fi$size) | fi$size == 0))
      cat("   >>> ATENCAO: ha .parquet com 0 bytes (zip/transferencia truncou).\n")
    cat("\n  teste de leitura (arrow::read_parquet no 1o arquivo):\n")
    r <- tryCatch(as.data.frame(arrow::read_parquet(amostra[[1]])),
                  error = function(e) e)
    if (inherits(r, "error")) cat("   >>> ERRO:", conditionMessage(r), "\n")
    else cat("   OK - linhas:", nrow(r), " colunas:", ncol(r), "\n")

    cat("\n  teste open_dataset (o que o app realmente usa):\n")
    d <- tryCatch(arrow::open_dataset(inc), error = function(e) e)
    if (inherits(d, "error")) {
      d <- tryCatch(arrow::open_dataset(inc, partitioning = "hive"), error = function(e) e)
    }
    if (inherits(d, "error")) cat("   >>> ERRO:", conditionMessage(d), "\n")
    else {
      n <- tryCatch(as.numeric(d$num_rows), error = function(e) NA)
      if (!is.finite(n)) n <- tryCatch(nrow(dplyr::collect(dplyr::select(d, 1))), error = function(e) NA)
      cat("   OK - colunas:", paste(head(names(d), 8), collapse = ","),
          " | total de linhas:", n, "\n")
    }
  } else {
    cat("  >>> FALHA: nenhum .parquet em incidence/. O zip nao trouxe a pasta data/parquet completa?\n")
  }
  cat("\n")
}

## 3) Shapefile do mapa (fwmalharecife) ---------------------------------------
cat("--- SHAPEFILE DO MAPA ---\n")
sr <- Sys.getenv("SINAN_SUPPORT_ROOT", unset = "")
cands <- character(0)
if (nzchar(sr)) cands <- c(cands, sr)
anc <- function(p, up = 10) { o <- character(0); c <- normalizePath(p, mustWork = FALSE)
  for (i in 0:up) { o <- c(o, c); pr <- dirname(c); if (identical(pr, c)) break; c <- pr }; unique(o) }
base_for_search <- if (!is.na(ROOT)) file.path(ROOT, "data", "parquet") else APP_DIR
cands <- c(cands, file.path(anc(APP_DIR), "fwmalharecife"), file.path(anc(base_for_search), "fwmalharecife"))
hit <- unique(cands[dir.exists(cands)])
if (length(hit) == 0) {
  cat("  >>> FALHA: pasta 'fwmalharecife' nao encontrada e SINAN_SUPPORT_ROOT vazio.\n")
  cat("  >>> O mapa fica BRANCO (sem poligonos).\n")
  cat("  candidatos testados:\n"); for (x in cands) cat("     ", x, "\n")
} else {
  cat("  pasta encontrada:", hit[[1]], "\n")
  shp <- list.files(hit[[1]], pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  cat("  .shp presentes:", paste(basename(shp), collapse = " | "), "\n")
  if (requireNamespace("sf", quietly = TRUE)) {
    for (s in shp) {
      g <- tryCatch(sf::st_read(s, quiet = TRUE), error = function(e) e)
      if (inherits(g, "error")) cat("  ERRO lendo", basename(s), ":", conditionMessage(g), "\n")
      else cat("  OK", basename(s), "- feicoes:", nrow(g), "- colunas:", paste(head(names(g), 12), collapse = ","), "\n")
    }
  }
}
cat("\n")

## 4) Pacotes + arrow/parquet capability -------------------------------------
cat("--- PACOTES ---\n")
pk <- c("shiny","bslib","bsicons","leaflet","sf","arrow","dplyr","echarts4r",
        "htmlwidgets","htmltools","later","magrittr","rlang")
for (p in pk) cat(sprintf("  %-12s %s\n", p, requireNamespace(p, quietly = TRUE)))
if (requireNamespace("arrow", quietly = TRUE)) {
  cat("\n  arrow ", as.character(utils::packageVersion("arrow")), "\n")
  ok_pq <- tryCatch(arrow::arrow_with_parquet(), error = function(e) NA)
  cat("  arrow_with_parquet():", ok_pq, "  <<< tem que ser TRUE\n")
  info <- tryCatch(arrow::arrow_info(), error = function(e) NULL)
  if (!is.null(info)) print(info$capabilities)
}
cat("\n")

## 5) sf / GDAL ---------------------------------------------------------------
if (requireNamespace("sf", quietly = TRUE)) {
  cat("--- sf / GDAL ---\n")
  print(sf::sf_extSoftVersion())
}
cat("\n=== fim ===\n")
