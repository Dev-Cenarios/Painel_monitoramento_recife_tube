# dashs/shiny/core/R/paths.R

suppressPackageStartupMessages({
  library(arrow)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.paths_env <- new.env(parent = emptyenv())
.ds_env <- new.env(parent = emptyenv())

.norm <- function(p) {
  if (is.null(p) || !nzchar(as.character(p))) return(NULL)
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

.has_parquet <- function(path) {
  if (is.null(path) || !dir.exists(path)) return(FALSE)
  x <- tryCatch(list.files(path, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE), error = function(e) character(0))
  length(x) > 0
}

.find_root_up <- function(start, max_up = 8L) {
  cur <- .norm(start %||% getwd())
  if (is.null(cur)) return(NULL)

  for (i in seq_len(max_up + 1L)) {
    dash <- file.path(cur, "data", "parquet")
    if (dir.exists(dash)) return(cur)
    nxt <- dirname(cur)
    if (identical(nxt, cur)) break
    cur <- nxt
  }
  NULL
}

sinan_paths <- function(app_dir = NULL, root = NULL, strict = TRUE) {
  app_dir <- .norm(app_dir %||% getwd())
  root0 <- .norm(root %||% .find_root_up(app_dir, max_up = 10L) %||% .find_root_up(getwd(), max_up = 10L))
  if (is.null(root0)) {
    if (isTRUE(strict)) stop("ROOT não encontrado. Esperado: <ROOT>/data/parquet")
    return(NULL)
  }

  dash <- file.path(root0, "data", "parquet")

  out <- list(
    ROOT = root0,
    DASHBOARD_DIR = dash,

    PATH_TS = file.path(dash, "_cache_ts"),
    PATH_INCIDENCE = file.path(dash, "incidence"),
    PATH_PIRAMIDES = file.path(dash, "piramides"),
    PATH_SINAN_LANDING = file.path(dash, "sinan_landing")
  )

  for (nm in names(out)) out[[nm]] <- .norm(out[[nm]])

  out$OK <- list(
    DASHBOARD_DIR = dir.exists(out$DASHBOARD_DIR),
    TS = dir.exists(out$PATH_TS) && .has_parquet(out$PATH_TS),
    INCIDENCE = dir.exists(out$PATH_INCIDENCE) && .has_parquet(out$PATH_INCIDENCE),
    PIRAMIDES = dir.exists(out$PATH_PIRAMIDES) && .has_parquet(out$PATH_PIRAMIDES),
    SINAN_LANDING = dir.exists(out$PATH_SINAN_LANDING) && .has_parquet(out$PATH_SINAN_LANDING)
  )

  out
}

sinan_paths_cached <- function(app_dir = NULL, root = NULL, strict = TRUE) {
  key <- paste0("paths::", .norm(root %||% app_dir %||% getwd()))
  if (exists(key, envir = .paths_env, inherits = FALSE)) return(get(key, envir = .paths_env, inherits = FALSE))
  p <- sinan_paths(app_dir = app_dir, root = root, strict = strict)
  assign(key, p, envir = .paths_env)
  p
}

sinan_open_ds <- function(path, format = "parquet", partitioning = "hive") {
  path <- .norm(path)
  if (is.null(path) || !dir.exists(path)) return(NULL)
  key <- paste0("ds::", path)
  if (exists(key, envir = .ds_env, inherits = FALSE)) return(get(key, envir = .ds_env, inherits = FALSE))
  ds <- tryCatch(arrow::open_dataset(path, format = format, partitioning = partitioning), error = function(e) NULL)
  assign(key, ds, envir = .ds_env)
  ds
}

sinan_ds <- function(paths, name = c("TS","INCIDENCE","PIRAMIDES","SINAN_LANDING")) {
  name <- toupper(as.character(name[[1]]))
  p <- paths
  if (is.null(p) || !is.list(p)) return(NULL)

  if (name == "TS") return(sinan_open_ds(p$PATH_TS))
  if (name == "INCIDENCE") return(sinan_open_ds(p$PATH_INCIDENCE))
  if (name == "PIRAMIDES") return(sinan_open_ds(p$PATH_PIRAMIDES))
  if (name == "SINAN_LANDING") return(sinan_open_ds(p$PATH_SINAN_LANDING))

  NULL
}

sinan_paths_report <- function(paths) {
  p <- paths
  if (is.null(p) || !is.list(p)) return(data.frame())

  as.data.frame(list(
    ROOT = p$ROOT,
    DASHBOARD_DIR = p$DASHBOARD_DIR,
    PATH_TS = p$PATH_TS,
    PATH_INCIDENCE = p$PATH_INCIDENCE,
    PATH_PIRAMIDES = p$PATH_PIRAMIDES,
    PATH_SINAN_LANDING = p$PATH_SINAN_LANDING,
    OK_DASHBOARD_DIR = isTRUE(p$OK$DASHBOARD_DIR),
    OK_TS = isTRUE(p$OK$TS),
    OK_INCIDENCE = isTRUE(p$OK$INCIDENCE),
    OK_PIRAMIDES = isTRUE(p$OK$PIRAMIDES),
    OK_SINAN_LANDING = isTRUE(p$OK$SINAN_LANDING)
  ), stringsAsFactors = FALSE)
}
