# dashs/shiny/core/R/app_shell.R
# Shell principal do SINAN (UI + Server)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

# Cache global compartilhado entre módulos para melhorar performance
# Inicializa um ambiente persistente para armazenar dados em cache
if (!exists(".SINAN_GLOBAL_CACHE", inherits = TRUE)) {
  .SINAN_GLOBAL_CACHE <- new.env(parent = emptyenv())
  # Metadados para TTL (time-to-live) e diagnóstico
  .SINAN_CACHE_META <- new.env(parent = emptyenv())
}

# Helper para cache com TTL (em segundos)
.cache_get_or_compute <- function(key, compute_fn, ttl_secs = 600) {
  if (exists(key, envir = .SINAN_GLOBAL_CACHE, inherits = FALSE)) {
    meta <- tryCatch(get(paste0(key, "__meta"), envir = .SINAN_CACHE_META, inherits = FALSE), error = function(e) NULL)
    if (!is.null(meta) && is.numeric(meta$timestamp)) {
      elapsed <- as.numeric(Sys.time()) - meta$timestamp
      if (elapsed < ttl_secs) {
        return(get(key, envir = .SINAN_GLOBAL_CACHE, inherits = FALSE))
      }
    }
  }
  
  # Cache miss ou expirado: computar
  result <- compute_fn()
  assign(key, result, envir = .SINAN_GLOBAL_CACHE)
  assign(paste0(key, "__meta"), list(timestamp = as.numeric(Sys.time()), size = object.size(result)), envir = .SINAN_CACHE_META)
  result
}

# Helper para diagnóstico de cache
.cache_diagnostics <- function() {
  keys <- ls(.SINAN_GLOBAL_CACHE)
  total_size <- sum(sapply(keys, function(k) object.size(get(k, envir = .SINAN_GLOBAL_CACHE))))
  list(
    count = length(keys),
    size_mb = as.numeric(total_size) / 1024 / 1024,
    keys = head(keys, 20)
  )
}

.bs_alert <- function(..., color = c("danger","warning","info","success","primary","secondary"), title = NULL) {
  color <- match.arg(color)

  if ("alert" %in% getNamespaceExports("bslib")) {
    if (!is.null(title)) {
      out <- tryCatch(bslib::alert(..., color = color, title = title), error = function(e) NULL)
      if (!is.null(out)) return(out)
    }
    out <- tryCatch(bslib::alert(..., color = color), error = function(e) NULL)
    if (!is.null(out)) return(out)
  }

  cls <- paste("alert", paste0("alert-", color))
  shiny::tags$div(
    class = cls,
    role  = "alert",
    style = "border-radius:14px; font-weight:700; box-shadow:0 10px 22px rgba(0,0,0,.08);",
    if (!is.null(title)) shiny::tags$div(style="font-weight:900; margin-bottom:4px;", title),
    ...
  )
}
# ---------------------------------------------------------------------
# Base helpers / flags
# ---------------------------------------------------------------------
if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    a
  }
}

# Flags (debug de erros/stacktrace pode existir, mas Debug UI fica DESLIGADO e sem render)
if (!exists("SINAN_DEBUG", inherits = TRUE))    SINAN_DEBUG <- FALSE
if (!exists("SINAN_SAFE_UI", inherits = TRUE))  SINAN_SAFE_UI <- FALSE
if (!exists("SINAN_DEBUG_UI", inherits = TRUE)) SINAN_DEBUG_UI <- FALSE

.opt_bool <- function(opt_name, env_name, default = FALSE) {
  vopt <- getOption(opt_name, NULL)
  if (is.logical(vopt) && length(vopt) == 1L && !is.na(vopt)) return(isTRUE(vopt))
  venv <- Sys.getenv(env_name, unset = "")
  if (nzchar(venv) && tolower(venv) %in% c("1", "true", "yes", "y", "on")) return(TRUE)
  if (nzchar(venv) && tolower(venv) %in% c("0", "false", "no", "n", "off")) return(FALSE)
  isTRUE(default)
}

SINAN_DEBUG    <- .opt_bool("sinan.debug",    "SINAN_DEBUG",    default = SINAN_DEBUG)
SINAN_SAFE_UI  <- .opt_bool("sinan.safe_ui",  "SINAN_SAFE_UI",  default = SINAN_SAFE_UI)

#  Intuitivo: Debug UI SEMPRE OFF (remove a barra "Debug / Dump / Logs em ...")
SINAN_DEBUG_UI <- FALSE

options(
  shiny.sanitize.errors = !isTRUE(SINAN_DEBUG),
  shiny.fullstacktrace  = isTRUE(SINAN_DEBUG)
)

.core_debug_enabled     <- function() isTRUE(SINAN_DEBUG)
.core_debug_ui_enabled  <- function() FALSE
.core_safe_ui_enabled   <- function() isTRUE(SINAN_SAFE_UI)

.core_normpath <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# ---------------------------------------------------------------------
# PATCH HELPERS 
# ---------------------------------------------------------------------
.core_patch_helpers <- function() {
  if (!exists("%||%", mode = "function", inherits = TRUE)) {
    assign("%||%", function(a, b) if (is.null(a) || length(a) == 0) b else a, envir = .GlobalEnv)
  }

  if (!exists(".res", mode = "function", inherits = TRUE)) {
    assign(".res", function(x) {
      if (is.null(x)) return(NULL)
      if (is.function(x)) return(tryCatch(x(), error = function(e) NULL))
      x
    }, envir = .GlobalEnv)
  }

  if (!exists(".as_chr1", mode = "function", inherits = TRUE)) {
    assign(".as_chr1", function(x, default = NULL, ...) {
      x <- tryCatch(.res(x), error = function(e) x)
      if (is.null(x) || length(x) == 0) return(default)
      v <- as.character(x)
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) == 0) default else v[[1]]
    }, envir = .GlobalEnv)
  }

  if (!exists(".as_int1", mode = "function", inherits = TRUE)) {
    assign(".as_int1", function(x, default = NA_integer_, ...) {
      x <- tryCatch(.res(x), error = function(e) x)
      v <- suppressWarnings(as.integer(x))
      if (length(v) == 0) return(default)
      v <- v[!is.na(v)]
      if (length(v) == 0) return(default)
      out <- v[[1]]
      if (!is.finite(out)) default else out
    }, envir = .GlobalEnv)
  }

  if (!exists(".debug_enabled", mode = "function", inherits = TRUE)) {
    assign(".debug_enabled", function(cfg = NULL, ...) isTRUE(SINAN_DEBUG), envir = .GlobalEnv)
  }

  if (!exists(".dbg_box_ui", mode = "function", inherits = TRUE)) {
    assign(".dbg_box_ui", function(ns, id, ...) shiny::tagList(), envir = .GlobalEnv)
  }

  if (!exists(".dbg_make", mode = "function", inherits = TRUE)) {
    assign(".dbg_make", function(enabled = FALSE, session = shiny::getDefaultReactiveDomain(), prefix = NULL, ...) {
      enabled <- isTRUE(enabled)
      buf <- character(0)

      push <- function(...) {
        if (!enabled) return(invisible(NULL))
        msg <- paste(..., collapse = " ")
        buf <<- c(buf, msg)

        if (exists(".sinan_log_write", mode = "function", inherits = TRUE)) {
          sec <- if (!is.null(prefix) && nzchar(prefix)) prefix else "dbg"
          try(.sinan_log_write(sec, msg, session = session), silent = TRUE)
        }
        invisible(NULL)
      }

      get <- function() buf
      list(enabled = enabled, push = push, get = get)
    }, envir = .GlobalEnv)
  }

  invisible(NULL)
}

.core_patch_helpers()

.core_rv_get_now <- function(rv) {
  if (is.null(rv) || !is.function(rv)) return(NULL)
  tryCatch(shiny::isolate(rv()), error = function(e) NULL)
}
.core_rv_has_now <- function(rv) {
  x <- .core_rv_get_now(rv)
  is.character(x) && length(x) == 1L && nzchar(x)
}

# ---------------------------------------------------------------------
# Localização do core/R e source(utils.R)
# ---------------------------------------------------------------------
.core_find_core_r_dir <- function() {
  if (exists("CORE_R_DIR", inherits = TRUE)) {
    p <- .core_normpath(get("CORE_R_DIR", inherits = TRUE))
    if (dir.exists(p)) return(p)
  }

  if (exists("APP_DIR", inherits = TRUE)) {
    app <- .core_normpath(get("APP_DIR", inherits = TRUE))
    cand <- .core_normpath(file.path(app, "..", "..", "R"))
    if (dir.exists(cand)) return(cand)
  }

  p <- .core_normpath(tryCatch(getwd(), error = function(e) "."))

  for (i in 0:12) {
    cand1 <- .core_normpath(file.path(p, "dashs", "shiny", "core", "R"))
    if (dir.exists(cand1)) return(cand1)

    cand2 <- .core_normpath(file.path(p, "core", "R"))
    if (dir.exists(cand2)) return(cand2)

    cand3 <- .core_normpath(file.path(p, "R"))
    if (dir.exists(cand3) && file.exists(file.path(cand3, "utils.R"))) return(cand3)

    p2 <- dirname(p)
    if (identical(p2, p)) break
    p <- p2
  }

  NULL
}

.core_source_once <- local({
  .seen <- new.env(parent = emptyenv())
  function(path) {
    p <- .core_normpath(path)
    if (!nzchar(p) || !file.exists(p)) return(FALSE)
    if (exists(p, envir = .seen, inherits = FALSE)) return(TRUE)
    tryCatch(source(p, local = FALSE), error = function(e) stop(e))
    assign(p, TRUE, envir = .seen)
    TRUE
  }
})

CORE_R_DIR <- .core_find_core_r_dir() %||%
  (if (exists("CORE_R_DIR", inherits = TRUE)) get("CORE_R_DIR", inherits = TRUE) else NULL) %||%
  NULL

if (!is.null(CORE_R_DIR) && dir.exists(CORE_R_DIR)) {
  .core_source_once(file.path(CORE_R_DIR, "utils.R"))
  .core_patch_helpers()
}

# ---------------------------------------------------------------------
# Utils diversos
# ---------------------------------------------------------------------
.core_year_min <- function(cfg) as.integer(cfg$YEAR_MIN %||% cfg$year_min %||% 2010L)
.core_year_max <- function(cfg) as.integer(cfg$YEAR_MAX %||% cfg$year_max %||% 2025L)

.core_call_compat <- function(fun, ...) {
  dots <- list(...)
  fml <- tryCatch(names(formals(fun)), error = function(e) NULL)
  if (is.null(fml)) return(do.call(fun, dots))
  if ("..." %in% fml) return(do.call(fun, dots))
  keep <- dots[names(dots) %in% fml]
  do.call(fun, keep)
}

.core_req_fun <- function(name) {
  if (!exists(name, mode = "function", inherits = TRUE)) {
    stop("Função não encontrada: ", name, " (verifique source() / nome no arquivo do módulo).", call. = FALSE)
  }
}

.core_unwrap <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.function(x)) return(tryCatch(x(), error = function(e) NULL))
  x
}

.core_nav_init <- function(year_max = 2025L) {
  shiny::reactiveValues(
    level  = "UF",
    uf     = "PE",
    mun    = NULL,
    detail = FALSE,
    ano    = as.integer(year_max),
    metric = "incid",
    years_available = NULL,
    year_min = NA_integer_,
    year_max = as.integer(year_max),
    region_view    = "MUN",
    selected_macro = NULL,
    selected_micro = NULL,
    tb_grau_incapacidade = NULL
  )
}

.core_norm_key <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  x <- gsub("[^A-Z0-9]+", "", x)
  x
}

.core_norm_doenca_values <- function(doenca) {
  d <- toupper(as.character(doenca %||% ""))
  if (!nzchar(d)) return(character(0))
  c(d)
}

.core_has_parquet <- function(path) {
  dir.exists(path) &&
    length(list.files(path, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)) > 0
}

.core_find_dashboard_dir <- function(cfg) {
  dash <- cfg$DASHBOARD_DIR %||% cfg$dashboard_dir %||% NULL
  if (is.character(dash) && nzchar(dash) && dir.exists(dash)) return(.core_normpath(dash))

  root <- cfg$ROOT %||% cfg$root %||% NULL
  if (is.character(root) && nzchar(root)) {
    cand <- file.path(root, "data", "parquet")
    if (dir.exists(cand)) return(.core_normpath(cand))
  }

  p <- .core_normpath(tryCatch(getwd(), error = function(e) "."))
  for (i in 0:12) {
    cand <- file.path(p, "data", "parquet")
    if (dir.exists(cand)) return(.core_normpath(cand))
    p2 <- dirname(p)
    if (identical(p2, p)) break
    p <- p2
  }
  NULL
}

.core_intro_title <- function(pack) {
  d0 <- toupper(trimws(as.character(pack$DOENCA %||% pack$doenca %||% "")))
  if (d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")) return("Painel de Monitoramento da Tuberculose de Recife")
  if (!nzchar(d0)) return("Painel de Monitoramento de PE")
  paste0("Painel de Monitoramento de ", d0, " de PE")
}

.core_intro_asset_src <- function(cfg, file_name) {
  dash <- .core_find_dashboard_dir(cfg)
  if (is.null(dash) || !nzchar(dash)) return(NULL)

  base_dir <- tryCatch(normalizePath(dirname(dash), winslash = "/", mustWork = FALSE), error = function(e) NULL)
  img_root <- Sys.getenv("SINAN_IMG_ROOT", unset = "")
  img_root <- if (nzchar(img_root)) tryCatch(normalizePath(img_root, winslash = "/", mustWork = FALSE), error = function(e) NULL) else NULL

  file_candidates <- unique(c(
    file_name,
    if (identical(file_name, "Bandeira_de_Pernambuco.jpeg")) "Bandeira_Recife.jpeg" else NULL,
    if (identical(file_name, "cenarios_logo_full.jpeg")) "Cenarios_Logo.jpeg" else NULL
  ))

  dir_candidates <- unique(Filter(nzchar, c(
    img_root,
    if (!is.null(base_dir)) file.path(base_dir, "Imagens") else NULL,
    base_dir
  )))

  p <- NULL
  for (dir_cand in dir_candidates) {
    for (file_cand in file_candidates) {
      cand <- tryCatch(normalizePath(file.path(dir_cand, file_cand), winslash = "/", mustWork = FALSE), error = function(e) NULL)
      if (!is.null(cand) && file.exists(cand)) {
        p <- cand
        break
      }
    }
    if (!is.null(p)) break
  }

  if (is.null(p) || !file.exists(p)) return(NULL)

  tryCatch(shiny::addResourcePath("sinan_assets", dirname(p)), error = function(e) NULL)
  mtime <- tryCatch(as.integer(file.info(p)$mtime), error = function(e) as.integer(Sys.time()))
  paste0("sinan_assets/", basename(p), "?v=", mtime)
}

.core_years_from_fs <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- unique(paths[dir.exists(paths)])
  if (length(paths) == 0) return(integer(0))

  yrs <- integer(0)
  for (p in paths) {
    d <- tryCatch(
      list.files(p, pattern = "^ano=\\d{4}$", recursive = TRUE, full.names = FALSE),
      error = function(e) character(0)
    )
    if (length(d) == 0) next
    m <- regmatches(d, regexpr("ano=\\d{4}", d))
    y <- suppressWarnings(as.integer(gsub("^ano=", "", m)))
    y <- y[is.finite(y)]
    if (length(y) > 0) yrs <- c(yrs, y)
  }
  sort(unique(yrs))
}

.core_open_dataset_safe <- function(path_or_paths) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(NULL)

  if (is.null(path_or_paths) || length(path_or_paths) == 0) return(NULL)
  paths <- as.character(path_or_paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0) return(NULL)
  paths <- paths[file.exists(paths) | dir.exists(paths)]
  if (length(paths) == 0) return(NULL)

  fn <- arrow::open_dataset
  fml <- names(formals(fn))

  .try <- function(part_hive = TRUE, unify = FALSE) {
    args <- list(sources = paths, format = "parquet")
    if ("partitioning" %in% fml && isTRUE(part_hive)) args$partitioning <- "hive"
    if ("unify_schemas" %in% fml) args$unify_schemas <- isTRUE(unify)
    if ("use_threads" %in% fml) args$use_threads <- TRUE
    tryCatch(do.call(fn, args), error = function(e) NULL)
  }

  ds <- .try(TRUE, FALSE);  if (!is.null(ds)) return(ds)
  ds <- .try(TRUE, TRUE);   if (!is.null(ds)) return(ds)
  ds <- .try(FALSE, FALSE); if (!is.null(ds)) return(ds)
  ds <- .try(FALSE, TRUE);  if (!is.null(ds)) return(ds)
  NULL
}

.core_years_from_ds <- function(ds, doenca) {
  if (is.null(ds)) return(integer(0))
  if (!requireNamespace("dplyr", quietly = TRUE)) return(integer(0))

  cols <- tryCatch(names(ds), error = function(e) character(0))
  if (length(cols) == 0) return(integer(0))

  ycol <- if ("ano" %in% cols) "ano" else if ("ANO" %in% cols) "ANO" else NULL
  if (is.null(ycol)) return(integer(0))

  q <- ds
  dd <- .core_norm_doenca_values(doenca)
  if (length(dd) > 0) {
    if ("doenca" %in% cols) q <- tryCatch(dplyr::filter(q, .data$doenca %in% dd), error = function(e) q)
    if ("DOENCA" %in% cols) q <- tryCatch(dplyr::filter(q, .data$DOENCA %in% dd), error = function(e) q)
  }

  yrs <- tryCatch({
    q |>
      dplyr::select(dplyr::all_of(ycol)) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(1)
  }, error = function(e) integer(0))

  yrs <- suppressWarnings(as.integer(yrs))
  yrs <- yrs[is.finite(yrs)]
  sort(unique(yrs))
}

.core_years_cache <- local({
  e <- new.env(parent = emptyenv())
  function(key, value = NULL, set = FALSE) {
    if (!set) {
      if (exists(key, envir = e, inherits = FALSE)) return(get(key, envir = e, inherits = FALSE))
      return(NULL)
    }
    assign(key, value, envir = e)
    invisible(value)
  }
})

.core_discover_years_available <- function(cfg, pack, data_api = NULL) {
  doenca <- (pack$DOENCA %||% cfg$DOENCA) %||% ""
  doenca <- as.character(doenca)

  if (is.list(data_api) && is.function(data_api$get_years_available)) {
    yrs_api <- tryCatch({
      fml <- tryCatch(names(formals(data_api$get_years_available)), error = function(e) NULL)
      if (!is.null(fml) && length(fml) > 0) data_api$get_years_available("BR") else data_api$get_years_available()
    }, error = function(e) integer(0))
    yrs_api <- suppressWarnings(as.integer(yrs_api))
    yrs_api <- yrs_api[is.finite(yrs_api)]
    if (length(yrs_api) > 0) return(sort(unique(yrs_api)))
  }

  cfg2 <- (if (!is.null(data_api) && is.list(data_api)) data_api$cfg else NULL) %||% cfg

  paths <- NULL
  if (!is.null(cfg2$PATHS) && is.list(cfg2$PATHS) && !is.null(cfg2$PATHS$PATH_INCIDENCE)) {
    paths <- cfg2$PATHS$PATH_INCIDENCE
  } else {
    paths <- cfg2$PATH_INCIDENCE %||% c(cfg2$PATH_INCIDENCE_BR, cfg2$PATH_INCIDENCE_UF, cfg2$PATH_INCIDENCE_MUN)
  }

  dash <- .core_find_dashboard_dir(cfg2)
  if (!is.null(dash) && dir.exists(dash)) {
    paths <- c(
      paths,
      file.path(dash, "incidence"),
      file.path(dash, "incidencia"),
      file.path(dash, "incidence_br"),
      file.path(dash, "incidence_uf"),
      file.path(dash, "incidence_mun"),
      file.path(dash, "_cache_incidence"),
      file.path(dash, "_cache_inc")
    )
  }

  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- unique(paths[dir.exists(paths)])
  if (length(paths) == 0) return(integer(0))
  if (!any(vapply(paths, .core_has_parquet, logical(1)))) return(integer(0))

  key <- paste0(.core_norm_key(doenca), "::", paste(sort(.core_normpath(paths)), collapse = "|"))
  cached <- .core_years_cache(key)
  if (!is.null(cached)) return(cached)

  yrs <- .core_years_from_fs(paths)
  if (length(yrs) > 0) {
    .core_years_cache(key, yrs, set = TRUE)
    return(yrs)
  }

  ds <- .core_open_dataset_safe(paths)
  if (is.null(ds)) return(integer(0))

  yrs <- .core_years_from_ds(ds, doenca = doenca)
  .core_years_cache(key, yrs, set = TRUE)
  yrs
}

# ---------------------------------------------------------------------
# Slider temporal
# ---------------------------------------------------------------------
.core_snap_year <- function(y, yrs) {
  y <- suppressWarnings(as.integer(y))
  if (!is.finite(y)) return(NA_integer_)
  yrs <- suppressWarnings(as.integer(yrs))
  yrs <- yrs[is.finite(yrs)]
  if (length(yrs) == 0) return(y)
  if (y %in% yrs) return(y)
  yrs[which.min(abs(yrs - y))][[1]]
}

.core_year_slider_bind <- function(
  input, output, session,
  nav,
  year_min_fallback = 2010L,
  year_max_fallback = 2025L,
  output_id = "year_slider_ui",
  input_id  = "year_slider",
  label = "Ano",
  debounce_ms = 500L
) {
  stopifnot(!is.null(nav))

  badge_id  <- paste0(output_id, "__badges")

  years_r <- shiny::reactive({
    yrs <- .core_unwrap(nav$years_available)
    yrs <- suppressWarnings(as.integer(yrs))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) > 0) return(sort(unique(yrs)))

    ymn <- suppressWarnings(as.integer(.core_unwrap(nav$year_min) %||% year_min_fallback))
    ymx <- suppressWarnings(as.integer(.core_unwrap(nav$year_max) %||% year_max_fallback))
    if (!is.finite(ymn)) ymn <- year_min_fallback
    if (!is.finite(ymx)) ymx <- year_max_fallback
    if (ymx < ymn) { tmp <- ymn; ymn <- ymx; ymx <- tmp }
    seq.int(ymn, ymx)
  })

  current_year_r <- shiny::reactive({
    y <- .core_unwrap(nav$ano)
    y <- suppressWarnings(as.integer(y))
    if (!is.finite(y)) y <- suppressWarnings(as.integer(tail(years_r(), 1)))
    .core_snap_year(y, years_r())
  })

  output[[output_id]] <- shiny::renderUI({
    yrs <- years_r()
    current_yr <- .core_snap_year(.core_unwrap(nav$ano) %||% max(yrs), yrs)

    shiny::tags$div(
      class = "sinan-yearbox",
      shiny::tags$div(
        class = "sinan-yearbox-head",
        shiny::tags$div(class = "sinan-yearbox-badges", shiny::uiOutput(badge_id)),
        # Spinner durante carregamento
        shiny::uiOutput(paste0(output_id, "__spinner"), style = "display: none;")
      ),
      shiny::tags$div(
        class = "sinan-yearbox-body",
        shiny::selectizeInput(
          inputId = input_id,
          label = NULL,
          choices = setNames(as.character(yrs), as.character(yrs)),
          selected = as.character(current_yr),
          width = "100%",
          options = list(dropdownParent = "body")
        )
      )
    )
  })

  output[[badge_id]] <- shiny::renderUI({
    y0 <- current_year_r()

    shiny::tags$span(
      class = "sinan-year-pill",
      sprintf("%s: %s", label, ifelse(is.finite(y0), y0, "—"))
    )
  })

  year_raw <- shiny::reactive({
    val <- input[[input_id]]
    if (is.null(val) || !nzchar(val)) return(NULL)
    suppressWarnings(as.integer(val))
  })
  year_db  <- shiny::debounce(year_raw, millis = as.integer(debounce_ms))

  shiny::observeEvent(year_db(), {
    yrs <- years_r()
    y   <- .core_snap_year(year_db(), yrs)
    if (!is.finite(y)) return(invisible(NULL))
    if (!identical(suppressWarnings(as.integer(nav$ano)), y)) nav$ano <- y
    invisible(NULL)
  }, ignoreInit = TRUE)

  # Não precisa mais de updateSliderInput, os botões são re-renderizados automaticamente
  shiny::observeEvent(nav$ano, {
    # A UI já re-renderiza com o ano correto
    invisible(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(years_r(), {
    # A UI já re-renderiza com anos disponíveis corretos
    invisible(NULL)
  }, ignoreInit = TRUE)
  
  # Spinner durante carregamento de dados
  spinner_id <- paste0(output_id, "__spinner")
  output[[spinner_id]] <- shiny::renderUI({
    shiny::tags$div(
      class = "sinan-year-spinner",
      shiny::tags$div(
        class = "atom-spinner",
        shiny::tags$div(class = "spinner-inner"),
        shiny::tags$div(class = "spinner-line"),
        shiny::tags$div(class = "spinner-line"),
        shiny::tags$div(class = "spinner-line")
      ),
      shiny::tags$span(class = "spinner-text", "Carregando...")
    )
  })
  
  # Controlar visibilidade do spinner
  shiny::observe({
    is_loading <- shiny::isTruthy(input[[input_id]]) && 
      !identical(suppressWarnings(as.integer(input[[input_id]])), suppressWarnings(as.integer(nav$ano)))
    
    session$sendCustomMessage(
      type = "toggleYearSpinner",
      list(spinner_id = spinner_id, show = is_loading)
    )
  })
}

.core_metric_label <- function(metric) {
  m <- .core_metric_norm(metric)
  if (identical(m, "incid")) return("Incidência")
  if (identical(m, "cases")) return("Casos")
  if (identical(m, "obitos")) return("Óbitos")
  if (identical(m, "cura")) return("Curas entre casos novos")
  if (identical(m, "pop")) return("População")
  toupper(m)
}

.core_metric_box_bind <- function(
  output,
  nav,
  output_id = "metric_box_ui"
) {
  stopifnot(!is.null(nav))

  help_text <- "Ano: recorte temporal aplicado ao painel. KPI: indicador ativo aplicado no mapa, cards e gráficos."

  output[[output_id]] <- shiny::renderUI({
    metric_now <- .core_metric_norm(.core_unwrap(nav$metric) %||% "incid")
    metric_label <- .core_metric_label(metric_now)

    shiny::tags$div(
      class = "sinan-yearbox sinan-kpibox",
      shiny::tags$div(
        class = "sinan-yearbox-head",
        shiny::tags$div(
          class = "sinan-yearbox-badges",
          shiny::tags$span(
            class = "sinan-year-pill sinan-kpi-pill",
            sprintf("Métrica: %s", metric_label)
          )
        ),
        shiny::tags$span(
          class = "sinan-box-help",
          title = help_text,
          `aria-label` = "Ajuda sobre os boxes de Ano e KPI",
          "i"
        )
      )
    )
  })
}

.core_err_card <- function(title = "Falha ao renderizar", subtitle = NULL, detail = NULL) {
  bslib::card(
    full_screen = FALSE,
    bslib::card_header(
      shiny::tags$div(
        style = "display:flex;gap:10px;align-items:center;",
        shiny::tags$span("\u26A0\uFE0F"),
        shiny::tags$div(
          shiny::tags$div(style = "font-weight:800;", title),
          if (!is.null(subtitle) && nzchar(subtitle)) shiny::tags$div(style = "opacity:.85;", subtitle) else NULL
        )
      )
    ),
    bslib::card_body(
      if (!is.null(detail) && nzchar(detail)) shiny::tags$pre(style = "white-space:pre-wrap;", detail) else NULL,
      shiny::tags$div(style = "opacity:.8;font-size:12px;margin-top:8px;",
                      "Dica: ative SINAN_DEBUG=TRUE para stacktrace completo.")
    )
  )
}

.core_try_ui <- function(expr, title = "Falha ao renderizar UI") {
  tryCatch(expr, error = function(e) {
    .core_err_card(
      title = title,
      subtitle = conditionMessage(e),
      detail = if (isTRUE(SINAN_DEBUG)) paste(capture.output(print(e)), collapse = "\n") else ""
    )
  })
}

.core_try_server <- function(expr, on_error = NULL) {
  tryCatch(expr, error = function(e) {
    if (is.function(on_error)) {
      try(on_error(e), silent = TRUE)
    }
    NULL
  })
}

# ---------------------------------------------------------------------
# cor dinâmica do slider + pill do ano
# ---------------------------------------------------------------------
.core_hex_to_rgb <- function(hex) {
  if (is.null(hex) || !nzchar(hex)) return(NULL)
  h <- toupper(trimws(as.character(hex[[1]])))
  if (!grepl("^#", h)) h <- paste0("#", h)
  if (!grepl("^#[0-9A-F]{6}$", h)) return(NULL)
  r <- strtoi(substr(h, 2, 3), 16L)
  g <- strtoi(substr(h, 4, 5), 16L)
  b <- strtoi(substr(h, 6, 7), 16L)
  paste0(r, ",", g, ",", b)
}

.core_metric_norm <- function(metric) {
  m <- tolower(trimws(as.character(metric %||% "")))
  if (!nzchar(m)) return("incid")

  if (m %in% c("incid", "incidencia", "incidence", "incid_100k", "incid_100k_total", "incid100k")) return("incid")
  if (m %in% c("cases", "casos", "casos_total", "case", "qt_casos", "total_casos")) return("cases")
  if (m %in% c("obitos", "óbito", "obito", "deaths", "mortes", "casos_obitos", "total_obitos")) return("obitos")
  if (m %in% c("cura", "curas", "cured", "casos_cura", "total_cura", "encerr_cura")) return("cura")
  if (m %in% c("pop", "populacao", "população", "population", "pop_total")) return("pop")

  if (m %in% c("incid", "cases", "obitos", "cura", "pop")) return(m)
  "incid"
}

.core_metric_color <- function(metric, pack = NULL) {
  m <- .core_metric_norm(metric)
  cols <- (pack$COLORS %||% pack$colors) %||% list()

  primary   <- cols$primary   %||% "#111827"
  secondary <- cols$secondary %||% "#2563EB"
  danger    <- cols$danger    %||% "#DC2626"
  success   <- cols$success   %||% "#16A34A"

  if (m == "cases")  return(danger)
  if (m == "obitos") return(danger)
  if (m == "cura")   return(success)
  if (m == "pop")    return(primary)
  secondary
}

.core_metric_bind <- function(input, session, nav, pack) {
  stopifnot(!is.null(nav))

  candidates <- c(
    "metric", "metric_sel", "metric_selected",
    "kpi", "kpi_sel", "kpi_metric", "kpi_metric_sel",
    "kpi_selected", "kpi_value",
    "kpis_metric", "kpis_metric_sel",
    "cards_metric", "cards_metric_sel"
  )

  .pick_from_input <- function() {
    for (id in candidates) {
      key <- paste0("kpis-", id)
      v <- tryCatch(input[[key]], error = function(e) NULL)
      if (!is.null(v) && length(v) > 0 && nzchar(as.character(v[[1]]))) return(as.character(v[[1]]))
    }
    for (id in candidates) {
      v <- tryCatch(input[[id]], error = function(e) NULL)
      if (!is.null(v) && length(v) > 0 && nzchar(as.character(v[[1]]))) return(as.character(v[[1]]))
    }
    NULL
  }

  metric_ui <- shiny::reactive({
    .core_metric_norm(.pick_from_input())
  })

  shiny::observeEvent(metric_ui(), {
    m <- metric_ui()
    if (!nzchar(m)) return(invisible(NULL))
    cur <- .core_metric_norm(.core_unwrap(nav$metric))
    if (!identical(cur, m)) nav$metric <- m
    invisible(NULL)
  }, ignoreInit = TRUE)

  metric <- shiny::reactive({
    .core_metric_norm(.core_unwrap(nav$metric))
  })

  shiny::observe({
    m <- metric()
    col <- .core_metric_color(m, pack = pack)
    rgb <- .core_hex_to_rgb(col)
    session$sendCustomMessage("sinan_set_accent", list(hex = col, rgb = rgb))
  })

  metric
}

.core_is_tuberculose <- function(cfg = NULL, pack = NULL) {
  d <- toupper(trimws(as.character(
    (pack$DOENCA %||% pack$doenca %||% cfg$DOENCA %||% cfg$doenca) %||% ""
  )))
  d %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")
}

.core_tb_grau_vars <- function(cfg = NULL, pack = NULL) {
  vals <- c(
    pack$TB_GRAU_INCAPACIDADE_VAR,
    pack$TB_GRAU_INCAPACIDADE_VARS,
    cfg$TB_GRAU_INCAPACIDADE_VAR,
    cfg$TB_GRAU_INCAPACIDADE_VARS,
    "GRAU_INCAPACIDADE_FISICA",
    "GRAU_INCAPACIDADE",
    "GRAU_INCAP",
    "GRAUINC",
    "NU_GRAU",
    "TP_GRAU",
    "AVALIA_N"
  )
  vals <- as.character(unlist(vals, use.names = FALSE))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  unique(vals)
}

.core_tb_grau_filter_bind <- function(input, output, session, nav, cfg, pack, data_api, ano,
                                      output_id = "tb_grau_filter_ui",
                                      input_id = "tb_grau_filter") {
  stopifnot(!is.null(nav))

  all_value <- "__ALL__"
  choice_state <- shiny::reactiveVal(list(var = NULL, choices = character(0), labels = character(0)))

  .norm_val <- function(x) {
    x <- as.character(x %||% "")
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) character(0) else x
  }

  choices_r <- shiny::reactive({
    if (!.core_is_tuberculose(cfg, pack) || is.null(data_api) || !is.list(data_api) ||
        !is.function(data_api$get_composicao)) {
      return(list(var = NULL, choices = character(0), labels = character(0)))
    }

    y <- suppressWarnings(as.integer(.core_unwrap(ano)))
    if (!is.finite(y)) return(list(var = NULL, choices = character(0), labels = character(0)))

    vars <- .core_tb_grau_vars(cfg, pack)
    for (v in vars) {
      df <- tryCatch(
        data_api$get_composicao(
          ano = y, year = y, level = "UF", nivel = "UF", uf = "PE",
          doenca = "TUBERCULOSE", vars = v, top_n = 5000
        ),
        error = function(e) data.frame()
      )
      if (!is.data.frame(df) || nrow(df) == 0) next

      nms <- names(df)
      c_lab <- c("valor_lbl", "VALOR_LBL", "categoria", "CATEGORIA", "cat", "CAT")
      c_val <- c("valor", "VALOR", "value", "VALUE")
      lab_col <- c_lab[c_lab %in% nms][[1]] %||% NULL
      val_col <- c_val[c_val %in% nms][[1]] %||% lab_col
      if (is.null(lab_col) || is.null(val_col)) next

      vals <- .norm_val(df[[val_col]])
      labs <- .norm_val(df[[lab_col]])
      if (length(vals) == 0 || length(labs) == 0) next

      tab <- unique(data.frame(value = vals, label = labs, stringsAsFactors = FALSE))
      tab <- tab[!is.na(tab$value) & nzchar(tab$value) & !is.na(tab$label) & nzchar(tab$label), , drop = FALSE]
      if (nrow(tab) == 0) next
      tab <- tab[order(tab$label), , drop = FALSE]
      return(list(var = v, choices = tab$value, labels = stats::setNames(tab$value, tab$label)))
    }

    list(var = NULL, choices = character(0), labels = character(0))
  })

  output[[output_id]] <- shiny::renderUI({
    st <- choices_r()
    choice_state(st)
    if (is.null(st$var) || length(st$choices) == 0) return(NULL)

    cur <- tryCatch(input[[input_id]], error = function(e) NULL)
    sel <- if (!is.null(cur) && cur %in% c(all_value, st$choices)) cur else all_value

    shiny::tags$div(
      class = "sinan-yearbox sinan-tb-grau-box",
      shiny::tags$div(
        class = "sinan-yearbox-head",
        shiny::tags$div(
          class = "sinan-yearbox-badges",
          shiny::tags$span(class = "sinan-year-pill", "Grau de incapacidade fisica")
        )
      ),
      shiny::tags$div(
        class = "sinan-yearbox-body",
        shiny::selectInput(
          inputId = input_id,
          label = NULL,
          choices = c("Todas as categorias" = all_value, st$labels),
          selected = sel,
          width = "100%"
        )
      )
    )
  })

  shiny::observeEvent(input[[input_id]], {
    st <- choice_state()
    val <- as.character(input[[input_id]] %||% all_value)
    if (is.null(st$var) || !nzchar(st$var) || identical(val, all_value)) {
      nav$tb_grau_incapacidade <- NULL
    } else {
      nav$tb_grau_incapacidade <- list(var = st$var, value = val)
    }
  }, ignoreInit = FALSE)
}

# ---------------------------------------------------------------------
# CSS/JS globais do shell
# ---------------------------------------------------------------------
.core_shell_css <- function() {
  "
  :root{
    --sinan-accent: #2563EB;
    --sinan-accent-rgb: 37, 99, 235;
  }

  html, body { height: 100%; }
  .bslib-page-fill, .bslib-page-fill > .container-fluid { height: 100%; }
  .bslib-sidebar-layout, .bslib-sidebar-layout > .main { min-height: 0; height: 100%; }

  .sinan-app{
    height: 100%;
    min-height: 0;
    display: flex;
    flex-direction: column;
    gap: 12px;
    padding: 12px;
    box-sizing: border-box;
    overflow-y: auto;
  }
  .sinan-intro{
    flex: 0 0 auto;
    min-height: 0;
    display:grid;
    grid-template-columns: minmax(0, 1fr) auto minmax(0, 1fr);
    align-items:center;
    gap: 14px;
    padding: 10px 12px;
    border-radius: 14px;
    border: 1px solid rgba(0,0,0,.08);
    background: linear-gradient(180deg, rgba(255,255,255,.96), rgba(247,250,255,.92));
    box-shadow: 0 10px 22px rgba(0,0,0,.08);
  }
  .sinan-intro-flagbox{
    display:flex;
    align-items:center;
    justify-content:flex-start;
    min-width: 0;
    width: 100%;
  }
  .sinan-intro-title{
    margin:0;
    font-size: clamp(18px, 2.1vw, 30px);
    line-height: 1.08;
    font-weight: 900;
    letter-spacing: .01em;
    color: #0b1220;
    text-wrap: balance;
    text-align: center;
    justify-self: center;
    width: 100%;
  }
  .sinan-intro-brand{
    display:flex;
    align-items:center;
    justify-content:flex-end;
    gap: 10px;
    min-width: 0;
    width: 100%;
  }
  .sinan-intro-flag{
    display:block;
    width:auto;
    height:auto;
    max-height: 54px;
    max-width: min(18vw, 140px);
    object-fit: contain;
  }
  .sinan-intro-logo{
    display:block;
    width:auto;
    height:auto;
    max-height: 66px;
    max-width: min(33vw, 290px);
    object-fit: contain;
  }
  .sinan-kpis{ flex: 0 0 auto; min-height: 0; }

  .sinan-mainrow{
    flex: 0 0 auto;
    min-height: 760px;
    display: flex;
    gap: 12px;
    align-items: stretch;
  }
  .sinan-bottomrow{
    flex: 0 0 auto;
    min-height: 0;
    margin-top: 22px;
    clear: both;
  }
  .sinan-evol-external-row{
    display:flex;
    flex-direction:row;
    align-items:stretch;
    flex-wrap:nowrap;
    gap:12px;
    width:100%;
  }
  .sinan-evol-external-chart{
    flex:1 1 50%;
    min-width:0;
    display:flex;
    flex-direction:column;
  }
  .sinan-leftcol{
    flex: 1 1 0;
    min-width: 0;
    min-height: 760px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }
  .sinan-rightcol{
    flex: 1 1 0;
    min-width: 0;
    min-height: 760px;
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .sinan-map-panel{
    flex: 1 1 auto !important;
    min-height: 760px;
    height: 100% !important;
    position: relative;
    z-index: 1;
  }

  .sinan-leftcol > :not(.sinan-map-panel){
    position: relative;
    z-index: 20;
  }

  .sinan-rightcol > * { flex: 1 1 auto; min-height: 760px; }

  .leaflet.html-widget, .leaflet-container,
  .plotly.html-widget,
  .echarts4r.html-widget{
    height: 100% !important;
    min-height: 0 !important;
    width: 100% !important;
  }
  .card, .bslib-card { min-height: 0; }

  .sinan-rightcol .bslib-card,
  .sinan-rightcol .card{
    overflow: hidden;
  }
  .sinan-rightcol .bslib-card .card-body,
  .sinan-rightcol .card .card-body{
    padding-left: 10px !important;
    padding-right: 10px !important;
    padding-top: 10px !important;
    padding-bottom: 8px !important;
  }
  .sinan-rightcol .tab-content{
    padding-left: 0 !important;
    padding-right: 0 !important;
  }
  .sinan-rightcol .tab-pane{
    padding-left: 0 !important;
    padding-right: 0 !important;
  }
  .sinan-rightcol .html-widget{
    margin-left: -6px !important;
    margin-right: 0 !important;
  }

  @media (max-width: 992px){
    .sinan-evol-external-row{
      flex-direction:column;
    }
    .sinan-intro{
      grid-template-columns: auto minmax(0, 1fr);
      align-items: center;
      gap: 8px;
    }
    .sinan-intro-flagbox{
      min-width: 0;
      grid-column: 1;
      grid-row: 1;
    }
    .sinan-intro-title{
      grid-column: 2;
      grid-row: 1;
      text-align: center;
    }
    .sinan-intro-brand{
      grid-column: 1 / -1;
      justify-content: center;
      min-width: 0;
    }
    .sinan-intro-flag{ max-height: 50px; max-width: 34vw; }
    .sinan-intro-logo{ max-height: 58px; max-width: 75vw; }
  }

  .sinan-yearbox{
    border: 1px solid rgba(0,0,0,.08);
    border-radius: 16px;
    padding: 8px 12px 10px 12px;
    background: rgba(0,0,0,.012);
    margin-bottom: 8px;
    box-shadow: 0 10px 28px rgba(0,0,0,.05);
    position: relative;
    z-index: 25;
  }
  .sinan-yearbox-head{
    display:flex;
    align-items:center;
    justify-content:space-between;
    margin-bottom: 6px;
    gap: 8px;
    flex-wrap: wrap;
  }
  .sinan-yearbox-badges{ display:flex; gap:8px; align-items:center; flex-wrap:wrap; }
  .sinan-year-pill{
    font-size: 12px;
    font-weight: 800;
    padding: 5px 10px;
    border-radius: 999px;
    background: rgba(var(--sinan-accent-rgb), .12);
    color: var(--sinan-accent);
    border: 1px solid rgba(var(--sinan-accent-rgb), .22);
  }
  .sinan-kpibox{
    margin-top: 0;
  }
  .sinan-kpi-pill{
    background: rgba(var(--sinan-accent-rgb), .12);
    color: var(--sinan-accent);
    border-color: rgba(var(--sinan-accent-rgb), .22);
  }
  .sinan-box-help{
    display:inline-flex;
    align-items:center;
    justify-content:center;
    width:18px;
    height:18px;
    border-radius:50%%;
    font-size:11px;
    font-weight:800;
    cursor:help;
    user-select:none;
    color: var(--sinan-accent);
    background: rgba(var(--sinan-accent-rgb), .08);
    border: 1px solid rgba(var(--sinan-accent-rgb), .24);
  }

  /* Dropdown de anos */
  .sinan-yearbox-body select {
    font-size: 13px;
    font-weight: 600;
    padding: 7px 11px;
    border-radius: 8px;
    background: white;
    color: #333;
    border: 1.5px solid rgba(var(--sinan-accent-rgb), .3);
    cursor: pointer;
    transition: all 0.2s ease;
    appearance: none;
    padding-right: 32px;
    background-image: url(\'data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 width=%2212%22 height=%2212%22 viewBox=%220 0 12 12%22%3E%3Cpath fill=%22%23333%22 d=%22M1.5 4.5l4.5 4.5 4.5-4.5%22/%3E%3C/svg%3E\');
    background-repeat: no-repeat;
    background-position: right 10px center;
    position: relative;
    z-index: 1000;
  }

  .sinan-yearbox-body select:hover {
    border-color: rgba(var(--sinan-accent-rgb), .6);
    background-color: rgba(var(--sinan-accent-rgb), .03);
  }

  .sinan-yearbox-body select:focus {
    outline: none;
    border-color: var(--sinan-accent);
    box-shadow: 0 0 0 3px rgba(var(--sinan-accent-rgb), .1);
    background-color: white;
    z-index: 1001;
  }

  /* Garantir que dropdown aberto fica por cima */
  select {
    z-index: 1000 !important;
  }

  select:focus {
    z-index: 1001 !important;
  }

  option {
    z-index: 1001 !important;
  }

  .sinan-map-arrow-highlight{
    outline: 3px solid rgba(255,193,7,.55) !important;
    box-shadow: 0 0 0 6px rgba(255,193,7,.15), 0 10px 30px rgba(0,0,0,.15) !important;
    border-radius: 10px !important;
    animation: sinanPulse 1.4s ease-in-out infinite;
  }
  @keyframes sinanPulse{
    0% { transform: translateZ(0) scale(1); }
    50% { transform: translateZ(0) scale(1.05); }
    100% { transform: translateZ(0) scale(1); }
  }

  /* Loading spinner - Indicador de carregamento */
  .sinan-loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.35);
    display: none;
    align-items: center;
    justify-content: center;
    z-index: 9999;
    backdrop-filter: blur(2px);
  }

  .sinan-loading-overlay.active {
    display: flex;
  }

  .sinan-loading-spinner {
    width: 60px;
    height: 60px;
    border: 5px solid rgba(255, 255, 255, 0.3);
    border-top: 5px solid #fff;
    border-radius: 50%;
    animation: sinanSpin 0.8s linear infinite;
    box-shadow: 0 0 20px rgba(0, 0, 0, 0.3);
  }

  .sinan-loading-text {
    position: absolute;
    bottom: 20%;
    color: #fff;
    font-size: 16px;
    font-weight: 600;
    text-align: center;
    letter-spacing: 1px;
  }

  @keyframes sinanSpin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  /* Atom spinner para indicador de carregamento do ano */
  .sinan-year-spinner {
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
    padding: 8px 12px;
    border-radius: 8px;
    background: rgba(var(--sinan-accent-rgb), 0.08);
    border: 1px solid rgba(var(--sinan-accent-rgb), 0.2);
  }

  .atom-spinner {
    width: 20px;
    height: 20px;
    position: relative;
    flex: 0 0 20px;
  }

  .atom-spinner .spinner-inner {
    position: absolute;
    width: 6px;
    height: 6px;
    background: var(--sinan-accent);
    border-radius: 50%;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
  }

  .atom-spinner .spinner-line {
    position: absolute;
    width: 100%;
    height: 100%;
    border-radius: 50%;
    border: 1.5px solid transparent;
    border-top-color: var(--sinan-accent);
    top: 0;
    left: 0;
    opacity: 0.6;
  }

  .atom-spinner .spinner-line:nth-child(2) {
    width: 80%;
    height: 80%;
    top: 10%;
    left: 10%;
    border-top-color: rgba(var(--sinan-accent-rgb), 0.4);
    animation: atom-spin-1 1.2s linear infinite;
  }

  .atom-spinner .spinner-line:nth-child(3) {
    width: 60%;
    height: 60%;
    top: 20%;
    left: 20%;
    border-top-color: rgba(var(--sinan-accent-rgb), 0.2);
    animation: atom-spin-2 1.4s linear infinite reverse;
  }

  @keyframes atom-spin-1 {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }

  @keyframes atom-spin-2 {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(-360deg); }
  }

  .spinner-text {
    font-size: 12px;
    font-weight: 600;
    color: var(--sinan-accent);
    white-space: nowrap;
  }

  .sinan-legacy-nav, .sinan-legacy-scope, .legacy-nav, .legacy-scope { display:none !important; }
  .sinan-shell-error { margin-bottom: 12px; }
  "
}

.core_shell_js <- function(ns_client_ready = "__client_ready__") {
  sprintf("
  (function(){
    function ping(){
      try { Shiny.setInputValue('%s', Date.now(), {priority:'event'}); } catch(e){}
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', ping, {once:true});
    else ping();

    function norm(s){ return (s||'').toString().replace(/\\s+/g,' ').trim().toLowerCase(); }

    function hasLeaflet(el){
      try { return !!(el && el.querySelector && el.querySelector('.leaflet-container')); }
      catch(e){ return false; }
    }

    function removeNode(n){
      if(!n || n.nodeType !== 1) return;
      if(n.getAttribute && n.getAttribute('data-sinan-pruned') === '1') return;
      if(hasLeaflet(n)) return;
      try { n.setAttribute('data-sinan-pruned','1'); } catch(e){}
      try { n.remove(); } catch(e){
        try { n.style.display = 'none'; } catch(e2){}
      }
    }

    function findLegacyNavContainerFromButton(btn){
      if(!btn || !btn.closest) return null;

      var el = btn;
      for(var k=0; k<10; k++){
        if(!el || !el.parentElement) break;
        el = el.parentElement;

        if(hasLeaflet(el)) continue;

        var txt = norm(el.textContent || '');
        if(txt.indexOf('navegação:') === -1) continue;

        var btns = el.querySelectorAll('button, a, input[type=\"button\"], input[type=\"submit\"]');
        var hasVoltar = false, hasReset = false;
        for(var i=0;i<btns.length;i++){
          var btxt = norm(btns[i].textContent || btns[i].value || '');
          if(btxt === 'voltar') hasVoltar = true;
          if(btxt === 'reset')  hasReset  = true;
        }
        if(hasVoltar || hasReset) return el;
      }
      return null;
    }

    function pruneLegacyNavScopeGlobal(){
      var btns = document.querySelectorAll('button, a, input[type=\"button\"], input[type=\"submit\"]');
      btns.forEach(function(b){
        var btxt = norm(b.textContent || b.value || '');
        if(btxt !== 'voltar' && btxt !== 'reset') return;

        var c = findLegacyNavContainerFromButton(b);
        if(c) removeNode(c);
      });

      var nodes = document.querySelectorAll('div, p, small, span');
      nodes.forEach(function(el){
        if(!el || el.nodeType !== 1) return;
        if(hasLeaflet(el)) return;

        var txt = norm(el.textContent || '');
        if(!txt) return;
        if(!(txt.startsWith('escopo:'))) return;

        var c = el;
        for(var k=0;k<6;k++){
          if(!c || !c.parentElement) break;
          var p = c.parentElement;
          if(hasLeaflet(p)) break;
          var ptxt = norm(p.textContent || '');
          if(ptxt.indexOf('escopo:') !== -1 && ptxt.length <= 220) c = p;
          else break;
        }
        removeNode(c);
      });
    }

    function hideTrocaRapida(){
      var heads = document.querySelectorAll('.sinan-map-head');
      if(!heads || !heads.length) return;

      heads.forEach(function(head){
        var lbls = head.querySelectorAll('label');
        for (var i=0;i<lbls.length;i++){
          var t = norm(lbls[i].textContent);
          if (t === 'município (troca rápida)' || t === 'municipio (troca rapida)' || t === 'município (troca rapida)') {
            head.style.display = 'none';
            head.setAttribute('data-sinan-hidden', 'troca-rapida');
            break;
          }
        }
      });
    }

    function highlightLeafletArrow(){
      var bars = document.querySelectorAll('.leaflet-bar');
      bars.forEach(function(bar){
        var btns = bar.querySelectorAll('a, button');
        if (!btns || btns.length === 0) return;

        for (var i=0; i<btns.length; i++){
          var b = btns[i];
          if (!b || b.getAttribute('data-sinan-hi') === '1') continue;

          if (b.classList && (b.classList.contains('leaflet-control-zoom-in') || b.classList.contains('leaflet-control-zoom-out'))) continue;

          var title = norm(b.getAttribute('title') || '');
          var txt = norm(b.textContent || '');
          var html = norm(b.innerHTML || '');

          var looks =
            title.match(/(voltar|back|retornar|inicio|início|home|reset|recuar)/) ||
            txt.match(/(voltar|back|home|reset)/) ||
            html.indexOf('arrow') !== -1 || html.indexOf('chevron') !== -1 || html.indexOf('undo') !== -1 ||
            html.indexOf('↩') !== -1;

          if (looks || i === 0) {
            b.classList.add('sinan-map-arrow-highlight');
            b.setAttribute('data-sinan-hi', '1');
            break;
          }
        }
      });
    }

    try{
      if (window.Shiny && Shiny.addCustomMessageHandler) {
        Shiny.addCustomMessageHandler('sinan_set_accent', function(msg){
          try{
            var hex = (msg && msg.hex) ? String(msg.hex) : '';
            var rgb = (msg && msg.rgb) ? String(msg.rgb) : '';
            if(!hex) return;
            var root = document.documentElement;
            root.style.setProperty('--sinan-accent', hex);
            if(rgb) root.style.setProperty('--sinan-accent-rgb', rgb);
            window.requestAnimationFrame(function(){
              window.dispatchEvent(new Event('resize'));
            });
          }catch(e){}
        });

        Shiny.addCustomMessageHandler('toggleYearSpinner', function(msg){
          try{
            var spinner_id = (msg && msg.spinner_id) ? String(msg.spinner_id) : '';
            var show = (msg && msg.show) ? true : false;
            if(!spinner_id) return;
            var el = document.getElementById(spinner_id);
            if(!el) return;
            if(show) {
              el.style.display = '';
            } else {
              el.style.display = 'none';
            }
          }catch(e){}
        });

        // Esconder loading apenas na primeira vez que Shiny conecta
        var loadingShown = false;
        $(document).on('shiny:connected', function() {
          if (!loadingShown) {
            loadingShown = true;
            console.log('Shiny conectado - aguardando dados...');
            
            // Aguarda 3 segundos para dados carregarem, então remove o spinner
            setTimeout(function() {
              var overlay = document.getElementById('loading_overlay');
              if (overlay) {
                console.log('Removendo spinner após carregamento inicial');
                overlay.classList.remove('active');
              }
            }, 3000);
          }
        });
      }
    }catch(e){}

    var scheduled = false;
    function run(){
      if(scheduled) return;
      scheduled = true;
      window.requestAnimationFrame(function(){
        scheduled = false;
        pruneLegacyNavScopeGlobal();
        hideTrocaRapida();
        highlightLeafletArrow();
      });
    }

    run();
    document.addEventListener('shiny:connected', run);
    document.addEventListener('shown.bs.tab', function(){ setTimeout(run, 80); }, true);

    var obs = new MutationObserver(function(){ run(); });
    obs.observe(document.documentElement, { childList: true, subtree: true });
  })();
  ", ns_client_ready)
}

.core_pack_css <- function(pack = NULL) {
  if (!.core_is_tuberculose(pack = pack)) return("")

  "
  :root{
    --tb-blue-dark: #12346B;
    --tb-blue-primary: #1E73BE;
    --tb-blue-accent: #4A8EFF;
    --tb-orange: #DB400A;
    --tb-bg: #F4F4F4;
    --tb-surface: #FFFFFF;
    --tb-text-muted: #6C757D;
    --tb-border: #DEE2E6;
    --bs-primary: #1E73BE;
    --bs-primary-rgb: 30, 115, 190;
    --bs-danger: #DB400A;
    --bs-danger-rgb: 219, 64, 10;
    --bs-info: #4A8EFF;
    --bs-info-rgb: 74, 142, 255;
    --bs-secondary: #6C757D;
    --bs-secondary-rgb: 108, 117, 125;
    --bs-body-bg: #F4F4F4;
    --bs-body-color: #12346B;
    --bs-border-color: #DEE2E6;
    --sinan-accent: #1E73BE;
    --sinan-accent-rgb: 30, 115, 190;
  }

  html, body, .bslib-page-fill, .sinan-app { background: var(--tb-bg); }
  body { color: var(--tb-blue-dark); }

  .sinan-intro{
    background: var(--tb-surface);
    border-color: var(--tb-border);
    box-shadow: 0 10px 22px rgba(18, 52, 107, .10);
  }
  .sinan-intro-title { color: var(--tb-blue-dark); }

  .card, .bslib-card, .sinan-yearbox{
    background: var(--tb-surface);
    border-color: var(--tb-border);
  }
  .card-header, .bslib-card .card-header,
  .card-title, .bslib-card h1, .bslib-card h2, .bslib-card h3,
  .bslib-card h4, .bslib-card h5, .bslib-card h6,
  .sinan-yearbox-head { color: var(--tb-blue-dark); }

  .text-muted, .form-text, small, .small,
  .leaflet-control-attribution { color: var(--tb-text-muted) !important; }

  a, .nav-link { color: var(--tb-blue-primary); }
  a:hover, .nav-link:hover { color: var(--tb-blue-dark); }
  .nav-tabs .nav-link.active{
    color: var(--tb-blue-dark);
    border-color: var(--tb-border) var(--tb-border) var(--tb-surface);
  }

  .btn-primary, .action-button.btn-primary{
    color: var(--tb-surface);
    background-color: var(--tb-blue-primary);
    border-color: var(--tb-blue-primary);
  }
  .btn-primary:hover, .action-button.btn-primary:hover{
    background-color: var(--tb-blue-dark);
    border-color: var(--tb-blue-dark);
  }
  .btn-outline-primary{
    color: var(--tb-blue-primary);
    border-color: var(--tb-blue-primary);
  }
  .btn-outline-primary:hover{
    color: var(--tb-surface);
    background-color: var(--tb-blue-primary);
  }

  .form-control, .form-select, .selectize-input{
    color: var(--tb-blue-dark);
    background-color: var(--tb-surface);
    border-color: var(--tb-border);
  }
  .form-control:focus, .form-select:focus, .selectize-input.focus{
    border-color: var(--tb-blue-accent);
    box-shadow: 0 0 0 .2rem rgba(74, 142, 255, .22);
  }
  .selectize-dropdown .active{
    color: var(--tb-surface);
    background-color: var(--tb-blue-accent);
  }

  .alert-danger{
    color: var(--tb-orange);
    background: rgba(219, 64, 10, .08);
    border-color: var(--tb-orange);
  }
  .sinan-map-panel .leaflet-container{
    background: var(--tb-surface) !important;
  }
  .sinan-map-control, .sinan-map-busy .box,
  .sinan-backbtn, .sinan-legend-counts,
  .sinan-map-panel .info.legend{
    color: var(--tb-blue-dark);
    background: rgba(255, 255, 255, .96);
    border-color: var(--tb-border);
  }
  .sinan-map-toolbar .control-label,
  .sinan-map-toolbar .radio-inline{
    color: var(--tb-blue-dark);
  }
  .sinan-help-badge{
    color: var(--tb-surface);
    background: var(--tb-blue-primary);
    box-shadow: 0 4px 10px rgba(30, 115, 190, .30);
  }
  .sinan-hoverbox .box{
    color: var(--tb-surface);
    background: rgba(18, 52, 107, .97);
    border-color: var(--tb-blue-accent);
  }
  .sinan-help-tooltip .tooltip-inner{
    color: var(--tb-blue-dark) !important;
    background: var(--tb-surface) !important;
    border-color: var(--tb-border);
  }
  "
}

.core_shell_head <- function(js_ready_id, pack = NULL) {
  shiny::singleton(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(.core_shell_css())),
      shiny::tags$style(shiny::HTML(.core_pack_css(pack))),
      shiny::tags$script(shiny::HTML(.core_shell_js(js_ready_id)))
    )
  )
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------
core_dashboard_ui <- function(id, cfg, pack) {
  .core_patch_helpers()
  ns <- shiny::NS(id)

  title <- (pack$TITLE %||% pack$DOENCA) %||% cfg$DOENCA
  year_min <- .core_year_min(cfg)
  year_max <- .core_year_max(cfg)

  ui_shell_error <- shiny::uiOutput(ns("shell_error_ui"))
  ui_year <- shiny::uiOutput(ns("year_slider_ui"))
  ui_tb_grau <- shiny::uiOutput(ns("tb_grau_filter_ui"))
  ui_metric <- shiny::uiOutput(ns("metric_box_ui"))
  ui_intro <- shiny::uiOutput(ns("shell_intro_ui"))

  ui_state <- .core_try_ui({
    if (exists("mod_state_ui", mode = "function")) {
      .core_call_compat(mod_state_ui, id = ns("state"), cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div()
  }, title = "Falha ao renderizar: mod_state_ui")

  ui_kpis <- .core_try_ui({
    if (exists("mod_kpis_ui", mode = "function")) {
      .core_call_compat(mod_kpis_ui, id = ns("kpis"), cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div()
  }, title = "Falha ao renderizar: mod_kpis_ui")

  ui_map_raw <- .core_try_ui({
    if (exists("mod_map_ui", mode = "function")) {
      .core_call_compat(mod_map_ui, id = ns("map"), cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div("Mapa indisponível")
  }, title = "Falha ao renderizar: mod_map_ui")

  ui_map <- shiny::tags$div(class = "sinan-map-panel", ui_map_raw)

  ui_charts <- .core_try_ui({
    if (exists("mod_charts_ui", mode = "function")) {
      .core_call_compat(mod_charts_ui, id = ns("charts"), cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div("Gráficos indisponíveis")
  }, title = "Falha ao renderizar: mod_charts_ui")

  ui_evol_external <- NULL
  ui_comp_external <- shiny::uiOutput(paste0(ns("charts"), "-comp_external_ui"))
  
  shiny::tagList(
    .core_shell_head(ns("__client_ready__"), pack = pack),
    # Overlay de carregamento
    shiny::tags$div(
      id = ns("loading_overlay"),
      class = "sinan-loading-overlay",
      shiny::tags$div(class = "sinan-loading-spinner"),
      shiny::tags$div(class = "sinan-loading-text", "Carregando dados...")
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::tagList(
          shiny::tags$h4(title),
          shiny::tags$hr(),
          ui_shell_error,
          ui_year,
          ui_tb_grau,
          ui_metric,
          ui_state
        ),
        width = 380
      ),
      shiny::tags$div(
        class = "sinan-app",
        ui_intro,
        shiny::tags$div(class = "sinan-kpis", ui_kpis),
        shiny::tags$div(
          class = "sinan-mainrow",
          shiny::tags$div(class = "sinan-leftcol", ui_map),
          shiny::tags$div(class = "sinan-rightcol", ui_charts)
        ),
        shiny::tags$div(class = "sinan-bottomrow", shiny::tagList(ui_evol_external, ui_comp_external))
      )
    )
  )
}

core_dashboard_server <- function(id, cfg, pack) {
  shiny::moduleServer(id, function(input, output, session) {
    .core_patch_helpers()

    shell_error <- shiny::reactiveVal(NULL)

    output[["shell_error_ui"]] <- shiny::renderUI({
      msg <- shell_error()
      if (is.null(msg) || !nzchar(msg)) return(NULL)

      shiny::tags$div(
        class = "sinan-shell-error",
        .bs_alert(msg, color = "danger", title = "Erro")
      )
    })

    output[["shell_intro_ui"]] <- shiny::renderUI({
      src_flag <- .core_intro_asset_src(cfg, "Bandeira_de_Pernambuco.jpeg")
      src_logo <- .core_intro_asset_src(cfg, "cenarios_logo_full.jpeg")

      shiny::tags$div(
        class = "sinan-intro",
        shiny::tags$div(
          class = "sinan-intro-flagbox",
          if (!is.null(src_flag) && nzchar(src_flag)) shiny::tags$img(src = src_flag, class = "sinan-intro-flag", alt = "Bandeira de Pernambuco")
        ),
        shiny::tags$h2(class = "sinan-intro-title", .core_intro_title(pack)),
        shiny::tags$div(
          class = "sinan-intro-brand",
          if (!is.null(src_logo) && nzchar(src_logo)) shiny::tags$img(src = src_logo, class = "sinan-intro-logo", alt = "Logo da rede")
        )
      )
    })


    .core_try_server(.core_req_fun("mod_data_init"), on_error = function(e) {
      shell_error(paste0("Shell: mod_data_init não está disponível. ", conditionMessage(e)))
    })
    if (.core_rv_has_now(shell_error)) return(invisible(NULL))

    .core_try_server({
      year_min_cfg <- .core_year_min(cfg)
      year_max_cfg <- .core_year_max(cfg)

      data_api <- mod_data_init(cfg, pack)

      yrs <- .core_discover_years_available(cfg, pack, data_api = data_api)
      yrs <- yrs[is.finite(yrs) & yrs >= year_min_cfg & yrs <= year_max_cfg]
      year_min_eff <- if (length(yrs) > 0) min(yrs) else year_min_cfg
      year_max_eff <- if (length(yrs) > 0) max(yrs) else year_max_cfg
      yrs_eff <- if (length(yrs) > 0) sort(unique(yrs)) else seq.int(year_min_eff, year_max_eff)

      nav <- .core_nav_init(year_max = year_max_eff)
      lock_recife <- isTRUE(cfg$LOCK_RECIFE %||% cfg$lock_recife %||% FALSE)
      lock_mun7 <- .to_mun7(cfg$RECIFE_MUN7 %||% cfg$recife_mun7 %||% "2611606")
      lock_uf <- .to_uf(cfg$RECIFE_UF %||% cfg$recife_uf %||% "PE")
      nav$lock_recife <- lock_recife
      nav$lock_mun7 <- lock_mun7
      nav$lock_uf <- lock_uf
      nav$years_available <- yrs_eff
      nav$year_min <- as.integer(year_min_eff)
      nav$year_max <- as.integer(year_max_eff)
      nav$ano <- as.integer(year_max_eff)

      if (isTRUE(lock_recife)) {
        nav$level <- "MUN"
        nav$uf <- lock_uf
        nav$mun <- lock_mun7
        nav$detail <- FALSE
        nav$region_view <- "MUN"
        nav$selected_macro <- NULL
        nav$selected_micro <- NULL
      }

      .core_year_slider_bind(
        input = input, output = output, session = session,
        nav = nav,
        year_min_fallback = year_min_eff,
        year_max_fallback = year_max_eff,
        output_id = "year_slider_ui",
        input_id  = "year_slider",
        label = "Ano",
        debounce_ms = 350L
      )

      .core_metric_box_bind(
        output = output,
        nav = nav,
        output_id = "metric_box_ui"
      )

      # Debounce reativo para evitar múltiplos renders quando usuário arrasta slider
      year_debounced <- shiny::debounce(
        shiny::reactive({
          y <- .core_unwrap(nav$ano) %||% year_max_eff
          y <- suppressWarnings(as.integer(y))
          if (!is.finite(y)) y <- year_max_eff
          y
        }),
        millis = 350L
      )

      ano <- shiny::reactive({
        y <- year_debounced()
        yrs2 <- suppressWarnings(as.integer(.core_unwrap(nav$years_available)))
        yrs2 <- yrs2[is.finite(yrs2)]
        if (length(yrs2) > 0 && !(y %in% yrs2)) y <- max(yrs2)
        y
      })

      metric <- .core_metric_bind(input = input, session = session, nav = nav, pack = pack)

      .core_tb_grau_filter_bind(
        input = input, output = output, session = session,
        nav = nav, cfg = cfg, pack = pack, data_api = data_api, ano = ano,
        output_id = "tb_grau_filter_ui",
        input_id = "tb_grau_filter"
      )

      if (exists("mod_state_server", mode = "function")) {
        .core_try_server(.core_call_compat(
          mod_state_server,
          id = "state",
          cfg = cfg,
          pack = pack,
          nav = nav,
          data_api = data_api,
          ano = ano,
          metric = metric,
          year_min = year_min_eff,
          year_max = year_max_eff,
          ano_min = year_min_eff,
          ano_max = year_max_eff
        ), on_error = function(e) shell_error(paste0("mod_state_server: ", conditionMessage(e))))
      }

      if (exists("mod_map_server", mode = "function")) {
        .core_try_server(.core_call_compat(
          mod_map_server,
          id = "map",
          cfg = cfg,
          pack = pack,
          nav = nav,
          state = nav,
          data_api = data_api,
          ano = ano,
          metric = metric
        ), on_error = function(e) shell_error(paste0("mod_map_server: ", conditionMessage(e))))
      }

      if (exists("mod_kpis_server", mode = "function")) {
        .core_try_server(.core_call_compat(
          mod_kpis_server,
          id = "kpis",
          cfg = cfg,
          pack = pack,
          nav = nav,
          state = nav,
          ano = ano,
          metric = metric,
          data_api = data_api
        ), on_error = function(e) shell_error(paste0("mod_kpis_server: ", conditionMessage(e))))
      }

      if (exists("mod_charts_server", mode = "function")) {
        .core_try_server(.core_call_compat(
          mod_charts_server,
          id = "charts",
          cfg = cfg,
          pack = pack,
          nav = nav,
          state = nav,
          ano = ano,
          metric = metric,
          data_api = data_api
        ), on_error = function(e) shell_error(paste0("mod_charts_server: ", conditionMessage(e))))
      }

      invisible(NULL)
    }, on_error = function(e) {
      shell_error(paste0("Shell: erro ao inicializar navegação/dados. ", conditionMessage(e)))
    })

    invisible(NULL)
  })
}

# ---------------------------------------------------------------------
# UI/Server 
# ---------------------------------------------------------------------
core_build_ui <- function(cfg, pack) {
  .core_patch_helpers()

  year_min <- .core_year_min(cfg)
  year_max <- .core_year_max(cfg)

  ui_shell_error <- shiny::uiOutput("shell_error_ui")
  ui_year <- shiny::uiOutput("year_slider_ui")
  ui_tb_grau <- shiny::uiOutput("tb_grau_filter_ui")
  ui_metric <- shiny::uiOutput("metric_box_ui")
  ui_intro <- shiny::uiOutput("shell_intro_ui")

  ui_state <- .core_try_ui({
    if (exists("mod_state_ui", mode = "function")) {
      .core_call_compat(mod_state_ui, id = "state", cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div()
  }, title = "Falha ao renderizar: mod_state_ui")

  ui_kpis <- .core_try_ui({
    if (exists("mod_kpis_ui", mode = "function")) {
      .core_call_compat(mod_kpis_ui, id = "kpis", cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div()
  }, title = "Falha ao renderizar: mod_kpis_ui")

  ui_map_raw <- .core_try_ui({
    if (exists("mod_map_ui", mode = "function")) {
      .core_call_compat(mod_map_ui, id = "map", cfg = cfg, pack = pack,
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div("Mapa indisponível")
  }, title = "Falha ao renderizar: mod_map_ui")

  ui_map <- shiny::tags$div(class = "sinan-map-panel", ui_map_raw)

  ui_charts_main <- .core_try_ui({
    if (exists("mod_charts_ui", mode = "function")) {
      .core_call_compat(mod_charts_ui, id = "charts_main", cfg = cfg, pack = pack,
                        which = "main", title = "Gráficos",
                        year_min = year_min, year_max = year_max, ano_min = year_min, ano_max = year_max)
    } else shiny::tags$div("Gráficos indisponíveis")
  }, title = "Falha ao renderizar: mod_charts_ui")

  ui_evol_external_main <- shiny::uiOutput("charts_main-evol_external_ui")
  ui_comp_external_main <- shiny::uiOutput("charts_main-comp_external_ui")

  src_favicon <- .core_intro_asset_src(cfg, "favicon.jpg")

  bslib::page_fillable(
    title = pack$TITLE %||% "SINAN",
    .core_shell_head("__client_ready__", pack = pack),
    shiny::tags$head(
      shiny::tags$script(shiny::HTML("
        document.addEventListener('shown.bs.tab', function () {
          window.dispatchEvent(new Event('resize'));
        }, true);
      ")),
      if (!is.null(src_favicon) && nzchar(src_favicon)) {
        shiny::tags$link(rel = "icon", type = "image/jpeg", href = src_favicon)
      }
    ),
    # Overlay de carregamento
    shiny::tags$div(
      id = "loading_overlay",
      class = "sinan-loading-overlay active",
      shiny::tags$div(class = "sinan-loading-spinner"),
      shiny::tags$div(class = "sinan-loading-text", "Carregando dados...")
    ),
    shiny::tags$div(
      class = "sinan-app",
      ui_shell_error,
      ui_intro,
      shiny::tags$div(class = "sinan-kpis", ui_kpis),
      shiny::tags$div(
        class = "sinan-mainrow",
        shiny::tags$div(class = "sinan-leftcol", shiny::tagList(ui_year, ui_tb_grau, ui_metric, ui_state, ui_map)),
        shiny::tags$div(class = "sinan-rightcol", ui_charts_main)
      ),
      shiny::tags$div(class = "sinan-bottomrow", shiny::tagList(ui_evol_external_main, ui_comp_external_main))
    )
  )
}

core_build_server <- function(cfg, pack) {
  force(cfg); force(pack)
  function(input, output, session) {
    .core_patch_helpers()

    shell_error <- shiny::reactiveVal(NULL)
    output[["shell_error_ui"]] <- shiny::renderUI({
      msg <- shell_error()
      if (is.null(msg) || !nzchar(msg)) return(NULL)
      shiny::tags$div(class = "sinan-shell-error",
                     .bs_alert(color = "danger", title = "Erro"))

    })

    output[["shell_intro_ui"]] <- shiny::renderUI({
      src_flag <- .core_intro_asset_src(cfg, "Bandeira_de_Pernambuco.jpeg")
      src_logo <- .core_intro_asset_src(cfg, "cenarios_logo_full.jpeg")

      shiny::tags$div(
        class = "sinan-intro",
        shiny::tags$div(
          class = "sinan-intro-flagbox",
          if (!is.null(src_flag) && nzchar(src_flag)) shiny::tags$img(src = src_flag, class = "sinan-intro-flag", alt = "Bandeira de Pernambuco")
        ),
        shiny::tags$h2(class = "sinan-intro-title", .core_intro_title(pack)),
        shiny::tags$div(
          class = "sinan-intro-brand",
          if (!is.null(src_logo) && nzchar(src_logo)) shiny::tags$img(src = src_logo, class = "sinan-intro-logo", alt = "Logo da rede")
        )
      )
    })

    .core_try_server(.core_req_fun("mod_data_init"), on_error = function(e) {
      shell_error(paste0("Shell: mod_data_init não está disponível. ", conditionMessage(e)))
    })
    .core_try_server(.core_req_fun("mod_kpis_server"),   on_error = function(e) shell_error(paste0("Shell: mod_kpis_server ausente. ", conditionMessage(e))))
    .core_try_server(.core_req_fun("mod_map_server"),    on_error = function(e) shell_error(paste0("Shell: mod_map_server ausente. ", conditionMessage(e))))
    .core_try_server(.core_req_fun("mod_charts_server"), on_error = function(e) shell_error(paste0("Shell: mod_charts_server ausente. ", conditionMessage(e))))
    if (exists("mod_state_server", mode = "function", inherits = TRUE)) {
      .core_try_server(.core_req_fun("mod_state_server"), on_error = function(e) shell_error(paste0("Shell: mod_state_server ausente. ", conditionMessage(e))))
    }

    if (.core_rv_has_now(shell_error)) return(invisible(NULL))

    .core_try_server({
      year_min_cfg <- .core_year_min(cfg)
      year_max_cfg <- .core_year_max(cfg)

      data_api <- mod_data_init(cfg, pack)

      yrs <- .core_discover_years_available(cfg, pack, data_api = data_api)
      yrs <- yrs[is.finite(yrs) & yrs >= year_min_cfg & yrs <= year_max_cfg]
      year_min_eff <- if (length(yrs) > 0) min(yrs) else year_min_cfg
      year_max_eff <- if (length(yrs) > 0) max(yrs) else year_max_cfg
      yrs_eff <- if (length(yrs) > 0) sort(unique(yrs)) else seq.int(year_min_eff, year_max_eff)

      nav <- .core_nav_init(year_max = year_max_eff)
      nav$years_available <- yrs_eff
      nav$year_min <- as.integer(year_min_eff)
      nav$year_max <- as.integer(year_max_eff)
      nav$ano <- as.integer(year_max_eff)

      .core_year_slider_bind(
        input = input, output = output, session = session,
        nav = nav,
        year_min_fallback = year_min_eff,
        year_max_fallback = year_max_eff,
        output_id = "year_slider_ui",
        input_id  = "year_slider",
        label = "Ano"
      )

      .core_metric_box_bind(
        output = output,
        nav = nav,
        output_id = "metric_box_ui"
      )

      ano <- shiny::reactive({
        y <- .core_unwrap(nav$ano) %||% year_max_eff
        y <- suppressWarnings(as.integer(y))
        if (!is.finite(y)) y <- year_max_eff
        yrs2 <- suppressWarnings(as.integer(.core_unwrap(nav$years_available)))
        yrs2 <- yrs2[is.finite(yrs2)]
        if (length(yrs2) > 0 && !(y %in% yrs2)) y <- max(yrs2)
        y
      })

      metric <- .core_metric_bind(input = input, session = session, nav = nav, pack = pack)

      .core_tb_grau_filter_bind(
        input = input, output = output, session = session,
        nav = nav, cfg = cfg, pack = pack, data_api = data_api, ano = ano,
        output_id = "tb_grau_filter_ui",
        input_id = "tb_grau_filter"
      )

      if (exists("mod_state_server", mode = "function")) {
        .core_try_server(.core_call_compat(
          mod_state_server,
          id = "state",
          cfg = cfg,
          pack = pack,
          nav = nav,
          year_min = year_min_eff,
          year_max = year_max_eff,
          ano_min = year_min_eff,
          ano_max = year_max_eff,
          ano = ano,
          metric = metric,
          data_api = data_api
        ), on_error = function(e) shell_error(paste0("mod_state_server: ", conditionMessage(e))))
      }

      .core_try_server(.core_call_compat(mod_map_server,
        id = "map", cfg = cfg, pack = pack, state = nav, nav = nav, ano = ano, metric = metric, data_api = data_api
      ), on_error = function(e) shell_error(paste0("mod_map_server: ", conditionMessage(e))))

      .core_try_server(.core_call_compat(mod_kpis_server,
        id = "kpis", cfg = cfg, pack = pack, state = nav, nav = nav, ano = ano, metric = metric, data_api = data_api
      ), on_error = function(e) shell_error(paste0("mod_kpis_server: ", conditionMessage(e))))

      .core_try_server(.core_call_compat(mod_charts_server,
        id = "charts_main", cfg = cfg, pack = pack, state = nav, nav = nav, ano = ano, metric = metric,
        data_api = data_api, which = "main"
      ), on_error = function(e) shell_error(paste0("mod_charts_server: ", conditionMessage(e))))

      # ---------------------------------------------------------------
      # Controle de indicador de carregamento
      # ---------------------------------------------------------------
      # Spinner aparece por padrão e é removido após 3s pelo JavaScript
      # (Apenas na primeira vez que o dashboard abre)



      invisible(NULL)
    }, on_error = function(e) {
      shell_error(paste0("Shell: erro ao inicializar (single-app). ", conditionMessage(e)))
    })

    invisible(NULL)
  }
}
