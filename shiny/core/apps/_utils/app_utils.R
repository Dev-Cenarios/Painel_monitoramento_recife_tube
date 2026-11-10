# core/apps/utils/app_utils.R
# ------------------------------------------------------------------------------
# Helpers para inicializar qualquer app em core/apps/<DOENCA>/app.R
# Corrigido: source() do core agora ocorre em um environment persistente (CORE_ENV)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

.app_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

# Environment persistente para manter funções do core (core_get_config, core_build_ui, etc.)
.core_env_get <- function() {
  if (!exists(".CORE_ENV", envir = globalenv(), inherits = FALSE)) {
    assign(".CORE_ENV", new.env(parent = globalenv()), envir = globalenv())
  }
  get(".CORE_ENV", envir = globalenv(), inherits = FALSE)
}

app_paths <- function(app_dir = NULL) {
  APP_DIR  <- .app_norm(app_dir %||% getwd())
  CORE_DIR <- .app_norm(file.path(APP_DIR, "..", ".."))
  SHINY_DIR <- .app_norm(file.path(CORE_DIR, ".."))

  list(
    APP_DIR  = APP_DIR,
    CORE_DIR = CORE_DIR,
    SHINY_DIR = SHINY_DIR,
    APP_NAME = toupper(basename(APP_DIR))
  )
}

app_infer_disease <- function(app_name) {
  nm <- toupper(trimws(app_name %||% ""))

  if (nm %in% c("TUBERCULOSE","TB","TUB")) return(list(DOENCA = "TUBERCULOSE", DISEASE_DIR = "TUBERCULOSE"))

  list(DOENCA = nm, DISEASE_DIR = nm)
}

app_source_core <- function(paths) {
  CORE_DIR <- paths$CORE_DIR
  CORE_ENV <- .core_env_get()

  # Core base (persistente)
  source(file.path(CORE_DIR, "R", "utils.R"),     local = CORE_ENV, encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "config.R"),    local = CORE_ENV, encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "app_shell.R"), local = CORE_ENV, encoding = "UTF-8")

  # Módulos no globalenv (como você já fazia)
  source(file.path(CORE_DIR, "R", "modules", "mod_state.R"),  local = globalenv(), encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "modules", "mod_data.R"),   local = globalenv(), encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "modules", "mod_kpis.R"),   local = globalenv(), encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "modules", "mod_map.R"),    local = globalenv(), encoding = "UTF-8")
  source(file.path(CORE_DIR, "R", "modules", "mod_charts.R"), local = globalenv(), encoding = "UTF-8")

  # Performance utils - cache compartilhado e otimizações reativas
  perf_utils <- file.path(CORE_DIR, "R", "performance_utils.R")
  if (file.exists(perf_utils)) {
    source(perf_utils, local = globalenv(), encoding = "UTF-8")
  }

  invisible(TRUE)
}

app_load_pack <- function(paths, disease_dir) {
  CORE_DIR <- paths$CORE_DIR
  pack_path <- file.path(CORE_DIR, "diseases", disease_dir, "disease_pack.R")

  if (!file.exists(pack_path)) {
    stop("disease_pack.R não encontrado em: ", pack_path, call. = FALSE)
  }

  # Carrega o pack no env atual (persistente não é necessário aqui)
  source(pack_path, local = TRUE, encoding = "UTF-8")

  if (!exists("get_disease_pack", mode = "function", inherits = TRUE)) {
    stop("get_disease_pack() não foi definido por: ", pack_path, call. = FALSE)
  }

  pack0 <- get_disease_pack()
  pack  <- if (is.function(pack0)) pack0() else pack0

  if (!is.list(pack)) {
    stop("get_disease_pack() deve retornar list(). Arquivo: ", pack_path, call. = FALSE)
  }

  pack
}

run_core_app <- function(app_dir = NULL, year_min = 2010L, year_max = 2025L,
                         doenca = NULL, disease_dir = NULL) {
  paths <- app_paths(app_dir)
  inf <- app_infer_disease(paths$APP_NAME)

  doenca      <- toupper(trimws(as.character(doenca %||% inf$DOENCA)))
  disease_dir <- toupper(trimws(as.character(disease_dir %||% inf$DISEASE_DIR)))

  app_source_core(paths)
  pack <- app_load_pack(paths, disease_dir)

  # se pack define DOENCA, ele manda
  doenca_pack <- toupper(as.character(pack$DOENCA %||% pack$doenca %||% doenca))
  if (nzchar(doenca_pack)) doenca <- doenca_pack

  CORE_ENV <- .core_env_get()

  # valida presença das funções core
  if (!exists("core_get_config", envir = CORE_ENV, inherits = FALSE)) {
    stop("core_get_config() não foi carregada. Verifique core/R/config.R.", call. = FALSE)
  }
  if (!exists("core_build_ui", envir = CORE_ENV, inherits = FALSE)) {
    stop("core_build_ui() não foi carregada. Verifique core/R/app_shell.R.", call. = FALSE)
  }
  if (!exists("core_build_server", envir = CORE_ENV, inherits = FALSE)) {
    stop("core_build_server() não foi carregada. Verifique core/R/app_shell.R.", call. = FALSE)
  }

  cfg <- CORE_ENV$core_get_config(
    app_dir  = paths$APP_DIR,
    doenca   = doenca,
    year_min = as.integer(year_min),
    year_max = as.integer(year_max)
  )

  ui <- CORE_ENV$core_build_ui(cfg, pack)
  server <- CORE_ENV$core_build_server(cfg, pack)

  shinyApp(ui, server)
}
