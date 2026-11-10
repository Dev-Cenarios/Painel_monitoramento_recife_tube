# dashs/shiny/core/R/modules/mod_kpis.R

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(bsicons)
})

if (!exists(".SINAN_UTILS_LOADED", inherits = TRUE) || !isTRUE(get(".SINAN_UTILS_LOADED", inherits = TRUE))) {
  if (exists(".source_utils_if_needed", mode = "function", inherits = TRUE)) {
    .source_utils_if_needed()
  } else {
    p <- normalizePath(tryCatch(getwd(), error = function(e) "."), winslash = "/", mustWork = FALSE)
    for (i in 0:12) {
      cand <- file.path(p, "dashs", "shiny", "core", "R", "utils.R")
      if (file.exists(cand)) { source(cand, local = FALSE); break }
      p2 <- dirname(p)
      if (identical(p2, p)) break
      p <- p2
    }
  }
}

# ----------------------------------------------------------------------
# Falbacks
# ----------------------------------------------------------------------
if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}
if (!exists(".res", mode = "function", inherits = TRUE)) {
  .res <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.function(x)) return(tryCatch(x(), error = function(e) NULL))
    x
  }
}
if (!exists(".obj_get", mode = "function", inherits = TRUE)) {
  .obj_get <- function(obj, key, default = NULL) {
    obj <- .res(obj)
    if (is.null(obj)) return(default)
    if (inherits(obj, "reactivevalues")) return(tryCatch(obj[[key]] %||% default, error = function(e) default))
    if (is.list(obj)) return(obj[[key]] %||% default)
    if (is.environment(obj) && exists(key, envir = obj, inherits = FALSE)) return(get(key, envir = obj, inherits = FALSE) %||% default)
    default
  }
}
if (!exists(".as_chr1", mode = "function", inherits = TRUE)) {
  .as_chr1 <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0) return(default)
    out <- as.character(x[[1]])
    if (is.na(out) || !nzchar(out)) default else out
  }
}
if (!exists(".to_uf", mode = "function", inherits = TRUE)) {
  .to_uf <- function(x) {
    if (is.null(x)) return(NA_character_)
    x <- toupper(trimws(as.character(x)))
    x <- ifelse(nzchar(x), x, NA_character_)
    x
  }
}
if (!exists(".to_mun7", mode = "function", inherits = TRUE)) {
  .to_mun7 <- function(x) {
    if (is.null(x)) return(NA_character_)
    s <- gsub("\\D+", "", as.character(x))
    s <- ifelse(nzchar(s), s, NA_character_)
    short <- !is.na(s) & nchar(s) < 7
    if (any(short)) s[short] <- sprintf("%07s", s[short])
    s <- gsub(" ", "0", s)
    long <- !is.na(s) & nchar(s) > 7
    if (any(long)) s[long] <- substr(s[long], 1, 7)
    s
  }
}

if (!exists(".sinan_log_open", mode = "function", inherits = TRUE))  .sinan_log_open  <- function(...) invisible(NULL)
if (!exists(".sinan_log_write", mode = "function", inherits = TRUE)) .sinan_log_write <- function(...) invisible(NULL)

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------
.scalar_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  out <- as.character(x)[1]
  if (is.na(out) || !nzchar(out)) default else out
}
.scalar_int <- function(x, default = 0L) {
  if (is.null(x) || length(x) == 0) return(default)
  out <- suppressWarnings(as.integer(x)[1])
  if (!isTRUE(is.finite(out))) default else out
}
.kpi_norm_level <- function(x) {
  lvl <- toupper(.scalar_chr(x, "UF"))
  if (!lvl %in% c("BR", "UF", "MUN")) "UF" else lvl
}
.kpi_fmt_int <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0 || !isTRUE(is.finite(v[1]))) return("0")
  formatC(round(v[1]), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
}
.kpi_fmt_num <- function(x, digits = 2) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0 || !isTRUE(is.finite(v[1]))) return("NA")
  formatC(round(v[1], digits), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

.col_pick <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  nm <- names(df)
  for (c in candidates) if (c %in% nm) return(c)
  up <- toupper(nm)
  for (c in candidates) {
    i <- match(toupper(c), up)
    if (!is.na(i)) return(nm[[i]])
  }
  NULL
}

# Helper para normalizar nomes de colunas (remove acentos, case-insensitive)
.norm_str <- function(s) {
  if (!is.character(s)) return(s)
  # Remove acentos
  s <- iconv(s, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Uppercase
  toupper(s)
}

.col_pick_norm <- function(nms, candidates) {
  norm_nms <- .norm_str(nms)
  for (c in candidates) {
    cc <- .norm_str(c)
    i <- match(cc, norm_nms)
    if (!is.na(i)) return(nms[[i]])
  }
  NULL
}

.kpi_recife_bairro_key <- function(x) {
  x <- as.character(x %||% "")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x[x == "BAIRRO DO RECIFE"] <- "RECIFE"
  x[x == "POCO DA PANELA"] <- "POCO"
  ifelse(nzchar(x), x, NA_character_)
}

.kpi_find_dashboard_dir <- function(cfg = NULL) {
  cfg <- .res(cfg)
  dash <- .obj_get(cfg, "DASHBOARD_DIR", NULL) %||% .obj_get(cfg, "dashboard_dir", NULL)
  if (is.character(dash) && length(dash) > 0 && nzchar(dash[[1]]) && dir.exists(dash[[1]])) {
    return(normalizePath(dash[[1]], winslash = "/", mustWork = FALSE))
  }
  root <- .obj_get(cfg, "ROOT", NULL) %||% .obj_get(cfg, "root", NULL)
  if (is.character(root) && length(root) > 0 && nzchar(root[[1]])) {
    cand <- file.path(root[[1]], "data", "parquet")
    if (dir.exists(cand)) return(normalizePath(cand, winslash = "/", mustWork = FALSE))
  }
  wd <- tryCatch(getwd(), error = function(e) ".")
  cur <- normalizePath(wd, winslash = "/", mustWork = FALSE)
  for (i in 0:12) {
    cand <- file.path(cur, "data", "parquet")
    if (dir.exists(cand)) return(normalizePath(cand, winslash = "/", mustWork = FALSE))
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  NULL
}

.kpi_num_br <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

.kpi_read_recife_bairro_incidence <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL) {
    dash <- .kpi_find_dashboard_dir(cfg)
    if (is.null(dash) || !nzchar(dash)) return(data.frame())

    dir <- file.path(dash, "incidence", "doenca=TUBERCULOSE", "nivel=BAIRRO")
    if (!dir.exists(dir)) return(data.frame())

    files <- list.files(dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
    if (length(files) == 0) return(data.frame())

    mt <- suppressWarnings(max(as.numeric(file.info(files)$mtime), na.rm = TRUE))
    if (!is.finite(mt)) mt <- 0
    key <- paste0(dir, "|", length(files), "|", mt)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))

    parts <- lapply(files, function(fp) tryCatch(as.data.frame(arrow::read_parquet(fp)), error = function(e) NULL))
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0) return(data.frame())

    out <- do.call(rbind, parts)
    if (is.null(out) || nrow(out) == 0 || !"bairro_key" %in% names(out)) return(data.frame())

    assign(key, out, envir = cache)
    out
  }
})

.kpi_read_data_freshness <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL, doenca = "TUBERCULOSE") {
    dash <- .kpi_find_dashboard_dir(cfg)
    if (is.null(dash) || !nzchar(dash)) return(NULL)

    fp <- file.path(dash, "meta", paste0("doenca=", doenca), "freshness.parquet")
    if (!file.exists(fp)) return(NULL)

    mt <- suppressWarnings(as.numeric(file.info(fp)$mtime))
    if (!is.finite(mt)) mt <- 0
    key <- paste0(fp, "|", mt)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))

    out <- tryCatch(as.data.frame(arrow::read_parquet(fp)), error = function(e) NULL)
    if (!is.data.frame(out) || nrow(out) == 0) return(NULL)

    assign(key, out, envir = cache)
    out
  }
})

.kpi_tb_age_years <- function(x) {
  d <- suppressWarnings(as.integer(gsub("\\D+", "", trimws(as.character(x %||% "")))))
  out <- rep(NA_real_, length(d))
  ok4 <- is.finite(d) & d >= 4000 & d <= 4999
  out[ok4] <- d[ok4] - 4000
  ok_plain <- is.finite(d) & d >= 0 & d < 130 & !ok4
  out[ok_plain] <- d[ok_plain]
  out
}
.std_uf_metrics <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  c_uf    <- .col_pick(df, c("uf","UF","sigla_uf","SIGLA_UF","sg_uf","SG_UF"))
  if (is.null(c_uf)) return(NULL)

  c_cases <- .col_pick(df, c("casos_total","cases_total","casos","TOTAL_CASOS","qt_casos"))
  c_obt   <- .col_pick(df, c("casos_obitos","obitos_total","obitos","TOTAL_OBITOS","qt_obitos"))
  c_cura  <- .col_pick(df, c("casos_cura","cura_total","curas_total","curas","TOTAL_CURAS"))
  c_pop   <- .col_pick(df, c("pop_total","POP_TOTAL","pop"))
  c_incid <- .col_pick(df, c("incid_100k_total","incidencia_100k_total","incid_100k","INCID_100K_TOTAL","incid"))

  out <- data.frame(
    uf = .to_uf(df[[c_uf]]),
    casos_total = if (!is.null(c_cases)) suppressWarnings(as.numeric(df[[c_cases]])) else NA_real_,
    casos_obitos = if (!is.null(c_obt))  suppressWarnings(as.numeric(df[[c_obt]]))  else NA_real_,
    casos_cura   = if (!is.null(c_cura)) suppressWarnings(as.numeric(df[[c_cura]])) else NA_real_,
    pop_total    = if (!is.null(c_pop))  suppressWarnings(as.numeric(df[[c_pop]]))  else NA_real_,
    incid_100k_total = if (!is.null(c_incid)) suppressWarnings(as.numeric(df[[c_incid]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$uf) & nzchar(out$uf), , drop = FALSE]
  out
}

.std_mun_metrics <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  c_id   <- .col_pick(df, c("cod_mun7","COD_MUN7","geo_id","GEO_ID","CD_MUN","cd_mun","mun","MUN","cd_ibge","CD_IBGE"))
  if (is.null(c_id)) return(NULL)

  c_uf   <- .col_pick(df, c("uf","UF","sigla_uf","SIGLA_UF","sg_uf","SG_UF"))
  c_cases <- .col_pick(df, c("casos_total","cases_total","casos","TOTAL_CASOS","qt_casos"))
  c_obt   <- .col_pick(df, c("casos_obitos","obitos_total","obitos","TOTAL_OBITOS","qt_obitos"))
  c_cura  <- .col_pick(df, c("casos_cura","cura_total","curas_total","curas","TOTAL_CURAS"))
  c_pop   <- .col_pick(df, c("pop_total","POP_TOTAL","pop"))
  c_incid <- .col_pick(df, c("incid_100k_total","incidencia_100k_total","incid_100k","INCID_100K_TOTAL","incid"))

  out <- data.frame(
    cod_mun7 = .to_mun7(df[[c_id]]),
    uf = if (!is.null(c_uf)) .to_uf(df[[c_uf]]) else NA_character_,
    casos_total = if (!is.null(c_cases)) suppressWarnings(as.numeric(df[[c_cases]])) else NA_real_,
    casos_obitos = if (!is.null(c_obt))  suppressWarnings(as.numeric(df[[c_obt]]))  else NA_real_,
    casos_cura   = if (!is.null(c_cura)) suppressWarnings(as.numeric(df[[c_cura]])) else NA_real_,
    pop_total    = if (!is.null(c_pop))  suppressWarnings(as.numeric(df[[c_pop]]))  else NA_real_,
    incid_100k_total = if (!is.null(c_incid)) suppressWarnings(as.numeric(df[[c_incid]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$cod_mun7) & nzchar(out$cod_mun7), , drop = FALSE]
  out
}


.kpi_metric_key_norm <- function(x) {
  x <- tolower(trimws(as.character(x %||% "cases")))
  if (length(x) == 0) return("cases")
  x <- x[[1]]
  if (is.na(x) || !nzchar(x)) return("cases")
  if (x %in% c("case", "cases", "casos")) return("cases")
  if (x %in% c("incid", "incidence", "incidencia", "incidência")) return("incid")
  if (x %in% c("obito", "obitos", "óbito", "óbitos", "deaths")) return("obitos")
  if (x %in% c("cura", "curas", "cure")) return("cura")
  if (x %in% c("pop", "populacao", "população", "population")) return("pop")
  "cases"
}

# ----------------------------------------------------------------------
# CSS
# ----------------------------------------------------------------------
.kpis_css <- function() {
  singleton(tags$style(HTML("
    .kpi-wrap{width:100%; display:block;}
    .kpi-grid{
      width:100%;
      display:grid;
      grid-template-columns:repeat(auto-fit, minmax(180px, 1fr));
      gap:.75rem;
      align-items:stretch;
    }
    @media (max-width: 1240px){
      .kpi-grid{ grid-template-columns:repeat(auto-fit, minmax(210px, 1fr)); }
    }
    @media (max-width: 860px){
      .kpi-grid{ grid-template-columns:repeat(auto-fit, minmax(200px, 1fr)); }
    }
    @media (max-width: 460px){
      .kpi-grid{ grid-template-columns:1fr; }
    }

    .kpi-card{
      --kpi-accent:#0F766E;
      --rx:0deg;--ry:0deg;--tz:0px;
      border-radius:18px;
      border:1px solid rgba(15,23,42,.10);
      background:linear-gradient(180deg, rgba(255,255,255,.97), rgba(255,255,255,.88));
      box-shadow:0 10px 26px rgba(2,6,23,.08);
      overflow:hidden;
      position:relative;
      transform:translateZ(var(--tz)) rotateX(var(--rx)) rotateY(var(--ry));
      transform-origin:center;
      transition:transform .14s ease, box-shadow .14s ease, border-color .14s ease;
      will-change:transform;
      backface-visibility:hidden;
      touch-action:manipulation;
    }
    .kpi-card::after{
      content:'';
      position:absolute; inset:-1px;
      border-radius:18px;
      opacity:0;
      pointer-events:none;
      transition:opacity .14s ease;
      background:
        radial-gradient(220px 120px at 15% 10%, color-mix(in srgb, var(--kpi-accent) 22%, transparent), transparent 65%),
        radial-gradient(220px 120px at 85% 0%, rgba(255,255,255,.35), transparent 55%);
    }

    .kpi-inner{
      display:flex; gap:12px; align-items:center;
      padding:12px 12px;
      position:relative; z-index:1;
      min-width:0;
    }
    .kpi-accent{width:9px;height:46px;border-radius:999px;background:var(--kpi-accent);flex:0 0 auto;}
    .kpi-text{min-width:0;}
    .kpi-title{
      font-size:13px;
      font-weight:700;
      opacity:.74;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
      margin-bottom:3px;
    }
    .kpi-value{
      font-size:24px;
      font-weight:900;
      letter-spacing:-.2px;
      line-height:1.03;
      display:flex;
      align-items:baseline;
      gap:6px;
    }
    .kpi-note-badge{
      display:inline-flex;
      align-items:center;
      justify-content:center;
      width:15px;
      height:15px;
      border-radius:50%;
      font-size:9px;
      font-weight:800;
      line-height:1;
      cursor:help;
      user-select:none;
      flex:0 0 auto;
      color: var(--sinan-accent);
      background: rgba(var(--sinan-accent-rgb), .12);
      border:1px solid rgba(var(--sinan-accent-rgb), .28);
    }
    .kpi-sub{
      font-size:12px;
      opacity:.70;
      margin-top:3px;
      white-space:nowrap; overflow:hidden; text-overflow:ellipsis;
    }
    .kpi-delta{margin-top:5px; font-size:12px; font-weight:800;}
    .kpi-good{color:#16A34A;}
    .kpi-bad{color:#B42318;}
    .kpi-flat{opacity:.74;}

    .kpi-icon{
      margin-left:auto; flex:0 0 auto;
      width:36px;height:36px;border-radius:999px;
      display:flex;align-items:center;justify-content:center;
      background:rgba(15,23,42,.05);
      border:1px solid rgba(15,23,42,.08);
      box-shadow:0 10px 22px rgba(2,6,23,.10);
      opacity:.92;
    }

    .kpi-card.kpi-clickable{cursor:pointer;user-select:none;}
    .kpi-card.kpi-clickable:hover{
      border-color:rgba(15,23,42,.16);
      box-shadow:0 18px 44px rgba(2,6,23,.12);
    }
    .kpi-card.kpi-clickable:hover::after{opacity:1;}
    .kpi-card.kpi-clickable:active{--tz:-2px; box-shadow:0 12px 30px rgba(2,6,23,.14);}
    .kpi-card.kpi-clickable:focus-visible{
      outline:none;
      border-color:color-mix(in srgb, var(--kpi-accent) 55%, rgba(15,23,42,.18));
      box-shadow:0 0 0 2px color-mix(in srgb, var(--kpi-accent) 35%, transparent), 0 18px 44px rgba(2,6,23,.12);
    }
    .kpi-card.is-selected{
      border-color:color-mix(in srgb, var(--kpi-accent) 60%, rgba(15,23,42,.18));
      box-shadow:0 22px 56px rgba(2,6,23,.14), 0 0 0 2px color-mix(in srgb, var(--kpi-accent) 28%, transparent);
    }
    .kpi-card.is-selected::after{opacity:1;}

    .kpi-help-row{
      display:flex;
      align-items:center;
      justify-content:flex-end;
      margin:0 2px 8px 2px;
      gap:10px;
    }
    .kpi-update-info{
      display:flex;
      flex-direction:column;
      align-items:flex-end;
      line-height:1.3;
      font-size:11px;
      color:#6b7280;
    }
    .kpi-update-info b{ color:#374151; font-weight:600; }
    .kpi-help-badge{
      display:inline-flex;
      align-items:center;
      justify-content:center;
      width:18px;
      height:18px;
      border-radius:999px;
      background:#0b8a8f;
      color:#fff;
      font-size:11px;
      font-weight:900;
      line-height:1;
      cursor:help;
      box-shadow:0 6px 14px rgba(11,138,143,.35);
    }
    .sinan-help-tooltip .tooltip-inner{
      background:#fff !important;
      color:#000 !important;
      font-weight:800 !important;
      border:1px solid rgba(0,0,0,.18);
      box-shadow:0 12px 26px rgba(0,0,0,.18);
      max-width:460px;
      text-align:left;
      padding:10px 12px;
    }
    .sinan-help-tooltip .tooltip-arrow::before{ border-top-color:#fff !important; }
    .sinan-help-text{
      font-size:12px;
      line-height:1.3;
      font-weight:800;
    }

    @media (prefers-reduced-motion: reduce){
      .kpi-card{transition:none !important; transform:none !important;}
    }
  ")))
}

# ----------------------------------------------------------------------
# JS: click + keyboard
# ----------------------------------------------------------------------
.kpis_js <- function(id) {
  ns <- NS(id)
  input_id <- ns("kpi_select")
  root_id  <- ns("kpi_grid")

  singleton(tags$script(HTML(sprintf("
(function(){
  function sendKpi(key){
    key = String(key || '');
    if(!key) return;
    Shiny.setInputValue('%s', {key: key, nonce: Date.now()}, {priority: 'event'});
  }
  var rootSel = '#%s';

  function initTooltips(){
    try{
      if(!window.bootstrap || !window.bootstrap.Tooltip) return;
      var nodes = document.querySelectorAll('[data-bs-toggle=tooltip]');
      nodes.forEach(function(el){
        try{ window.bootstrap.Tooltip.getOrCreateInstance(el); }catch(e){}
      });
    }catch(e){}
  }
  setTimeout(initTooltips, 0);

  $(document).on('pointerdown', rootSel + ' .kpi-card.kpi-clickable[data-kpi]', function(e){
    try{ e.preventDefault(); }catch(_){}
    try{ e.stopPropagation(); }catch(_){}
    sendKpi($(this).attr('data-kpi'));
  });
  $(document).on('click', rootSel + ' .kpi-card.kpi-clickable[data-kpi]', function(e){
    try{ e.preventDefault(); }catch(_){}
    try{ e.stopPropagation(); }catch(_){}
    sendKpi($(this).attr('data-kpi'));
  });
  $(document).on('keydown', rootSel + ' .kpi-card.kpi-clickable[data-kpi]', function(e){
    var code = e.which || e.keyCode;
    if(code === 13 || code === 32){
      e.preventDefault();
      sendKpi($(this).attr('data-kpi'));
    }
  });
})();",
    input_id, root_id
  ))))
}

# ----------------------------------------------------------------------
# Card builder
# ----------------------------------------------------------------------
.kpi_make_card <- function(
  key,
  title,
  value,
  subtitle = NULL,
  delta = NULL,
  icon = NULL,
  accent = "#0F766E",
  selected = FALSE,
  clickable = TRUE,
  note = NULL
) {
  key <- as.character(key %||% "")
  cls <- c("kpi-card")
  if (isTRUE(clickable)) cls <- c(cls, "kpi-clickable")
  if (isTRUE(selected))  cls <- c(cls, "is-selected")

  tags$div(
    class = paste(cls, collapse = " "),
    `data-kpi` = if (nzchar(key)) key else NULL,
    style = paste0("--kpi-accent:", accent, ";"),
    tabindex = if (isTRUE(clickable)) "0" else NULL,
    role = if (isTRUE(clickable)) "button" else NULL,
    `aria-pressed` = if (isTRUE(clickable)) if (isTRUE(selected)) "true" else "false" else NULL,
    `aria-label` = if (isTRUE(clickable) && nzchar(title)) paste0("Selecionar métrica: ", title) else NULL,
    tags$div(
      class = "kpi-inner",
      tags$div(class = "kpi-accent"),
      tags$div(
        class = "kpi-text",
        tags$div(class = "kpi-title", title),
        tags$div(class = "kpi-value", value, note),
        if (!is.null(subtitle)) tags$div(class = "kpi-sub", subtitle),
        if (!is.null(delta))    tags$div(class = "kpi-delta", delta)
      ),
      if (!is.null(icon)) tags$div(class = "kpi-icon", icon)
    )
  )
}

# Nota exibida nas KPIs que usam populacao por bairro como denominador
# (Incidencia, Taxa de mortalidade), apenas quando um bairro esta selecionado.
# Nesses anos sem Censo/Estimativa real, a populacao do bairro e interpolada
# geometricamente entre Censo 2010, Censo 2022 e Estimativa 2024.
.kpi_pop_estimada_note <- function() {
  tags$span(
    class = "kpi-note-badge",
    title = "População estimada",
    `data-bs-title` = paste0(
      "<div class='sinan-help-text'>",
      "População do bairro estimada por interpolação/extrapolação geométrica<br>",
      "entre Censo 2010, Censo 2022 e Estimativa 2024, nos anos sem dado direto.<br>",
      "Anos com Censo/Estimativa real usam o valor informado, sem cálculo.",
      "</div>"
    ),
    `aria-label` = "População do bairro estimada por interpolação geométrica",
    `data-bs-toggle` = "tooltip",
    `data-bs-html` = "true",
    `data-bs-custom-class` = "sinan-help-tooltip",
    `data-bs-placement` = "top",
    "i"
  )
}

# ----------------------------------------------------------------------
# Delta badge:
#  - Para cases/incid/obitos: DECRÉSCIMO = bom (verde), ACRÉSCIMO = ruim (vermelho)
#  - Para curas: DECRÉSCIMO = ruim (vermelho), ACRÉSCIMO = bom (verde)
# ----------------------------------------------------------------------
.kpi_delta_badge <- function(cur, prev, is_rate = FALSE, good_when_decrease = TRUE, label_suffix = "vs ano anterior") {
  cur  <- suppressWarnings(as.numeric(cur))
  prev <- suppressWarnings(as.numeric(prev))
  if (length(cur) == 0 || length(prev) == 0) return(NULL)
  if (!isTRUE(is.finite(cur[1])) || !isTRUE(is.finite(prev[1]))) return(NULL)

  d <- cur[1] - prev[1]
  if (abs(d) < 1e-9) return(tags$span(class = "kpi-flat", paste0("\u2248 sem variação ", label_suffix)))

  arrow <- if (d > 0) "\u2191" else "\u2193"
  is_decrease <- d < 0

  good <- if (isTRUE(good_when_decrease)) is_decrease else !is_decrease
  cls  <- if (isTRUE(good)) "kpi-good" else "kpi-bad"

  txt <- if (isTRUE(is_rate)) paste0(arrow, " ", .kpi_fmt_num(d, 2), " ", label_suffix)
  else paste0(arrow, " ", .kpi_fmt_int(d), " ", label_suffix)

  tags$span(class = cls, txt)
}

# ----------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------
mod_kpis_ui <- function(id, ..., pack = NULL) {
  ns <- NS(id)

  # Resolve pack object
  pack_obj <- if (is.function(pack)) tryCatch(pack(), error = function(e) NULL) else pack
  labs <- if (is.list(pack_obj)) pack_obj[["KPI_LABELS"]] else list()
  if (!is.list(labs)) labs <- list()

  # KPI_LAYOUT define a ordem; fallback padrão se não definido
  layout <- if (is.list(pack_obj)) pack_obj[["KPI_LAYOUT"]] else NULL
  if (is.null(layout) || length(layout) == 0) {
    layout <- c("incid", "cases", "cases_0_14", "taxa_det_0_14", "obitos", "cura")
  }

  # KPIs sempre disponíveis (não precisam de KPI_LABELS para existir)
  always_available <- c("cases", "incid", "obitos", "cura", "pop")

  # Monta slots na ordem do layout
  slots <- lapply(layout, function(key) {
    # KPIs condicionais: só incluir se definidos em KPI_LABELS
    if (!key %in% always_available && is.null(labs[[key]])) return(NULL)
    uiOutput(ns(paste0("kpi_", key)))
  })
  slots <- Filter(Negate(is.null), slots)

  tagList(
    .kpis_css(),
    .kpis_js(id),
    div(
      class = "kpi-wrap",
      div(
        class = "kpi-help-row",
        uiOutput(ns("kpi_update_info")),
        tags$span(
          class = "kpi-help-badge",
          title = "Ajuda sobre selecao de KPIs",
          `data-bs-title` = paste0(
            "<div class='sinan-help-text'>",
            "Casos novos, Obitos e Curas sao KPIs selecionaveis.<br>",
            "Clique em uma KPI para mudar a metrica ativa do painel.<br>",
            "O mapa passa a colorir e priorizar a metrica escolhida.<br>",
            "Graficos e comparacoes temporais acompanham a selecao.",
            "</div>"
          ),
          `aria-label` = "Ajuda sobre selecao de KPIs",
          `data-bs-toggle` = "tooltip",
          `data-bs-html` = "true",
          `data-bs-custom-class` = "sinan-help-tooltip",
          `data-bs-placement` = "top",
          "i"
        )
      ),
      do.call(div, c(list(id = ns("kpi_grid"), class = "kpi-grid"), slots))
    )
  )
}

# ----------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------
mod_kpis_server <- function(
  id,
  cfg = NULL,
  pack = NULL,
  ...,
  nav = NULL,
  ano = NULL,
  data_api = NULL,
  state = NULL,
  data = NULL
) {
  dots <- list(...)
  if (is.null(cfg)  && !is.null(dots$cfg))  cfg  <- dots$cfg
  if (is.null(pack) && !is.null(dots$pack)) pack <- dots$pack
  if (is.null(state) && !is.null(dots$state)) state <- dots$state
  if (is.null(data)  && !is.null(dots$data))  data  <- dots$data
  if (is.null(data_api)) data_api <- dots$data_api %||% data_api %||% data
  if (is.null(nav) && !is.null(dots$nav)) nav <- dots$nav
  if (is.null(nav) && !is.null(state)) nav <- state

  moduleServer(id, function(input, output, session) {

    cfg_ref  <- cfg
    pack_ref <- pack

    .sinan_log_open(session = session, cfg = .res(cfg_ref), pack = .res(pack_ref))
    logw <- function(section, ...) .sinan_log_write(paste0("kpis.", section), ..., session = session)

    # Lookup PE municípios → macro/micro (carregado uma vez)
    .pe_lookup_cache <- local({
      val <- NULL
      function() {
        if (!is.null(val)) return(val)
        if (exists(".read_pe_municipios_lookup", mode = "function", inherits = TRUE)) {
          val <<- tryCatch(.read_pe_municipios_lookup(cfg = .res(cfg_ref)), error = function(e) NULL)
        }
        val
      }
    })

    .filter_muns_by_region <- function(dfm, selected_macro = NULL, selected_micro = NULL) {
      if (is.null(dfm) || !is.data.frame(dfm) || nrow(dfm) == 0) return(dfm)
      if (is.null(selected_macro) && is.null(selected_micro)) return(dfm)
      lk <- .pe_lookup_cache()
      if (is.null(lk) || !is.data.frame(lk) || nrow(lk) == 0) return(dfm)
      if (!is.null(selected_micro) && nzchar(selected_micro)) {
        codes <- lk$cod_mun7[lk$micro_key == selected_micro]
      } else if (!is.null(selected_macro) && nzchar(selected_macro)) {
        codes <- lk$cod_mun7[lk$macro_key == selected_macro]
      } else {
        return(dfm)
      }
      if (length(codes) == 0) return(dfm)
      dfm[dfm$cod_mun7 %in% codes, , drop = FALSE]
    }

    .get_pack <- function() .res(pack_ref) %||% list()

    # cores: herda do disease_pack (suporta extensão COLORS$cases/incid/obitos/cura/pop)
    .pack_color <- function(kpi_key) {
      p <- .get_pack()
      cols <- .obj_get(p, "COLORS", list()) %||% list()

      # extensão opcional (preferência máxima)
      direct <- .obj_get(cols, kpi_key, NULL)
      if (is.character(direct) && nzchar(direct)) return(direct)

      # base (3 cores do pack)
      primary   <- .obj_get(cols, "primary",   "#0F766E")
      secondary <- .obj_get(cols, "secondary", "#0B5FA5")
      danger    <- .obj_get(cols, "danger",    "#D14900")

      # mapping padrão
      if (kpi_key == "cases")  return(danger)
      if (kpi_key == "incid")  return(secondary)
      if (kpi_key == "pop")    return(primary)

      #  IMPORTANT: ÓBITOS/CURAS também devem "publicar" uma cor consistente
      # (se você não definir no pack, usa fallback padrão)
      if (kpi_key == "obitos") return("#B42318")
      if (kpi_key == "cura")   return("#16A34A")

      primary
    }

    .pack_label <- function(kpi_key, fallback) {
      p <- .get_pack()
      labs <- .obj_get(p, "KPI_LABELS", list()) %||% list()
      v <- .obj_get(labs, kpi_key, NULL)
      if (is.character(v) && nzchar(v)) return(v)
      fallback
    }

    # -------------------------------
    # resolve data_api contract
    # -------------------------------
    .resolve_api <- function() {
      api1 <- .res(data_api); if (!is.null(api1)) return(api1)
      api2 <- .res(.obj_get(state, "data_api", NULL)); if (!is.null(api2)) return(api2)
      api3 <- .res(.obj_get(state, "data", NULL)); if (!is.null(api3)) return(api3)
      api4 <- .res(.obj_get(.get_pack(), "data_api", NULL)); if (!is.null(api4)) return(api4)
      api5 <- .res(.obj_get(.get_pack(), "data", NULL)); if (!is.null(api5)) return(api5)
      NULL
    }
    .safe_call <- function(fun, ...) tryCatch(fun(...), error = function(e) e)
    .tb_grau_filter <- function() {
      x <- .obj_get(nav, "tb_grau_incapacidade", NULL) %||% .obj_get(state, "tb_grau_incapacidade", NULL)
      if (is.null(x) || !is.list(x) || is.null(x$value) || is.null(x$var)) return(NULL)
      if (!nzchar(as.character(x$value)) || identical(as.character(x$value), "__ALL__")) return(NULL)
      x
    }
    .tb_grau_args <- function() {
      x <- .tb_grau_filter()
      if (is.null(x)) list() else list(tb_grau_incapacidade = x, grau_incapacidade = x)
    }
    .tb_grau_key <- function() {
      x <- .tb_grau_filter()
      if (is.null(x)) "" else paste0("|tb_grau=", x$var, ":", x$value)
    }
    .safe_get <- function(name, ...) {
      api <- .resolve_api()
      if (is.null(api)) return(NULL)
      args <- c(list(...), .tb_grau_args())

      if (is.list(api) && is.function(api[[name]])) {
        r <- do.call(.safe_call, c(list(api[[name]]), args))
        if (inherits(r, "error")) { logw("api_error", paste0(name, "(): ", r$message)); return(NULL) }
        return(r)
      }
      if (is.environment(api) && exists(name, envir = api, inherits = FALSE)) {
        fun <- get(name, envir = api, inherits = FALSE)
        if (is.function(fun)) {
          r <- do.call(.safe_call, c(list(fun), args))
          if (inherits(r, "error")) { logw("api_error", paste0(name, "(): ", r$message)); return(NULL) }
          return(r)
        }
      }
      NULL
    }

    .safe_df <- function(x) {
      if (is.null(x)) return(data.frame())
      if (inherits(x, "tbl")) return(as.data.frame(x))
      if (is.data.frame(x)) return(x)
      data.frame()
    }

    # -------------------------------
    # caches com compartilhamento global
    # Usa cache global .SINAN_GLOBAL_CACHE para evitar recarregamentos
    # quando múltiplos módulos consultam os mesmos dados
    # -------------------------------
    
    # Obtém ou cria caches locais vinculadas ao global
    .get_global_cache <- function() {
      if (exists(".SINAN_GLOBAL_CACHE", inherits = TRUE)) {
        return(get(".SINAN_GLOBAL_CACHE", inherits = TRUE))
      }
      new.env(parent = emptyenv())
    }

    global_cache <- .get_global_cache()
    
    # Caches locais com fallback para o global se disponível
    cache_uf  <- new.env(parent = emptyenv())  # key: "y"
    cache_mun <- new.env(parent = emptyenv())  # key: "y|UF"

    .get_uf_metrics_cached <- function(y) {
      ky <- paste0("kpi_uf|", as.character(as.integer(y)), .tb_grau_key())
      
      # Verifica cache global primeiro
      if (exists(ky, envir = global_cache, inherits = FALSE)) {
        return(get(ky, envir = global_cache, inherits = FALSE))
      }
      
      # Fallback para cache local
      ky_local <- paste0(as.character(as.integer(y)), .tb_grau_key())
      if (exists(ky_local, envir = cache_uf, inherits = FALSE)) {
        return(get(ky_local, envir = cache_uf, inherits = FALSE))
      }
      
      df <- .safe_get("get_uf_metrics", ano = y, year = y, y = y)
      out <- .std_uf_metrics(df)
      
      # Armazena em ambos os caches
      assign(ky, out, envir = global_cache)
      assign(ky_local, out, envir = cache_uf)
      out
    }

    .selected_bairro_key <- function() {
      invisible(.obj_get(nav, "_map_tick", NULL)); invisible(.obj_get(state, "_map_tick", NULL))
      key <- .obj_get(nav, "selected_bairro_key", NULL) %||% .obj_get(state, "selected_bairro_key", NULL)
      key <- .scalar_chr(key, "")
      if (nzchar(key)) key else NULL
    }

    .selected_bairro_name <- function() {
      invisible(.obj_get(nav, "_map_tick", NULL)); invisible(.obj_get(state, "_map_tick", NULL))
      nm <- .obj_get(nav, "selected_bairro_name", NULL) %||% .obj_get(state, "selected_bairro_name", NULL)
      .scalar_chr(nm, .selected_bairro_key() %||% "")
    }

    .is_tb_recife_bairro <- function() {
      d0 <- toupper(.scalar_chr(.obj_get(.get_pack(), "DOENCA", NULL), ""))
      isTRUE(.obj_get(.res(cfg_ref), "LOCK_RECIFE", FALSE) %||% .obj_get(.res(cfg_ref), "lock_recife", FALSE)) && d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB") && !is.null(.selected_bairro_key())
    }

    .tb_recife_bairro_rows <- function(y = NULL) {
      if (!.is_tb_recife_bairro()) return(data.frame())
      df <- .kpi_read_recife_bairro_incidence(.res(cfg_ref))
      if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
      key <- .selected_bairro_key()
      df <- df[df$bairro_key == key, , drop = FALSE]
      yy <- suppressWarnings(as.integer(y))
      if (length(yy) > 0 && is.finite(yy[[1]])) df <- df[suppressWarnings(as.integer(df$ano)) == yy[[1]], , drop = FALSE]
      df
    }

    .tb_recife_bairro_pop <- function() {
      df <- .tb_recife_bairro_rows(NULL)
      if (!is.data.frame(df) || nrow(df) == 0) return(0)
      v <- suppressWarnings(as.numeric(df$pop_total[[1]]))
      if (is.finite(v)) v else 0
    }

    .tb_recife_bairro_metrics <- function(y) {
      df <- .tb_recife_bairro_rows(y)
      pop_val <- .tb_recife_bairro_pop()

      if (!is.data.frame(df) || nrow(df) == 0) {
        return(list(
          cases = 0, obitos = 0, cura = 0, pop = pop_val,
          incid = if (pop_val > 0) 0 else NA_real_,
          mortalidade = if (pop_val > 0) 0 else NA_real_,
          letalidade = NA_real_
        ))
      }
      row <- df[1, , drop = FALSE]
      cases <- suppressWarnings(as.numeric(row$casos_total[[1]]))
      obitos <- suppressWarnings(as.numeric(row$casos_obitos[[1]]))
      cura <- suppressWarnings(as.numeric(row$casos_cura[[1]]))
      incid <- suppressWarnings(as.numeric(row$incid_100k_total[[1]]))
      mortalidade <- suppressWarnings(as.numeric(row$mortalidade_100k_total[[1]]))
      letalidade <- suppressWarnings(as.numeric(row$letalidade_pct[[1]]))
      list(
        cases = if (is.finite(cases)) cases else 0,
        obitos = if (is.finite(obitos)) obitos else 0,
        cura = if (is.finite(cura)) cura else 0,
        pop = pop_val,
        incid = if (pop_val > 0 && is.finite(incid)) incid else NA_real_,
        mortalidade = if (pop_val > 0 && is.finite(mortalidade)) mortalidade else NA_real_,
        letalidade = if (is.finite(cases) && cases > 0 && is.finite(letalidade)) letalidade else NA_real_
      )
    }
    .get_mun_metrics_cached <- function(y, uf) {
      u <- .to_uf(uf)
      if (is.na(u) || !nzchar(u)) return(NULL)
      
      ky <- paste0("kpi_mun|", as.character(as.integer(y)), "|", u, .tb_grau_key())
      
      # Verifica cache global primeiro
      if (exists(ky, envir = global_cache, inherits = FALSE)) {
        return(get(ky, envir = global_cache, inherits = FALSE))
      }
      
      # Fallback para cache local
      ky_local <- paste0(as.character(as.integer(y)), "|", u, .tb_grau_key())
      if (exists(ky_local, envir = cache_mun, inherits = FALSE)) {
        return(get(ky_local, envir = cache_mun, inherits = FALSE))
      }
      
      df <- .safe_get("get_mun_metrics", ano = y, year = y, uf = u, sg_uf = u)
      out <- .std_mun_metrics(df)
      if (!is.null(out) && any(!is.na(out$uf))) out <- out[out$uf == u | is.na(out$uf), , drop = FALSE]
      
      # Armazena em ambos os caches
      assign(ky, out, envir = global_cache)
      assign(ky_local, out, envir = cache_mun)
      out
    }

    obitos_cache <- new.env(parent = emptyenv())

    .dashboard_dir_kpi <- function() {
      cfg0 <- .res(cfg_ref)
      dash <- .obj_get(cfg0, "DASHBOARD_DIR", NULL) %||% .obj_get(cfg0, "dashboard_dir", NULL)
      dash <- .scalar_chr(dash, "")
      if (nzchar(dash) && dir.exists(dash)) return(dash)

      pths <- .obj_get(cfg0, "PATHS", NULL)
      if (is.list(pths)) {
        dash2 <- .scalar_chr(.obj_get(pths, "DASHBOARD_DIR", NULL), "")
        if (nzchar(dash2) && dir.exists(dash2)) return(dash2)
      }

      root <- .scalar_chr(.obj_get(cfg0, "ROOT", NULL) %||% .obj_get(cfg0, "root", NULL), "")
      if (nzchar(root)) {
        cand <- file.path(root, "data", "parquet")
        if (dir.exists(cand)) return(cand)
      }

      ""
    }

    .obitos_dir <- function(y, level) {
      dash <- .dashboard_dir_kpi()
      if (!nzchar(dash)) return(NULL)
      disease <- toupper(.scalar_chr(.obj_get(.get_pack(), "DOENCA", NULL), ""))
      lvl <- .kpi_norm_level(level)
      cand <- file.path(dash, "obitos", paste0("doenca=", disease), paste0("nivel=", lvl), paste0("ano=", as.integer(y)))
      if (dir.exists(cand)) cand else NULL
    }

    .count_obitos_fallback <- function(y, level, uf = NULL, mun = NULL) {
      lvl <- .kpi_norm_level(level)
      ky <- paste(level, y, .to_uf(uf), .to_mun7(mun), sep = "|")
      if (exists(ky, envir = obitos_cache, inherits = FALSE)) return(get(ky, envir = obitos_cache, inherits = FALSE))

      root <- .obitos_dir(y, lvl)
      if (is.null(root)) {
        assign(ky, NA_real_, envir = obitos_cache)
        return(NA_real_)
      }

      files <- tryCatch(list.files(root, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE), error = function(e) character(0))
      if (length(files) == 0) {
        assign(ky, NA_real_, envir = obitos_cache)
        return(NA_real_)
      }

      u <- .to_uf(uf)
      m7 <- .to_mun7(mun)
      m6 <- if (!is.na(m7) && nzchar(m7)) substr(m7, 1L, 6L) else NA_character_
      total <- 0
      found <- FALSE

      for (path in files) {
        df <- tryCatch(arrow::read_parquet(path), error = function(e) NULL)
        if (!is.data.frame(df) || nrow(df) == 0) next

        if (lvl %in% c("UF", "MUN")) {
          uf_col <- .col_pick_norm(names(df), c("uf", "UF", "sg_uf", "SIGLA_UF"))
          if (!is.null(uf_col) && !is.na(u) && nzchar(u)) {
            df <- df[toupper(trimws(as.character(df[[uf_col]]))) == u, , drop = FALSE]
          }
        }

        if (lvl == "MUN" && nrow(df) > 0) {
          mun_col <- .col_pick_norm(names(df), c("cod_mun7", "COD_MUN7", "cod_mun6", "COD_MUN6", "cod_mun", "COD_MUN", "geo_id", "GEO_ID"))
          if (!is.null(mun_col) && !is.na(m6) && nzchar(m6)) {
            vals <- gsub("\\D+", "", as.character(df[[mun_col]]))
            vals6 <- substr(vals, 1L, 6L)
            df <- df[vals6 == m6, , drop = FALSE]
          }
        }

        if (nrow(df) == 0) next
        total <- total + nrow(df)
        found <- TRUE
      }

      out <- if (found) as.numeric(total) else NA_real_
      assign(ky, out, envir = obitos_cache)
      out
    }

    .cases_period_compare <- function(y, nav) {
      y <- suppressWarnings(as.integer(y))
      if (!isTRUE(is.finite(y))) return(NULL)
      nav <- nav %||% list(level = "UF", uf = "PE", mun = NULL)
      lvl <- .kpi_norm_level(nav$level %||% "UF")

      args <- list(
        ano = c(y - 1L, y), anos = c(y - 1L, y), years = c(y - 1L, y),
        level = lvl, nivel = lvl,
        uf = nav$uf, mun = nav$mun,
        cod_mun7 = .to_mun7(nav$mun),
        geo_id = nav$mun,
        doenca = .obj_get(.get_pack(), "DOENCA", NULL),
        metric = "cases", indicador = "cases", variavel = "cases"
      )

      df <- tryCatch(do.call(.safe_get, c(list("get_time_series"), args)), error = function(e) NULL)
      df <- .safe_df(df)
      if (nrow(df) == 0) return(NULL)

      c_year  <- .col_pick_norm(names(df), c("ano", "ANO", "year", "YEAR"))
      c_month <- .col_pick_norm(names(df), c("mes", "MES", "month", "MONTH"))
      c_val   <- .col_pick_norm(names(df), c("valor", "VALOR", "n", "N", "total", "TOTAL", "casos", "cases", "casos_total", "cases_total"))
      if (is.null(c_year) || is.null(c_month) || is.null(c_val)) return(NULL)

      c_metric <- .col_pick_norm(names(df), c("metric", "indicador", "variavel", "tipo"))
      if (!is.null(c_metric)) {
        df <- df[tolower(trimws(as.character(df[[c_metric]]))) %in% c("cases", "casos"), , drop = FALSE]
      }

      if (lvl == "UF") {
        c_uf <- .col_pick_norm(names(df), c("uf", "UF", "sg_uf", "SIGLA_UF"))
        if (!is.null(c_uf) && !is.null(nav$uf) && nzchar(nav$uf)) {
          df <- df[toupper(trimws(as.character(df[[c_uf]]))) == toupper(nav$uf), , drop = FALSE]
        }
      }
      if (lvl == "MUN") {
        c_mun <- .col_pick_norm(names(df), c("cod_mun7", "COD_MUN7", "geo_id", "GEO_ID", "cd_mun", "CD_MUN", "cd_ibge", "CD_IBGE"))
        if (!is.null(c_mun) && !is.null(nav$mun) && nzchar(nav$mun)) {
          m <- .to_mun7(nav$mun)
          df <- df[.to_mun7(df[[c_mun]]) == m, , drop = FALSE]
        }
      }

      df$year <- suppressWarnings(as.integer(df[[c_year]]))
      df$month <- suppressWarnings(as.integer(df[[c_month]]))
      df$value <- suppressWarnings(as.numeric(df[[c_val]]))
      df <- df[is.finite(df$year) & is.finite(df$month) & is.finite(df$value), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)

      cur <- df[df$year == y, , drop = FALSE]
      if (nrow(cur) == 0) return(NULL)

      last_month <- suppressWarnings(max(cur$month, na.rm = TRUE))
      if (!isTRUE(is.finite(last_month)) || last_month <= 0) return(NULL)
      last_month <- min(last_month, 12L)

      cur_sum <- sum(cur$value[cur$month <= last_month], na.rm = TRUE)
      prev <- df[df$year == (y - 1L) & df$month <= last_month, , drop = FALSE]
      if (nrow(prev) == 0) return(NULL)
      prev_sum <- sum(prev$value, na.rm = TRUE)

      list(cur = cur_sum, prev = prev_sum, last_month = last_month)
    }

    .kpi_period_compare_metric <- function(metric_key, y, nav, value_mode = c("sum", "mean")) {
      y <- suppressWarnings(as.integer(y))
      if (!isTRUE(is.finite(y))) return(NULL)
      nav <- nav %||% list(level = "UF", uf = "PE", mun = NULL)
      lvl <- .kpi_norm_level(nav$level %||% "UF")
      mkey <- tolower(trimws(as.character(metric_key %||% "cases")))
      mkey <- mkey[[1]]
      if (is.na(mkey) || !nzchar(mkey)) mkey <- "cases"
      value_mode <- match.arg(value_mode)

      args <- list(
        ano = c(y - 1L, y), anos = c(y - 1L, y), years = c(y - 1L, y),
        level = lvl, nivel = lvl,
        uf = nav$uf, mun = nav$mun,
        cod_mun7 = .to_mun7(nav$mun),
        geo_id = nav$mun,
        doenca = .obj_get(.get_pack(), "DOENCA", NULL),
        metric = mkey, indicador = mkey, variavel = mkey
      )

      df <- tryCatch(do.call(.safe_get, c(list("get_time_series"), args)), error = function(e) NULL)
      df <- .safe_df(df)
      if (nrow(df) == 0) return(NULL)

      c_year  <- .col_pick_norm(names(df), c("ano", "ANO", "year", "YEAR"))
      c_month <- .col_pick_norm(names(df), c("mes", "MES", "month", "MONTH"))
      if (is.null(c_year) || is.null(c_month)) return(NULL)

      val_candidates <- switch(
        mkey,
        "obitos" = c("casos_obitos", "obitos_total", "obitos", "deaths", "qt_obitos", "QT_OBITOS", "TOTAL_OBITOS"),
        "cura" = c("casos_cura", "cura_total", "curas", "qt_curas", "QT_CURAS", "TOTAL_CURAS"),
        "incid" = c("incid_100k_total", "incid_100k", "incidencia_100k_total", "incid", "INCID_100K_TOTAL", "INCID_100K", "INCID"),
        "cases_0_14" = c("casos_0_14", "CASOS_0_14", "cases_0_14", "CASES_0_14"),
        "taxa_det_0_14" = c("taxa_det_0_14", "TAXA_DET_0_14", "taxa_det", "taxadet", "taxa"),
        c("valor", "VALOR", "n", "N", "total", "TOTAL", "casos", "cases", "casos_total", "cases_total")
      )

      c_val <- .col_pick_norm(names(df), val_candidates)
      if (is.null(c_val)) return(NULL)

      c_metric <- .col_pick_norm(names(df), c("metric", "indicador", "variavel", "tipo"))
      if (!is.null(c_metric)) {
        metric_norm <- tolower(trimws(as.character(df[[c_metric]])))
        metric_norm <- iconv(metric_norm, from = "", to = "ASCII//TRANSLIT")
        metric_norm <- gsub("[^a-z0-9_]+", "", metric_norm)
        mkey_norm <- iconv(mkey, from = "", to = "ASCII//TRANSLIT")
        mkey_norm <- gsub("[^a-z0-9_]+", "", mkey_norm)
        df <- df[metric_norm == mkey_norm, , drop = FALSE]
      }

      if (lvl == "UF") {
        c_uf <- .col_pick_norm(names(df), c("uf", "UF", "sg_uf", "SIGLA_UF"))
        if (!is.null(c_uf) && !is.null(nav$uf) && nzchar(nav$uf)) {
          df <- df[toupper(trimws(as.character(df[[c_uf]]))) == toupper(nav$uf), , drop = FALSE]
        }
      }
      if (lvl == "MUN") {
        c_mun <- .col_pick_norm(names(df), c("cod_mun7", "COD_MUN7", "geo_id", "GEO_ID", "cd_mun", "CD_MUN", "cd_ibge", "CD_IBGE"))
        if (!is.null(c_mun) && !is.null(nav$mun) && nzchar(nav$mun)) {
          m <- .to_mun7(nav$mun)
          df <- df[.to_mun7(df[[c_mun]]) == m, , drop = FALSE]
        }
      }

      df$year <- suppressWarnings(as.integer(df[[c_year]]))
      df$month <- suppressWarnings(as.integer(df[[c_month]]))
      df$value <- suppressWarnings(as.numeric(df[[c_val]]))
      df <- df[is.finite(df$year) & is.finite(df$month) & is.finite(df$value), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)

      cur <- df[df$year == y, , drop = FALSE]
      if (nrow(cur) == 0) return(NULL)

      last_month <- suppressWarnings(max(cur$month, na.rm = TRUE))
      if (!isTRUE(is.finite(last_month)) || last_month <= 0) return(NULL)
      last_month <- min(last_month, 12L)

      cur_vals <- cur$value[cur$month <= last_month]
      prev <- df[df$year == (y - 1L) & df$month <= last_month, , drop = FALSE]
      if (nrow(prev) == 0) return(NULL)
      prev_vals <- prev$value

      cur_out <- if (identical(value_mode, "mean")) mean(cur_vals, na.rm = TRUE) else sum(cur_vals, na.rm = TRUE)
      prev_out <- if (identical(value_mode, "mean")) mean(prev_vals, na.rm = TRUE) else sum(prev_vals, na.rm = TRUE)

      list(cur = cur_out, prev = prev_out, last_month = last_month)
    }

    # -------------------------------
    # NAV deps
    # -------------------------------
    .touch_nav_deps <- function(x) {
      if (is.null(x)) return(invisible(NULL))
      invisible(.obj_get(x, "_map_tick", NULL))
      invisible(.obj_get(x, "map_tick", NULL))
      invisible(.obj_get(x, "nav_tick", NULL))
      invisible(.obj_get(x, "tick", NULL))
      invisible(.obj_get(x, "level", NULL))
      invisible(.obj_get(x, "nivel", NULL))
      invisible(.obj_get(x, "uf", NULL))
      invisible(.obj_get(x, "UF", NULL))
      invisible(.obj_get(x, "sigla_uf", NULL))
      invisible(.obj_get(x, "sg_uf", NULL))
      invisible(.obj_get(x, "mun", NULL))
      invisible(.obj_get(x, "selected_bairro_key", NULL))
      invisible(.obj_get(x, "selected_bairro_name", NULL))
      invisible(.obj_get(x, "MUN", NULL))
      invisible(.obj_get(x, "cod_mun7", NULL))
      invisible(.obj_get(x, "geo_id", NULL))
      invisible(.obj_get(x, "cd_mun", NULL))
      invisible(.obj_get(x, "cd_ibge", NULL))
      invisible(.obj_get(x, "ano", NULL))
    }

    .nav_get_level <- function() {
      lvl <- .obj_get(nav, "level", NULL) %||% .obj_get(nav, "nivel", NULL) %||% .obj_get(state, "level", NULL) %||% .obj_get(state, "nivel", NULL)
      .kpi_norm_level(lvl %||% "UF")
    }
    .nav_get_uf <- function() {
      v <- .obj_get(nav, "uf", NULL) %||% .obj_get(nav, "UF", NULL) %||% .obj_get(nav, "sigla_uf", NULL) %||% .obj_get(nav, "sg_uf", NULL) %||%
        .obj_get(state, "uf", NULL) %||% .obj_get(state, "UF", NULL) %||% .obj_get(state, "sigla_uf", NULL) %||% .obj_get(state, "sg_uf", NULL)
      .as_chr1(v, "PE")
    }
    .nav_get_mun <- function() {
      v <- .obj_get(nav, "mun", NULL) %||% .obj_get(nav, "MUN", NULL) %||% .obj_get(nav, "cod_mun7", NULL) %||% .obj_get(nav, "geo_id", NULL) %||% .obj_get(nav, "cd_mun", NULL) %||% .obj_get(nav, "cd_ibge", NULL) %||%
        .obj_get(state, "mun", NULL) %||% .obj_get(state, "MUN", NULL) %||% .obj_get(state, "cod_mun7", NULL) %||% .obj_get(state, "geo_id", NULL) %||% .obj_get(state, "cd_mun", NULL) %||% .obj_get(state, "cd_ibge", NULL)
      .as_chr1(v, NULL)
    }

    nav1 <- reactive({
      .touch_nav_deps(state)
      .touch_nav_deps(nav)

      lvl <- .nav_get_level()
      uf  <- .nav_get_uf()
      mun <- .nav_get_mun()

      sel_macro <- .obj_get(nav, "selected_macro", NULL)
      sel_micro <- .obj_get(nav, "selected_micro", NULL)

      if (identical(lvl, "BR")) { uf <- NULL; mun <- NULL; sel_macro <- NULL; sel_micro <- NULL }
      else if (identical(lvl, "UF")) { mun <- NULL }

      list(level = lvl, uf = uf, mun = mun, selected_macro = sel_macro, selected_micro = sel_micro, selected_bairro_key = .selected_bairro_key(), selected_bairro_name = .selected_bairro_name())
    })

    year1 <- reactive({
      aext <- .res(ano)
      if (is.function(aext)) {
        y <- suppressWarnings(as.integer(aext()))
        if (isTRUE(is.finite(y))) return(y)
      }
      y <- suppressWarnings(as.integer(.obj_get(state, "ano", NA_integer_)))
      if (isTRUE(is.finite(y))) return(y)
      suppressWarnings(as.integer(.obj_get(.res(cfg_ref), "YEAR_MAX", 2025L)))
    })

    # -------------------------------
    # agregação
    # -------------------------------
    .agg_from_df <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(list(cases = 0, obitos = NA_real_, cura = NA_real_, pop = 0, incid = NA_real_))
      }
      
      # Garantir que as colunas existem antes de somar
      cases <- if ("casos_total" %in% names(df)) sum(df$casos_total, na.rm = TRUE) else 0
      pop   <- if ("pop_total" %in% names(df)) sum(df$pop_total, na.rm = TRUE) else 0
      obits <- if ("casos_obitos" %in% names(df)) sum(df$casos_obitos, na.rm = TRUE) else NA_real_
      cura  <- if ("casos_cura" %in% names(df)) sum(df$casos_cura, na.rm = TRUE) else NA_real_
      
      # Se a soma retornou 0 mas existem linhas com NA, manter como informativo
      # mas se retornou 0 com dados válidos (sum de valores reais = 0), manter 0
      if (is.finite(obits) && obits == 0 && "casos_obitos" %in% names(df)) {
        if (all(is.na(df$casos_obitos))) obits <- NA_real_
      }
      if (is.finite(cura) && cura == 0 && "casos_cura" %in% names(df)) {
        if (all(is.na(df$casos_cura))) cura <- NA_real_
      }
      
      incid <- if (isTRUE(is.finite(cases)) && isTRUE(is.finite(pop)) && pop > 0) (cases / pop) * 1e5 else NA_real_
      mortalidade <- if (isTRUE(is.finite(obits)) && isTRUE(is.finite(pop)) && pop > 0) (obits / pop) * 1e5 else NA_real_
      letalidade <- if (isTRUE(is.finite(obits)) && isTRUE(is.finite(cases)) && cases > 0) (obits / cases) * 100 else NA_real_
      list(cases = cases, obitos = obits, cura = cura, pop = pop, incid = incid, mortalidade = mortalidade, letalidade = letalidade)
    }

    .scope_compute <- function(y, level, uf = NULL, mun = NULL, selected_macro = NULL, selected_micro = NULL) {
      if (.is_tb_recife_bairro()) return(.tb_recife_bairro_metrics(y))
      lvl <- .kpi_norm_level(level)
      if (lvl == "BR") {
        df <- .get_uf_metrics_cached(y)
        out <- .agg_from_df(df)
        if (!isTRUE(is.finite(out$obitos))) out$obitos <- .count_obitos_fallback(y, lvl, uf = uf, mun = mun)
        if (isTRUE(is.finite(out$obitos)) && isTRUE(is.finite(out$pop)) && out$pop > 0) out$mortalidade <- (out$obitos / out$pop) * 1e5
        if (isTRUE(is.finite(out$obitos)) && isTRUE(is.finite(out$cases)) && out$cases > 0) out$letalidade <- (out$obitos / out$cases) * 100
        return(out)
      }

      u <- .to_uf(uf)
      if (is.na(u) || !nzchar(u)) return(.agg_from_df(NULL))

      dfm <- .get_mun_metrics_cached(y, u)
      if (lvl == "MUN") {
        m <- .to_mun7(mun)
        if (!is.na(m) && nzchar(m) && is.data.frame(dfm) && nrow(dfm) > 0) {
          dfm <- dfm[dfm$cod_mun7 == m, , drop = FALSE]
        }
      } else {
        # UF level: filter by macro/micro region if selected
        dfm <- .filter_muns_by_region(dfm, selected_macro, selected_micro)
      }
      out <- .agg_from_df(dfm)
      if (!isTRUE(is.finite(out$obitos))) out$obitos <- .count_obitos_fallback(y, lvl, uf = uf, mun = mun)
      if (isTRUE(is.finite(out$obitos)) && isTRUE(is.finite(out$pop)) && out$pop > 0) out$mortalidade <- (out$obitos / out$pop) * 1e5
      if (isTRUE(is.finite(out$obitos)) && isTRUE(is.finite(out$cases)) && out$cases > 0) out$letalidade <- (out$obitos / out$cases) * 100
      out
    }

    scope_pack <- reactive({
      y <- year1()
      n <- nav1()
      cur <- .scope_compute(y, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro)

      y0 <- as.integer(y) - 1L
      prev <- if (isTRUE(is.finite(y0))) .scope_compute(y0, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro) else NULL

      list(y = y, nav = n, cur = cur, prev = prev)
    })

    .scope_compute_hiv_pos_pct <- function(y, level, uf = NULL, mun = NULL) {
      out_empty <- list(hiv_pos_pct = NA_real_, hiv_pos = NA_real_, hiv_tested = NA_real_)
      if (.is_tb_recife_bairro()) {
        dfb <- .tb_recife_bairro_rows(y)
        if (!is.data.frame(dfb) || nrow(dfb) == 0) return(out_empty)
        row <- dfb[1, , drop = FALSE]
        tested <- suppressWarnings(as.numeric(row$hiv_tested[[1]]))
        pos <- suppressWarnings(as.numeric(row$hiv_pos[[1]]))
        pct <- suppressWarnings(as.numeric(row$hiv_pos_pct[[1]]))
        return(list(
          hiv_pos_pct = if (is.finite(tested) && tested > 0 && is.finite(pct)) pct else NA_real_,
          hiv_pos = if (is.finite(pos)) pos else NA_real_,
          hiv_tested = if (is.finite(tested)) tested else NA_real_
        ))
      }

      d0 <- .as_chr1(.obj_get(.get_pack(), "DOENCA", NULL), "")
      d0 <- toupper(trimws(as.character(d0 %||% "")))
      if (!d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")) return(out_empty)

      lvl <- .kpi_norm_level(level)
      u <- .to_uf(uf)
      m <- .to_mun7(mun)

      args <- list(
        ano = y,
        level = lvl, nivel = lvl,
        uf = if (!is.na(u) && nzchar(u)) u else NULL,
        mun = if (!is.na(m) && nzchar(m)) m else NULL,
        cod_mun7 = if (!is.na(m) && nzchar(m)) m else NULL,
        geo_id = if (!is.na(m) && nzchar(m)) m else NULL,
        doenca = .obj_get(.get_pack(), "DOENCA", NULL),
        vars = "HIV",
        top_n = 2000
      )

      df <- tryCatch(do.call(.safe_get, c(list("get_composicao"), args)), error = function(e) NULL)
      df <- .safe_df(df)
      if (!is.data.frame(df) || nrow(df) == 0) return(out_empty)

      c_lbl <- .col_pick_norm(names(df), c("valor_lbl", "VALOR_LBL", "categoria", "CATEGORIA", "cat", "CAT"))
      c_tot <- .col_pick_norm(names(df), c("total", "TOTAL", "n", "N", "valor", "VALOR"))
      if (is.null(c_lbl) || is.null(c_tot)) return(out_empty)

      lbl <- as.character(df[[c_lbl]])
      tot <- suppressWarnings(as.numeric(df[[c_tot]]))
      ok <- is.finite(tot) & !is.na(lbl) & nzchar(trimws(lbl))
      if (!any(ok)) return(out_empty)

      lbl <- lbl[ok]
      tot <- tot[ok]

      lbl_n <- tolower(trimws(lbl))
      lbl_n <- iconv(lbl_n, from = "", to = "ASCII//TRANSLIT")
      lbl_n <- gsub("[^a-z0-9 ]+", " ", lbl_n)
      lbl_n <- gsub("\\s+", " ", lbl_n)

      is_excluded <- grepl("ignor|sem inform|nao inform|branco|nao realiz|pendente|andamento|inconclus|nao se aplica", lbl_n)
      is_neg <- grepl("negativ|nao reag", lbl_n)
      is_pos <- !is_neg & grepl("positiv|reagente", lbl_n)

      hiv_pos <- sum(tot[is_pos], na.rm = TRUE)
      hiv_tested <- sum(tot[is_pos | is_neg], na.rm = TRUE)
      if (!isTRUE(is.finite(hiv_tested)) || hiv_tested <= 0) {
        hiv_tested <- sum(tot[!is_excluded], na.rm = TRUE)
      }

      pct <- if (isTRUE(is.finite(hiv_pos)) && isTRUE(is.finite(hiv_tested)) && hiv_tested > 0) {
        (hiv_pos / hiv_tested) * 100
      } else {
        NA_real_
      }

      list(hiv_pos_pct = pct, hiv_pos = hiv_pos, hiv_tested = hiv_tested)
    }

    scope_pack_hiv <- reactive({
      y <- year1()
      n <- nav1()

      cur <- .scope_compute_hiv_pos_pct(y, n$level, n$uf, n$mun)
      y0 <- as.integer(y) - 1L
      prev <- if (isTRUE(is.finite(y0))) .scope_compute_hiv_pos_pct(y0, n$level, n$uf, n$mun) else NULL

      list(y = y, nav = n, cur = cur, prev = prev)
    })

    .scope_compute_interrupcao_trat_pct <- function(y, level, uf = NULL, mun = NULL) {
      out_empty <- list(interrupcao_trat_pct = NA_real_, abandono = NA_real_, encerramento_total = NA_real_)
      if (.is_tb_recife_bairro()) {
        dfb <- .tb_recife_bairro_rows(y)
        if (!is.data.frame(dfb) || nrow(dfb) == 0) return(out_empty)
        row <- dfb[1, , drop = FALSE]
        total <- suppressWarnings(as.numeric(row$encerramento_total[[1]]))
        abandono <- suppressWarnings(as.numeric(row$abandono[[1]]))
        pct <- suppressWarnings(as.numeric(row$interrupcao_trat_pct[[1]]))
        return(list(
          interrupcao_trat_pct = if (is.finite(total) && total > 0 && is.finite(pct)) pct else NA_real_,
          abandono = if (is.finite(abandono)) abandono else NA_real_,
          encerramento_total = if (is.finite(total)) total else NA_real_
        ))
      }

      d0 <- .as_chr1(.obj_get(.get_pack(), "DOENCA", NULL), "")
      d0 <- toupper(trimws(as.character(d0 %||% "")))
      if (!d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")) return(out_empty)

      lvl <- .kpi_norm_level(level)
      u <- .to_uf(uf)
      m <- .to_mun7(mun)

      args <- list(
        ano = y,
        level = lvl, nivel = lvl,
        uf = if (!is.na(u) && nzchar(u)) u else NULL,
        mun = if (!is.na(m) && nzchar(m)) m else NULL,
        cod_mun7 = if (!is.na(m) && nzchar(m)) m else NULL,
        geo_id = if (!is.na(m) && nzchar(m)) m else NULL,
        doenca = .obj_get(.get_pack(), "DOENCA", NULL),
        vars = "SITUA_ENCE"
      )

      df <- tryCatch(do.call(.safe_get, c(list("get_variaveis_interesse"), args)), error = function(e) NULL)
      df <- .safe_df(df)
      if (!is.data.frame(df) || nrow(df) == 0) {
        df <- tryCatch(do.call(.safe_get, c(list("get_composicao"), c(args, list(top_n = 2000)))), error = function(e) NULL)
        df <- .safe_df(df)
      }
      if (!is.data.frame(df) || nrow(df) == 0) return(out_empty)

      c_val <- .col_pick_norm(names(df), c("valor", "VALOR", "value", "VALUE"))
      c_lbl <- .col_pick_norm(names(df), c("valor_lbl", "VALOR_LBL", "categoria", "CATEGORIA", "cat", "CAT"))
      c_tot <- .col_pick_norm(names(df), c("n", "N", "total", "TOTAL"))
      if (is.null(c_tot) || (is.null(c_val) && is.null(c_lbl))) return(out_empty)

      tot <- suppressWarnings(as.numeric(df[[c_tot]]))
      ok <- is.finite(tot) & tot >= 0
      if (!any(ok)) return(out_empty)

      val <- if (!is.null(c_val)) trimws(as.character(df[[c_val]])) else rep(NA_character_, nrow(df))
      lbl <- if (!is.null(c_lbl)) as.character(df[[c_lbl]]) else rep(NA_character_, nrow(df))
      lbl_n <- tolower(trimws(lbl))
      lbl_n <- iconv(lbl_n, from = "", to = "ASCII//TRANSLIT")
      lbl_n <- gsub("[^a-z0-9 ]+", " ", lbl_n)
      lbl_n <- gsub("\\s+", " ", lbl_n)

      is_abandono <- val == "2" | grepl("\\babandon", lbl_n)
      abandono <- sum(tot[ok & is_abandono], na.rm = TRUE)
      encerramento_total <- sum(tot[ok], na.rm = TRUE)
      pct <- if (isTRUE(is.finite(encerramento_total)) && encerramento_total > 0) (abandono / encerramento_total) * 100 else NA_real_

      list(interrupcao_trat_pct = pct, abandono = abandono, encerramento_total = encerramento_total)
    }

    scope_pack_interrupcao_trat <- reactive({
      y <- year1()
      n <- nav1()

      cur <- .scope_compute_interrupcao_trat_pct(y, n$level, n$uf, n$mun)
      y0 <- as.integer(y) - 1L
      prev <- if (isTRUE(is.finite(y0))) .scope_compute_interrupcao_trat_pct(y0, n$level, n$uf, n$mun) else NULL

      list(y = y, nav = n, cur = cur, prev = prev)
    })

    # Agregação para 0-14 (casos_0_14 e taxa_det_0_14)
    .agg_0_14_from_df <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(list(casos_0_14 = 0, taxa_det_0_14 = NA_real_))
      }
      cols <- names(df)
      c_cases <- .col_pick_norm(cols, c("casos_0_14", "CASOS_0_14", "valor", "casos_total", "casos", "total", "n"))
      c_taxa  <- .col_pick_norm(cols, c("taxa_det_0_14", "taxa_det", "taxadet", "taxa"))
      if (is.null(c_cases)) return(list(casos_0_14 = 0, taxa_det_0_14 = NA_real_))
      casos_0_14 <- sum(suppressWarnings(as.numeric(df[[c_cases]])), na.rm = TRUE)
      taxa_det_0_14 <- if (!is.null(c_taxa)) {
        vv <- suppressWarnings(as.numeric(df[[c_taxa]]))
        vv <- vv[is.finite(vv)]
        if (length(vv) > 0) mean(vv, na.rm = TRUE) else NA_real_
      } else NA_real_
      list(casos_0_14 = casos_0_14, taxa_det_0_14 = taxa_det_0_14)
    }

    .safe_get_any <- function(fun_names, ...) {
      for (nm in fun_names) {
        out <- tryCatch(do.call(.safe_get, c(list(nm), list(...))), error = function(e) NULL)
        out <- .safe_df(out)
        if (is.data.frame(out) && nrow(out) > 0) return(out)
      }
      data.frame()
    }

    # Cálculo de 0-14 por escopo (BR/UF/MUN)
    .scope_compute_0_14 <- function(y, level, uf = NULL, mun = NULL, selected_macro = NULL, selected_micro = NULL) {
      if (.is_tb_recife_bairro()) {
        dfb <- .tb_recife_bairro_rows(y)
        ages <- .kpi_tb_age_years(dfb$idade_sinan)
        casos <- sum(is.finite(ages) & ages <= 14, na.rm = TRUE)
        return(list(casos_0_14 = casos, taxa_det_0_14 = NA_real_))
      }
      lvl <- .kpi_norm_level(level)
      if (lvl == "BR") {
        df <- .safe_get("get_uf_metrics_0_14", ano = y, year = y, y = y)
        df_std <- .std_uf_metrics_safe(df)
        if (is.null(df_std)) df_std <- df
        out <- .agg_0_14_from_df(df_std)
        return(out)
      }

      u <- .to_uf(uf)
      if (is.na(u) || !nzchar(u)) return(.agg_0_14_from_df(NULL))

      if (lvl == "UF" && is.null(selected_macro) && is.null(selected_micro)) {
        df <- .safe_get("get_uf_metrics_0_14", ano = y, year = y, y = y, uf = u, sg_uf = u)
        df_std <- .std_uf_metrics_safe(df)
        if (is.null(df_std)) df_std <- df
        if (is.data.frame(df_std) && nrow(df_std) > 0 && "uf" %in% names(df_std)) {
          df_std <- df_std[df_std$uf == u, , drop = FALSE]
        }
        out <- .agg_0_14_from_df(df_std)
        return(out)
      }

      dfm <- .safe_get("get_mun_metrics_0_14", ano = y, year = y, uf = u, sg_uf = u)
      dfm_std <- .std_mun_metrics_safe(dfm)
      if (is.null(dfm_std)) dfm_std <- dfm
      
      if (lvl == "MUN") {
        m <- .to_mun7(mun)
        if (!is.na(m) && nzchar(m) && is.data.frame(dfm_std) && nrow(dfm_std) > 0) {
          dfm_std <- dfm_std[dfm_std$cod_mun7 == m, , drop = FALSE]
        }
      } else {
        # UF level with macro/micro filter
        dfm_std <- .filter_muns_by_region(dfm_std, selected_macro, selected_micro)
      }
      out <- .agg_0_14_from_df(dfm_std)
      out
    }

    # Helper para padronizar saída de 0-14 (similar a .std_uf_metrics)
    .std_uf_metrics_safe <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      cols <- names(df)
      uf_col <- .col_pick_norm(cols, c("uf","UF","sigla_uf","SIGLA_UF","sg_uf","SG_UF","geo_id", "municipio", "município", "nome municipio", "nome_municipio", "nome município", "codigo", "código"))
      if (is.null(uf_col)) return(NULL)
      c_cases <- .col_pick_norm(cols, c("casos_0_14", "CASOS_0_14", "valor", "casos_total", "casos", "total"))
      c_taxa  <- .col_pick_norm(cols, c("taxa_det_0_14", "taxa_det", "taxadet", "taxa"))
      out <- data.frame(
        uf = .to_uf(df[[uf_col]]),
        casos_0_14 = if (!is.null(c_cases)) suppressWarnings(as.numeric(df[[c_cases]])) else NA_real_,
        taxa_det_0_14 = if (!is.null(c_taxa)) suppressWarnings(as.numeric(df[[c_taxa]])) else NA_real_,
        stringsAsFactors = FALSE
      )
      out <- out[!is.na(out$uf) & nzchar(out$uf), , drop = FALSE]
      out
    }

    .std_mun_metrics_safe <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      cols <- names(df)
      key_col <- .col_pick_norm(cols, c("cod_mun7","COD_MUN7","geo_id","GEO_ID","CD_MUN","cd_mun","codigo"))
      if (is.null(key_col)) return(NULL)
      uf_col <- .col_pick_norm(cols, c("uf","UF","sigla_uf","SIGLA_UF","sg_uf","SG_UF","geo_id"))
      c_cases <- .col_pick_norm(cols, c("casos_0_14", "CASOS_0_14", "valor", "casos_total", "casos", "total"))
      c_taxa  <- .col_pick_norm(cols, c("taxa_det_0_14", "taxa_det", "taxadet", "taxa"))
      out <- data.frame(
        cod_mun7 = .to_mun7(df[[key_col]]),
        uf = if (!is.null(uf_col)) .to_uf(df[[uf_col]]) else NA_character_,
        casos_0_14 = if (!is.null(c_cases)) suppressWarnings(as.numeric(df[[c_cases]])) else NA_real_,
        taxa_det_0_14 = if (!is.null(c_taxa)) suppressWarnings(as.numeric(df[[c_taxa]])) else NA_real_,
        stringsAsFactors = FALSE
      )
      out <- out[!is.na(out$cod_mun7) & nzchar(out$cod_mun7), , drop = FALSE]
      out
    }

    # Scope pack com 0-14 (com comparação mensal)
    scope_pack_0_14 <- reactive({
      y <- year1()
      n <- nav1()

      # Tentar comparação mensal primeiro
      cmp <- .kpi_period_compare_metric("cases_0_14", y, n, value_mode = "sum")
      
      if (!is.null(cmp) && !is.null(cmp$cur) && !is.null(cmp$prev)) {
        annual_cur <- .scope_compute_0_14(y, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro)
        annual_prev <- if (isTRUE(is.finite(y - 1L))) .scope_compute_0_14(y - 1L, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro) else NULL

        # Usar dados mensais de comparação
        cur <- list(
          casos_0_14 = cmp$cur,
          taxa_det_0_14 = annual_cur$taxa_det_0_14
        )
        prev <- list(
          casos_0_14 = cmp$prev,
          taxa_det_0_14 = if (is.null(annual_prev)) NA_real_ else annual_prev$taxa_det_0_14
        )
        return(list(y = y, nav = n, cur = cur, prev = prev, last_month = cmp$last_month))
      }
      
      # Fallback: comparação anual completa
      cur <- .scope_compute_0_14(y, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro)
      y0 <- as.integer(y) - 1L
      prev <- if (isTRUE(is.finite(y0))) .scope_compute_0_14(y0, n$level, n$uf, n$mun, n$selected_macro, n$selected_micro) else NULL

      list(y = y, nav = n, cur = cur, prev = prev, last_month = NULL)
    })

    ctx_label <- reactive({
      sp <- scope_pack()
      n <- sp$nav; y <- sp$y
      if (!is.null(n$selected_bairro_key) && nzchar(n$selected_bairro_key)) return(paste0("Recife \u2022 ", n$selected_bairro_name %||% n$selected_bairro_key, " \u2022 ", y))
      if (n$level == "BR") return(paste0("Brasil \u2022 ", y))
      if (n$level == "UF") {
        base <- paste0(n$uf, " \u2022 ", y)
        if (!is.null(n$selected_micro) && nzchar(n$selected_micro)) {
          lk <- .pe_lookup_cache()
          nm <- if (!is.null(lk)) unique(lk$nome_micro[lk$micro_key == n$selected_micro])[1] else n$selected_micro
          return(paste0(n$uf, " \u2022 ", nm %||% n$selected_micro, " \u2022 ", y))
        }
        if (!is.null(n$selected_macro) && nzchar(n$selected_macro)) {
          lk <- .pe_lookup_cache()
          nm <- if (!is.null(lk)) unique(lk$nome_macro[lk$macro_key == n$selected_macro])[1] else n$selected_macro
          return(paste0(n$uf, " \u2022 ", nm %||% n$selected_macro, " \u2022 ", y))
        }
        return(base)
      }
      paste0("Recife \u2022 ", y)
    })

    # -------------------------------
    # Métrica global (clicar KPI -> atualiza estado + publica cor)
    # -------------------------------
    sel_metric <- reactiveVal("incid")

    .publish_metric_color <- function(metric_key) {
      col <- .pack_color(metric_key)
      if (inherits(state, "reactivevalues")) {
        state[["metric_color"]] <- col
        t0 <- .scalar_int(tryCatch(state[["metric_color_tick"]], error = function(e) NULL), 0L)
        state[["metric_color_tick"]] <- t0 + 1L
      }
      invisible(col)
    }

    .push_metric_to_state <- function(key) {
      k <- key
      if (is.list(k) && !is.null(k$key)) k <- k$key
      k <- .kpi_metric_key_norm(.scalar_chr(k, "incid"))
      allowed <- c("cases", "incid", "obitos", "cura", "pop")
      if (!isTRUE(k %in% allowed)) k <- "incid"

      sel_metric(k)

      if (inherits(state, "reactivevalues")) {
        state[["metric"]] <- k
        tick_raw <- tryCatch(state[["metric_tick"]], error = function(e) NULL)
        tick <- .scalar_int(tick_raw, default = 0L)
        state[["metric_tick"]] <- tick + 1L
      }

      .publish_metric_color(k)
      invisible(k)
    }

    observeEvent(TRUE, {
      .push_metric_to_state("incid")
    }, once = TRUE, ignoreInit = FALSE)

    # clique na KPI
    observeEvent(input$kpi_select, {
      info <- input$kpi_select
      key  <- if (is.list(info) && !is.null(info$key)) info$key else info
      .push_metric_to_state(key %||% "incid")
      logw("metric_select", paste0("metric=", sel_metric()))
    }, ignoreInit = TRUE)

    # sync reverso: se outro módulo mudar state$metric, reflete no KPI e republlica cor
    observeEvent({
      if (inherits(state, "reactivevalues")) {
        .obj_get(state, "metric_tick", 0L)
        .obj_get(state, "metric", NULL)
      } else NULL
    }, {
      k <- .kpi_metric_key_norm(.obj_get(state, "metric", "incid"))
      if (!identical(sel_metric(), k)) {
        sel_metric(k)
        .publish_metric_color(k)
        logw("metric_sync", paste0("metric=", k))
      }
    }, ignoreInit = TRUE)

    # -------------------------------
    # Render cards
    # -------------------------------
    .is_tuberculose <- function() {
      d0 <- .as_chr1(.obj_get(.get_pack(), "DOENCA", NULL), "")
      d0 <- toupper(trimws(as.character(d0 %||% "")))
      d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")
    }

    # Verifica se o disease pack define um KPI específico via KPI_LABELS
    .has_kpi <- function(key) {
      labs <- .obj_get(.get_pack(), "KPI_LABELS", list()) %||% list()
      !is.null(labs[[key]]) && nzchar(labs[[key]])
    }

    output$kpi_update_info <- renderUI({
      doenca0 <- toupper(.as_chr1(.obj_get(.get_pack(), "DOENCA", NULL), "TUBERCULOSE"))
      fr <- tryCatch(.kpi_read_data_freshness(cfg_ref, doenca = doenca0), error = function(e) NULL)
      if (!is.data.frame(fr) || nrow(fr) == 0) return(NULL)

      dados_ate <- suppressWarnings(as.Date(fr$dados_ate[[1]]))
      processado_em <- suppressWarnings(as.Date(fr$processado_em[[1]]))
      if (is.na(dados_ate) && is.na(processado_em)) return(NULL)

      div(
        class = "kpi-update-info",
        if (!is.na(dados_ate)) tags$span(HTML(sprintf("Dados até <b>%s</b>", format(dados_ate, "%d/%m/%Y")))),
        if (!is.na(processado_em)) tags$span(HTML(sprintf("Processado em <b>%s</b>", format(processado_em, "%d/%m/%Y"))))
      )
    })

    output$kpi_cases <- renderUI({
      sp <- scope_pack()
      cur  <- sp$cur$cases
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$cases

      delta_label <- "vs ano anterior"
      period_cmp <- .cases_period_compare(sp$y, sp$nav)
      if (!is.null(period_cmp) && isTRUE(is.finite(period_cmp$last_month)) && period_cmp$last_month < 12L) {
        cur <- period_cmp$cur
        prev <- period_cmp$prev
        delta_label <- "vs mesmo período do ano anterior"
      }

      .kpi_make_card(
        key = "cases",
        title = .pack_label("cases", "Casos novos"),
        value = .kpi_fmt_int(cur),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = FALSE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("activity", size = "18px"),
        accent = .pack_color("cases"),
        selected = identical(sel_metric(), "cases"),
        clickable = TRUE
      )
    })

    output$kpi_cases_0_14 <- renderUI({
      if (!.has_kpi("cases_0_14")) return(NULL)
      sp <- scope_pack_0_14()
      cur  <- sp$cur$casos_0_14
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$casos_0_14

      delta_label <- "vs ano anterior"
      period_cmp <- .kpi_period_compare_metric("cases_0_14", sp$y, sp$nav, value_mode = "sum")
      if (!is.null(period_cmp) && isTRUE(is.finite(period_cmp$last_month)) && period_cmp$last_month < 12L) {
        cur <- period_cmp$cur
        prev <- period_cmp$prev
        delta_label <- "vs mesmo período do ano anterior"
      }

      title_raw <- .pack_label("cases_0_14", "Casos novos (0-14 anos)")
      if (!grepl("0\\s*(a|-)?\\s*14", title_raw, ignore.case = TRUE)) {
        title_raw <- paste0(title_raw, " (0 a 14 anos)")
      }
      title_html <- HTML(gsub("\\s*\\(?0\\s*(a|-)?\\s*14\\s*anos\\)?", "<br>(0 a 14 anos)", title_raw, ignore.case = TRUE))

      .kpi_make_card(
        key = "cases_0_14",
        title = title_html,
        value = .kpi_fmt_int(cur),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = FALSE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("people", size = "18px"),
        accent = .pack_color("cases_0_14"),
        selected = FALSE,
        clickable = FALSE
      )
    })

    output$kpi_taxa_det_0_14 <- renderUI({
      if (!.has_kpi("taxa_det_0_14")) return(NULL)
      sp <- scope_pack_0_14()
      cur  <- sp$cur$taxa_det_0_14
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$taxa_det_0_14

      delta_label <- "vs ano anterior"

      title_raw <- .pack_label("taxa_det_0_14", "Taxa de detecção (0 a 14 anos)")
      if (!grepl("0\\s*(a|-)?\\s*14", title_raw, ignore.case = TRUE)) {
        title_raw <- paste0(title_raw, " (0 a 14 anos)")
      }
      title_html <- HTML(gsub("\\s*\\(?0\\s*(a|-)?\\s*14\\s*anos\\)?", "<br>(0 a 14 anos)", title_raw, ignore.case = TRUE))

      .kpi_make_card(
        key = "taxa_det_0_14",
        title = title_html,
        value = if (!isTRUE(is.finite(cur))) "NA" else .kpi_fmt_num(cur, 2),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("speedometer2", size = "18px"),
        accent = .pack_color("taxa_det_0_14"),
        selected = FALSE,
        clickable = FALSE
      )
    })

    output$kpi_incid <- renderUI({
      sp <- scope_pack()
      cur  <- sp$cur$incid
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$incid

      delta_label <- "vs ano anterior"
      period_cmp <- .cases_period_compare(sp$y, sp$nav)
      if (!is.null(period_cmp) && isTRUE(is.finite(period_cmp$last_month)) && period_cmp$last_month < 12L) {
        pop_cur <- sp$cur$pop
        pop_prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$pop
        if (isTRUE(is.finite(pop_cur)) && pop_cur > 0 && isTRUE(is.finite(pop_prev)) && pop_prev > 0) {
          cur <- (period_cmp$cur / pop_cur) * 1e5
          prev <- (period_cmp$prev / pop_prev) * 1e5
          delta_label <- "vs mesmo período do ano anterior"
        }
      }

      title_raw <- .pack_label("incid", "Incidência")
      title_html <- if (grepl("taxa\\s+de\\s+detec", title_raw, ignore.case = TRUE)) {
        HTML(gsub("taxa\\s+de\\s+detec[a-zçãõ]*", "Taxa de detecção<br>", title_raw, ignore.case = TRUE))
      } else {
        title_raw
      }

      .kpi_make_card(
        key = "incid",
        title = title_html,
        value = if (!isTRUE(is.finite(cur))) "NA" else .kpi_fmt_num(cur, 2),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("speedometer2", size = "18px"),
        accent = .pack_color("incid"),
        selected = identical(sel_metric(), "incid"),
        clickable = TRUE,
        note = if (.is_tb_recife_bairro()) .kpi_pop_estimada_note() else NULL
      )
    })

    output$kpi_obitos <- renderUI({
      sp <- scope_pack()
      cur  <- sp$cur$obitos
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$obitos

      delta_label <- "vs ano anterior"
      period_cmp <- .kpi_period_compare_metric("obitos", sp$y, sp$nav, value_mode = "sum")
      if (!is.null(period_cmp) && isTRUE(is.finite(period_cmp$last_month)) && period_cmp$last_month < 12L) {
        cur <- period_cmp$cur
        prev <- period_cmp$prev
        delta_label <- "vs mesmo período do ano anterior"
      }

      .kpi_make_card(
        key = "obitos",
        title = .pack_label("obitos", "Óbitos"),
        value = if (!isTRUE(is.finite(cur))) "NA" else .kpi_fmt_int(cur),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = FALSE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("heartbreak", size = "18px"),
        accent = .pack_color("obitos"),
        selected = identical(sel_metric(), "obitos"),
        clickable = TRUE
      )
    })

    output$kpi_cura <- renderUI({
      sp <- scope_pack()
      cur  <- sp$cur$cura
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$cura

      delta_label <- "vs ano anterior"

      .kpi_make_card(
        key = "cura",
        title = .pack_label("cura", "Curas entre casos novos"),
        value = if (!isTRUE(is.finite(cur))) "NA" else .kpi_fmt_int(cur),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = FALSE, good_when_decrease = FALSE, label_suffix = delta_label),
        icon = bsicons::bs_icon("patch-check", size = "18px"),
        accent = .pack_color("cura"),
        selected = identical(sel_metric(), "cura"),
        clickable = TRUE
      )
    })

    output$kpi_pop <- renderUI({
      sp <- scope_pack()
      cur <- sp$cur$pop

      .kpi_make_card(
        key = "pop",
        title = .pack_label("pop", "População"),
        value = .kpi_fmt_int(cur),
        subtitle = ctx_label(),
        delta = NULL,
        icon = bsicons::bs_icon("people", size = "18px"),
        accent = .pack_color("pop"),
        selected = identical(sel_metric(), "pop"),
        clickable = FALSE
      )
    })

    output$kpi_mortalidade <- renderUI({
      if (!.has_kpi("mortalidade")) return(NULL)
      sp <- scope_pack()
      cur  <- sp$cur$mortalidade
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$mortalidade

      delta_label <- "vs ano anterior"

      .kpi_make_card(
        key = "mortalidade",
        title = .pack_label("mortalidade", "Taxa de mortalidade"),
        value = if (!isTRUE(is.finite(cur))) "NA" else .kpi_fmt_num(cur, 1),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("graph-down", size = "18px"),
        accent = .pack_color("mortalidade"),
        selected = FALSE,
        clickable = FALSE,
        note = if (.is_tb_recife_bairro()) .kpi_pop_estimada_note() else NULL
      )
    })

    output$kpi_letalidade <- renderUI({
      if (!.has_kpi("letalidade")) return(NULL)
      sp <- scope_pack()
      cur  <- sp$cur$letalidade
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$letalidade

      delta_label <- "vs ano anterior"

      .kpi_make_card(
        key = "letalidade",
        title = .pack_label("letalidade", "Letalidade"),
        value = if (!isTRUE(is.finite(cur))) "NA" else paste0(.kpi_fmt_num(cur, 1), "%"),
        subtitle = ctx_label(),
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = delta_label),
        icon = bsicons::bs_icon("percent", size = "18px"),
        accent = .pack_color("letalidade"),
        selected = FALSE,
        clickable = FALSE
      )
    })

    output$kpi_hiv_pos_pct <- renderUI({
      if (!.has_kpi("hiv_pos_pct") || !.is_tuberculose()) return(NULL)
      sp <- scope_pack_hiv()
      cur  <- sp$cur$hiv_pos_pct
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$hiv_pos_pct

      tested_cur <- sp$cur$hiv_tested
      tested_txt <- if (isTRUE(is.finite(tested_cur))) paste0("Testados: ", .kpi_fmt_int(tested_cur)) else NULL
      subtitle_txt <- if (!is.null(tested_txt)) paste0(ctx_label(), " • ", tested_txt) else ctx_label()

      .kpi_make_card(
        key = "hiv_pos_pct",
        title = .pack_label("hiv_pos_pct", "HIV positivo na testagem (%)"),
        value = if (!isTRUE(is.finite(cur))) "NA" else paste0(.kpi_fmt_num(cur, 1), "%"),
        subtitle = subtitle_txt,
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = "vs ano anterior"),
        icon = bsicons::bs_icon("percent", size = "18px"),
        accent = .pack_color("hiv_pos_pct"),
        selected = FALSE,
        clickable = FALSE
      )
    })

    output$kpi_interrupcao_trat_pct <- renderUI({
      if (!.has_kpi("interrupcao_trat_pct") || !.is_tuberculose()) return(NULL)
      sp <- scope_pack_interrupcao_trat()
      cur  <- sp$cur$interrupcao_trat_pct
      prev <- if (is.null(sp$prev)) NA_real_ else sp$prev$interrupcao_trat_pct

      abandono_cur <- sp$cur$abandono
      total_cur <- sp$cur$encerramento_total
      detalhe <- if (isTRUE(is.finite(abandono_cur)) && isTRUE(is.finite(total_cur))) {
        paste0("Abandono: ", .kpi_fmt_int(abandono_cur), " de ", .kpi_fmt_int(total_cur))
      } else {
        NULL
      }
      subtitle_txt <- if (!is.null(detalhe)) paste0(ctx_label(), " - ", detalhe) else ctx_label()

      .kpi_make_card(
        key = "interrupcao_trat_pct",
        title = .pack_label("interrupcao_trat_pct", "Interrupcao de tratamento (%)"),
        value = if (!isTRUE(is.finite(cur))) "NA" else paste0(.kpi_fmt_num(cur, 1), "%"),
        subtitle = subtitle_txt,
        delta = .kpi_delta_badge(cur, prev, is_rate = TRUE, good_when_decrease = TRUE, label_suffix = "vs ano anterior"),
        icon = bsicons::bs_icon("slash-circle", size = "18px"),
        accent = .pack_color("interrupcao_trat_pct"),
        selected = FALSE,
        clickable = FALSE
      )
    })

    invisible(list(
      metric = reactive(sel_metric())
    ))
  })
}
