# core/R/modules/mod_charts.R

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(rlang)
  library(bslib)
  library(echarts4r)
  library(htmlwidgets)
  library(arrow)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.is_true <- function(x) isTRUE(x) || identical(toupper(trimws(as.character(x %||% ""))), "TRUE") || identical(as.character(x %||% ""), "1")
 

# -------------------------------------------------------------------------
# Utils
# -------------------------------------------------------------------------

.res <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.function(x)) return(tryCatch(x(), error = function(e) NULL))
  x
}

.is_rv <- function(x) inherits(x, "reactivevalues")

.obj_get <- function(obj, key, default = NULL) {
  obj <- .res(obj)
  if (is.null(obj)) return(default)
  if (.is_rv(obj)) return(tryCatch(obj[[key]] %||% default, error = function(e) default))
  if (is.list(obj)) return(obj[[key]] %||% default)
  if (is.environment(obj) && exists(key, envir = obj, inherits = FALSE)) {
    return(get(key, envir = obj, inherits = FALSE) %||% default)
  }
  default
}

.as_int1 <- function(x, default = NA_integer_) {
  x <- .res(x)
  v <- suppressWarnings(as.integer(x))
  if (length(v) == 0) return(default)
  v <- v[!is.na(v)]
  if (length(v) == 0) return(default)
  out <- v[[1]]
  if (!is.finite(out)) default else out
}

.as_chr1 <- function(x, default = NULL) {
  x <- .res(x)
  if (is.null(x) || length(x) == 0) return(default)
  v <- suppressWarnings(as.character(x[1]))
  if (is.na(v) || !nzchar(v)) default else v
}

.pick <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) {
    nms_u  <- toupper(nms)
    cand_u <- toupper(candidates)
    j <- match(cand_u, nms_u)
    j <- j[is.finite(j)]
    if (length(j) == 0) return(NULL)
    return(nms[[j[[1]]]])
  }
  hit[[1]]
}


.safe_df <- function(x) {
  if (is.null(x)) return(data.frame())
  if (inherits(x, "tbl")) return(as.data.frame(x))
  if (!is.data.frame(x)) return(data.frame())
  x
}

.msg_panel <- function(text) {
  div(
    style = "height:100%; display:flex; align-items:center; justify-content:center; color:#666; font-weight:600;",
    text
  )
}

.norm_uf <- function(x) {
  v <- .as_chr1(x, NULL)
  if (is.null(v)) return(NULL)
  v <- toupper(trimws(v))
  if (!nzchar(v)) NULL else v
}
 
.mun_code_digits <- function(x) {
  v <- as.character(x %||% "")
  v <- gsub("\\D+", "", v)
  if (!nzchar(v)) return(NA_character_)
  if (nchar(v) > 7) v <- substr(v, 1, 7)
  if (nchar(v) < 6) return(NA_character_)
  v
}

.norm_mun_digits <- function(x) {
  v <- .as_chr1(x, NULL)
  if (is.null(v)) return(NULL)
  v <- gsub("\\D+", "", v)
  if (!nzchar(v)) return(NULL)
  if (nchar(v) > 7) v <- substr(v, 1, 7)
  v
}

.mun_code_6 <- function(x) {
  v <- .norm_mun_digits(x)
  if (is.null(v) || !nzchar(v)) return(NULL)
  if (nchar(v) >= 6) substr(v, 1, 6) else NULL
}

.mun_code_7 <- function(x) {
  v <- .norm_mun_digits(x)
  if (is.null(v) || !nzchar(v)) return(NULL)
  if (nchar(v) == 7) v else NULL
}

.scope_level_candidates <- function(level) {
  lvl <- toupper(as.character(level %||% "UF"))
  if (lvl %in% c("MUN", "MUNICIPIO", "MUNICIPIOS", "MUNIN", "MUN_INC", "MUN_IN")) {
    return(c("MUN", "MUNIN", "MUNICIPIO", "MUNICIPIOS", "MUN_INC", "MUN_IN"))
  }
  if (lvl %in% c("UF", "ESTADO", "STATE")) return(c("UF"))
  if (lvl %in% c("BR", "BRASIL", "BRAZIL")) return(c("BR"))
  c(lvl)
}

.normalize_scope <- function(level, uf = NULL, mun = NULL) {
  lvl <- toupper(as.character(level %||% "UF"))
  if (!lvl %in% c("BR", "UF", "MUN")) lvl <- "UF"

  mun_raw <- if (lvl == "MUN") .norm_mun_digits(mun) else NULL
  mun6 <- if (!is.null(mun_raw) && nchar(mun_raw) >= 6) substr(mun_raw, 1, 6) else NULL
  mun7 <- if (!is.null(mun_raw) && nchar(mun_raw) == 7) mun_raw else NULL

  list(
    level = lvl,
    uf    = if (lvl %in% c("UF", "MUN")) .norm_uf(uf) else NULL,
    mun   = mun_raw,
    mun6  = mun6,
    mun7  = mun7
  )
}


.ts_value_col <- function(metric_key, ts_names) {
  metric_key <- .norm_key(metric_key)
  ts_names_u <- toupper(ts_names)

  want <- switch(
    metric_key,
    "cases"  = "CASOS",
    "obitos" = "CASOS_OBITOS",
    "cura"   = "CASOS_CURA",
    "incid"  = "INCID_100K",
    NA_character_
  )

  if (is.na(want)) return(NULL)

  idx <- match(want, ts_names_u)
  if (!is.na(idx)) ts_names[[idx]] else NULL
}



.find_dashboard_dir <- function() {
  opt <- tryCatch(getOption("sinan.dashboard_dir", NULL), error = function(e) NULL)
  if (!is.null(opt) && nzchar(opt) && dir.exists(opt)) {
    return(normalizePath(opt, winslash = "/", mustWork = FALSE))
  }
  env <- Sys.getenv("SINAN_DASHBOARD_DIR", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }

  wd <- tryCatch(getwd(), error = function(e) ".")
  wd <- tryCatch(normalizePath(wd, winslash = "/", mustWork = FALSE), error = function(e) wd)

  cur <- wd
  for (i in 0:10) {
    cand <- file.path(cur, "data", "parquet")
    if (dir.exists(cand)) return(cand)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  NULL
}

.charts_name_key <- function(x) {
  x <- as.character(x %||% "")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  ifelse(nzchar(x), x, NA_character_)
}

.charts_recife_bairro_key <- function(x) {
  k <- .charts_name_key(x)
  k[!is.na(k) & k == "BAIRRO DO RECIFE"] <- "RECIFE"
  k[!is.na(k) & k == "POCO DA PANELA"] <- "POCO"
  k
}

.charts_read_tb_recife_raw <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL) {
    dash <- .as_chr1(.obj_get(cfg, "DASHBOARD_DIR", NULL), NULL) %||%
      .as_chr1(tryCatch(.find_dashboard_dir(), error = function(e) NULL), NULL)
    if (is.null(dash) || !nzchar(dash)) return(data.frame())

    dir <- file.path(dash, "sinan", "TUBERCULOSE")
    if (!dir.exists(dir)) return(data.frame())

    files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0) return(data.frame())

    mt <- suppressWarnings(max(as.numeric(file.info(files)$mtime), na.rm = TRUE))
    if (!is.finite(mt)) mt <- 0
    key <- paste0(dir, "|", length(files), "|", mt)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))

    parts <- lapply(files, function(fp) {
      tryCatch({
        df <- as.data.frame(arrow::read_parquet(fp))
        names(df) <- toupper(names(df))
        df
      }, error = function(e) NULL)
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0) return(data.frame())

    out <- dplyr::bind_rows(parts)
    if (is.null(out) || nrow(out) == 0) return(data.frame())

    assign(key, out, envir = cache)
    out
  }
})

.charts_read_recife_bairro_incidence <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL) {
    dash <- .as_chr1(.obj_get(cfg, "DASHBOARD_DIR", NULL), NULL) %||%
      .as_chr1(tryCatch(.find_dashboard_dir(), error = function(e) NULL), NULL)
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

.read_tb_recife_bairro_rank <- function(cfg = NULL, year = NULL, top_n = 15L) {
  raw <- .charts_read_recife_bairro_incidence(cfg)
  if (!is.data.frame(raw) || nrow(raw) == 0) return(data.frame())

  yy <- suppressWarnings(as.integer(year))
  yy <- if (length(yy) > 0 && is.finite(yy[[1]])) yy[[1]] else NA_integer_
  tn <- suppressWarnings(as.integer(top_n))
  if (!is.finite(tn) || tn <= 0) tn <- 15L

  df <- raw
  if (is.finite(yy) && "ano" %in% names(df)) {
    df <- df[suppressWarnings(as.integer(df$ano)) == yy, , drop = FALSE]
  }
  if (nrow(df) == 0) return(data.frame())

  out <- data.frame(
    cat = as.character(df$bairro_nome),
    key = as.character(df$bairro_key),
    val = suppressWarnings(as.numeric(df$casos_total)),
    uf = "PE",
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$val) & nzchar(out$cat) & nzchar(out$key), , drop = FALSE]
  out <- out[order(-out$val), , drop = FALSE]
  utils::head(out, tn)
}

# Memoizado: a derivacao abaixo (normalizacao de acentos em ~27 mil nomes de
# bairro + dois regex sobre DT_NOTIFIC) custava ~180-215 ms e rodava de novo a
# CADA chamada. Ha 6 pontos de chamada em mod_charts, todos disparados por um
# clique em bairro -- dai o clique de segundos. O resultado depende so de `raw`,
# que ja e cacheado, entao chavear pela identidade do raw e suficiente.
.read_tb_recife_detail <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL) {
  raw <- .charts_read_tb_recife_raw(cfg)
  if (!is.data.frame(raw) || nrow(raw) == 0 || !"NM_BAIRRO" %in% names(raw)) return(data.frame())

  .ck <- paste0("detail|", nrow(raw), "|", ncol(raw))
  if (exists(.ck, envir = cache, inherits = FALSE)) return(get(.ck, envir = cache, inherits = FALSE))

  raw$ano <- suppressWarnings(as.integer(raw$NU_ANO))
  raw$bairro <- trimws(enc2utf8(as.character(raw$NM_BAIRRO)))
  raw$bairro_key <- .charts_recife_bairro_key(raw$bairro)
  dt <- if ("DT_NOTIFIC" %in% names(raw)) trimws(as.character(raw$DT_NOTIFIC)) else NA_character_
  raw$mes <- suppressWarnings(as.integer(NA))
  iso <- grepl("^\\d{4}-\\d{2}", dt)
  raw$mes[iso] <- suppressWarnings(as.integer(substr(dt[iso], 6, 7)))
  br <- grepl("^\\d{2}/\\d{2}/\\d{4}$", dt)
  raw$mes[br] <- suppressWarnings(as.integer(substr(dt[br], 4, 5)))
  raw <- raw[!is.na(raw$bairro_key) & nzchar(raw$bairro_key), , drop = FALSE]
  assign(.ck, raw, envir = cache)
  raw
  }
})

# Memoizado por (bairro, ano). O recorte em si e barato, mas cada chamada
# passava antes pelo .read_tb_recife_detail; com os dois caches, o 2o ao 6o
# grafico de um clique saem de graca. Os filtros sao identicos aos originais,
# inclusive na semantica de NA -- isto e mudanca de performance, nao de numero.
.tb_recife_local_rows <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL, bairro_key = NULL, year = NULL) {
    df <- .read_tb_recife_detail(cfg)
    if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

    key <- as.character(bairro_key %||% "")
    yy <- suppressWarnings(as.integer(year))
    yy1 <- if (length(yy) > 0 && is.finite(yy[[1]])) yy[[1]] else NA_integer_
    .ck <- paste0("rows|", nrow(df), "|", key, "|", yy1)
    if (exists(.ck, envir = cache, inherits = FALSE)) return(get(.ck, envir = cache, inherits = FALSE))

    # Teto simples: 94 bairros x 16 anos cabem, mas nao deixamos crescer sem fim.
    if (length(ls(envir = cache, all.names = TRUE)) > 600L) rm(list = ls(envir = cache, all.names = TRUE), envir = cache)

    if (nzchar(key)) df <- df[df$bairro_key == key, , drop = FALSE]
    if (is.finite(yy1)) df <- df[df$ano == yy1, , drop = FALSE]

    assign(.ck, df, envir = cache)
    df
  }
})

# Decodifica NU_IDADE_N (SINAN): 1o digito = unidade (4=anos,3=meses,2=dias,1=horas),
# ultimos 3 digitos = valor. Mesma logica de _decode_uXXX_to_years em build_piramides.py.
.decode_nu_idade_n_years <- function(x) {
  n <- suppressWarnings(as.integer(trimws(as.character(x))))
  out <- rep(NA_real_, length(n))
  ok <- !is.na(n)
  if (any(ok)) {
    unit <- n[ok] %/% 1000L
    val  <- n[ok] %% 1000L
    yrs <- rep(NA_real_, length(unit))
    yrs[unit == 4] <- val[unit == 4]
    yrs[unit == 3] <- val[unit == 3] / 12.0
    yrs[unit == 2] <- val[unit == 2] / 365.25
    yrs[unit == 1] <- val[unit == 1] / (24.0 * 365.25)
    out[ok] <- yrs
  }
  out
}

.tb_recife_pyr_evt <- function(cfg = NULL, bairro_key = NULL, year = NULL) {
  df <- .tb_recife_local_rows(cfg, bairro_key, year)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())

  c_sexo  <- .pick(names(df), c("CS_SEXO", "cs_sexo"))
  c_idade <- .pick(names(df), c("NU_IDADE_N", "nu_idade_n"))
  if (is.null(c_sexo) || is.null(c_idade)) return(data.frame())

  sexo <- toupper(trimws(as.character(df[[c_sexo]])))
  idade_years <- .decode_nu_idade_n_years(df[[c_idade]])
  faixa_ord <- suppressWarnings(as.integer(floor(idade_years)))

  out <- data.frame(sexo = sexo, faixa_ord = faixa_ord, valor = 1L, stringsAsFactors = FALSE)
  out <- out[
    out$sexo %in% c("M", "F") &
      is.finite(out$faixa_ord) & out$faixa_ord >= 0 & out$faixa_ord <= 130,
    , drop = FALSE
  ]
  if (nrow(out) == 0) return(data.frame())

  out %>%
    dplyr::group_by(.data$sexo, .data$faixa_ord) %>%
    dplyr::summarise(valor = sum(.data$valor), .groups = "drop") %>%
    as.data.frame()
}

.read_sinan_dict <- local({
  cache <- new.env(parent = emptyenv())

  function(cfg = NULL) {
    dash <- .as_chr1(.obj_get(cfg, "DASHBOARD_DIR", NULL), NULL) %||%
      .as_chr1(tryCatch(.find_dashboard_dir(), error = function(e) NULL), NULL)
    if (is.null(dash) || !nzchar(dash)) return(NULL)

    dir <- file.path(dash, "sinan_dict", "doenca=TUBERCULOSE")
    if (!dir.exists(dir)) return(NULL)

    files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0) return(NULL)

    mt <- suppressWarnings(max(as.numeric(file.info(files)$mtime), na.rm = TRUE))
    if (!is.finite(mt)) mt <- 0
    key <- paste0(dir, "|", length(files), "|", mt)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))

    parts <- lapply(files, function(fp) {
      tryCatch(as.data.frame(arrow::read_parquet(fp, col_select = c("variavel", "valor", "valor_lbl"))), error = function(e) NULL)
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0) return(NULL)

    out <- do.call(rbind, parts)
    if (is.null(out) || nrow(out) == 0) return(NULL)

    out$variavel <- toupper(trimws(as.character(out$variavel)))
    out$valor <- trimws(as.character(out$valor))
    out$valor_lbl <- as.character(out$valor_lbl)
    out <- out[!duplicated(out[, c("variavel", "valor")]), , drop = FALSE]

    assign(key, out, envir = cache)
    out
  }
})

.decode_tb_recife_categoria <- function(cfg, var_key, valores) {
  dict <- tryCatch(.read_sinan_dict(cfg), error = function(e) NULL)
  if (!is.data.frame(dict) || nrow(dict) == 0) return(valores)

  sub <- dict[dict$variavel == toupper(var_key), c("valor", "valor_lbl"), drop = FALSE]
  if (nrow(sub) == 0) return(valores)

  m <- match(valores, sub$valor)
  out <- valores
  hit <- !is.na(m) & !is.na(sub$valor_lbl[m]) & nzchar(trimws(sub$valor_lbl[m]))
  out[hit] <- sub$valor_lbl[m[hit]]
  out
}

.tb_recife_comp_df <- function(cfg = NULL, bairro_key = NULL, year = NULL, var_sel = NULL, top_n = 800) {
  v <- as.character(var_sel %||% "")
  if (!nzchar(v)) return(data.frame())
  df <- .tb_recife_local_rows(cfg, bairro_key, year)
  if (!is.data.frame(df) || nrow(df) == 0 || !(v %in% names(df))) return(data.frame())
  val <- trimws(as.character(df[[v]]))
  val[is.na(val) | !nzchar(val)] <- "Ignorado/Branco"
  val <- .decode_tb_recife_categoria(cfg, v, val)

  # table() com uma única categoria perde a classe "table" ao passar por sort(),
  # o que faz as.data.frame() devolver 1 coluna em vez de 2 (categoria/total).
  # Por isso convertemos para data.frame antes de ordenar, nunca depois.
  out <- as.data.frame(table(val), stringsAsFactors = FALSE)
  names(out) <- c("categoria", "total")
  out <- out[order(-out$total), , drop = FALSE]
  out$variavel <- v
  out$total <- suppressWarnings(as.numeric(out$total))
  tn <- suppressWarnings(as.integer(top_n)); if (!is.finite(tn) || tn <= 0) tn <- 800L
  out <- out[is.finite(out$total) & out$total > 0, c("variavel", "categoria", "total"), drop = FALSE]
  utils::head(out, tn)
}

.tb_recife_year_series <- function(cfg = NULL, bairro_key = NULL) {
  df <- .tb_recife_local_rows(cfg, bairro_key, NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  df <- df[is.finite(df$ano), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())
  out <- stats::aggregate(list(casos = rep(1, nrow(df))), by = list(x = as.character(df$ano)), FUN = sum)
  out$ano_ord <- suppressWarnings(as.integer(out$x))
  out <- out[order(out$ano_ord), , drop = FALSE]
  out
}

.tb_recife_month_series <- function(cfg = NULL, bairro_key = NULL, years = NULL) {
  df <- .tb_recife_local_rows(cfg, bairro_key, NULL)
  if (!is.data.frame(df) || nrow(df) == 0) return(data.frame())
  yy <- suppressWarnings(as.integer(years))
  yy <- yy[is.finite(yy)]
  if (length(yy) > 0) df <- df[df$ano %in% yy, , drop = FALSE]
  df <- df[is.finite(df$ano) & is.finite(df$mes) & df$mes >= 1 & df$mes <= 12, , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())
  df$periodo <- sprintf("%04d-%02d", df$ano, df$mes)
  out <- stats::aggregate(list(casos = rep(1, nrow(df))), by = list(periodo = df$periodo), FUN = sum)
  out <- out[order(out$periodo), , drop = FALSE]
  out
}
.open_incidence_ds <- function() {
  dash <- .find_dashboard_dir()
  wd <- tryCatch(getwd(), error = function(e) ".")
  wd <- tryCatch(normalizePath(wd, winslash = "/", mustWork = FALSE), error = function(e) wd)

  candidates <- character(0)
  if (!is.null(dash)) candidates <- c(candidates, file.path(dash, "incidence"))

  cur <- wd
  for (i in 0:10) {
    candidates <- c(candidates, file.path(cur, "data", "parquet", "canonical", "incidence"))
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  candidates <- unique(candidates)
  for (p in candidates) {
    if (dir.exists(p)) {
      ds <- tryCatch(arrow::open_dataset(p, format = "parquet"), error = function(e) NULL)
      if (!is.null(ds)) return(ds)
    }
  }
  NULL
}

.open_sinan_landing_ds <- function() {
  dash <- .find_dashboard_dir()
  if (is.null(dash)) return(NULL)
  p <- file.path(dash, "sinan_landing")
  if (!dir.exists(p)) return(NULL)
  tryCatch(arrow::open_dataset(p, format = "parquet"), error = function(e) NULL)
}

.open_piramides_ds <- function() {
  dash <- .find_dashboard_dir()
  if (is.null(dash)) return(NULL)
  p <- file.path(dash, "piramides")
  if (!dir.exists(p)) return(NULL)
  tryCatch(arrow::open_dataset(p, format = "parquet"), error = function(e) NULL)
}

# -------------------------------------------------------------------------
# Logging (RRMD)
# -------------------------------------------------------------------------

.find_logs_dir <- function() {
  wd <- tryCatch(getwd(), error = function(e) ".")
  wd <- tryCatch(normalizePath(wd, winslash = "/", mustWork = FALSE), error = function(e) wd)

  candidates <- character(0)
  cur <- wd
  for (i in 0:8) {
    candidates <- c(candidates, file.path(cur, "dashs", "shiny", "logs"))
    candidates <- c(candidates, file.path(cur, "logs"))
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  candidates <- unique(candidates)
  ok <- candidates[file.exists(candidates)]
  if (length(ok) >= 1) return(ok[[1]])

  prefer <- file.path(wd, "dashs", "shiny", "logs")
  tryCatch(dir.create(prefer, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  if (dir.exists(prefer)) return(prefer)

  tmp <- file.path(tempdir(), "sinan_logs")
  tryCatch(dir.create(tmp, recursive = TRUE, showWarnings = FALSE), error = function(e) NULL)
  tmp
}

.safe_deparse <- function(x, max_chars = 12000) {
  out <- tryCatch({
    s <- paste(capture.output(str(x, max.level = 2, vec.len = 8)), collapse = "\n")
    s
  }, error = function(e) {
    paste0("<str() failed: ", conditionMessage(e), ">")
  })
    
  if (nchar(out) > max_chars) paste0(substr(out, 1, max_chars), "\n... <truncado>") else out
}

.write_rrmd_log <- function(kind, title, details = list()) {
  dir <- .find_logs_dir()
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  f <- file.path(dir, sprintf("debug_%s_%s_%s.Rmd", kind, ts, paste0(sample(c(letters, 0:9), 6, TRUE), collapse = "")))

  lines <- c(
    "---",
    paste0("title: \"", gsub("\"", "'", title %||% "Debug"), "\""),
    "output: html_document",
    "params: {}",
    "---",
    "",
    paste0("**Timestamp:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("  \n**Kind:** ", kind),
    ""
  )

  if (length(details) > 0) {
    lines <- c(lines, "## Detalhes (dump)", "")
    for (nm in names(details)) {
      val <- details[[nm]]
      block <- c(
        paste0("### ", nm),
        "",
        "```",
        .safe_deparse(val),
        "```",
        ""
      )
      lines <- c(lines, block)
    }
  }

  lines <- c(
    lines,
    "## Session",
    "```{r}",
    "try(sessionInfo(), silent = TRUE)",
    "```",
    ""
  )

  tryCatch(writeLines(lines, f, useBytes = TRUE), error = function(e) NULL)
  f
}

.with_rrmd_log <- function(kind, title, ctx_builder, expr, on_error_message = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      ctx <- tryCatch(ctx_builder(e), error = function(e2) list(log_error = conditionMessage(e2)))
      .write_rrmd_log(kind = kind, title = title, details = ctx)
      msg <- on_error_message %||% paste0("Erro ao renderizar: ", conditionMessage(e))
      .ec_message(msg)
    }
  )
}

# -------------------------------------------------------------------------
# ECharts helpers
# -------------------------------------------------------------------------

.ec_palette <- function(cfg = NULL, pack = NULL, type = c("default", "bars", "lines")) {
  type <- match.arg(type)
  
  # Tenta obter paleta específica por tipo
  if (type == "bars") {
    p <- .obj_get(cfg, "ECHARTS_PALETTE_BARS", NULL) %||% .obj_get(pack, "ECHARTS_PALETTE_BARS", NULL)
    if (!is.null(p)) {
      p <- as.character(unlist(p))
      p <- p[!is.na(p) & nzchar(p)]
      if (length(p) >= 3) return(p)
    }
  }
  
  if (type == "lines") {
    p <- .obj_get(cfg, "ECHARTS_PALETTE_LINES", NULL) %||% .obj_get(pack, "ECHARTS_PALETTE_LINES", NULL)
    if (!is.null(p)) {
      p <- as.character(unlist(p))
      p <- p[!is.na(p) & nzchar(p)]
      if (length(p) >= 3) return(p)
    }
  }
  
  # Fallback para paleta geral
  p <- .obj_get(cfg, "ECHARTS_PALETTE", NULL) %||% .obj_get(pack, "ECHARTS_PALETTE", NULL)
  if (!is.null(p)) {
    p <- as.character(unlist(p))
    p <- p[!is.na(p) & nzchar(p)]
    if (length(p) >= 3) return(p)
  }
  c(
    "#0B8A8F", "#E39D00", "#1C5D99", "#4C9F70", "#6B3FA0",
    "#2E7D32", "#C62828", "#8C564B", "#546E7A", "#7F7F7F"
  )
}

.ec_message <- function(msg, palette = NULL) {
  d <- data.frame(x = 1, y = 1)
  p <- e_charts(d, x) %>%
    e_scatter(y, symbolSize = 1, name = "") %>%
    e_tooltip(show = FALSE) %>%
    e_legend(show = FALSE) %>%
    e_x_axis(show = FALSE) %>%
    e_y_axis(show = FALSE) %>%
    e_grid(left = 0, right = 0, top = 0, bottom = 0) %>%
    e_title(text = msg, left = "center", top = "middle", textStyle = list(fontSize = 13))
  if (!is.null(palette)) p <- p %>% e_color(palette)
  p
}

.ec_tooltip_position <- htmlwidgets::JS(
  "function(point, params, dom, rect, size){
     try{
       var viewW = size.viewSize[0], viewH = size.viewSize[1];
       var boxW  = size.contentSize[0], boxH = size.contentSize[1];

       var x = point[0] + 14;
       if(x + boxW + 8 > viewW) x = point[0] - boxW - 14;
       if(x < 8) x = 8;
       if(x + boxW + 8 > viewW) x = viewW - boxW - 8;

       var y = point[1] - boxH - 12;
       if(y < 8) y = 8;
       if(y + boxH + 8 > viewH) y = Math.max(8, viewH - boxH - 8);

       return [x, y];
     }catch(e){
       return [point[0], point[1]];
     }
   }"
)

.ec_js_extract_num <- "
function extractNum(v){
  try{
    if(v==null) return null;

    if(typeof v === 'object' && v.value != null) v = v.value;

    if(Array.isArray(v)){
      for(var i=v.length-1;i>=0;i--){
        var n = Number(v[i]);
        if(!isNaN(n) && isFinite(n)) return n;
      }
      for(var j=0;j<v.length;j++){
        var n2 = Number(v[j]);
        if(!isNaN(n2) && isFinite(n2)) return n2;
      }
      return null;
    }

    var n3 = Number(v);
    if(isNaN(n3) || !isFinite(n3)) return null;
    return n3;
  }catch(e){
    return null;
  }
}
"

.ec_tooltip_fmt_item_label <- function(label, abs_values = FALSE) {
  lab <- gsub("'", "\\\\'", as.character(label %||% ""))
  htmlwidgets::JS(sprintf(
    "function(p){
       try{
         %s
         var name  = (p && p.name!=null) ? String(p.name) : '';
         var v = null;
         if(p && p.value!=null) v = extractNum(p.value);
         if(v==null && p && p.data!=null) v = extractNum(p.data);
         if(v==null && p && p.data && p.data.value!=null) v = extractNum(p.data.value);
         if(v==null) return '';

         %s
         var vv = Math.abs(v).toLocaleString('pt-BR');

         var html = '<div style=\"font-weight:900; font-size:11px; margin:0 0 4px 0;\">' + (name||'') + '</div>';
         html += '<div style=\"display:flex; gap:8px; align-items:center;\">';
         html += '<span style=\"opacity:.9;\">%s</span>';
         html += '<span style=\"margin-left:auto; font-weight:900;\">' + vv + '</span>';
         html += '</div>';
         return html;
       }catch(e){ return ''; }
     }",
    .ec_js_extract_num,
    if (isTRUE(abs_values)) "v = Math.abs(v);" else "",
    lab
  ))
}

.ec_tooltip_fmt_item <- function(abs_values = FALSE) {
  htmlwidgets::JS(sprintf(
    "function(p){
       try{
         %s
         var name  = (p && p.name!=null) ? String(p.name) : '';
         var sname = (p && p.seriesName!=null) ? String(p.seriesName) : '';

         var v = null;
         if(p && p.value!=null) v = extractNum(p.value);
         if(v==null && p && p.data!=null) v = extractNum(p.data);
         if(v==null && p && p.data && p.data.value!=null) v = extractNum(p.data.value);
         if(v==null) return '';

         %s
         var vv = Math.abs(v).toLocaleString('pt-BR');
         var html = '<div style=\"font-weight:900; font-size:11px; margin:0 0 4px 0;\">' + (name||'') + '</div>';
         html += '<div style=\"display:flex; gap:8px; align-items:center;\">';
         html += '<span style=\"opacity:.9;\">' + (sname||'') + '</span>';
         html += '<span style=\"margin-left:auto; font-weight:900;\">' + vv + '</span>';
         html += '</div>';
         return html;
       }catch(e){ return ''; }
     }",
    .ec_js_extract_num,
    if (isTRUE(abs_values)) "v = Math.abs(v);" else ""
  ))
}

.ec_tooltip_fmt_comp <- function() {
  htmlwidgets::JS(
    "function(p){
       try{
         var name  = (p && p.name!=null) ? String(p.name) : '';
         var sname = (p && p.seriesName!=null) ? String(p.seriesName) : '';

         var perc = null;
         if(p && p.value!=null && typeof p.value === 'number') perc = p.value;
         if(perc==null && p && p.data && p.data.value!=null) perc = Number(p.data.value);
         if(perc==null || isNaN(perc)) return '';

         var total = (p && p.data && p.data.total!=null) ? Number(p.data.total) : null;

         var percStr = perc.toLocaleString('pt-BR', {minimumFractionDigits:1, maximumFractionDigits:1}) + '%';
         var vv = (total!=null && !isNaN(total)) ? (percStr + ' (' + total.toLocaleString('pt-BR') + ')') : percStr;

         var html = '<div style=\"font-weight:900; font-size:11px; margin:0 0 4px 0;\">' + (name||'') + '</div>';
         html += '<div style=\"display:flex; gap:8px; align-items:center;\">';
         html += '<span style=\"opacity:.9;\">' + (sname||'') + '</span>';
         html += '<span style=\"margin-left:auto; font-weight:900;\">' + vv + '</span>';
         html += '</div>';
         return html;
       }catch(e){ return ''; }
     }"
  )
}

.ec_tooltip_fmt_axis <- function(abs_values = FALSE) {
  htmlwidgets::JS(sprintf(
    "function(ps){
       try{
         %s
         var arr = (ps && ps.length) ? ps : [];
         if(!arr.length) return '';
         var lbl = (arr[0] && arr[0].axisValue!=null) ? String(arr[0].axisValue) : '';

         var html = '<div style=\"font-weight:900; font-size:11px; margin:0 0 4px 0;\">' + (lbl||'') + '</div>';

         for(var i=0;i<arr.length;i++){
           var p = arr[i] || {};
           var v = null;

           if(p.value!=null) v = extractNum(p.value);
           if(v==null && p.data!=null) v = extractNum(p.data);
           if(v==null && p.data && p.data.value!=null) v = extractNum(p.data.value);
           if(v==null) continue;

           %s
           var vv = Math.abs(v).toLocaleString('pt-BR');
           html += '<div style=\"display:flex; gap:8px; align-items:center; margin:2px 0;\">';
           html += '<span>' + (p.marker||'') + '</span>';
           html += '<span style=\"opacity:.9;\">' + (p.seriesName||'') + '</span>';
           html += '<span style=\"margin-left:auto; font-weight:900;\">' + vv + '</span>';
           html += '</div>';
         }
         return html;
       }catch(e){ return ''; }
     }",
    .ec_js_extract_num,
    if (isTRUE(abs_values)) "v = Math.abs(v);" else ""
  ))
}

.ec_apply_tooltip <- function(p, tooltip_list) {
  tooltip_list <- tooltip_list %||% list()
  if (!is.list(tooltip_list)) tooltip_list <- list()
  out <- tryCatch(
    do.call(echarts4r::e_tooltip, c(list(p), tooltip_list)),
    error = function(e) p
  )
  out
}

.ec_hide_tip_on_mouseleave <- function(p) {
  htmlwidgets::onRender(
    p,
    "function(el, x){
       function inst(){
         try{ if(el && el.__echarts__) return el.__echarts__; }catch(e){}
         try{
           if(window.echarts){
             var c = window.echarts.getInstanceByDom(el);
             if(c) return c;
           }
         }catch(e){}
         return null;
       }

       function hide(){
         try{
           var c = inst();
           if(c) c.dispatchAction({type:'hideTip'});
         }catch(e){}
       }

       function bindChartLeave(){
         try{
           var c = inst();
           if(!c || !c.getZr) return;
           var zr = c.getZr();
           if(!zr || el.__sinanHideTipBound) return;
           el.__sinanHideTipBound = true;
           zr.on('globalout', hide);
         }catch(e){}
       }

       try{
         el.addEventListener('mouseleave', hide, true);
         el.addEventListener('mouseout', function(ev){
           try{
             var r = el.getBoundingClientRect();
             var x = ev.clientX, y = ev.clientY;
             if(x < r.left || x > r.right || y < r.top || y > r.bottom) hide();
           }catch(e){}
         }, true);
         document.addEventListener('scroll', hide, true);
         document.addEventListener('click', function(ev){
           try{
             if(!el.contains(ev.target)) hide();
           }catch(e){}
         }, true);
         bindChartLeave();
         setTimeout(bindChartLeave, 60);
       }catch(e){}
     }"
  )
}

.ec_enable_legend_dblclick <- function(p) {
  htmlwidgets::onRender(
    p,
    "function(el, x){
       function inst(){
         try{ if(el && el.__echarts__) return el.__echarts__; }catch(e){}
         try{
           if(window.echarts){
             var c = window.echarts.getInstanceByDom(el);
             if(c) return c;
           }
         }catch(e){}
         return null;
       }
       try{
         var c = inst();
         if(!c) return;
         var lastName = null;
         var lastTime = 0;
         c.on('legendselectchanged', function(params){
           try{
             var now = Date.now();
             var isDbl = (params && params.name === lastName && (now - lastTime) < 420);
             lastName = params ? params.name : null;
             lastTime = now;
             if(!isDbl) return;

             var opt = c.getOption ? c.getOption() : null;
             var legend = opt && opt.legend && opt.legend.length ? opt.legend[0] : null;
             var series = opt && opt.series ? opt.series : [];
             var names = [];
             if(legend && legend.data && legend.data.length){
               names = legend.data.slice();
             }else{
               for(var i=0;i<series.length;i++){
                 if(series[i] && series[i].name) names.push(series[i].name);
               }
             }
             if(!params || !params.name || !names.length) return;

             var selected = legend && legend.selected ? legend.selected : {};
             var onlyThis = true;
             for(var j=0;j<names.length;j++){
               var nm = names[j];
               var on = (selected[nm] !== false);
               if(nm !== params.name && on){ onlyThis = false; break; }
             }
             var next = {};
             for(var k=0;k<names.length;k++){
               var n = names[k];
               next[n] = onlyThis ? true : (n === params.name);
             }
             c.setOption({legend:{selected: next}}, false);
           }catch(e){}
         });
       }catch(e){}
     }"
  )
}

.ec_finalize <- function(p,
                         palette = NULL,
                         datazoom = FALSE,
                         tooltip_trigger = "axis",
                         tooltip_abs = FALSE,
                         tooltip_formatter = NULL,
                         tooltip_background = NULL,
                         grid_left = 52,
                         grid_right = 16,
                         grid_top = 26,
                         grid_bottom = 56,
                         legend_top = 0,
                         legend_bottom = NULL,
                         legend_orient = "horizontal",
                         legend_show = TRUE) {

  if (is.null(tooltip_background) || length(tooltip_background) == 0 ||
      !nzchar(as.character(tooltip_background[[1]]))) {
    palette_upper <- toupper(as.character(unlist(palette %||% character(0))))
    tooltip_background <- if ("#DB400A" %in% palette_upper) "#DB400A" else "rgba(17,24,39,0.96)"
  }

  tt_common <- list(
    show = TRUE,
    confine = TRUE,
    renderMode = "html",
    backgroundColor = tooltip_background,
    borderWidth = 0,
    position = .ec_tooltip_position,
    triggerOn = "mousemove",
    showDelay = 0,
    hideDelay = 0,
    transitionDuration = 0,
    appendToBody = FALSE,
    extraCssText = paste(
      "box-shadow:0 10px 22px rgba(0,0,0,.22);",
      "border-radius:10px;",
      "padding:6px 8px;",
      "color:#fff;",
      "font-size:10.5px;",
      "line-height:1.15;",
      "max-width:240px;",
      "max-height:140px;",
      "overflow:hidden;",
      "height:auto !important;",
      "min-height:0 !important;",
      "display:inline-block;",
      "box-sizing:border-box;",
      "pointer-events:none;",
      sep = " "
    ),
    textStyle = list(color = "#fff")
  )

  if (identical(tooltip_trigger, "item")) {
    fmt <- tooltip_formatter %||% .ec_tooltip_fmt_item(abs_values = tooltip_abs)
    p <- .ec_apply_tooltip(p, c(list(trigger = "item", formatter = fmt), tt_common))
  } else {
    fmt <- tooltip_formatter %||% .ec_tooltip_fmt_axis(abs_values = tooltip_abs)
    p <- .ec_apply_tooltip(
      p,
      c(list(
        trigger = "axis",
        axisPointer = list(type = "shadow"),
        formatter = fmt
      ), tt_common)
    )
  }

  if (isTRUE(legend_show)) {
    legend_args <- list(
      type = "scroll",
      left = 0,
      right = 0,
      itemGap = 8,
      orient = legend_orient,
      textStyle = list(fontSize = 10),
      selectedMode = "multiple"
    )
    if (!is.null(legend_bottom)) {
      legend_args$bottom <- legend_bottom
    } else {
      legend_args$top <- legend_top
    }
    p <- do.call(e_legend, c(list(p), legend_args))
  } else {
    p <- p %>% e_legend(show = FALSE)
  }

  p <- p %>%
    e_grid(left = grid_left, right = grid_right, top = grid_top, bottom = grid_bottom, containLabel = TRUE) %>%
    e_text_style(
      fontFamily = "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif",
      fontSize = 11
    )

  if (!is.null(palette)) p <- p %>% e_color(palette)

  p <- .ec_enable_legend_dblclick(p)

  if (isTRUE(datazoom)) {
    p <- p %>% e_datazoom(type = "inside") %>% e_datazoom(type = "slider", height = 16, bottom = 6)
  }

  # O tooltip deve existir apenas durante a interacao com o grafico.
  # Aplicado aqui para alcançar todos os graficos que usam o finalizador comum.
  .ec_hide_tip_on_mouseleave(p)
}

.ec_force_resize_on_tab <- function(p) {
  htmlwidgets::onRender(
    p,
    "function(el, x){
       function inst(){
         try{ if(el && el.__echarts__) return el.__echarts__; }catch(e){}
         try{
           if(window.echarts){
             var c = window.echarts.getInstanceByDom(el);
             if(c) return c;
           }
         }catch(e){}
         return null;
       }

       function isVisible(){
         try{
           if(!el) return false;
           var r = el.getBoundingClientRect();
           return (r.width>0 && r.height>0);
         }catch(e){ return false; }
       }

       function rs(){
         try{
           var c = inst();
           if(c && isVisible()) c.resize();
         }catch(e){}
       }

       function tick(){ rs(); }

       setTimeout(tick, 0);
       setTimeout(tick, 80);
       setTimeout(tick, 200);
       setTimeout(tick, 420);

       document.addEventListener('shown.bs.tab', function(){
         setTimeout(tick, 0);
         setTimeout(tick, 120);
         setTimeout(tick, 320);
       }, true);

       window.addEventListener('resize', function(){
         setTimeout(tick, 80);
         setTimeout(tick, 220);
       });

       try{
         var obs = new MutationObserver(function(){
           setTimeout(tick, 60);
         });
         obs.observe(document.body, {attributes:true, childList:true, subtree:true});
       }catch(e){}

       document.addEventListener('click', function(){ setTimeout(tick, 120); }, true);
     }"
  )
}

.ec_attach_click <- function(p, click_input_id) {
  if (is.null(click_input_id) || !nzchar(click_input_id)) return(p)
  htmlwidgets::onRender(
    p,
    sprintf(
      "function(el, x){
         function inst(){
           try{ if(el && el.__echarts__) return el.__echarts__; }catch(e){}
           try{
             if(window.echarts){
               var c = window.echarts.getInstanceByDom(el);
               if(c) return c;
             }
           }catch(e){}
           return null;
         }
         function bind(){
           try{
             var c = inst();
             if(!c) return;
             try{ c.off('click'); }catch(e){}
             c.on('click', function(params){
               try{
                 if(window.Shiny && Shiny.setInputValue){
                   Shiny.setInputValue('%s', {name: params && params.name, idx: params && params.dataIndex}, {priority:'event'});
                 }
               }catch(e){}
             });
           }catch(e){}
         }
         setTimeout(bind, 0);
         setTimeout(bind, 180);
         document.addEventListener('shown.bs.tab', function(){ setTimeout(bind, 140); }, true);
       }",
      click_input_id
    )
  )
}

.ec_widget_from_option <- function(opt) {
  htmlwidgets::createWidget(
    name = "echarts4r",
    x = list(opts = opt),
    width = NULL,
    height = NULL,
    package = "echarts4r"
  )
}

# -------------------------------------------------------------------------
# Arrow helpers
# -------------------------------------------------------------------------

.ds_names <- function(ds) {
  if (is.null(ds)) return(character(0))
  tryCatch({
    sc <- arrow::schema(ds)
    if (!is.null(sc)) return(sc$names)
    character(0)
  }, error = function(e) {
    tryCatch(ds$schema$names, error = function(e2) character(0))
  })
}

.ds_pick <- function(ds, candidates) {
  nms <- .ds_names(ds)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(hit[[1]])
  nms_up <- toupper(nms)
  cand_up <- toupper(candidates)
  j <- match(cand_up, nms_up)
  j <- j[is.finite(j)]
  if (length(j)) return(nms[[j[[1]]]])
  NULL
}

.best_match <- function(req, choices) {
  req0 <- as.character(req %||% "")
  if (!nzchar(trimws(req0))) return(NULL)

  ch0 <- as.character(choices %||% character(0))
  ch0 <- ch0[!is.na(ch0) & nzchar(trimws(ch0))]
  if (!length(ch0)) return(NULL)

  reqN <- toupper(gsub("[^A-Z0-9]+", "", trimws(req0)))
  chN  <- toupper(gsub("[^A-Z0-9]+", "", trimws(ch0)))

  j <- which(chN == reqN)
  if (length(j)) return(ch0[[j[[1]]]])

  j <- which(startsWith(chN, reqN))
  if (length(j)) return(ch0[[j[[1]]]])
  j <- which(startsWith(reqN, chN))
  if (length(j)) return(ch0[[j[[1]]]])

  reqU <- toupper(trimws(req0))
  chU  <- toupper(trimws(ch0))
  j <- which(chU == reqU)
  if (length(j)) return(ch0[[j[[1]]]])

  NULL
}

.df_filter_context <- function(df, sc, doenca = NULL, tipo = NULL) {
  df <- .safe_df(df)
  if (nrow(df) == 0) return(df)

  nms <- names(df)

  c_nivel <- .pick(nms, c("nivel","NIVEL","level","LEVEL"))
  c_uf    <- .pick(nms, c("uf","UF","sg_uf","SG_UF","sigla_uf","SIGLA_UF"))
  c_geo   <- .pick(nms, c("geo_id","GEO_ID","cod_mun7","COD_MUN7","cod_mun6","COD_MUN6","cd_mun","CD_MUN","mun","MUN"))

  if (!is.null(c_nivel) && !is.null(sc$level) && nzchar(sc$level)) {
    if (!identical(toupper(sc$level), "BR")) {
      v <- toupper(trimws(as.character(df[[c_nivel]]))); v[is.na(v)] <- ""
      df <- df[v %in% .scope_level_candidates(sc$level), , drop = FALSE]
      if (nrow(df) == 0) return(df)
    }
  }

  if (!is.null(c_uf) && !is.null(sc$uf) && nzchar(sc$uf)) {
    v <- toupper(trimws(as.character(df[[c_uf]]))); v[is.na(v)] <- ""
    df <- df[v == toupper(sc$uf), , drop = FALSE]
    if (nrow(df) == 0) return(df)
  }
  if (!is.null(sc$mun) && nzchar(sc$mun)) {
    mun_cols <- intersect(
      c("geo_id","GEO_ID","cod_mun7","COD_MUN7","cod_mun6","COD_MUN6","cd_mun","CD_MUN","mun","MUN"),
      nms
    )
    if (length(mun_cols) > 0) {
      tgt7 <- sc$mun7 %||% .mun_code_7(sc$mun)
      tgt6 <- sc$mun6 %||% .mun_code_6(sc$mun)
      keep <- rep(FALSE, nrow(df))

      for (cc in mun_cols) {
        v <- as.character(df[[cc]])
        v[is.na(v)] <- ""
        v <- gsub("\\D+", "", v)
        v6 <- ifelse(nchar(v) >= 6, substr(v, 1, 6), v)
        hit <- rep(FALSE, length(v))
        if (!is.null(tgt7) && nzchar(tgt7)) hit <- hit | (v == tgt7)
        if (!is.null(tgt6) && nzchar(tgt6)) hit <- hit | (v6 == tgt6)
        keep <- keep | hit
      }

      df <- df[keep, , drop = FALSE]
      if (nrow(df) == 0) return(df)
    } else if (!is.null(c_geo)) {
      v <- as.character(df[[c_geo]])
      v[is.na(v)] <- ""
      v <- gsub("\\D+", "", v)
      v6 <- ifelse(nchar(v) >= 6, substr(v, 1, 6), v)
      tgt6 <- sc$mun6 %||% if (nchar(sc$mun) >= 6) substr(sc$mun, 1, 6) else sc$mun
      df <- df[(v == sc$mun) | (v6 == tgt6), , drop = FALSE]
      if (nrow(df) == 0) return(df)
    }
  }

  c_doenca <- .pick(nms, c("doenca","DOENCA","disease","DISEASE"))
  if (!is.null(c_doenca) && !is.null(doenca) && nzchar(doenca)) {
    v <- toupper(trimws(as.character(df[[c_doenca]]))); v[is.na(v)] <- ""
    df <- df[v == toupper(doenca), , drop = FALSE]
    if (nrow(df) == 0) return(df)
  }

  c_tipo <- .pick(nms, c("tipo","TIPO"))
  if (!is.null(c_tipo) && !is.null(tipo) && nzchar(tipo)) {
    v <- toupper(trimws(as.character(df[[c_tipo]]))); v[is.na(v)] <- ""
    df <- df[v == toupper(tipo), , drop = FALSE]
  }

  df
}

.tbl_filter_scope_arrow <- function(tbl, sc, col_geo = NULL, col_uf = NULL) {
  tryCatch({
    if (!identical(sc$level, "BR")) {
      if (!is.null(col_uf) && !is.null(sc$uf) && nzchar(sc$uf)) {
        tbl <- tbl %>% filter(!!sym(col_uf) == sc$uf)
      } else if (!is.null(col_geo) && !is.null(sc$uf) && nzchar(sc$uf) && identical(sc$level, "UF")) {
        tbl <- tbl %>% filter(!!sym(col_geo) == sc$uf)
      }
    }

    if (identical(sc$level, "MUN")) {
      gid7 <- sc$mun7 %||% .mun_code_7(sc$mun)
      gid6 <- sc$mun6 %||% .mun_code_6(sc$mun)
      gids_chr <- unique(stats::na.omit(c(gid7, gid6, sc$mun)))
      gids_chr <- as.character(gids_chr)
      gids_chr <- gids_chr[nzchar(gids_chr)]

      if (length(gids_chr) > 0 && !is.null(col_geo)) {
        gids_num <- suppressWarnings(as.numeric(gids_chr))
        gids_num <- gids_num[is.finite(gids_num)]

        tbl2 <- tryCatch({
          if (length(gids_num) > 0) {
            tbl %>% filter((!!sym(col_geo)) %in% gids_chr | (!!sym(col_geo)) %in% gids_num)
          } else {
            tbl %>% filter((!!sym(col_geo)) %in% gids_chr)
          }
        }, error = function(e) NULL)
        if (!is.null(tbl2)) tbl <- tbl2
      }
    }
    tbl
  }, error = function(e) {
    tbl
  })
}

# -------------------------------------------------------------------------
# Time series
# -------------------------------------------------------------------------

.norm_key <- function(x) {
  x <- .as_chr1(x, "")
  x <- tolower(trimws(x))
  x2 <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  x2 <- gsub("[^a-z0-9]+", "", x2)
  x2
}

.ts_filter_long_by_metric <- function(ts, metric_key) {
  ts <- .safe_df(ts)
  if (nrow(ts) == 0) return(ts)

  nms <- names(ts)

  c_tipo <- .pick(nms, c("tipo","TIPO","variavel","VARIAVEL","indicador","INDICADOR","metric","METRIC","kpi","KPI"))
  if (is.null(c_tipo)) return(ts)

  c_val <- .pick(nms, c("valor","VALOR","value","VALUE","n","N","total","TOTAL"))
  if (is.null(c_val)) return(ts)

  m <- .norm_key(metric_key %||% "cases")
  variants <- switch(
    m,
    "obitos" = c("obitos","obito","deaths","death","casosobitos","casos_obitos","casos-obitos","totalobitos","total_obitos"),
    "cura"   = c("casos_cura","casos-cura","totalcura","total_cura","totalcuras"),
    "incid"  = c("incid","incidencia","incidência","incid100k","incid_100k","incid100k_total","incid_100k_total","incid_100k"),
    c("cases","casos","caso","totalcasos","casos_total","total")
  )

  vv <- as.character(ts[[c_tipo]])
  vvN <- tolower(gsub("[^a-z0-9]+", "", suppressWarnings(iconv(vv, from="", to="ASCII//TRANSLIT"))))

  keep <- vvN %in% tolower(gsub("[^a-z0-9]+", "", suppressWarnings(iconv(variants, from="", to="ASCII//TRANSLIT"))))
  ts2 <- ts[isTRUE(keep) | keep, , drop = FALSE]

  if (nrow(ts2) == 0) return(ts)
  ts2
}


.detect_metric_col <- function(df, metric) {
  df <- .safe_df(df)
  if (nrow(df) == 0) return(NULL)
  nms <- names(df)

  m <- .norm_key(metric %||% "cases")

  if (m %in% c("incid", "incidence", "incidencia", "incid100k", "incidencia100k")) {
    return(.pick(nms, c("incid_100k_total","incid_100k","incid100k","incid","INCID_100K_TOTAL","INCID_100K","INCID")))
  }
  if (m %in% c("obitos", "obito", "deaths", "death")) {
    return(.pick(nms, c("casos_obitos","obitos_total","obitos","deaths","qt_obitos","QT_OBITOS","TOTAL_OBITOS")))
  }
  if (m %in% c("cura", "curas", "cured")) {
    return(.pick(nms, c("casos_cura","cura_total","curas","qt_curas","QT_CURAS","TOTAL_CURAS")))
  }

  .pick(nms, c("casos","casos_total","cases","cases_total","QT_CASOS","TOTAL_CASOS","TOTAL"))
}

.month_levels_pt <- c(
  "janeiro","fevereiro","março","abril","maio","junho",
  "julho","agosto","setembro","outubro","novembro","dezembro"
)

.month_name_pt <- function(mes_int, mes_nome = NULL) {
  mes_int <- suppressWarnings(as.integer(mes_int))
  mes_int <- ifelse(is.finite(mes_int), pmax(pmin(mes_int, 12L), 1L), NA_integer_)

  base <- ifelse(is.na(mes_int), NA_character_, .month_levels_pt[mes_int])

  if (is.null(mes_nome)) return(base)

  x <- tolower(trimws(as.character(mes_nome)))
  x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  x_ascii <- gsub("[^a-z]+", "", x_ascii)

  map <- c(
    jan="janeiro", janeiro="janeiro",
    fev="fevereiro", fevereiro="fevereiro",
    mar="março", marco="março", março="março",
    abr="abril", abril="abril",
    mai="maio", maio="maio",
    jun="junho", junho="junho",
    jul="julho", julho="julho",
    ago="agosto", agosto="agosto",
    set="setembro", setembro="setembro",
    out="outubro", outubro="outubro",
    nov="novembro", novembro="novembro",
    dez="dezembro", dezembro="dezembro"
  )

  picked <- unname(map[x_ascii])
  bad <- is.na(picked) | !nzchar(picked)
  picked[bad] <- base[bad]

  picked
}

.norm_comp_cat <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  gsub("[^A-Z0-9]+", "", x)
}

.ts_prepare_month_cachets <- function(df, value_col, year_focus, denom_pop = NULL, metric_key = "cases") {
  df <- .safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  yf <- .as_int1(year_focus, NA_integer_)
  if (!is.finite(yf)) return(data.frame())

  nms <- names(df)
  if (!("ano" %in% nms) && "ANO" %in% nms) df$ano <- df$ANO
  if (!("mes" %in% nms) && "MES" %in% nms) df$mes <- df$MES
  if (!("mes_nome" %in% nms) && "MES_NOME" %in% nms) df$mes_nome <- df$MES_NOME

  if (!("ano" %in% names(df)) || !("mes" %in% names(df))) return(data.frame())
  if (is.null(value_col) || !(value_col %in% names(df))) return(data.frame())

  df$ano   <- suppressWarnings(as.integer(df$ano))
  df$mes   <- suppressWarnings(as.integer(df$mes))
  df$valor <- suppressWarnings(as.numeric(df[[value_col]]))

  df <- df[is.finite(df$ano) & is.finite(df$mes) & is.finite(df$valor), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  df$mes <- pmax(pmin(df$mes, 12L), 1L)
  df$mes_nome <- .month_name_pt(df$mes, df$mes_nome %||% NULL)

  agg <- df %>%
    group_by(ano, mes, mes_nome) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop")

  iqr_by_month <- agg %>%
    filter(ano != yf) %>%
    group_by(mes, mes_nome) %>%
    summarise(
      q1 = suppressWarnings(as.numeric(stats::quantile(valor, 0.25, na.rm = TRUE))),
      q3 = suppressWarnings(as.numeric(stats::quantile(valor, 0.75, na.rm = TRUE))),
      .groups = "drop"
    )

  cur <- agg %>%
    filter(ano == yf) %>%
    group_by(mes, mes_nome) %>%
    summarise(valor_ano = sum(valor, na.rm = TRUE), .groups = "drop")

  prev_years <- c(yf - 1L, yf - 2L, yf - 3L)
  prev_1 <- agg %>%
    filter(ano == prev_years[[1]]) %>%
    group_by(mes, mes_nome) %>%
    summarise(prev_1 = sum(valor, na.rm = TRUE), .groups = "drop")
  prev_2 <- agg %>%
    filter(ano == prev_years[[2]]) %>%
    group_by(mes, mes_nome) %>%
    summarise(prev_2 = sum(valor, na.rm = TRUE), .groups = "drop")
  prev_3 <- agg %>%
    filter(ano == prev_years[[3]]) %>%
    group_by(mes, mes_nome) %>%
    summarise(prev_3 = sum(valor, na.rm = TRUE), .groups = "drop")

  months_df <- data.frame(
    mes = 1:12,
    mes_nome = .month_levels_pt,
    stringsAsFactors = FALSE
  )

  out <- months_df %>%
    left_join(cur,  by = c("mes", "mes_nome")) %>%
    left_join(prev_1, by = c("mes", "mes_nome")) %>%
    left_join(prev_2, by = c("mes", "mes_nome")) %>%
    left_join(prev_3, by = c("mes", "mes_nome")) %>%
    left_join(iqr_by_month, by = c("mes", "mes_nome")) %>%
    arrange(mes)


  if (identical(.norm_key(metric_key), "incid")) {
    pop <- suppressWarnings(as.numeric(denom_pop))
    pop <- pop[is.finite(pop) & pop > 0]
    if (length(pop) >= 1) {
      p1 <- pop[[1]]
      cols_scale <- c("valor_ano", "prev_1", "prev_2", "prev_3", "q1", "q3")
      for (cc in cols_scale) {
        if (cc %in% names(out)) out[[cc]] <- (out[[cc]] / p1) * 1e5
      }
    } else {
      out$valor_ano <- NA_real_
      out$prev_1 <- NA_real_
      out$prev_2 <- NA_real_
      out$prev_3 <- NA_real_
      out$q1 <- NA_real_
      out$q3 <- NA_real_
    }
  }

  out$iqr <- suppressWarnings(as.numeric(out$q3)) - suppressWarnings(as.numeric(out$q1))
  out$iqr <- ifelse(is.finite(out$iqr) & out$iqr >= 0, out$iqr, NA_real_)

  out$x <- factor(out$mes_nome, levels = .month_levels_pt)
  attr(out, "x_title") <- "Mês"
  attr(out, "horizon") <- "months"
  attr(out, "year_focus") <- yf
  attr(out, "prev_years") <- prev_years
  out
}

.ts_prepare_year_cachets <- function(df, value_col, year_focus, denom_pop = NULL, metric_key = "cases") {
  df <- .safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  yf <- .as_int1(year_focus, NA_integer_)
  if (!is.finite(yf)) return(data.frame())

  nms <- names(df)
  if (!("ano" %in% nms) && "ANO" %in% nms) df$ano <- df$ANO
  if (!("ano" %in% names(df))) return(data.frame())
  if (is.null(value_col) || !(value_col %in% names(df))) return(data.frame())

  df$ano   <- suppressWarnings(as.integer(df$ano))
  df$valor <- suppressWarnings(as.numeric(df[[value_col]]))
  df <- df[is.finite(df$ano) & is.finite(df$valor), , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())

  ann <- df %>%
    group_by(ano) %>%
    summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    arrange(ano)

  ann$media_prev3 <- NA_real_
  for (i in seq_len(nrow(ann))) {
    y <- ann$ano[[i]]
    prev <- ann %>% filter(.data$ano %in% (y - 3):(y - 1))
    if (nrow(prev) >= 1) ann$media_prev3[[i]] <- mean(prev$valor_total, na.rm = TRUE)
  }

  if (identical(.norm_key(metric_key), "incid")) {
    pop <- suppressWarnings(as.numeric(denom_pop))
    pop <- pop[is.finite(pop) & pop > 0]
    if (length(pop) >= 1) {
      p1 <- pop[[1]]
      ann$valor_total  <- (ann$valor_total  / p1) * 1e5
      ann$media_prev3  <- (ann$media_prev3  / p1) * 1e5
    } else {
      ann$valor_total <- NA_real_
      ann$media_prev3 <- NA_real_
    }
  }

  out <- ann %>%
    transmute(
      x = as.character(.data$ano),
      valor_ano = .data$valor_total,
      media_prev3 = .data$media_prev3
    )

  attr(out, "x_title") <- "Ano"
  attr(out, "horizon") <- "years"
  out
}

.ts_prepare <- function(df, value_col, year_focus, metric_key = "cases", denom_pop = NULL, horizon = c("months", "years")) {
  horizon <- match.arg(horizon)
  df <- .safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  nms <- names(df)
  has_month <- (("ano" %in% nms || "ANO" %in% nms) && ("mes" %in% nms || "MES" %in% nms))

  if (identical(horizon, "months") && has_month) {
    return(.ts_prepare_month_cachets(df, value_col = value_col, year_focus = year_focus, denom_pop = denom_pop, metric_key = metric_key))
  }
  .ts_prepare_year_cachets(df, value_col = value_col, year_focus = year_focus, denom_pop = denom_pop, metric_key = metric_key)
}

.plot_ts <- function(ts_df, palette = NULL, history_palette = NULL) {
  ts_df <- .safe_df(ts_df)
  if (nrow(ts_df) == 0) return(.ec_message("Sem dados na série temporal (ano/escopo).", palette = palette))

  # Cores derivadas da paleta do disease pack (fallback: roxo)
  pal <- if (is.character(palette) && length(palette) >= 2) palette else c("#7C3AED", "#2D1B69")
  col_main   <- pal[1]  # linha principal (média / ano selecionado)
  col_dark   <- pal[2]  # ênfase (ano selecionado quando separado)

  x_title <- attr(ts_df, "x_title") %||% "Mês"
  hist_pal <- if (is.character(history_palette) && length(history_palette) >= 3) {
    history_palette[seq_len(3)]
  } else {
    c("#FACC15", "#3B82F6", "#10B981")
  }

  hz <- attr(ts_df, "horizon") %||% "months"
  y_focus <- suppressWarnings(as.integer(attr(ts_df, "year_focus") %||% NA_integer_))

  if (!"x" %in% names(ts_df)) ts_df$x <- seq_len(nrow(ts_df))
  ts_df$x <- as.character(ts_df$x)

  if ("media_prev3" %in% names(ts_df)) ts_df$media_prev3 <- suppressWarnings(as.numeric(ts_df$media_prev3))
  if ("valor_ano" %in% names(ts_df))   ts_df$valor_ano   <- suppressWarnings(as.numeric(ts_df$valor_ano))
  if ("forecast" %in% names(ts_df)) ts_df$forecast <- suppressWarnings(as.numeric(ts_df$forecast))
  if ("forecast_lower" %in% names(ts_df)) ts_df$forecast_lower <- suppressWarnings(as.numeric(ts_df$forecast_lower))
  if ("forecast_upper" %in% names(ts_df)) ts_df$forecast_upper <- suppressWarnings(as.numeric(ts_df$forecast_upper))

  # Para anos incompletos, separar últimos 2 meses disponíveis em linha pontilhada
  if (identical(hz, "months") && "valor_ano" %in% names(ts_df) && isTRUE(is.finite(y_focus))) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (y_focus >= current_year) {
      # Identificar últimos 2 meses com dados disponíveis (não-NA)
      valid_idx <- which(is.finite(ts_df$valor_ano))
      if (length(valid_idx) >= 2) {
        last_2_idx <- tail(valid_idx, 2)
        ts_df$valor_ano_completo <- ts_df$valor_ano
        ts_df$valor_ano_incompleto <- ts_df$valor_ano
        ts_df$valor_ano_completo[last_2_idx] <- NA_real_
        ts_df$valor_ano_incompleto[-last_2_idx] <- NA_real_
      }
    }
  }

  p <- e_charts(ts_df, x)

  if ("media_prev3" %in% names(ts_df) && identical(hz, "years")) {
    p <- p %>% e_line(
      media_prev3,
      name = "Média 3 anos anteriores",
      symbol = "circle", symbolSize = 5, showSymbol = TRUE,
      connectNulls = FALSE
    )
  }
  
  if ("valor_ano" %in% names(ts_df)) {
    line_style <- NULL
    item_style <- NULL
    if (identical(hz, "months")) {
      if ("valor_ano_completo" %in% names(ts_df) && "valor_ano_incompleto" %in% names(ts_df)) {
        # Ano incompleto: duas linhas (completa + incompleta pontilhada)
        p <- p %>% e_line(
          valor_ano_completo,
          name = "Ano selecionado",
          symbol = "circle", symbolSize = 5, showSymbol = TRUE,
          connectNulls = FALSE,
          lineStyle = list(color = col_dark, width = 3),
          itemStyle = list(color = col_dark)
        )
        p <- p %>% e_line(
          valor_ano_incompleto,
          name = "Meses incompletos",
          symbol = "circle", symbolSize = 5, showSymbol = TRUE,
          connectNulls = FALSE,
          lineStyle = list(color = "#F59E0B", width = 2.5, type = "dashed"),
          itemStyle = list(color = "#F59E0B")
        )
      } else {
        # Ano completo: linha normal
        line_style <- list(color = col_dark, width = 3)
        item_style <- list(color = col_dark)
        p <- p %>% e_line(
          valor_ano,
          name = "Ano selecionado",
          symbol = "circle", symbolSize = 5, showSymbol = TRUE,
          connectNulls = FALSE,
          lineStyle = line_style,
          itemStyle = item_style
        )
      }
    } else if (identical(hz, "years")) {
      line_style <- list(width = 3.2)
      p <- p %>% e_line(
        valor_ano,
        name = "Total anual",
        symbol = "circle", symbolSize = 5, showSymbol = TRUE,
        connectNulls = FALSE,
        lineStyle = line_style,
        itemStyle = item_style
      )
    }
  }
  if (identical(hz, "months") && ("prev_1" %in% names(ts_df))) {
    prev_years <- attr(ts_df, "prev_years") %||% c(NA_integer_, NA_integer_, NA_integer_)
    name_1 <- if (isTRUE(is.finite(prev_years[[1]]))) as.character(prev_years[[1]]) else "Ano -1"
    name_2 <- if (isTRUE(is.finite(prev_years[[2]]))) as.character(prev_years[[2]]) else "Ano -2"
    name_3 <- if (isTRUE(is.finite(prev_years[[3]]))) as.character(prev_years[[3]]) else "Ano -3"
    p <- p %>% e_line(
      prev_1,
      name = name_1,
      symbol = "circle", symbolSize = 4, showSymbol = FALSE,
      connectNulls = FALSE,
      lineStyle = list(color = hist_pal[[1]], width = 1.2, type = "dashed"),
      itemStyle = list(color = hist_pal[[1]])
    )
    if ("prev_2" %in% names(ts_df)) {
      p <- p %>% e_line(
        prev_2,
        name = name_2,
        symbol = "circle", symbolSize = 4, showSymbol = FALSE,
        connectNulls = FALSE,
        lineStyle = list(color = hist_pal[[2]], width = 1.2, type = "dashed"),
        itemStyle = list(color = hist_pal[[2]])
      )
    }
    if ("prev_3" %in% names(ts_df)) {
      p <- p %>% e_line(
        prev_3,
        name = name_3,
        symbol = "circle", symbolSize = 4, showSymbol = FALSE,
        connectNulls = FALSE,
        lineStyle = list(color = hist_pal[[3]], width = 1.2, type = "dashed"),
        itemStyle = list(color = hist_pal[[3]])
      )
    }
  }
  if (identical(hz, "months") && ("q1" %in% names(ts_df)) && ("q3" %in% names(ts_df))) {
    ts_df$iqr <- suppressWarnings(as.numeric(ts_df$iqr))
    p <- p %>% e_line(
      q1,
      name = "Q1",
      symbol = "circle", symbolSize = 4, showSymbol = TRUE,
      connectNulls = FALSE,
      lineStyle = list(color = "#94A3B8", width = 1.5),
      itemStyle = list(color = "#94A3B8")
    )
    p <- p %>% e_line(
      q1,
      name = " ",
      stack = "iqr",
      symbol = "none",
      lineStyle = list(opacity = 0),
      itemStyle = list(opacity = 0),
      tooltip = list(show = FALSE)
    )
    p <- p %>% e_line(
      iqr,
      name = " ",
      stack = "iqr",
      symbol = "none",
      lineStyle = list(opacity = 0),
      areaStyle = list(opacity = 0.18, color = col_main),
      itemStyle = list(opacity = 0),
      tooltip = list(show = FALSE)
    )
    p <- p %>% e_line(
      q3,
      name = "Q3",
      symbol = "circle", symbolSize = 4, showSymbol = TRUE,
      connectNulls = FALSE,
      lineStyle = list(color = "#94A3B8", width = 1.5),
      itemStyle = list(color = "#94A3B8")
    )
  }
  if ("cases_0_14" %in% names(ts_df)) {
    p <- p %>% e_line(
      cases_0_14,
      name = "Casos (0 a 14)",
      symbol = "circle", symbolSize = 5, showSymbol = TRUE,
      connectNulls = TRUE,
      lineStyle = list(color = "#C4B5FD", width = 2),
      itemStyle = list(color = "#C4B5FD")
    )
  }
  # Forecast removido (previsão de 15 anos)

  dz <- if (identical(hz, "years")) (nrow(ts_df) > 12) else TRUE
  lbl_interval <- if (identical(hz, "months")) 1 else if (nrow(ts_df) >= 16) 1 else 0

  p <- p %>%
    e_x_axis(
      name = x_title,
      type = "category",
      boundaryGap = FALSE,
      nameLocation = "middle",
      nameGap = 28,
      axisLabel = list(fontSize = 11, interval = lbl_interval, rotate = 45)
    ) %>%
    e_y_axis(name = "", min = 0, axisLabel = list(fontSize = 11)) %>%
    .ec_finalize(
      palette = palette,
      datazoom = dz,
      tooltip_trigger = "axis",
      grid_left = 46,
      grid_top = if (identical(hz, "months")) 28 else 28,
      grid_bottom = if (dz) 66 else 58,
      legend_top = 0,
      legend_bottom = NULL,
      legend_orient = "horizontal"
    )

  .ec_force_resize_on_tab(p)
}


# -------------------------------------------------------------------------
# Ranking
# -------------------------------------------------------------------------

.rank_value_col <- function(cols, kind) {
  if (kind == "obitos") {
    .pick(cols, c("casos_obitos", "obitos_total", "obitos", "deaths", "qt_obitos", "QT_OBITOS", "TOTAL_OBITOS"))
  } else if (kind == "cura") {
    .pick(cols, c("casos_cura", "cura_total", "curas", "qt_curas", "QT_CURAS", "TOTAL_CURAS"))
  } else if (kind == "incid") {
    .pick(cols, c("incid_100k_total", "incid_100k_M", "incid_100k_F", "incid_100k", "INCID_100K_TOTAL", "INCID_100K", "INCID"))
  } else {
    .pick(cols, c("casos_total", "casos", "cases_total", "cases", "TOTAL", "QT_CASOS", "TOTAL_CASOS"))
  }
}

.rank_prepare_for_plot <- function(out, top_n) {
  out <- .safe_df(out)
  if (nrow(out) == 0) return(out)
  out$cat <- as.character(out$cat)
  out$key <- as.character(out$key)
  out$val <- suppressWarnings(as.numeric(out$val))
  out <- out[is.finite(out$val) & nzchar(out$cat) & nzchar(out$key), , drop = FALSE]
  if (nrow(out) == 0) return(out)

  tn <- suppressWarnings(as.integer(top_n))
  if (!is.finite(tn) || tn <= 0) tn <- 10L

  out <- out %>% arrange(desc(.data$val)) %>% slice_head(n = tn) %>% arrange(.data$val)
  out
}

.plot_rank <- function(out_plot, click_input_id = NULL, palette = NULL, metric_label = "Casos") {
  out_plot <- .safe_df(out_plot)
  if (nrow(out_plot) == 0 || !all(c("cat", "val", "key") %in% names(out_plot))) {
    return(.ec_message("Sem dados para ranking (ano/escopo).", palette = palette))
  }

  out_plot$cat <- as.character(out_plot$cat)
  out_plot$key <- as.character(out_plot$key)
  out_plot$val <- suppressWarnings(as.numeric(out_plot$val))
  out_plot <- out_plot[is.finite(out_plot$val) & nzchar(out_plot$cat), , drop = FALSE]
  if (nrow(out_plot) == 0) return(.ec_message("Sem dados para ranking (ano/escopo).", palette = palette))

  fmt <- .ec_tooltip_fmt_item_label(metric_label, abs_values = FALSE)

  p <- e_charts(out_plot, cat) %>%
    e_bar(val, name = metric_label, label = list(show = FALSE)) %>%
    e_flip_coords() %>%
    e_x_axis(
      type = "value",
      axisLabel = list(
        fontSize = 11,
        formatter = htmlwidgets::JS(
          "function(v){
             if(v==null || isNaN(v)) return '';
             try { return Number(v).toLocaleString('pt-BR'); } catch(e){ return v; }
           }"
        )
      )
    ) %>%
    e_y_axis(type = "category", axisLabel = list(interval = 0, fontSize = 11)) %>%
    .ec_finalize(
      palette = palette,
      datazoom = nrow(out_plot) > 12,
      tooltip_trigger = "item",
      tooltip_formatter = fmt,
      grid_left = 44, grid_top = 18, grid_bottom = 56,
      legend_show = FALSE
    )

  p <- .ec_attach_click(p, click_input_id)
  p <- .ec_force_resize_on_tab(p)
  p
}

# Top-N a partir do dataset incidence (fallback local) — filtrando cedo no Arrow
.inc_rank_topn <- function(doenca, y, nivel = c("UF", "MUN"), uf = NULL, kind = "cases", top_n = 15L) {
  nivel <- match.arg(nivel)
  ds <- .open_incidence_ds()
  if (is.null(ds)) return(data.frame())

  c_doenca <- .ds_pick(ds, c("doenca","DOENCA","disease","DISEASE"))
  c_ano    <- .ds_pick(ds, c("ano","ANO","year","YEAR"))
  c_nivel  <- .ds_pick(ds, c("nivel","NIVEL","level","LEVEL"))
  c_uf     <- .ds_pick(ds, c("uf","UF","sg_uf","SG_UF"))
  c_nome   <- .ds_pick(ds, c("nome_mun","NOME_MUN","geo_nome","GEO_NOME","municipio","MUNICIPIO","mun_nome","MUN_NOME","nome","NOME","nm_mun","NM_MUN"))
  c_mun6   <- .ds_pick(ds, c("cod_mun6","COD_MUN6","cd_mun6","CD_MUN6","geo_id6","GEO_ID6"))
  c_mun7   <- .ds_pick(ds, c("cod_mun7","COD_MUN7","cd_mun7","CD_MUN7"))
  c_geo    <- .ds_pick(ds, c("geo_id","GEO_ID","id_geo","ID_GEO","cd_geo","CD_GEO"))
  c_sexo   <- .ds_pick(ds, c("sexo","SEXO","cs_sexo","CS_SEXO","sex","SEX","tp_sexo","TP_SEXO"))

  cols <- .ds_names(ds)
  val_col <- .rank_value_col(cols, kind)
  if (is.null(val_col)) return(data.frame())

  tn <- suppressWarnings(as.integer(top_n))
  if (!is.finite(tn) || tn <= 0) tn <- if (nivel == "UF") 10L else 15L

  key_col <- c_mun7 %||% c_geo %||% c_mun6 %||% NULL

  if (nivel == "UF"  && is.null(c_uf)) return(data.frame())
  if (nivel == "MUN" && is.null(key_col) && is.null(c_nome)) return(data.frame())

  need <- unique(c(c_doenca, c_ano, c_nivel, c_uf, c_nome, key_col, c_sexo, val_col))
  need <- need[!is.na(need) & nzchar(need)]
  tbl <- ds %>% select(!!!syms(need))

  if (!is.null(c_ano)) {
    y_int <- suppressWarnings(as.integer(y))
    if (is.finite(y_int)) {
      tbl_try <- tryCatch(tbl %>% filter(!!sym(c_ano) == y_int), error = function(e) NULL)
      if (!is.null(tbl_try)) {
        tbl <- tbl_try
      } else {
        tbl_try2 <- tryCatch(tbl %>% filter(!!sym(c_ano) == as.character(y_int)), error = function(e) NULL)
        if (!is.null(tbl_try2)) tbl <- tbl_try2
      }
    }
  }

  if (!is.null(c_doenca) && !is.null(doenca) && nzchar(doenca)) {
    avail <- tryCatch({
      tbl %>% select(!!sym(c_doenca)) %>% distinct() %>% head(500) %>% collect() %>% pull(1)
    }, error = function(e) character(0))
    d_match <- .best_match(doenca, avail) %||% doenca
    tbl <- tryCatch(tbl %>% filter(!!sym(c_doenca) == d_match), error = function(e) tbl)
  }

   # evita zerar ranking quando o dataset vem apenas com M/F ou codificação diferente.
   if (!is.null(c_sexo)) {
     sx_samp <- tryCatch({
       tbl %>% select(!!sym(c_sexo)) %>% head(2500) %>% collect() %>% pull(1)
     }, error = function(e) character(0))
     sx_samp <- toupper(trimws(as.character(sx_samp)))
     alvo <- c("AMBOS","AMBOS OS SEXOS","TOTAL","T","0","9","A")
     if (length(intersect(sx_samp, alvo)) > 0) {
       tbl2 <- tryCatch(tbl %>% filter(toupper(!!sym(c_sexo)) %in% alvo), error = function(e) NULL)
       if (!is.null(tbl2)) tbl <- tbl2
     }
   }

  if (nivel == "UF") {
    if (!is.null(c_nivel)) {
      tbl_try <- tryCatch(tbl %>% filter(!!sym(c_nivel) %in% c("UF","uf","Uf")), error = function(e) NULL)
      if (!is.null(tbl_try)) tbl <- tbl_try
    }

    q <- tryCatch({
      if (identical(kind, "incid")) {
        tbl %>%
          group_by(!!sym(c_uf)) %>%
          summarise(val = max(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(.data$val)) %>%
          head(tn)
      } else {
        tbl %>%
          group_by(!!sym(c_uf)) %>%
          summarise(val = sum(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(.data$val)) %>%
          head(tn)
      }
    }, error = function(e) NULL)

    df <- if (!is.null(q)) tryCatch(collect(q), error = function(e) data.frame()) else data.frame()
    df <- .safe_df(df); if (!nrow(df)) return(df)

    out <- df %>% transmute(
      cat = as.character(.data[[c_uf]]),
      key = as.character(.data[[c_uf]]),
      val = suppressWarnings(as.numeric(.data$val))
    )
    out <- out[is.finite(out$val) & nzchar(out$cat), , drop = FALSE]
    return(out)
  }

  if (!is.null(c_nivel)) {
    tbl_try <- tryCatch(
      tbl %>% filter(!!sym(c_nivel) %in% c("MUN","mun","Mun","MUNICIPIO","MUNICÍPIO")),
      error = function(e) NULL
    )
    if (!is.null(tbl_try)) tbl <- tbl_try
  }

  if (!is.null(uf) && nzchar(uf) && !is.null(c_uf)) {
    uf2 <- toupper(trimws(as.character(uf)))
    tbl <- tryCatch(tbl %>% filter(!!sym(c_uf) == uf2), error = function(e) tbl)
  }

  key_use <- key_col %||% c_nome
  nm_use  <- c_nome %||% key_use
  if (is.null(key_use) || is.null(nm_use)) return(data.frame())

  grp_cols <- unique(c(key_use, nm_use, c_uf))
  grp_cols <- grp_cols[!is.na(grp_cols) & nzchar(grp_cols)]

  q_fast <- tryCatch({
    if (identical(kind, "incid")) {
      tbl %>%
        group_by(!!!syms(grp_cols)) %>%
        summarise(val = max(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(.data$val)) %>%
        head(max(tn * 8L, 400L))
    } else {
      tbl %>%
        group_by(!!!syms(grp_cols)) %>%
        summarise(val = sum(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(.data$val)) %>%
        head(max(tn * 8L, 400L))
    }
  }, error = function(e) NULL)

  df_fast <- if (!is.null(q_fast)) tryCatch(collect(q_fast), error = function(e) data.frame()) else data.frame()
  df_fast <- .safe_df(df_fast)

  df <- df_fast
  if (!nrow(df)) {
    pick_min <- unique(c(key_use, nm_use, c_uf, val_col))
    pick_min <- pick_min[!is.na(pick_min) & nzchar(pick_min)]

    tbl_min <- tryCatch(tbl %>% select(!!!syms(pick_min)), error = function(e) NULL)
    if (is.null(tbl_min)) return(data.frame())

    df <- tryCatch(collect(tbl_min), error = function(e) data.frame())
    df <- .safe_df(df)
    if (!nrow(df)) return(data.frame())

    df[[key_use]] <- as.character(df[[key_use]])
    df[[nm_use]]  <- as.character(df[[nm_use]])
    df[[val_col]] <- suppressWarnings(as.numeric(df[[val_col]]))

    df$key_digits <- vapply(df[[key_use]], .mun_code_digits, FUN.VALUE = character(1))
    df <- df[
      is.finite(df[[val_col]]) &
        !is.na(df$key_digits) & nzchar(df$key_digits) &
        !is.na(df[[nm_use]]) & nzchar(df[[nm_use]]),
      , drop = FALSE
    ]
    if (!nrow(df)) return(data.frame())

    if (!is.null(c_uf) && c_uf %in% names(df)) {
      if (identical(kind, "incid")) {
        df <- df %>%
          group_by(.data$key_digits, .data[[nm_use]], .data[[c_uf]]) %>%
          summarise(val = max(.data[[val_col]], na.rm = TRUE), .groups = "drop")
      } else {
        df <- df %>%
          group_by(.data$key_digits, .data[[nm_use]], .data[[c_uf]]) %>%
          summarise(val = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop")
      }
    } else {
      if (identical(kind, "incid")) {
        df <- df %>%
          group_by(.data$key_digits, .data[[nm_use]]) %>%
          summarise(val = max(.data[[val_col]], na.rm = TRUE), .groups = "drop")
      } else {
        df <- df %>%
          group_by(.data$key_digits, .data[[nm_use]]) %>%
          summarise(val = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop")
      }
    }
  } else {
    df[[key_use]] <- as.character(df[[key_use]])
    df[[nm_use]]  <- as.character(df[[nm_use]])
    df$val <- suppressWarnings(as.numeric(df$val))
    df$key_digits <- vapply(df[[key_use]], .mun_code_digits, FUN.VALUE = character(1))
    df <- df[is.finite(df$val) & !is.na(df$key_digits) & nzchar(df$key_digits) & nzchar(df[[nm_use]]), , drop = FALSE]
    if (!nrow(df)) return(data.frame())
  }

  df <- df %>% arrange(desc(.data$val)) %>% slice_head(n = tn)

  out <- df %>% transmute(
    uf  = if (!is.null(c_uf) && c_uf %in% names(df)) as.character(.data[[c_uf]]) else NA_character_,
    cat = as.character(.data[[nm_use]]),
    key = as.character(.data$key_digits),
    val = suppressWarnings(as.numeric(.data$val))
  )
  out <- out[is.finite(out$val) & nzchar(out$cat) & nzchar(out$key), , drop = FALSE]
  out
}

# -------------------------------------------------------------------------
# Pirâmide
# -------------------------------------------------------------------------

.PYR_AGE_LEVELS_TOPDOWN <- c(
  "80 anos ou mais",
  "70 a 79 anos",
  "60 a 69 anos",
  "50 a 59 anos",
  "40 a 49 anos",
  "30 a 39 anos",
  "20 a 29 anos",
  "15 a 19 anos",
  "10 a 14 anos",
  "5 a 9 anos",
  "0 a 4 anos"
)

.pyr_canon_age_label <- function(age_start) {
  a <- suppressWarnings(as.integer(age_start))
  if (!is.finite(a)) return(NA_character_)
  if (a >= 80) return("80 anos ou mais")
  if (a >= 70) return("70 a 79 anos")
  if (a >= 60) return("60 a 69 anos")
  if (a >= 50) return("50 a 59 anos")
  if (a >= 40) return("40 a 49 anos")
  if (a >= 30) return("30 a 39 anos")
  if (a >= 20) return("20 a 29 anos")
  if (a >= 15) return("15 a 19 anos")
  if (a >= 10) return("10 a 14 anos")
  if (a >= 5)  return("5 a 9 anos")
  if (a >= 0)  return("0 a 4 anos")
  NA_character_
}

.pyr_prepare_wide <- function(df, kind = c("pop", "evt")) {
  kind <- match.arg(kind)
  df <- .safe_df(df)
  if (nrow(df) == 0) return(data.frame())

  nms <- names(df)

  age_col <- .pick(nms, c(
    "faixa_etaria", "FAIXA_ETARIA", "faixa", "FAIXA",
    "faixa_idade", "FAIXA_IDADE", "grupo_idade", "GRUPO_IDADE",
    "grupo_etario", "GRUPO_ETARIO", "idade_grupo", "IDADE_GRUPO",
    "age_group", "AGE_GROUP"
  ))
  ord_col <- .pick(nms, c("faixa_ord","FAIXA_ORD","idade_ini","IDADE_INI","idade_inicio","IDADE_INICIO","age_start","AGE_START","ord","ORD"))
  sex_col <- .pick(nms, c("sexo","SEXO","cs_sexo","CS_SEXO","sex","SEX","tp_sexo","TP_SEXO"))

  val_col <- .pick(nms, c(
    "valor", "VALOR",
    "casos", "CASOS",
    "casos_obitos", "obitos", "OBITOS",
    "casos_cura", "cura", "CURA",
    "n", "N", "total", "TOTAL", "count", "COUNT", "qt", "QT", "qtd", "QTD",
    "pop", "POP"
  ))

  if (is.null(sex_col) || is.null(val_col) || (is.null(ord_col) && is.null(age_col))) return(data.frame())

  df[[sex_col]] <- as.character(df[[sex_col]])
  df[[val_col]] <- suppressWarnings(as.numeric(df[[val_col]]))

  if (!is.null(ord_col) && ord_col %in% names(df)) {
    df$age_start <- suppressWarnings(as.integer(df[[ord_col]]))
  } else {
    df$age_start <- suppressWarnings(as.integer(gsub("^\\s*([0-9]+).*$", "\\1", as.character(df[[age_col]]))))
  }

  # If ord_col produced no valid integers, try extracting from age_col/faixa_etaria more flexibly
  if (all(is.na(df$age_start))) {
    if (!is.null(age_col) && age_col %in% names(df)) {
      df$age_start <- suppressWarnings(as.integer(gsub("[^0-9]+", "", as.character(df[[age_col]]))))
      df$age_start[is.na(df$age_start)] <- suppressWarnings(as.integer(gsub("^\\s*([0-9]+).*$", "\\1", as.character(df[[age_col]]))))
    } else if ("faixa_etaria" %in% names(df)) {
      df$age_start <- suppressWarnings(as.integer(gsub("[^0-9]+", "", as.character(df$faixa_etaria))))
    }
  }

  df$age_label <- vapply(df$age_start, .pyr_canon_age_label, FUN.VALUE = character(1))

  df <- df[
    is.finite(df[[val_col]]) &
      !is.na(df$age_label) & nzchar(df$age_label) &
      !is.na(df[[sex_col]]) & nzchar(df[[sex_col]]),
    , drop = FALSE
  ]
  if (nrow(df) == 0) return(data.frame())

  sx <- toupper(trimws(df[[sex_col]]))
  df$sexo_std <- dplyr::case_when(
    sx %in% c("M", "MASC", "MASCULINO", "HOMEM", "HOMENS", "1") ~ "Homens",
    sx %in% c("F", "FEM", "FEMININO", "MULHER", "MULHERES", "2") ~ "Mulheres",
    TRUE ~ df[[sex_col]]
  )

  agg <- df %>%
    group_by(.data$age_label, .data$sexo_std) %>%
    summarise(v = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop")

  base <- data.frame(age_label = .PYR_AGE_LEVELS_TOPDOWN, stringsAsFactors = FALSE)

  homens <- agg %>%
    filter(.data$sexo_std == "Homens") %>%
    transmute(age_label = .data$age_label, Homens = v)

  mulheres <- agg %>%
    filter(.data$sexo_std == "Mulheres") %>%
    transmute(age_label = .data$age_label, Mulheres = v)

  out <- base %>%
    left_join(homens, by = "age_label") %>%
    left_join(mulheres, by = "age_label")

  out$Homens   <- suppressWarnings(as.numeric(out$Homens));   out$Homens[!is.finite(out$Homens)] <- 0
  out$Mulheres <- suppressWarnings(as.numeric(out$Mulheres)); out$Mulheres[!is.finite(out$Mulheres)] <- 0

  names(out)[names(out) == "Homens"]   <- paste0("Homens_", kind)
  names(out)[names(out) == "Mulheres"] <- paste0("Mulheres_", kind)

  out
}

.pyr_colors <- function(pal = NULL) {
  if (is.null(pal)) pal <- c("#0B8A8F", "#E39D00")
  pal <- as.character(unlist(pal))
  pal <- pal[!is.na(pal) & nzchar(pal)]
  c1 <- pal[1]; c2 <- pal[2]
  if (length(c1) == 0 || is.na(c1) || !nzchar(c1)) c1 <- "#0B8A8F"
  if (length(c2) == 0 || is.na(c2) || !nzchar(c2)) c2 <- "#E39D00"
  list(male_dark = c1, female_dark = c2, male_light = c1, female_light = c2)
}

.plot_piramide_ibge <- function(df_pop, df_evt, palette = NULL, evt_label = "CASOS") {
  df_pop <- .safe_df(df_pop)
  df_evt <- .safe_df(df_evt)

  if (nrow(df_pop) == 0 && nrow(df_evt) == 0) {
    return(.ec_message("Sem dados de pirâmide (ano/escopo).", palette = palette))
  }

  w_pop <- .pyr_prepare_wide(df_pop, "pop")
  w_evt <- .pyr_prepare_wide(df_evt, "evt")

  # If prepare produced no rows, try a more tolerant manual aggregation from raw dfs
  if (nrow(w_pop) == 0) w_pop <- data.frame(age_label = .PYR_AGE_LEVELS_TOPDOWN, Homens_pop = 0, Mulheres_pop = 0)
  if (nrow(w_evt) == 0) w_evt <- data.frame(age_label = .PYR_AGE_LEVELS_TOPDOWN, Homens_evt = 0, Mulheres_evt = 0)

  # If both prepared tables exist but all event values are zero, attempt manual fallback
  try({
    total_evt_vals <- sum(ifelse("Homens_evt" %in% names(w_evt), w_evt$Homens_evt, 0), na.rm = TRUE) +
      sum(ifelse("Mulheres_evt" %in% names(w_evt), w_evt$Mulheres_evt, 0), na.rm = TRUE)

    if (!is.finite(total_evt_vals) || total_evt_vals == 0) {
      df_raw <- .safe_df(df_evt)
      if (nrow(df_raw) > 0) {
        # Build age_label from faixa_etaria or faixa_ord
        if (!"age_label" %in% names(df_raw)) {
          if ("faixa_etaria" %in% names(df_raw)) {
            df_raw$age_label <- as.character(df_raw$faixa_etaria)
          } else if ("faixa_ord" %in% names(df_raw)) {
            df_raw$age_label <- as.character(df_raw$faixa_ord)
          } else {
            df_raw$age_label <- NA_character_
          }
        }
        # normalize age_label to canonical buckets when possible
        df_raw$age_start_try <- suppressWarnings(as.integer(gsub("[^0-9]+", "", df_raw$age_label)))
        df_raw$age_label2 <- vapply(df_raw$age_start_try, function(a) {
          if (is.na(a)) return(NA_character_)
          .pyr_canon_age_label(a)
        }, FUN.VALUE = character(1))
        df_raw$age_label_final <- ifelse(!is.na(df_raw$age_label2) & nzchar(df_raw$age_label2), df_raw$age_label2, df_raw$age_label)

        sex_col <- .pick(names(df_raw), c("sexo","SEXO","cs_sexo","CS_SEXO","sex","SEX","tp_sexo","TP_SEXO"))
        val_col <- .pick(names(df_raw), c("valor","VALOR","casos","CASOS","n","N","total","TOTAL","count","COUNT","qt","QT","qtd","QTD"))
        if (!is.null(sex_col) && !is.null(val_col) && ("age_label_final" %in% names(df_raw))) {
          df_ag <- df_raw %>%
            filter(!is.na(.data$age_label_final) & nzchar(.data$age_label_final)) %>%
            mutate(sexo_tmp = toupper(trimws(as.character(.data[[sex_col]])))) %>%
            mutate(sexo_cat = dplyr::case_when(
              sexo_tmp %in% c("M","MASC","MASCULINO","HOMEM","HOMENS","1") ~ "Homens",
              sexo_tmp %in% c("F","FEM","FEMININO","MULHER","MULHERES","2") ~ "Mulheres",
              TRUE ~ as.character(.data[[sex_col]])
            )) %>%
            group_by(age_label_final, sexo_cat) %>%
            summarise(v = sum(as.numeric(.data[[val_col]]), na.rm = TRUE), .groups = "drop")

          if (nrow(df_ag) > 0) {
            homens_df <- df_ag %>% filter(sexo_cat == "Homens") %>% transmute(age_label = age_label_final, Homens_evt = v)
            mulheres_df <- df_ag %>% filter(sexo_cat == "Mulheres") %>% transmute(age_label = age_label_final, Mulheres_evt = v)
            w_evt_manual <- data.frame(age_label = .PYR_AGE_LEVELS_TOPDOWN, stringsAsFactors = FALSE)
            w_evt_manual <- w_evt_manual %>% left_join(homens_df, by = c("age_label" = "age_label")) %>% left_join(mulheres_df, by = c("age_label" = "age_label"))
            w_evt_manual$Homens_evt[is.na(w_evt_manual$Homens_evt)] <- 0
            w_evt_manual$Mulheres_evt[is.na(w_evt_manual$Mulheres_evt)] <- 0
            w_evt <- w_evt_manual
          }
        }
      }
    }
  }, silent = TRUE)

  out <- full_join(
    w_pop %>% select(age_label, Homens_pop, Mulheres_pop),
    w_evt %>% select(age_label, Homens_evt, Mulheres_evt),
    by = "age_label"
  )

  for (cc in c("Homens_pop", "Mulheres_pop", "Homens_evt", "Mulheres_evt")) {
    if (!cc %in% names(out)) out[[cc]] <- 0
    out[[cc]] <- suppressWarnings(as.numeric(out[[cc]]))
    out[[cc]][!is.finite(out[[cc]])] <- 0
  }

  out$age_label <- factor(out$age_label, levels = .PYR_AGE_LEVELS_TOPDOWN)
  out <- out %>% arrange(age_label)

  max_abs <- suppressWarnings(max(c(out$Homens_pop, out$Mulheres_pop, out$Homens_evt, out$Mulheres_evt), na.rm = TRUE))
  if (!is.finite(max_abs) || max_abs <= 0) max_abs <- 1

  col <- .pyr_colors(palette)
  # Ensure arrays passed to echarts are explicit numeric vectors and match age_label length
  age_vec <- as.character(out$age_label)
  n_age <- length(age_vec)
  hp <- suppressWarnings(as.numeric(out$Homens_pop)); if (length(hp) != n_age) hp <- rep(0, n_age)
  he <- suppressWarnings(as.numeric(out$Homens_evt)); if (length(he) != n_age) he <- rep(0, n_age)
  mp <- suppressWarnings(as.numeric(out$Mulheres_pop)); if (length(mp) != n_age) mp <- rep(0, n_age)
  me <- suppressWarnings(as.numeric(out$Mulheres_evt)); if (length(me) != n_age) me <- rep(0, n_age)

  opt <- list(
    animation = FALSE,
    tooltip = list(
      trigger = "axis",
      confine = TRUE,
      renderMode = "html",
      axisPointer = list(type = "shadow"),
      backgroundColor = "rgba(17,24,39,0.96)",
      borderWidth = 0,
      position = .ec_tooltip_position,
      extraCssText = paste(
        "box-shadow:0 10px 22px rgba(0,0,0,.22);",
        "border-radius:10px;",
        "padding:6px 8px;",
        "color:#fff;",
        "font-size:10.5px;",
        "line-height:1.15;",
        "max-width:260px;",
        "max-height:140px;",
        "overflow:hidden;",
        "pointer-events:none;",
        sep = " "
      ),
      formatter = .ec_tooltip_fmt_axis(abs_values = FALSE)
    ),
    legend = list(
      type = "scroll",
      top = 2,
      left = 0,
      right = 0,
      itemGap = 8,
      textStyle = list(fontSize = 10)
    ),
    grid = list(
      list(left = "2%",  right = "52%", top = 44, bottom = 48, containLabel = TRUE),
      list(left = "49%", right = "49%", top = 44, bottom = 48, containLabel = FALSE),
      list(left = "52%", right = "2%",  top = 44, bottom = 48, containLabel = TRUE)
    ),
    xAxis = list(
      list(type = "value", gridIndex = 0, inverse = TRUE, min = 0, max = max_abs,
           axisLabel = list(formatter = htmlwidgets::JS("function(v){ try{return Number(v).toLocaleString('pt-BR');}catch(e){return v;} }"), fontSize = 10),
           splitLine = list(show = TRUE)),
      list(type = "value", gridIndex = 1, show = FALSE, min = 0, max = max_abs),
      list(type = "value", gridIndex = 2, min = 0, max = max_abs,
           axisLabel = list(formatter = htmlwidgets::JS("function(v){ try{return Number(v).toLocaleString('pt-BR');}catch(e){return v;} }"), fontSize = 10),
           splitLine = list(show = TRUE))
    ),
    yAxis = list(
       list(type = "category", gridIndex = 0, data = age_vec, axisLabel = list(show = FALSE), axisTick = list(show = FALSE), axisLine = list(show = FALSE)),
       list(type = "category", gridIndex = 1, data = age_vec,
           axisLabel = list(show = TRUE, fontSize = 10, color = "rgba(0,0,0,.72)", align = "center"),
           axisTick = list(show = FALSE), axisLine = list(show = FALSE), splitLine = list(show = FALSE)),
       list(type = "category", gridIndex = 2, data = age_vec, axisLabel = list(show = FALSE), axisTick = list(show = FALSE), axisLine = list(show = FALSE))
    ),
    series = list(
       list(name = "População (Homens)", type = "bar", xAxisIndex = 0, yAxisIndex = 0,
         data = hp, barMaxWidth = 16, itemStyle = list(color = col$male_light, opacity = 0.18),
           emphasis = list(focus = "series"), z = 1, barGap = "-100%", barCategoryGap = "35%"),
       list(name = sprintf("%s (Homens)", evt_label), type = "bar", xAxisIndex = 0, yAxisIndex = 0,
         data = he, barMaxWidth = 16, itemStyle = list(color = col$male_dark, opacity = 0.92),
           emphasis = list(focus = "series"), z = 2, barGap = "-100%", barCategoryGap = "35%"),
       list(name = "População (Mulheres)", type = "bar", xAxisIndex = 2, yAxisIndex = 2,
         data = mp, barMaxWidth = 16, itemStyle = list(color = col$female_light, opacity = 0.18),
           emphasis = list(focus = "series"), z = 1, barGap = "-100%", barCategoryGap = "35%"),
       list(name = sprintf("%s (Mulheres)", evt_label), type = "bar", xAxisIndex = 2, yAxisIndex = 2,
         data = me, barMaxWidth = 16, itemStyle = list(color = col$female_dark, opacity = 0.92),
           emphasis = list(focus = "series"), z = 2, barGap = "-100%", barCategoryGap = "35%")
    )
  )

  p <- .ec_widget_from_option(opt)
  p <- .ec_hide_tip_on_mouseleave(p)
  .ec_force_resize_on_tab(p)
}

# Pirâmide simples com echarts4r (barra horizontal, homens à esquerda)
.plot_piramide_simple_echarts <- function(df_evt, palette = NULL, evt_label = "CASOS") {
  df_evt <- .safe_df(df_evt)
  if (nrow(df_evt) == 0) return(.ec_message("Sem dados de pirâmide (ano/escopo).", palette = palette))

  w_evt <- .pyr_prepare_wide(df_evt, "evt")
  if (nrow(w_evt) == 0) return(.ec_message("Sem dados de pirâmide (ano/escopo).", palette = palette))

  # Normaliza estrutura
  if (!"Homens_evt" %in% names(w_evt)) w_evt$Homens_evt <- 0
  if (!"Mulheres_evt" %in% names(w_evt)) w_evt$Mulheres_evt <- 0

  w_evt$Homens_evt <- suppressWarnings(as.numeric(w_evt$Homens_evt))
  w_evt$Mulheres_evt <- suppressWarnings(as.numeric(w_evt$Mulheres_evt))
  w_evt$Homens_evt[!is.finite(w_evt$Homens_evt)] <- 0
  w_evt$Mulheres_evt[!is.finite(w_evt$Mulheres_evt)] <- 0

  # Order so youngest at bottom and oldest at top in the horizontal bars
  w_evt$age_label <- factor(w_evt$age_label, levels = rev(.PYR_AGE_LEVELS_TOPDOWN))
  w_evt <- w_evt %>% arrange(age_label)

  plot_df <- data.frame(
    age_label = as.character(w_evt$age_label),
    Homens = -abs(w_evt$Homens_evt),
    Mulheres = abs(w_evt$Mulheres_evt),
    stringsAsFactors = FALSE
  )

  # Paleta
  col <- .pyr_colors(palette)

  p <- e_charts(plot_df, age_label) %>%
    e_bar(Homens, name = sprintf("Homens (%s)", evt_label), itemStyle = list(color = col$male_dark), barGap = "-100%", barCategoryGap = "35%") %>%
    e_bar(Mulheres, name = sprintf("Mulheres (%s)", evt_label), itemStyle = list(color = col$female_dark), barGap = "-100%", barCategoryGap = "35%") %>%
    e_flip_coords() %>%
    e_x_axis(
      axisLabel = list(
        formatter = htmlwidgets::JS("function(v){ try{return Math.abs(Number(v)).toLocaleString('pt-BR');}catch(e){return v;} }")
      )
    ) %>%
    e_y_axis(type = "category") %>%
    .ec_finalize(palette = palette, datazoom = FALSE, tooltip_trigger = "axis",
                 grid_left = 40, grid_top = 18, grid_bottom = 56, legend_show = TRUE)

  .ec_force_resize_on_tab(p)
}

# -------------------------------------------------------------------------
# Variáveis de interesse 
# -------------------------------------------------------------------------

.plot_comp <- function(df, var_sel = NULL, top_n = 30, palette = NULL) {
  df <- .safe_df(df)
  if (nrow(df) == 0) return(.ec_message("Sem dados de variáveis de interesse (ano/escopo).", palette = palette))

  nms <- names(df)
  if (!"variavel" %in% nms) {
    c0 <- .pick(nms, c("variavel", "VARIAVEL"))
    if (!is.null(c0)) df$variavel <- df[[c0]]
  }
  if (!"categoria" %in% nms) {
    c0 <- .pick(nms, c("categoria", "valor_lbl", "VALOR_LBL", "cat", "CAT"))
    if (!is.null(c0)) df$categoria <- df[[c0]]
  }
  if (!"total" %in% nms) {
    c0 <- .pick(nms, c("total", "n", "N", "TOTAL"))
    if (!is.null(c0)) df$total <- df[[c0]]
  }

  df$variavel <- toupper(trimws(as.character(df$variavel)))
  df$categoria <- as.character(df$categoria)
  df$total <- suppressWarnings(as.numeric(df$total))
  df <- df[is.finite(df$total) & nzchar(df$variavel) & nzchar(df$categoria), , drop = FALSE]
  if (nrow(df) == 0) return(.ec_message("Sem dados de variáveis de interesse (ano/escopo).", palette = palette))

  vars <- sort(unique(df$variavel))
  if (is.null(var_sel) || !nzchar(var_sel) || !(toupper(var_sel) %in% vars)) var_sel <- vars[[1]]
  df <- df %>% filter(.data$variavel == toupper(var_sel))

  if (identical(toupper(var_sel), "CS_GESTANT")) {
    cat_norm <- .norm_comp_cat(df$categoria)
    df <- df[!cat_norm %in% c("NAOSEAPLICA", "NAOAPLICA"), , drop = FALSE]
  }

  df_tot <- df %>%
    group_by(.data$categoria) %>%
    summarise(total = sum(.data$total, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(.data$total))

  total_all <- sum(df_tot$total, na.rm = TRUE)

  tn <- suppressWarnings(as.integer(top_n))
  if (!is.finite(tn) || tn <= 0) tn <- 30L
  if (nrow(df_tot) > tn) df_tot <- df_tot %>% slice_head(n = tn)

  df_tot$perc <- if (isTRUE(is.finite(total_all)) && total_all > 0) {
    (df_tot$total / total_all) * 100
  } else {
    NA_real_
  }

  df_tot <- df_tot %>% arrange(.data$total)
  df_tot$categoria <- as.character(df_tot$categoria)

  p <- e_charts(df_tot, categoria) %>%
    e_bar(perc, name = "Percentual") %>%
    e_flip_coords() %>%
    e_x_axis(
      type = "value",
      axisLabel = list(
        fontSize = 11,
        formatter = htmlwidgets::JS("function(v){ if(v==null || isNaN(v)) return ''; try{return Number(v).toLocaleString('pt-BR',{minimumFractionDigits:1, maximumFractionDigits:1}) + '%';}catch(e){return v + '%';} }")
      )
    ) %>%
    e_y_axis(type = "category", axisLabel = list(interval = 0, fontSize = 11))

  # Carrega a quantidade (total) junto de cada barra para o tooltip exibir "percentual (quantidade)"
  if (length(p$x$opts$series) >= 1 && length(p$x$opts$series[[1]]$data) == nrow(df_tot)) {
    p$x$opts$series[[1]]$data <- lapply(seq_len(nrow(df_tot)), function(i) {
      list(value = df_tot$perc[[i]], total = df_tot$total[[i]])
    })
  }

  p <- p %>%
    .ec_finalize(palette = palette, datazoom = nrow(df_tot) > 12, tooltip_trigger = "item",
                 tooltip_formatter = .ec_tooltip_fmt_comp(),
                 grid_left = 50, grid_top = 18, grid_bottom = 56, legend_show = FALSE)

  .ec_force_resize_on_tab(p)
}

# -------------------------------------------------------------------------
# API dispatch
# -------------------------------------------------------------------------

.api_get_fun <- function(api, name) {
  if (is.null(api) || !nzchar(name)) return(NULL)
  if (is.list(api) && is.function(api[[name]])) return(api[[name]])
  if (is.environment(api) && exists(name, envir = api, inherits = FALSE)) {
    f <- get(name, envir = api, inherits = FALSE)
    if (is.function(f)) return(f)
  }
  if (is.list(api) && !is.null(api[[name]]) && is.function(api[[name]])) return(api[[name]])
  NULL
}

.call_with_accepted_args <- function(fun, args) {
  if (!is.function(fun)) return(NULL)
  fs <- tryCatch(names(formals(fun)), error = function(e) NULL)
  if (is.null(fs)) return(tryCatch(do.call(fun, args), error = function(e) NULL))
  if ("..." %in% fs) return(tryCatch(do.call(fun, args), error = function(e) NULL))
  keep <- intersect(names(args), fs)
  tryCatch(do.call(fun, args[keep]), error = function(e) NULL)
}

.api_call_any <- function(api, candidates, args) {
  for (nm in candidates) {
    f <- .api_get_fun(api, nm)
    if (!is.null(f)) {
      out <- .call_with_accepted_args(f, args)
      if (!is.null(out)) return(out)
    }
  }
  NULL
}


# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------
mod_charts_ui <- function(id, cfg = NULL, pack = NULL, title = NULL, ...) {

  ns <- NULL
  if (is.function(id)) {
    ns <- id
  } else {
    id <- as.character(id)[1]
    ns <- shiny::NS(id)
  }

  pack_obj <- if (is.function(pack)) tryCatch(pack(), error = function(e) NULL) else pack
  cfg_obj <- if (is.function(cfg)) tryCatch(cfg(), error = function(e) NULL) else cfg
  doenca_ui <- toupper(trimws(as.character(
    .obj_get(pack_obj, "DOENCA", NULL) %||% .obj_get(cfg_obj, "DOENCA", NULL) %||% ""
  )))
  show_help_ui <- doenca_ui %in% c("TUBERCULOSE", "TB", "TUBERCULOSIS")

  .help_text <- function(lines) {
    lines <- as.character(lines %||% character(0))
    paste0("<div class='sinan-help-text'>", paste(lines, collapse = "<br>"), "</div>")
  }

  .help_badge <- function(label, tip_html = label) {
    tags$span(
      class = "sinan-help-badge",
      title = label,
      `aria-label` = label,
      `data-bs-toggle` = "tooltip",
      `data-bs-title` = tip_html,
      `data-bs-html` = "true",
      `data-bs-custom-class` = "sinan-help-tooltip",
      `data-bs-placement` = "top",
      "i"
    )
  }

  evol_label <- if (show_help_ui) {
    tagList(
      "Evolução temporal",
      .help_badge(
        label = "Ajuda da evolucao temporal",
        tip_html = .help_text(c(
          "Meses do ano: compara a sazonalidade dentro do ano selecionado.",
          "Todos os anos: mostra tendencia historica anual.",
          "A metrica escolhida nos KPIs guia o destaque dos graficos.",
          "Series podem variar por doenca e disponibilidade de dados."
        ))
      )
    )
  } else {
    "Evolução temporal"
  }

  tagList(
    tags$head(
      tags$style(HTML("
        .sinan-charts-root{
          --sinan-accent: #0B8A8F;
          --sinan-accent-soft: rgba(11,138,143,.18);
          --sinan-accent-soft2: rgba(11,138,143,.35);
        }
        .sinan-debugbar .form-check{ margin:0; display:flex; align-items:center; gap:.35rem; }
         .sinan-debugbar .form-check-label{ font-weight:900; font-size:12px; color: rgba(0,0,0,.65); }
         .sinan-debugbar .btn{
           border-radius: 12px;
           font-weight: 900;
           font-size: 12px;
           padding: 6px 10px;
         }
         .sinan-debugpanel{
           margin: 0 4px 8px 4px;
           border-radius: 14px;
           border: 1px solid rgba(0,0,0,.10);
           background: rgba(255,255,255,.92);
           box-shadow: 0 14px 34px rgba(0,0,0,.10);
           overflow:hidden;
         }
         .sinan-debugpanel pre{
           margin:0;
           padding: 10px 12px;
           font-size: 11px;
           line-height: 1.25;
           max-height: 220px;
           overflow:auto;
         }
        .sinan-panel{min-width:0; height:100%; min-height: clamp(760px, 80vh, 980px);}
        .sinan-panel .bslib-card-body,
        .sinan-panel .card-body{min-width:0; overflow:hidden; height:100%;}
        .sinan-charts{height:100%; min-height:0; min-width:0; width:100%; overflow:hidden; display:flex; flex-direction:column;}
        .sinan-charts .tab-content{height:100%; min-height:0; min-width:0; overflow:hidden; position:relative; z-index:1; isolation:isolate;}
        .sinan-charts-root .nav{position:relative; z-index:50; pointer-events:auto; isolation:isolate;}
        .sinan-charts-root .nav *{pointer-events:auto;}

        .sinan-charts-pane{
          flex:1 1 auto;
          min-width:0;
          overflow:visible;
          min-height: 780px;
        }
        .sinan-charts .tab-pane{
          height:100%;
          min-height:0;
          min-width:0;
          overflow:hidden;
          gap:.35rem;
          padding-top:.25rem;
        }
        .sinan-charts .tab-pane.show.active{
          display:flex;
          flex-direction:column;
        }
        .sinan-evol-layout-months{
          display:flex;
          flex-direction:column;
          height:50%;
          gap:12px;
        }
        .sinan-evol-chart-half{
          flex:1 1 50%;
          min-height:0;
          display:flex;
          flex-direction:column;
        }
        .sinan-evol-layout-years{
          display:flex;
          flex-direction:column;
          height:50%;
          gap:12px;
        }
        .sinan-evol-main-chart{
          flex:0 0 auto;
          min-height:200px;
        }
        .sinan-evol-bottom-row{
          flex:1 1 auto;
          display:flex;
          flex-direction:row;
          gap:12px;
          min-height:180px;
        }
        .sinan-evol-bottom-chart{
          flex:1 1 50%;
          min-width:0;
          display:flex;
          flex-direction:column;
        }
        .sinan-comp, .sinan-pyr{
          flex:1 1 50%;
          min-height:0;
          display:flex;
          flex-direction:column;
          overflow:hidden;
        }
        .sinan-echart-fill{flex:1 1 auto; min-height:0; overflow:hidden;}
        .sinan-charts .echarts4r.html-widget{
          width:100% !important;
          height:100% !important;
          min-height: 340px !important;
          min-width:0 !important;
        }
        .sinan-charts .echarts4r.html-widget > div{width:100% !important; height:100% !important;}

        .sinan-charts .sinan-comp-controls{
          display:flex; gap:.7rem; align-items:flex-end; flex-wrap:wrap;
          padding:.15rem 0 .05rem 0;
        }
        .sinan-charts .sinan-comp-controls .form-group{margin:0;}

        .sinan-charts-kpis{
          display:grid;
          grid-template-columns: repeat(2, minmax(220px, 1fr));
          gap:.65rem;
          padding:.25rem .1rem .5rem .1rem;
        }
        .sinan-mini-kpi{
          border-radius: 14px;
          border: 2.5px solid #5B21B6;
          background: linear-gradient(180deg, rgba(255,255,255,.98), rgba(250,245,255,.92));
          box-shadow: 0 14px 26px rgba(91,33,182,.22), 0 6px 18px rgba(0,0,0,.08);
          padding: 10px 12px;
          display:flex;
          flex-direction:column;
          gap:2px;
          min-width:0;
        }
        .sinan-mini-kpi .kpi-title{
          font-size: 11px;
          font-weight: 900;
          letter-spacing: .04em;
          text-transform: uppercase;
          color: rgba(0,0,0,.60);
        }
        .sinan-mini-kpi .kpi-value{
          font-size: 22px;
          font-weight: 900;
          color: #0b1220;
        }
        .sinan-mini-kpi .kpi-sub{
          font-size: 11px;
          font-weight: 700;
          color: rgba(0,0,0,.55);
        }
        .sinan-comp-note{
          margin-top: 6px;
          font-size: 12px;
          font-weight: 700;
          color: rgba(0,0,0,.62);
        }

        .sinan-seg{
          display:inline-flex;
          border-radius: 999px;
          padding: 3px;
          background: rgba(0,0,0,.04);
          box-shadow:
            inset 0 1px 2px rgba(0,0,0,.10),
            0 8px 22px rgba(0,0,0,.08);
          border: 1px solid rgba(0,0,0,.08);
          gap: 4px;
          user-select:none;
        }
        .sinan-seg input{display:none;}
        .sinan-seg label{
          margin:0;
          padding: 8px 12px;
          border-radius: 999px;
          font-size: 12px;
          font-weight: 800;
          cursor: pointer;
          color: rgba(0,0,0,.62);
          transition: transform .12s ease, box-shadow .12s ease, background .12s ease, color .12s ease;
          background: transparent;
        }
        .sinan-seg input:checked + label{
          color: #0b1220;
          background: linear-gradient(180deg, rgba(255,255,255,.92), rgba(255,255,255,.70));
          box-shadow:
            0 10px 22px rgba(0,0,0,.14),
            inset 0 1px 0 rgba(255,255,255,.9);
          transform: translateY(-1px);
          outline: 2px solid var(--sinan-accent-soft);
          outline-offset: 2px;
        }

        .sinan-charts-root .nav-tabs{
          border-bottom: 0 !important;
          gap: 10px;
          padding: 8px 6px 10px 6px;
          background: linear-gradient(180deg, rgba(255,255,255,.82), rgba(255,255,255,.0));
          backdrop-filter: blur(8px);
          border-radius: 16px;
        }
        .sinan-charts-root .nav-tabs .nav-link{
          position: relative;
          border: 1px solid rgba(0,0,0,.10) !important;
          border-bottom: 0 !important;
          border-radius: 14px !important;
          padding: 10px 14px !important;
          font-weight: 900 !important;
          font-size: 13px !important;
          color: rgba(0,0,0,.65) !important;
          background: linear-gradient(180deg, rgba(255,255,255,.92), rgba(245,247,250,.88)) !important;
          box-shadow:
            0 10px 22px rgba(0,0,0,.10),
            inset 0 1px 0 rgba(255,255,255,.85) !important;
          transform: translateY(0);
          transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease, color .12s ease;
        }
        .sinan-charts-root .nav-tabs .nav-link:hover{
          transform: translateY(-1px);
          box-shadow:
            0 14px 26px rgba(0,0,0,.14),
            inset 0 1px 0 rgba(255,255,255,.9) !important;
          border-color: rgba(0,0,0,.14) !important;
        }
        .sinan-charts-root .nav-tabs .nav-link.active{
          color: #0b1220 !important;
          border-color: var(--sinan-accent) !important;
          background:
            linear-gradient(180deg, rgba(255,255,255,.98), rgba(255,255,255,.82)) !important;
          box-shadow:
            0 18px 30px rgba(0,0,0,.16),
            0 0 0 3px var(--sinan-accent-soft),
            inset 0 1px 0 rgba(255,255,255,.95) !important;
          transform: translateY(-2px);
        }
        .sinan-charts-root .nav-tabs .nav-link.active::after{
          content:'';
          position:absolute;
          left: 12px;
          right: 12px;
          bottom: -2px;
          height: 3px;
          border-radius: 999px;
          background: var(--sinan-accent);
          box-shadow: 0 8px 18px var(--sinan-accent-soft2);
        }
        .sinan-help-badge{
          display:inline-flex;
          align-items:center;
          justify-content:center;
          width:16px;
          height:16px;
          margin-left:6px;
          border-radius:999px;
          background:#0b8a8f;
          color:#fff;
          font-size:10px;
          font-weight:900;
          line-height:1;
          cursor:help;
          vertical-align:middle;
          box-shadow:0 4px 10px rgba(11,138,143,.35);
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

        .sinan-selectize{width:100%; max-width:640px;}
        .sinan-selectize .selectize-control{width:100%;}
        .sinan-selectize .selectize-input{
          border-radius: 14px !important;
          padding: 10px 12px !important;
          border: 1px solid rgba(0,0,0,.10) !important;
          box-shadow: 0 8px 22px rgba(0,0,0,.08) !important;
          background: #fff !important;
          font-size: 13px !important;
        }
        .sinan-selectize .selectize-input.focus{
          border-color: var(--sinan-accent) !important;
          box-shadow: 0 10px 28px rgba(0,0,0,.08), 0 0 0 3px var(--sinan-accent-soft) !important;
        }
        .sinan-selectize .selectize-dropdown{
          border-radius: 14px !important;
          border: 1px solid rgba(0,0,0,.10) !important;
          box-shadow: 0 14px 34px rgba(0,0,0,.16) !important;
          overflow: hidden;
        }
        .sinan-selectize .selectize-dropdown .option{
          padding: 10px 12px !important;
          font-size: 13px !important;
        }

        .sinan-topicos-box{
          width: 100%;
          min-width: 0;
        }
        .sinan-topicos-box .card-body{
          padding: 14px 16px 16px 16px !important;
        }
        .sinan-topicos-title{
          margin: 0 0 12px 2px;
          font-size: 18px;
          line-height: 1.2;
          font-weight: 800;
          color: #111827;
        }
        .sinan-topicos-grid{
          display: grid !important;
          grid-template-columns: repeat(2, minmax(0, 1fr)) !important;
          gap: 12px;
          width: 100%;
          min-width: 0;
          align-items: stretch;
        }
        .sinan-topico-item{
          min-width: 0;
          min-height: 360px;
          display: flex;
          flex-direction: column;
          border: 1px solid rgba(17,24,39,.10);
          border-radius: 8px;
          background: #fff;
          padding: 10px;
          overflow: hidden;
        }
        .sinan-topico-title{
          margin: 0 0 8px 2px;
          font-size: 13px;
          line-height: 1.25;
          font-weight: 800;
          color: #374151;
        }
        .sinan-topico-chart{
          flex: 1 1 auto;
          min-height: 320px;
          min-width: 0;
        }
        .sinan-topico-chart .echarts4r.html-widget{
          width: 100% !important;
          height: 320px !important;
          min-height: 320px !important;
        }
        @media (max-width: 900px){
          .sinan-topicos-grid{ grid-template-columns: 1fr !important; }
        }

        @media (max-width: 1400px){
          .sinan-charts-pane{ height: clamp(600px, 70vh, 920px); min-height: 600px; }
          .sinan-charts .echarts4r.html-widget{ min-height: 600px !important; }
        }
        @media (max-width: 1100px){
          .sinan-charts-pane{ height: clamp(560px, 66vh, 820px); min-height: 560px; }
          .sinan-charts .echarts4r.html-widget{ min-height: 560px !important; }
          .sinan-selectize{ max-width: none; }
        }
        @media (max-width: 900px){
          .sinan-charts-kpis{ grid-template-columns: 1fr; }
        }
      ")),
      tags$script(HTML(sprintf("
        (function(){
          function hexToRgb(hex){
            try{
              if(!hex) return null;
              hex = String(hex).trim();
              if(hex[0] === '#') hex = hex.slice(1);
              if(hex.length === 3) hex = hex[0]+hex[0]+hex[1]+hex[1]+hex[2]+hex[2];
              if(hex.length !== 6) return null;
              var r = parseInt(hex.slice(0,2), 16);
              var g = parseInt(hex.slice(2,4), 16);
              var b = parseInt(hex.slice(4,6), 16);
              if([r,g,b].some(function(v){ return isNaN(v); })) return null;
              return {r:r,g:g,b:b};
            }catch(e){ return null; }
          }
          function setAccent(root, color){
            try{
              if(!root || !color) return;
              var rgb = hexToRgb(color);
              root.style.setProperty('--sinan-accent', color);
              if(rgb){
                root.style.setProperty('--sinan-accent-soft',  'rgba(' + rgb.r + ',' + rgb.g + ',' + rgb.b + ',.18)');
                root.style.setProperty('--sinan-accent-soft2', 'rgba(' + rgb.r + ',' + rgb.g + ',' + rgb.b + ',.35)');
              }
            }catch(e){}
          }

          function installTabFix(rootId){
            try{
              var root = document.getElementById(rootId);
              if(!root) return;
              root.addEventListener('click', function(ev){
                var a = ev.target && ev.target.closest ? ev.target.closest('.nav-tabs .nav-link') : null;
                if(!a) return;
                try{
                  if(window.bootstrap && window.bootstrap.Tab){
                    ev.preventDefault();
                    var inst = window.bootstrap.Tab.getOrCreateInstance(a);
                    inst.show();
                  }
                }catch(e){}
              }, true);
            }catch(e){}
          }

          function installTooltips(rootId){
            try{
              if(!window.bootstrap || !window.bootstrap.Tooltip) return;
              var root = document.getElementById(rootId);
              if(!root) return;
              var nodes = root.querySelectorAll('[data-bs-toggle=tooltip]');
              nodes.forEach(function(el){
                try{ window.bootstrap.Tooltip.getOrCreateInstance(el); }catch(e){}
              });
            }catch(e){}
          }

          if(window.Shiny && Shiny.addCustomMessageHandler){
            Shiny.addCustomMessageHandler('sinan_set_accent', function(msg){
              try{
                if(!msg) return;
                var id = msg.id || '';
                var color = msg.color || '';
                if(!id || !color) return;
                var root = document.getElementById(id);
                if(!root) return;
                setAccent(root, color);
              }catch(e){}
            });
          }

          installTabFix('%s');
          setTimeout(function(){ installTooltips('%s'); }, 0);
        })();
      ", ns("charts_root"), ns("charts_root"))))
    ),

    bslib::card(
      class = "shadow-sm sinan-panel",
      style = "height:100%; min-height:0; min-width:0;",
      bslib::card_body(
        style = "height:100%; min-height:0; min-width:0; padding:.15rem .25rem; overflow:hidden;",
        div(
          id = ns("charts_root"),
          class = "sinan-charts-root",
          div(
            class = "sinan-charts",
            div(
              class = "sinan-charts-pane",
              bslib::navset_tab(
                id = ns("charts_tabs"),
                bslib::nav_panel(
                  evol_label,
                  div(
                    class = "sinan-comp-controls",
                    div(
                      class = "form-group",
                      div(
                        class = "sinan-seg",
                        tags$input(id = ns("evol_hz_m"), type = "radio", name = ns("evol_horizon"), value = "months", checked = "checked"),
                        tags$label(`for` = ns("evol_hz_m"), "Meses do ano"),
                        tags$input(id = ns("evol_hz_y"), type = "radio", name = ns("evol_horizon"), value = "years"),
                        tags$label(`for` = ns("evol_hz_y"), "Todos os anos")
                      ),
                      tags$input(id = ns("evol_horizon"), type = "hidden", value = "months"),
                      tags$script(HTML(sprintf("
                        (function(){
                          var hid = document.getElementById('%s');
                          var r1 = document.getElementById('%s');
                          var r2 = document.getElementById('%s');
                          function sync(){
                            try{
                              var v = (r2 && r2.checked) ? 'years' : 'months';
                              hid.value = v;
                              if(window.Shiny && Shiny.setInputValue){
                                Shiny.setInputValue('%s', v, {priority:'event'});
                              }
                            }catch(e){}
                          }
                          if(r1) r1.addEventListener('change', sync);
                          if(r2) r2.addEventListener('change', sync);
                          setTimeout(sync, 0);
                        })();
                      ",
                        ns("evol_horizon"),
                        ns("evol_hz_m"),
                        ns("evol_hz_y"),
                        ns("evol_horizon")
                      )))
                    )
                  ),
                  uiOutput(ns("evol_layout_ui"))
                ),
                bslib::nav_panel(
                  "Ranking de bairros",
                  div(class = "sinan-comp-controls", uiOutput(ns("rank_controls_ui"))),
                  div(class = "sinan-echart-fill", echarts4rOutput(ns("rank_main"), height = "720px"))
                ),
                bslib::nav_panel("Pirâmide etária", uiOutput(ns("piramide_ui")))
              )
            )
          )
        )
      )
    )
  )
}

# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

mod_charts_server <- function(id, cfg, pack, nav = NULL, state = NULL, ano = NULL, metric = NULL,
                             data_api = NULL, data = NULL, ...) {

  moduleServer(id, function(input, output, session) {

    cfg_r  <- reactive(.res(cfg))
    pack_r <- reactive(.res(pack))
     dbg_default <- reactive({
       .is_true(getOption("sinan.debug", FALSE)) ||
         .is_true(Sys.getenv("SINAN_DEBUG", unset = "FALSE")) ||
         .is_true(.obj_get(cfg_r(), "DEBUG", FALSE)) ||
         .is_true(.obj_get(pack_r(), "DEBUG", FALSE))
     })
 
     observeEvent(dbg_default(), {
       tryCatch(updateCheckboxInput(session, "debug_on", value = isTRUE(dbg_default())), error = function(e) NULL)     }, ignoreInit = FALSE)
 
     last_dump_r <- reactiveVal(NULL)
 
     output$debug_last <- renderUI({
       p <- last_dump_r()
       if (is.null(p) || !nzchar(p)) return(NULL)
       tags$span(style="font-size:11px; font-weight:900; color: rgba(0,0,0,.65);",
                 paste0("Último dump: ", basename(p)))
     })

    pal <- reactive({
      as.character(.ec_palette(cfg_r(), pack_r()))
    })
    
    pal_bars <- reactive({
      as.character(.ec_palette(cfg_r(), pack_r(), type = "bars"))
    })
    
    pal_lines <- reactive({
      as.character(.ec_palette(cfg_r(), pack_r(), type = "lines"))
    })
    
    .scope_fallbacks <- function(sc) {
      sc <- sc %||% list(level="UF", uf="PE", mun=NULL, mun6=NULL, mun7=NULL)
      if (identical(sc$level, "MUN")) {
        return(list(
          sc,
          .normalize_scope("UF", uf = sc$uf, mun = NULL),
          .normalize_scope("BR", uf = NULL, mun = NULL)
        ))
      }
      if (identical(sc$level, "UF")) {
        return(list(sc, .normalize_scope("BR", uf = NULL, mun = NULL)))
      }
      list(sc)
    }

    .resolve_doenca <- function(...) {
      vals <- list(...)
      vals <- lapply(vals, function(v) .as_chr1(v, NULL))
      vals <- vals[!vapply(vals, is.null, logical(1))]
      if (length(vals) == 0) return(NULL)
      v <- toupper(trimws(as.character(vals[[1]])))
      if (!nzchar(v)) NULL else v
    }

    .doenca <- reactive({
      d <- .resolve_doenca(
        .obj_get(pack_r(), "DOENCA", NULL),
        .obj_get(pack_r(), "doenca", NULL),
        .obj_get(pack_r(), "DISEASE", NULL),
        .obj_get(pack_r(), "disease", NULL),

        .obj_get(state, "DOENCA", NULL),
        .obj_get(state, "doenca", NULL),
        .obj_get(nav,   "DOENCA", NULL),
        .obj_get(nav,   "doenca", NULL),

        .obj_get(cfg_r(),  "DOENCA", NULL),
        .obj_get(cfg_r(),  "doenca", NULL),

        Sys.getenv("SINAN_DOENCA", unset = ""),
        Sys.getenv("DOENCA", unset = "")
      )
      d %||% "TUBERCULOSE"
    })

    cache_env   <- new.env(parent = emptyenv())
    rank_map_r  <- reactiveVal(NULL)
    comp_vars_r <- reactiveVal(character(0))

    .is_tuberculose <- function() {
      d0 <- toupper(trimws(as.character(.doenca() %||% "")))
      d0 %in% c("TUBERCULOSE", "TUBERCULOSIS", "TB")
    }

    .ts_history_palette <- function() {
      if (!isTRUE(.is_tuberculose())) return(NULL)
      cols <- .obj_get(pack_r(), "COLORS", list()) %||% list()
      c(
        .obj_get(cols, "accent", "#4A8EFF") %||% "#4A8EFF",
        .obj_get(cols, "danger", "#DB400A") %||% "#DB400A",
        .obj_get(cols, "text_muted", "#6C757D") %||% "#6C757D"
      )
    }

    .is_recife_lock <- function() {
      isTRUE(.obj_get(cfg_r(), "LOCK_RECIFE", FALSE) %||% .obj_get(cfg_r(), "lock_recife", FALSE))
    }

    .selected_bairro_key <- function() {
      invisible(.obj_get(nav, "_map_tick", NULL)); invisible(.obj_get(state, "_map_tick", NULL))
      key <- .obj_get(nav, "selected_bairro_key", NULL) %||% .obj_get(state, "selected_bairro_key", NULL)
      key <- .as_chr1(key, "")
      if (nzchar(key)) key else NULL
    }

    .selected_bairro_name <- function() {
      invisible(.obj_get(nav, "_map_tick", NULL)); invisible(.obj_get(state, "_map_tick", NULL))
      .as_chr1(.obj_get(nav, "selected_bairro_name", NULL) %||% .obj_get(state, "selected_bairro_name", NULL), .selected_bairro_key() %||% "")
    }

    .is_tb_recife_bairro <- function() {
      isTRUE(.is_tuberculose()) && isTRUE(.is_recife_lock()) && !is.null(.selected_bairro_key())
    }
    tb_grau_filter_r <- reactive({
      x <- .obj_get(nav, "tb_grau_incapacidade", NULL) %||% .obj_get(state, "tb_grau_incapacidade", NULL)
      if (is.null(x) || !is.list(x) || is.null(x$value) || is.null(x$var)) return(NULL)
      val <- as.character(x$value %||% "")
      var <- as.character(x$var %||% "")
      if (!nzchar(val) || identical(val, "__ALL__") || !nzchar(var)) return(NULL)
      list(var = var, value = val)
    })

    .scope_label <- function() {
      sc <- scope_r()
      y  <- ano_r()
      if (is.null(sc)) return(as.character(y))
      if (.is_tb_recife_bairro()) return(paste0("Recife \u2022 ", .selected_bairro_name(), " \u2022 ", y))
      if (identical(sc$level, "BR")) return(paste0("Brasil \u2022 ", y))
      if (identical(sc$level, "UF")) return(paste0(sc$uf %||% "", " \u2022 ", y))
      paste0(sc$uf %||% "", " \u2022 ", sc$mun %||% sc$mun7 %||% "", " \u2022 ", y)
    }

    .mini_kpi_card <- function(title, value, subtitle) {
      tags$div(
        class = "sinan-mini-kpi",
        tags$div(class = "kpi-title", title),
        tags$div(class = "kpi-value", value),
        tags$div(class = "kpi-sub", subtitle)
      )
    }

    .fmt_int <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) == 0 || !isTRUE(is.finite(v[1]))) return("0")
      formatC(round(v[1]), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
    }

    .comp_fetch_df <- function(var_sel, top_n = 800) {
      sc <- scope_r()
      y  <- ano_r()
      tb_grau <- tb_grau_filter_r()
      bairro_key <- .selected_bairro_key()
      k <- paste0(
        "comp_raw|", sc$level, "|", sc$uf %||% "", "|", sc$mun %||% "", "|bairro=", bairro_key %||% "",
        "|", y, "|", .doenca(), "|v=", var_sel %||% "", "|cap=", top_n,
        "|tb_grau=", if (is.null(tb_grau)) "" else paste0(tb_grau$var, ":", tb_grau$value)
      )
      if (exists(k, envir = cache_env, inherits = FALSE)) {
        return(get(k, envir = cache_env, inherits = FALSE))
      }

      if (.is_tb_recife_bairro()) {
        df_local <- .tb_recife_comp_df(cfg_r(), bairro_key = bairro_key, year = y, var_sel = var_sel, top_n = top_n)
        assign(k, df_local, envir = cache_env)
        return(df_local)
      }

      df <- data.frame()
      api <- .api()
      if (!is.null(api)) {
        df <- .get_composicao(
          api, y, sc,
          vars = if (nzchar(var_sel %||% "")) var_sel else NULL,
          top_n = 600,
          tb_grau_incapacidade = tb_grau
        )
      }
      df <- .safe_df(df)
      if (nrow(df) == 0) {
        df <- .landing_comp(.doenca(), y, sc, var_sel = var_sel, top_n = top_n)
      }
      assign(k, df, envir = cache_env)
      df
    }

    if (is.null(data_api)) data_api <- data

    .api_raw <- reactive({
      a <- .res(data_api); if (!is.null(a)) return(a)
      a <- .res(data); if (!is.null(a)) return(a)
      a <- .res(.obj_get(state, "data_api", NULL)); if (!is.null(a)) return(a)
      a <- .res(.obj_get(state, "api", NULL)); if (!is.null(a)) return(a)
      a <- .res(.obj_get(state, "data", NULL)); if (!is.null(a)) return(a)
      a <- .res(.obj_get(nav, "data_api", NULL)); if (!is.null(a)) return(a)
      a <- .res(.obj_get(pack_r(), "data_api", NULL)); if (!is.null(a)) return(a)
      a <- .res(.obj_get(cfg_r(), "data_api", NULL)); if (!is.null(a)) return(a)
      a <- tryCatch(getOption("sinan.data_api", NULL), error = function(e) NULL)
      .res(a)
    })

    .api_set_doenca <- function(api, d) {
      if (is.null(api) || is.null(d) || !nzchar(d)) return(invisible(NULL))
      f <- .api_get_fun(api, "set_doenca") %||% .api_get_fun(api, "set_disease") %||% .api_get_fun(api, "set_context")
      if (is.function(f)) {
        tryCatch(.call_with_accepted_args(f, list(doenca = d, disease = d, name = d)), error = function(e) NULL)
      }
      invisible(NULL)
    }

    observeEvent(.doenca(), {
      tryCatch(rm(list = ls(envir = cache_env, all.names = TRUE), envir = cache_env), error = function(e) NULL)
      rank_map_r(NULL)
      .api_set_doenca(.api_raw(), .doenca())
    }, ignoreInit = FALSE)

    output$debug_state <- renderPrint({
      if (!isTRUE(input$debug_on)) return(invisible(NULL))

      safe_chr1 <- function(x, fallback = "<NULL>") {
        x <- .res(x)
        if (is.null(x) || length(x) == 0) return(fallback)
        x1 <- as.character(x[[1]])
        x1 <- trimws(x1)
        if (is.na(x1) || !nzchar(x1)) return(fallback)
        x1
      }

      safe_chrvec <- function(x, fallback = character(0)) {
        x <- .res(x)
        if (is.null(x) || length(x) == 0) return(fallback)
        v <- as.character(x)
        v <- v[!is.na(v)]
        v <- trimws(v)
        v <- v[nzchar(v)]
        v
      }

      safe_name_vec <- function(x, fallback = "unknown") {
        v <- safe_chrvec(x, fallback = character(0))
        if (!length(v)) return(fallback)
        v
      }


      tryCatch({
        api <- .api()
        sc  <- scope_r()
        y   <- ano_r()
        m   <- metric_r()
        d   <- .doenca()

        cat("=== mod_charts DEBUG ===\n")
        cat("time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
        cat("doenca:", safe_chr1(d), "\n")
        cat("ano:", safe_chr1(y), "\n")
        cat("metric:", safe_chr1(m), "label:", safe_chr1(metric_label_r()), "\n")

        lvl <- safe_chr1(sc$level)
        uf  <- safe_chr1(sc$uf)
        mun <- safe_chr1(sc$mun)

        cat("scope:", paste0(lvl, " uf=", uf, " mun=", mun), "\n")
        cat("accent:", safe_chr1(accent_color_r()), "\n")

        dash_dir <- tryCatch(.find_dashboard_dir(), error = function(e) NULL)
        cat("dashboard_dir:", safe_chr1(dash_dir), "\n")

        nm <- tryCatch(names(as.list(api)), error = function(e) character(0))
        nm <- safe_name_vec(nm, "api_fn")
        cat("api functions (head):\n")
        print(utils::head(nm, 40))

        has_inc  <- !is.null(tryCatch(.open_incidence_ds(), error = function(e) NULL))
        has_land <- !is.null(tryCatch(.open_sinan_landing_ds(), error = function(e) NULL))
        has_pyr  <- !is.null(tryCatch(.open_piramides_ds(), error = function(e) NULL))
        cat("datasets: incidence=", has_inc, " landing=", has_land, " piramides=", has_pyr, "\n\n")


        cat("\n-- quick probes --\n")

        ya <- tryCatch(.get_years_available(api, sc), error = function(e) integer(0))
        cat("years_available:", if (length(ya)) paste(utils::head(ya, 20), collapse = ", ") else "<none>", "\n")

        yrs <- if (length(ya)) intersect(seq.int(max(ya) - 3L, max(ya)), ya) else integer(0)
        ts  <- tryCatch(.get_time_series(api, yrs, sc, metric_key = m), error = function(e) data.frame())
        cat("ts rows:", nrow(ts), " cols:", paste(names(ts), collapse = ", "), "\n")

      }, error = function(e) {
        cat("DEBUG_STATE ERROR:\n")
        cat(conditionMessage(e), "\n")
      })
    })

 
     observeEvent(input$debug_dump, {
       if (!isTRUE(input$debug_on)) return()
       ctx <- list(
         doenca = .doenca(),
         ano = ano_r(),
         metric = metric_r(),
         metric_label = metric_label_r(),
         scope = scope_r(),
         accent = accent_color_r(),
         dashboard_dir = tryCatch(.find_dashboard_dir(), error = function(e) NULL),
         api_names = tryCatch(names(as.list(.api())), error = function(e) NULL)
       )
       f <- .write_rrmd_log(kind = "manual", title = "mod_charts debug dump", details = ctx)
       last_dump_r(f)
     }, ignoreInit = TRUE)

    .has_any_fun <- function(api, nms) {
      for (nm in nms) if (!is.null(.api_get_fun(api, nm))) return(TRUE)
      FALSE
    }

    .api <- reactive({
      a <- .api_raw()
      if (!is.null(a) && .has_any_fun(a, c(
        "get_years_available", "get_time_series", "get_scope",
        "get_uf_metrics", "get_mun_metrics", "get_piramide", "get_composicao"
      ))) {
        .api_set_doenca(a, .doenca())
        return(a)
      }
      a
    })

    observeEvent(list(cfg_r(), pack_r()), {
      dash_dir <- .as_chr1(.obj_get(cfg_r(), "DASHBOARD_DIR", NULL), NULL) %||%
        .as_chr1(.obj_get(pack_r(), "DASHBOARD_DIR", NULL), NULL)
      if (!is.null(dash_dir) && nzchar(dash_dir) && dir.exists(dash_dir)) {
        tryCatch(options(sinan.dashboard_dir = dash_dir), error = function(e) NULL)
      }
    }, ignoreInit = TRUE)

    level_r <- reactive(.as_chr1(.obj_get(nav, "level", NULL), NULL) %||% .as_chr1(.obj_get(state, "level", NULL), NULL) %||% "UF")
    uf_r    <- reactive(.as_chr1(.obj_get(nav, "uf", NULL), NULL) %||% .as_chr1(.obj_get(state, "uf", NULL), NULL) %||% "PE")
    mun_r   <- reactive(
      .as_chr1(.obj_get(nav, "mun", NULL), NULL) %||%
      .as_chr1(.obj_get(nav, "MUN", NULL), NULL) %||%
      .as_chr1(.obj_get(nav, "cod_mun7", NULL), NULL) %||%
      .as_chr1(.obj_get(nav, "geo_id", NULL), NULL) %||%
      .as_chr1(.obj_get(nav, "cd_mun", NULL), NULL) %||%
      .as_chr1(.obj_get(nav, "cd_ibge", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "mun", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "MUN", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "cod_mun7", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "geo_id", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "cd_mun", NULL), NULL) %||%
      .as_chr1(.obj_get(state, "cd_ibge", NULL), NULL)
    )

    ano_r <- reactive({
      y <- if (is.function(ano)) ano() else ano
      y <- .as_int1(y, NA_integer_)
      if (!is.finite(y)) .as_int1(.obj_get(cfg_r(), "YEAR_MAX", 2025L), 2025L) else y
    })

    metric_r <- reactive({
      m <- if (is.function(metric)) metric() else metric
      m <- as.character(m %||% .obj_get(state, "metric", NULL) %||% .obj_get(nav, "metric", NULL) %||% "incid")
      m <- tolower(trimws(m))
      if (!m %in% c("incid", "cases", "obitos", "cura")) m <- "incid"
      m
    })

    metric_label_r <- reactive({
      lbl <- .as_chr1(.obj_get(state, "metric_label", NULL), NULL) %||%
        .as_chr1(.obj_get(state, "kpi_label", NULL), NULL) %||%
        .as_chr1(.obj_get(nav, "metric_label", NULL), NULL)

      if (!is.null(lbl) && nzchar(lbl)) return(lbl)

      labs <- .obj_get(pack_r(), "KPI_LABELS", list()) %||% list()
      if (metric_r() %in% names(labs)) {
        v <- .as_chr1(labs[[metric_r()]], NULL)
        if (!is.null(v) && nzchar(v)) return(v)
      }

      switch(
        metric_r(),
        "cases"  = "Casos novos",
        "incid"  = "Incidência",
        "obitos" = "Óbitos",
        "cura"   = "Curas",
        "Casos"
      )
    })

    scope_r <- reactive(.normalize_scope(level_r(), uf_r(), mun_r()))

    accent_color_r <- reactive({
      c0 <- .as_chr1(.obj_get(state, "kpi_color", NULL), NULL) %||%
        .as_chr1(.obj_get(state, "metric_color", NULL), NULL) %||%
        .as_chr1(.obj_get(nav, "kpi_color", NULL), NULL) %||%
        .as_chr1(.obj_get(cfg_r(), "ACCENT_COLOR", NULL), NULL)

      if (!is.null(c0) && nzchar(c0)) return(c0)

      switch(
        metric_r(),
        "incid"  = "#0B8A8F",
        "cases"  = "#0B8A8F",
        "obitos" = "#B91C1C",
        "cura"   = "#16A34A",
        "#0B8A8F"
      )
    })

    observeEvent(accent_color_r(), {
      tryCatch(
        session$sendCustomMessage(
          "sinan_set_accent",
          list(id = session$ns("charts_root"), color = accent_color_r())
        ),
        error = function(e) NULL
      )
    }, ignoreInit = FALSE)

    .get_years_available <- function(api, sc) {
      args <- list(level = sc$level, nivel = sc$level, uf = sc$uf, mun = sc$mun, doenca = .doenca())
      yy <- .api_call_any(api, c("get_years_available", "years_available", "get_years"), args)
      yy <- suppressWarnings(as.integer(unlist(yy)))
      yy <- yy[is.finite(yy)]
      sort(unique(yy))
    }

    .get_time_series <- function(api, yrs, sc, metric_key) {
      base_args <- list(
        ano = yrs, anos = yrs, years = yrs,
        level = sc$level, nivel = sc$level,
        uf = sc$uf,
        mun = sc$mun,
        cod_mun7 = sc$mun7,
        cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = .doenca()
      )

      ts <- .api_call_any(
        api,
        c("get_time_series", "get_timeseries", "get_ts", "get_series", "series_temporal", "time_series"),
        base_args
      )
      ts <- .safe_df(ts)
      ts <- .df_filter_context(ts, sc = sc, doenca = .doenca(), tipo = NULL)
      if (nrow(ts) > 0) return(ts)

      metric_variants <- switch(
        tolower(metric_key %||% ""),
        "obitos" = c("obitos", "deaths", "CASOS_OBITOS", "OBITOS"),
        "cura"   = c("cura", "curas", "CASOS_CURA", "CURA"),
        "incid"  = c("incid", "incidencia", "INCID_100K"),
        c("cases", "casos", "CASOS")
      )

      for (mv in metric_variants) {
        args2 <- c(base_args, list(metric = mv, indicador = mv, variavel = mv))
        ts2 <- .api_call_any(
          api,
          c("get_time_series", "get_timeseries", "get_ts", "get_series", "series_temporal", "time_series"),
          args2
        )
        ts2 <- .safe_df(ts2)
        ts2 <- .df_filter_context(ts2, sc = sc, doenca = .doenca(), tipo = NULL)
        if (nrow(ts2) > 0) return(ts2)
      }

      data.frame()
    }

    .get_metrics_0_14 <- function(api, y, sc) {
      args <- list(
        ano = y, anos = y, years = y,
        level = sc$level, nivel = sc$level,
        uf = sc$uf, mun = sc$mun,
        cod_mun7 = sc$mun7, cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = .doenca()
      )

      df <- .api_call_any(api, c("get_metrics_0_14", "get_0_14_metrics", "metrics_0_14"), args)
      df <- .safe_df(df)
      if (nrow(df) == 0) return(data.frame())

      df <- .df_filter_context(df, sc = sc, doenca = .doenca())
      if (nrow(df) == 0) return(data.frame())

      c_year <- .col_pick_norm(names(df), c("ano", "ANO", "year", "YEAR"))
      if (!is.null(c_year)) {
        yr_want <- suppressWarnings(as.integer(y))
        yr_got  <- suppressWarnings(as.integer(df[[c_year]]))
        df <- df[is.finite(yr_got) & yr_got == yr_want, , drop = FALSE]
      }

      df
    }

    .get_scope <- function(api, y, sc) {
      args <- list(
        ano = y, year = y,
        level = sc$level, nivel = sc$level,
        uf = sc$uf,
        mun = sc$mun,
        cod_mun7 = sc$mun7,
        cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = .doenca()
      )
      d <- .api_call_any(api, c("get_scope", "scope", "get_summary", "get_kpis", "summary_scope"), args)
      d <- .safe_df(d)
      .df_filter_context(d, sc = sc, doenca = .doenca(), tipo = NULL)
    }

    .landing_tbl <- function(doenca, y, sc, var_sel = NULL) {
      ds <- .open_sinan_landing_ds()
      if (is.null(ds)) return(NULL)

      c_doenca <- .ds_pick(ds, c("doenca", "DOENCA"))
      c_ano    <- .ds_pick(ds, c("ano", "ANO"))
      c_nivel  <- .ds_pick(ds, c("nivel", "NIVEL"))
      c_geo    <- .ds_pick(ds, c("geo_id", "GEO_ID"))
      c_uf     <- .ds_pick(ds, c("uf", "UF"))
      c_var    <- .ds_pick(ds, c("variavel", "VARIAVEL"))

      tbl <- ds

      if (!is.null(c_doenca) && !is.null(doenca) && nzchar(doenca)) {
        avail <- tryCatch({
          tbl %>% select(!!sym(c_doenca)) %>% distinct() %>% head(200) %>% collect() %>% pull(1)
        }, error = function(e) character(0))
        d_match <- .best_match(doenca, avail) %||% doenca
        tbl <- tryCatch(tbl %>% filter(!!sym(c_doenca) == d_match), error = function(e) tbl)
      }

      if (!is.null(c_ano)) {
        tbl <- tryCatch(tbl %>% filter(!!sym(c_ano) == as.integer(y)), error = function(e) tbl)
      }

      if (!is.null(c_nivel) && !is.null(sc$level) && nzchar(sc$level)) {
        lvl_vals <- .scope_level_candidates(sc$level)
        tbl <- tryCatch(tbl %>% filter(!!sym(c_nivel) %in% lvl_vals), error = function(e) tbl)
      }

      tbl <- .tbl_filter_scope_arrow(tbl, sc, col_geo = c_geo, col_uf = c_uf)

      if (!is.null(var_sel) && nzchar(var_sel) && !is.null(c_var)) {
        availv <- tryCatch({
          tbl %>% select(!!sym(c_var)) %>% distinct() %>% head(500) %>% collect() %>% pull(1)
        }, error = function(e) character(0))
        v_match <- .best_match(toupper(var_sel), availv) %||% toupper(var_sel)
        tbl <- tryCatch(tbl %>% filter(!!sym(c_var) == v_match), error = function(e) tbl)
      }

      tbl
    }

    .landing_vars_available <- function(doenca, y, sc) {
      tbl <- .landing_tbl(doenca, y, sc, var_sel = NULL)
      if (is.null(tbl)) return(character(0))

      ds <- .open_sinan_landing_ds()
      c_var <- .ds_pick(ds, c("variavel", "VARIAVEL"))
      if (is.null(c_var)) return(character(0))

      vv <- tryCatch({
        tbl %>% select(!!sym(c_var)) %>% distinct() %>% collect() %>% pull(1)
      }, error = function(e) character(0))

      vv <- toupper(trimws(as.character(vv)))
      vv <- vv[!is.na(vv) & nzchar(vv)]
      sort(unique(vv))
    }

    .landing_comp <- function(doenca, y, sc, var_sel = NULL, top_n = 400) {
      tbl <- .landing_tbl(doenca, y, sc, var_sel = var_sel)
      if (is.null(tbl)) return(data.frame())

      ds <- .open_sinan_landing_ds()
      c_var <- .ds_pick(ds, c("variavel", "VARIAVEL"))
      c_lbl <- .ds_pick(ds, c("valor_lbl", "VALOR_LBL"))
      c_n   <- .ds_pick(ds, c("n", "N"))

      if (is.null(c_var) || is.null(c_lbl) || is.null(c_n)) return(data.frame())

      df <- tryCatch({
        tbl %>% select(!!sym(c_var), !!sym(c_lbl), !!sym(c_n)) %>% collect()
      }, error = function(e) data.frame())

      df <- .safe_df(df)
      if (nrow(df) == 0) return(df)

      names(df)[names(df) == c_lbl] <- "categoria"
      names(df)[names(df) == c_n] <- "total"
      names(df)[names(df) == c_var] <- "variavel"

      df$variavel <- toupper(trimws(as.character(df$variavel)))
      df$categoria <- as.character(df$categoria)
      df$total <- suppressWarnings(as.numeric(df$total))
      df <- df[is.finite(df$total) & nzchar(df$variavel) & nzchar(df$categoria), , drop = FALSE]

      if (!is.null(var_sel) && nzchar(var_sel)) {
        df <- df %>% filter(.data$variavel == toupper(var_sel))
      }

      if (nrow(df) > top_n) {
        df <- df %>%
          group_by(.data$variavel, .data$categoria) %>%
          summarise(total = sum(.data$total, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(.data$total)) %>%
          slice_head(n = top_n)
      }
      df
    }

    .pyr_local_read <- function(doenca = NULL, tipo = NULL, y, sc) {
      ds <- .open_piramides_ds()
      if (is.null(ds)) return(data.frame())

      c_doenca <- .ds_pick(ds, c("doenca", "DOENCA"))
      c_tipo   <- .ds_pick(ds, c("tipo", "TIPO"))
      c_ano    <- .ds_pick(ds, c("ano", "ANO"))
      c_nivel  <- .ds_pick(ds, c("nivel", "NIVEL"))
      c_geo    <- .ds_pick(ds, c("geo_id", "GEO_ID"))
      c_uf     <- .ds_pick(ds, c("uf", "UF"))

      tbl <- ds
      if (!is.null(c_ano))   tbl <- tryCatch(tbl %>% filter(!!sym(c_ano) == as.integer(y)), error = function(e) tbl)
      if (!is.null(c_nivel)) {
        lvl_vals <- .scope_level_candidates(sc$level)
        tbl <- tryCatch(tbl %>% filter(!!sym(c_nivel) %in% lvl_vals), error = function(e) tbl)
      }

      if (!is.null(c_tipo) && !is.null(tipo) && nzchar(tipo)) {
        tbl <- tryCatch(tbl %>% filter(!!sym(c_tipo) == toupper(tipo)), error = function(e) tbl)
      }

      if (!is.null(c_doenca) && !is.null(doenca) && nzchar(doenca)) {
        tbl <- tryCatch(tbl %>% filter(toupper(!!sym(c_doenca)) == toupper(doenca)), error = function(e) tbl)
      }

      tbl <- .tbl_filter_scope_arrow(tbl, sc, col_geo = c_geo, col_uf = c_uf)

      tryCatch(collect(tbl), error = function(e) data.frame())
    }


    .rank_from_api <- function(api, y, sc, kind = "cases", mode = c("ufs","mun")) {
      mode <- match.arg(mode)
      if (is.null(api)) return(data.frame())

      .as_rank_ready <- function(df) {
        df <- .safe_df(df)
        if (!nrow(df)) return(df)
        nms <- names(df)
        if (all(c("cat","key","val") %in% nms)) {
          df$cat <- as.character(df$cat)
          df$key <- as.character(df$key)
          df$val <- suppressWarnings(as.numeric(df$val))
          if (!"uf" %in% nms) df$uf <- NA_character_
          return(df[is.finite(df$val) & nzchar(df$cat) & nzchar(df$key), , drop=FALSE])
        }
        df
      }

      df <- .api_call_any(
        api,
        c("get_ranking", "ranking", "get_rank", "get_rankings"),
        list(
          ano = y, year = y,
          doenca = .doenca(),
          level = if (mode == "ufs") "UF" else "MUN",
          nivel = if (mode == "ufs") "UF" else "MUN",
          mode  = mode
        )
      )
      df <- .as_rank_ready(df)
      df <- .safe_df(df)
      if (nrow(df)) return(df)

      if (identical(sc$level, "BR") && identical(mode, "ufs")) {
        df <- .api_call_any(
          api,
          c("get_uf_metrics","uf_metrics","metrics_uf","get_rank_uf","ranking_uf","get_ranking_ufs"),
          list(ano=y, year=y, doenca=.doenca(), level="UF", nivel="UF")
        )
        df <- .as_rank_ready(df)
        df <- .safe_df(df); if (!nrow(df)) return(df)

        cols <- names(df)
        val_col <- .rank_value_col(cols, kind) %||% .pick(cols, c("valor","VALOR","total","TOTAL"))
        if (is.null(val_col)) return(data.frame())

        c_uf <- .pick(cols, c("uf","UF","sg_uf","SG_UF","geo_id","GEO_ID","cat","CAT"))
        if (is.null(c_uf)) return(data.frame())

        out <- df %>% transmute(
          cat = as.character(.data[[c_uf]]),
          key = as.character(.data[[c_uf]]),
          val = suppressWarnings(as.numeric(.data[[val_col]]))
        )
        return(out[is.finite(out$val) & nzchar(out$cat), , drop=FALSE])
      }

      args_list <- list(
        list(ano=y, year=y, doenca=.doenca(), nivel="MUN", level="MUN"),
        list(ano=y, year=y, doenca=.doenca(), nivel="MUNICIPIO", level="MUNICIPIO"),
        list(ano=y, year=y, doenca=.doenca(), nivel="MUN", level="BR"),
        list(ano=y, year=y, doenca=.doenca(), uf = sc$uf %||% NULL, nivel="MUN", level="MUN"),
        list(ano=y, year=y, doenca=.doenca(), uf = sc$uf %||% NULL),
        list(ano=y, year=y, doenca=.doenca(), level="MUN")
      )

      df <- data.frame()
      for (aa in args_list) {
        df <- .api_call_any(
          api,
          c("get_mun_metrics","mun_metrics","metrics_mun","get_rank_mun","ranking_mun","ranking_municipios","get_ranking_municipios"),
          aa
        ) %||% data.frame()
        df <- .as_rank_ready(df)
        df <- .safe_df(df)
        if (nrow(df)) break
      }
      if (!nrow(df)) return(df)

      cols <- names(df)
      val_col <- .rank_value_col(cols, kind) %||% .pick(cols, c("valor","VALOR","total","TOTAL"))
      if (is.null(val_col)) return(data.frame())

      c_nome <- .pick(cols, c("geo_nome","GEO_NOME","nome_mun","NOME_MUN","municipio","MUNICIPIO","nome","NOME","cat","CAT"))
      c_key  <- .pick(cols, c("geo_id","GEO_ID","cod_mun7","COD_MUN7","cod_mun6","COD_MUN6","cd_mun","CD_MUN","mun","MUN","key","KEY"))
      c_uf   <- .pick(cols, c("uf","UF","sg_uf","SG_UF"))

      if (is.null(c_nome) || is.null(c_key)) return(data.frame())

      out <- df %>% transmute(
        uf  = if (!is.null(c_uf)) as.character(.data[[c_uf]]) else NA_character_,
        cat = as.character(.data[[c_nome]]),
        key = as.character(.data[[c_key]]),
        val = suppressWarnings(as.numeric(.data[[val_col]]))
      )
      out[is.finite(out$val) & nzchar(out$cat) & nzchar(out$key), , drop=FALSE]
    }

    .get_composicao <- function(api, y, sc, vars = NULL, top_n = 200, tb_grau_incapacidade = NULL) {
      args <- list(
        ano = y,
        level = sc$level, nivel = sc$level,
        uf = sc$uf,
        mun = sc$mun,
        cod_mun7 = sc$mun7,
        cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = .doenca(),
        vars = vars,
        top_n = top_n,
        tb_grau_incapacidade = tb_grau_incapacidade,
        grau_incapacidade = tb_grau_incapacidade
      )
      d <- .api_call_any(api, c("get_composicao", "get_profile", "get_sociodemografico", "get_composition"), args)
      d <- .safe_df(d)
      d <- .df_filter_context(d, sc = sc, doenca = .doenca(), tipo = NULL)
      if (nrow(d) == 0) {
        var1 <- NULL
        if (is.character(vars) && length(vars) >= 1) var1 <- vars[[1]]
        d <- .landing_comp(.doenca(), y, sc, var_sel = var1, top_n = max(400L, as.integer(top_n %||% 400L)))
      }
      d
    }

    .get_composicao_vars_available <- function(api, y, sc) {
      args <- list(
        ano = y,
        level = sc$level, nivel = sc$level,
        uf = sc$uf,
        mun = sc$mun,
        cod_mun7 = sc$mun7,
        cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = .doenca()
      )
      vv <- .api_call_any(api, c("get_composicao_vars_available", "get_vars_available", "vars_available", "composicao_vars_available"), args)
      vv <- as.character(unlist(vv))
      vv <- vv[!is.na(vv) & nzchar(vv)]
      vv <- sort(unique(vv))
      if (length(vv) == 0) vv <- .landing_vars_available(.doenca(), y, sc)
      vv
    }

    .get_piramide_broad <- function(api, y, sc, doenca, tipo = NULL) {

      args0 <- list(
        ano = y,
        level = sc$level, nivel = sc$level,
        uf = sc$uf,
        mun = sc$mun,
        cod_mun7 = sc$mun7,
        cod_mun6 = sc$mun6,
        geo_id = sc$mun %||% sc$mun6 %||% sc$mun7,
        doenca = doenca
      )

      d_all <- .api_call_any(api, c("get_piramide", "get_pyramid", "piramide", "pyramid"), args0)
      d_all <- .safe_df(d_all)
      d_all <- .df_filter_context(d_all, sc = sc, doenca = doenca, tipo = NULL)

      if (nrow(d_all) > 0 && !is.null(tipo) && nzchar(tipo)) {
        c_tipo <- .pick(names(d_all), c("tipo","TIPO"))
        if (!is.null(c_tipo)) {
          want <- toupper(tipo)
          got  <- toupper(trimws(as.character(d_all[[c_tipo]])))
          d1 <- d_all[got == want, , drop=FALSE]
          if (nrow(d1) > 0) return(d1)

          if (want == "POP") {
            d2 <- d_all[got %in% c("POP","POPULACAO","POPULAÇÃO","POP_TOTAL","POPULACAO_TOTAL","POPULAÇÃO_TOTAL"), , drop=FALSE]
            if (nrow(d2) > 0) return(d2)
          }
        }
      }
      if (nrow(d_all) > 0 && (is.null(tipo) || !nzchar(tipo))) return(d_all)

      args1 <- c(args0, list(tipo = tipo))
      d <- .api_call_any(api, c("get_piramide", "get_pyramid", "piramide", "pyramid"), args1)
      d <- .safe_df(d)
      .df_filter_context(d, sc = sc, doenca = doenca, tipo = tipo)
    }

    .set_nav <- function(level = NULL, uf = NULL, mun = NULL) {
      lvl <- if (!is.null(level)) toupper(as.character(level)) else NULL

      if (.is_rv(state)) {
        if (!is.null(lvl)) state$level <- lvl
        if (!is.null(uf)) state$uf <- uf
        if (!is.null(mun)) state$mun <- mun
        if (!is.null(lvl) && lvl == "BR") { state$uf <- NULL; state$mun <- NULL }
        if (!is.null(lvl) && lvl == "UF") { state$mun <- NULL }
      }

      if (.is_rv(nav)) {
        if (!is.null(lvl)) nav$level <- lvl
        if (!is.null(uf)) nav$uf <- uf
        if (!is.null(mun)) nav$mun <- mun
        if (!is.null(lvl) && lvl == "BR") { nav$uf <- NULL; nav$mun <- NULL }
        if (!is.null(lvl) && lvl == "UF") { nav$mun <- NULL }
      }
    }

    rank_mode_r <- reactive({
      v <- .as_chr1(input$rank_mode, "ufs")
      if (!v %in% c("ufs", "mun")) v <- "ufs"
      v
    })

    output$rank_controls_ui <- renderUI({
      sc <- scope_r()
      if (!identical(sc$level, "BR")) return(NULL)

      ns <- session$ns
      
      # Determina se pode mostrar municípios (apenas se não estiver em nível BR)
      sc <- scope_r()
      can_show_mun <- !identical(sc$level, "BR")
      
      div(
        class = "form-group",
        tags$label(tags$strong("Nível do ranking")),
        div(
          class = "sinan-seg",
          tags$input(id = ns("rank_mode_ufs"), type = "radio", name = ns("rank_mode"), value = "ufs", checked = "checked"),
          tags$label(`for` = ns("rank_mode_ufs"), "UFs"),
          # Municípios: desabilitado se estiver em nível BR
          if (can_show_mun) {
            list(
              tags$input(id = ns("rank_mode_mun"), type = "radio", name = ns("rank_mode"), value = "mun"),
              tags$label(`for` = ns("rank_mode_mun"), "Municípios")
            )
          } else {
            list(
              tags$input(id = ns("rank_mode_mun"), type = "radio", name = ns("rank_mode"), value = "mun", disabled = "disabled"),
              tags$label(`for` = ns("rank_mode_mun"), tags$span("Municípios", style = "color: #999; cursor: not-allowed;"), title = "Selecione uma UF para ver municípios")
            )
          }
        ),
        tags$input(id = ns("rank_mode"), type = "hidden", value = "ufs"),
        tags$script(HTML(sprintf("
          (function(){
            var hid = document.getElementById('%s');
            var r1  = document.getElementById('%s');
            var r2  = document.getElementById('%s');
            function sync(){
              try{
                var v = (r2 && r2.checked && !r2.disabled) ? 'mun' : 'ufs';
                hid.value = v;
                if(window.Shiny && Shiny.setInputValue){
                  Shiny.setInputValue('%s', v, {priority:'event'});
                }
              }catch(e){}
            }
            if(r1) r1.addEventListener('change', sync);
            if(r2) r2.addEventListener('change', sync);
            setTimeout(sync, 0);
          })();
        ", ns("rank_mode"), ns("rank_mode_ufs"), ns("rank_mode_mun"), ns("rank_mode"))))
      )
    })

    observeEvent(input$rank_click, {
      cl <- input$rank_click
      if (is.null(cl)) return()
      sc <- scope_r()
      idx <- suppressWarnings(as.integer(cl$idx))
      if (!is.finite(idx)) return()

      mp <- rank_map_r()
      if (!is.data.frame(mp) || nrow(mp) == 0 || !all(c("key","cat") %in% names(mp))) return()
      j <- idx + 1L
      if (j < 1 || j > nrow(mp)) return()

      ky_raw <- as.character(mp$key[[j]] %||% "")
      ky <- .norm_mun_digits(ky_raw) %||% ky_raw
      if (!nzchar(ky)) return()

      if (identical(sc$level, "BR")) {
        if (identical(rank_mode_r(), "mun")) {
          uf_from <- .norm_uf(mp$uf[[j]] %||% "")
          if (!is.null(uf_from) && nzchar(uf_from)) {
            .set_nav("MUN", uf = uf_from, mun = ky)
          }
        } else {
          .set_nav("UF", uf = .norm_uf(ky), mun = NULL)
        }
      } else if (sc$level %in% c("UF", "MUN")) {
        .set_nav("MUN", uf = sc$uf, mun = ky)
      }
    }, ignoreInit = TRUE)

    evol_horizon_r <- reactive({
      v <- .as_chr1(input$evol_horizon, "months")
      if (!v %in% c("months", "years")) v <- "months"
      v
    })

    .ts_attach_forecast <- function(prepared, fc) {
      prepared <- .safe_df(prepared)
      if (nrow(prepared) == 0 || is.null(fc)) return(prepared)

      prepared$forecast <- NA_real_
      prepared$forecast_lower <- NA_real_
      prepared$forecast_upper <- NA_real_

      add <- data.frame(
        x = as.character(fc$years),
        valor_ano = NA_real_,
        media_prev3 = NA_real_,
        forecast = as.numeric(fc$forecast),
        forecast_lower = as.numeric(fc$lower),
        forecast_upper = as.numeric(fc$upper),
        stringsAsFactors = FALSE
      )

      out <- rbind(prepared, add)
      attr(out, "x_title") <- attr(prepared, "x_title")
      attr(out, "horizon") <- attr(prepared, "horizon")
      out
    }

    .linear_forecast_years <- function(prepared, horizon = 15L, allow_negative = FALSE) {
      df <- .safe_df(prepared)
      if (!"x" %in% names(df) || !"valor_ano" %in% names(df)) return(NULL)

      years <- suppressWarnings(as.integer(df$x))
      values <- suppressWarnings(as.numeric(df$valor_ano))
      ok <- is.finite(years) & is.finite(values)
      years <- years[ok]
      values <- values[ok]

      if (length(values) < 2) return(NULL)

      last_year <- max(years, na.rm = TRUE)

      result <- tryCatch({
        fit <- stats::lm(values ~ years)
        coefs <- stats::coef(fit)
        slope <- as.numeric(coefs[2])
        intercept <- as.numeric(coefs[1])

        new_years <- seq.int(last_year + 1L, last_year + horizon)
        preds <- slope * new_years + intercept
        if (!allow_negative) preds <- pmax(0, preds)

        resid <- stats::residuals(fit)
        sigma <- stats::sd(resid, na.rm = TRUE)
        if (!is.finite(sigma) || sigma <= 0) sigma <- stats::sd(values, na.rm = TRUE) * 0.1

        list(
          years = new_years,
          forecast = preds,
          lower = if (!allow_negative) pmax(0, preds - 1.96 * sigma) else preds - 1.96 * sigma,
          upper = preds + 1.96 * sigma
        )
      }, error = function(e) NULL)

      result
    }

    .lstm_forecast_years <- function(prepared, horizon = 15L, seed = 123L, allow_negative = FALSE) {
      if (!requireNamespace("keras", quietly = TRUE)) return(NULL)

      df <- .safe_df(prepared)
      if (!"x" %in% names(df) || !"valor_ano" %in% names(df)) return(NULL)

      years <- suppressWarnings(as.integer(df$x))
      values <- suppressWarnings(as.numeric(df$valor_ano))
      ok <- is.finite(years) & is.finite(values)
      years <- years[ok]
      values <- values[ok]

      if (length(values) < 6) return(NULL)

      last_year <- max(years, na.rm = TRUE)
      window <- min(5L, length(values) - 1L)

      mu <- mean(values, na.rm = TRUE)
      sdv <- stats::sd(values, na.rm = TRUE)
      if (!is.finite(sdv) || sdv <= 0) {
        pred <- rep(tail(values, 1), horizon)
        lower <- if (allow_negative) pred else pmax(0, pred)
        upper <- if (allow_negative) pred else pmax(0, pred)
        return(list(
          years = seq.int(last_year + 1L, last_year + horizon),
          forecast = pred,
          lower = lower,
          upper = upper
        ))
      }

      scaled <- (values - mu) / sdv
      n <- length(scaled)
      if (n <= window) return(NULL)

      X <- array(0, dim = c(n - window, window, 1))
      Y <- array(0, dim = c(n - window, 1))
      for (i in seq_len(n - window)) {
        X[i, , 1] <- scaled[i:(i + window - 1L)]
        Y[i, 1] <- scaled[i + window]
      }

      set.seed(as.integer(seed))
      tryCatch(keras::k_clear_session(), error = function(e) NULL)

      model <- keras::keras_model_sequential() |>
        keras::layer_lstm(units = 32, input_shape = c(window, 1)) |>
        keras::layer_dense(units = 1)

      model |> keras::compile(optimizer = "adam", loss = "mse")

      model |> keras::fit(
        x = X,
        y = Y,
        epochs = 40,
        batch_size = min(16L, n - window),
        verbose = 0
      )

      train_pred <- as.numeric(model |> keras::predict(X, verbose = 0))
      train_pred <- (train_pred * sdv) + mu
      actual_tail <- values[(window + 1L):n]
      resid <- actual_tail - train_pred
      sigma <- stats::sd(resid, na.rm = TRUE)
      if (!is.finite(sigma)) sigma <- 0

      last_seq <- tail(scaled, window)
      preds <- numeric(horizon)
      for (i in seq_len(horizon)) {
        x_in <- array(last_seq, dim = c(1, window, 1))
        p <- as.numeric(model |> keras::predict(x_in, verbose = 0))
        preds[[i]] <- p
        last_seq <- c(last_seq[-1L], p)
      }

      preds <- (preds * sdv) + mu
      lower <- preds - 1.96 * sigma
      upper <- preds + 1.96 * sigma
      if (!allow_negative) {
        lower <- pmax(0, lower)
        upper <- pmax(0, upper)
      }

      list(
        years = seq.int(last_year + 1L, last_year + horizon),
        forecast = preds,
        lower = lower,
        upper = upper
      )
    }

    .render_ts_metric <- function(metric_key = "cases", horizon = "months") {
      step <- "init"
      ts_last <- NULL
      prepared_last <- NULL
      val_col_last <- NULL
      .with_rrmd_log(
        kind = "timeseries",
        title = "Falha ao renderizar série temporal",
        ctx_builder = function(e) {
          api <- .api()
          sc <- scope_r()
          y <- ano_r()
          list(
            error = conditionMessage(e),
            step = step,
            val_col = val_col_last,
            ts_cols = tryCatch(names(ts_last), error = function(e2) NULL),
            prepared_cols = tryCatch(names(prepared_last), error = function(e2) NULL),
            prepared_preview = tryCatch(.safe_deparse(utils::head(prepared_last, 5)), error = function(e2) NULL),
            doenca = .doenca(),
            scope = sc,
            ano = y,
            metric_key = metric_key,
            horizon = horizon,
            palette = pal(),
            api_has = if (is.null(api)) "<NULL>" else names(as.list(api))
          )
        },
        expr = {
          step <- "get_api"
          api <- .api()
          sc <- scope_r()
          y <- ano_r()
          if (.is_tb_recife_bairro()) {
            by <- if (identical(horizon, "years")) .tb_recife_year_series(cfg_r(), .selected_bairro_key()) else .tb_recife_month_series(cfg_r(), .selected_bairro_key(), seq.int(y - 3L, y))
            by <- .safe_df(by)
            if (nrow(by) == 0) return(.ec_message("Sem dados para o bairro selecionado.", palette = pal_lines()))
            if ("periodo" %in% names(by)) by$x <- by$periodo
            p <- e_charts(by, x) %>%
              e_bar(casos, name = "Casos") %>%
              e_title(paste0("Casos de tuberculose - ", .selected_bairro_name()), left = 8, top = 4, textStyle = list(fontSize = 14, fontWeight = "bold")) %>%
              e_x_axis(type = "category", axisLabel = list(fontSize = 10, rotate = 45)) %>%
              e_y_axis(name = "Casos", min = 0) %>%
              .ec_finalize(palette = pal_lines(), datazoom = nrow(by) > 12, tooltip_trigger = "axis", grid_left = 48, grid_top = 58, grid_bottom = 64, legend_top = NULL, legend_show = FALSE)
            return(.ec_force_resize_on_tab(p))
          }

          if (is.null(api)) return(.ec_message("API de dados indisponível (data_api=NULL).", palette = pal()))

          step <- "get_years"
          ya <- .get_years_available(api, sc)

          if (length(ya) > 0) {
            if (!(y %in% ya)) y <- max(ya)

            if (identical(horizon, "years")) {
              yrs <- ya
            } else {
              yrs <- intersect(seq.int(y - 3L, y), ya)
              if (length(yrs) == 0) yrs <- y
            }
          } else {
            if (identical(horizon, "years")) {
              yrs <- seq.int(max(1970L, y - 30L), y)   
            } else {
              yrs <- seq.int(y - 3L, y)
            }
          }


          step <- "cache_key"
          k <- paste0("ts|", sc$level, "|", sc$uf %||% "", "|", sc$mun %||% "", "|", y, "|", metric_key, "|", horizon, "|", .doenca())
          if (exists(k, envir = cache_env, inherits = FALSE)) {
            prepared <- get(k, envir = cache_env, inherits = FALSE)
            return(.plot_ts(
              prepared,
              palette = pal_lines(),
              history_palette = .ts_history_palette()
            ))
          }

          step <- "get_time_series"
          metric_key_ts <- metric_key
          if (identical(.norm_key(metric_key), "incid")) metric_key_ts <- "cases"
          ts <- .get_time_series(api, yrs, sc, metric_key_ts)
          ts_last <- ts
          if (nrow(ts) == 0) return(.ec_message("Sem dados na série temporal (ano/escopo).", palette = pal_lines()))
          step <- "filter_metric"
          ts <- .ts_filter_long_by_metric(ts, metric_key_ts)
          ts_last <- ts

          step <- "detect_metric_col"
          val_col <- .detect_metric_col(ts, metric_key_ts) %||%
            .pick(names(ts), c("valor", "n", "total", "casos", "casos_obitos", "casos_cura"))
          if (is.null(val_col)) {
            val_col <- .pick(names(ts), c("valor", "VALOR", "value", "VALUE", "n", "N", "total", "TOTAL", "casos", "casos_obitos", "casos_cura"))
          }
          val_col_last <- val_col
          denom_pop <- NULL
          if (identical(metric_key, "incid")) {
            step <- "scope_for_incid"
            scp <- .get_scope(api, y, sc)
            if (is.data.frame(scp) && nrow(scp) > 0) {
              pcol <- .pick(names(scp), c("pop_total", "POP_TOTAL", "pop", "POP", "populacao_total", "POPULACAO_TOTAL"))
              if (!is.null(pcol)) denom_pop <- suppressWarnings(as.numeric(scp[[pcol]][[1]]))
            }
          }

          # (removido bloco de 0-14 anos na série temporal)

          step <- "prepare_ts"
          prepared <- tryCatch(
            .ts_prepare(ts, value_col = val_col, year_focus = y, metric_key = metric_key, denom_pop = denom_pop, horizon = horizon),
            error = function(e) {
              logger <- tryCatch(logger, error = function(e2) NULL)
              if (!is.null(logger)) logger$err("ts_prepare failed", list(err = conditionMessage(e)))
              data.frame()
            }
          )
          prepared_last <- prepared
          if (!is.data.frame(prepared) || nrow(prepared) == 0) {
            assign(k, data.frame(), envir = cache_env)
            return(.ec_message("Série temporal indisponível para este recorte.", palette = pal_lines()))
          }


          # Forecast removido (previsão de 15 anos)

          assign(k, prepared, envir = cache_env)
          step <- "plot_ts"
          .plot_ts(
            prepared,
            palette = pal_lines(),
            history_palette = .ts_history_palette()
          )
        }
      )
    }

    .render_tb_ts_years_combo <- function() {
      .with_rrmd_log(
        kind = "timeseries_tb_combo",
        title = "Falha ao renderizar série temporal TB (casos + incidência)",
        ctx_builder = function(e) {
          list(
            error = conditionMessage(e),
            doenca = .doenca(),
            scope = scope_r(),
            ano = ano_r()
          )
        },
        expr = {
          api <- .api()
          sc  <- scope_r()
          y   <- ano_r()
          if (.is_tb_recife_bairro()) {
            base <- .tb_recife_year_series(cfg_r(), .selected_bairro_key())
            base <- .safe_df(base)
            if (nrow(base) == 0) return(.ec_message("Sem dados anuais para o bairro selecionado.", palette = pal_lines()))
            p <- e_charts(base, x) %>%
              e_bar(casos, name = "Número de casos novos") %>%
              e_title(paste0("Casos novos de tuberculose por ano - ", .selected_bairro_name()), left = 8, top = 4, textStyle = list(fontSize = 14, fontWeight = "bold")) %>%
              e_x_axis(name = "Ano", type = "category", axisLabel = list(fontSize = 11, rotate = 45)) %>%
              e_y_axis(name = "Casos novos", min = 0) %>%
              .ec_finalize(palette = pal_bars(), datazoom = nrow(base) > 12, tooltip_trigger = "axis", grid_left = 52, grid_right = 28, grid_top = 60, grid_bottom = 66, legend_top = NULL, legend_show = FALSE)
            return(.ec_force_resize_on_tab(p))
          }

          if (is.null(api)) return(.ec_message("API de dados indisponível (data_api=NULL).", palette = pal_lines()))

          ya <- .get_years_available(api, sc)
          yrs <- if (length(ya) > 0) sort(unique(ya)) else seq.int(max(1970L, y - 30L), y)
          if (length(yrs) == 0) return(.ec_message("Sem anos disponíveis para a série temporal.", palette = pal_lines()))

          k <- paste0("ts_tb_combo|", sc$level, "|", sc$uf %||% "", "|", sc$mun %||% "", "|", min(yrs), "-", max(yrs), "|", .doenca())
          if (exists(k, envir = cache_env, inherits = FALSE)) {
            return(get(k, envir = cache_env, inherits = FALSE))
          }

          .prep_year_series <- function(metric_key) {
            ts <- .get_time_series(api, yrs, sc, metric_key)
            ts <- .safe_df(ts)
            if (nrow(ts) == 0) return(data.frame())

            ts <- .ts_filter_long_by_metric(ts, metric_key)
            if (nrow(ts) == 0) return(data.frame())

            vcol <- .detect_metric_col(ts, metric_key) %||%
              .pick(names(ts), c("valor", "VALOR", "value", "VALUE", "n", "N", "total", "TOTAL", "casos", "casos_total", "incid_100k", "INCID_100K"))
            if (is.null(vcol) || !(vcol %in% names(ts))) return(data.frame())

            out <- .ts_prepare(
              ts,
              value_col = vcol,
              year_focus = max(yrs),
              metric_key = "cases",
              denom_pop = NULL,
              horizon = "years"
            )
            out <- .safe_df(out)
            if (nrow(out) == 0 || !all(c("x", "valor_ano") %in% names(out))) return(data.frame())

            out <- data.frame(
              x = as.character(out$x),
              valor = suppressWarnings(as.numeric(out$valor_ano)),
              stringsAsFactors = FALSE
            )
            out <- out[nzchar(out$x) & is.finite(out$valor), , drop = FALSE]
            out
          }

          cases_y <- .prep_year_series("cases")
          incid_y <- .prep_year_series("incid")

          if (nrow(cases_y) == 0 && nrow(incid_y) == 0) {
            return(.ec_message("Sem dados para série anual da tuberculose.", palette = pal_lines()))
          }

          if (nrow(cases_y) > 0) names(cases_y)[names(cases_y) == "valor"] <- "casos"
          if (nrow(incid_y) > 0) names(incid_y)[names(incid_y) == "valor"] <- "incidencia"

          base <- merge(cases_y, incid_y, by = "x", all = TRUE)
          base$ano_ord <- suppressWarnings(as.integer(base$x))
          base <- base[order(base$ano_ord, na.last = TRUE), , drop = FALSE]
          base <- base[is.finite(base$casos) | is.finite(base$incidencia), , drop = FALSE]
          if (nrow(base) == 0) {
            return(.ec_message("Sem dados para série anual da tuberculose.", palette = pal_lines()))
          }

          cols_bar <- pal_bars()
          cols_line <- pal_lines()
          col_cases <- if (is.character(cols_bar) && length(cols_bar) >= 1) cols_bar[[1]] else "#0B8A8F"
          map_pal_incid <- .obj_get(pack_r(), "MAP_PALETTE_INCID", NULL) %||% .obj_get(pack_r(), "MAP_PALETTE_CASES", NULL)
          col_incid <- if (is.character(map_pal_incid) && length(map_pal_incid) >= 4) {
            map_pal_incid[[4]]
          } else if (is.character(cols_line) && length(cols_line) >= 1) {
            cols_line[[1]]
          } else {
            "#2563EB"
          }

          p <- e_charts(base, x) %>%
            e_title(
              "Coeficiente de incidencia e numero de casos novos de tuberculose por ano de diagnóstico",
              left = 8,
              top = 4,
              textStyle = list(fontSize = 14, fontWeight = "bold", align = "left")
            ) %>%
            e_bar(
              casos,
              name = "Número de casos novos",
              y_index = 0,
              itemStyle = list(color = col_cases)
            ) %>%
            e_line(
              incidencia,
              name = "Coeficiente de incidência",
              y_index = 1,
              symbol = "circle",
              symbolSize = 5,
              showSymbol = TRUE,
              connectNulls = TRUE,
              lineStyle = list(color = col_incid, width = 3),
              itemStyle = list(color = col_incid)
            ) %>%
            e_x_axis(
              name = "Ano",
              type = "category",
              boundaryGap = TRUE,
              nameLocation = "middle",
              nameGap = 28,
              axisLabel = list(fontSize = 11, interval = if (nrow(base) >= 16) 1 else 0, rotate = 45)
            ) %>%
            e_y_axis(name = "Casos novos", min = 0, axisLabel = list(fontSize = 11)) %>%
            e_y_axis(index = 1, name = "Coeficiente de incidência", position = "right", min = 0, axisLabel = list(fontSize = 11)) %>%
            .ec_finalize(
              palette = c(col_cases, col_incid),
              datazoom = nrow(base) > 12,
              tooltip_trigger = "axis",
              grid_left = 52,
              grid_right = 56,
              grid_top = 74,
              grid_bottom = if (nrow(base) > 12) 66 else 58,
              legend_top = 24,
              legend_orient = "horizontal"
            )

          p <- .ec_force_resize_on_tab(p)
          assign(k, p, envir = cache_env)
          p
        }
      )
    }

    .render_ts_historic <- function(metric_key = "cases") {
      .with_rrmd_log(
        kind = "ts_historic",
        title = "Falha ao renderizar histórico mensal",
        ctx_builder = function(e) {
          list(
            error = conditionMessage(e),
            doenca = .doenca(),
            scope = scope_r(),
            ano = ano_r(),
            metric_key = metric_key
          )
        },
        expr = {
          api <- .api()
          sc <- scope_r()
          y <- ano_r()
          if (.is_tb_recife_bairro()) {
            year_end <- y - 4L
            if (year_end < 2010L) return(.ec_message("Período histórico indisponível.", palette = pal_lines()))
            agg <- .tb_recife_month_series(cfg_r(), .selected_bairro_key(), seq.int(2010L, year_end))
            agg <- .safe_df(agg)
            if (nrow(agg) == 0) return(.ec_message("Sem dados no histórico mensal para o bairro selecionado.", palette = pal_lines()))
            agg$x <- agg$periodo
            p <- e_charts(agg, x) %>%
              e_line(casos, name = "Casos", symbol = "circle", symbolSize = 3) %>%
              e_title(paste0("Histórico mensal - ", .selected_bairro_name()), left = 8, top = 4, textStyle = list(fontSize = 14, fontWeight = "bold")) %>%
              e_x_axis(type = "category", axisLabel = list(fontSize = 10, rotate = 45)) %>%
              e_y_axis(name = "Casos", min = 0) %>%
              .ec_finalize(palette = pal_lines(), datazoom = TRUE, tooltip_trigger = "axis", grid_left = 48, grid_top = 58, grid_bottom = 64, legend_top = NULL, legend_show = FALSE)
            return(.ec_force_resize_on_tab(p))
          }

          if (is.null(api)) return(.ec_message("API de dados indisponível.", palette = pal_lines()))

          year_end <- y - 4L
          year_start <- 2010L
          if (year_end < year_start) return(.ec_message("Período histórico indisponível.", palette = pal_lines()))

          yrs <- seq.int(year_start, year_end)

          metric_key_ts <- metric_key
          if (identical(.norm_key(metric_key), "incid")) metric_key_ts <- "cases"
          
          ts <- .get_time_series(api, yrs, sc, metric_key_ts)
          if (nrow(ts) == 0) return(.ec_message("Sem dados no histórico mensal.", palette = pal_lines()))
          
          ts <- .ts_filter_long_by_metric(ts, metric_key_ts)
          val_col <- .detect_metric_col(ts, metric_key_ts) %||%
            .pick(names(ts), c("valor", "n", "total", "casos"))

          nms <- names(ts)
          if (!("ano" %in% nms) && "ANO" %in% nms) ts$ano <- ts$ANO
          if (!("mes" %in% nms) && "MES" %in% nms) ts$mes <- ts$MES

          if (!("ano" %in% names(ts)) || !("mes" %in% names(ts)) || is.null(val_col)) {
            return(.ec_message("Estrutura de dados incompatível.", palette = pal_lines()))
          }

          ts$ano <- suppressWarnings(as.integer(ts$ano))
          ts$mes <- suppressWarnings(as.integer(ts$mes))
          ts$valor <- suppressWarnings(as.numeric(ts[[val_col]]))
          ts <- ts[is.finite(ts$ano) & is.finite(ts$mes) & is.finite(ts$valor), , drop = FALSE]
          
          if (nrow(ts) == 0) return(.ec_message("Sem dados no histórico mensal.", palette = pal_lines()))

          ts$mes <- pmax(pmin(ts$mes, 12L), 1L)
          ts$periodo <- sprintf("%04d-%02d", ts$ano, ts$mes)

          agg <- ts %>%
            group_by(ano, mes, periodo) %>%
            summarise(casos = sum(valor, na.rm = TRUE), .groups = "drop") %>%
            arrange(ano, mes)

          if (nrow(agg) == 0) return(.ec_message("Sem dados no histórico mensal.", palette = pal_lines()))

          agg$periodo_label <- paste0(agg$ano, "-", sprintf("%02d", agg$mes))

          p <- e_charts(agg, periodo_label) %>%
            e_line(casos, name = "Casos", symbol = "circle", symbolSize = 3, 
                   lineStyle = list(width = 2), showSymbol = FALSE, connectNulls = TRUE) %>%
            e_x_axis(name = "Período (Ano-Mês)", type = "category", 
                     axisLabel = list(fontSize = 9, rotate = 45, interval = "auto")) %>%
            e_y_axis(name = "Casos", min = 0, axisLabel = list(fontSize = 10)) %>%
            .ec_finalize(
              palette = pal_lines(),
              datazoom = (year_end - year_start + 1) > 8,
              tooltip_trigger = "axis",
              grid_left = 48,
              grid_top = 40,
              grid_bottom = 68,
              legend_top = NULL,
              legend_bottom = 5,
              legend_orient = "horizontal"
            )

          .ec_force_resize_on_tab(p)
        }
      )
    }

    .render_rank <- function(kind = c("cases", "incid", "obitos", "cura")) {
      kind <- match.arg(kind)

      .with_rrmd_log(
        kind = "ranking",
        title = "Falha ao renderizar ranking",
        ctx_builder = function(e) {
          sc <- scope_r()
          y <- ano_r()
          list(
            error = conditionMessage(e),
            doenca = .doenca(),
            scope = sc,
            ano = y,
            kind = kind,
            rank_mode = rank_mode_r(),
            palette = pal()
          )
        },
        expr = {
          sc <- scope_r()
          y <- ano_r()
          mlab <- metric_label_r()
          click_id <- session$ns("rank_click")

          if (isTRUE(.is_tuberculose()) && isTRUE(.is_recife_lock())) {
            raw <- .read_tb_recife_bairro_rank(cfg_r(), year = y, top_n = 15L)
            raw <- .safe_df(raw)
            if (nrow(raw) == 0) {
              return(.ec_message("Sem dados para ranking de bairros.", palette = pal_bars()))
            }

            out_plot <- .rank_prepare_for_plot(raw, top_n = 15)
            rank_map_r(out_plot)
            return(.plot_rank(out_plot, click_input_id = NULL, palette = pal_bars(), metric_label = "Casos"))
          }

          if (sc$level == "BR") {
            if (identical(rank_mode_r(), "mun")) {
              # Tenta carregamento otimizado: ranking pré-processado de municípios
              raw <- tryCatch({
                ranking_path <- file.path(.find_dashboard_dir(), "ranking_municipios_brasil", "data.parquet")
                if (file.exists(ranking_path)) {
                  df <- arrow::read_parquet(ranking_path)
                  df <- df %>%
                    filter(doenca == .doenca(), ano == ano_r()) %>%
                    arrange(ranking_posicao) %>%
                    head(15) %>%
                    collect()
                  df
                } else {
                  data.frame()
                }
              }, error = function(e) data.frame())
              
              # Fallback: tenta ler do dataset de incidência agregando UFs
              if (nrow(raw) == 0) {
                raw <- .inc_rank_topn(.doenca(), y, nivel = "MUN", uf = NULL, kind = kind, top_n = 15)
              }
              if (nrow(raw) == 0) {
                raw <- .rank_from_api(.api(), y, sc, kind = kind, mode = "mun")
              }
              if (nrow(raw) == 0) return(.ec_message("Ranking de municípios (Brasil) indisponível.", palette = pal_bars()))

              out_plot <- .rank_prepare_for_plot(raw, top_n = 15)
              rank_map_r(out_plot)
              return(.plot_rank(out_plot, click_input_id = click_id, palette = pal_bars(), metric_label = mlab))
            } else {
              raw <- .inc_rank_topn(.doenca(), y, nivel = "UF", kind = kind, top_n = 10)
              if (nrow(raw) == 0) raw <- .rank_from_api(.api(), y, sc, kind = kind, mode = "ufs")
              if (nrow(raw) == 0) return(.ec_message("Sem dados para ranking (ano/escopo).", palette = pal_bars()))

              out_plot <- .rank_prepare_for_plot(raw, top_n = 10)
              rank_map_r(out_plot)
              return(.plot_rank(out_plot, click_input_id = click_id, palette = pal_bars(), metric_label = mlab))
            }
          } else {
            uf0 <- sc$uf
            if (is.null(uf0) || !nzchar(uf0)) return(.ec_message("Selecione uma UF para ver municípios.", palette = pal_bars()))

            raw <- .inc_rank_topn(.doenca(), y, nivel = "MUN", uf = uf0, kind = kind, top_n = 15)
            if (nrow(raw) == 0) raw <- .rank_from_api(.api(), y, sc, kind = kind, mode = "mun")
            if (nrow(raw) == 0) return(.ec_message("Sem dados para ranking (ano/escopo).", palette = pal_bars()))

            out_plot <- .rank_prepare_for_plot(raw, top_n = 15)
            rank_map_r(out_plot)
            return(.plot_rank(out_plot, click_input_id = click_id, palette = pal_bars(), metric_label = mlab))
          }
        }
      )
    }

    # -----------------------------------------------------------------------
    # Outputs
    # -----------------------------------------------------------------------

    # ---- Time Series -----------------------------------------------

    output$ts_main <- {
      renderEcharts4r({
        if (identical(evol_horizon_r(), "years") && .is_tuberculose()) {
          return(.render_tb_ts_years_combo())
        }
        .render_ts_metric(
          metric_key = metric_r(),
          horizon    = evol_horizon_r()
        )
      })
    }

    output$evol_layout_ui <- renderUI({
      ns <- session$ns
      hz <- evol_horizon_r()
      
      if (identical(hz, "months")) {
        div(
          class = "sinan-evol-layout-months",
          div(
            class = "sinan-evol-chart-half",
            div(class = "sinan-echart-fill", echarts4rOutput(ns("ts_main"), height = "100%"))
          ),
          div(
            class = "sinan-evol-chart-half",
            tags$h5(style = "margin:0 0 8px 8px; font-weight:600; font-size:14px;", "Epicurva por mês"),
            div(class = "sinan-echart-fill", style = "flex:1;", echarts4rOutput(ns("ts_historic"), height = "100%"))
          )
        )
      } else {
        div(
          class = "sinan-evol-layout-years",
          div(
            class = "sinan-evol-main-chart",
            div(class = "sinan-echart-fill", echarts4rOutput(ns("ts_main"), height = "400px"))
          )
        )
      }
    })

    output$ts_historic <- renderEcharts4r({

      if (evol_horizon_r() != "months") return(NULL)
      .render_ts_historic(metric_key = metric_r())
    })

    # ---- Ranking ----------------------------------------------------------------

    output$rank_main <- {
      out <- renderEcharts4r({
        .render_rank(metric_r())
      })

      if ("bindCache" %in% getNamespaceExports("shiny")) {
        out <- shiny::bindCache(
          out,
          ano_r(),
          scope_r()$level,
          scope_r()$uf,
          scope_r()$mun,
          metric_r(),
          rank_mode_r(),
          .doenca()
        )
      }
      out
    }

    # ---- Variáveis de interesse UI/plot -----------------------------------------

    .comp_vars_for_scope <- function() {
      api <- .api()
      sc  <- scope_r()
      y   <- ano_r()

      vv <- character(0)
      for (sc_try in .scope_fallbacks(sc)) {
        if (!is.null(api)) vv <- .get_composicao_vars_available(api, y, sc_try)
        if (length(vv) == 0) vv <- .landing_vars_available(.doenca(), y, sc_try)
        vv <- vv[!is.na(vv) & nzchar(vv)]
        if (length(vv) > 0) break
      }

      preferred <- .obj_get(pack_r(), "PREFERRED_COMPOSITION_VARS", NULL)
      if (!is.null(preferred) && length(preferred) > 0) {
        vv <- vv[vv %in% preferred]
      }

      vv <- unique(vv)
      comp_vars_r(vv)
      vv
    }

    .comp_var_label <- function(v) {
      labels <- .obj_get(pack_r(), "COMPOSITION_VAR_LABELS", list())
      if (is.list(labels) && !is.null(labels[[v]])) labels[[v]] else v
    }

    .comp_topic_output_id <- function(v, i) {
      paste0("comp_topic_", i, "_", gsub("[^A-Za-z0-9_]+", "_", v))
    }

    # Exibe todos os graficos em uma grade de duas colunas.
    #
    # A grade de 10 topicos custava ~350 ms cada, renderizada sempre, para todo
    # usuario: medido, ~2,8 s dos 6,5 s de abertura. O caminho de um grafico so
    # ja existia pronto neste arquivo (output$comp_ui + comp_vars_ui +
    # comp_plot, este ultimo com bindCache), apenas nao estava montado -- e o
    # comp_plot era calculado assim mesmo, por causa do suspendWhenHidden
    # FALSE na linha ~5299. O painel montava 10 graficos e computava um 11o
    # que ninguem via.
    #
    # Reversivel sem editar codigo: SINAN_COMP_GRID=0 volta ao seletor com
    # um grafico por vez.
    .comp_grid_enabled <- function() {
      !identical(Sys.getenv("SINAN_COMP_GRID", unset = "1"), "0")
    }
    output$comp_ui <- renderUI({
      ns <- session$ns
      div(
        class = "sinan-comp",
        div(class = "sinan-comp-controls", uiOutput(ns("comp_vars_ui"))),
        div(class = "sinan-echart-fill", echarts4rOutput(ns("comp_plot"), height = "720px")),
        div(class = "sinan-comp-note", uiOutput(ns("comp_note")))
      )
    })

    # Reactive que filtra variáveis pela lista PREFERRED_COMPOSITION_VARS do disease pack
    comp_vars_filtered_r <- reactive({
      vv <- comp_vars_r()
      preferred <- .obj_get(pack_r(), "PREFERRED_COMPOSITION_VARS", NULL)

      # Se tem preferidas, filtra; senão retorna todas
      if (!is.null(preferred) && length(preferred) > 0) {
        vv <- vv[vv %in% preferred]
      }
      
      vv
    })

    output$comp_external_ui <- renderUI({
      .with_rrmd_log(
        kind = "vars_interesse_box",
        title = "Falha ao carregar box de tópicos de interesse",
        ctx_builder = function(e) {
          list(error = conditionMessage(e), doenca = .doenca(), scope = scope_r(), ano = ano_r())
        },
        expr = {
          ns <- session$ns
          vv <- .comp_vars_for_scope()

          if (length(vv) == 0) {
            return(
              bslib::card(
                class = "shadow-sm sinan-panel sinan-topicos-box",
                bslib::card_body(
                  div(class = "sinan-topicos-title", "Tópicos de interesse"),
                  .msg_panel("Sem tópicos de interesse disponíveis (ano/escopo).")
                )
              )
            )
          }

          if (!.comp_grid_enabled()) {
            # Um grafico por vez, escolhido pelo seletor. Reaproveita
            # output$comp_ui, que ja existia montado com controles + comp_plot.
            return(
              bslib::card(
                class = "shadow-sm sinan-panel sinan-topicos-box",
                bslib::card_body(
                  div(class = "sinan-topicos-title", "Tópicos de interesse"),
                  # Montado aqui em vez de reusar output$comp_ui: aquele bloco usa
                  # a classe sinan-echart-fill, que dentro deste card estica o
                  # container para ~6.900 px de altura. Altura fixa resolve.
                  div(class = "sinan-comp-controls", uiOutput(ns("comp_vars_ui"))),
                  div(echarts4rOutput(ns("comp_plot"), height = "420px")),
                  div(class = "sinan-comp-note", uiOutput(ns("comp_note")))
                )
              )
            )
          }

          bslib::card(
            class = "shadow-sm sinan-panel sinan-topicos-box",
            bslib::card_body(
              div(class = "sinan-topicos-title", "Tópicos de interesse"),
              div(
                class = "sinan-topicos-grid",
                lapply(seq_along(vv), function(i) {
                  v <- vv[[i]]
                  div(
                    class = "sinan-topico-item",
                    div(class = "sinan-topico-title", .comp_var_label(v)),
                    div(
                      class = "sinan-topico-chart",
                      echarts4rOutput(ns(.comp_topic_output_id(v, i)), height = "320px")
                    )
                  )
                })
              )
            )
          )
        }
      )
    })

    observe({
      # Sem a grade, nao registramos os 10 outputs. Isso e o que economiza o
      # tempo: com suspendWhenHidden = FALSE eles rodariam mesmo sem aparecer
      # na tela, entao tirar so da UI nao bastaria.
      if (!.comp_grid_enabled()) return(NULL)

      vv <- .comp_vars_for_scope()
      if (length(vv) == 0) return(NULL)

      for (i in seq_along(vv)) {
        local({
          var_i <- vv[[i]]
          out_id <- .comp_topic_output_id(var_i, i)

          output[[out_id]] <- renderEcharts4r({
            .with_rrmd_log(
              kind = "vars_interesse_box_plot",
              title = "Falha ao renderizar tópico de interesse",
              ctx_builder = function(e) {
                list(
                  error = conditionMessage(e),
                  doenca = .doenca(),
                  scope = scope_r(),
                  ano = ano_r(),
                  comp_var = var_i
                )
              },
              expr = {
                tn_cap <- .as_int1(.obj_get(cfg_r(), "COMP_TOPN_DEFAULT", 30L), 30L)
                if (!is.finite(tn_cap) || tn_cap <= 0) tn_cap <- 30L

                df <- .comp_fetch_df(var_i, top_n = 800)
                .plot_comp(df, var_sel = var_i, top_n = tn_cap, palette = pal_bars())
              }
            )
          })


          tryCatch(shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE), error = function(e) NULL)
        })
      }

      invisible(NULL)
    })
    output$comp_vars_ui <- renderUI({
      .with_rrmd_log(
        kind = "vars_interesse_vars",
        title = "Falha ao carregar variáveis de interesse",
        ctx_builder = function(e) {
          list(error = conditionMessage(e), doenca = .doenca(), scope = scope_r(), ano = ano_r())
        },
        expr = {
          api <- .api()
          sc  <- scope_r()
          y   <- ano_r()

          vv <- character(0)
          for (sc_try in .scope_fallbacks(sc)) {
            if (!is.null(api)) vv <- .get_composicao_vars_available(api, y, sc_try)
            if (length(vv) == 0) vv <- .landing_vars_available(.doenca(), y, sc_try)
            vv <- vv[!is.na(vv) & nzchar(vv)]
            if (length(vv) > 0) break
          }

          if (length(vv) == 0) return(.msg_panel("Sem variáveis de interesse disponíveis (ano/escopo)."))

          # Filtrar variáveis preferidas se configuradas
          preferred <- .obj_get(pack_r(), "PREFERRED_COMPOSITION_VARS", NULL)
          if (!is.null(preferred) && length(preferred) > 0) {
            vv <- vv[vv %in% preferred]
          }

          if (length(vv) == 0) return(.msg_panel("Sem variáveis de interesse disponíveis (ano/escopo)."))

          comp_vars_r(vv)

          # Criar mapeamento de labels para exibição
          var_labels <- .obj_get(pack_r(), "COMPOSITION_VAR_LABELS", list())
          vv_choices <- if (is.list(var_labels) && length(var_labels) > 0) {
            setNames(vv, sapply(vv, function(v) var_labels[[v]] %||% v))
          } else {
            vv
          }

          cur <- .as_chr1(input$comp_var, NULL)
          sel <- if (!is.null(cur) && nzchar(cur) && (cur %in% vv)) cur else vv[[1]]

          if (is.null(cur) || !nzchar(cur) || !(cur %in% vv)) {
            tryCatch(updateSelectizeInput(session, "comp_var", selected = sel), error = function(e) NULL)
          }

          div(
            class = "form-group sinan-selectize",
            selectizeInput(
              session$ns("comp_var"),
              NULL,
              choices = vv_choices,
              selected = sel,
              options = list(
                placeholder = "Busque uma variável...",
                maxOptions = 4000,
                create = FALSE,
                closeAfterSelect = TRUE
              ),
              width = "100%"
            )
          )
        }
      )
    })

    output$comp_plot <- {
      out <- renderEcharts4r({
        .with_rrmd_log(
          kind = "vars_interesse_plot",
          title = "Falha ao renderizar variáveis de interesse",
          ctx_builder = function(e) {
            list(
              error    = conditionMessage(e),
              doenca   = .doenca(),
              scope    = scope_r(),
              ano      = ano_r(),
              comp_var = .as_chr1(input$comp_var, NULL)
            )
          },
          expr = {
            api <- .api()
            sc  <- scope_r()
            y   <- ano_r()

            var_sel <- .as_chr1(input$comp_var, NULL)
            if (is.null(var_sel) || !nzchar(var_sel)) {
              vv0 <- comp_vars_r()
              if (length(vv0) >= 1) var_sel <- vv0[[1]]
            }

            tn_cap <- .as_int1(.obj_get(cfg_r(), "COMP_TOPN_DEFAULT", 30L), 30L)
            if (!is.finite(tn_cap) || tn_cap <= 0) tn_cap <- 30L

            k <- paste0(
              "comp|", sc$level, "|", sc$uf %||% "", "|", sc$mun %||% "",
              "|", y, "|", .doenca(), "|v=", var_sel %||% "", "|cap=", tn_cap,
              "|tb_grau=", {
                gf <- tb_grau_filter_r()
                if (is.null(gf)) "" else paste0(gf$var, ":", gf$value)
              }
            )

            if (exists(k, envir = cache_env, inherits = FALSE)) {
              df <- get(k, envir = cache_env, inherits = FALSE)
              return(.plot_comp(df, var_sel = var_sel, top_n = tn_cap, palette = pal_bars()))
            }

            df <- .comp_fetch_df(var_sel, top_n = 800)

            assign(k, df, envir = cache_env)
            .plot_comp(df, var_sel = var_sel, top_n = tn_cap, palette = pal_bars())
          }
        )
      })

      if ("bindCache" %in% getNamespaceExports("shiny")) {
        out <- shiny::bindCache(out, ano_r(), scope_r()$level, scope_r()$uf, scope_r()$mun, .doenca(), input$comp_var, tb_grau_filter_r())
      }
      out
    }

    output$comp_note <- renderUI({
      var_sel <- .as_chr1(input$comp_var, NULL)
      if (is.null(var_sel) || !nzchar(var_sel) || !identical(toupper(var_sel), "CS_GESTANT")) return(NULL)

      df <- .comp_fetch_df(var_sel, top_n = 800)
      df <- .safe_df(df)
      if (nrow(df) == 0) return(NULL)

      c_var <- .pick(names(df), c("variavel", "VARIAVEL"))
      c_cat <- .pick(names(df), c("categoria", "valor_lbl", "VALOR_LBL", "cat", "CAT"))
      c_tot <- .pick(names(df), c("total", "n", "N", "TOTAL"))
      if (is.null(c_var) || is.null(c_cat) || is.null(c_tot)) return(NULL)

      df$variavel <- toupper(trimws(as.character(df[[c_var]])))
      df$categoria <- as.character(df[[c_cat]])
      df$total <- suppressWarnings(as.numeric(df[[c_tot]]))
      df <- df[is.finite(df$total) & nzchar(df$variavel) & nzchar(df$categoria), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)

      df <- df[df$variavel == "CS_GESTANT", , drop = FALSE]
      if (nrow(df) == 0) return(NULL)

      cat_norm <- .norm_comp_cat(df$categoria)
      na_total <- sum(df$total[cat_norm %in% c("NAOSEAPLICA", "NAOAPLICA")], na.rm = TRUE)

      tags$span(paste0("Nao se aplica: ", .fmt_int(na_total)))
    })

    # ---- Pirâmide ----------------------------------------------------------------

    output$piramide_ui <- renderUI({
      ns <- session$ns
      div(
        class = "sinan-pyr",
        div(class = "sinan-echart-fill", echarts4rOutput(ns("piramide"), height = "720px"))
      )
    })

    output$piramide <- {
      out <- renderEcharts4r({
        .with_rrmd_log(
          kind = "piramide",
          title = "Falha ao renderizar pirâmide",
          ctx_builder = function(e) {
            sc <- scope_r()
            y  <- ano_r()
            list(
              error   = conditionMessage(e),
              doenca  = .doenca(),
              scope   = sc,
              ano     = y,
              metric  = metric_r(),
              palette = pal()
            )
          },
          expr = {
            api    <- .api()
            sc     <- scope_r()
            y      <- ano_r()
            doenca <- .doenca()

            if (.is_tb_recife_bairro()) {
              bkey  <- .selected_bairro_key()
              bname <- .selected_bairro_name()

              df_evt_bairro <- .tb_recife_pyr_evt(cfg_r(), bairro_key = bkey, year = y)
              if (nrow(df_evt_bairro) == 0) {
                return(.ec_message(
                  sprintf("Sem dados de pirâmide para %s (ano/escopo).", bname %||% "bairro selecionado"),
                  palette = pal()
                ))
              }

              pyr_palette_bairro <- pal_bars()
              if (toupper(trimws(as.character(doenca %||% ""))) %in%
                  c("TUBERCULOSE", "TUBERCULOSIS", "TB", "TUB")) {
                pack_cols_bairro <- .obj_get(pack, "COLORS", list()) %||% list()
                female_color_bairro <- .obj_get(pack_cols_bairro, "danger", "#DB400A") %||% "#DB400A"
                if (length(pyr_palette_bairro) < 2L) pyr_palette_bairro <- c(pyr_palette_bairro, female_color_bairro)
                pyr_palette_bairro[[2]] <- female_color_bairro
              }

              return(.plot_piramide_simple_echarts(
                df_evt = df_evt_bairro,
                palette = pyr_palette_bairro,
                evt_label = "CASOS"
              ))
            }

            if (is.null(api) && is.null(.open_piramides_ds())) {
              return(.ec_message("Dados de pirâmide indisponíveis (API e fallback local ausentes).", palette = pal()))
            }

            # Force cases-only pyramid
            evt <- "CASOS"
            evt_label <- "CASOS"

            k_raw_evt <- paste0("pyr_raw_evt|", sc$level, "|", sc$uf %||% "", "|", sc$mun %||% "", "|", y, "|", doenca, "|", evt)

            raw_evt <- if (exists(k_raw_evt, envir = cache_env, inherits = FALSE)) get(k_raw_evt, envir = cache_env, inherits = FALSE) else NULL

            if (is.null(raw_evt)) {
              raw_evt <- data.frame()
              if (!is.null(api)) {
                raw_evt <- .get_piramide_broad(api, y, sc, doenca = doenca, tipo = evt)
              }
              if (!is.data.frame(raw_evt) || nrow(raw_evt) == 0) {
                raw_evt <- .pyr_local_read(doenca = doenca, tipo = evt, y = y, sc = sc)
              }
              assign(k_raw_evt, raw_evt, envir = cache_env)
            }

            df_evt <- .safe_df(raw_evt)

            if (nrow(df_evt) == 0) {
              return(.ec_message("Sem dados de pirâmide (ano/escopo).", palette = pal()))
            }

            # Pirâmide simples com echarts4r (casos)
            # Na tuberculose, mulheres usam a cor de atencao do layout.
            pyr_palette <- pal_bars()
            if (toupper(trimws(as.character(doenca %||% ""))) %in%
                c("TUBERCULOSE", "TUBERCULOSIS", "TB", "TUB")) {
              pack_cols <- .obj_get(pack, "COLORS", list()) %||% list()
              female_color <- .obj_get(pack_cols, "danger", "#DB400A") %||% "#DB400A"
              if (length(pyr_palette) < 2L) pyr_palette <- c(pyr_palette, female_color)
              pyr_palette[[2]] <- female_color
            }

            .plot_piramide_simple_echarts(
              df_evt = df_evt,
              palette = pyr_palette,
              evt_label = evt_label
            )
          }
        )
      })

      if ("bindCache" %in% getNamespaceExports("shiny")) {
        out <- shiny::bindCache(out, ano_r(), scope_r()$level, scope_r()$uf, scope_r()$mun, .doenca(), metric_r(), .selected_bairro_key() %||% "")
      }
      out
    }

    # ---- outputOptions  -----------------------------------

    .safe_outputOptions <- function(name, ...) {
      tryCatch({
        if (!is.null(output[[name]])) shiny::outputOptions(output, name, ...)
      }, error = function(e) NULL)
    }

        observe({
          .safe_outputOptions("ts_main", suspendWhenHidden = FALSE)
          .safe_outputOptions("rank_main", suspendWhenHidden = FALSE)
    
          .safe_outputOptions("comp_plot", suspendWhenHidden = FALSE)
          .safe_outputOptions("comp_ui", suspendWhenHidden = FALSE)
          .safe_outputOptions("comp_vars_ui", suspendWhenHidden = FALSE)
          .safe_outputOptions("comp_external_ui", suspendWhenHidden = FALSE)
    
          .safe_outputOptions("piramide", suspendWhenHidden = FALSE)
          .safe_outputOptions("piramide_ui", suspendWhenHidden = FALSE)
        })
      })
    }
