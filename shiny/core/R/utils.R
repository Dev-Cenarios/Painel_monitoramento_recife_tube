# dashs/shiny/core/R/utils.R


SINAN_DEBUG <- FALSE

.SINAN_UTILS_LOADED <- TRUE

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  a
}

.num <- function(x) suppressWarnings(as.numeric(x))

.fmt_int <- function(x) {
  x <- .num(x)
  if (length(x) == 0 || !is.finite(x[1])) return("NA")
  format(as.integer(round(x[1])), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

.fmt_num <- function(x, digits = 2) {
  x <- .num(x)
  if (length(x) == 0 || !is.finite(x[1])) return("NA")
  format(round(x[1], digits), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

.try_set_locale <- function() {
  locs <- c("pt_BR.UTF-8","Portuguese_Brazil.utf8","Portuguese_Brazil.1252","Portuguese_Brazil")
  for (lc in locs) try(suppressWarnings(Sys.setlocale("LC_CTYPE", lc)), silent = TRUE)
  try(options(OutDec = ","), silent = TRUE)
}

.maybe_fix_utf8 <- function(x) {
  if (!is.character(x) || length(x) == 0) return(x)
  idx <- !is.na(x) & (grepl("Ã.|Â.", x, perl = TRUE))
  if (!any(idx)) return(x)
  y <- x
  conv <- suppressWarnings(iconv(y[idx], from = "latin1", to = "UTF-8"))
  y[idx] <- ifelse(is.na(conv), y[idx], conv)
  y
}

.sanitize_df <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  chr <- vapply(df, is.character, logical(1))
  if (any(chr)) for (nm in names(df)[chr]) df[[nm]] <- .maybe_fix_utf8(df[[nm]])
  df
}

.hash_key <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(x, algo = "xxhash64"))
  x <- gsub("[^A-Za-z0-9_\\-]", "_", x)
  substr(x, 1, 180)
}

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

.obj_set <- function(obj, key, value) {
  obj <- .res(obj)
  if (is.null(obj)) return(invisible(NULL))
  if (.is_rv(obj)) {
    obj[[key]] <- value
    return(invisible(NULL))
  }
  if (is.environment(obj)) {
    assign(key, value, envir = obj)
    return(invisible(NULL))
  }
  invisible(NULL)
}

.pick_scalar <- function(x, fallback = NULL) {
  x <- .res(x)
  if (is.null(x) || length(x) == 0) return(fallback)
  if (is.list(x) && length(x) == 1) x <- x[[1]]
  if (is.null(x) || length(x) == 0) return(fallback)
  if (is.atomic(x)) return(x[[1]] %||% fallback)
  x[[1]] %||% x %||% fallback
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
  v <- as.character(x)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) default else v[[1]]
}

.debug_enabled <- function(cfg = NULL) {
  cfg <- .res(cfg)
  if (isTRUE(.obj_get(cfg, "DEBUG", FALSE))) return(TRUE)
  if (isTRUE(getOption("sinan.debug", FALSE))) return(TRUE)
  if (identical(Sys.getenv("SINAN_DEBUG"), "1")) return(TRUE)
  isTRUE(get("SINAN_DEBUG", envir = .GlobalEnv, inherits = TRUE))
}

.dbg_make <- function(enabled, max_lines = 350L) {
  log_rv <- shiny::reactiveVal(character(0))
  push <- function(...) {
    if (!isTRUE(enabled)) return(invisible(NULL))
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste(..., collapse = " "))
    cat(msg, "\n")
    old <- log_rv()
    new <- c(old, msg)
    if (length(new) > max_lines) new <- tail(new, max_lines)
    log_rv(new)
    invisible(NULL)
  }
  list(enabled = enabled, push = push, get = log_rv)
}

.dbg_box_ui <- function(ns, id) {
  shiny::tags$details(
    open = FALSE,
    style = "margin-top:8px;",
    shiny::tags$summary(style="font-weight:700; cursor:pointer;", "Debug"),
    shiny::tags$pre(
      style = "max-height:240px; overflow:auto; background:#0b1020; color:#d6e4ff; padding:10px; border-radius:10px; font-size:12px;",
      shiny::verbatimTextOutput(ns(id))
    )
  )
}

.safe_list_dirs <- function(path, depth = 2L, max_items = 80L) {
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) return(character(0))
  out <- character(0)
  cur <- normalizePath(path, winslash = "/", mustWork = FALSE)
  out <- c(out, cur)
  for (i in seq_len(depth)) {
    nxt <- unlist(lapply(out, function(p) {
      d <- tryCatch(list.dirs(p, full.names = TRUE, recursive = FALSE), error = function(e) character(0))
      d
    }), use.names = FALSE)
    nxt <- nxt[dir.exists(nxt)]
    if (length(nxt) == 0) break
    out <- unique(c(out, nxt))
    if (length(out) > max_items) break
  }
  unique(out)
}

.find_dashboard_dir <- function(cfg) {
  cfg <- .res(cfg)

  dash <- .obj_get(cfg, "DASHBOARD_DIR", NULL)
  if (is.character(dash) && nzchar(dash) && dir.exists(dash)) {
    return(normalizePath(dash, winslash = "/", mustWork = FALSE))
  }

  root <- .obj_get(cfg, "ROOT", NULL)
  if (is.character(root) && nzchar(root)) {
    cand <- file.path(root, "data", "parquet")
    if (dir.exists(cand)) return(normalizePath(cand, winslash = "/", mustWork = FALSE))
  }

  p <- normalizePath(tryCatch(getwd(), error = function(e) "."), winslash = "/", mustWork = FALSE)
  for (i in 0:10) {
    cand <- file.path(p, "data", "parquet")
    if (dir.exists(cand)) return(normalizePath(cand, winslash = "/", mustWork = FALSE))
    cand2 <- file.path(p, "dashs", "shiny", "core", "data", "parquet")
    if (dir.exists(cand2)) return(normalizePath(cand2, winslash = "/", mustWork = FALSE))
    p2 <- dirname(p)
    if (identical(p2, p)) break
    p <- p2
  }

  NULL
}

.UF_CODE_TO_SIGLA <- c(
  `11`="RO",`12`="AC",`13`="AM",`14`="RR",`15`="PA",`16`="AP",`17`="TO",
  `21`="MA",`22`="PI",`23`="CE",`24`="RN",`25`="PB",`26`="PE",`27`="AL",`28`="SE",`29`="BA",
  `31`="MG",`32`="ES",`33`="RJ",`35`="SP",
  `41`="PR",`42`="SC",`43`="RS",
  `50`="MS",`51`="MT",`52`="GO",`53`="DF"
)

.strip_accents <- function(x) {
  y <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
  ifelse(is.na(y), x, y)
}

.UF_NAME_TO_SIGLA <- c(
  "ACRE"="AC","ALAGOAS"="AL","AMAPA"="AP","AMAZONAS"="AM","BAHIA"="BA","CEARA"="CE",
  "DISTRITO FEDERAL"="DF","ESPIRITO SANTO"="ES","GOIAS"="GO","MARANHAO"="MA",
  "MATO GROSSO"="MT","MATO GROSSO DO SUL"="MS","MINAS GERAIS"="MG","PARA"="PA",
  "PARAIBA"="PB","PARANA"="PR","PERNAMBUCO"="PE","PIAUI"="PI","RIO DE JANEIRO"="RJ",
  "RIO GRANDE DO NORTE"="RN","RIO GRANDE DO SUL"="RS","RONDONIA"="RO","RORAIMA"="RR",
  "SANTA CATARINA"="SC","SAO PAULO"="SP","SERGIPE"="SE","TOCANTINS"="TO"
)

.to_uf <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x <- trimws(x)
  x <- ifelse(nzchar(x), x, NA_character_)
  if (all(is.na(x))) return(x)

  x_up <- toupper(x)
  x_norm <- toupper(.strip_accents(x_up))

  is_num <- !is.na(x_norm) & grepl("^[0-9]+$", x_norm)
  if (any(is_num)) x_norm[is_num] <- unname(.UF_CODE_TO_SIGLA[x_norm[is_num]])

  needs_map <- !is.na(x_norm) & (nchar(x_norm) != 2 | grepl("\\s", x_norm))
  if (any(needs_map)) x_norm[needs_map] <- unname(.UF_NAME_TO_SIGLA[x_norm[needs_map]])

  x_norm <- ifelse(!is.na(x_norm) & nzchar(x_norm), x_norm, NA_character_)
  x_norm
}

.to_mun7 <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  x <- gsub("\\D+", "", x)
  x <- ifelse(nzchar(x), x, NA_character_)
  if (!requireNamespace("stringr", quietly = TRUE)) return(x)
  stringr::str_pad(x, 7, pad = "0")
}

.norm_str <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  trimws(x)
}

.doenca_vals <- function(doenca) {
  d <- .norm_str(doenca %||% "")
  if (!nzchar(d)) return(character(0))
  c(d, tolower(d))
}

.has_parquet <- function(path) {
  if (is.null(path) || !dir.exists(path)) return(FALSE)
  length(list.files(path, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)) > 0
}

.find_existing_dir <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  for (p in paths) if (dir.exists(p) && .has_parquet(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  NULL
}

.part_paths <- function(base, doencas, ano, level = NULL, uf = NULL) {
  if (is.null(base)) return(character(0))
  y <- as.integer(ano)
  if (!is.finite(y)) return(character(0))

  lvl <- toupper(as.character(level %||% ""))
  lvl_parts <- unique(na.omit(c(
    if (nzchar(lvl)) paste0("nivel=", lvl),
    if (nzchar(lvl)) paste0("level=", lvl)
  )))

  dd <- doencas[!is.na(doencas) & nzchar(as.character(doencas))]
  if (length(dd) == 0) dd <- character(0)

  u <- if (!is.null(uf) && nzchar(uf)) .to_uf(uf) else NULL

  out <- character(0)
  addp <- function(...) out <<- c(out, file.path(base, ...))

  if (length(dd) > 0) {
    for (d in dd) {
      addp(paste0("doenca=", d), paste0("ano=", y))
      addp(paste0("ano=", y), paste0("doenca=", d))
    }
  } else {
    addp(paste0("ano=", y))
  }

  if (length(lvl_parts) > 0) {
    for (lv in lvl_parts) {
      if (length(dd) > 0) {
        for (d in dd) {
          addp(lv, paste0("doenca=", d), paste0("ano=", y))
          addp(lv, paste0("ano=", y), paste0("doenca=", d))
        }
      } else {
        addp(lv, paste0("ano=", y))
      }

      if (!is.null(u) && nzchar(u)) {
        if (length(dd) > 0) {
          for (d in dd) {
            addp(lv, paste0("uf=", u), paste0("doenca=", d), paste0("ano=", y))
            addp(lv, paste0("doenca=", d), paste0("uf=", u), paste0("ano=", y))
            addp(lv, paste0("uf=", u), paste0("ano=", y), paste0("doenca=", d))
          }
        } else {
          addp(lv, paste0("uf=", u), paste0("ano=", y))
        }
      }
    }
  }

  out <- unique(out)
  out[dir.exists(out)]
}

.ds_cache <- local({ new.env(parent = emptyenv()) })

.open_ds_cached <- function(path) {
  if (is.null(path) || !dir.exists(path)) return(NULL)
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (exists(key, envir = .ds_cache, inherits = FALSE)) return(get(key, envir = .ds_cache, inherits = FALSE))

  if (!requireNamespace("arrow", quietly = TRUE)) {
    assign(key, NULL, envir = .ds_cache)
    return(NULL)
  }

  fn <- arrow::open_dataset
  fml <- tryCatch(names(formals(fn)), error = function(e) character(0))
  args <- list(sources = path, format = "parquet")
  if ("partitioning" %in% fml) args$partitioning <- "hive"
  if ("unify_schemas" %in% fml) args$unify_schemas <- TRUE
  if ("use_threads" %in% fml) args$use_threads <- TRUE

  ds <- tryCatch(do.call(fn, args), error = function(e) NULL)
  assign(key, ds, envir = .ds_cache)
  ds
}

.read_incidence_df <- function(base, doencas, level, ano, uf = NULL, mun = NULL, keep_extra = NULL) {
  lvl <- toupper(as.character(level %||% "UF"))
  if (!lvl %in% c("BR","UF","MUN")) lvl <- "UF"
  y <- as.integer(ano)
  if (!is.finite(y)) return(data.frame())

  p <- .find_existing_dir(.part_paths(
    base, doencas, y,
    level = lvl,
    uf = if (lvl == "MUN") uf else NULL
  ))
  if (is.null(p)) return(data.frame())

  ds <- .open_ds_cached(p)
  if (is.null(ds)) return(data.frame())

  base_keep <- c(
    "uf","UF",
    "geo_id","GEO_ID",
    "cod_mun7","COD_MUN7",
    "geo_nome","GEO_NOME",
    "nome_mun","NOME_MUN",
    "casos_total","CASOS_TOTAL",
    "pop_total","POP_TOTAL",
    "casos_obitos","CASOS_OBITOS",
    "obitos_total","OBITOS_TOTAL",
    "incid_100k_total","INCID_100K_TOTAL"
  )
  if (!is.null(keep_extra)) base_keep <- unique(c(base_keep, keep_extra))

  keep <- intersect(names(ds), base_keep)
  q <- ds
  if (length(keep) > 0 && requireNamespace("dplyr", quietly = TRUE)) {
    q <- tryCatch(dplyr::select(q, dplyr::all_of(keep)), error = function(e) q)
  }

  df <- tryCatch(dplyr::collect(q), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(data.frame())
  df <- as.data.frame(df)

  if (!"uf" %in% names(df) && "UF" %in% names(df)) df$uf <- df$UF
  if ("uf" %in% names(df)) df$uf <- .to_uf(df$uf)

  id_src <- NULL
  if ("geo_id" %in% names(df)) id_src <- "geo_id"
  if (is.null(id_src) && "GEO_ID" %in% names(df)) id_src <- "GEO_ID"
  if (is.null(id_src) && "cod_mun7" %in% names(df)) id_src <- "cod_mun7"
  if (is.null(id_src) && "COD_MUN7" %in% names(df)) id_src <- "COD_MUN7"
  if (!is.null(id_src)) df$geo_id <- .to_mun7(df[[id_src]])

  if (!"geo_nome" %in% names(df)) {
    if ("GEO_NOME" %in% names(df)) df$geo_nome <- df$GEO_NOME
    if (!"geo_nome" %in% names(df) && "nome_mun" %in% names(df)) df$geo_nome <- df$nome_mun
    if (!"geo_nome" %in% names(df) && "NOME_MUN" %in% names(df)) df$geo_nome <- df$NOME_MUN
  }
  if ("geo_nome" %in% names(df)) df$geo_nome <- as.character(df$geo_nome)

  if (!"casos_total" %in% names(df) && "CASOS_TOTAL" %in% names(df)) df$casos_total <- df$CASOS_TOTAL
  if (!"pop_total" %in% names(df) && "POP_TOTAL" %in% names(df)) df$pop_total <- df$POP_TOTAL
  if (!"incid_100k_total" %in% names(df) && "INCID_100K_TOTAL" %in% names(df)) df$incid_100k_total <- df$INCID_100K_TOTAL

  if (!"casos_obitos" %in% names(df)) {
    if ("CASOS_OBITOS" %in% names(df)) df$casos_obitos <- df$CASOS_OBITOS
    if (!"casos_obitos" %in% names(df) && "obitos_total" %in% names(df)) df$casos_obitos <- df$obitos_total
    if (!"casos_obitos" %in% names(df) && "OBITOS_TOTAL" %in% names(df)) df$casos_obitos <- df$OBITOS_TOTAL
  }

  if (!"casos_total" %in% names(df)) df$casos_total <- NA_real_
  if (!"pop_total" %in% names(df)) df$pop_total <- NA_real_
  if (!"casos_obitos" %in% names(df)) df$casos_obitos <- NA_real_
  if (!"incid_100k_total" %in% names(df)) df$incid_100k_total <- NA_real_

  df$casos_total <- suppressWarnings(as.numeric(df$casos_total))
  df$pop_total <- suppressWarnings(as.numeric(df$pop_total))
  df$casos_obitos <- suppressWarnings(as.numeric(df$casos_obitos))
  df$incid_100k_total <- suppressWarnings(as.numeric(df$incid_100k_total))

  if (lvl == "UF" && !is.null(uf) && nzchar(.to_uf(uf)) && "uf" %in% names(df)) {
    df <- df[df$uf == .to_uf(uf), , drop = FALSE]
  }
  if (lvl == "MUN") {
    if (!is.null(uf) && nzchar(.to_uf(uf)) && "uf" %in% names(df)) df <- df[df$uf == .to_uf(uf), , drop = FALSE]
    if (!is.null(mun) && nzchar(.to_mun7(mun)) && "geo_id" %in% names(df)) df <- df[df$geo_id == .to_mun7(mun), , drop = FALSE]
  }

  .sanitize_df(df)
}

.scan_years_in_base <- function(base, max_dirs = 5000L) {
  if (is.null(base) || !dir.exists(base)) return(integer(0))
  dirs <- tryCatch(list.dirs(base, recursive = TRUE, full.names = TRUE), error = function(e) character(0))
  if (length(dirs) > max_dirs) dirs <- dirs[seq_len(max_dirs)]
  hits <- grep("[/\\\\](ano=|year=)\\d{4}($|[/\\\\])", dirs, value = TRUE)
  if (length(hits) == 0) return(integer(0))
  ys <- suppressWarnings(as.integer(gsub(".*(ano=|year=)(\\d{4}).*", "\\2", hits)))
  ys <- ys[is.finite(ys)]
  sort(unique(ys))
}


.source_utils_if_needed <- function() {
  if (exists(".SINAN_UTILS_LOADED", inherits = TRUE) && isTRUE(get(".SINAN_UTILS_LOADED", inherits = TRUE))) return(invisible(TRUE))


  p <- normalizePath(tryCatch(getwd(), error = function(e) "."), winslash = "/", mustWork = FALSE)
  for (i in 0:12) {
    cand1 <- file.path(p, "dashs", "shiny", "core", "R", "utils.R")
    cand2 <- file.path(p, "core", "R", "utils.R")
    cand3 <- file.path(p, "R", "utils.R")
    cand <- c(cand1, cand2, cand3)
    cand <- cand[file.exists(cand)]
    if (length(cand) > 0) {
      source(cand[[1]], local = FALSE)
      return(invisible(TRUE))
    }
    p2 <- dirname(p)
    if (identical(p2, p)) break
    p <- p2
  }

  invisible(FALSE)
}
