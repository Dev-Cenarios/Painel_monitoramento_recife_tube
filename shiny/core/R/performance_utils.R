# dashs/shiny/core/R/performance_utils.R
# Utilitários para otimização de performance e renderização

# Debounce reativo com controle fino
# Evita múltiplos renders quando o usuário está interagindo (ex: arrastar slider)
.reactive_debounce <- function(expr, millis = 300L) {
  shiny::debounce(shiny::reactive(expr), millis = as.integer(millis))
}

# Throttle reativo - garante que expressão é avaliada no máximo a cada N ms
# Útil para eventos de scroll/resize
.reactive_throttle <- function(expr, millis = 500L) {
  shiny::throttle(shiny::reactive(expr), millis = as.integer(millis))
}

# Output com debounce automático para renderUI pesados
# Evita flickers e re-renders desnecessários durante arrastamento de slider
.render_ui_debounced <- function(expr, millis = 350L) {
  function() {
    .reactive_debounce(expr, millis = millis)()
  }
}

# Cache compartilhado entre módulos para dados de parquet
# Reduz I/O quando múltiplos módulos consultam o mesmo ano
.get_shared_data_cache <- function() {
  if (!exists(".SINAN_DATA_CACHE", inherits = TRUE)) {
    assign(".SINAN_DATA_CACHE", new.env(parent = emptyenv()), envir = globalenv())
  }
  get(".SINAN_DATA_CACHE", inherits = TRUE)
}

# Limpa cache compartilhado (útil para debug ou quando dados mudarem)
.clear_shared_data_cache <- function() {
  cache <- .get_shared_data_cache()
  rm(list = ls(envir = cache), envir = cache)
  invisible(TRUE)
}

# Chave de cache com namespace para evitar colisões
# Exemplo: .cache_key("kpi", "uf", 2023) -> "kpi|uf|2023"
.cache_key <- function(...) {
  parts <- as.character(c(...))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = "|")
}

# Recupera valor do cache compartilhado
.cache_get <- function(key, default = NULL) {
  cache <- .get_shared_data_cache()
  if (exists(key, envir = cache, inherits = FALSE)) {
    return(get(key, envir = cache, inherits = FALSE))
  }
  default
}

# Armazena valor no cache compartilhado
.cache_set <- function(key, value) {
  cache <- .get_shared_data_cache()
  assign(key, value, envir = cache)
  invisible(value)
}

# Recupera ou computa valor com cache
# .cache_get_or_compute("ts_data_2023", function() load_slow_data(2023))
.cache_get_or_compute <- function(key, compute_fn) {
  cached <- .cache_get(key)
  if (!is.null(cached)) return(cached)
  
  value <- tryCatch(compute_fn(), error = function(e) NULL)
  if (!is.null(value)) {
    .cache_set(key, value)
  }
  value
}

# Observador com debounce - útil para reatividade de formulário
# Evita disparar múltiplas vezes enquanto usuário está interagindo
.observe_debounced <- function(expr, millis = 350L, session = NULL) {
  debounced_val <- shiny::debounce(shiny::reactive(TRUE), millis = as.integer(millis))
  
  shiny::observe({
    debounced_val()
    expr()
  })
}

# Condição que verifica se cache está "quente" (recentemente atualizado)
# Útil para invalidar cache após certo tempo
.cache_is_warm <- function(key, max_age_seconds = 300L) {
  cache <- .get_shared_data_cache()
  ts_key <- paste0(key, "|ts")
  
  if (!exists(ts_key, envir = cache, inherits = FALSE)) return(FALSE)
  
  ts <- get(ts_key, envir = cache, inherits = FALSE)
  age <- as.numeric(Sys.time()) - as.numeric(ts)
  age < as.numeric(max_age_seconds)
}

# Armazena valor com timestamp para invalidação automática
.cache_set_with_ttl <- function(key, value, ttl_seconds = 300L) {
  cache <- .get_shared_data_cache()
  assign(key, value, envir = cache)
  assign(paste0(key, "|ts"), Sys.time(), envir = cache)
  invisible(value)
}

# Recupera valor do cache se ainda estiver válido (TTL)
.cache_get_if_warm <- function(key, default = NULL, max_age_seconds = 300L) {
  if (.cache_is_warm(key, max_age_seconds)) {
    return(.cache_get(key, default))
  }
  default
}

# Wrapper para função que carrega dados com cache automático
# Útil para mod_data.R
.memoize_data_loader <- function(fn, ttl_seconds = 300L) {
  function(...) {
    args <- list(...)
    cache_key <- .cache_key("data", paste(names(args), args, sep="=", collapse="|"))
    
    .cache_get_or_compute(
      cache_key,
      function() {
        result <- tryCatch(do.call(fn, args), error = function(e) NULL)
        if (!is.null(result)) {
          .cache_set_with_ttl(cache_key, result, ttl_seconds)
        }
        result
      }
    )
  }
}
