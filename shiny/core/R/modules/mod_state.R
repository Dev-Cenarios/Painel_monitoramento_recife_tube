if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

mod_state_ui <- function(id, cfg = NULL, pack = NULL, ...) {
  ns <- shiny::NS(id)
  cfg_obj <- if (is.function(cfg)) tryCatch(cfg(), error = function(e) NULL) else cfg
  lock_recife <- isTRUE(cfg_obj$LOCK_RECIFE %||% cfg_obj$lock_recife %||% FALSE)

  if (isTRUE(lock_recife)) {
    return(shiny::tagList())
  }

  shiny::tagList(
    shiny::tags$div(
      style = "display:flex; flex-direction:column; gap:10px;",
      shiny::tags$div(
        style = "font-size:12px; opacity:.75;",
        "Navegação: clique no mapa para BR → UF → MUN. Use Voltar/Reset quando necessário."
      ),
      shiny::tags$div(
        style = "display:flex; gap:8px; flex-wrap:wrap;",
        shiny::actionButton(ns("btn_back"), "Voltar", class = "btn-outline-secondary"),
        shiny::actionButton(ns("btn_reset"), "Reset", class = "btn-outline-danger")
      ),
      shiny::tags$div(
        style = "font-size:12px; opacity:.85;",
        shiny::textOutput(ns("crumb"))
      )
    )
  )
}

mod_state_server <- function(id, nav, ano, data_api = NULL, pack = NULL, cfg = NULL, ...) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg_obj <- if (is.function(cfg)) tryCatch(cfg(), error = function(e) NULL) else cfg
    lock_recife <- isTRUE(cfg_obj$LOCK_RECIFE %||% cfg_obj$lock_recife %||% FALSE)
    lock_mun7 <- as.character(cfg_obj$RECIFE_MUN7 %||% cfg_obj$recife_mun7 %||% "2611606")

    output$crumb <- shiny::renderText({
      lvl <- nav$level %||% "UF"
      y <- suppressWarnings(as.integer(ano()))
      y <- if (is.finite(y)) y else NA_integer_
      yr_txt <- if (!is.na(y)) paste0(" \u2022 Ano: ", y) else ""

      rv <- nav$region_view %||% "MUN"
      sel_macro <- nav$selected_macro %||% NULL
      sel_micro <- nav$selected_micro %||% NULL
      mun_val <- nav$mun %||% "\u2014"
      mun_label <- if (isTRUE(lock_recife) && identical(as.character(mun_val), lock_mun7)) "Recife" else mun_val

      if (lvl == "BR") return(paste0("Escopo: Brasil", yr_txt))
      if (lvl == "UF") {
        base <- paste0("Escopo: UF ", nav$uf %||% "\u2014")
        if (identical(rv, "MACRO")) base <- paste0(base, " \u2022 Macrorregi\u00f5es")
        if (identical(rv, "MICRO") && !is.null(sel_macro)) base <- paste0(base, " \u2022 Macro: ", sel_macro, " \u2022 Microrregi\u00f5es")
        else if (identical(rv, "MICRO")) base <- paste0(base, " \u2022 Microrregi\u00f5es")
        if (identical(rv, "MUN") && !is.null(sel_micro)) base <- paste0(base, " \u2022 Micro: ", sel_micro, " \u2022 Munic\u00edpios")
        return(paste0(base, yr_txt))
      }
      paste0("Escopo: ", nav$uf %||% "\u2014", " \u2022 Munic\u00edpio: ", mun_label, if (isTRUE(nav$detail)) " (detalhe)" else "", yr_txt)
    })

    shiny::observeEvent(input$btn_reset, {
      if (isTRUE(lock_recife)) return()
      nav$level <- "BR"
      nav$uf <- NULL
      nav$mun <- NULL
      nav$detail <- FALSE
      nav$region_view <- "MUN"
      nav$selected_macro <- NULL
      nav$selected_micro <- NULL
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_back, {
      if (isTRUE(lock_recife)) return()
      if ((nav$level %||% "UF") == "MUN" && isTRUE(nav$detail)) { nav$detail <- FALSE; return() }
      if ((nav$level %||% "UF") == "MUN") {
        nav$level <- "UF"; nav$mun <- NULL; nav$detail <- FALSE
        # If we drilled from micro → mun, step back to micro
        if (!is.null(nav$selected_micro)) {
          nav$selected_micro <- NULL
          nav$region_view <- "MICRO"
        }
        return()
      }
      if ((nav$level %||% "UF") == "UF") {
        rv <- nav$region_view %||% "MUN"
        # If viewing filtered micros, step back to macro
        if (identical(rv, "MICRO") && !is.null(nav$selected_macro)) {
          nav$selected_macro <- NULL
          nav$region_view <- "MACRO"
          return()
        }
        nav$level <- "BR"; nav$uf <- NULL; nav$mun <- NULL; nav$detail <- FALSE
        nav$region_view <- "MUN"; nav$selected_macro <- NULL; nav$selected_micro <- NULL
        return()
      }
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
