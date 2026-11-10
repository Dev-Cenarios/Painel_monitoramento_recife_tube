# scripts/perfil_app_real.R
# -----------------------------------------------------------------------------
# Perfila o app DE VERDADE, com todos os modulos e reativos, nao uma simulacao.
#
# Os scripts anteriores medem leitores isolados. Este liga o Rprof, sobe o app,
# e desliga sozinho depois de JANELA segundos -- tempo de abrir o painel no
# navegador. O resultado inclui tudo que a simulacao nao pega: cascata de
# reativos, renderizacao de outputs, serializacao de widgets.
#
# Uso:
#   Rscript scripts/perfil_app_real.R [porta] [janela_seg]
#   ... e abrir http://127.0.0.1:<porta> no navegador dentro da janela.
# -----------------------------------------------------------------------------

args   <- commandArgs(trailingOnly = TRUE)
PORTA  <- if (length(args) > 0) as.integer(args[[1]]) else 8123L
JANELA <- if (length(args) > 1) as.numeric(args[[2]]) else 40

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
})
SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
SAIDA <- file.path(SHINY_DIR, "logs", "perfil_app_real.txt")
dir.create(dirname(SAIDA), recursive = TRUE, showWarnings = FALSE)

prof <- tempfile(fileext = ".out")

suppressPackageStartupMessages({ library(shiny); library(later) })

later::later(function() {
  Rprof(NULL)
  s <- tryCatch(summaryRprof(prof), error = function(e) NULL)
  con <- file(SAIDA, open = "wt", encoding = "UTF-8")
  on.exit(close(con))

  if (is.null(s)) { writeLines("sem amostras", con); return(invisible(NULL)) }

  writeLines(sprintf("tempo amostrado: %.2f s", s$sampling.time), con)

  writeLines("\n== POR SELF TIME (onde a CPU esteve) ==", con)
  st <- s$by.self; st <- utils::head(st[order(-st$self.time), , drop = FALSE], 25)
  for (i in seq_len(nrow(st))) {
    writeLines(sprintf("%-46s %7.2fs %6.1f%%", substr(rownames(st)[i], 1, 46),
                       st$self.time[i], st$self.pct[i]), con)
  }

  writeLines("\n== FUNCOES DO PROJETO (total time) ==", con)
  tt <- s$by.total
  meus <- grep("^\\.(core_|kpi|ec_|read_|charts_|tb_|pyr_|plot_|std_|inc_|ts_|rank_|df_|open_|prep_|aggregate_|simplify|sf_fix|find_|query_)|^mod_|^core_",
               rownames(tt), value = TRUE)
  tt <- tt[meus, , drop = FALSE]
  tt <- utils::head(tt[order(-tt$total.time), , drop = FALSE], 30)
  for (i in seq_len(nrow(tt))) {
    writeLines(sprintf("%-46s %7.2fs %6.1f%%", substr(rownames(tt)[i], 1, 46),
                       tt$total.time[i], tt$total.pct[i]), con)
  }
  cat("[perfil] gravado em", SAIDA, "\n")
}, delay = JANELA)

cat("[perfil] Rprof ligado. Abra http://127.0.0.1:", PORTA,
    " -- janela de ", JANELA, "s\n", sep = "")
Rprof(prof, interval = 0.005, line.profiling = FALSE)

shiny::runApp(file.path(SHINY_DIR, "core", "apps", "TUBERCULOSE"),
              host = "127.0.0.1", port = PORTA, launch.browser = FALSE)
