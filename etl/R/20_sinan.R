# etl/R/20_sinan.R
# -----------------------------------------------------------------------------
# sinan/TUBERCULOSE/<ano>.parquet  -- microdado dos casos de Recife.
#
# !!! SENSIVEL: tem nome, CNS, nome da mae, telefone, endereco e coordenadas.
# O painel le este dataset para o mapa e os graficos por bairro. Mantido
# identico ao layout antigo: colunas do SINAN em minusculas, todas texto, mais
# `doenca` no fim; um arquivo por ano de notificacao.
# -----------------------------------------------------------------------------

grava_sinan_microdado <- function(sinan, saida) {
  cols <- setdiff(names(sinan), "ano")
  base <- file.path(saida, "sinan", DOENCA)
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  for (a in sort(unique(sinan$ano))) {
    df <- sinan[sinan$ano == a, cols, drop = FALSE]
    names(df) <- tolower(names(df))
    df$doenca <- DOENCA
    sch <- arrow::schema(lapply(stats::setNames(names(df), names(df)), function(n) arrow::utf8()))
    arrow::write_parquet(arrow::arrow_table(df, schema = sch), file.path(base, paste0(a, ".parquet")))
  }
  msg(sprintf("sinan/: %d arquivos (%d-%d)", length(unique(sinan$ano)), min(sinan$ano), max(sinan$ano)))
}

grava_pop_svs <- function(pop, saida) {
  sch <- arrow::schema(ano = arrow::int32(), uf = arrow::utf8(), cod_mun = arrow::utf8(),
                       sexo = arrow::int32(), idade = arrow::int32(), pop = arrow::int32())
  for (a in sort(unique(pop$ano))) {
    grava_parquet(pop[pop$ano == a, ], file.path(saida, "popsvs"), list(year = a), "pops.parquet", sch)
  }
  msg("popsvs/: populacao de Recife por ano/sexo/idade")
}
