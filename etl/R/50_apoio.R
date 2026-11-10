# etl/R/50_apoio.R
# -----------------------------------------------------------------------------
# dim_geo/municipios_centroids/uf=<UF>/   centroides dos municipios (mapa BR/UF)
# meta/doenca=TUBERCULOSE/freshness.parquet   selo "dados ate" do painel
# -----------------------------------------------------------------------------

grava_dim_geo <- function(ref, saida) {
  g <- ref$centroides
  sch <- arrow::schema(cod_mun7 = arrow::utf8(), nome_mun = arrow::utf8(), lon = arrow::float64(), lat = arrow::float64())
  for (u in sort(unique(g$uf))) {
    grava_parquet(g[g$uf == u, c("cod_mun7", "nome_mun", "lon", "lat")],
                  file.path(saida, "dim_geo", "municipios_centroids"), list(uf = u), "part-0.parquet", sch)
  }
  msg(sprintf("dim_geo/: %d municipios", nrow(g)))
}

grava_meta <- function(ano_max, saida) {
  d <- data.frame(doenca = DOENCA, dados_ate = as.Date(sprintf("%d-12-31", ano_max)),
                  processado_em = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), tz = "UTC"))
  sch <- arrow::schema(doenca = arrow::utf8(), dados_ate = arrow::date32(), processado_em = arrow::timestamp("us"))
  grava_parquet(d, file.path(saida, "meta"), list(doenca = DOENCA), "freshness.parquet", sch)
  msg(sprintf("meta/: dados ate %d-12-31", ano_max))
}
