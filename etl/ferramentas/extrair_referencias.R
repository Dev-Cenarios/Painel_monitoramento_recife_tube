# etl/ferramentas/extrair_referencias.R
# -----------------------------------------------------------------------------
# Congela, a partir dos parquets atuais, as tabelas de referencia que o ETL
# precisa e que NAO podem ser recalculadas com os insumos brutos desta pasta.
# Rodado UMA VEZ (em 2026-11) para criar etl/ref/. Fica versionado para
# documentar a origem de cada arquivo de etl/ref/.
#
# Tudo o que sai daqui e agregado ou dicionario. Nenhuma linha identifica
# paciente:
#   rotulos_sinan.csv        dicionario de dados do SINAN-TB (variavel, codigo, rotulo)
#   bairros_recife.csv       os 94 bairros: chave, nome de exibicao
#   obitos_bairro.csv        obitos por bairro e ano, congelados do ETL antigo
#   nacional_anual.csv       Brasil por ano (casos, curas, populacao) -- veio do
#                            SINAN nacional do DATASUS, que nao esta nesta pasta
#   nacional_mensal.csv      Brasil por mes (casos, obitos, curas)
#   nacional_obitos.csv      Brasil por ano, obitos do SIM
#   municipios_centroids.csv centroides dos 5.570 municipios (lon/lat)
#
# Uso (da raiz do projeto):
#   Rscript etl/ferramentas/extrair_referencias.R [data/parquet] [ano_max]
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(arrow); library(dplyr) })

args    <- commandArgs(trailingOnly = TRUE)
PARQ    <- if (length(args) >= 1) args[[1]] else "data/parquet"
ANO_MAX <- if (length(args) >= 2) as.integer(args[[2]]) else 2023L
REF     <- file.path("etl", "ref")
dir.create(REF, recursive = TRUE, showWarnings = FALSE)

ler <- function(sub) {
  fs <- list.files(file.path(PARQ, sub), pattern = "[.]parquet$", recursive = TRUE, full.names = TRUE)
  bind_rows(lapply(fs, function(f) {
    df <- as.data.frame(read_parquet(f))
    kv <- regmatches(f, gregexpr("[A-Za-z_]+=[^/]+", f))[[1]]
    for (k in kv) { a <- strsplit(k, "=")[[1]]; if (!a[1] %in% names(df)) df[[a[1]]] <- a[2] }
    for (n in names(df)) if (is.factor(df[[n]])) df[[n]] <- as.character(df[[n]])
    if ("ano" %in% names(df)) df$ano <- as.integer(as.character(df$ano))
    df
  }))
}
grava <- function(df, nome) {
  write.csv(df, file.path(REF, nome), row.names = FALSE, fileEncoding = "UTF-8", na = "")
  cat(sprintf("  %-26s %6d linhas\n", nome, nrow(df)))
}

cat("Extraindo referencias de", PARQ, "\n")

# Dicionario: so (variavel, valor, rotulo) dos codigos que TEM rotulo.
# Os sem rotulo ficam de fora de proposito: incluem campos de texto livre
# (AGRAVOUTDE, EXTRAPUL_O) digitados na notificacao, que podem identificar
# alguem e nao podem ir para o git. Contagens sao recalculadas pelo ETL.
d <- ler("sinan_dict")
grava(d |> filter(!is.na(valor), !is.na(valor_lbl)) |> distinct(variavel, valor, valor_lbl) |> arrange(variavel, valor),
      "rotulos_sinan.csv")

b <- ler("incidence/doenca=TUBERCULOSE/nivel=BAIRRO")
grava(b |> distinct(bairro_key, bairro_nome) |> arrange(bairro_key), "bairros_recife.csv")

# Obitos por bairro: a regra do ETL antigo nao foi descoberta. Por decisao do
# projeto (11/2026), os numeros de 2010-ANO_MAX ficam congelados como estavam.
# So contagem por bairro/ano -- o mesmo numero que o painel ja publica.
grava(b |> filter(ano <= ANO_MAX) |> select(ano, bairro_key, casos_obitos) |> arrange(ano, bairro_key), "obitos_bairro.csv")

inc_br <- ler("incidence/doenca=TUBERCULOSE/nivel=BR") |> filter(ano <= ANO_MAX)
grava(inc_br |> select(ano, casos_total, casos_obitos, obitos_M, obitos_F, casos_cura, cura_M, cura_F,
                       casos_M, casos_F, pop_total, pop_M, pop_F) |> arrange(ano), "nacional_anual.csv")

ts_br <- ler("_cache_ts/nivel=BR") |> filter(ano <= ANO_MAX)
grava(ts_br |> select(ano, mes, casos, casos_obitos, casos_cura) |> arrange(ano, mes), "nacional_mensal.csv")

ob_br <- ler("obitos/doenca=TUBERCULOSE/nivel=BR") |> filter(ano <= ANO_MAX)
grava(ob_br |> select(ano, obitos_sim, obitos_M, obitos_F) |> arrange(ano), "nacional_obitos.csv")

geo <- ler("dim_geo/municipios_centroids")
grava(geo |> select(uf, cod_mun7, nome_mun, lon, lat) |> arrange(uf, cod_mun7), "municipios_centroids.csv")

cat("Pronto.\n")
