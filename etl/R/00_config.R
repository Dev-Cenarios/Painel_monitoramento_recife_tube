# etl/R/00_config.R
# -----------------------------------------------------------------------------
# Constantes e caminhos do ETL. Tudo o que muda de uma carga para outra esta
# aqui ou vem da linha de comando de etl/run_etl.R.
# -----------------------------------------------------------------------------

DOENCA <- "TUBERCULOSE"

# Recife
RECIFE <- list(
  uf        = "PE",
  cod_uf    = "26",
  cod_mun7  = "2611606",
  cod_mun6  = "261160",
  nome_mun  = "Recife - PE",
  nome_uf   = "RECIFE"   # o painel antigo gravava assim o geo_nome do nivel UF
)

ANO_MIN <- 2010L

# Regras de selecao dos casos do SINAN (descobertas por engenharia reversa
# dos parquets de 2010-2023; reproduzem exatamente os 27.580 casos):
#   - TRATAMENTO em 1,2,3,4,6  -> exclui 5 (transferencia) e registros sem tipo
#   - SITUA_ENCE diferente de 6 -> exclui "mudanca de diagnostico"
SINAN_TRATAMENTO_OK <- c("1", "2", "3", "4", "6")
SINAN_SITUA_EXCLUI  <- c("6")

# Codigos de SITUA_ENCE usados nos indicadores
SIT_CURA      <- "1"
SIT_ABANDONO  <- "2"
SIT_OBITO_TB  <- "3"

# Obito por tuberculose no SIM: causa basica A15-A19
CID_TB <- sprintf("A%02d", 15:19)

# Faixas etarias das piramides (inicio de cada faixa)
FAIXAS <- data.frame(
  faixa_ord    = c(0L, 5L, 10L, 15L, 20L, 30L, 40L, 50L, 60L, 70L, 80L),
  faixa_etaria = c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                   "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                   "60 a 69 anos", "70 a 79 anos", "80 anos ou mais"),
  stringsAsFactors = FALSE
)

# Variaveis do SINAN contadas em sinan_landing/ e sinan_dict/ (graficos de
# composicao). Mesmas 42 do ETL antigo.
COMPOSICAO_VARS <- c(
  "AGRAVAIDS", "AGRAVALCOO", "AGRAVDIABE", "AGRAVDOENC", "AGRAVDROGA", "AGRAVOUTDE", "AGRAVOUTRA",
  "AGRAVTABAC", "BACILOSC_1", "BACILOSC_2", "BACILOSC_3", "BACILOSC_4", "BACILOSC_5", "BACILOSC_6",
  "BACILOSC_E", "BENEF_GOV", "CS_ESCOL_N", "CS_GESTANT", "CS_RACA", "CS_ZONA", "CULTURA_ES",
  "DOENCA_TRA", "EXTRAPUL_O", "FLXRECEBI", "FORMA", "HISTOPATOL", "HIV", "IDENT_MICR", "IN_VINCULA",
  "MIGRADO_W", "MUN_TRANSF", "NDUPLIC_N", "NU_COMU_EX", "POP_IMIG", "POP_LIBER", "POP_RUA",
  "POP_SAUDE", "RAIOX_TORA", "SITUA_ENCE", "TRANSF", "TRATAMENTO", "TRATSUP_AT"
)

MESES <-c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho",
           "agosto", "setembro", "outubro", "novembro", "dezembro")

# Grafias do SINAN/SIM que nao batem com o nome oficial do bairro.
# chave normalizada encontrada no dado -> chave oficial (etl/ref/bairros_recife.csv)
BAIRRO_APELIDOS <- c(
  "BAIRRO DO RECIFE"   = "RECIFE",
  "POCO DA PANELA"     = "POCO",
  "ALTO STA TEREZINHA" = "ALTO SANTA TEREZINHA"   # grafia de pop_bairro_estimado.csv
)

etl_caminhos <- function(raiz, saida = NULL) {
  dados <- file.path(raiz, "data")
  list(
    raiz            = raiz,
    ref             = file.path(raiz, "etl", "ref"),
    sinan_csv       = file.path(dados, "csv", "SINAN_RECIFE_TB.csv"),
    sim_csv         = file.path(dados, "csv", "SIM_RECIFE_TB.csv"),
    pop_bairro_csv  = file.path(dados, "csv", "pop_bairro_estimado.csv"),
    pop_svs_dir     = file.path(dados, "parquet", "popsvs"),   # POPSBR<aa>.zip
    saida           = saida %||% file.path(dados, "parquet_novo")
  )
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
