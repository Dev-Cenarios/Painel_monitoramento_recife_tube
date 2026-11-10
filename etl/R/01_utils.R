# etl/R/01_utils.R
# -----------------------------------------------------------------------------
# Leitura, escrita e pequenas transformacoes compartilhadas.
# -----------------------------------------------------------------------------

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", ..., "\n")

# CSV do DATASUS exportado em UTF-8. Tudo como texto e "NA" mantido como texto:
# e assim que o painel antigo gravava o microdado (o parquet tem "NA" literal).
ler_csv_texto <- function(caminho, sep = ",") {
  if (!file.exists(caminho)) stop("Arquivo de entrada nao encontrado: ", caminho, call. = FALSE)
  utils::read.csv(caminho, sep = sep, colClasses = "character", check.names = FALSE,
                  encoding = "UTF-8", na.strings = character(0))
}

# Valor "vazio" no microdado: NA de verdade, "" ou o texto "NA".
vazio <- function(x) is.na(x) | x == "" | x == "NA"

# Chave de comparacao de nomes (bairro, municipio): sem acento, maiuscula,
# so letras/digitos separados por um espaco.
chave_nome <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

chave_bairro <- function(x) {
  k <- chave_nome(x)
  hit <- !is.na(k) & k %in% names(BAIRRO_APELIDOS)
  k[hit] <- unname(BAIRRO_APELIDOS[k[hit]])
  k
}

# "dd/mm/aaaa" (SINAN) -> ano / mes
ano_de_data <- function(x) suppressWarnings(as.integer(substr(x, 7, 10)))
mes_de_data <- function(x) suppressWarnings(as.integer(substr(x, 4, 5)))

# Idade em anos a partir do codigo do SINAN (NU_IDADE_N, 4 digitos) ou do SIM
# (IDADE, 3 digitos). Primeiro digito = unidade: 4 = anos, 5 = anos + 100
# (SIM), 0-3 = horas/dias/meses (menos de 1 ano).
idade_anos <- function(cod, sistema = c("SINAN", "SIM")) {
  sistema <- match.arg(sistema)
  cod <- as.character(cod)
  cod[vazio(cod)] <- NA_character_
  un <- suppressWarnings(as.integer(substr(cod, 1, 1)))
  v  <- suppressWarnings(as.integer(substr(cod, 2, nchar(cod))))
  out <- rep(NA_integer_, length(cod))
  out[!is.na(un) & un %in% 0:3] <- 0L
  out[!is.na(un) & un == 4] <- v[!is.na(un) & un == 4]
  if (sistema == "SIM") out[!is.na(un) & un == 5] <- 100L + v[!is.na(un) & un == 5]
  out
}

faixa_de_idade <- function(idade) {
  ord <- FAIXAS$faixa_ord
  i <- findInterval(idade, ord)
  out <- rep(NA_integer_, length(idade))
  ok <- !is.na(idade) & idade >= 0 & i >= 1
  out[ok] <- ord[i[ok]]
  out
}

# Grava um data.frame como dataset Parquet particionado no estilo Hive:
#   <base>/<k1>=<v1>/<k2>=<v2>/<arquivo>
# `particao` e uma lista nomeada (ordem = ordem das pastas). As colunas de
# particao continuam DENTRO do arquivo quando `manter` as listar, como no
# layout antigo (ex.: `ano` aparece na pasta e no arquivo).
grava_parquet <- function(df, base, particao = list(), arquivo = "part-00000.parquet", schema = NULL) {
  dir <- base
  for (k in names(particao)) dir <- file.path(dir, paste0(k, "=", particao[[k]]))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tab <- if (is.null(schema)) arrow::arrow_table(df) else arrow::arrow_table(df, schema = schema)
  arrow::write_parquet(tab, file.path(dir, arquivo))
  invisible(file.path(dir, arquivo))
}

# num/den*por. Denominador ausente -> NA; denominador zero -> `se_zero`.
taxa <- function(num, den, por = 1e5, se_zero = NA_real_) {
  n <- max(length(num), length(den))
  num <- rep_len(as.numeric(num), n); den <- rep_len(as.numeric(den), n)
  out <- num / den * por
  out[!is.na(den) & den == 0] <- se_zero
  out[is.na(den)] <- NA_real_
  out
}
