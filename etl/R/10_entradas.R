# etl/R/10_entradas.R
# -----------------------------------------------------------------------------
# Leitura dos insumos brutos. Cada funcao devolve um data.frame em memoria;
# nada aqui grava em disco.
# -----------------------------------------------------------------------------

# SINAN-TB de Recife: export do SINAN Net ja com geocodificacao (colunas
# address, LATITUDE, LONGITUDE, DSC_LOCALIDADE...). CONTEM DADO PESSOAL.
ler_sinan <- function(cam, ano_max) {
  msg("SINAN: lendo", basename(cam$sinan_csv))
  raw <- ler_csv_texto(cam$sinan_csv)
  n0 <- nrow(raw)
  ok <- raw$TRATAMENTO %in% SINAN_TRATAMENTO_OK & !(raw$SITUA_ENCE %in% SINAN_SITUA_EXCLUI)
  df <- raw[ok, , drop = FALSE]
  # Campo vazio vira nulo; o texto "NA" do export e mantido (layout antigo).
  for (cn in names(df)) { v <- df[[cn]]; v[!is.na(v) & v == ""] <- NA_character_; df[[cn]] <- v }
  df$ano <- suppressWarnings(as.integer(df$NU_ANO))
  df <- df[!is.na(df$ano) & df$ano >= ANO_MIN & df$ano <= ano_max, , drop = FALSE]
  msg(sprintf("SINAN: %d registros lidos, %d casos apos filtros (transferencia, mudanca de diagnostico, %d-%d)",
              n0, nrow(df), ANO_MIN, ano_max))
  df
}

# SIM de Recife: declaracoes de obito de residentes com TB em qualquer linha.
# Para o indicador conta so a causa basica A15-A19. CONTEM DADO PESSOAL.
ler_sim_tb <- function(cam, ano_max) {
  msg("SIM: lendo", basename(cam$sim_csv))
  raw <- ler_csv_texto(cam$sim_csv)
  raw$ano <- suppressWarnings(as.integer(substr(raw$DTOBITO, 5, 8)))  # DTOBITO = ddmmaaaa
  raw$mes <- suppressWarnings(as.integer(substr(raw$DTOBITO, 3, 4)))
  tb <- raw[substr(raw$CAUSABAS, 1, 3) %in% CID_TB & raw$CODMUNRES == RECIFE$cod_mun6 &
              !is.na(raw$ano) & raw$ano >= ANO_MIN & raw$ano <= ano_max, , drop = FALSE]
  tb$sexo <- ifelse(tb$SEXO %in% c("M", "1"), "M", ifelse(tb$SEXO %in% c("F", "2"), "F", "I"))
  tb$idade <- idade_anos(tb$IDADE, "SIM")
  msg(sprintf("SIM: %d declaracoes, %d obitos com causa basica TB em Recife", nrow(raw), nrow(tb)))
  tb
}

# Populacao SVS/DATASUS por municipio, ano, sexo e idade simples (POPSBR<aa>.zip
# -> pop<aa>.dbf). Devolve so Recife.
ler_pop_svs <- function(cam, anos) {
  out <- list()
  for (a in anos) {
    z <- file.path(cam$pop_svs_dir, sprintf("POPSBR%02d.zip", a %% 100))
    if (!file.exists(z)) stop("Populacao SVS nao encontrada para ", a, ": ", z, call. = FALSE)
    td <- tempfile("pop"); dir.create(td)
    nm <- utils::unzip(z, list = TRUE)$Name
    utils::unzip(z, files = nm[grepl("[.]dbf$", nm, ignore.case = TRUE)][1], exdir = td)
    dbf <- list.files(td, pattern = "[.]dbf$", ignore.case = TRUE, full.names = TRUE)[1]
    x <- foreign::read.dbf(dbf, as.is = TRUE)
    names(x) <- toupper(names(x))
    x <- x[x$COD_MUN %in% c(RECIFE$cod_mun7, RECIFE$cod_mun6), , drop = FALSE]
    unlink(td, recursive = TRUE)
    out[[length(out) + 1]] <- data.frame(
      ano = as.integer(a), uf = RECIFE$uf, cod_mun = RECIFE$cod_mun7,
      sexo = as.integer(x$SEXO), idade = pmin(as.integer(x$IDADE), 80L), pop = as.integer(x$POP),
      stringsAsFactors = FALSE
    )
  }
  pop <- do.call(rbind, out)
  pop <- stats::aggregate(pop ~ ano + uf + cod_mun + sexo + idade, data = pop, FUN = sum)
  pop <- pop[order(pop$ano, pop$sexo, pop$idade), ]
  msg(sprintf("Populacao SVS: Recife, %d-%d", min(anos), max(anos)))
  pop
}

# Populacao estimada por bairro (Censo 2010/2022 + interpolacao geometrica).
ler_pop_bairro <- function(cam) {
  pb <- utils::read.csv(cam$pop_bairro_csv, encoding = "UTF-8", check.names = FALSE, stringsAsFactors = FALSE)
  names(pb)[1] <- "bairro"                       # remove o BOM do cabecalho
  pb$bairro_key <- chave_bairro(pb$bairro)
  pb[c("bairro_key", "ano", "populacao")]
}

ler_referencias <- function(cam) {
  r <- function(f) utils::read.csv(file.path(cam$ref, f), colClasses = "character", encoding = "UTF-8",
                                   na.strings = "", stringsAsFactors = FALSE, check.names = FALSE)
  num <- function(df, cols) { for (c in cols) df[[c]] <- as.numeric(df[[c]]); df }
  list(
    rotulos  = r("rotulos_sinan.csv"),
    bairros  = r("bairros_recife.csv"),
    obitos_bairro = num(r("obitos_bairro.csv"), c("ano", "casos_obitos")),
    br_anual = num(r("nacional_anual.csv"), c("ano", "casos_total", "casos_obitos", "obitos_M", "obitos_F", "casos_cura",
                                              "cura_M", "cura_F", "casos_M", "casos_F", "pop_total", "pop_M", "pop_F")),
    br_mes   = num(r("nacional_mensal.csv"), c("ano", "mes", "casos", "casos_obitos", "casos_cura")),
    br_obit  = num(r("nacional_obitos.csv"), c("ano", "obitos_sim", "obitos_M", "obitos_F")),
    centroides = num(r("municipios_centroids.csv"), c("lon", "lat"))
  )
}
