# etl/R/40_composicao.R
# -----------------------------------------------------------------------------
# sinan_landing/  contagem de casos por variavel x valor x sexo, por ano
#                 (graficos de composicao)
# sinan_dict/     dicionario: rotulo de cada codigo e em que anos aparece
#
# As variaveis estao em COMPOSICAO_VARS (00_config.R); os rotulos vem de
# etl/ref/rotulos_sinan.csv (dicionario do SINAN-TB).
# ATENCAO: AGRAVOUTDE e EXTRAPUL_O sao texto livre da notificacao; o ETL antigo
# os contava assim mesmo e o painel os le. Como estes datasets ficam em data/
# (fora do git), isso nao vaza -- mas vale revisar se devem continuar aqui. Os niveis BR, UF e MUN repetem as contagens de Recife, como no
# ETL antigo.
# -----------------------------------------------------------------------------

grava_composicao <- function(sinan, ref, saida) {
  rot <- ref$rotulos
  vars <- sort(COMPOSICAO_VARS)
  falta <- setdiff(vars, names(sinan))
  if (length(falta)) stop("Variaveis do dicionario ausentes no SINAN: ", paste(falta, collapse = ", "), call. = FALSE)
  lbl <- function(v, x) rot$valor_lbl[match(paste(v, x), paste(rot$variavel, rot$valor))]

  # long: uma linha por caso x variavel
  long <- do.call(rbind, lapply(vars, function(v) data.frame(ano = sinan$ano, sexo = sinan$CS_SEXO, variavel = v,
                                                             valor = sinan[[v]], stringsAsFactors = FALSE)))
  long <- long[!is.na(long$valor), ]   # campo vazio nao entra; o texto "NA" entra (codigo "NA")
  por_sexo <- stats::aggregate(list(n = rep(1L, nrow(long))), long[c("ano", "sexo", "variavel", "valor")], FUN = sum)
  total    <- stats::aggregate(list(n = rep(1L, nrow(long))), long[c("ano", "variavel", "valor")], FUN = sum)
  total$sexo <- "TOTAL"
  cont <- rbind(por_sexo, total[names(por_sexo)])
  cont$valor_lbl <- lbl(cont$variavel, cont$valor)

  # Variaveis codificadas (as que tem rotulo no dicionario) com codigo novo.
  # Campos de texto livre (AGRAVOUTDE, EXTRAPUL_O...) nao tem rotulo e ficam de fora do aviso.
  codificadas <- unique(rot$variavel[!is.na(rot$valor_lbl)])
  sem_rotulo <- unique(cont[is.na(cont$valor_lbl) & cont$valor != "NA" & cont$variavel %in% codificadas, c("variavel", "valor")])
  if (nrow(sem_rotulo)) msg(sprintf("AVISO: %d codigo(s) sem rotulo em etl/ref/rotulos_sinan.csv, nas variaveis: %s",
                                    nrow(sem_rotulo), paste(unique(sem_rotulo$variavel), collapse = ", ")))

  sch <- arrow::schema(doenca = arrow::utf8(), nivel = arrow::utf8(), geo_id = arrow::utf8(), uf = arrow::utf8(),
                       geo_nome = arrow::utf8(), sexo = arrow::utf8(), variavel = arrow::utf8(), valor = arrow::utf8(),
                       n = arrow::int64(), valor_lbl = arrow::utf8(), ano = arrow::int32())
  geos <- list(BR = c("BR", "BR", "Brasil"), UF = c(RECIFE$uf, RECIFE$uf, RECIFE$uf),
               MUN = c(RECIFE$cod_mun6, RECIFE$uf, RECIFE$nome_mun))
  for (a in sort(unique(cont$ano))) {
    x <- cont[cont$ano == a, ]
    x <- x[order(x$variavel, x$sexo, x$valor), ]
    for (nv in names(geos)) {
      g <- geos[[nv]]
      d <- data.frame(doenca = DOENCA, nivel = nv, geo_id = g[1], uf = g[2], geo_nome = g[3], x[c("sexo", "variavel", "valor", "n", "valor_lbl", "ano")])
      grava_parquet(d, file.path(saida, "sinan_landing"), list(doenca = DOENCA, nivel = nv, ano = a), schema = sch)
    }
  }
  msg(sprintf("sinan_landing/: %d variaveis, BR/UF/MUN", length(vars)))

  # dicionario
  t <- stats::aggregate(list(n_total = total$n), total[c("variavel", "valor")], FUN = sum)
  anos <- stats::aggregate(list(ano = total$ano), total[c("variavel", "valor")], FUN = function(z) sort(unique(z)), simplify = FALSE)
  t$anos <- anos$ano[match(paste(t$variavel, t$valor), paste(anos$variavel, anos$valor))]
  t$n_years <- lengths(t$anos)
  t$valor_lbl <- lbl(t$variavel, t$valor)
  t <- t[order(t$variavel, t$valor), ]
  dict <- data.frame(doenca = DOENCA, variavel = t$variavel, valor = t$valor, valor_lbl = t$valor_lbl, stringsAsFactors = FALSE)
  dict$anos <- I(lapply(t$anos, as.integer))
  dict$n_total <- t$n_total
  dict$n_years <- as.integer(t$n_years)
  sch_d <- arrow::schema(doenca = arrow::utf8(), variavel = arrow::utf8(), valor = arrow::utf8(), valor_lbl = arrow::utf8(),
                         anos = arrow::list_of(arrow::int32()), n_total = arrow::int64(), n_years = arrow::int32())
  grava_parquet(dict, file.path(saida, "sinan_dict"), list(doenca = DOENCA), schema = sch_d)
  msg(sprintf("sinan_dict/: %d codigos", nrow(dict)))
}
