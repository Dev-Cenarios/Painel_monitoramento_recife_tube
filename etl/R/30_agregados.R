# etl/R/30_agregados.R
# -----------------------------------------------------------------------------
# Agregados que o painel le: incidence/, _cache_ts/, obitos/, piramides/.
#
# Niveis: o painel e travado em Recife. Os niveis MUN e UF repetem os numeros
# de Recife (assim era no ETL antigo); o nivel BR vem de etl/ref/nacional_*.csv.
# -----------------------------------------------------------------------------

i64 <- arrow::int64; i32 <- arrow::int32; f64 <- arrow::float64; txt <- arrow::utf8

conta <- function(cond, grupo, niveis) as.integer(table(factor(grupo[cond], levels = niveis)))

# ---------------------------------------------------------------------------
# Resumo anual de Recife (base de incidence MUN/UF e obitos MUN/UF)
# ---------------------------------------------------------------------------
resumo_recife <- function(sinan, sim, pop, anos) {
  s <- sinan; m <- sim
  popt <- stats::aggregate(pop ~ ano + sexo, data = pop, FUN = sum)
  pp <- function(sx) popt$pop[match(paste(anos, sx), paste(popt$ano, popt$sexo))]
  data.frame(
    ano          = anos,
    casos_total  = conta(rep(TRUE, nrow(s)), s$ano, anos),
    casos_M      = conta(s$CS_SEXO == "M", s$ano, anos),
    casos_F      = conta(s$CS_SEXO == "F", s$ano, anos),
    casos_cura   = conta(s$SITUA_ENCE == SIT_CURA, s$ano, anos),
    cura_M       = conta(s$SITUA_ENCE == SIT_CURA & s$CS_SEXO == "M", s$ano, anos),
    cura_F       = conta(s$SITUA_ENCE == SIT_CURA & s$CS_SEXO == "F", s$ano, anos),
    casos_obitos = conta(rep(TRUE, nrow(m)), m$ano, anos),       # SIM, causa basica TB
    obitos_M     = conta(m$sexo == "M", m$ano, anos),
    obitos_F     = conta(m$sexo == "F", m$ano, anos),
    pop_M        = pp(1L),
    pop_F        = pp(2L)
  ) |> transform(pop_total = pop_M + pop_F)
}

# ---------------------------------------------------------------------------
# incidence/doenca=TUBERCULOSE/nivel={MUN,UF,BR,BAIRRO}/ano=<a>/
# ---------------------------------------------------------------------------
grava_incidence <- function(res, ref, bairro, saida) {
  base <- file.path(saida, "incidence")
  metricas <- c("casos_total", "casos_obitos", "obitos_M", "obitos_F", "casos_cura", "cura_M", "cura_F",
                "casos_M", "casos_F", "pop_total", "pop_M", "pop_F")
  sch_metr <- c(stats::setNames(rep(list(i64()), length(metricas)), metricas),
                list(incid_100k_total = f64(), incid_100k_M = f64(), incid_100k_F = f64()))
  com_taxas <- function(d) {
    d$incid_100k_total <- taxa(d$casos_total, d$pop_total)
    d$incid_100k_M     <- taxa(d$casos_M, d$pop_M)
    d$incid_100k_F     <- taxa(d$casos_F, d$pop_F)
    d
  }
  res <- com_taxas(res)
  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    mun <- data.frame(doenca = DOENCA, ano = r$ano, uf = RECIFE$uf, cod_mun7 = RECIFE$cod_mun7,
                      cod_mun6 = RECIFE$cod_mun6, nome_mun = RECIFE$nome_mun, r[c(metricas, names(sch_metr)[13:15])])
    grava_parquet(mun, base, list(doenca = DOENCA, nivel = "MUN", ano = r$ano), schema = do.call(arrow::schema,
      c(list(doenca = txt(), ano = i32(), uf = txt(), cod_mun7 = txt(), cod_mun6 = txt(), nome_mun = txt()), sch_metr)))
    uf <- data.frame(doenca = DOENCA, ano = r$ano, uf = RECIFE$uf, r[c(metricas, names(sch_metr)[13:15])])
    grava_parquet(uf, base, list(doenca = DOENCA, nivel = "UF", ano = r$ano), schema = do.call(arrow::schema,
      c(list(doenca = txt(), ano = i32(), uf = txt()), sch_metr)))
  }
  br <- com_taxas(ref$br_anual[ref$br_anual$ano %in% res$ano, ])
  for (i in seq_len(nrow(br))) {
    d <- data.frame(doenca = DOENCA, br[i, c("ano", metricas, names(sch_metr)[13:15])])
    grava_parquet(d, base, list(doenca = DOENCA, nivel = "BR", ano = br$ano[i]), schema = do.call(arrow::schema,
      c(list(doenca = txt(), ano = i32()), sch_metr)))
  }
  sch_b <- arrow::schema(doenca = txt(), ano = i32(), uf = txt(), bairro_key = txt(), bairro_nome = txt(),
    casos_total = i64(), casos_M = i64(), casos_F = i64(), casos_obitos = i64(), casos_cura = i64(),
    encerramento_total = i64(), abandono = i64(), interrupcao_trat_pct = f64(), hiv_tested = i64(),
    hiv_pos = i64(), hiv_pos_pct = f64(), pop_total = i64(), incid_100k_total = f64(),
    mortalidade_100k_total = f64(), letalidade_pct = f64())
  for (a in sort(unique(bairro$ano))) {
    grava_parquet(bairro[bairro$ano == a, names(sch_b)], base, list(doenca = DOENCA, nivel = "BAIRRO", ano = a), schema = sch_b)
  }
  msg(sprintf("incidence/: MUN, UF, BR e BAIRRO (%d bairros)", length(unique(bairro$bairro_key))))
}

# Indicadores por bairro de residencia (chave = NM_BAIRRO normalizado).
resumo_bairros <- function(sinan, sim, pop_bairro, ref, anos) {
  bairros <- ref$bairros
  s <- sinan; s$k <- chave_bairro(s$NM_BAIRRO)
  m <- sim;   m$k <- chave_bairro(m$BAIRES)
  grid <- expand.grid(bairro_key = bairros$bairro_key, ano = anos, stringsAsFactors = FALSE)
  ks <- paste(s$k, s$ano); km <- paste(m$k, m$ano); kg <- paste(grid$bairro_key, grid$ano)
  cnt <- function(cond, k) { t <- table(k[cond]); v <- as.integer(t[kg]); v[is.na(v)] <- 0L; v }
  out <- data.frame(
    doenca = DOENCA, ano = grid$ano, uf = RECIFE$uf, bairro_key = grid$bairro_key,
    bairro_nome = bairros$bairro_nome[match(grid$bairro_key, bairros$bairro_key)],
    casos_total = cnt(rep(TRUE, nrow(s)), ks),
    casos_M     = cnt(s$CS_SEXO == "M", ks),
    casos_F     = cnt(s$CS_SEXO == "F", ks),
    casos_obitos = cnt(rep(TRUE, nrow(m)), km),                          # SIM; substituido abaixo nos anos congelados
    casos_cura  = cnt(s$SITUA_ENCE == SIT_CURA, ks),
    encerramento_total = cnt(!vazio(s$SITUA_ENCE), ks),
    abandono    = cnt(s$SITUA_ENCE == SIT_ABANDONO, ks),
    hiv_pos     = cnt(s$HIV == "1", ks),
    stringsAsFactors = FALSE
  )
  out$hiv_tested <- out$casos_total

  # Obitos por bairro: anos presentes em etl/ref/obitos_bairro.csv usam os
  # numeros congelados do ETL antigo (regra original desconhecida). Anos novos
  # usam o SIM (causa basica TB, bairro de residencia, ano do obito).
  ob <- ref$obitos_bairro
  congelado <- grid$ano %in% ob$ano
  out$casos_obitos[congelado] <- as.integer(ob$casos_obitos[match(kg[congelado], paste(ob$bairro_key, ob$ano))])
  out$casos_obitos[congelado & is.na(out$casos_obitos)] <- 0L
  novos <- sort(unique(grid$ano[!congelado]))
  if (length(novos)) msg("Obitos por bairro: anos", paste(novos, collapse = ", "),
                         "pela regra SIM (fora de etl/ref/obitos_bairro.csv) -- nao comparaveis com os anos congelados")
  pb <- pop_bairro[pop_bairro$ano %in% anos, ]
  out$pop_total <- round(pb$populacao[match(kg, paste(pb$bairro_key, pb$ano))])
  out$interrupcao_trat_pct  <- taxa(out$abandono, out$encerramento_total, 100, se_zero = 0)
  out$hiv_pos_pct           <- taxa(out$hiv_pos, out$hiv_tested, 100, se_zero = 0)
  out$incid_100k_total      <- taxa(out$casos_total, out$pop_total, se_zero = 0)
  out$mortalidade_100k_total <- taxa(out$casos_obitos, out$pop_total, se_zero = 0)
  out$letalidade_pct        <- taxa(out$casos_obitos, out$casos_total, 100, se_zero = 0)
  sem_pop <- unique(out$bairro_key[is.na(out$pop_total)])
  if (length(sem_pop)) msg("AVISO: bairros sem populacao em pop_bairro_estimado.csv:", paste(sem_pop, collapse = ", "))
  out[order(out$ano, out$bairro_key), ]
}

# ---------------------------------------------------------------------------
# _cache_ts/nivel={BR,UF,MUN}/doenca=TUBERCULOSE/ano=<a>/   (serie mensal)
# Mes = mes da DT_NOTIFIC. Obitos aqui sao do SINAN (SITUA_ENCE = 3), como no
# ETL antigo -- diferente do KPI anual, que usa o SIM.
# ---------------------------------------------------------------------------
grava_series <- function(sinan, res, ref, saida) {
  base <- file.path(saida, "_cache_ts")
  sch <- arrow::schema(nivel = txt(), doenca = txt(), ano = i32(), mes = i32(), mes_nome = txt(), uf = txt(),
                       geo_id = txt(), geo_nome = txt(), casos = i64(), casos_obitos = i64(), casos_cura = i64(),
                       pop_total = i64(), incid_100k = f64())
  s <- sinan; s$mes <- mes_de_data(s$DT_NOTIFIC)
  for (a in res$ano) {
    x <- s[s$ano == a, ]
    pop <- res$pop_total[res$ano == a]
    d <- data.frame(ano = a, mes = 1:12, mes_nome = MESES,
                    casos = conta(rep(TRUE, nrow(x)), x$mes, 1:12),
                    casos_obitos = conta(x$SITUA_ENCE == SIT_OBITO_TB, x$mes, 1:12),
                    casos_cura = conta(x$SITUA_ENCE == SIT_CURA, x$mes, 1:12),
                    pop_total = pop)
    d$incid_100k <- taxa(d$casos, d$pop_total)
    for (nv in c("MUN", "UF")) {
      dd <- data.frame(nivel = nv, doenca = DOENCA, d[c("ano", "mes", "mes_nome")], uf = RECIFE$uf,
                       geo_id = RECIFE$cod_mun6, geo_nome = if (nv == "MUN") RECIFE$nome_mun else RECIFE$nome_uf,
                       d[c("casos", "casos_obitos", "casos_cura", "pop_total", "incid_100k")])
      grava_parquet(dd, base, list(nivel = nv, doenca = DOENCA, ano = a), schema = sch)
    }
  }
  br <- ref$br_mes[ref$br_mes$ano %in% res$ano, ]
  for (a in unique(br$ano)) {
    x <- br[br$ano == a, ]; x <- x[order(x$mes), ]
    pop <- ref$br_anual$pop_total[ref$br_anual$ano == a]
    dd <- data.frame(nivel = "BR", doenca = DOENCA, ano = a, mes = x$mes, mes_nome = MESES[x$mes], uf = "BR",
                     geo_id = "BR", geo_nome = "Brasil", casos = x$casos, casos_obitos = x$casos_obitos,
                     casos_cura = x$casos_cura, pop_total = pop, incid_100k = taxa(x$casos, pop))
    grava_parquet(dd, base, list(nivel = "BR", doenca = DOENCA, ano = a), schema = sch)
  }
  msg("_cache_ts/: series mensais MUN, UF, BR")
}

# ---------------------------------------------------------------------------
# obitos/doenca=TUBERCULOSE/nivel={MUN,UF,BR}/ano=<a>/part-0.parquet
# ---------------------------------------------------------------------------
grava_obitos <- function(res, ref, saida) {
  base <- file.path(saida, "obitos")
  sch_m <- arrow::schema(coduf = txt(), codmun = txt(), curas_sinan = i32(), obitos_sinan = i32(), casos_sinan = i32(),
                         obitos_sim = i32(), casosobitos = i32(), obitos_M = i32(), obitos_F = i32())
  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    for (nv in c("MUN", "UF")) {
      d <- data.frame(coduf = RECIFE$cod_uf, codmun = if (nv == "MUN") RECIFE$cod_mun6 else NA_character_,
                      curas_sinan = r$casos_cura, obitos_sinan = r$casos_obitos, casos_sinan = r$casos_total,
                      obitos_sim = r$casos_obitos, casosobitos = r$casos_obitos, obitos_M = r$obitos_M, obitos_F = r$obitos_F)
      grava_parquet(d, base, list(doenca = DOENCA, nivel = nv, ano = r$ano), "part-0.parquet", sch_m)
    }
  }
  sch_b <- arrow::schema(cod_uf = txt(), cod_mun = txt(), curas_sinan = i32(), obitos_sinan = i32(), casos_sinan = i32(),
                         obitos_sim = i32(), casos_obitos = i32(), obitos_M = i32(), obitos_F = i32())
  br <- ref$br_obit[ref$br_obit$ano %in% res$ano, ]
  for (i in seq_len(nrow(br))) {
    d <- data.frame(cod_uf = NA_character_, cod_mun = NA_character_, curas_sinan = 0L, obitos_sinan = 0L, casos_sinan = 0L,
                    obitos_sim = br$obitos_sim[i], casos_obitos = br$obitos_sim[i], obitos_M = br$obitos_M[i], obitos_F = br$obitos_F[i])
    grava_parquet(d, base, list(doenca = DOENCA, nivel = "BR", ano = br$ano[i]), "part-0.parquet", sch_b)
  }
  msg("obitos/: MUN, UF, BR")
}

# ---------------------------------------------------------------------------
# piramides/nivel={MUN,UF}/tipo={CASOS,CURA,OBITOS}/doenca=TUBERCULOSE/ano=<a>/
# ratio = valor / populacao da faixa * 100
# ---------------------------------------------------------------------------
grava_piramides <- function(sinan, pop, anos, saida) {
  base <- file.path(saida, "piramides")
  sch <- arrow::schema(nivel = txt(), tipo = txt(), doenca = txt(), ano = i32(), geo_id = txt(), uf = txt(),
                       geo_nome = txt(), sexo = txt(), faixa_ord = i32(), faixa_etaria = txt(), valor = i64(),
                       pop = i64(), ratio = f64())
  s <- sinan; s$faixa <- faixa_de_idade(idade_anos(s$NU_IDADE_N, "SINAN")); s$sexo <- s$CS_SEXO
  p <- pop;   p$faixa <- faixa_de_idade(p$idade); p$sexo <- ifelse(p$sexo == 1L, "M", "F")
  # OBITOS da piramide = SINAN (SITUA_ENCE 3, ano de notificacao), como no ETL
  # antigo -- nao o SIM, que e a fonte do KPI de mortalidade.
  fontes <- list(CASOS = s, CURA = s[s$SITUA_ENCE == SIT_CURA, ], OBITOS = s[s$SITUA_ENCE %in% SIT_OBITO_TB, ])
  for (a in anos) {
    pa <- p[p$ano == a, ]
    for (tp in names(fontes)) {
      x <- fontes[[tp]]; x <- x[x$ano == a, ]
      d <- do.call(rbind, lapply(c("M", "F"), function(sx) {
        data.frame(sexo = sx, faixa_ord = FAIXAS$faixa_ord, faixa_etaria = FAIXAS$faixa_etaria,
                   valor = conta(x$sexo == sx, x$faixa, FAIXAS$faixa_ord),
                   pop = vapply(FAIXAS$faixa_ord, function(f) sum(pa$pop[pa$sexo == sx & pa$faixa == f]), numeric(1)))
      }))
      d$ratio <- taxa(d$valor, d$pop, 100)
      for (nv in c("MUN", "UF")) {
        dd <- data.frame(nivel = nv, tipo = tp, doenca = DOENCA, ano = a, geo_id = RECIFE$cod_mun6, uf = RECIFE$uf,
                         geo_nome = if (nv == "MUN") RECIFE$nome_mun else RECIFE$nome_uf, d)
        grava_parquet(dd, base, list(nivel = nv, tipo = tp, doenca = DOENCA, ano = a), schema = sch)
      }
    }
  }
  msg("piramides/: CASOS, CURA, OBITOS por sexo e faixa etaria")
}
