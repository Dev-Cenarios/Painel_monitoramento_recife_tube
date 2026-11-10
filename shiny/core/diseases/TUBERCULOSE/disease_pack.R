# dashs/shiny/core/diseases/TUBERCULOSE/disease_pack.R
get_disease_pack <- function() {
  list(
    DOENCA = "TUBERCULOSE",
    TITLE  = "Monitoramento Tuberculose",
    COLORS = list(
      # tema
      institutional = "#12346B",
      primary       = "#12346B",
      secondary     = "#1E73BE",
      accent        = "#4A8EFF",
      danger        = "#DB400A",
      warning       = "#DB400A",
      success       = "#4A8EFF",
      background    = "#F4F4F4",
      surface       = "#FFFFFF",
      text_muted    = "#6C757D",
      border        = "#DEE2E6",

      # cores por métrica
      cases       = "#12346B",
      obitos      = "#DB400A",
      cura        = "#1E73BE",
      pop         = "#6C757D",
      incid       = "#1E73BE",
      mortalidade = "#DB400A",
      letalidade  = "#DB400A",
      hiv_pos_pct = "#4A8EFF",
      interrupcao_trat_pct = "#DB400A"
    ),
    KPI_LABELS = list(
      cases         = "Casos novos",
      incid         = "Incidência por 100 mil hab.",
      cura          = "Curas",
      mortalidade   = "Taxa de mortalidade por 100 mil hab.",
      hiv_pos_pct   = "HIV positivo na testagem (%)",
      interrupcao_trat_pct = "Interrupcao de tratamento (%)"
    ),
    KPI_LAYOUT = c("incid", "cases", "mortalidade", "interrupcao_trat_pct", "hiv_pos_pct", "cura"),
    MAP_PALETTE_CASES = c(
      "#E6C9A8",
      "#D9B08A",
      "#C89666",
      "#B37D42",
      "#8B5E2E",
      "#6B4423",
      "#4A2E17"
    ),
    MAP_PALETTE_INCID = c(
      "#E6C9A8",
      "#D9B08A",
      "#C89666",
      "#B37D42",
      "#8B5E2E",
      "#6B4423",
      "#4A2E17"
    ),
    ECHARTS_PALETTE_BARS = c(
      "#1E73BE",
      "#4A8EFF",
      "#12346B",
      "#6C757D",
      "#DB400A",
      "#1E73BE",
      "#4A8EFF",
      "#12346B",
      "#6C757D",
      "#DB400A"
    ),
    ECHARTS_PALETTE_LINES = c(
      "#1E73BE",
      "#12346B",
      "#4A8EFF",
      "#DB400A",
      "#6C757D",
      "#1E73BE",
      "#12346B",
      "#4A8EFF",
      "#DB400A",
      "#6C757D"
    ),
    PREFERRED_COMPOSITION_VARS = c(
      "TRATAMENTO",
      "HIV",
      "FORMA",
      "CS_RACA",
      "RACA_COR",
      "AGRAVALCOO",
      "SITUA_ENCE",
      "POP_RUA",
      "POP_SAUDE",
      "AGRAVDROGA",
      "AGRAVTABAC"
    ),
    COMPOSITION_VAR_LABELS = list(
      TRATAMENTO  = "Tipo de tratamento",
      HIV         = "Coinfecção HIV",
      FORMA       = "Forma clínica da tuberculose",
      CS_RACA     = "Raça/Cor",
      RACA_COR    = "Raça/Cor",
      AGRAVALCOO  = "Agravo: Alcoolismo",
      SITUA_ENCE  = "Situação de encerramento",
      POP_RUA     = "População em situação de rua",
      POP_SAUDE   = "Profissional de saúde",
      AGRAVDROGA  = "Agravo: Uso de drogas",
      AGRAVTABAC  = "Agravo: Tabagismo"
    )
  )
}
