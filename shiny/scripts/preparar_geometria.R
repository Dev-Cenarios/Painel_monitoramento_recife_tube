# scripts/preparar_geometria.R
# -----------------------------------------------------------------------------
# Pre-processa a malha de bairros de Recife para um arquivo pronto de usar.
#
# Por que existe: ler o shapefile e passar por st_make_valid + st_cast +
# st_simplify + st_transform custa ~525 ms, e isso rodava dentro da aplicacao,
# uma vez por processo R. O resultado e sempre o mesmo, entao nao ha razao para
# calcula-lo em tempo de resposta. Mesma ideia do scripts/preparar_geometria.py
# do painel RecifeTB.
#
# Alem de evitar o recalculo, corrige um bug silencioso: o st_simplify da
# aplicacao rodava em lon/lat e nao simplificava nada (24.677 vertices antes e
# depois). Aqui projetamos antes, e a malha cai para 3.776 vertices / 47,9 KB.
#
# Rode de novo se o shapefile mudar:
#   Rscript scripts/preparar_geometria.R
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({ library(sf) })

# Tolerancia de simplificacao, em METROS (CRS projetado).
# Medido nesta malha (26.942 vertices originais, desvio de area por bairro):
#   2 m -> 8.582 vertices (32%), desvio mediano 0,032% / maximo 0,181%  <- padrao
#   5 m -> 5.310 vertices (20%), mediano 0,093% / maximo 0,679%
#  10 m -> 3.786 vertices (14%), mediano 0,152% / maximo 1,537%
# O padrao e 2 m: conservador de proposito, porque muda o que aparece na tela.
# Se o payload importar mais que o detalhe, 5 m ainda e defensavel.
#   Rscript scripts/preparar_geometria.R 5
args <- commandArgs(trailingOnly = TRUE)
TOL_M <- if (length(args) > 0) as.numeric(args[[1]]) else 2

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f) > 0) dirname(normalizePath(f[[1]], winslash = "/")) else getwd()
})
SHINY_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
ROOT      <- normalizePath(file.path(SHINY_DIR, ".."), winslash = "/", mustWork = FALSE)

ORIGEM <- file.path(ROOT, "data", "fwmalharecife")
DESTINO_DIR <- file.path(ROOT, "data", "geo")
DESTINO <- file.path(DESTINO_DIR, "recife_bairros.rds")

if (!dir.exists(ORIGEM)) stop("Malha nao encontrada: ", ORIGEM)
dir.create(DESTINO_DIR, recursive = TRUE, showWarnings = FALSE)

# --- as mesmas transformacoes que mod_map.R aplicava em runtime ---------------
.fix <- function(g) {
  g <- tryCatch(sf::st_zm(g, drop = TRUE, what = "ZM"), error = function(e) g)
  g <- tryCatch(sf::st_make_valid(g), error = function(e) g)
  ok <- tryCatch(!sf::st_is_empty(g), error = function(e) rep(TRUE, nrow(g)))
  if (any(!ok)) g <- g[ok, , drop = FALSE]
  g <- tryCatch({
    geom <- sf::st_geometry(g)
    tipos <- suppressWarnings(as.character(sf::st_geometry_type(geom, by_geometry = TRUE)))
    if (any(!tipos %in% c("POLYGON", "MULTIPOLYGON"))) {
      geom <- suppressWarnings(sf::st_collection_extract(geom, "POLYGON", warn = FALSE))
    }
    geom <- suppressWarnings(sf::st_cast(geom, "MULTIPOLYGON", warn = FALSE))
    sf::st_set_geometry(g, geom)
  }, error = function(e) g)
  tryCatch(sf::st_transform(g, 4326), error = function(e) g)
}

# ATENCAO -- st_simplify em coordenadas geograficas (lon/lat) nao funciona.
# Medido nesta malha: com EPSG:4326 a reducao para em 85% dos vertices, seja a
# tolerancia 11 m ou 222 m. Era o que mod_map.R fazia: pagava o custo do
# st_simplify e nao recebia simplificacao nenhuma.
#
# Projetando para UTM 25S (EPSG:31985), simplificando em metros e voltando para
# 4326, 10 m de tolerancia leva a 14% dos vertices. Num mapa de bairros em zoom
# 11-12, 10 m e sub-pixel: o desenho nao muda a olho.
.simplificar <- function(g, tol_m = 10) {
  crs0 <- sf::st_crs(g)
  gm <- tryCatch(sf::st_transform(g, 31985), error = function(e) NULL)
  if (is.null(gm)) return(g)
  gs <- tryCatch(sf::st_simplify(gm, dTolerance = tol_m, preserveTopology = TRUE),
                 error = function(e) NULL)
  if (is.null(gs)) return(g)
  tryCatch(sf::st_transform(gs, crs0), error = function(e) g)
}

.vertices <- function(g) {
  sum(vapply(sf::st_geometry(g), function(x) nrow(sf::st_coordinates(x)), integer(1)))
}

.name_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  gsub("[^A-Z0-9]+", "", x)
}

# --- localizar a camada de bairros -------------------------------------------
arquivos <- list.files(ORIGEM, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
arquivos <- arquivos[!grepl("logradouro", basename(arquivos), ignore.case = TRUE)]
if (length(arquivos) == 0) stop("Nenhum .shp de bairros em: ", ORIGEM)

t0 <- as.numeric(Sys.time())
bairros <- NULL

for (caminho in arquivos) {
  g0 <- tryCatch(sf::st_read(caminho, quiet = TRUE), error = function(e) NULL)
  if (is.null(g0) || nrow(g0) == 0) next

  attrs <- as.data.frame(sf::st_drop_geometry(g0))
  col_bairro <- intersect(c("EBAIRRNOME", "BAIRRO", "NOME_BAIRRO", "bairro", "nome_bairro"), names(attrs))
  if (length(col_bairro) == 0) next
  col_bairro <- col_bairro[[1]]

  g <- .fix(g0)
  if (is.null(g) || nrow(g) == 0) next

  col_id <- intersect(c("CBAIRRCODI", "VBAIRROID", "OBJECTID", "ID", "id"), names(attrs))
  g$shape_name <- trimws(enc2utf8(as.character(g[[col_bairro]])))
  g$shape_name[is.na(g$shape_name) | !nzchar(g$shape_name)] <- "Bairro nao informado"
  g$shape_id <- if (length(col_id) > 0) as.character(g[[col_id[[1]]]]) else as.character(seq_len(nrow(g)))
  vazio <- is.na(g$shape_id) | !nzchar(g$shape_id)
  g$shape_id[vazio] <- as.character(seq_len(nrow(g)))[vazio]

  bairros <- .fix(.simplificar(g, tol_m = TOL_M))
  break
}

if (is.null(bairros)) stop("Nao consegui montar a camada de bairros.")
gasto <- (as.numeric(Sys.time()) - t0) * 1000

saveRDS(list(bairros = bairros, distritos = NULL), DESTINO, compress = "gzip")

cat("\n=== geometria preparada ===\n")
cat(sprintf("origem .............. %s\n", ORIGEM))
cat(sprintf("destino ............. %s\n", DESTINO))
cat(sprintf("poligonos ........... %d\n", nrow(bairros)))
cat(sprintf("vertices ............ %s\n", format(.vertices(bairros), big.mark = " ")))
cat(sprintf("tamanho ............. %.1f KB\n", file.info(DESTINO)$size / 1024))
cat(sprintf("custo evitado ....... %.0f ms por processo R\n", gasto))

# --- so informativo: quanto custaria simplificar mais -------------------------
cat(sprintf("tolerancia usada .... %g m", TOL_M), "
")
