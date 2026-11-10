suppressPackageStartupMessages({ library(shiny) })

APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

UTILS_R <- normalizePath(file.path(APP_DIR, "..", "_utils", "app_utils.R"),
                         winslash = "/", mustWork = FALSE)
source(UTILS_R, local = TRUE, encoding = "UTF-8")

run_core_app(app_dir = APP_DIR, year_min = 2010L, year_max = 2023L)
