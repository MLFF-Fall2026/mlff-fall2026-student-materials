repos <- c(CRAN = "https://cloud.r-project.org")
options(repos = repos)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE, restart = FALSE)
}

packages <- c(
  "tidyverse", "tidyquant", "quantmod", "PerformanceAnalytics",
  "yaml", "lubridate", "slider", "zoo", "xts", "gt", "plotly",
  "scales", "htmltools", "gh", "testthat"
)

renv::install(packages)
renv::snapshot(prompt = FALSE)
message("Dependencies installed and renv.lock updated.")
