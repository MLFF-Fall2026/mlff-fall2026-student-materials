# =============================================================================
# 01-build-static-universe.R
#
# One-time preparation script. Obtains and freezes a single static snapshot of
# S&P 500 constituents for use as the investable universe on every formation
# date in the Momentum Strategy exercise.
#
# This is NOT a point-in-time membership database. See README.md and the QMD
# limitations section for the survivorship / look-ahead caveats this implies.
#
# Run once. Rendering the instructional QMD never calls this script.
# Output: data/sp500-static-universe.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyquant)
  library(dplyr)
  library(readr)
  library(here)
})

out_dir <- here("Topics", "MomentumStrategy", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

snapshot_date <- Sys.Date()

# Current S&P 500 constituents from Yahoo/tidyquant.
sp500_raw <- tq_index("SP500")

universe <- sp500_raw |>
  transmute(
    ticker = toupper(trimws(symbol)),
    company = company,
    snapshot_date = snapshot_date,
    source = "tidyquant::tq_index('SP500')"
  ) |>
  filter(!is.na(ticker), ticker != "") |>
  distinct(ticker, .keep_all = TRUE) |>
  arrange(ticker)

stopifnot(nrow(universe) > 400)

write_csv(universe, file.path(out_dir, "sp500-static-universe.csv"))

message(sprintf(
  "Froze %d constituents to %s (snapshot %s).",
  nrow(universe),
  file.path(out_dir, "sp500-static-universe.csv"),
  snapshot_date
))
