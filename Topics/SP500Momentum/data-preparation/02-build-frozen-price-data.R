# =============================================================================
# 02-build-frozen-price-data.R
#
# One-time preparation script. Obtains daily adjusted prices for the static
# S&P 500 universe and IVV, builds a balanced instructional universe, derives
# month-end adjusted prices, validates the frozen outputs, and saves RDS files.
#
# Retained window is sufficient to form the May, June, and July 2026 momentum
# observations and to run the June-August 2026 daily evaluation:
#   - May 2026 momentum uses monthly returns May 2025 - March 2026, so the
#     earliest month-end price required is April 2025 (for the May 2025 return).
#   - Daily observations run through the end of August 2026.
# Trading dates are resolved from the observed data, not the calendar.
#
# Run once. Rendering the instructional QMD never calls this script.
# Outputs:
#   data/daily-adjusted-prices.rds    (date, ticker, adjusted_price, asset_role)
#   data/monthly-adjusted-prices.rds  (date, ticker, adjusted_price, asset_role)
# =============================================================================

suppressPackageStartupMessages({
  library(tidyquant)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
  library(here)
})

data_dir <- here("Topics", "MomentumStrategy", "data")

benchmark_ticker <- "IVV"

# Pull a little before April 2025 so the April month-end is well covered, and
# through the start of September 2026 so August is complete.
price_from <- as.Date("2025-03-01")
price_to <- as.Date("2026-09-01")

# Formation months (last trading day used) and evaluation months.
formation_months <- as.Date(c("2026-05-01", "2026-06-01", "2026-07-01"))
evaluation_months <- as.Date(c("2026-06-01", "2026-07-01", "2026-08-01"))

universe <- read_csv(
  file.path(data_dir, "sp500-static-universe.csv"),
  show_col_types = FALSE
)

all_tickers <- unique(c(universe$ticker, benchmark_ticker))

message(sprintf(
  "Fetching daily prices for %d symbols ...",
  length(all_tickers)
))

# tq_get drops symbols it cannot retrieve (with a warning) rather than failing.
raw_daily <- tq_get(
  all_tickers,
  from = price_from,
  to = price_to,
  get = "stock.prices"
)

daily <- raw_daily |>
  transmute(
    date = as.Date(date),
    ticker = toupper(trimws(symbol)),
    adjusted_price = adjusted
  ) |>
  filter(!is.na(adjusted_price), adjusted_price > 0) |>
  distinct(date, ticker, .keep_all = TRUE) |>
  arrange(ticker, date)

fetched <- sort(unique(daily$ticker))
missing_symbols <- setdiff(all_tickers, fetched)
if (length(missing_symbols) > 0) {
  message(sprintf(
    "Note: %d symbols returned no data and are dropped: %s",
    length(missing_symbols),
    paste(missing_symbols, collapse = ", ")
  ))
}
stopifnot(benchmark_ticker %in% fetched)

# The benchmark defines the canonical market calendar. Restricting every symbol
# to IVV's trading days guarantees each portfolio day has a benchmark return and
# removes isolated non-benchmark trading dates (e.g. a Yahoo gap where IVV lacks
# a record but a few constituents do).
canonical_dates <- daily |>
  filter(ticker == benchmark_ticker) |>
  pull(date) |>
  unique()
daily <- daily |> filter(date %in% canonical_dates)

# Month-end = last available trading observation within each calendar month.
monthly <- daily |>
  mutate(month = floor_date(date, "month")) |>
  group_by(ticker, month) |>
  filter(date == max(date)) |>
  ungroup() |>
  transmute(date, ticker, adjusted_price, month) |>
  arrange(ticker, date)

# --- Balanced instructional universe -----------------------------------------
# Monthly coverage required for the three momentum formations: the union of all
# monthly returns entering any of the May/June/July 2026 momentum values. The
# earliest is the May 2025 return (needs April 2025 month-end); the latest is
# the May 2026 return (needs May 2026 month-end, for July formation's momentum).
required_month_ends <- seq(
  as.Date("2025-04-01"),
  as.Date("2026-05-01"),
  by = "month"
)

# Daily coverage required across the June-August 2026 evaluation window.
eval_daily_dates <- daily |>
  filter(
    ticker == benchmark_ticker,
    date >= as.Date("2026-06-01"),
    date <= as.Date("2026-08-31")
  ) |>
  pull(date) |>
  sort()

monthly_ok <- monthly |>
  mutate(month = floor_date(date, "month")) |>
  filter(month %in% required_month_ends) |>
  group_by(ticker) |>
  summarise(n_months = n_distinct(month), .groups = "drop") |>
  filter(n_months == length(required_month_ends)) |>
  pull(ticker)

daily_ok <- daily |>
  filter(date %in% eval_daily_dates) |>
  group_by(ticker) |>
  summarise(n_days = n_distinct(date), .groups = "drop") |>
  filter(n_days == length(eval_daily_dates)) |>
  pull(ticker)

balanced_constituents <- sort(intersect(
  intersect(monthly_ok, daily_ok),
  universe$ticker
))

message(sprintf(
  "Balanced universe: %d constituents (from %d fetched) satisfy full monthly + daily coverage.",
  length(balanced_constituents),
  length(setdiff(fetched, benchmark_ticker))
))
stopifnot(length(balanced_constituents) > 50)

keep_tickers <- c(balanced_constituents, benchmark_ticker)

add_role <- function(df) {
  df |>
    mutate(
      asset_role = if_else(
        ticker == benchmark_ticker,
        "benchmark",
        "constituent"
      )
    ) |>
    select(date, ticker, adjusted_price, asset_role) |>
    arrange(ticker, date)
}

daily_frozen <- daily |>
  filter(ticker %in% keep_tickers) |>
  add_role()

monthly_frozen <- monthly |>
  select(date, ticker, adjusted_price) |>
  filter(ticker %in% keep_tickers) |>
  add_role()

# --- Validate frozen outputs before saving -----------------------------------
stopifnot(
  all(
    c("date", "ticker", "adjusted_price", "asset_role") %in% names(daily_frozen)
  ),
  all(
    c("date", "ticker", "adjusted_price", "asset_role") %in%
      names(monthly_frozen)
  ),
  nrow(daily_frozen) == nrow(distinct(daily_frozen, date, ticker)),
  nrow(monthly_frozen) == nrow(distinct(monthly_frozen, date, ticker)),
  all(daily_frozen$adjusted_price > 0),
  all(monthly_frozen$adjusted_price > 0),
  benchmark_ticker %in% daily_frozen$ticker,
  benchmark_ticker %in% monthly_frozen$ticker
)

saveRDS(daily_frozen, file.path(data_dir, "daily-adjusted-prices.rds"))
saveRDS(monthly_frozen, file.path(data_dir, "monthly-adjusted-prices.rds"))

message(sprintf(
  "Saved daily (%d rows, %s to %s) and monthly (%d rows) frozen prices for %d symbols.",
  nrow(daily_frozen),
  min(daily_frozen$date),
  max(daily_frozen$date),
  nrow(monthly_frozen),
  length(keep_tickers)
))
