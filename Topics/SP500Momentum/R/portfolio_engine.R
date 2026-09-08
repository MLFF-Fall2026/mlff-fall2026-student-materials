# =============================================================================
# portfolio_engine.R
#
# Generic, configuration-driven infrastructure for the Momentum Strategy 2
# exercise. All objects are tidy long-form: one row per relevant
# date/ticker.
#
# Deliberately NOT here: the feature -> signal -> position -> raw -> scaled ->
# portfolio weight transformation. That economic core is shown inline in the
# QMD (single holdout) and then abstracted into a *local* function inside the
# QMD (multiple holdouts). This file keeps only reusable infrastructure:
#   - monthly returns and 12-2 momentum helpers;
#   - holdout-schedule resolution from observed trading dates;
#   - a generic rolling driver that maps a QMD-supplied weight function over
#     the schedule;
#   - daily security contributions, portfolio returns, equity indices,
#     drawdowns, active returns, and exposure reconciliation on the single
#     `portfolio_weight` column.
#
# Functions receive the settings list (or the relevant elements) rather than
# relying on hidden assumptions.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
})

# ---- Feature helpers --------------------------------------------------------

#' Monthly simple returns from month-end adjusted prices (long form).
#' Adds `month` (month-floor) for joining to formation months.
compute_monthly_returns <- function(monthly_prices) {
  monthly_prices |>
    arrange(ticker, date) |>
    group_by(ticker) |>
    mutate(monthly_return = adjusted_price / dplyr::lag(adjusted_price) - 1) |>
    ungroup() |>
    mutate(month = floor_date(date, "month"))
}

#' 12-2 momentum: product of (1 + monthly_return) over lags 2..12, minus 1.
#' Exactly 11 return factors; NA until 12 prior months exist.
compute_momentum <- function(monthly_returns) {
  lags <- 2:12
  monthly_returns |>
    arrange(ticker, date) |>
    group_by(ticker) |>
    mutate(
      momentum = purrr::reduce(
        lags,
        function(acc, k) acc * (1 + dplyr::lag(monthly_return, k)),
        .init = 1
      ) -
        1
    ) |>
    ungroup()
}

# ---- Holdout schedule -------------------------------------------------------

#' Resolve exact formation, effective, and evaluation dates from daily prices.
#' formation_date = last trading day of the formation month.
#' effective_date = first trading day of the evaluation month.
#' eval_start/eval_end bracket the evaluation month's trading days.
build_holdout_schedule <- function(
  daily_prices,
  formation_months,
  evaluation_months,
  benchmark
) {
  bench_days <- daily_prices |>
    filter(ticker == benchmark) |>
    mutate(month = floor_date(date, "month"))

  last_day <- function(m) max(bench_days$date[bench_days$month == m])
  first_day <- function(m) min(bench_days$date[bench_days$month == m])

  tibble(
    formation_month = as.Date(formation_months),
    evaluation_month = as.Date(evaluation_months)
  ) |>
    mutate(
      formation_date = purrr::map_vec(formation_month, last_day),
      effective_date = purrr::map_vec(evaluation_month, first_day),
      eval_start = effective_date,
      eval_end = purrr::map_vec(
        evaluation_month,
        ~ max(bench_days$date[bench_days$month == .x])
      )
    )
}

# ---- Generic rolling driver -------------------------------------------------

#' Apply a QMD-supplied weight function to every row of the holdout schedule
#' and stack the results into one continuous panel.
#'
#' This is infrastructure only: it loops formation months, hands each
#' formation-date momentum cross-section to `weight_fn()`, and stamps the
#' schedule dates. All economics (cutoffs, signal, position, relative strength,
#' raw / scaled / portfolio weights) live entirely inside `weight_fn`, which is
#' defined locally in the QMD so students can read it without opening a file.
#'
#' `weight_fn(momentum_df)` must return one row per ticker with columns:
#'   ticker, momentum, q25, q75, median_mom, signal, position,
#'   relative_strength, raw_weight, scaled_weight, portfolio_weight.
build_complete_panel <- function(momentum_panel, schedule, weight_fn) {
  purrr::pmap_dfr(schedule, function(...) {
    row <- tibble(...)
    mom <- momentum_panel |>
      filter(month == row$formation_month) |>
      select(ticker, momentum)

    weight_fn(mom) |>
      mutate(
        formation_date = row$formation_date,
        effective_date = row$effective_date,
        evaluation_month = row$evaluation_month
      ) |>
      select(
        formation_date,
        effective_date,
        evaluation_month,
        ticker,
        momentum,
        q25,
        q75,
        median_mom,
        signal,
        position,
        relative_strength,
        raw_weight,
        scaled_weight,
        portfolio_weight
      )
  })
}

#' Active-holdings view: nonzero, non-missing portfolio weights.
active_holdings <- function(complete_panel) {
  complete_panel |>
    transmute(
      formation_date,
      effective_date,
      evaluation_month,
      ticker,
      signal,
      position,
      weight = portfolio_weight
    ) |>
    filter(!is.na(weight), weight != 0)
}

# ---- Exposure reconciliation ------------------------------------------------

#' Long, short, gross, and net exposure per evaluation month, on the final
#' portfolio weights.
exposure_summary <- function(complete_panel) {
  complete_panel |>
    mutate(w = portfolio_weight) |>
    group_by(evaluation_month) |>
    summarise(
      n_long = sum(position == 1L, na.rm = TRUE),
      n_short = sum(position == -1L, na.rm = TRUE),
      long_exposure = sum(w[w > 0], na.rm = TRUE),
      short_exposure = sum(w[w < 0], na.rm = TRUE),
      gross_exposure = sum(abs(w), na.rm = TRUE),
      net_exposure = sum(w, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Daily returns and contributions ----------------------------------------

#' Daily simple asset returns from daily adjusted prices (long form).
compute_asset_daily_returns <- function(daily_prices) {
  daily_prices |>
    arrange(ticker, date) |>
    group_by(ticker) |>
    mutate(daily_return = adjusted_price / dplyr::lag(adjusted_price) - 1) |>
    ungroup()
}

#' Daily security contributions C_{i,d} = w^p_{i} * R_{i,d} using the final
#' portfolio weights. Constant monthly target weights => w^p_{i,d-1} = w^p_i
#' across the holding month. Only evaluation-month dates for held names return.
build_daily_contributions <- function(complete_panel, asset_returns) {
  weights <- complete_panel |>
    transmute(evaluation_month, ticker, weight = portfolio_weight) |>
    filter(!is.na(weight), weight != 0)

  asset_returns |>
    mutate(evaluation_month = floor_date(date, "month")) |>
    inner_join(weights, by = c("evaluation_month", "ticker")) |>
    filter(!is.na(daily_return)) |>
    transmute(
      date,
      evaluation_month,
      ticker,
      weight,
      asset_return = daily_return,
      contribution = weight * daily_return
    ) |>
    arrange(date, ticker)
}

#' Aggregate contributions to a continuous daily portfolio series with equity
#' index (base 100) and drawdown. Returns compound continuously across months.
aggregate_portfolio_returns <- function(contributions) {
  contributions |>
    group_by(date, evaluation_month) |>
    summarise(portfolio_return = sum(contribution), .groups = "drop") |>
    arrange(date) |>
    mutate(
      equity_index = 100 * cumprod(1 + portfolio_return),
      drawdown = equity_index / cummax(equity_index) - 1
    )
}

#' Benchmark daily returns over the evaluation dates present in a portfolio.
benchmark_daily_returns <- function(asset_returns, benchmark, dates) {
  asset_returns |>
    filter(ticker == benchmark, date %in% dates, !is.na(daily_return)) |>
    arrange(date) |>
    transmute(
      date,
      benchmark_return = daily_return,
      benchmark_index = 100 * cumprod(1 + daily_return),
      benchmark_drawdown = benchmark_index / cummax(benchmark_index) - 1
    )
}

#' Active (benchmark-relative) daily returns: R_p - R_IVV.
active_daily_returns <- function(port_returns, bench_returns) {
  port_returns |>
    inner_join(bench_returns, by = "date") |>
    mutate(active_return = portfolio_return - benchmark_return) |>
    select(
      date,
      evaluation_month,
      portfolio_return,
      benchmark_return,
      active_return
    )
}
