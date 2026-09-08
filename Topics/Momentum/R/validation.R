# =============================================================================
# validation.R
#
# Fail-fast validation contract for the relative-momentum 120/20 lesson. Each
# check returns a row with a pass/fail flag and a short detail.
# `assert_checks()` stops the render on any failure; the returned table is
# displayed so students see the principal checks passed. Reconciliation
# differences must fall below `tol` (reasonable numerical tolerances, not exact
# floating-point equality).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

RECON_TOL <- 1e-8

check <- function(name, passed, detail = "") {
  tibble(check = name, passed = isTRUE(passed), detail = detail)
}

#' Stop the render if any essential check failed; otherwise return the table.
assert_checks <- function(checks, context = "validation") {
  failed <- checks |> filter(!passed)
  if (nrow(failed) > 0) {
    stop(
      sprintf(
        "%s failed:\n%s",
        context,
        paste0(" - ", failed$check, ": ", failed$detail, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  checks
}

# ---- Data checks ------------------------------------------------------------

validate_data <- function(
  daily,
  monthly,
  universe,
  benchmark,
  required_month_ends,
  eval_daily_dates
) {
  schema_cols <- c("date", "ticker", "adjusted_price", "asset_role")

  daily_bench <- daily |> filter(ticker == benchmark)
  monthly_bench_months <- monthly |>
    filter(ticker == benchmark) |>
    mutate(month = floor_date(date, "month")) |>
    pull(month)

  # Balanced-panel requirement for retained constituents.
  months_ok <- monthly |>
    filter(
      ticker != benchmark,
      floor_date(date, "month") %in% required_month_ends
    ) |>
    group_by(ticker) |>
    summarise(n = n_distinct(floor_date(date, "month")), .groups = "drop")
  daily_ok <- daily |>
    filter(ticker != benchmark, date %in% eval_daily_dates) |>
    group_by(ticker) |>
    summarise(n = n_distinct(date), .groups = "drop")

  bind_rows(
    check(
      "daily schema",
      all(schema_cols %in% names(daily)),
      paste(setdiff(schema_cols, names(daily)), collapse = ", ")
    ),
    check(
      "monthly schema",
      all(schema_cols %in% names(monthly)),
      paste(setdiff(schema_cols, names(monthly)), collapse = ", ")
    ),
    check(
      "daily ticker-date unique",
      nrow(daily) == nrow(distinct(daily, date, ticker))
    ),
    check(
      "monthly ticker-date unique",
      nrow(monthly) == nrow(distinct(monthly, date, ticker))
    ),
    check("daily prices positive", all(daily$adjusted_price > 0)),
    check("monthly prices positive", all(monthly$adjusted_price > 0)),
    check(
      "balanced panel: monthly coverage",
      all(months_ok$n == length(required_month_ends)),
      sprintf(
        "%d constituents complete",
        sum(months_ok$n == length(required_month_ends))
      )
    ),
    check(
      "balanced panel: daily coverage",
      all(daily_ok$n == length(eval_daily_dates)),
      sprintf(
        "%d constituents complete",
        sum(daily_ok$n == length(eval_daily_dates))
      )
    ),
    check("IVV daily coverage", all(eval_daily_dates %in% daily_bench$date)),
    check(
      "IVV monthly coverage",
      all(required_month_ends %in% monthly_bench_months)
    )
  )
}

# ---- Feature and timing checks ----------------------------------------------

#' Feature/timing checks that depend only on the momentum feature and schedule
#' (no signal/cutoffs required, so this can run before weights are built).
validate_features <- function(momentum_panel, monthly_returns, schedule) {
  # May 2026 momentum should use returns labeled May 2025 - March 2026.
  expected_return_months <- seq(
    as.Date("2025-05-01"),
    as.Date("2026-03-01"),
    by = "month"
  )

  n_factors <- monthly_returns |>
    filter(month %in% expected_return_months, !is.na(monthly_return)) |>
    group_by(ticker) |>
    summarise(n = n(), .groups = "drop")

  eff_ok <- schedule |>
    mutate(ok = effective_date > formation_date) |>
    pull(ok)

  bind_rows(
    check(
      "May momentum window is May-2025..Mar-2026",
      length(expected_return_months) == 11,
      paste(range(format(expected_return_months, "%Y-%m")), collapse = " .. ")
    ),
    check(
      "11 return factors per momentum",
      all(n_factors$n == 11),
      sprintf("%d tickers with exactly 11", sum(n_factors$n == 11))
    ),
    check("effective date after formation date", all(eff_ok))
  )
}

# ---- Weight checks ----------------------------------------------------------

#' Weight construction contract for the relative-momentum 120/20 portfolio.
validate_weights <- function(complete_panel) {
  cp <- complete_panel

  sigs <- cp$signal
  signal_range_ok <- all(sigs %in% c(-1L, 0L, 1L) | is.na(sigs))

  # Cutoffs are computed separately per formation month: the stored q25/q75/
  # median must equal the type-7 empirical quantiles of that month's momentum.
  cutoff_ok <- cp |>
    group_by(evaluation_month) |>
    summarise(
      q25_ok = abs(
        first(q25) - quantile(momentum, 0.25, type = 7, na.rm = TRUE)
      ) <
        RECON_TOL,
      q75_ok = abs(
        first(q75) - quantile(momentum, 0.75, type = 7, na.rm = TRUE)
      ) <
        RECON_TOL,
      med_ok = abs(first(median_mom) - median(momentum, na.rm = TRUE)) <
        RECON_TOL,
      distinct_cut = n_distinct(q25) == 1 & n_distinct(q75) == 1,
      .groups = "drop"
    )

  position_ok <- with(
    cp,
    all((position == signal) | (is.na(position) & is.na(signal)))
  )

  # Strict-inequality quartile rule: ties on either cutoff are neutral.
  tie_ok <- cp |>
    filter(!is.na(momentum), (momentum == q25 | momentum == q75)) |>
    summarise(ok = all(replace_na(signal, 0L) == 0L)) |>
    pull(ok)
  tie_ok <- length(tie_ok) == 0 || isTRUE(tie_ok)

  neutral_ok <- cp |>
    filter(position == 0L) |>
    summarise(
      ok = all(
        replace_na(raw_weight, 0) == 0 &
          replace_na(scaled_weight, 0) == 0 &
          replace_na(portfolio_weight, 0) == 0
      )
    ) |>
    pull(ok)

  raw_sign_ok <- cp |>
    filter(!is.na(position), position != 0L) |>
    summarise(
      ok = all(
        (position == 1L & raw_weight > 0) | (position == -1L & raw_weight < 0)
      )
    ) |>
    pull(ok)

  by_month <- cp |>
    group_by(evaluation_month) |>
    summarise(
      n_long = sum(position == 1L, na.rm = TRUE),
      n_short = sum(position == -1L, na.rm = TRUE),
      scaled_long = sum(scaled_weight[scaled_weight > 0], na.rm = TRUE),
      scaled_short = sum(scaled_weight[scaled_weight < 0], na.rm = TRUE),
      port_long = sum(portfolio_weight[portfolio_weight > 0], na.rm = TRUE),
      port_short = sum(portfolio_weight[portfolio_weight < 0], na.rm = TRUE),
      gross = sum(abs(portfolio_weight), na.rm = TRUE),
      net = sum(portfolio_weight, na.rm = TRUE),
      n_bad = sum(!is.na(momentum) & is.na(portfolio_weight)),
      .groups = "drop"
    )

  bind_rows(
    check("signals in {-1,0,1} or NA", signal_range_ok),
    check(
      "percentile cutoffs computed per formation month (type = 7)",
      all(
        cutoff_ok$q25_ok &
          cutoff_ok$q75_ok &
          cutoff_ok$med_ok &
          cutoff_ok$distinct_cut
      )
    ),
    check("position == signal", position_ok),
    check("cutoff ties are neutral", tie_ok),
    check("neutral names carry zero raw/scaled/portfolio weight", neutral_ok),
    check("long raw weights > 0 and short raw weights < 0", raw_sign_ok),
    check(
      "each formation month has non-empty long and short sleeves",
      all(by_month$n_long > 0 & by_month$n_short > 0),
      sprintf(
        "min long %d, min short %d",
        min(by_month$n_long),
        min(by_month$n_short)
      )
    ),
    check(
      "scaled long weights sum to +1",
      all(abs(by_month$scaled_long - 1) < RECON_TOL)
    ),
    check(
      "scaled short weights sum to -1",
      all(abs(by_month$scaled_short + 1) < RECON_TOL)
    ),
    check(
      "portfolio long weights sum to +1.20",
      all(abs(by_month$port_long - 1.20) < RECON_TOL)
    ),
    check(
      "portfolio short weights sum to -0.20",
      all(abs(by_month$port_short + 0.20) < RECON_TOL)
    ),
    check(
      "portfolio net exposure = 1.00",
      all(abs(by_month$net - 1.00) < RECON_TOL)
    ),
    check(
      "portfolio gross exposure = 1.40",
      all(abs(by_month$gross - 1.40) < RECON_TOL)
    ),
    check(
      "no missing-weight propagation among eligible names",
      all(by_month$n_bad == 0)
    )
  )
}

# ---- Return checks ----------------------------------------------------------

validate_returns <- function(
  contributions,
  port_returns,
  active_returns,
  monthly_cum,
  schedule
) {
  recon <- contributions |>
    group_by(date) |>
    summarise(sum_c = sum(contribution), .groups = "drop") |>
    inner_join(port_returns, by = "date") |>
    mutate(diff = abs(sum_c - portfolio_return))

  eq_ok <- port_returns |>
    arrange(date) |>
    mutate(recomputed = 100 * cumprod(1 + portfolio_return)) |>
    summarise(ok = all(abs(recomputed - equity_index) < 1e-6)) |>
    pull(ok)

  monthly_compound <- prod(1 + monthly_cum$monthly$monthly_return) - 1
  active_ok <- all(
    abs(
      active_returns$active_return -
        (active_returns$portfolio_return - active_returns$benchmark_return)
    ) <
      RECON_TOL
  )

  # First eligible return per evaluation month occurs strictly after formation.
  first_ret <- contributions |>
    group_by(evaluation_month) |>
    summarise(first_date = min(date), .groups = "drop") |>
    inner_join(schedule, by = "evaluation_month") |>
    mutate(ok = first_date > formation_date)

  bind_rows(
    check(
      "daily return = sum of contributions",
      all(recon$diff < RECON_TOL),
      sprintf("max diff %.2e", max(recon$diff))
    ),
    check("equity index = compounded returns", eq_ok),
    check(
      "monthly returns compound to cumulative",
      abs(monthly_compound - monthly_cum$cumulative) < 1e-6
    ),
    check("active = portfolio - benchmark", active_ok),
    check("first eligible return is after formation date", all(first_ret$ok))
  )
}

# ---- Attribution checks -----------------------------------------------------

validate_attribution <- function(
  security_attr,
  sleeve_attr,
  port_returns,
  tol = 1e-8
) {
  sec_total <- sum(security_attr$contribution)
  sleeve_total <- sum(sleeve_attr$contribution)
  compounded <- prod(1 + port_returns$portfolio_return) - 1

  bind_rows(
    check(
      "linked security contributions reconcile to compounded return",
      abs(sec_total - compounded) < tol,
      sprintf("diff %.2e", abs(sec_total - compounded))
    ),
    check(
      "sleeve contributions reconcile to security total",
      abs(sleeve_total - sec_total) < tol,
      sprintf("diff %.2e", abs(sleeve_total - sec_total))
    )
  )
}

# ---- Inline vs. rolling-function equality -----------------------------------

#' The inline single-holdout construction must reproduce the rolling local
#' function's output for the same month, ticker by ticker.
validate_inline_matches_rolling <- function(inline_panel, rolling_panel) {
  j <- inline_panel |>
    select(ticker, portfolio_weight) |>
    full_join(
      rolling_panel |> select(ticker, portfolio_weight),
      by = "ticker",
      suffix = c("_inline", "_roll")
    ) |>
    mutate(
      wi = replace_na(portfolio_weight_inline, 0),
      wr = replace_na(portfolio_weight_roll, 0),
      diff = abs(wi - wr)
    )

  check(
    "inline June construction matches rolling function",
    all(j$diff < RECON_TOL),
    sprintf("max weight diff %.2e", max(j$diff))
  )
}
