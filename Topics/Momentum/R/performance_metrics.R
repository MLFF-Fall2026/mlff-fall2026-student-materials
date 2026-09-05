# =============================================================================
# performance_metrics.R
#
# Performance, risk, benchmark-relative, exposure, and turnover metrics using
# the LeaderBoard conventions:
#   - Sharpe = mean(r)/sd(r)*sqrt(252), zero risk-free;
#   - realized vol = sd(r)*sqrt(252);
#   - max drawdown = min(equity/cummax(equity) - 1);
#   - tracking error = sd(active)*sqrt(252);
#   - information ratio = mean(active)/sd(active)*sqrt(252);
#   - beta = cov(r, r_b)/var(r_b) (full-period, given the short sample);
#   - turnover = sum(|w_t - w_{t-1}|), first target = 0, weekly Monday-start;
#   - turnover-adjusted Sharpe = SR - lambda * mean(weekly turnover).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
})

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) NA_real_ else sd(x)
}

compound_return <- function(r) prod(1 + r[is.finite(r)]) - 1

week_start_monday <- function(d) {
  floor_date(as.Date(d), unit = "week", week_start = 1)
}

#' Core performance/risk metrics for one portfolio's daily returns, optionally
#' benchmark-relative. `min_obs` flags illustrative-only risk estimates.
performance_metrics <- function(
  port_returns,
  bench_returns = NULL,
  settings,
  label = NULL
) {
  ann <- settings$annualization
  r <- port_returns$portfolio_return
  eq <- 100 * cumprod(1 + r)

  out <- tibble(
    portfolio = label %||% NA_character_,
    n_days = sum(is.finite(r)),
    total_return = compound_return(r),
    ann_volatility = safe_sd(r) * sqrt(ann),
    sharpe = mean(r, na.rm = TRUE) / safe_sd(r) * sqrt(ann),
    max_drawdown = min(eq / cummax(eq) - 1)
  )

  if (!is.null(bench_returns)) {
    j <- inner_join(port_returns, bench_returns, by = "date")
    active <- j$portfolio_return - j$benchmark_return
    out <- out |>
      mutate(
        active_return = compound_return(active),
        tracking_error = safe_sd(active) * sqrt(ann),
        information_ratio = mean(active, na.rm = TRUE) /
          safe_sd(active) *
          sqrt(ann),
        beta = if (var(j$benchmark_return) > 0) {
          cov(j$portfolio_return, j$benchmark_return) / var(j$benchmark_return)
        } else {
          NA_real_
        }
      )
  }

  out |>
    mutate(risk_estimate_reliable = n_days >= settings$min_risk_obs)
}

#' Monthly and cumulative returns from a daily portfolio series.
monthly_and_cumulative <- function(port_returns) {
  monthly <- port_returns |>
    group_by(evaluation_month) |>
    summarise(
      monthly_return = compound_return(portfolio_return),
      .groups = "drop"
    )
  cumulative <- compound_return(port_returns$portfolio_return)
  list(monthly = monthly, cumulative = cumulative)
}

# ---- Turnover ---------------------------------------------------------------

#' Monthly target-weight (reconstitution) turnover between successive monthly
#' portfolio-weight vectors. Full ticker union per pair, missing weights treated
#' as zero. First = 0. Change assigned to the effective_date of the later
#' formation. This measures reconstitution trades only; it does NOT capture the
#' intra-month trades implied by daily rebalancing back to constant targets.
turnover_events <- function(complete_panel, schedule) {
  months <- schedule |>
    arrange(evaluation_month) |>
    select(evaluation_month, effective_date)

  target <- function(m) {
    complete_panel |>
      filter(evaluation_month == m) |>
      transmute(ticker, w = portfolio_weight) |>
      mutate(w = replace_na(w, 0)) |>
      filter(w != 0)
  }

  res <- vector("list", nrow(months))
  for (i in seq_len(nrow(months))) {
    if (i == 1) {
      res[[i]] <- tibble(
        evaluation_month = months$evaluation_month[i],
        effective_date = months$effective_date[i],
        turnover = 0
      )
    } else {
      cur <- target(months$evaluation_month[i])
      prev <- target(months$evaluation_month[i - 1])
      merged <- full_join(cur, prev, by = "ticker", suffix = c("_c", "_p")) |>
        mutate(w_c = replace_na(w_c, 0), w_p = replace_na(w_p, 0))
      res[[i]] <- tibble(
        evaluation_month = months$evaluation_month[i],
        effective_date = months$effective_date[i],
        turnover = sum(abs(merged$w_c - merged$w_p))
      )
    }
  }
  bind_rows(res)
}

#' Weekly turnover (Monday-start weeks), zero-filled across the span, plus the
#' average weekly turnover used in the turnover-adjusted Sharpe.
weekly_turnover <- function(events) {
  ev <- events |>
    mutate(week_start = week_start_monday(effective_date)) |>
    group_by(week_start) |>
    summarise(weekly_turnover = sum(turnover), .groups = "drop")

  span <- seq(min(ev$week_start), max(ev$week_start), by = "week")
  weeks <- tibble(week_start = span) |>
    left_join(ev, by = "week_start") |>
    mutate(weekly_turnover = replace_na(weekly_turnover, 0))

  list(weeks = weeks, avg_weekly = mean(weeks$weekly_turnover))
}

#' Turnover-adjusted Sharpe = SR - lambda * avg weekly turnover.
turnover_adjusted_sharpe <- function(sharpe, avg_weekly, settings) {
  sharpe - settings$lambda * avg_weekly
}

# ---- Composition ------------------------------------------------------------

#' Composition per evaluation month on the final portfolio weights: counts,
#' exposures, effective number of bets (1/sum(p^2)) and top-10 concentration
#' (share of gross).
composition_metrics <- function(complete_panel, tol = 1e-8) {
  complete_panel |>
    mutate(w = portfolio_weight) |>
    filter(!is.na(w), abs(w) > tol) |>
    group_by(evaluation_month) |>
    summarise(
      n_long = sum(w > 0),
      n_short = sum(w < 0),
      n_total = n(),
      long_exposure = sum(w[w > 0]),
      short_exposure = sum(w[w < 0]),
      gross_exposure = sum(abs(w)),
      net_exposure = sum(w),
      effective_bets = 1 / sum((abs(w) / sum(abs(w)))^2),
      top10_concentration = sum(sort(abs(w), decreasing = TRUE)[seq_len(min(
        10L,
        n()
      ))]) /
        sum(abs(w)),
      .groups = "drop"
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
