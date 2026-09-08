# =============================================================================
# attribution.R
#
# Security and sleeve attribution with Carino multi-period linking, so linked
# contributions reconcile exactly to the compounded portfolio return being
# decomposed.
#
# Carino:  k_d = log(1+R_d)/R_d  (=1 at R_d=0),  K = log(1+R)/R  (=1 at R=0),
#          C_j = sum_d c_{j,d} * k_d / K.
#
# All attribution uses the single relative-momentum 120/20 portfolio's own
# daily/cumulative return, built from `portfolio_weight`.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

CARINO_TOL <- 1e-10

#' Daily Carino factors k_d given a daily return series and the total return.
carino_factors <- function(daily_return, total_return) {
  k_d <- if_else(
    abs(daily_return) < CARINO_TOL,
    1,
    log1p(daily_return) / daily_return
  )
  K <- if (abs(total_return) < CARINO_TOL) {
    1
  } else {
    log1p(total_return) / total_return
  }
  list(k_d = k_d, K = K)
}

#' Filter a daily object to an attribution period: "all" or an evaluation month.
filter_period <- function(df, period) {
  if (is.null(period) || identical(period, "all")) {
    df
  } else {
    filter(df, evaluation_month == as.Date(period))
  }
}

#' Linked security-level contributions for one portfolio.
#' `contributions` from build_daily_contributions(); `port_returns` from
#' aggregate_portfolio_returns(). Both are filtered to `period` first.
security_attribution <- function(contributions, port_returns, period = "all") {
  contribs <- filter_period(contributions, period)
  ports <- filter_period(port_returns, period) |> arrange(date)

  total_return <- prod(1 + ports$portfolio_return) - 1
  cf <- carino_factors(ports$portfolio_return, total_return)
  day_factor <- tibble(date = ports$date, kK = cf$k_d / cf$K)

  contribs |>
    inner_join(day_factor, by = "date") |>
    mutate(linked = contribution * kK) |>
    group_by(ticker) |>
    summarise(
      sleeve = if (mean(weight) >= 0) "Long" else "Short",
      contribution = sum(linked),
      .groups = "drop"
    ) |>
    arrange(desc(contribution))
}

#' Sleeve-level linked contributions (long / short) for one portfolio.
sleeve_attribution <- function(security_attr) {
  security_attr |>
    group_by(sleeve) |>
    summarise(contribution = sum(contribution), .groups = "drop")
}

#' Bucket securities into top-5 positive / negative contributors and the rest.
contributor_buckets <- function(security_attr, n_top = 5) {
  pos <- security_attr |>
    filter(contribution > 0) |>
    arrange(desc(contribution))
  neg <- security_attr |> filter(contribution < 0) |> arrange(contribution)

  bind_rows(
    pos |> slice_head(n = n_top) |> mutate(bucket = "Top Positive"),
    tibble(
      ticker = "Other Positive",
      sleeve = NA_character_,
      contribution = sum(pos$contribution[-seq_len(min(n_top, nrow(pos)))]),
      bucket = "Other Positive"
    ),
    neg |> slice_head(n = n_top) |> mutate(bucket = "Top Negative"),
    tibble(
      ticker = "Other Negative",
      sleeve = NA_character_,
      contribution = sum(neg$contribution[-seq_len(min(n_top, nrow(neg)))]),
      bucket = "Other Negative"
    )
  ) |>
    filter(
      !is.na(contribution),
      contribution != 0 | ticker %in% security_attr$ticker
    )
}

#' Reconciliation: linked total minus the portfolio's compounded return.
attribution_reconciliation <- function(
  security_attr,
  port_returns,
  period = "all"
) {
  ports <- filter_period(port_returns, period)
  linked_total <- sum(security_attr$contribution)
  compounded <- prod(1 + ports$portfolio_return) - 1
  tibble(
    linked_total = linked_total,
    compounded_return = compounded,
    difference = linked_total - compounded
  )
}
