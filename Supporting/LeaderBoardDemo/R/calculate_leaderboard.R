source("Supporting/LeaderBoardDemo/R/common.R")

build_daily_positions <- function(accepted, teams, official_days) {
  rows <- vector("list", length(teams) * length(official_days))
  z <- 1L

  for (team in teams) {
    team_subs <- accepted |>
      filter(.data$team == .env$team, !is.na(effective_date)) |>
      arrange(effective_date, matrix_date, commit_time)

    for (d in official_days) {
      eligible <- team_subs |> filter(effective_date <= d)
      if (nrow(eligible) == 0L) {
        weights <- numeric()
        matrix_date <- as.Date(NA_character_)
        effective_date <- as.Date(NA_character_)
        filename <- NA_character_
      } else {
        current <- eligible |> slice_tail(n = 1)
        weights <- unlist(current$weights[[1]])
        matrix_date <- unlist(current$matrix_date[[1]])
        effective_date <- unlist(current$effective_date[[1]])
        filename <- unlist(current$filename[[1]])
      }

      rows[[z]] <- tibble(
        team = team,
        date = as.Date(d),
        matrix_date = matrix_date,
        effective_date = effective_date,
        filename = filename,
        net_exposure = sum(.env$weights),
        gross_exposure = sum(abs(.env$weights)),
        weights = list(.env$weights)
      )
      z <- z + 1L
    }
  }

  bind_rows(rows)
}

compute_turnover_events <- function(accepted, teams, official_days) {
  events <- list()

  for (team in teams) {
    subs <- accepted |>
      filter(
        .data$team == .env$team,
        !is.na(effective_date),
        effective_date <= max(official_days)
      ) |>
      arrange(effective_date, matrix_date, commit_time)

    if (nrow(subs) == 0L) {
      next
    }

    prior <- NULL
    for (j in seq_len(nrow(subs))) {
      current <- subs$weights[[j]]
      if (is.null(prior)) {
        to <- 0
      } else {
        universe <- union(names(prior), names(current))
        p <- setNames(rep(0, length(universe)), universe)
        c <- p
        p[names(prior)] <- prior
        c[names(current)] <- current
        to <- sum(abs(c - p))
      }

      events[[length(events) + 1L]] <- tibble(
        team = team,
        turnover_date = subs$effective_date[[j]],
        matrix_date = subs$matrix_date[[j]],
        filename = subs$filename[[j]],
        turnover = to
      )
      prior <- current
    }
  }

  if (length(events) == 0L) {
    return(tibble(
      team = character(),
      turnover_date = as.Date(character()),
      matrix_date = as.Date(character()),
      filename = character(),
      turnover = double()
    ))
  }

  bind_rows(events)
}

compute_weekly_turnover <- function(turnover_events, teams, official_days) {
  weeks <- tibble(
    date = official_days,
    week_start = week_start_monday(official_days)
  ) |>
    distinct(week_start)

  base <- tidyr::crossing(team = teams, week_start = weeks$week_start)

  if (nrow(turnover_events) == 0L) {
    return(base |> mutate(weekly_turnover = 0) |> arrange(team, week_start))
  }

  event_totals <- turnover_events |>
    mutate(week_start = week_start_monday(turnover_date)) |>
    group_by(team, week_start) |>
    summarise(weekly_turnover = sum(turnover), .groups = "drop")

  base |>
    left_join(event_totals, by = c("team", "week_start")) |>
    mutate(weekly_turnover = replace_na(weekly_turnover, 0)) |>
    arrange(team, week_start)
}

compute_one_day_return <- function(weights, date, market_panel) {
  if (length(weights) == 0L) {
    return(list(
      portfolio_return = 0,
      data_status = "ok_cash",
      warning = NA_character_
    ))
  }

  needed <- tibble(symbol = names(weights), weight = as.numeric(weights)) |>
    left_join(
      market_panel |>
        filter(.data$date == .env$date) |>
        select(symbol, asset_return, market_status, price_source),
      by = "symbol"
    )

  if (anyNA(needed$asset_return)) {
    bad <- needed$symbol[is.na(needed$asset_return)]
    return(list(
      portfolio_return = NA_real_,
      data_status = "market_data_pending",
      warning = paste0(
        "Missing required return for: ",
        paste(bad, collapse = ", ")
      )
    ))
  }

  close_fallback <- needed$symbol[needed$price_source == "close_fallback"]
  warn <- if (length(close_fallback) > 0L) {
    paste0(
      "Close used because Adjusted was unavailable: ",
      paste(close_fallback, collapse = ", ")
    )
  } else {
    NA_character_
  }

  list(
    portfolio_return = sum(needed$weight * needed$asset_return),
    data_status = if_else(is.na(warn), "ok", "ok_with_warning"),
    warning = warn
  )
}

compute_daily_returns <- function(
  positions,
  market_panel,
  benchmark,
  official_days
) {
  benchmark <- normalize_ticker(benchmark)
  benchmark_returns <- market_panel |>
    filter(symbol == benchmark, date %in% official_days) |>
    select(date, benchmark_return = asset_return)

  daily <- purrr::pmap_dfr(
    positions,
    function(
      team,
      date,
      matrix_date,
      effective_date,
      filename,
      weights,
      net_exposure,
      gross_exposure
    ) {
      r <- compute_one_day_return(weights, as.Date(date), market_panel)
      tibble(
        team = team,
        date = as.Date(date),
        matrix_date = as.Date(matrix_date),
        effective_date = as.Date(effective_date),
        filename = filename,
        portfolio_return_raw = r$portfolio_return,
        data_status = r$data_status,
        data_warning = r$warning,
        net_exposure = net_exposure,
        gross_exposure = gross_exposure
      )
    }
  ) |>
    left_join(benchmark_returns, by = "date") |>
    group_by(team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      blocked = cumany(is.na(portfolio_return_raw)),
      performance_status = if_else(blocked, "market_data_pending", data_status),
      portfolio_return = if_else(blocked, NA_real_, portfolio_return_raw),
      active_return = portfolio_return - benchmark_return,
      equity_index = {
        out <- rep(NA_real_, dplyr::n())
        level <- 100
        for (i in seq_along(portfolio_return)) {
          if (is.na(portfolio_return[[i]])) {
            out[[i]] <- level
          } else {
            level <- level * (1 + portfolio_return[[i]])
            out[[i]] <- level
          }
        }
        out
      },
      drawdown = equity_index / cummax(equity_index) - 1
    ) |>
    ungroup()

  benchmark_curve <- benchmark_returns |>
    arrange(date) |>
    mutate(
      equity_index = 100 * cumprod(1 + benchmark_return),
      drawdown = equity_index / cummax(equity_index) - 1
    )

  list(daily = daily, benchmark = benchmark_curve)
}

risk_metrics <- function(r, active, min_obs, annualization_days) {
  n <- length(r)
  if (n < min_obs || anyNA(r) || anyNA(active)) {
    return(tibble(
      annualized_volatility = NA_real_,
      sharpe = NA_real_,
      information_ratio = NA_real_,
      sortino = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_,
      var_95 = NA_real_,
      cvar_95 = NA_real_
    ))
  }

  sd_r <- safe_sd(r)
  sd_active <- safe_sd(active)
  ann_vol <- if (is.na(sd_r)) NA_real_ else sd_r * sqrt(annualization_days)
  sharpe <- if (is.na(sd_r)) {
    NA_real_
  } else {
    mean(r) / sd_r * sqrt(annualization_days)
  }
  info <- if (is.na(sd_active)) {
    NA_real_
  } else {
    mean(active) / sd_active * sqrt(annualization_days)
  }

  sortino_periodic <- tryCatch(
    as.numeric(PerformanceAnalytics::SortinoRatio(r, MAR = 0)),
    error = function(e) NA_real_
  )
  sortino <- sortino_periodic * sqrt(annualization_days)

  sk <- tryCatch(
    as.numeric(PerformanceAnalytics::skewness(r, method = "sample")),
    error = function(e) NA_real_
  )
  ku <- tryCatch(
    as.numeric(PerformanceAnalytics::kurtosis(r, method = "sample")),
    error = function(e) NA_real_
  )

  var95 <- as.numeric(stats::quantile(
    r,
    probs = 0.05,
    type = 7,
    names = FALSE,
    na.rm = FALSE
  ))
  cvar95 <- mean(r[r <= var95])

  tibble(
    annualized_volatility = ann_vol,
    sharpe = sharpe,
    information_ratio = info,
    sortino = sortino,
    skewness = sk,
    kurtosis = ku,
    var_95 = var95,
    cvar_95 = cvar95
  )
}

calculate_rolling_beta <- function(daily, min_obs = 10L) {
  daily |>
    group_by(team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      rolling_beta_10d = slider::slide2_dbl(
        portfolio_return,
        benchmark_return,
        .before = min_obs - 1L,
        .complete = TRUE,
        .f = function(x, y) {
          if (
            length(x) != min_obs || anyNA(x) || anyNA(y) || stats::var(y) == 0
          ) {
            return(NA_real_)
          }
          stats::cov(x, y) / stats::var(y)
        }
      )
    ) |>
    ungroup()
}

horizon_metrics <- function(team_df, as_of, trailing_days) {
  team_df <- team_df |> filter(date <= as_of) |> arrange(date)
  week_start <- week_start_monday(as_of)
  month_start <- as.Date(lubridate::floor_date(as_of, "month"))
  trailing_dates <- tail(team_df$date, trailing_days)

  tibble(
    wtd = compound_return(team_df$portfolio_return[team_df$date >= week_start]),
    mtd = compound_return(team_df$portfolio_return[
      team_df$date >= month_start
    ]),
    trailing_4w = if (nrow(team_df) < trailing_days) {
      NA_real_
    } else {
      compound_return(team_df$portfolio_return[
        team_df$date %in% trailing_dates
      ])
    },
    itd = compound_return(team_df$portfolio_return)
  )
}

build_summary <- function(daily, benchmark_daily, weekly_turnover, cfg, as_of) {
  teams <- unique(daily$team)
  min_obs <- cfg$trading$min_risk_observations
  annualization <- cfg$trading$annualization_days
  trailing_days <- cfg$trading$trailing_4w_trading_days
  lambda <- cfg$trading$turnover_penalty_lambda
  current_week <- week_start_monday(as_of)

  team_summary <- purrr::map_dfr(teams, function(team_name) {
    x <- daily |> filter(team == team_name, date <= as_of) |> arrange(date)
    h <- horizon_metrics(x, as_of, trailing_days)
    risk <- risk_metrics(
      x$portfolio_return,
      x$active_return,
      min_obs,
      annualization
    )
    to_weeks <- weekly_turnover |>
      filter(team == team_name, week_start <= current_week)
    avg_to <- mean(to_weeks$weekly_turnover)
    current_to <- to_weeks |>
      filter(week_start == current_week) |>
      summarise(x = sum(weekly_turnover)) |>
      pull(x)
    if (length(current_to) == 0L) {
      current_to <- 0
    }

    max_dd <- if (all(is.na(x$drawdown))) {
      NA_real_
    } else {
      min(x$drawdown, na.rm = TRUE)
    }
    srf <- if (is.na(risk$sharpe)) NA_real_ else risk$sharpe - lambda * avg_to
    current <- x |> slice_tail(n = 1)

    bind_cols(
      tibble(
        team = team_name,
        as_of = as_of,
        observations = sum(!is.na(x$portfolio_return)),
        rank_eligible = !current$blocked,
        current_net_exposure = current$net_exposure,
        current_gross_exposure = current$gross_exposure,
        current_week_turnover = current_to,
        avg_weekly_turnover = avg_to,
        max_drawdown = max_dd,
        sr_f = srf
      ),
      h,
      risk
    )
  }) |>
    mutate(
      rank = if_else(
        rank_eligible & !is.na(sr_f),
        competition_rank_desc(sr_f),
        NA_integer_
      )
    ) |>
    arrange(rank, desc(sr_f), team)

  b <- benchmark_daily |>
    filter(date <= as_of) |>
    arrange(date) |>
    transmute(date, portfolio_return = benchmark_return, active_return = 0)
  b_h <- horizon_metrics(b, as_of, trailing_days)
  b_risk <- risk_metrics(
    b$portfolio_return,
    b$portfolio_return - b$portfolio_return,
    min_obs,
    annualization
  )
  # Benchmark information ratio is undefined because active returns versus itself are identically zero.
  b_risk$information_ratio <- NA_real_
  b_dd <- benchmark_daily |>
    filter(date <= as_of) |>
    summarise(x = min(drawdown, na.rm = TRUE)) |>
    pull(x)

  benchmark_summary <- bind_cols(
    tibble(
      team = cfg$trading$benchmark,
      as_of = as_of,
      observations = nrow(b),
      rank_eligible = FALSE,
      current_net_exposure = 1,
      current_gross_exposure = 1,
      current_week_turnover = NA_real_,
      avg_weekly_turnover = NA_real_,
      max_drawdown = b_dd,
      sr_f = NA_real_,
      rank = NA_integer_
    ),
    b_h,
    b_risk
  )

  list(team_summary = team_summary, benchmark_summary = benchmark_summary)
}

compute_fund_composition <- function(positions, teams, as_of, tol = 1e-8) {
  valid <- positions |>
    filter(team %in% teams, date <= as_of, !is.na(effective_date)) |>
    group_by(team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(n_positions = purrr::map_int(weights, ~ sum(abs(.x) > tol))) |>
    ungroup()

  history_stats <- valid |>
    group_by(team) |>
    summarise(
      max_positions = max(n_positions),
      min_positions = min(n_positions),
      .groups = "drop"
    )

  current_stats <- valid |>
    group_by(team) |>
    slice_tail(n = 1) |>
    ungroup() |>
    mutate(
      current_positions = n_positions,
      abs_w = purrr::map(
        weights,
        ~ {
          w <- abs(.x)
          w[w > tol]
        }
      ),
      gross = purrr::map_dbl(abs_w, sum),
      effective_bets = purrr::map2_dbl(abs_w, gross, function(w, g) {
        if (length(w) == 0L || g <= 0) {
          return(NA_real_)
        }
        p <- w / g
        1 / sum(p^2)
      }),
      top10_concentration = purrr::map2_dbl(abs_w, gross, function(w, g) {
        if (length(w) == 0L || g <= 0) {
          return(NA_real_)
        }
        top10 <- sort(w, decreasing = TRUE)[seq_len(min(10L, length(w)))]
        sum(top10) / g
      })
    ) |>
    select(team, current_positions, effective_bets, top10_concentration)

  tibble(team = teams) |>
    left_join(history_stats, by = "team") |>
    left_join(current_stats, by = "team")
}

compute_return_attribution <- function(
  positions,
  daily,
  market_panel,
  teams,
  as_of,
  top_n = 5L,
  tol = 1e-10
) {
  daily_flags <- daily |>
    filter(team %in% teams, date <= as_of) |>
    select(team, date, blocked)

  # Unnest each team-date's named weight vector into long (team, date, symbol, weight) rows.
  pos_long <- positions |>
    filter(team %in% teams, date <= as_of) |>
    select(team, date, weights) |>
    mutate(
      weights = purrr::map(weights, function(w) {
        if (length(w) == 0L) {
          return(tibble(symbol = character(), weight = double()))
        }
        tibble(symbol = names(w), weight = as.numeric(w))
      })
    ) |>
    tidyr::unnest(weights)

  returns_long <- market_panel |> select(symbol, date, asset_return)

  # Beginning-of-day weight x security return, zeroed on blocked days (consistent
  # with how the equity curve freezes rather than compounding an unknown return).
  contrib <- pos_long |>
    left_join(daily_flags, by = c("team", "date")) |>
    left_join(returns_long, by = c("symbol", "date")) |>
    mutate(
      blocked = tidyr::replace_na(blocked, TRUE),
      raw_contribution = if_else(
        blocked | is.na(asset_return),
        0,
        weight * asset_return
      )
    )

  # Carino logarithmic smoothing: link daily contributions so they reconcile
  # exactly to each fund's compounded since-inception return.
  team_daily_return <- daily |>
    filter(team %in% teams, date <= as_of) |>
    group_by(team) |>
    arrange(date, .by_group = TRUE) |>
    mutate(r_used = if_else(is.na(portfolio_return), 0, portfolio_return)) |>
    mutate(
      k_t = if_else(abs(r_used) < tol, 1, log1p(r_used) / r_used)
    ) |>
    ungroup() |>
    select(team, date, k_t)

  team_totals <- daily |>
    filter(team %in% teams, date <= as_of) |>
    group_by(team) |>
    arrange(date, .by_group = TRUE) |>
    slice_tail(n = 1) |>
    ungroup() |>
    transmute(
      team,
      total_return = equity_index / 100 - 1,
      K = if_else(
        abs(total_return) < tol,
        1,
        log1p(total_return) / total_return
      )
    )

  contrib_scaled <- contrib |>
    left_join(team_daily_return, by = c("team", "date")) |>
    left_join(team_totals |> select(team, K), by = "team") |>
    mutate(scaled_contribution = raw_contribution * (k_t / K))

  security_level <- contrib_scaled |>
    group_by(team, symbol) |>
    summarise(
      total_contribution = sum(scaled_contribution),
      .groups = "drop"
    ) |>
    filter(abs(total_contribution) > tol)

  reconciliation <- security_level |>
    group_by(team) |>
    summarise(attribution_total = sum(total_contribution), .groups = "drop") |>
    right_join(tibble(team = teams), by = "team") |>
    mutate(attribution_total = tidyr::replace_na(attribution_total, 0)) |>
    left_join(team_totals |> select(team, total_return), by = "team") |>
    mutate(difference = attribution_total - total_return)

  bucket_one_fund <- function(df) {
    pos <- df |>
      filter(total_contribution > 0) |>
      arrange(desc(total_contribution))
    neg <- df |> filter(total_contribution < 0) |> arrange(total_contribution)

    n_pos <- nrow(pos)
    n_neg <- nrow(neg)
    top_pos <- pos |> slice_head(n = min(top_n, n_pos))
    rest_pos <- pos |> slice_tail(n = max(0L, n_pos - top_n))
    top_neg <- neg |> slice_head(n = min(top_n, n_neg))
    rest_neg <- neg |> slice_tail(n = max(0L, n_neg - top_n))

    rows <- list()
    if (nrow(top_neg) > 0L) {
      rows$top_neg <- top_neg |>
        transmute(
          label = symbol,
          contribution = total_contribution,
          category = "Top Negative",
          order = row_number()
        )
    }
    if (nrow(rest_neg) > 0L) {
      rows$other_neg <- tibble(
        label = "Other Negative",
        contribution = sum(rest_neg$total_contribution),
        category = "Other Negative",
        order = 1
      )
    }
    if (nrow(rest_pos) > 0L) {
      rows$other_pos <- tibble(
        label = "Other Positive",
        contribution = sum(rest_pos$total_contribution),
        category = "Other Positive",
        order = 1
      )
    }
    if (nrow(top_pos) > 0L) {
      rows$top_pos <- top_pos |>
        arrange(total_contribution) |>
        transmute(
          label = symbol,
          contribution = total_contribution,
          category = "Top Positive",
          order = row_number()
        )
    }
    bind_rows(rows)
  }

  chart_data <- security_level |>
    tidyr::nest(data = -team) |>
    mutate(bucketed = purrr::map(data, bucket_one_fund)) |>
    select(team, bucketed) |>
    tidyr::unnest(bucketed)

  if (nrow(chart_data) == 0L) {
    chart_data <- tibble(
      team = character(),
      label = character(),
      contribution = double(),
      category = character(),
      order = integer()
    )
  }

  chart_data <- chart_data |>
    mutate(
      category = factor(
        category,
        levels = c(
          "Top Negative",
          "Other Negative",
          "Other Positive",
          "Top Positive"
        )
      )
    ) |>
    group_by(team) |>
    arrange(
      team,
      match(
        category,
        c("Top Negative", "Other Negative", "Other Positive", "Top Positive")
      ),
      order
    ) |>
    mutate(stack_order = row_number()) |>
    ungroup()

  list(
    security_level = security_level,
    reconciliation = reconciliation,
    chart_data = chart_data
  )
}

build_operational_status <- function(
  validation_log,
  accepted,
  daily,
  teams,
  as_of
) {
  purrr::map_dfr(teams, function(team_name) {
    attempts <- validation_log |>
      filter(team == team_name) |>
      arrange(matrix_date, commit_time, version)
    latest_attempt <- attempts |> slice_tail(n = 1)

    effective <- accepted |>
      filter(
        team == team_name,
        !is.na(effective_date),
        effective_date <= as_of
      ) |>
      arrange(effective_date, matrix_date, commit_time) |>
      slice_tail(n = 1)

    current_daily <- daily |>
      filter(team == team_name, date <= as_of) |>
      slice_tail(n = 1)

    tibble(
      team = team_name,
      effective_matrix = if (nrow(effective)) {
        effective$filename[[1]]
      } else {
        NA_character_
      },
      matrix_date = if (nrow(effective)) {
        effective$matrix_date[[1]]
      } else {
        as.Date(NA_character_)
      },
      effective_date = if (nrow(effective)) {
        effective$effective_date[[1]]
      } else {
        as.Date(NA_character_)
      },
      latest_attempt_status = if (nrow(latest_attempt)) {
        latest_attempt$status[[1]]
      } else {
        "no_submission"
      },
      latest_attempt_message = if (nrow(latest_attempt)) {
        latest_attempt$message[[1]]
      } else {
        "No Position Matrix submitted"
      },
      market_data_status = if (nrow(current_daily)) {
        current_daily$performance_status[[1]]
      } else {
        NA_character_
      },
      market_data_warning = if (nrow(current_daily)) {
        current_daily$data_warning[[1]]
      } else {
        NA_character_
      }
    )
  })
}
