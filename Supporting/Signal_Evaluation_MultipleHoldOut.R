signal_evaluation_multipleholdout <- function(
  signal_position_ts,
  returns_ts,
  Meta,
  return_threshold = 0,
  dates = NULL,
  keep_static_results = FALSE,
  verbose = TRUE
) {
  # -----------------------
  # Validation
  # -----------------------
  stopifnot(
    all(c("date", "ticker", "signal_position") %in% names(signal_position_ts)),
    all(c("date", "ticker", "test_returns") %in% names(returns_ts)),
    is.list(Meta)
  )

  signal_position_ts <- signal_position_ts %>%
    mutate(date = as.Date(date))

  returns_ts <- returns_ts %>%
    mutate(date = as.Date(date))

  if (!is.null(dates)) {
    dates <- as.Date(dates)
    signal_position_ts <- filter(signal_position_ts, date %in% dates)
    returns_ts <- filter(returns_ts, date %in% dates)
  }

  run_dates <- sort(intersect(
    unique(signal_position_ts$date),
    unique(returns_ts$date)
  ))

  if (length(run_dates) == 0) {
    stop("No overlapping dates between signals and returns.")
  }

  # -----------------------
  # Storage
  # -----------------------
  by_date <- vector("list", length(run_dates))
  static_results <- if (keep_static_results) {
    vector("list", length(run_dates))
  } else {
    NULL
  }

  # -----------------------
  # Loop
  # -----------------------
  for (i in seq_along(run_dates)) {
    d <- run_dates[i]
    if (verbose) {
      cat(
        "Evaluating",
        as.character(d),
        sprintf("(%d/%d)\n", i, length(run_dates))
      )
    }

    sig_d <- signal_position_ts %>%
      filter(date == d) %>%
      select(ticker, signal_position)

    ret_d <- returns_ts %>%
      filter(date == d) %>%
      select(ticker, test_returns)

    res_static <- tryCatch(
      signal_evaluation_singleholdout(
        test_returns = ret_d,
        signal_position = sig_d,
        Meta = Meta,
        return_threshold = return_threshold
      ),
      error = function(e) {
        message("Static evaluation failed on ", d, ": ", e$message)
        NULL
      }
    )

    if (keep_static_results) {
      static_results[[i]] <- res_static
    }

    if (is.null(res_static)) {
      by_date[[i]] <- tibble(
        date = d,
        ic_all = NA_real_,
        ic_acted = NA_real_,
        hit_overall = NA_real_,
        hit_acted = NA_real_,
        coverage = NA_real_,
        n = NA_integer_
      )
      next
    }

    # ---- IC
    ic_tbl <- res_static$summary_ic_tbl
    ic_all <- ic_tbl$All[ic_tbl$Metric == "IC"]
    ic_acted <- ic_tbl$Acted_Only[ic_tbl$Metric == "IC"]

    # ---- Hit rate
    hit_tbl <- res_static$hit_table
    hit_overall <- hit_tbl$Overall[hit_tbl$Metric == "Hit rate"]
    hit_acted <- hit_tbl$ActedOnly[hit_tbl$Metric == "Hit rate"]
    coverage <- hit_tbl$Overall[hit_tbl$Metric == "Coverage"]

    # ---- N
    n_obs <- nrow(res_static$evaluation_data)

    by_date[[i]] <- tibble(
      date = d,
      ic_all = ic_all,
      ic_acted = ic_acted,
      hit_overall = hit_overall,
      hit_acted = hit_acted,
      coverage = coverage,
      n = n_obs
    )
  }

  by_date <- bind_rows(by_date) %>% arrange(date)

  # -----------------------
  # Simple time-series summaries
  # -----------------------
  ts_summary <- tibble(
    Metric = c("IC (All)", "IC (Acted)", "Hit (Acted)", "Coverage"),
    Mean = c(
      mean(by_date$ic_all, na.rm = TRUE),
      mean(by_date$ic_acted, na.rm = TRUE),
      mean(by_date$hit_acted, na.rm = TRUE),
      mean(by_date$coverage, na.rm = TRUE)
    ),
    SD = c(
      sd(by_date$ic_all, na.rm = TRUE),
      sd(by_date$ic_acted, na.rm = TRUE),
      sd(by_date$hit_acted, na.rm = TRUE),
      sd(by_date$coverage, na.rm = TRUE)
    )
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))

  # -----------------------
  # Return
  # -----------------------
  list(
    by_date = by_date,
    ts_summary = ts_summary,
    static_results = static_results,
    Meta = Meta,
    return_threshold = return_threshold
  )
}
