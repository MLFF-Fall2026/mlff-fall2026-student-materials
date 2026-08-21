source("Supporting/LeaderBoardDemo/R/common.R")

fetch_quantmod_fallback <- function(symbol, from, to) {
  x <- tryCatch(
    quantmod::getSymbols(
      Symbols = symbol,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE,
      warnings = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(x) || NROW(x) == 0L) {
    return(tibble())
  }

  adjusted <- tryCatch(as.numeric(quantmod::Ad(x)), error = function(e) {
    rep(NA_real_, NROW(x))
  })
  close <- tryCatch(as.numeric(quantmod::Cl(x)), error = function(e) {
    rep(NA_real_, NROW(x))
  })

  tibble(
    symbol = symbol,
    date = as.Date(zoo::index(x)),
    close = close,
    adjusted = adjusted
  )
}

fetch_market_prices <- function(symbols, from, to) {
  symbols <- unique(normalize_ticker(symbols))

  message("Fetching Yahoo Finance data for ", length(symbols), " symbols...")
  raw <- tryCatch(
    tidyquant::tq_get(
      symbols,
      get = "stock.prices",
      from = from,
      to = as.Date(to) + 1,
      complete_cases = FALSE
    ),
    error = function(e) tibble()
  )

  if (nrow(raw) > 0L) {
    raw <- raw |>
      transmute(
        symbol = normalize_ticker(symbol),
        date = as.Date(date),
        close = as.numeric(close),
        adjusted = as.numeric(adjusted)
      )
  }

  missing_symbols <- setdiff(symbols, unique(raw$symbol %||% character()))
  if (length(missing_symbols) > 0L) {
    fallback <- purrr::map_dfr(
      missing_symbols,
      ~ fetch_quantmod_fallback(.x, from = from, to = as.Date(to) + 1)
    )
    raw <- bind_rows(raw, fallback)
  }

  if (nrow(raw) == 0L) {
    stop(
      "Yahoo Finance returned no market data. Check network access and ticker availability."
    )
  }

  raw |>
    mutate(
      price = dplyr::coalesce(adjusted, close),
      price_source = case_when(
        !is.na(adjusted) ~ "adjusted",
        is.na(adjusted) & !is.na(close) ~ "close_fallback",
        TRUE ~ NA_character_
      )
    ) |>
    filter(!is.na(price)) |>
    arrange(symbol, date)
}

build_market_panel <- function(
  raw_prices,
  benchmark,
  official_start,
  official_end
) {
  benchmark <- normalize_ticker(benchmark)
  benchmark_prices <- raw_prices |>
    filter(symbol == benchmark) |>
    arrange(date)

  if (nrow(benchmark_prices) == 0L) {
    stop("Benchmark ", benchmark, " was not returned by Yahoo Finance.")
  }

  official_days <- benchmark_prices$date[
    benchmark_prices$date >= official_start &
      benchmark_prices$date <= official_end
  ]
  if (length(official_days) == 0L) {
    stop(
      "No benchmark trading days exist inside the requested prototype period."
    )
  }

  prior_day <- previous_trading_day(min(official_days), benchmark_prices$date)
  if (is.na(prior_day)) {
    stop(
      "A benchmark observation before prototype inception is required to calculate Day 1 returns."
    )
  }

  panel_days <- sort(unique(c(prior_day, official_days)))
  symbols <- sort(unique(raw_prices$symbol))

  panel <- tidyr::crossing(symbol = symbols, date = panel_days) |>
    left_join(
      raw_prices |>
        select(
          symbol,
          date,
          observed_price = price,
          observed_source = price_source
        ),
      by = c("symbol", "date")
    ) |>
    group_by(symbol) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      last_observed_date = if (any(!is.na(observed_price))) {
        max(date[!is.na(observed_price)])
      } else {
        as.Date(NA_character_)
      },
      price = zoo::na.locf(observed_price, na.rm = FALSE),
      carried = is.na(observed_price) & !is.na(price),
      market_status = case_when(
        !is.na(observed_price) ~ "ok",
        carried &
          !is.na(last_observed_date) &
          date <= last_observed_date ~ "market_closed_carry_forward",
        TRUE ~ "pending"
      ),
      price_source = case_when(
        market_status == "market_closed_carry_forward" ~ "carry_forward",
        market_status == "ok" ~ observed_source,
        TRUE ~ NA_character_
      ),
      asset_return = if_else(
        market_status == "pending" | lag(market_status) == "pending",
        NA_real_,
        price / lag(price) - 1
      )
    ) |>
    ungroup()

  available_symbols <- raw_prices |>
    distinct(symbol) |>
    pull(symbol)

  list(
    raw_prices = raw_prices,
    panel = panel,
    official_days = official_days,
    prior_day = prior_day,
    available_symbols = available_symbols
  )
}
