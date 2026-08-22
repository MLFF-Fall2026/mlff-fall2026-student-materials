# =============================================================================
# 02_update_market_data.R
# =============================================================================
# Purpose
# -------
# Retrieve fresh, time-sensitive market data for the fixed ticker list created
# by 01_build_universe.R.
#
# This script answers the measurement question:
#   "What are the current market characteristics of the fixed sample?"
#
# It reads data/universe/ticker_master_latest.rds, downloads a new snapshot of
# quote fields and recent daily prices, computes 30-trading-day liquidity, and
# writes both time-stamped files and stable *_latest aliases.
#
# Run frequency
# -------------
# Run this script whenever fresh prices, market capitalization, valuation
# ratios, or recent liquidity measures are needed. Market data are ALWAYS
# downloaded during the current execution. This script never reuses a cached
# quote or price-history dataset.
#
# Promotion safeguard
# -------------------
# A new run replaces the *_latest market files only after basic quality checks
# pass. If the provider is broadly unavailable or the successful-data rate is
# unacceptably low, diagnostic logs are written and the script stops without
# replacing the prior successful *_latest snapshot.
#
# Primary outputs
# ---------------
# data/market/market_snapshot_<timestamp>.{csv,rds}
# data/market/market_snapshot_latest.{csv,rds}
# data/market/price_history_<timestamp>.{csv,rds}
# data/market/price_history_latest.{csv,rds}
# data/market/market_metadata_<timestamp>.{csv,rds}
# data/market/market_metadata_latest.{csv,rds}
# data/market/market_quality_<timestamp>.{csv,rds}
# data/market/market_quality_latest.{csv,rds}
# data/logs/quote_log_<timestamp>.{csv,rds}
# data/logs/price_log_<timestamp>.{csv,rds}
# data/logs/failed_symbols_<timestamp>.{csv,rds}
# =============================================================================

# =============================================================================
# Section 1: Package checks and user-adjustable configuration
# =============================================================================
# This section verifies dependencies and defines the lookback window, batching,
# retry behavior, and minimum quality required before a snapshot becomes latest.

required_packages <- c(
  "tidyverse",
  "tidyquant",
  "quantmod"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Install the following required packages before running this script: ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyquant)
  library(quantmod)
})

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  width = 120
)

# Market-data settings.
# lookback_calendar_days must cover enough *trading* days for the longest
# return horizon computed downstream (Module 7's 60-trading-day return).
# 60 trading days spans roughly 84-90 calendar days once weekends and
# US market holidays are excluded; 100 leaves a safety margin so short
# holiday-heavy stretches still yield a full 60-trading-day window.
lookback_calendar_days <- 100L
liquidity_trading_days <- 30L
minimum_valid_trading_days <- 20L

# Batch sizes limit the scope of individual provider calls. The current
# quantmod Yahoo implementation splits direct quote requests above 99 symbols;
# 90 leaves a small safety margin and keeps our own batch logs easy to read.
quote_batch_size <- 90L
provider_quote_limit <- 99L
price_batch_size <- 50L

# Retry and pacing settings.
max_retry_attempts <- 3L
max_recursive_split_depth <- 2L
retry_base_delay_seconds <- 1
pause_between_batches_seconds <- 0.35

# A poor partial download should not silently become the new "latest" dataset.
# These thresholds are deliberately configurable near the top of the script.
minimum_market_data_success_rate <- 0.50
minimum_analysis_eligible_rate <- 0.20

# =============================================================================
# Section 2: Project paths and snapshot identifiers
# =============================================================================
# This section resolves the project root, creates output folders, and defines a
# unique ID that links every file written by this market-data run.

get_script_directory <- function() {
  command_arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_arguments, value = TRUE)

  if (length(file_argument) > 0L) {
    script_path <- sub("^--file=", "", file_argument[[1]])
    return(dirname(normalizePath(script_path, mustWork = FALSE)))
  }

  normalizePath(getwd(), mustWork = FALSE)
}

project_directory <- get_script_directory()
universe_directory <- file.path(project_directory, "data", "universe")
market_directory <- file.path(project_directory, "data", "market")
logs_directory <- file.path(project_directory, "data", "logs")

dir.create(market_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_directory, recursive = TRUE, showWarnings = FALSE)

snapshot_started_at <- Sys.time()
market_data_as_of <- as.Date(snapshot_started_at)
market_snapshot_id <- paste0(
  "M_",
  format(snapshot_started_at, "%Y-%m-%d_%H%M%S")
)
file_timestamp <- format(snapshot_started_at, "%Y-%m-%d_%H%M%S")

price_from_date <- market_data_as_of - lookback_calendar_days
price_to_date <- market_data_as_of + 1L

# =============================================================================
# Section 3: General cleaning, validation, and file-output helpers
# =============================================================================
# These functions standardize source output and ensure that dated and latest
# files are written consistently. The latest aliases are updated only after the
# snapshot-level quality checks pass near the end of the script.

snake_names <- function(x) {
  x |>
    stringr::str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "") |>
    stringr::str_to_lower()
}

clean_text_na <- function(x) {
  cleaned <- as.character(x)
  cleaned <- stringr::str_squish(cleaned)

  missing_tokens <- c(
    "",
    "NA",
    "N/A",
    "NONE",
    "NULL",
    "NAN",
    "--",
    "-"
  )

  cleaned[stringr::str_to_upper(cleaned) %in% missing_tokens] <- NA_character_
  cleaned
}

safe_numeric <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  cleaned <- clean_text_na(x)
  cleaned <- stringr::str_replace_all(cleaned, ",", "")
  cleaned <- stringr::str_replace_all(cleaned, "\\$", "")
  cleaned <- stringr::str_replace_all(cleaned, "%", "")

  suppressWarnings(as.numeric(cleaned))
}

positive_or_na <- function(x) {
  cleaned <- safe_numeric(x)
  cleaned[!is.finite(cleaned) | cleaned <= 0] <- NA_real_
  cleaned
}

ensure_columns <- function(data, columns) {
  missing_columns <- setdiff(columns, names(data))

  for (column in missing_columns) {
    data[[column]] <- rep(NA, nrow(data))
  }

  data
}

first_existing_column <- function(data, candidates, required = TRUE) {
  matches <- intersect(candidates, names(data))

  if (length(matches) > 0L) {
    return(matches[[1]])
  }

  if (required) {
    stop(
      paste0(
        "None of the required columns were found: ",
        paste(candidates, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  NULL
}

column_or_default <- function(data, candidates, default_value = NA) {
  column_name <- first_existing_column(data, candidates, required = FALSE)

  if (is.null(column_name)) {
    return(rep(default_value, nrow(data)))
  }

  data[[column_name]]
}

normalize_yahoo_symbol <- function(x) {
  normalized <- clean_text_na(x)
  normalized <- stringr::str_to_upper(normalized)
  normalized <- stringr::str_replace_all(normalized, "\\s+", "")
  normalized <- stringr::str_replace(
    normalized,
    "^([A-Z0-9]+)\\^([A-Z0-9]+)$",
    "\\1-P\\2"
  )
  normalized <- stringr::str_replace(
    normalized,
    "^([A-Z0-9]+)[.]PR([A-Z0-9]+)$",
    "\\1-P\\2"
  )
  normalized <- stringr::str_replace(
    normalized,
    "^([A-Z0-9]+)-PR([A-Z0-9]+)$",
    "\\1-P\\2"
  )
  normalized <- stringr::str_replace_all(normalized, "[./]", "-")
  normalized <- stringr::str_replace_all(normalized, "\\$", "-P")
  normalized <- stringr::str_replace_all(normalized, "[^A-Z0-9=^-]", "")
  normalized[normalized == ""] <- NA_character_

  normalized
}

safe_rate <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }

  numerator / denominator
}

safe_median <- function(x) {
  finite_values <- x[is.finite(x)]

  if (length(finite_values) == 0L) {
    return(NA_real_)
  }

  stats::median(finite_values)
}

split_vector <- function(x, size) {
  if (length(x) == 0L) {
    return(list())
  }

  size <- max(1L, as.integer(size))
  split(x, ceiling(seq_along(x) / size))
}

validate_required_columns <- function(data, required_columns, object_name) {
  if (!is.data.frame(data)) {
    stop(object_name, " is not a data frame.", call. = FALSE)
  }

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        object_name,
        " is missing required columns: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

write_dataset_versions <- function(data, directory, stem, timestamp) {
  dated_rds <- file.path(directory, paste0(stem, "_", timestamp, ".rds"))
  dated_csv <- file.path(directory, paste0(stem, "_", timestamp, ".csv"))
  latest_rds <- file.path(directory, paste0(stem, "_latest.rds"))
  latest_csv <- file.path(directory, paste0(stem, "_latest.csv"))

  saveRDS(data, dated_rds)
  readr::write_csv(data, dated_csv, na = "")

  copied_rds <- file.copy(dated_rds, latest_rds, overwrite = TRUE)
  copied_csv <- file.copy(dated_csv, latest_csv, overwrite = TRUE)

  if (!isTRUE(copied_rds) || !isTRUE(copied_csv)) {
    stop(
      paste0("Unable to update the latest files for dataset: ", stem),
      call. = FALSE
    )
  }

  tibble::tibble(
    dataset = stem,
    dated_rds = dated_rds,
    dated_csv = dated_csv,
    latest_rds = latest_rds,
    latest_csv = latest_csv
  )
}

write_archival_log <- function(data, directory, stem, timestamp) {
  dated_rds <- file.path(directory, paste0(stem, "_", timestamp, ".rds"))
  dated_csv <- file.path(directory, paste0(stem, "_", timestamp, ".csv"))

  saveRDS(data, dated_rds)
  readr::write_csv(data, dated_csv, na = "")

  tibble::tibble(
    dataset = stem,
    dated_rds = dated_rds,
    dated_csv = dated_csv
  )
}

# =============================================================================
# Section 4: Load and validate the fixed ticker master
# =============================================================================
# Stage 2 deliberately depends on the static output of Stage 1. The universe ID
# and symbol hash are carried into every market-data output so Stage 3 can stop
# if someone accidentally mixes files from different universe builds.

ticker_master_path <- file.path(
  universe_directory,
  "ticker_master_latest.rds"
)
universe_metadata_path <- file.path(
  universe_directory,
  "universe_metadata_latest.rds"
)

required_input_files <- c(ticker_master_path, universe_metadata_path)
missing_input_files <- required_input_files[!file.exists(required_input_files)]

if (length(missing_input_files) > 0L) {
  stop(
    paste0(
      "Required Stage 1 files are missing: ",
      paste(missing_input_files, collapse = ", "),
      ". Run 01_build_universe.R first."
    ),
    call. = FALSE
  )
}

ticker_master_df <- readRDS(ticker_master_path)
universe_metadata_df <- readRDS(universe_metadata_path)

validate_required_columns(
  ticker_master_df,
  c(
    "symbol_yahoo",
    "company",
    "security_type",
    "is_sp500",
    "sector_clean",
    "industry_clean",
    "universe_build_id",
    "universe_symbol_hash"
  ),
  "ticker_master_df"
)

validate_required_columns(
  universe_metadata_df,
  c(
    "universe_build_id",
    "universe_as_of",
    "universe_symbol_hash"
  ),
  "universe_metadata_df"
)

if (nrow(universe_metadata_df) != 1L) {
  stop(
    "universe_metadata_latest.rds must contain exactly one metadata row.",
    call. = FALSE
  )
}

input_universe_build_id <- as.character(
  universe_metadata_df$universe_build_id[[1]]
)
input_universe_symbol_hash <- as.character(
  universe_metadata_df$universe_symbol_hash[[1]]
)
input_universe_as_of <- as.Date(
  universe_metadata_df$universe_as_of[[1]]
)

if (
  any(ticker_master_df$universe_build_id != input_universe_build_id) ||
    any(ticker_master_df$universe_symbol_hash != input_universe_symbol_hash)
) {
  stop(
    "The ticker master and universe metadata do not describe the same universe build.",
    call. = FALSE
  )
}

download_symbols <- ticker_master_df |>
  dplyr::filter(!is.na(symbol_yahoo)) |>
  dplyr::pull(symbol_yahoo) |>
  unique() |>
  sort()

if (length(download_symbols) == 0L) {
  stop("The ticker master contains no usable Yahoo symbols.", call. = FALSE)
}

# =============================================================================
# Section 5: Condition capture, retries, and provider-failure detection
# =============================================================================
# These helpers preserve partial results, retry temporary failures, and detect
# provider-wide problems. A global failure stops additional batches so the
# script does not repeatedly send requests during a broad outage.

capture_call <- function(function_to_run) {
  captured_warnings <- character()

  result <- tryCatch(
    withCallingHandlers(
      function_to_run(),
      warning = function(warning_condition) {
        captured_warnings <<- c(
          captured_warnings,
          conditionMessage(warning_condition)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      structure(
        list(message = conditionMessage(error_condition)),
        class = "captured_error"
      )
    }
  )

  if (inherits(result, "captured_error")) {
    return(list(
      ok = FALSE,
      value = NULL,
      warnings = unique(captured_warnings),
      error = result$message
    ))
  }

  list(
    ok = TRUE,
    value = result,
    warnings = unique(captured_warnings),
    error = NA_character_
  )
}

retry_call <- function(
  function_to_run,
  attempts = max_retry_attempts,
  base_delay_seconds = retry_base_delay_seconds
) {
  attempts <- max(1L, as.integer(attempts))
  warning_messages <- character()
  error_messages <- character()

  for (attempt_number in seq_len(attempts)) {
    attempt <- capture_call(function_to_run)
    warning_messages <- c(warning_messages, attempt$warnings)

    if (isTRUE(attempt$ok)) {
      return(list(
        ok = TRUE,
        value = attempt$value,
        warnings = unique(warning_messages),
        errors = unique(error_messages),
        attempts = attempt_number
      ))
    }

    error_messages <- c(error_messages, attempt$error)

    if (attempt_number < attempts) {
      Sys.sleep(base_delay_seconds * 2^(attempt_number - 1L))
    }
  }

  list(
    ok = FALSE,
    value = NULL,
    warnings = unique(warning_messages),
    errors = unique(error_messages),
    attempts = attempts
  )
}

empty_condition_log <- function() {
  tibble::tibble(
    market_snapshot_id = character(),
    stage = character(),
    batch_id = character(),
    symbol_yahoo = character(),
    status = character(),
    message = character()
  )
}

condition_log_rows <- function(attempt, stage, batch_id, symbols) {
  warning_rows <- if (length(attempt$warnings) > 0L) {
    tibble::tibble(
      market_snapshot_id = market_snapshot_id,
      stage = stage,
      batch_id = batch_id,
      symbol_yahoo = NA_character_,
      status = "warning",
      message = attempt$warnings
    )
  } else {
    empty_condition_log()
  }

  error_rows <- if (length(attempt$errors) > 0L) {
    tibble::tibble(
      market_snapshot_id = market_snapshot_id,
      stage = stage,
      batch_id = batch_id,
      symbol_yahoo = if (length(symbols) == 1L) symbols else NA_character_,
      status = "error",
      message = attempt$errors
    )
  } else {
    empty_condition_log()
  }

  dplyr::bind_rows(warning_rows, error_rows)
}

is_global_download_error <- function(messages) {
  messages <- unique(stats::na.omit(messages))

  if (length(messages) == 0L) {
    return(FALSE)
  }

  combined_message <- stringr::str_to_lower(
    paste(messages, collapse = " | ")
  )

  stringr::str_detect(
    combined_message,
    paste0(
      "crumb|consent|could not resolve|failed to connect|connection refused|",
      "cannot open.*url|cannot open the connection|download failed|",
      "timed out|timeout|ssl|rate limit|too many requests|",
      "http[^0-9]*(401|403|429|500|502|503|504)|service unavailable"
    )
  )
}

# =============================================================================
# Section 6: Retrieve fresh snapshot quote fields
# =============================================================================
# This section obtains current price, market capitalization, shares
# outstanding, trailing P/E, price-to-book, and provider-reported average
# volume. The requested symbols
# are divided into batches, failed batches are retried, and non-global failures
# can be recursively split to isolate problematic symbols.

quote_fields <- c(
  "regularMarketPrice",
  "marketCap",
  "trailingPE",
  "priceToBook",
  "sharesOutstanding",
  "averageDailyVolume3Month",
  "averageDailyVolume10Day",
  "epsTrailingTwelveMonths",
  "epsForward",
  "earningsTimestamp",
  "fiftyTwoWeekHigh",
  "fiftyTwoWeekLow",
  "fiftyTwoWeekChangePercent"
)

empty_quote_rows <- function(symbols) {
  tibble::tibble(
    symbol_yahoo = symbols,
    quote_trade_time = NA_character_,
    quote_price = NA_real_,
    market_cap_quote = NA_real_,
    trailing_pe = NA_real_,
    price_to_book = NA_real_,
    shares_outstanding_quote = NA_real_,
    average_daily_volume_3m_quote = NA_real_,
    average_daily_volume_10d_quote = NA_real_,
    eps_trailing_ttm = NA_real_,
    eps_forward = NA_real_,
    earnings_timestamp = NA_character_,
    fifty_two_week_high = NA_real_,
    fifty_two_week_low = NA_real_,
    return_52_week_pct = NA_real_,
    quote_success = FALSE
  )
}

fetch_quote_batch_once <- function(symbols) {
  raw_quotes <- quantmod::getQuote(
    Symbols = symbols,
    src = "yahoo",
    what = quote_fields
  )

  if (!is.data.frame(raw_quotes) || nrow(raw_quotes) == 0L) {
    stop("Yahoo returned no quote rows for this batch.", call. = FALSE)
  }

  raw_quote_frame <- as.data.frame(
    raw_quotes,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  raw_quote_frame$row_symbol <- rownames(raw_quote_frame)
  rownames(raw_quote_frame) <- NULL
  names(raw_quote_frame) <- snake_names(names(raw_quote_frame))

  symbol_values <- column_or_default(
    raw_quote_frame,
    c("symbol", "ticker", "row_symbol"),
    default_value = NA_character_
  )
  trade_time_values <- column_or_default(
    raw_quote_frame,
    c("regular_market_time", "trade_time", "last_trade_time"),
    default_value = NA_character_
  )
  price_values <- column_or_default(
    raw_quote_frame,
    c(
      "regular_market_price",
      "last",
      "last_trade_price_only",
      "price"
    ),
    default_value = NA_real_
  )
  market_cap_values <- column_or_default(
    raw_quote_frame,
    c("market_cap", "market_capitalization"),
    default_value = NA_real_
  )
  trailing_pe_values <- column_or_default(
    raw_quote_frame,
    c("trailing_pe", "p_e_ratio", "pe_ratio"),
    default_value = NA_real_
  )
  price_to_book_values <- column_or_default(
    raw_quote_frame,
    c("price_to_book", "price_book"),
    default_value = NA_real_
  )
  shares_outstanding_values <- column_or_default(
    raw_quote_frame,
    c("shares_outstanding"),
    default_value = NA_real_
  )
  average_volume_3m_values <- column_or_default(
    raw_quote_frame,
    c(
      "average_daily_volume3_month",
      "average_daily_volume_3_month",
      "average_daily_volume"
    ),
    default_value = NA_real_
  )
  average_volume_10d_values <- column_or_default(
    raw_quote_frame,
    c(
      "average_daily_volume10_day",
      "average_daily_volume_10_day"
    ),
    default_value = NA_real_
  )
  eps_trailing_ttm_values <- column_or_default(
    raw_quote_frame,
    c("eps_trailing_twelve_months"),
    default_value = NA_real_
  )
  eps_forward_values <- column_or_default(
    raw_quote_frame,
    c("eps_forward"),
    default_value = NA_real_
  )
  earnings_timestamp_values <- column_or_default(
    raw_quote_frame,
    c("earnings_timestamp"),
    default_value = NA_character_
  )
  fifty_two_week_high_values <- column_or_default(
    raw_quote_frame,
    c("fifty_two_week_high"),
    default_value = NA_real_
  )
  fifty_two_week_low_values <- column_or_default(
    raw_quote_frame,
    c("fifty_two_week_low"),
    default_value = NA_real_
  )
  fifty_two_week_change_pct_values <- column_or_default(
    raw_quote_frame,
    c("fifty_two_week_change_percent"),
    default_value = NA_real_
  )

  quote_data <- tibble::tibble(
    symbol_yahoo = normalize_yahoo_symbol(symbol_values),
    quote_trade_time = as.character(trade_time_values),
    quote_price = positive_or_na(price_values),
    market_cap_quote = positive_or_na(market_cap_values),
    trailing_pe = safe_numeric(trailing_pe_values),
    price_to_book = safe_numeric(price_to_book_values),
    shares_outstanding_quote = positive_or_na(shares_outstanding_values),
    average_daily_volume_3m_quote = positive_or_na(
      average_volume_3m_values
    ),
    average_daily_volume_10d_quote = positive_or_na(
      average_volume_10d_values
    ),
    eps_trailing_ttm = safe_numeric(eps_trailing_ttm_values),
    eps_forward = safe_numeric(eps_forward_values),
    earnings_timestamp = as.character(earnings_timestamp_values),
    fifty_two_week_high = positive_or_na(fifty_two_week_high_values),
    fifty_two_week_low = positive_or_na(fifty_two_week_low_values),
    return_52_week_pct = safe_numeric(fifty_two_week_change_pct_values)
  ) |>
    dplyr::filter(!is.na(symbol_yahoo)) |>
    dplyr::distinct(symbol_yahoo, .keep_all = TRUE) |>
    dplyr::mutate(
      quote_success = dplyr::if_any(
        c(
          quote_price,
          market_cap_quote,
          trailing_pe,
          price_to_book,
          shares_outstanding_quote,
          average_daily_volume_3m_quote,
          average_daily_volume_10d_quote
        ),
        ~ is.finite(.x)
      )
    )

  # Rejoin to the requested vector so every requested symbol receives a row,
  # including symbols for which the provider returned no usable quote fields.
  tibble::tibble(symbol_yahoo = symbols) |>
    dplyr::left_join(quote_data, by = "symbol_yahoo") |>
    dplyr::mutate(quote_success = tidyr::replace_na(quote_success, FALSE))
}

fetch_quote_resilient <- function(symbols, batch_id, depth = 0L) {
  attempt <- retry_call(
    function() fetch_quote_batch_once(symbols),
    attempts = max_retry_attempts
  )

  attempt_log <- condition_log_rows(
    attempt,
    stage = "snapshot_quote",
    batch_id = batch_id,
    symbols = symbols
  )

  if (isTRUE(attempt$ok)) {
    quote_data <- attempt$value
    global_failure <-
      !any(quote_data$quote_success, na.rm = TRUE) &&
      is_global_download_error(attempt$warnings)

    no_data_log <- quote_data |>
      dplyr::filter(!quote_success) |>
      dplyr::transmute(
        market_snapshot_id = market_snapshot_id,
        stage = "snapshot_quote",
        batch_id = batch_id,
        symbol_yahoo = symbol_yahoo,
        status = dplyr::if_else(
          global_failure,
          "global_failure",
          "no_data"
        ),
        message = dplyr::if_else(
          global_failure,
          paste0(
            "No usable quote data were returned and the provider emitted ",
            "a connection, authorization, or rate-limit warning."
          ),
          "No requested quote field was returned."
        )
      )

    return(list(
      data = quote_data,
      log = dplyr::bind_rows(attempt_log, no_data_log),
      global_failure = global_failure
    ))
  }

  global_failure <- is_global_download_error(
    c(attempt$errors, attempt$warnings)
  )
  may_split <-
    !global_failure &&
    length(symbols) > 1L &&
    depth < max_recursive_split_depth

  if (may_split) {
    split_point <- ceiling(length(symbols) / 2L)
    left_symbols <- symbols[seq_len(split_point)]
    right_symbols <- symbols[-seq_len(split_point)]

    left_result <- fetch_quote_resilient(
      left_symbols,
      paste0(batch_id, "L"),
      depth = depth + 1L
    )
    right_result <- fetch_quote_resilient(
      right_symbols,
      paste0(batch_id, "R"),
      depth = depth + 1L
    )

    return(list(
      data = dplyr::bind_rows(left_result$data, right_result$data),
      log = dplyr::bind_rows(
        attempt_log,
        left_result$log,
        right_result$log
      ),
      global_failure = isTRUE(left_result$global_failure) &&
        isTRUE(right_result$global_failure)
    ))
  }

  failure_message <- paste(
    unique(c(attempt$errors, attempt$warnings)),
    collapse = " | "
  )

  if (identical(failure_message, "")) {
    failure_message <- "The quote batch failed without an error message."
  }

  list(
    data = empty_quote_rows(symbols),
    log = dplyr::bind_rows(
      attempt_log,
      tibble::tibble(
        market_snapshot_id = market_snapshot_id,
        stage = "snapshot_quote",
        batch_id = batch_id,
        symbol_yahoo = symbols,
        status = if (global_failure) "global_failure" else "failed_batch",
        message = failure_message
      )
    ),
    global_failure = global_failure
  )
}

fetch_all_quotes <- function(symbols) {
  effective_batch_size <- min(
    as.integer(quote_batch_size),
    as.integer(provider_quote_limit)
  )
  batches <- split_vector(symbols, effective_batch_size)
  results <- vector("list", length(batches))

  if (length(batches) == 0L) {
    return(list(
      data = empty_quote_rows(character()),
      log = empty_condition_log(),
      requested_symbols = symbols,
      effective_batch_size = effective_batch_size,
      fetched_at = Sys.time()
    ))
  }

  for (batch_number in seq_along(batches)) {
    message(
      "Retrieving quote batch ",
      batch_number,
      " of ",
      length(batches),
      "."
    )

    results[[batch_number]] <- fetch_quote_resilient(
      batches[[batch_number]],
      sprintf("Q%04d", batch_number)
    )

    if (isTRUE(results[[batch_number]]$global_failure)) {
      remaining_batches <- if (batch_number < length(batches)) {
        seq.int(batch_number + 1L, length(batches))
      } else {
        integer()
      }

      for (remaining_number in remaining_batches) {
        skipped_symbols <- batches[[remaining_number]]
        results[[remaining_number]] <- list(
          data = empty_quote_rows(skipped_symbols),
          log = tibble::tibble(
            market_snapshot_id = market_snapshot_id,
            stage = "snapshot_quote",
            batch_id = sprintf("Q%04d", remaining_number),
            symbol_yahoo = skipped_symbols,
            status = "skipped_after_global_failure",
            message = paste0(
              "Skipped after a provider-wide quote failure was detected."
            )
          ),
          global_failure = TRUE
        )
      }

      break
    }

    if (batch_number < length(batches)) {
      Sys.sleep(pause_between_batches_seconds)
    }
  }

  quote_data <- purrr::map_dfr(results, "data") |>
    dplyr::arrange(symbol_yahoo) |>
    dplyr::distinct(symbol_yahoo, .keep_all = TRUE)

  quote_data <- tibble::tibble(symbol_yahoo = symbols) |>
    dplyr::left_join(quote_data, by = "symbol_yahoo") |>
    dplyr::mutate(quote_success = tidyr::replace_na(quote_success, FALSE))

  list(
    data = quote_data,
    log = purrr::map_dfr(results, "log"),
    requested_symbols = symbols,
    effective_batch_size = effective_batch_size,
    fetched_at = Sys.time()
  )
}

quote_fetch_result <- fetch_all_quotes(download_symbols)
quote_df <- quote_fetch_result$data
quote_log <- dplyr::bind_rows(
  empty_condition_log(),
  quote_fetch_result$log
)

# =============================================================================
# Section 7: Retrieve fresh daily prices and calculate 30-day liquidity
# =============================================================================
# Historical prices are requested for a calendar window longer than 30 days.
# For each symbol, the script keeps the most recent 30 valid trading rows and
# calculates average share volume and average dollar volume using unadjusted
# close multiplied by daily volume. The liquidity metrics are set to NA when
# fewer than the configured minimum valid trading days are available.

empty_price_data <- function() {
  tibble::tibble(
    symbol_yahoo = character(),
    date = as.Date(character()),
    open = numeric(),
    high = numeric(),
    low = numeric(),
    close = numeric(),
    volume = numeric(),
    adjusted = numeric()
  )
}

fetch_price_batch_once <- function(symbols) {
  # complete_cases = TRUE returns a flat tibble for successful symbols and
  # drops failed symbols with a warning. Missing symbols are identified by
  # comparing the result with the requested vector below.
  raw_prices <- tidyquant::tq_get(
    x = symbols,
    get = "stock.prices",
    from = price_from_date,
    to = price_to_date,
    complete_cases = TRUE
  )

  if (!is.data.frame(raw_prices)) {
    stop("Yahoo returned an invalid historical-price object.", call. = FALSE)
  }

  if (nrow(raw_prices) == 0L) {
    return(empty_price_data())
  }

  names(raw_prices) <- snake_names(names(raw_prices))

  if (!("symbol" %in% names(raw_prices))) {
    if (length(symbols) == 1L) {
      raw_prices$symbol <- symbols[[1]]
    } else {
      stop(
        paste0(
          "Historical prices for a multi-symbol batch did not include ",
          "a symbol column."
        ),
        call. = FALSE
      )
    }
  }

  raw_prices <- ensure_columns(
    raw_prices,
    c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted")
  )

  raw_prices |>
    dplyr::transmute(
      symbol_yahoo = normalize_yahoo_symbol(symbol),
      date = as.Date(date),
      open = safe_numeric(open),
      high = safe_numeric(high),
      low = safe_numeric(low),
      close = safe_numeric(close),
      volume = safe_numeric(volume),
      adjusted = safe_numeric(adjusted)
    ) |>
    dplyr::filter(!is.na(symbol_yahoo), !is.na(date)) |>
    dplyr::distinct(symbol_yahoo, date, .keep_all = TRUE)
}

fetch_price_resilient <- function(symbols, batch_id, depth = 0L) {
  attempt <- retry_call(
    function() fetch_price_batch_once(symbols),
    attempts = max_retry_attempts
  )

  attempt_log <- condition_log_rows(
    attempt,
    stage = "historical_price",
    batch_id = batch_id,
    symbols = symbols
  )

  if (isTRUE(attempt$ok)) {
    price_data <- attempt$value
    successful_symbols <- price_data |>
      dplyr::filter(
        !is.na(date),
        is.finite(close),
        close > 0,
        is.finite(volume),
        volume >= 0
      ) |>
      dplyr::distinct(symbol_yahoo) |>
      dplyr::pull(symbol_yahoo)

    missing_symbols <- setdiff(symbols, successful_symbols)
    global_failure <-
      length(successful_symbols) == 0L &&
      is_global_download_error(attempt$warnings)

    no_data_log <- if (length(missing_symbols) > 0L) {
      tibble::tibble(
        market_snapshot_id = market_snapshot_id,
        stage = "historical_price",
        batch_id = batch_id,
        symbol_yahoo = missing_symbols,
        status = if (global_failure) "global_failure" else "no_data",
        message = if (global_failure) {
          paste0(
            "No usable price data were returned and the provider emitted ",
            "a connection, authorization, or rate-limit warning."
          )
        } else {
          "No valid close-and-volume observation was returned."
        }
      )
    } else {
      empty_condition_log()
    }

    return(list(
      data = price_data,
      log = dplyr::bind_rows(attempt_log, no_data_log),
      global_failure = global_failure
    ))
  }

  global_failure <- is_global_download_error(
    c(attempt$errors, attempt$warnings)
  )
  may_split <-
    !global_failure &&
    length(symbols) > 1L &&
    depth < max_recursive_split_depth

  if (may_split) {
    split_point <- ceiling(length(symbols) / 2L)
    left_symbols <- symbols[seq_len(split_point)]
    right_symbols <- symbols[-seq_len(split_point)]

    left_result <- fetch_price_resilient(
      left_symbols,
      paste0(batch_id, "L"),
      depth = depth + 1L
    )
    right_result <- fetch_price_resilient(
      right_symbols,
      paste0(batch_id, "R"),
      depth = depth + 1L
    )

    return(list(
      data = dplyr::bind_rows(left_result$data, right_result$data),
      log = dplyr::bind_rows(
        attempt_log,
        left_result$log,
        right_result$log
      ),
      global_failure = isTRUE(left_result$global_failure) &&
        isTRUE(right_result$global_failure)
    ))
  }

  failure_message <- paste(
    unique(c(attempt$errors, attempt$warnings)),
    collapse = " | "
  )

  if (identical(failure_message, "")) {
    failure_message <- paste0(
      "The historical-price batch failed without an error message."
    )
  }

  list(
    data = empty_price_data(),
    log = dplyr::bind_rows(
      attempt_log,
      tibble::tibble(
        market_snapshot_id = market_snapshot_id,
        stage = "historical_price",
        batch_id = batch_id,
        symbol_yahoo = symbols,
        status = if (global_failure) "global_failure" else "failed_batch",
        message = failure_message
      )
    ),
    global_failure = global_failure
  )
}

fetch_all_prices <- function(symbols) {
  batches <- split_vector(symbols, price_batch_size)
  results <- vector("list", length(batches))

  if (length(batches) == 0L) {
    return(list(
      data = empty_price_data(),
      log = empty_condition_log(),
      requested_symbols = symbols,
      effective_batch_size = price_batch_size,
      from = price_from_date,
      to = price_to_date,
      fetched_at = Sys.time()
    ))
  }

  for (batch_number in seq_along(batches)) {
    message(
      "Retrieving historical-price batch ",
      batch_number,
      " of ",
      length(batches),
      "."
    )

    results[[batch_number]] <- fetch_price_resilient(
      batches[[batch_number]],
      sprintf("P%04d", batch_number)
    )

    if (isTRUE(results[[batch_number]]$global_failure)) {
      remaining_batches <- if (batch_number < length(batches)) {
        seq.int(batch_number + 1L, length(batches))
      } else {
        integer()
      }

      for (remaining_number in remaining_batches) {
        skipped_symbols <- batches[[remaining_number]]
        results[[remaining_number]] <- list(
          data = empty_price_data(),
          log = tibble::tibble(
            market_snapshot_id = market_snapshot_id,
            stage = "historical_price",
            batch_id = sprintf("P%04d", remaining_number),
            symbol_yahoo = skipped_symbols,
            status = "skipped_after_global_failure",
            message = paste0(
              "Skipped after a provider-wide historical-price failure was detected."
            )
          ),
          global_failure = TRUE
        )
      }

      break
    }

    if (batch_number < length(batches)) {
      Sys.sleep(pause_between_batches_seconds)
    }
  }

  price_data <- purrr::map_dfr(results, "data") |>
    dplyr::distinct(symbol_yahoo, date, .keep_all = TRUE) |>
    dplyr::arrange(symbol_yahoo, date)

  list(
    data = price_data,
    log = purrr::map_dfr(results, "log"),
    requested_symbols = symbols,
    effective_batch_size = price_batch_size,
    from = price_from_date,
    to = price_to_date,
    fetched_at = Sys.time()
  )
}

price_fetch_result <- fetch_all_prices(download_symbols)
price_log <- dplyr::bind_rows(
  empty_condition_log(),
  price_fetch_result$log
)

valid_price_history_df <- price_fetch_result$data |>
  dplyr::filter(
    date <= market_data_as_of,
    is.finite(close),
    close > 0,
    is.finite(volume),
    volume >= 0
  ) |>
  dplyr::distinct(symbol_yahoo, date, .keep_all = TRUE) |>
  dplyr::arrange(symbol_yahoo, date)

liquidity_df <- valid_price_history_df |>
  dplyr::group_by(symbol_yahoo) |>
  dplyr::arrange(dplyr::desc(date), .by_group = TRUE) |>
  dplyr::slice_head(n = liquidity_trading_days) |>
  dplyr::summarise(
    valid_trading_days = dplyr::n(),
    latest_history_date = dplyr::first(date),
    earliest_history_date_used = dplyr::last(date),
    latest_close = dplyr::first(close),
    avg_daily_share_volume_raw = mean(volume, na.rm = TRUE),
    avg_daily_dollar_volume_raw = mean(close * volume, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    historical_data_available = valid_trading_days > 0L,
    liquidity_history_adequate = valid_trading_days >=
      minimum_valid_trading_days,
    avg_daily_share_volume_30d = dplyr::if_else(
      liquidity_history_adequate,
      avg_daily_share_volume_raw,
      NA_real_
    ),
    avg_daily_dollar_volume_30d = dplyr::if_else(
      liquidity_history_adequate,
      avg_daily_dollar_volume_raw,
      NA_real_
    )
  )

price_history_df <- valid_price_history_df |>
  dplyr::mutate(
    daily_dollar_volume = close * volume,
    market_snapshot_id = market_snapshot_id,
    market_data_as_of = market_data_as_of,
    market_retrieved_at = snapshot_started_at,
    universe_build_id = input_universe_build_id,
    universe_symbol_hash = input_universe_symbol_hash
  )

# =============================================================================
# Section 8: Assemble the analysis-ready market snapshot
# =============================================================================
# The output keeps every fixed ticker-master symbol, including failed symbols.
# An explicit eligibility flag and exclusion reason make the final analytical
# sample transparent without silently deleting observations in Stage 2.
#
# Current market capitalization comes from the fresh Yahoo market-cap field
# when available. When that field is missing, the script derives a fresh value
# from shares outstanding multiplied by the current reference price. The
# exchange-directory market cap retained in Stage 1 is never used as a current
# value because it belongs to the static universe-build date.

market_snapshot_df <- ticker_master_df |>
  dplyr::left_join(quote_df, by = "symbol_yahoo") |>
  dplyr::left_join(liquidity_df, by = "symbol_yahoo") |>
  dplyr::mutate(
    quote_success = tidyr::replace_na(quote_success, FALSE),
    historical_data_available = tidyr::replace_na(
      historical_data_available,
      FALSE
    ),
    liquidity_history_adequate = tidyr::replace_na(
      liquidity_history_adequate,
      FALSE
    ),
    valid_trading_days = tidyr::replace_na(valid_trading_days, 0L),
    valid_yahoo_data = quote_success | historical_data_available,
    reference_price = dplyr::case_when(
      is.finite(quote_price) & quote_price > 0 ~ quote_price,
      is.finite(latest_close) & latest_close > 0 ~ latest_close,
      TRUE ~ NA_real_
    ),
    price_source = dplyr::case_when(
      is.finite(quote_price) & quote_price > 0 ~ "Fresh Yahoo quote",
      is.finite(latest_close) & latest_close > 0 ~
        "Latest close from fresh history request",
      TRUE ~ "Unavailable"
    ),
    market_cap_from_shares = dplyr::if_else(
      is.finite(shares_outstanding_quote) &
        shares_outstanding_quote > 0 &
        is.finite(reference_price) &
        reference_price > 0,
      shares_outstanding_quote * reference_price,
      NA_real_
    ),
    market_cap = dplyr::case_when(
      is.finite(market_cap_quote) & market_cap_quote > 0 ~ market_cap_quote,
      is.finite(market_cap_from_shares) & market_cap_from_shares > 0 ~
        market_cap_from_shares,
      TRUE ~ NA_real_
    ),
    market_cap_source = dplyr::case_when(
      is.finite(market_cap_quote) & market_cap_quote > 0 ~
        "Fresh Yahoo market-cap quote",
      is.finite(market_cap_from_shares) & market_cap_from_shares > 0 ~
        "Fresh shares outstanding multiplied by current reference price",
      TRUE ~ "Unavailable"
    ),
    trailing_pe = dplyr::if_else(
      is.finite(trailing_pe),
      trailing_pe,
      NA_real_
    ),
    price_to_book = dplyr::if_else(
      is.finite(price_to_book),
      price_to_book,
      NA_real_
    ),
    eps_trailing_ttm = dplyr::if_else(
      is.finite(eps_trailing_ttm),
      eps_trailing_ttm,
      NA_real_
    ),
    eps_forward = dplyr::if_else(
      is.finite(eps_forward),
      eps_forward,
      NA_real_
    ),
    operating_margin = NA_real_,
    analysis_eligible = valid_yahoo_data &
      is.finite(market_cap) &
      market_cap > 0 &
      is.finite(reference_price) &
      reference_price > 0,
    analysis_exclusion_reason = dplyr::case_when(
      analysis_eligible ~ "Eligible",
      !valid_yahoo_data ~ "No valid quote or historical-price data",
      !(is.finite(market_cap) & market_cap > 0) ~
        "Missing positive current market capitalization",
      !(is.finite(reference_price) & reference_price > 0) ~
        "Missing positive current reference price",
      TRUE ~ "Other eligibility failure"
    ),
    benchmark_group = dplyr::if_else(
      is_sp500,
      "S&P 500",
      "Broader Market"
    ),
    market_snapshot_id = market_snapshot_id,
    market_data_as_of = market_data_as_of,
    market_retrieved_at = snapshot_started_at,
    universe_build_id = input_universe_build_id,
    universe_symbol_hash = input_universe_symbol_hash
  ) |>
  dplyr::arrange(symbol_yahoo)

# =============================================================================
# Section 9: Calculate quality diagnostics and failed-symbol logs
# =============================================================================
# These diagnostics determine whether the run is sufficiently complete to
# replace the previous latest files. They also create a concise failure table
# that can be reviewed without reading every provider warning.

requested_symbol_count <- length(download_symbols)
valid_market_data_count <- sum(
  market_snapshot_df$valid_yahoo_data,
  na.rm = TRUE
)
analysis_eligible_count <- sum(
  market_snapshot_df$analysis_eligible,
  na.rm = TRUE
)
quote_success_count <- sum(market_snapshot_df$quote_success, na.rm = TRUE)
history_success_count <- sum(
  market_snapshot_df$historical_data_available,
  na.rm = TRUE
)
adequate_liquidity_count <- sum(
  market_snapshot_df$liquidity_history_adequate,
  na.rm = TRUE
)

market_data_success_rate <- safe_rate(
  valid_market_data_count,
  requested_symbol_count
)
analysis_eligible_rate <- safe_rate(
  analysis_eligible_count,
  requested_symbol_count
)
quote_success_rate <- safe_rate(
  quote_success_count,
  requested_symbol_count
)
history_success_rate <- safe_rate(
  history_success_count,
  requested_symbol_count
)

latest_history_date_overall <- if (nrow(valid_price_history_df) == 0L) {
  as.Date(NA)
} else {
  max(valid_price_history_df$date, na.rm = TRUE)
}

market_quality_df <- dplyr::bind_rows(
  tibble::tibble(
    category = "Retrieval coverage",
    metric = c(
      "Requested symbols",
      "Symbols with usable quote data",
      "Symbols with usable historical data",
      "Symbols with either quote or historical data",
      "Symbols with adequate liquidity history",
      "Analysis-eligible symbols"
    ),
    value = c(
      requested_symbol_count,
      quote_success_count,
      history_success_count,
      valid_market_data_count,
      adequate_liquidity_count,
      analysis_eligible_count
    ),
    rate = c(
      1,
      quote_success_rate,
      history_success_rate,
      market_data_success_rate,
      safe_rate(adequate_liquidity_count, requested_symbol_count),
      analysis_eligible_rate
    )
  ),
  market_snapshot_df |>
    dplyr::count(analysis_exclusion_reason, name = "value") |>
    dplyr::transmute(
      category = "Eligibility outcome",
      metric = analysis_exclusion_reason,
      value = value,
      rate = safe_rate(value, requested_symbol_count)
    )
) |>
  dplyr::mutate(
    market_snapshot_id = market_snapshot_id,
    market_data_as_of = market_data_as_of,
    universe_build_id = input_universe_build_id,
    universe_symbol_hash = input_universe_symbol_hash
  )

failed_symbols_df <- market_snapshot_df |>
  dplyr::filter(!analysis_eligible) |>
  dplyr::select(
    market_snapshot_id,
    symbol_yahoo,
    symbol_original,
    company,
    exchange,
    is_sp500,
    quote_success,
    historical_data_available,
    valid_trading_days,
    market_cap_quote,
    shares_outstanding_quote,
    market_cap_from_shares,
    quote_price,
    latest_close,
    analysis_exclusion_reason
  )

market_metadata_df <- tibble::tibble(
  market_snapshot_id = market_snapshot_id,
  market_data_as_of = market_data_as_of,
  market_retrieved_at = snapshot_started_at,
  latest_history_date = latest_history_date_overall,
  price_from_date = price_from_date,
  price_to_date_exclusive = price_to_date,
  lookback_calendar_days = lookback_calendar_days,
  liquidity_trading_days = liquidity_trading_days,
  minimum_valid_trading_days = minimum_valid_trading_days,
  quote_batch_size = quote_fetch_result$effective_batch_size,
  price_batch_size = price_fetch_result$effective_batch_size,
  requested_symbols = requested_symbol_count,
  quote_success_count = quote_success_count,
  historical_success_count = history_success_count,
  valid_market_data_count = valid_market_data_count,
  analysis_eligible_count = analysis_eligible_count,
  quote_success_rate = quote_success_rate,
  historical_success_rate = history_success_rate,
  market_data_success_rate = market_data_success_rate,
  analysis_eligible_rate = analysis_eligible_rate,
  universe_build_id = input_universe_build_id,
  universe_as_of = input_universe_as_of,
  universe_symbol_hash = input_universe_symbol_hash,
  market_cache_used = FALSE,
  fresh_download_attempted = TRUE
)

# =============================================================================
# Section 10: Write diagnostics before evaluating promotion quality
# =============================================================================
# Logs are archival even when the run fails its promotion checks. This preserves
# the evidence needed to diagnose a provider outage without replacing a prior
# successful latest market snapshot.

archival_log_manifest <- dplyr::bind_rows(
  write_archival_log(
    quote_log,
    logs_directory,
    "quote_log",
    file_timestamp
  ),
  write_archival_log(
    price_log,
    logs_directory,
    "price_log",
    file_timestamp
  ),
  write_archival_log(
    failed_symbols_df,
    logs_directory,
    "failed_symbols",
    file_timestamp
  )
)

quality_failures <- character()

if (
  !is.finite(market_data_success_rate) ||
    market_data_success_rate < minimum_market_data_success_rate
) {
  quality_failures <- c(
    quality_failures,
    paste0(
      "market-data success rate ",
      scales::percent(market_data_success_rate, accuracy = 0.1),
      " is below the required ",
      scales::percent(minimum_market_data_success_rate, accuracy = 0.1)
    )
  )
}

if (
  !is.finite(analysis_eligible_rate) ||
    analysis_eligible_rate < minimum_analysis_eligible_rate
) {
  quality_failures <- c(
    quality_failures,
    paste0(
      "analysis-eligible rate ",
      scales::percent(analysis_eligible_rate, accuracy = 0.1),
      " is below the required ",
      scales::percent(minimum_analysis_eligible_rate, accuracy = 0.1)
    )
  )
}

if (analysis_eligible_count == 0L) {
  quality_failures <- c(
    quality_failures,
    "no symbols satisfy the analysis-eligibility requirements"
  )
}

if (length(quality_failures) > 0L) {
  stop(
    paste0(
      "The fresh market-data run did not pass the promotion checks, so the ",
      "existing *_latest market files were not replaced. Problems: ",
      paste(unique(quality_failures), collapse = "; "),
      ". Review the archival logs in ",
      logs_directory,
      "."
    ),
    call. = FALSE
  )
}

# =============================================================================
# Section 11: Promote the successful snapshot to dated and *_latest files
# =============================================================================
# Only a validated fresh download reaches this section. The stable latest file
# names allow 03_eda.qmd to remain unchanged across repeated market-data runs.

output_manifest <- dplyr::bind_rows(
  write_dataset_versions(
    market_snapshot_df,
    market_directory,
    "market_snapshot",
    file_timestamp
  ),
  write_dataset_versions(
    price_history_df,
    market_directory,
    "price_history",
    file_timestamp
  ),
  write_dataset_versions(
    market_metadata_df,
    market_directory,
    "market_metadata",
    file_timestamp
  ),
  write_dataset_versions(
    market_quality_df,
    market_directory,
    "market_quality",
    file_timestamp
  ),
  write_dataset_versions(
    quote_log,
    logs_directory,
    "quote_log",
    file_timestamp
  ),
  write_dataset_versions(
    price_log,
    logs_directory,
    "price_log",
    file_timestamp
  ),
  write_dataset_versions(
    failed_symbols_df,
    logs_directory,
    "failed_symbols",
    file_timestamp
  )
)

# =============================================================================
# Section 12: Console summary
# =============================================================================
# The final console output confirms the dates, data coverage, universe linkage,
# and locations of the latest files that Stage 3 will read.

cat("\n")
cat("=====================================================================\n")
cat("FRESH MARKET-DATA UPDATE COMPLETE\n")
cat("=====================================================================\n")
cat("Market snapshot ID:         ", market_snapshot_id, "\n", sep = "")
cat(
  "Market-data as-of date:     ",
  as.character(market_data_as_of),
  "\n",
  sep = ""
)
cat(
  "Latest history date:        ",
  as.character(latest_history_date_overall),
  "\n",
  sep = ""
)
cat("Universe build ID:          ", input_universe_build_id, "\n", sep = "")
cat("Universe symbol hash:       ", input_universe_symbol_hash, "\n", sep = "")
cat(
  "Requested symbols:          ",
  format(requested_symbol_count, big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Valid market-data symbols:  ",
  format(valid_market_data_count, big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Analysis-eligible symbols:  ",
  format(analysis_eligible_count, big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Market-data success rate:   ",
  scales::percent(market_data_success_rate, accuracy = 0.1),
  "\n",
  sep = ""
)
cat(
  "Analysis-eligible rate:     ",
  scales::percent(analysis_eligible_rate, accuracy = 0.1),
  "\n",
  sep = ""
)
cat("Market output directory:    ", market_directory, "\n", sep = "")
cat("\nLatest files updated:\n")
print(output_manifest |> dplyr::select(dataset, latest_rds, latest_csv))
cat("\nArchival diagnostic logs:\n")
print(archival_log_manifest)
cat("\nNext step: render 03_eda.qmd. It reads the *_latest files only.\n")
