# =============================================================================
# 01_build_universe.R
# =============================================================================
# Purpose
# -------
# Build the static security universe used by the rest of the project.
#
# This script answers the sample-definition question:
#   "Which listed securities and benchmark constituents belong in the study?"
#
# It downloads the NASDAQ, NYSE, and AMEX exchange directories, obtains the
# current S&P 500 constituent table, standardizes symbols and classifications,
# and creates three primary datasets:
#
#   1. universe_df
#      All usable exchange-directory records. This broad file is used for the
#      universe-scale and security-type analysis in Module 1.
#
#   2. ticker_master_df
#      One record per Yahoo-compatible Common Equity or ADR symbol, including
#      any S&P 500 constituent not matched to the exchange directories. This is
#      the fixed ticker list consumed by 02_update_market_data.R.
#
#   3. sp500_df
#      The fixed S&P 500 constituent, sector, and weight snapshot used by the
#      benchmark analyses in 03_eda.qmd.
#
# Run frequency
# -------------
# Run this script only when you intentionally want to redefine the sample,
# typically once per semester or assignment. Every run creates time-stamped
# archival files and replaces the corresponding *_latest files.
#
# Important design choice
# -----------------------
# Exchange-directory last-sale price and market capitalization are retained
# only as build-date source snapshots. They are NOT treated as current market
# data later. Current prices, market capitalization, valuation metrics, and
# liquidity are retrieved by 02_update_market_data.R each time it is run.
#
# Outputs
# -------
# data/universe/universe_<timestamp>.{csv,rds}
# data/universe/universe_latest.{csv,rds}
# data/universe/ticker_master_<timestamp>.{csv,rds}
# data/universe/ticker_master_latest.{csv,rds}
# data/universe/sp500_constituents_<timestamp>.{csv,rds}
# data/universe/sp500_constituents_latest.{csv,rds}
# data/universe/universe_metadata_<timestamp>.{csv,rds}
# data/universe/universe_metadata_latest.{csv,rds}
# data/universe/universe_quality_<timestamp>.{csv,rds}
# data/universe/universe_quality_latest.{csv,rds}
# =============================================================================

# =============================================================================
# Section 1: Package checks and user-adjustable configuration
# =============================================================================
# This section verifies dependencies and defines settings that control where
# files are written and how remote source calls are retried.

required_packages <- c(
  "tidyverse",
  "tidyquant"
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
})

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  width = 120
)

# User-adjustable settings. Paths are defined relative to the script when the
# file is run with Rscript, or relative to the active project directory when
# sourced interactively in RStudio.
max_retry_attempts <- 3L
retry_base_delay_seconds <- 1
exchange_names <- c("NASDAQ", "NYSE", "AMEX")

# =============================================================================
# Section 2: Project paths and output directories
# =============================================================================
# This section makes the scripts portable by resolving a project root and then
# creating a predictable data directory structure.

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
logs_directory <- file.path(project_directory, "data", "logs")

dir.create(universe_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_directory, recursive = TRUE, showWarnings = FALSE)

universe_built_at <- Sys.time()
universe_as_of <- as.Date(universe_built_at)
universe_build_id <- paste0(
  "U_",
  format(universe_built_at, "%Y-%m-%d_%H%M%S")
)
file_timestamp <- format(universe_built_at, "%Y-%m-%d_%H%M%S")

# =============================================================================
# Section 3: General data-cleaning and file-output helpers
# =============================================================================
# These reusable functions standardize source fields, protect against common
# text and numeric problems, and write both archival and *_latest versions.

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

normalize_yahoo_symbol <- function(x) {
  normalized <- clean_text_na(x)
  normalized <- stringr::str_to_upper(normalized)
  normalized <- stringr::str_replace_all(normalized, "\\s+", "")

  # Convert common preferred-share conventions to Yahoo-style symbols.
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

  # Yahoo generally represents share-class separators with a dash.
  normalized <- stringr::str_replace_all(normalized, "[./]", "-")
  normalized <- stringr::str_replace_all(normalized, "\\$", "-P")
  normalized <- stringr::str_replace_all(normalized, "[^A-Z0-9=^-]", "")
  normalized[normalized == ""] <- NA_character_

  normalized
}

standardize_sector <- function(x) {
  original <- clean_text_na(x)
  lower_case <- stringr::str_to_lower(original)

  dplyr::case_when(
    is.na(lower_case) ~ NA_character_,
    stringr::str_detect(lower_case, "information technology|technology") ~
      "Information Technology",
    stringr::str_detect(lower_case, "health care|healthcare|health") ~
      "Health Care",
    stringr::str_detect(lower_case, "financial services|financials|finance") ~
      "Financials",
    stringr::str_detect(
      lower_case,
      "consumer discretionary|consumer cyclical|consumer services"
    ) ~ "Consumer Discretionary",
    stringr::str_detect(
      lower_case,
      "consumer staples|consumer defensive|consumer non-durables"
    ) ~ "Consumer Staples",
    stringr::str_detect(
      lower_case,
      "communication services|telecommunication"
    ) ~ "Communication Services",
    stringr::str_detect(lower_case, "basic materials|materials") ~
      "Materials",
    stringr::str_detect(lower_case, "industrial") ~ "Industrials",
    stringr::str_detect(lower_case, "real estate") ~ "Real Estate",
    stringr::str_detect(lower_case, "utilit") ~ "Utilities",
    stringr::str_detect(lower_case, "energy") ~ "Energy",
    TRUE ~ stringr::str_to_title(original)
  )
}

classify_security_type <- function(company, symbol) {
  company_upper <- stringr::str_to_upper(
    dplyr::coalesce(clean_text_na(company), "")
  )
  symbol_upper <- stringr::str_to_upper(
    dplyr::coalesce(clean_text_na(symbol), "")
  )

  is_warrant_or_right <-
    stringr::str_detect(company_upper, "\\bWARRANTS?\\b|\\bRIGHTS?\\b") |
    stringr::str_detect(symbol_upper, "[-.](WS|WT|RT)$")

  is_adr <-
    stringr::str_detect(
      company_upper,
      "AMERICAN DEPOSITARY|DEPOSITARY RECEIPT|\\bADR\\b|\\bADS\\b"
    ) &
    !stringr::str_detect(company_upper, "PREFERRED")

  is_preferred <-
    stringr::str_detect(
      company_upper,
      "PREFERRED|PREFERENCE SHARE|DEPOSITARY SHARES.*PREFERRED|\\bPFD\\b"
    ) |
    stringr::str_detect(symbol_upper, "\\^|[-.]P[A-Z0-9]*$")

  is_etf_or_etn <- stringr::str_detect(
    company_upper,
    paste0(
      "\\bETF\\b|\\bETN\\b|EXCHANGE[- ]TRADED|",
      "ISHARES|SPDR|PROSHARES|DIREXION|GLOBAL X|WISDOMTREE|",
      "VANGUARD.*INDEX|INVESCO.*ETF|FIRST TRUST.*ETF"
    )
  ) &
    # Fund-sponsor names such as "WisdomTree" or "Vanguard" also match on the
    # sponsor's own publicly listed operating company (e.g. "WisdomTree Inc.
    # Common Stock"), which is not itself a fund. Require the absence of a
    # plain "common stock/shares" designation to avoid that false positive.
    !stringr::str_detect(
      company_upper,
      "\\bCOMMON STOCK\\b|\\bCOMMON SHARES\\b"
    )

  is_unit_or_spac <-
    stringr::str_detect(
      company_upper,
      "\\bUNITS?\\b|ACQUISITION CORP|BLANK CHECK|SPECIAL PURPOSE ACQUISITION|\\bSPAC\\b"
    ) |
    (stringr::str_detect(symbol_upper, "U$") &
      stringr::str_detect(company_upper, "ACQUISITION|HOLDINGS"))

  is_fund <- stringr::str_detect(
    company_upper,
    paste0(
      "CLOSED[- ]END|\\bFUND\\b|PORTFOLIO|MUTUAL FUND|",
      "INCOME TRUST|INVESTMENT TRUST|TERM TRUST"
    )
  )

  is_other_security <- stringr::str_detect(
    company_upper,
    "\\bBONDS?\\b|\\bDEBENTURES?\\b|CERTIFICATES?|STRUCTURED NOTES?"
  )

  # Any listing not identified by the exclusion rules is provisionally treated
  # as common equity. This is intentionally labeled a heuristic classification.
  dplyr::case_when(
    is_warrant_or_right ~ "Warrant or Right",
    is_adr ~ "ADR",
    is_preferred ~ "Preferred Stock",
    is_etf_or_etn ~ "ETF or ETN",
    is_unit_or_spac ~ "Unit or SPAC",
    is_fund ~ "Closed-End Fund or Other Fund",
    is_other_security ~ "Other or Unclassified",
    TRUE ~ "Common Equity"
  )
}

symbol_signature <- function(symbols) {
  symbols <- sort(unique(stats::na.omit(symbols)))
  temporary_file <- tempfile(fileext = ".txt")
  on.exit(unlink(temporary_file), add = TRUE)
  writeLines(symbols, temporary_file, useBytes = TRUE)
  substr(unname(tools::md5sum(temporary_file)), 1L, 16L)
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

# =============================================================================
# Section 4: Fault-tolerant source retrieval
# =============================================================================
# Remote data sources occasionally fail temporarily. These helpers retry the
# complete source call and preserve warning/error text for a useful failure
# message. Stage 1 does not silently fall back to an old universe because the
# purpose of running it is to create a deliberate new sample definition.

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

# =============================================================================
# Section 5: Download and standardize the three exchange directories
# =============================================================================
# Each exchange source is converted to the same schema before the records are
# combined. Time-sensitive fields are explicitly named as build-date snapshots
# so they cannot be mistaken for current market data in later scripts.

standardize_exchange_download <- function(data, exchange_name) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop(
      paste0("No records were returned for ", exchange_name, "."),
      call. = FALSE
    )
  }

  names(data) <- snake_names(names(data))
  data <- ensure_columns(
    data,
    c(
      "symbol",
      "company",
      "last_sale_price",
      "market_cap",
      "country",
      "ipo_year",
      "sector",
      "industry"
    )
  )

  data |>
    dplyr::transmute(
      symbol_original = clean_text_na(symbol),
      company = clean_text_na(company),
      exchange = stringr::str_to_upper(exchange_name),
      exchange_last_sale_at_universe_build = positive_or_na(last_sale_price),
      exchange_market_cap_at_universe_build = positive_or_na(market_cap),
      country = clean_text_na(country),
      ipo_year = suppressWarnings(as.integer(ipo_year)),
      sector_raw = clean_text_na(sector),
      industry_raw = clean_text_na(industry)
    )
}

fetch_exchange <- function(exchange_name) {
  result <- retry_call(function() {
    tidyquant::tq_exchange(exchange_name)
  })

  if (!isTRUE(result$ok)) {
    stop(
      paste0(
        "Unable to retrieve the ",
        exchange_name,
        " exchange directory after ",
        result$attempts,
        " attempts. Errors: ",
        paste(result$errors, collapse = " | ")
      ),
      call. = FALSE
    )
  }

  standardized <- standardize_exchange_download(result$value, exchange_name)

  list(
    data = standardized,
    status = tibble::tibble(
      source = exchange_name,
      records = nrow(standardized),
      attempts = result$attempts,
      warning_count = length(result$warnings),
      error_count = length(result$errors)
    )
  )
}

exchange_results <- purrr::map(exchange_names, fetch_exchange)

exchange_status <- purrr::map_dfr(exchange_results, "status")
raw_universe_df <- purrr::map_dfr(exchange_results, "data")

if (nrow(raw_universe_df) == 0L) {
  stop("The combined exchange universe contains no records.", call. = FALSE)
}

# =============================================================================
# Section 6: Download and standardize the S&P 500 constituent snapshot
# =============================================================================
# The S&P 500 table supplies benchmark membership, sector, and constituent
# weights. Positive weights are normalized to sum to one so the later sector
# analysis is robust to whether the source expresses weights as decimals or
# percentages.

standardize_sp500_download <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("No S&P 500 constituent records were returned.", call. = FALSE)
  }

  names(data) <- snake_names(names(data))

  symbol_column <- first_existing_column(data, c("symbol", "ticker"))
  company_column <- first_existing_column(
    data,
    c("company", "name"),
    required = FALSE
  )
  weight_column <- first_existing_column(
    data,
    c("weight", "weight_percent", "portfolio_weight"),
    required = FALSE
  )
  sector_column <- first_existing_column(
    data,
    c("sector", "gics_sector"),
    required = FALSE
  )

  standardized <- tibble::tibble(
    sp500_symbol_original = clean_text_na(data[[symbol_column]]),
    sp500_company = if (is.null(company_column)) {
      rep(NA_character_, nrow(data))
    } else {
      clean_text_na(data[[company_column]])
    },
    sp500_weight_raw = if (is.null(weight_column)) {
      rep(NA_real_, nrow(data))
    } else {
      safe_numeric(data[[weight_column]])
    },
    sp500_sector_raw = if (is.null(sector_column)) {
      rep(NA_character_, nrow(data))
    } else {
      clean_text_na(data[[sector_column]])
    }
  ) |>
    dplyr::mutate(
      symbol_yahoo = normalize_yahoo_symbol(sp500_symbol_original),
      sp500_sector_clean = standardize_sector(sp500_sector_raw)
    ) |>
    dplyr::filter(!is.na(symbol_yahoo)) |>
    dplyr::distinct(symbol_yahoo, .keep_all = TRUE)

  positive_weight_total <- sum(
    standardized$sp500_weight_raw[
      is.finite(standardized$sp500_weight_raw) &
        standardized$sp500_weight_raw > 0
    ],
    na.rm = TRUE
  )

  standardized |>
    dplyr::mutate(
      sp500_weight = dplyr::if_else(
        is.finite(sp500_weight_raw) &
          sp500_weight_raw > 0 &
          positive_weight_total > 0,
        sp500_weight_raw / positive_weight_total,
        NA_real_
      ),
      is_sp500 = TRUE,
      universe_as_of = universe_as_of,
      universe_build_id = universe_build_id
    )
}

fetch_sp500 <- function() {
  supports_use_fallback <-
    "use_fallback" %in% names(formals(tidyquant::tq_index))

  result <- retry_call(function() {
    if (supports_use_fallback) {
      tidyquant::tq_index("SP500", use_fallback = FALSE)
    } else {
      tidyquant::tq_index("SP500")
    }
  })

  if (!isTRUE(result$ok)) {
    stop(
      paste0(
        "Unable to retrieve the current S&P 500 constituent table after ",
        result$attempts,
        " attempts. No packaged fallback is used because this script is ",
        "creating a new universe snapshot. Errors: ",
        paste(result$errors, collapse = " | ")
      ),
      call. = FALSE
    )
  }

  standardized <- standardize_sp500_download(result$value)

  list(
    data = standardized,
    status = tibble::tibble(
      source = "S&P 500",
      records = nrow(standardized),
      attempts = result$attempts,
      warning_count = length(result$warnings),
      error_count = length(result$errors)
    )
  )
}

sp500_result <- fetch_sp500()
sp500_df <- sp500_result$data
source_status <- dplyr::bind_rows(exchange_status, sp500_result$status)

# =============================================================================
# Section 7: Clean and classify the full listed-security universe
# =============================================================================
# The broad universe keeps all security types for Module 1. Exact duplicates
# are removed, and repeated records for the same exchange/symbol are resolved
# by retaining the row with the most complete metadata.

raw_universe_count <- nrow(raw_universe_df)

invalid_record_summary <- raw_universe_df |>
  dplyr::summarise(
    starting_records = dplyr::n(),
    missing_symbol = sum(is.na(symbol_original)),
    missing_company = sum(is.na(company)),
    records_removed = sum(is.na(symbol_original) | is.na(company))
  )

universe_clean_stage_1 <- raw_universe_df |>
  dplyr::filter(!is.na(symbol_original), !is.na(company)) |>
  dplyr::mutate(
    symbol_original = stringr::str_to_upper(symbol_original),
    symbol_yahoo = normalize_yahoo_symbol(symbol_original),
    company = clean_text_na(company),
    sector_raw = clean_text_na(sector_raw),
    industry_raw = clean_text_na(industry_raw),
    country = clean_text_na(country),
    exchange_market_cap_at_universe_build = positive_or_na(
      exchange_market_cap_at_universe_build
    ),
    exchange_last_sale_at_universe_build = positive_or_na(
      exchange_last_sale_at_universe_build
    )
  )

exact_duplicate_count <- nrow(universe_clean_stage_1) -
  nrow(dplyr::distinct(universe_clean_stage_1))

universe_clean_stage_2 <- universe_clean_stage_1 |>
  dplyr::distinct()

same_listing_duplicates <- universe_clean_stage_2 |>
  dplyr::count(exchange, symbol_original, name = "record_count") |>
  dplyr::filter(record_count > 1L)

same_listing_duplicate_count <- sum(
  pmax(same_listing_duplicates$record_count - 1L, 0L)
)

universe_clean_stage_3 <- universe_clean_stage_2 |>
  dplyr::mutate(
    metadata_completeness_score = as.integer(!is.na(company)) +
      as.integer(!is.na(exchange_market_cap_at_universe_build)) +
      as.integer(!is.na(exchange_last_sale_at_universe_build)) +
      as.integer(!is.na(sector_raw)) +
      as.integer(!is.na(industry_raw)) +
      as.integer(!is.na(country))
  ) |>
  dplyr::group_by(exchange, symbol_original) |>
  dplyr::arrange(
    dplyr::desc(metadata_completeness_score),
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::select(-metadata_completeness_score)

universe_df <- universe_clean_stage_3 |>
  dplyr::mutate(
    security_type = classify_security_type(company, symbol_original),
    sector_exchange_clean = standardize_sector(sector_raw),
    industry_clean = clean_text_na(industry_raw)
  ) |>
  dplyr::left_join(
    sp500_df |>
      dplyr::select(
        symbol_yahoo,
        sp500_symbol_original,
        sp500_company,
        sp500_weight,
        sp500_sector_raw,
        sp500_sector_clean,
        is_sp500
      ),
    by = "symbol_yahoo"
  ) |>
  dplyr::mutate(
    is_sp500 = tidyr::replace_na(is_sp500, FALSE),
    benchmark_group = dplyr::if_else(
      is_sp500,
      "S&P 500",
      "Broader Market"
    ),
    # Prefer the benchmark's standardized sector for benchmark members, then
    # use the exchange directory sector for all other records.
    sector_clean = dplyr::coalesce(
      sp500_sector_clean,
      sector_exchange_clean
    ),
    sector_display = dplyr::coalesce(sector_clean, "Unclassified"),
    industry_display = dplyr::coalesce(industry_clean, "Unclassified"),
    symbol_was_normalized = !is.na(symbol_yahoo) &
      symbol_original != symbol_yahoo,
    universe_as_of = universe_as_of,
    universe_build_id = universe_build_id,
    security_type_method = "Heuristic name-and-symbol rules"
  )

# =============================================================================
# Section 8: Construct the fixed ticker master used for market-data retrieval
# =============================================================================
# The ticker master is not the same as the full listed universe. It keeps one
# Yahoo-compatible record per Common Equity or ADR symbol because Modules 2-4
# compare operating-company equities rather than unlike security types.
#
# Current S&P 500 constituents not matched to an exchange-directory record are
# added as supplements. This prevents a symbol-format or source-coverage gap
# from silently removing a benchmark member from the market-data request.

exchange_equity_candidates_all <- universe_df |>
  dplyr::filter(
    security_type %in% c("Common Equity", "ADR"),
    !is.na(symbol_yahoo)
  ) |>
  dplyr::mutate(
    source_record = "Exchange directory",
    exchange_priority = dplyr::case_when(
      exchange == "NYSE" ~ 1L,
      exchange == "NASDAQ" ~ 2L,
      exchange == "AMEX" ~ 3L,
      TRUE ~ 9L
    )
  )

candidate_duplicate_summary <- exchange_equity_candidates_all |>
  dplyr::count(symbol_yahoo, name = "candidate_records") |>
  dplyr::filter(candidate_records > 1L)

exchange_equity_candidates <- exchange_equity_candidates_all |>
  dplyr::arrange(
    symbol_yahoo,
    dplyr::desc(is_sp500),
    dplyr::desc(!is.na(sector_clean)),
    dplyr::desc(!is.na(industry_clean)),
    dplyr::desc(!is.na(exchange_market_cap_at_universe_build)),
    exchange_priority
  ) |>
  dplyr::group_by(symbol_yahoo) |>
  dplyr::slice_head(n = 1L) |>
  dplyr::ungroup() |>
  dplyr::select(-exchange_priority)

sp500_candidate_supplement <- sp500_df |>
  dplyr::anti_join(
    exchange_equity_candidates |>
      dplyr::distinct(symbol_yahoo),
    by = "symbol_yahoo"
  ) |>
  dplyr::transmute(
    symbol_original = sp500_symbol_original,
    symbol_yahoo = symbol_yahoo,
    company = dplyr::coalesce(sp500_company, sp500_symbol_original),
    exchange = NA_character_,
    country = NA_character_,
    ipo_year = NA_integer_,
    sector_raw = sp500_sector_raw,
    industry_raw = NA_character_,
    sector_exchange_clean = NA_character_,
    sector_clean = sp500_sector_clean,
    sector_display = dplyr::coalesce(sp500_sector_clean, "Unclassified"),
    industry_clean = NA_character_,
    industry_display = "Unclassified",
    security_type = "Common Equity",
    security_type_method = "S&P 500 constituent supplement",
    exchange_last_sale_at_universe_build = NA_real_,
    exchange_market_cap_at_universe_build = NA_real_,
    sp500_symbol_original = sp500_symbol_original,
    sp500_company = sp500_company,
    sp500_weight = sp500_weight,
    sp500_sector_raw = sp500_sector_raw,
    sp500_sector_clean = sp500_sector_clean,
    is_sp500 = TRUE,
    benchmark_group = "S&P 500",
    symbol_was_normalized = !is.na(symbol_yahoo) &
      sp500_symbol_original != symbol_yahoo,
    universe_as_of = universe_as_of,
    universe_build_id = universe_build_id,
    source_record = "S&P 500 supplement"
  )

ticker_master_df <- dplyr::bind_rows(
  exchange_equity_candidates,
  sp500_candidate_supplement
) |>
  dplyr::arrange(symbol_yahoo, dplyr::desc(is_sp500)) |>
  dplyr::distinct(symbol_yahoo, .keep_all = TRUE)

universe_symbol_hash <- symbol_signature(ticker_master_df$symbol_yahoo)

universe_df <- universe_df |>
  dplyr::mutate(universe_symbol_hash = universe_symbol_hash)

ticker_master_df <- ticker_master_df |>
  dplyr::mutate(universe_symbol_hash = universe_symbol_hash)

sp500_df <- sp500_df |>
  dplyr::mutate(universe_symbol_hash = universe_symbol_hash)

# =============================================================================
# Section 9: Build diagnostics and metadata
# =============================================================================
# These tables make sample construction auditable. They are saved alongside
# the data and printed to the console so students can see how many records were
# removed, normalized, classified, matched, or supplemented.

sp500_unmatched_to_exchange <- sp500_df |>
  dplyr::anti_join(
    universe_df |>
      dplyr::distinct(symbol_yahoo),
    by = "symbol_yahoo"
  )

normalization_and_matching_summary <- tibble::tibble(
  metric = c(
    "Raw exchange records",
    "Clean full-universe records",
    "Exact duplicate records removed",
    "Repeated exchange/symbol records resolved",
    "Symbols changed for Yahoo compatibility",
    "Unique Yahoo symbols in full universe",
    "S&P 500 constituent symbols",
    "S&P 500 symbols matched to full exchange universe",
    "S&P 500 symbols supplemented in ticker master",
    "Unique Common Equity/ADR ticker-master symbols"
  ),
  value = c(
    raw_universe_count,
    nrow(universe_df),
    exact_duplicate_count,
    same_listing_duplicate_count,
    sum(universe_df$symbol_was_normalized, na.rm = TRUE),
    dplyr::n_distinct(universe_df$symbol_yahoo, na.rm = TRUE),
    dplyr::n_distinct(sp500_df$symbol_yahoo, na.rm = TRUE),
    dplyr::n_distinct(
      universe_df$symbol_yahoo[universe_df$is_sp500],
      na.rm = TRUE
    ),
    nrow(sp500_candidate_supplement),
    nrow(ticker_master_df)
  )
)

universe_quality_df <- dplyr::bind_rows(
  invalid_record_summary |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(category = "Invalid-record screening", .before = 1),
  normalization_and_matching_summary |>
    dplyr::mutate(category = "Normalization and matching", .before = 1),
  universe_df |>
    dplyr::summarise(
      missing_yahoo_symbol = sum(is.na(symbol_yahoo)),
      missing_sector = sum(is.na(sector_clean)),
      missing_industry = sum(is.na(industry_clean)),
      other_or_unclassified_security_type = sum(
        security_type == "Other or Unclassified",
        na.rm = TRUE
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(category = "Full-universe missingness", .before = 1)
) |>
  dplyr::mutate(
    universe_as_of = universe_as_of,
    universe_build_id = universe_build_id,
    universe_symbol_hash = universe_symbol_hash
  )

universe_metadata_df <- tibble::tibble(
  universe_build_id = universe_build_id,
  universe_as_of = universe_as_of,
  universe_built_at = universe_built_at,
  universe_symbol_hash = universe_symbol_hash,
  full_universe_records = nrow(universe_df),
  ticker_master_symbols = nrow(ticker_master_df),
  sp500_constituent_symbols = nrow(sp500_df),
  sp500_symbols_matched_to_exchange = dplyr::n_distinct(
    universe_df$symbol_yahoo[universe_df$is_sp500],
    na.rm = TRUE
  ),
  sp500_symbols_supplemented = nrow(sp500_candidate_supplement),
  heuristic_security_classification = TRUE,
  current_market_data_included = FALSE,
  notes = paste0(
    "Universe membership is static for downstream scripts. ",
    "Exchange price and market-cap fields are build-date source snapshots only."
  )
)

# =============================================================================
# Section 10: Write archival and *_latest datasets
# =============================================================================
# The dated files preserve reproducibility. The *_latest aliases allow scripts
# 02 and 03 to remain unchanged when a deliberate new universe is created.

output_manifest <- dplyr::bind_rows(
  write_dataset_versions(
    universe_df,
    universe_directory,
    "universe",
    file_timestamp
  ),
  write_dataset_versions(
    ticker_master_df,
    universe_directory,
    "ticker_master",
    file_timestamp
  ),
  write_dataset_versions(
    sp500_df,
    universe_directory,
    "sp500_constituents",
    file_timestamp
  ),
  write_dataset_versions(
    universe_metadata_df,
    universe_directory,
    "universe_metadata",
    file_timestamp
  ),
  write_dataset_versions(
    universe_quality_df,
    universe_directory,
    "universe_quality",
    file_timestamp
  )
)

# Save detailed supplemental diagnostics as CSV files. These files are useful
# when symbol matching or duplicate resolution needs manual review.
readr::write_csv(
  source_status,
  file.path(
    logs_directory,
    paste0("universe_source_status_", file_timestamp, ".csv")
  ),
  na = ""
)
readr::write_csv(
  same_listing_duplicates,
  file.path(
    logs_directory,
    paste0("universe_duplicate_listings_", file_timestamp, ".csv")
  ),
  na = ""
)
readr::write_csv(
  candidate_duplicate_summary,
  file.path(
    logs_directory,
    paste0("ticker_master_duplicate_symbols_", file_timestamp, ".csv")
  ),
  na = ""
)
readr::write_csv(
  sp500_unmatched_to_exchange,
  file.path(
    logs_directory,
    paste0("sp500_unmatched_to_exchange_", file_timestamp, ".csv")
  ),
  na = ""
)

# =============================================================================
# Section 11: Console summary
# =============================================================================
# The script ends with a compact audit trail so the user can confirm the sample
# was created successfully before moving to the market-data update stage.

cat("\n")
cat("=====================================================================\n")
cat("STATIC UNIVERSE BUILD COMPLETE\n")
cat("=====================================================================\n")
cat("Universe build ID:          ", universe_build_id, "\n", sep = "")
cat(
  "Universe as-of date:        ",
  as.character(universe_as_of),
  "\n",
  sep = ""
)
cat("Universe symbol hash:       ", universe_symbol_hash, "\n", sep = "")
cat(
  "Full listed records:        ",
  format(nrow(universe_df), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Ticker-master symbols:      ",
  format(nrow(ticker_master_df), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "S&P 500 constituent rows:   ",
  format(nrow(sp500_df), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "S&P supplements added:      ",
  format(nrow(sp500_candidate_supplement), big.mark = ","),
  "\n",
  sep = ""
)
cat("Output directory:           ", universe_directory, "\n", sep = "")
cat("\nLatest files updated:\n")
print(output_manifest |> dplyr::select(dataset, latest_rds, latest_csv))
cat("\nNext step: run 02_update_market_data.R to obtain fresh market data.\n")
