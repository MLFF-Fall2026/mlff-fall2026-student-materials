source("Supporting/LeaderBoardDemo/R/common.R")

validate_one_snapshot <- function(row, cfg, trading_days, available_symbols) {
  team <- row$team[[1]]
  matrix_date_manifest <- as.Date(row$matrix_date[[1]])
  path <- row$snapshot_path[[1]]
  filename <- basename(path)
  commit_time <- lubridate::ymd_hms(
    row$commit_time_et[[1]],
    tz = cfg$trading$timezone,
    quiet = TRUE
  )

  result <- list(
    is_valid = FALSE,
    status = "invalid",
    message = NA_character_,
    normalized_weights = numeric(),
    filename_date = as.Date(NA_character_),
    filename_team = NA_character_,
    commit_time = commit_time
  )

  m <- stringr::str_match(
    filename,
    "^PositionMatrix-(\\d{4}-\\d{2}-\\d{2})-(.+)\\.csv$"
  )
  if (is.na(m[1, 1])) {
    result$message <- "Filename does not match PositionMatrix-YYYY-MM-DD-TeamName.csv"
    return(result)
  }

  filename_date <- as.Date(m[1, 2])
  filename_team <- m[1, 3]
  result$filename_date <- filename_date
  result$filename_team <- filename_team

  if (!identical(filename_team, team)) {
    result$message <- "Team name in filename does not exactly match configured team name"
    return(result)
  }
  if (!identical(filename_date, matrix_date_manifest)) {
    result$message <- "Manifest date and filename date do not match"
    return(result)
  }
  if (!(filename_date %in% trading_days)) {
    result$message <- "Position Matrix date is not an IVV trading day"
    return(result)
  }

  deadline <- submission_deadline_et(
    filename_date,
    cfg$trading$submission_deadline,
    cfg$trading$timezone
  )
  if (is.na(commit_time) || commit_time > deadline) {
    result$status <- "late"
    result$message <- "Committer timestamp is after the 5:00 PM ET deadline"
    return(result)
  }

  df <- tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
  if (is.null(df)) {
    result$message <- "CSV could not be parsed"
    return(result)
  }

  required <- c("Date", "Ticker", "Weight")
  if (!identical(sort(names(df)), sort(required))) {
    result$message <- "CSV must contain exactly Date, Ticker, Weight"
    return(result)
  }
  if (nrow(df) == 0L) {
    result$message <- "Position Matrix contains no rows"
    return(result)
  }

  dates <- suppressWarnings(as.Date(df$Date))
  if (
    anyNA(dates) ||
      length(unique(dates)) != 1L ||
      unique(dates) != filename_date
  ) {
    result$message <- "All Date values must be valid and exactly match the filename date"
    return(result)
  }

  tickers <- normalize_ticker(as.character(df$Ticker))
  if (any(is.na(tickers) | tickers == "")) {
    result$message <- "Ticker contains a missing or empty value"
    return(result)
  }
  if (anyDuplicated(tickers)) {
    result$message <- "Duplicate ticker after trimming and uppercasing"
    return(result)
  }

  weights <- suppressWarnings(as.numeric(df$Weight))
  if (any(!is.finite(weights))) {
    result$message <- "Weight must be finite numeric values"
    return(result)
  }

  unavailable <- setdiff(tickers[weights != 0], available_symbols)
  if (length(unavailable) > 0L) {
    result$message <- paste0(
      "Ticker not available from Yahoo Finance in the requested data window: ",
      paste(unavailable, collapse = ", ")
    )
    return(result)
  }

  names(weights) <- tickers
  weights <- weights[weights != 0]

  result$is_valid <- TRUE
  result$status <- "valid"
  result$message <- "Valid submission"
  result$normalized_weights <- weights
  result
}

process_prototype_submissions <- function(
  manifest,
  cfg,
  trading_days,
  available_symbols
) {
  validation <- purrr::pmap_dfr(
    manifest,
    function(
      team,
      matrix_date,
      version,
      commit_time_et,
      snapshot_path,
      scenario
    ) {
      row <- tibble(
        team = team,
        matrix_date = as.Date(matrix_date),
        version = version,
        commit_time_et = commit_time_et,
        snapshot_path = snapshot_path,
        scenario = scenario
      )
      out <- validate_one_snapshot(row, cfg, trading_days, available_symbols)
      tibble(
        team = team,
        matrix_date = as.Date(matrix_date),
        version = as.integer(version),
        commit_time_et = as.character(commit_time_et),
        commit_time = out$commit_time,
        filename = basename(snapshot_path),
        snapshot_path = snapshot_path,
        scenario = scenario,
        status = out$status,
        is_valid = out$is_valid,
        message = out$message,
        weights = list(out$normalized_weights)
      )
    }
  )

  # For each matrix date, use the latest valid pre-deadline version. A later malformed
  # or late version never destroys an earlier valid version.
  accepted <- validation |>
    filter(is_valid) |>
    group_by(team, matrix_date) |>
    arrange(desc(commit_time), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    mutate(
      effective_date = purrr::map_vec(
        matrix_date,
        ~ next_trading_day(.x, trading_days),
        .ptype = as.Date(NA_character_)
      )
    ) |>
    arrange(team, effective_date, matrix_date)

  list(validation_log = validation, accepted = accepted)
}
