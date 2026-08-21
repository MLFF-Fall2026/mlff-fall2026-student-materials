suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyquant)
  library(quantmod)
  library(PerformanceAnalytics)
  library(yaml)
  library(lubridate)
  library(slider)
  library(zoo)
  library(xts)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

read_config <- function(path = "Supporting/LeaderBoardDemo/config/demo_config.yml") {
  cfg <- yaml::read_yaml(path)
  stopifnot(!is.null(cfg$trading), !is.null(cfg$prototype))

  cfg$trading$start_date <- as.Date(cfg$trading$start_date)
  cfg$trading$end_date <- as.Date(cfg$trading$end_date)
  cfg$prototype$start_date <- as.Date(cfg$prototype$start_date)
  cfg$prototype$end_date <- as.Date(cfg$prototype$end_date)

  if (cfg$trading$start_date > cfg$trading$end_date) {
    stop("trading start_date must be on or before end_date.")
  }
  if (cfg$prototype$start_date > cfg$prototype$end_date) {
    stop("Demo start_date must be on or before end_date.")
  }

  cfg
}

normalize_ticker <- function(x) {
  x |> trimws() |> toupper()
}

compound_return <- function(x) {
  if (length(x) == 0L || anyNA(x)) {
    return(NA_real_)
  }
  prod(1 + x) - 1
}

safe_sd <- function(x) {
  if (length(x) < 2L || anyNA(x)) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) {
    return(NA_real_)
  }
  s
}

competition_rank_desc <- function(x) {
  dplyr::min_rank(dplyr::desc(x))
}

next_trading_day <- function(date, trading_days) {
  candidates <- trading_days[trading_days > as.Date(date)]
  if (length(candidates) == 0L) as.Date(NA_character_) else min(candidates)
}

previous_trading_day <- function(date, trading_days) {
  candidates <- trading_days[trading_days < as.Date(date)]
  if (length(candidates) == 0L) as.Date(NA_character_) else max(candidates)
}

submission_deadline_et <- function(matrix_date, deadline, tz) {
  lubridate::ymd_hms(
    paste(as.character(matrix_date), deadline),
    tz = tz,
    quiet = TRUE
  )
}

week_start_monday <- function(date) {
  as.Date(lubridate::floor_date(as.Date(date), unit = "week", week_start = 1))
}
