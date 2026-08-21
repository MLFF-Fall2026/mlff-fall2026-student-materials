source("Supporting/LeaderBoardDemo/R/common.R")

write_matrix_snapshot <- function(
  root,
  team,
  matrix_date,
  version,
  weights,
  malformed = FALSE,
  duplicate = FALSE
) {
  dir <- file.path(root, team, as.character(matrix_date), paste0("v", version))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  filename <- paste0("PositionMatrix-", matrix_date, "-", team, ".csv")
  path <- file.path(dir, filename)

  df <- tibble(
    Date = as.character(matrix_date),
    Ticker = names(weights),
    Weight = as.numeric(weights)
  )

  if (duplicate && nrow(df) > 0L) {
    df <- bind_rows(df, df[1, ])
  }
  if (malformed) {
    names(df)[names(df) == "Weight"] <- "PortfolioWeight"
  }

  readr::write_csv(df, path)
  path
}

make_strategy_weights <- function(team, week_index) {
  k <- week_index

  if (team == "NorthStarLong") {
    base <- c(
      AAPL = .18,
      MSFT = .18,
      NVDA = .14,
      JPM = .12,
      XOM = .10,
      COST = .10,
      JNJ = .08,
      XLK = .10
    )
    tilt <- 0.02 * sin(k / 2)
    base["NVDA"] <- base["NVDA"] + tilt
    base["JNJ"] <- base["JNJ"] - tilt
    return(base)
  }

  if (team == "MarketNeutralLab") {
    phase <- k %% 3
    if (phase == 0) {
      return(c(
        MSFT = .25,
        COST = .20,
        JPM = .15,
        IWM = -.20,
        XLE = -.20,
        XLU = -.20
      ))
    }
    if (phase == 1) {
      return(c(
        AAPL = .22,
        JNJ = .18,
        XLF = .20,
        QQQ = -.22,
        XOM = -.18,
        IWM = -.20
      ))
    }
    return(c(
      COST = .20,
      MSFT = .20,
      XLU = .20,
      NVDA = -.20,
      XLE = -.20,
      IWM = -.20
    ))
  }

  if (team == "LeveredMomentum") {
    if (k %% 2 == 0) {
      return(c(QQQ = .60, NVDA = .45, XLK = .30, XLU = -.15, JNJ = -.10))
    }
    return(c(
      QQQ = .55,
      AAPL = .35,
      MSFT = .35,
      IWM = .20,
      XLU = -.20,
      JNJ = -.15
    ))
  }

  if (team == "DefensiveCarry") {
    return(c(XLU = .35, JNJ = .25, COST = .20, XLE = .10, IVV = .10))
  }

  if (team == "ActiveRotation") {
    buckets <- list(
      c(XLK = .45, QQQ = .35, XLF = .20),
      c(XLE = .40, XLF = .35, IWM = .25),
      c(XLU = .40, JNJ = .30, COST = .30),
      c(NVDA = .35, AAPL = .30, MSFT = .25, IWM = .10)
    )
    return(buckets[[((k - 1L) %% length(buckets)) + 1L]])
  }

  stop("Unknown prototype team: ", team)
}

regular_submission_days <- function(trading_days, start_date, end_date) {
  all_days <- tibble(date = trading_days) |>
    mutate(
      week = week_start_monday(date),
      weekday = lubridate::wday(date, week_start = 1)
    )

  all_days |>
    group_by(week) |>
    summarise(
      friday_or_prior = max(date[weekday <= 5]),
      .groups = "drop"
    ) |>
    filter(friday_or_prior >= start_date - 7, friday_or_prior <= end_date) |>
    pull(friday_or_prior)
}

generate_prototype_submissions <- function(
  cfg,
  trading_days,
  root = "Supporting/LeaderBoardDemo/data/submissions"
) {
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  set.seed(cfg$prototype$seed)

  teams <- purrr::map_chr(cfg$prototype$teams, "team")
  due_days <- regular_submission_days(
    trading_days,
    cfg$prototype$start_date,
    cfg$prototype$end_date
  )

  manifest <- list()
  add_manifest <- function(
    team,
    matrix_date,
    version,
    commit_time,
    path,
    scenario = "normal"
  ) {
    manifest[[length(manifest) + 1L]] <<- tibble(
      team = team,
      matrix_date = as.Date(matrix_date),
      version = as.integer(version),
      commit_time_et = as.character(commit_time),
      snapshot_path = path,
      scenario = scenario
    )
  }

  for (team in teams) {
    for (j in seq_along(due_days)) {
      d <- due_days[[j]]

      # DefensiveCarry deliberately skips several weeks: no submission means no portfolio change.
      if (team == "DefensiveCarry" && j %in% c(2L, 3L, 6L, 8L)) {
        next
      }

      weights <- make_strategy_weights(team, j)
      if (team != "DefensiveCarry") {
        weights <- weights *
          (1 + stats::runif(length(weights), min = -0.015, max = 0.015))
        weights <- round(weights, 6)
      }
      commit_time <- paste0(d, " 15:30:00")

      # Edge case 1: NorthStar has a valid version, then a malformed pre-deadline overwrite.
      if (team == "NorthStarLong" && j == 4L) {
        p1 <- write_matrix_snapshot(root, team, d, 1L, weights)
        add_manifest(
          team,
          d,
          1L,
          paste0(d, " 15:00:00"),
          p1,
          "valid_before_bad_overwrite"
        )
        p2 <- write_matrix_snapshot(
          root,
          team,
          d,
          2L,
          weights,
          malformed = TRUE
        )
        add_manifest(
          team,
          d,
          2L,
          paste0(d, " 16:30:00"),
          p2,
          "malformed_overwrite"
        )
        next
      }

      # Edge case 2: MarketNeutral submits a duplicate-ticker matrix; prior valid portfolio persists.
      if (team == "MarketNeutralLab" && j == 5L) {
        p <- write_matrix_snapshot(root, team, d, 1L, weights, duplicate = TRUE)
        add_manifest(team, d, 1L, paste0(d, " 14:45:00"), p, "duplicate_ticker")
        next
      }

      # Edge case 3: LeveredMomentum submits after 5 PM; prior valid portfolio persists.
      if (team == "LeveredMomentum" && j == 6L) {
        p <- write_matrix_snapshot(root, team, d, 1L, weights)
        add_manifest(team, d, 1L, paste0(d, " 17:30:00"), p, "late_submission")
        next
      }

      p <- write_matrix_snapshot(root, team, d, 1L, weights)
      add_manifest(team, d, 1L, commit_time, p)
    }
  }

  # Edge case 4: ActiveRotation makes valid intraweek changes that become effective next trading day.
  active_days <- trading_days[
    trading_days >= cfg$prototype$start_date &
      trading_days <= cfg$prototype$end_date
  ]
  intraweek <- active_days[lubridate::wday(active_days, week_start = 1) == 2][c(
    3,
    6,
    9
  )]
  for (j in seq_along(intraweek)) {
    d <- intraweek[[j]]
    w <- c(XLK = .25, XLF = .25, XLE = .25, XLU = .25)
    if (j == 2L) {
      w <- c(QQQ = .35, IWM = .25, XLE = .20, XLF = .20)
    }
    if (j == 3L) {
      w <- c(COST = .30, JNJ = .25, XLU = .25, IVV = .20)
    }
    p <- write_matrix_snapshot(root, "ActiveRotation", d, 1L, w)
    add_manifest(
      "ActiveRotation",
      d,
      1L,
      paste0(d, " 16:10:00"),
      p,
      "valid_intraweek_rebalance"
    )
  }

  # Edge case 5: a weekend-dated submission is invalid by rule.
  weekend_date <- cfg$prototype$start_date + 13
  p <- write_matrix_snapshot(
    root,
    "ActiveRotation",
    weekend_date,
    1L,
    c(IVV = 1)
  )
  add_manifest(
    "ActiveRotation",
    weekend_date,
    1L,
    paste0(weekend_date, " 12:00:00"),
    p,
    "non_trading_day"
  )

  manifest_df <- bind_rows(manifest) |>
    arrange(team, matrix_date, version)

  readr::write_csv(manifest_df, "Supporting/LeaderBoardDemo/data/submission_manifest.csv")
  manifest_df
}
