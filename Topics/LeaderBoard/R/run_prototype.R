# This script fabricates five fictional funds' submissions and runs them
# through the same validation/calculation pipeline production will use. It is
# the pre-GitHub-integration stand-in described in README.md.
#
# Note: the standalone teaching demo (Supporting/Demo/) has its own
# copy of this script and does not use this file -- see Demo/README.md.

leaderboard_output_dir <- "Topics/LeaderBoard/data"

source("Topics/LeaderBoard/R/common.R")
source("Topics/LeaderBoard/R/fetch_market_data.R")
source("Topics/LeaderBoard/R/generate_prototype_inputs.R")
source("Topics/LeaderBoard/R/process_submissions.R")
source("Topics/LeaderBoard/R/calculate_leaderboard.R")


cfg <- read_config()
if (!isTRUE(cfg$prototype$enabled)) {
  stop("Prototype mode is disabled in config/teams.yml")
}

teams <- purrr::map_chr(cfg$prototype$teams, "team")
benchmark <- cfg$trading$benchmark
symbols <- unique(c(cfg$prototype$universe, benchmark))

fetch_from <- cfg$prototype$start_date - 14
fetch_to <- cfg$prototype$end_date

raw_prices <- fetch_market_prices(symbols, fetch_from, fetch_to)
benchmark_calendar <- raw_prices |>
  filter(symbol == normalize_ticker(benchmark)) |>
  arrange(date) |>
  pull(date)

market <- build_market_panel(
  raw_prices = raw_prices,
  benchmark = benchmark,
  official_start = cfg$prototype$start_date,
  official_end = cfg$prototype$end_date
)

manifest <- generate_prototype_submissions(
  cfg = cfg,
  trading_days = benchmark_calendar
)

submission_result <- process_prototype_submissions(
  manifest = manifest,
  cfg = cfg,
  trading_days = benchmark_calendar,
  available_symbols = market$available_symbols
)

positions <- build_daily_positions(
  accepted = submission_result$accepted,
  teams = teams,
  official_days = market$official_days
)

turnover_events <- compute_turnover_events(
  accepted = submission_result$accepted,
  teams = teams,
  official_days = market$official_days
)

weekly_turnover <- compute_weekly_turnover(
  turnover_events = turnover_events,
  teams = teams,
  official_days = market$official_days
)

returns_result <- compute_daily_returns(
  positions = positions,
  market_panel = market$panel,
  benchmark = benchmark,
  official_days = market$official_days
)

daily <- calculate_rolling_beta(
  returns_result$daily,
  min_obs = cfg$trading$min_risk_observations
)

as_of <- max(market$official_days)
summary_result <- build_summary(
  daily = daily,
  benchmark_daily = returns_result$benchmark,
  weekly_turnover = weekly_turnover,
  cfg = cfg,
  as_of = as_of
)

fund_composition <- compute_fund_composition(
  positions = positions,
  teams = teams,
  as_of = as_of
)

return_attribution <- compute_return_attribution(
  positions = positions,
  daily = daily,
  market_panel = market$panel,
  teams = teams,
  as_of = as_of
)

operational_status <- build_operational_status(
  validation_log = submission_result$validation_log,
  accepted = submission_result$accepted,
  daily = daily,
  teams = teams,
  as_of = as_of
)

close_fallback_symbols <- market$panel |>
  filter(date %in% market$official_days, price_source == "close_fallback") |>
  distinct(symbol) |>
  pull(symbol)

prototype_data <- list(
  meta = list(
    mode = "prototype",
    generated_at = Sys.time(),
    as_of = as_of,
    benchmark = benchmark,
    prototype_start = cfg$prototype$start_date,
    prototype_end = cfg$prototype$end_date,
    production_start = cfg$trading$start_date,
    production_end = cfg$trading$end_date,
    seed = cfg$prototype$seed,
    close_fallback_symbols = close_fallback_symbols
  ),
  team_summary = summary_result$team_summary,
  benchmark_summary = summary_result$benchmark_summary,
  daily_returns = daily,
  benchmark_daily = returns_result$benchmark,
  daily_positions = positions,
  fund_composition = fund_composition,
  return_attribution = return_attribution,
  turnover_events = turnover_events,
  weekly_turnover = weekly_turnover,
  accepted_submissions = submission_result$accepted,
  validation_log = submission_result$validation_log,
  operational_status = operational_status,
  market_price_sources = market$panel |>
    filter(date %in% market$official_days) |>
    # Use dplyr::count() explicitly: when this script is sourced after
    # Supporting/ProjectSetUp.R, mclust::count() (loaded later, for
    # clustering) masks dplyr::count() on the search path and has an
    # incompatible signature.
    dplyr::count(symbol, price_source, name = "observations"),
  published_snapshots = list(
    setNames(
      list(summary_result$team_summary),
      as.character(as_of)
    )
  )
)

dir.create(leaderboard_output_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(
  prototype_data,
  file.path(leaderboard_output_dir, "portfolio_history.rds")
)
readr::write_csv(
  summary_result$team_summary,
  file.path(leaderboard_output_dir, "current_team_summary.csv")
)
readr::write_csv(
  operational_status,
  file.path(leaderboard_output_dir, "current_operational_status.csv")
)
readr::write_csv(
  submission_result$validation_log |> select(-weights),
  file.path(leaderboard_output_dir, "submission_validation_log.csv")
)
readr::write_csv(
  turnover_events,
  file.path(leaderboard_output_dir, "turnover_events.csv")
)
readr::write_csv(
  weekly_turnover,
  file.path(leaderboard_output_dir, "weekly_turnover.csv")
)

message("Prototype calculations complete through ", as_of)
message("Saved data/portfolio_history.rds and inspection CSVs.")
