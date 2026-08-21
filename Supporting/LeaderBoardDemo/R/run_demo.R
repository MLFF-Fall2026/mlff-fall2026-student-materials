# Self-contained demo runner. Fabricates five fictional funds' submissions
# and runs them through the same validation/calculation logic as the real
# leaderboard, entirely within Supporting/LeaderBoardDemo/.

source("Supporting/LeaderBoardDemo/R/common.R")
source("Supporting/LeaderBoardDemo/R/fetch_market_data.R")
source("Supporting/LeaderBoardDemo/R/generate_prototype_inputs.R")
source("Supporting/LeaderBoardDemo/R/process_submissions.R")
source("Supporting/LeaderBoardDemo/R/calculate_leaderboard.R")

demo_output_dir <- "Supporting/LeaderBoardDemo/data"

cfg <- read_config()
if (!isTRUE(cfg$prototype$enabled)) {
  stop("Demo mode is disabled in LeaderBoardDemo/config/demo_config.yml")
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

demo_data <- list(
  meta = list(
    mode = "demo",
    generated_at = Sys.time(),
    as_of = as_of,
    benchmark = benchmark,
    demo_start = cfg$prototype$start_date,
    demo_end = cfg$prototype$end_date,
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

dir.create(demo_output_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(demo_data, file.path(demo_output_dir, "portfolio_demo.rds"))
readr::write_csv(
  summary_result$team_summary,
  file.path(demo_output_dir, "current_team_summary.csv")
)
readr::write_csv(
  operational_status,
  file.path(demo_output_dir, "current_operational_status.csv")
)
readr::write_csv(
  submission_result$validation_log |> select(-weights),
  file.path(demo_output_dir, "submission_validation_log.csv")
)
readr::write_csv(
  turnover_events,
  file.path(demo_output_dir, "turnover_events.csv")
)
readr::write_csv(
  weekly_turnover,
  file.path(demo_output_dir, "weekly_turnover.csv")
)

message("Demo calculations complete through ", as_of)
message("Saved ", demo_output_dir, "/portfolio_demo.rds and inspection CSVs.")
