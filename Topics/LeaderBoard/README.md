# Student Hedge Fund Leaderboard — Prototype

This repository is the **pre-GitHub-integration prototype** for the Econ490/690 student hedge fund leaderboard. It uses:

- five fictional funds;
- deterministic Position Matrix submissions and injected edge cases;
- real Yahoo Finance daily prices for a historical proxy period;
- the IVV trading calendar and IVV benchmark;
- the same return, turnover, risk, ranking, exposure, and dashboard logic intended for production.

## Prototype period and production period

The prototype uses **2025-09-22 through 2025-12-03** because the Fall 2026 trading period has not occurred yet. Production remains configured for **2026-09-21 through 2026-12-02** in `config/teams.yml`.

## Core rules implemented

- Position Matrix filenames: `PositionMatrix-YYYY-MM-DD-TeamName.csv`.
- Required columns: `Date`, `Ticker`, `Weight`.
- Team/date in the file must exactly match the filename and configured team.
- Tickers are trimmed and uppercased; duplicate tickers invalidate the full matrix.
- A ticker is accepted if Yahoo Finance returns usable price data; no asset-type classification is imposed.
- Zero weights are valid and are omitted from return calculations.
- GitHub commit logic is simulated with a manifest. The production version will use the GitHub **committer** timestamp.
- Deadline is 5:00 PM America/New_York, inclusive.
- The last valid version at or before the deadline wins; a later malformed or late version does not erase an earlier valid version.
- A valid matrix becomes effective for returns on the next IVV trading day.
- If no valid matrix has ever become effective, the team earns 0% daily return.
- Portfolio return is `sum(weight * asset_return)`. There is no separate cash, margin, financing, or short-sale proceeds account.
- Adjusted Yahoo prices are preferred. Close is used only when Adjusted is unavailable, with a warning.
- Official dates are IVV trading days. A bounded missing observation for another asset is carried forward as an own-market closure; an unresolved trailing missing observation is treated as market-data pending.
- Net exposure is `sum(w)` and gross exposure is `sum(abs(w))`.
- The first portfolio establishment has zero turnover.
- Subsequent turnover is computed across the union of old and new tickers on the matrix effective date.
- Weekly turnover uses Monday-Friday IVV trading weeks; weeks with no change count as zero, and the current partial week is included.
- `SR_f = SR - 0.5 * average_weekly_turnover`.
- Rank is based on `SR_f`; raw SR and average weekly turnover are displayed beside it.
- Competition ties use `1, 2, 2, 4` ranking.
- Risk statistics are `NA` until 10 valid daily observations.
- Trailing 4-week return requires 20 trading days.
- VaR 95% is the empirical 5th percentile using R quantile type 7; VaR/CVaR preserve return signs.
- IVV is displayed but never ranked.

## Injected prototype edge cases

The generated fake submission history deliberately includes:

1. a valid file followed by a malformed pre-deadline overwrite;
2. a duplicate-ticker matrix;
3. a late submission after 5:00 PM ET;
4. a weekend-dated matrix;
5. skipped weekly submissions;
6. valid intraweek reallocations.

These appear in the dashboard's **Prototype Validation Exceptions** table.

## Requirements

- R 4.3 or newer
- Quarto
- Internet access for Yahoo Finance

## First-time setup

```bash
Rscript R/bootstrap_dependencies.R
```

This creates/updates `renv.lock` and installs the project dependencies.

## Run the prototype

```bash
./run_prototype.sh
```

Or run the two stages separately:

```bash
Rscript R/run_prototype.R
quarto render index.qmd --output-dir docs
```

Open `docs/index.html` after rendering.

## Important files

```text
config/teams.yml                  Semester settings, prototype teams, and the (currently empty) production.teams roster
R/fetch_market_data.R            Yahoo ingestion and IVV-calendar alignment
R/generate_prototype_inputs.R    Fake Position Matrices and edge cases
R/process_submissions.R           Validation and last-valid-version logic
R/calculate_leaderboard.R        Returns, turnover, metrics, rankings, fund composition, return attribution
R/run_prototype.R                End-to-end fake-data runner (pre-GitHub-integration stand-in for production)
index.qmd                         Publication-ready Quarto dashboard (reads data/portfolio_history.rds)
data/portfolio_history.rds       Generated calculation cache used by index.qmd
Demo/                              Fully standalone teaching demo -- see Demo/README.md
```

## Demo vs. dashboard: fully separate, not just separate data

`Supporting/Demo/` is a **complete fork**, not a variant that reuses this
directory's code. It has its own copies of `common.R`, `fetch_market_data.R`,
`generate_prototype_inputs.R`, `process_submissions.R`, and
`calculate_leaderboard.R`, its own `config/demo_config.yml` (fake dates, fake
roster, no `production` section), and writes only to `Demo/data/`. Nothing
under `Supporting/Demo/` reads or writes anything under
`Topics/LeaderBoard/{R,config,data}/`, and vice versa. This means:

- rendering the demo can never affect `index.qmd`'s cached data, and building
  out real-portfolio ingestion here can never break the demo;
- the demo can be handed to students, copied, or archived as a single
  independent folder;
- a bug fix made in `R/calculate_leaderboard.R` will **not** automatically
  propagate to `Demo/R/calculate_leaderboard.R` -- the demo is a frozen
  teaching snapshot by design (see `Demo/README.md` for the tradeoff).

## Production integration later

The production phase will replace the prototype submission manifest with a GitHub API reader using `gh` and `TEAM_REPO_PAT`, reading the real team roster from `config/teams.yml`'s `production.teams` section instead of `prototype.teams`. The calculation functions and Quarto dashboard are intentionally independent of that ingestion layer, so `index.qmd` should not need to change. The later phase will also add the weekday GitHub Actions workflow, `setup-renv`, Quarto setup, commit-back to `main`, and GitHub Pages publishing from `/docs`. The production runner should avoid sourcing `Supporting/ProjectSetup.R` (the course-wide package loader); it pulls in ~50 packages unrelated to this pipeline and one of them (`mclust`) is already known to silently mask `dplyr::count()`. Use `R/common.R`'s lean dependency set instead, and keep any shared-verb calls in `R/calculate_leaderboard.R` explicitly namespaced (e.g. `dplyr::count()`) since that file may still be sourced into a `ProjectSetup.R` session by other course materials.
