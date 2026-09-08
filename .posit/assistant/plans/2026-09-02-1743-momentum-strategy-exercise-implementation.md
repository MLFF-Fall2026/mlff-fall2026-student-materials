# Momentum Strategy Exercise — Implementation Plan

## Goal

Build the instructor-only classroom exercise described in
`Momentum_Strategy_Architecture_Specification.md`: a static, reproducible,
config-driven demonstration that walks from a 12-2 momentum feature to signals,
positions, weights, two constructed portfolios (Gross-Normalized Signal
Portfolio and 120/20 Momentum Strategy), and a leaderboard-style performance /
attribution dashboard evaluated against IVV over a continuous June–August 2026
holdout.

Implementation follows the spec's §16 ordering. Only the **instructor** version
is produced in this phase.

## Confirmed decisions

- **Location:** `Topics/MomentumStrategy/` (alongside `Topics/Momentum/`,
  `Topics/LeaderBoard/`).
- **Frozen data:** built once during implementation via a live
  `tidyquant::tq_get()` download (full May–Aug 2026 window is available as of
  today, 2026-09-02). After that, rendering never downloads.
- **Column naming:** follow the spec §4.4 schema exactly —
  `date`, `ticker`, `adjusted_price`, `asset_role` (and `company` + provenance
  in the universe CSV). This overrides the older `Date` / `adj_close` /
  `OneMthSimpleRet` naming from `Topics/Momentum`.
- **Portfolio/metrics helpers written fresh:** `Supporting/ConstructPortfolio.R`
  and `Supporting/DailyPerformanceStats.R` do not exist; the new `R/` files are
  self-contained.

## Conventions inherited from the LeaderBoard (to reuse verbatim)

Sourced from `Topics/LeaderBoard/R/calculate_leaderboard.R` and
`Topics/LeaderBoard/R/common.R`:

- **Sharpe:** `mean(r)/sd(r)*sqrt(252)`, zero risk-free.
- **Realized vol:** `sd(r)*sqrt(252)`.
- **Max drawdown:** `min(equity/cummax(equity) - 1)`.
- **Tracking error:** `sd(active)`; **Information ratio:**
  `mean(active)/sd(active)*sqrt(252)`; `active = R_p - R_IVV`.
- **Beta:** full-period `cov(R_p, R_IVV)/var(R_IVV)` (a single number for this
  short sample rather than the leaderboard's 10-day rolling series).
- **Turnover:** `sum(|w_t - w_{t-1}|)`, first target = 0; weekly aggregation
  using `week_start_monday()` = `lubridate::floor_date(date, "week", week_start = 1)`
  (ISO Monday-start weeks).
- **Turnover-adjusted Sharpe:** `SR - lambda * mean(weekly_turnover)`, `lambda = 0.5`.
- **Carino linking:** `k_d = log1p(R_d)/R_d` (=1 at 0), `K = log1p(R)/R` (=1 at 0),
  `C_j = sum_d c_{j,d} * k_d/K`; near-zero tolerance `1e-10`.
- **Effective number of bets:** `1/sum(p^2)` with `p = |w|/sum(|w|)`.
- **Top-10 concentration:** `sum(top10 |w|)/sum(|w|)`.
- **Tolerances:** position-held `1e-8`; Carino/attribution `1e-10`.
- **Visual identity:** `theme_minimal(base_size = 12)`, legend bottom, benchmark
  drawn black/dashed; attribution bucket palette
  `Top Negative #a50f15`, `Other Negative #fcae91`, `Other Positive #a1d99b`,
  `Top Positive #006d2c`; `gt` tables with `sub_missing("—")`, centered columns;
  `ggplotly()`/`plot_ly()` for interactive multiple-holdout views; local
  `styles.css` adapted from the leaderboard's.

## Callout style (instructor QMD)

Match `Topics/Momentum/SP500_Mthly_Momentum_Signal-Instructor.qmd`:

- **Task:** `::: {.callout-caution collapse="false" title="Task"}`
- **Question:** `::: {.callout-tip title="Question"}`
- **Answer:** `::: {.callout-note collapse="true" title="Answer"}`
- **Output:** `::: {.callout-note collapse="true" title="Output"}` wrapping an
  `#| echo: false` chunk.

## Target file tree

```
Topics/MomentumStrategy/
  SP500-Momentum-Strategy-Instructor.qmd
  README.md
  styles.css
  R/
    portfolio_engine.R
    performance_metrics.R
    attribution.R
    validation.R
  data-preparation/
    01-build-static-universe.R
    02-build-frozen-price-data.R
  data/
    sp500-static-universe.csv
    daily-adjusted-prices.rds
    monthly-adjusted-prices.rds
```

## Work sequence (follows spec §16)

### Step 1 — Static universe snapshot (`data-preparation/01-build-static-universe.R`)
- `tidyquant::tq_index("SP500")` → freeze `ticker`, `company`, `snapshot_date`
  (+ source note) to `data/sp500-static-universe.csv`.
- No sector fields (out of scope per §4.1).
- Run once; validate non-empty, unique tickers.

### Step 2 — Frozen price files (`data-preparation/02-build-frozen-price-data.R`)
- `tq_get()` daily adjusted prices for universe tickers + IVV.
- History window: far enough back to form the **May 2026** momentum row (needs
  monthly returns May 2025–Mar 2026, so pull daily prices from ~Mar 2025) through
  end of **Aug 2026**. Resolve exact month-end trading dates from observed data,
  not the calendar.
- Derive month-end adjusted prices (last trading obs per month).
- **Balanced instructional universe:** keep only tickers with every required
  monthly obs (all three momentum formations) AND complete daily obs across
  Jun–Aug 2026; IVV must cover all required dates.
- Save `data/daily-adjusted-prices.rds` and `data/monthly-adjusted-prices.rds`
  with schema `date, ticker, adjusted_price, asset_role` (`asset_role` ∈
  {`constituent`, `benchmark`}).
- Built once with live download, then frozen.

### Step 3 — Validation contract (`R/validation.R`)
Fail-fast checks implementing spec §13 (data schema/uniqueness/positivity/
month-end correspondence/balanced panel/IVV coverage; feature & timing: 11
return factors, May-2026 labels, signals ∈ {−1,0,+1}, no look-ahead join,
effective = first trading day of next month; weight checks: sign agreement,
neutral→0, gross=1 for signal portfolio, 120/20 sleeve budgets, unused missing
sleeve, net/implied-cash reconcile; return checks: R_p = sum of contributions,
equity = compounded returns, monthly compound to cumulative, active = R_p − IVV;
attribution reconciliation within tolerance). Each essential failure stops the
render; a compact pass/fail reconciliation table is displayed.

### Step 4 — Portfolio engine (`R/portfolio_engine.R`)
Config-driven, tidy long-form. Functions to:
- build momentum → signal (`tau`) → position (= signal) → raw weight (= position);
- construct **signal weights** `s/ sum(|s|)` (gross = 1) and **strategy weights**
  (`1.20/N+` long, `−0.20/N−` short, unused sleeve stays unused);
- compute complete panel (`formation_date, effective_date, evaluation_month,
  ticker, momentum, signal, position, raw_weight, signal_weight, strategy_weight`)
  and a derived active-holdings (nonzero) view;
- apply constant monthly target weights to daily asset returns →
  `R_{p,d} = sum_i w_{i,d-1} R_{i,d}`, active returns, equity index (base 100),
  drawdowns; implied cash reported (`1 - sum w_signal`), not a holding.

### Step 5 — Performance & turnover metrics (`R/performance_metrics.R`)
Leaderboard conventions above: monthly/cumulative return, active return, annual
vol, max drawdown, gross/net exposure, implied cash, Sharpe, TE, IR, beta,
turnover (weekly, Monday-start), turnover-adjusted Sharpe, effective bets,
top-10 concentration. Risk stats over ~1 month labeled illustrative/imprecise.
No skew/kurtosis/VaR/CVaR in the main dashboard.

### Step 6 — Attribution (`R/attribution.R`)
- **Security & sleeve:** daily `c_{i,d} = w_{i,d-1} R_{i,d}`; aggregate to
  security, long sleeve, short sleeve, top-5 ±, other ±; Carino-linked to each
  portfolio's own compounded return.
- **Signal-vs-strategy:** `c_signal = R_signal`, `c_overlay = R_strategy −
  R_signal` (labeled "120/20 Strategy Overlay"); linked against the strategy's
  compounded return.
- Period selector: cumulative Jun–Aug (default), Jun, Jul, Aug.

### Step 7 — Instructor QMD: June single-holdout walkthrough
`SP500-Momentum-Strategy-Instructor.qmd` sections per spec §6: background/
objectives → setup/settings (visible settings object: `tau=0`, `B_L=1.20`,
`B_S=0.20`, benchmark IVV, annualization 252, min risk obs, `lambda=0.5`,
labels, holdout schedule) → load frozen data → validation → feature engineering
(monthly returns, 12-2 momentum via lags 2–12) → focused pre-formation EDA →
**June single-holdout** exposing every intermediate object (May momentum →
signals → positions → raw → signal weights → strategy weights → exposure
reconciliation → daily June returns → compact IVV comparison → attribution),
using static ggplot + compact `gt` tables.

### Step 8 — Generalize to continuous June–August multiple holdout
Same functions over the three-row schedule (May→Jun, Jun→Jul, Jul→Aug), one
continuous portfolio history compounding across months (no monthly reset), with
monthly returns still reported separately.

### Step 9 — Full dashboard
Leaderboard-style dashboard for the continuous analysis (spec §12.2 items 1–9):
comparative summary, monthly+cumulative returns, growth of $100 (anchored at
100), composition (long/short/gross/net/implied cash/effective bets/top-10),
turnover + SR vs SR_TO, security & sleeve attribution (period selector),
signal-vs-strategy attribution (two-bar chart + reconciliation table),
drawdown underwater plot, validation/reconciliation panel. Static ggplot for the
single-holdout teaching; interactive Plotly selectors for the multiple-holdout
views. Omit competition-specific components (§12.3).

### Step 10 — README, styles.css, render & reconcile
- `styles.css` adapted from leaderboard CSS (no external dependency).
- `README.md`: structure, frozen-data convention, render instructions,
  provenance, and the declared limitations (§15) + required universe disclosure
  text (§4.1).
- Render the QMD, inspect every displayed result, and confirm all reconciliation
  checks pass.

## Open considerations / risks

- **Live data dependency (Steps 1–2):** the one-time freeze needs network access
  to Yahoo Finance; if `tq_get()` is rate-limited or a few constituents fail, the
  balanced-panel filter will drop them — the retained universe size is
  data-dependent and will be reported, not assumed.
- **Balanced-panel size:** if the filter leaves very few shorts (or an empty
  short sleeve in some month), the 120/20 short budget stays unused per spec
  §7.6; the walkthrough should note this if it occurs.
- **`here()` / package loading:** the QMD will load its own packages and source
  its local `R/` files via `here()`; it will not depend on
  `Supporting/ProjectSetup.R`, keeping the project standalone.
- **Scope:** instructor version only; no student/complete version derived until
  this is validated and reviewed (§16).

## Verification

- Data-prep scripts run once and produce the three frozen files with correct
  schemas; validation script passes on them.
- QMD renders with no external network call and all §13 checks green.
- Attribution reconciles to compounded returns within `1e-10`.
- Dashboard reproduces leaderboard visual identity minus competition features.
