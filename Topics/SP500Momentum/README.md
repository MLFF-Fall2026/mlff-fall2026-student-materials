# Relative-Momentum 120/20 Strategy Exercise (Instructor)

A standalone, static classroom demonstration of how a systematic **feature** is
transformed, one layer at a time, into an implementable long/short **portfolio**:

```
feature → signal → position → raw strategy weights → scaled strategy weights → portfolio weights
```

It constructs a 12-2 momentum feature on S&P 500 constituents, converts it into a
cross-sectional **quartile signal** (long the top quartile, short the bottom quartile),
sizes positions by relative momentum strength, normalizes each sleeve, and applies a
**120/20** capital-and-leverage mandate to produce a single **Relative-Momentum 120/20**
portfolio. That portfolio is evaluated against **IVV** over a continuous June–August 2026
holdout and attributed by security and sleeve.

Signal *performance* evaluation (predictive tests, information coefficients, hit rates) is
handled elsewhere in the course and is deliberately out of scope here.

This is the **instructor version only**.

## Construction contract

For each formation date `t`, over eligible constituents with non-missing momentum:

- **Feature:** `mom = prod_{k=2}^{12}(1 + R_{t-k}) - 1` (11 monthly return factors).
- **Cutoffs / median:** `q25`, `q75` via `quantile(..., type = 7, na.rm = TRUE)`;
  `median_mom` via `median(..., na.rm = TRUE)`.
- **Signal:** `+1 if mom > q75`, `-1 if mom < q25`, else `0` (strict inequalities; ties
  neutral).
- **Position:** `position = signal` (no extra conviction/timing/regime rule here).
- **Relative strength:** `a = |mom - median_mom|`.
- **Raw weight:** `raw_weight = position * a`.
- **Scaled weight:** normalize sleeves separately so long sums to `+1`, short to `-1`.
- **Portfolio weight:** `B_L * scaled` for longs, `B_S * scaled` for shorts
  (`B_L = 1.20`, `B_S = 0.20`) ⇒ long `+1.20`, short `-0.20`, net `1.00`, gross `1.40`.

Returns use the final portfolio weights only: `C_{i,d} = portfolio_weight_i * R_{i,d}`,
`R_{p,d} = sum_i C_{i,d}`, with constant monthly target weights (frictionless daily
rebalancing).

## Project structure

```
MomentumStrategy2/
  SP500-Momentum-Strategy-Instructor.qmd   Instructional narrative + dashboard
  README.md
  styles.css                               Local leaderboard-style CSS
  R/
    portfolio_engine.R      Feature helpers, schedule, rolling driver, daily returns,
                            contributions, exposures (generic infrastructure only)
    performance_metrics.R   Performance, risk, benchmark-relative, turnover, composition
    attribution.R           Security / sleeve attribution (Carino linking)
    validation.R            Fail-fast data/timing/weight/return/attribution checks
  data-preparation/
    01-build-static-universe.R   Freeze one static S&P 500 constituent snapshot
    02-build-frozen-price-data.R Freeze daily + month-end adjusted prices
  data/
    sp500-static-universe.csv    ticker, company, snapshot_date, source
    daily-adjusted-prices.rds    date, ticker, adjusted_price, asset_role
    monthly-adjusted-prices.rds  date, ticker, adjusted_price, asset_role
```

The economically meaningful transformation is shown **inline** in the single-holdout
section and then captured in a **local function inside the QMD** for the rolling
multiple-holdout section, so the core strategy logic stays readable without opening `R/`.
The files in `R/` hold only generic infrastructure.

## Frozen-data convention

The project uses **frozen data**. Rendering the QMD never downloads anything; it only reads
the files in `data/`. The scripts in `data-preparation/` are distributed for provenance and
reproducibility and were run **once** to create the frozen files (they call Yahoo Finance
via `tidyquant::tq_get()`). Students do not need to run them.

The benchmark (IVV) defines the canonical market calendar: all prices are restricted to
IVV's trading days so every portfolio day has a benchmark return.

## Render instructions

Open `SP500-Momentum-Strategy-Instructor.qmd` in Positron/RStudio and render, or:

```
quarto render SP500-Momentum-Strategy-Instructor.qmd
```

Rendering runs the full validation contract; any essential failure stops the render and
names the failed condition. The QMD loads course-wide packages and helpers by sourcing
`../../Supporting/ProjectSetup.R`; the lesson-specific engine in `R/` and the frozen data in
`data/` remain local to this project.

## Rebuilding the frozen data (optional)

```r
source("data-preparation/01-build-static-universe.R")
source("data-preparation/02-build-frozen-price-data.R")
```

## Provenance

- Constituent universe: `tidyquant::tq_index("SP500")`, snapshot recorded in the CSV.
- Prices: Yahoo Finance adjusted closes via `tidyquant::tq_get()`.

## Declared limitations

- Static, not point-in-time, S&P 500 membership; the same universe is used for every
  formation date.
- Balanced-panel selection conditions on full-period data availability.
- Both universe choices may introduce survivorship and look-ahead bias.
- Yahoo Finance rather than an institutional data source; adjusted closes as the return
  basis.
- Frictionless constant target weights: holding constant *target* weights implies trading
  back to target daily; those maintenance trades and all transaction, financing, and
  short-borrow costs are omitted. Reported turnover is reconstitution turnover only.
- Zero risk-free rate and zero return on residual cash.
- Risk and benchmark-relative statistics over a ~3-month sample are illustrative.
- A leveraged long/short portfolio (140% gross, 100% net) is only imperfectly comparable to
  long-only IVV.
- The exercise is pedagogical; it does not establish that momentum is reliably profitable.
