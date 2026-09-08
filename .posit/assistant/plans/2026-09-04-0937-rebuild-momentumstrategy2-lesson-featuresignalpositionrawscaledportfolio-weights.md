# Rebuild `MomentumStrategy2` — feature → signal → position → raw → scaled → portfolio weights

## Objective

Create `Topics/MomentumStrategy2/`, built from `Topics/MomentumStrategy/`, that teaches the
transformation of a **12-2 momentum feature** into an implementable **relative-momentum
120/20 long/short portfolio**, evaluated against IVV. The lesson must make the layered
architecture explicit and pedagogically transparent:

```
feature → signal → position → raw strategy weights → scaled strategy weights → portfolio weights
```

Signal *performance* evaluation is out of scope. There is exactly **one** constructed
portfolio (relative-momentum 120/20) plus IVV as benchmark. The old two-portfolio design
(Gross-Normalized Signal Portfolio + signal-vs-strategy attribution + `tau`) is removed.

## Confirmed decisions

- **File scope:** copy only the canonical working set into `MomentumStrategy2` (skip the
  `-v2/-v3/-v4` drafts and the stale `.html`; a fresh render produces the new HTML).
- **README:** rewrite to describe the new single-portfolio architecture.
- **Turnover-adjusted Sharpe:** keep it and the `lambda = 0.5` setting for the 120/20 portfolio.
- **Do not** modify `data-preparation/` scripts or regenerate frozen data; copy them verbatim.

---

## Notation / construction contract (authoritative for this lesson)

For each formation date `t`, over eligible constituents with non-missing momentum:

1. **Feature (retained):** `mom_{i,t} = prod_{k=2}^{12}(1 + R_{i,t-k}) - 1` (11 factors).
2. **Quartile cutoffs:** `q25 = quantile(mom, 0.25, type = 7, na.rm = TRUE)`,
   `q75 = quantile(mom, 0.75, type = 7, na.rm = TRUE)`. Document `type = 7`.
3. **Signal (strict inequalities; ties neutral):**
   `s = +1 if mom > q75; -1 if mom < q25; 0 otherwise`.
4. **Position:** `s_pos = s` (no extra conviction/timing/regime rule). Retain both objects.
5. **Median:** `mom_tilde = median(mom)`.
6. **Relative strength:** `a = |mom - mom_tilde|`.
7. **Raw weight:** `raw_weight = s_pos * a` (signed, pre-normalized size).
8. **Scaled weight (per-sleeve normalization to ±1):**
   `G+ = sum(raw[raw>0])`, `G- = sum(|raw[raw<0]|)`;
   `scaled = raw/G+ if raw>0; raw/G- if raw<0; 0 otherwise`.
   Long scaled sum to +1, short scaled sum to -1.
9. **Portfolio weight (120/20 mandate lives here):**
   `portfolio_weight = B_L*scaled if scaled>0; B_S*scaled if scaled<0; 0 otherwise`
   with `B_L = 1.20`, `B_S = 0.20`. Short scaled are already negative → multiply by
   positive `B_S` (no extra sign). Result: long sum +1.20, short sum -0.20, net 1.00,
   gross 1.40.

**Timing:** signal formed on the month's last trading day is effective on the first
trading day of the next month; it does not earn the formation-date return. Weights are
constant target weights within the holding month (frictionless daily rebalancing; no
costs/taxes/impact/financing/borrow). Daily contribution `C_{i,d} = portfolio_weight * R_{i,d}`;
`R_{p,d} = sum_i C_{i,d}`. Returns computed **only** from `portfolio_weight`.

**Column names (canonical):** `momentum`, `q25`, `q75`, `median_mom`, `signal`,
`position`, `relative_strength`, `raw_weight`, `scaled_weight`, `portfolio_weight`.
Remove `signal_weight` and `strategy_weight`.

---

## Step 1 — Create the folder and copy the canonical set

Copy into `Topics/MomentumStrategy2/`:
- `styles.css` (verbatim)
- `R/` → `portfolio_engine.R`, `performance_metrics.R`, `attribution.R`, `validation.R`
  (then edit as below)
- `data/` → `daily-adjusted-prices.rds`, `monthly-adjusted-prices.rds`,
  `sp500-static-universe.csv` (verbatim, frozen)
- `data-preparation/` → both scripts (verbatim)
- `README.md` (then rewrite)
- `SP500-Momentum-Strategy-Instructor.qmd` (then fully rewrite)

Do **not** copy `-v2/-v3/-v4.qmd` or the old `.html`.

---

## Step 2 — Rewrite supporting R files (only what's retained/renamed)

### `R/portfolio_engine.R`
- **Keep unchanged:** `compute_monthly_returns`, `compute_momentum`,
  `build_holdout_schedule`, `compute_asset_daily_returns`,
  `aggregate_portfolio_returns`, `benchmark_daily_returns`, `active_daily_returns`.
- **Remove:** `build_positions`, `add_weights`, `build_complete_panel` in their current
  form (they encode `tau` and the dual signal/strategy weights).
  - The **core weight-construction logic is NOT moved into this file.** Per the spec it
    lives as a local function *inside the QMD*. `portfolio_engine.R` keeps only generic
    infrastructure.
- **Add a thin generic helper** used by the QMD's local function is *not* placed here;
  instead keep a small generic **`build_complete_panel(momentum_panel, schedule, weight_fn)`**
  that maps the QMD-defined `weight_fn` over schedule rows (infrastructure only: it just
  loops formation months and binds rows; all economics live in `weight_fn`). This keeps the
  rolling loop generic while the transformation stays visible in the QMD.
- **Rewrite `exposure_summary(panel)`** to use the single `portfolio_weight` column
  (drop the `weight_col` argument): return `n_long`, `n_short`, `long_exposure`,
  `short_exposure`, `gross_exposure`, `net_exposure` per `evaluation_month`.
- **Rewrite `build_daily_contributions(panel, asset_returns)`** to read
  `portfolio_weight` (drop `weight_col`).
- **Keep `active_holdings`** but point it at `portfolio_weight`.

### `R/attribution.R`
- **Keep:** `carino_factors`, `filter_period`, `security_attribution`,
  `sleeve_attribution`, `contributor_buckets`, `attribution_reconciliation`.
- **Remove:** `signal_vs_strategy_attribution` (no second portfolio).
- `security_attribution` already consumes generic contributions/port returns — no change
  beyond it now being fed `portfolio_weight`-based contributions.

### `R/performance_metrics.R`
- **Keep:** `performance_metrics`, `monthly_and_cumulative`, `turnover_events`,
  `weekly_turnover`, `turnover_adjusted_sharpe`, `composition_metrics`, helpers.
- **Rewrite** `turnover_events(panel, schedule)` and `composition_metrics(panel)` to drop
  the `weight_col` argument and use `portfolio_weight`.
- Keep `lambda`/turnover-adjusted Sharpe (confirmed). Label turnover clearly as **monthly
  target-weight / reconstitution turnover** in the QMD narrative.

### `R/validation.R`
- **Keep:** `check`, `assert_checks`, `validate_data`, `RECON_TOL`.
- **Rewrite `validate_features`** to drop `tau` references; keep the 11-factor and
  effective-after-formation checks; add "percentile cutoffs computed separately per
  formation month" and "signal in {-1,0,1} or NA".
- **Rewrite `validate_weights(panel)`** to check, with tolerances (`RECON_TOL`):
  - `position == signal` everywhere;
  - signal values ∈ {-1,0,+1} (NA allowed for unavailable);
  - cutoff ties are neutral (mom == q25 or == q75 ⇒ signal 0);
  - neutral ⇒ `raw_weight`, `scaled_weight`, `portfolio_weight` all 0;
  - long `raw_weight` > 0, short `raw_weight` < 0;
  - scaled long sum = +1, scaled short sum = -1 (per formation month);
  - portfolio long sum = +1.20, portfolio short sum = -0.20;
  - net exposure = 1.00, gross exposure = 1.40;
  - each formation month has non-empty long and short sleeves (else **fail loudly**);
  - no division by zero / silent NA propagation (G+ and G- > 0).
- **Rewrite `validate_returns`** — keep daily-return = sum-of-contributions, equity index,
  monthly→cumulative compounding, active = port − bench; add "first eligible return occurs
  after formation date".
- **Rewrite `validate_attribution`** — keep security linked-total reconciliation to the
  portfolio's compounded return; drop the signal-vs-strategy reconciliation; add sleeve
  reconciliation.
- **New check** used from the QMD: **inline June construction == rolling-function June
  construction** (compare `portfolio_weight` per ticker within tolerance).

---

## Step 3 — Rewrite the QMD (`SP500-Momentum-Strategy-Instructor.qmd`)

Preserve the Instructor formatting conventions (Q/A, Task, callout-note "Output",
callout-tip "Question", callout-note "Answer", callout-important limitations, tabsets, `gt_compact`).

### Front matter / Background / Architecture / Notation
- Update Goal + Architecture prose: one relative-momentum 120/20 portfolio vs IVV; the six
  layers named explicitly; momentum is a *feature*, not the strategy; a weighted portfolio
  is not a "signal".
- Rewrite the **Notation** table to the pipeline
  `X_{i,t},Z_t → s → s_pos → raw_weight → scaled_weight → portfolio_weight` with the new
  definitions (quartile signal; `a = |mom - median|`; per-sleeve normalization; 120/20 in
  the portfolio layer). Remove `tau`. Add symbols `q25,q75`, `mom_tilde`, `a`, `G+,G-`,
  `B_L,B_S`, `N+/N-`.

### Housekeeping / settings
- `base <- here("Topics", "MomentumStrategy2")`.
- `settings`: remove `tau`; keep `B_L=1.20`, `B_S=0.20`, `benchmark`, `annualization`,
  `min_risk_obs`, `lambda`; `labels` reduced to `strategy` (rename to e.g.
  `"Relative-Momentum 120/20"`) and `benchmark`. Remove the `signal` label and the
  `signal`/`strategy` two-portfolio color scheme (keep two colors: strategy + benchmark).
- Update `attr_tab` helper to the single portfolio (drop signal-vs-strategy).

### Data / schedule / data validation
- Unchanged loading; keep frozen-data note. Keep holdout schedule (May→Jun, Jun→Jul,
  Jul→Aug). Update the "Key assumptions" callout: remove `tau`; state the quartile rule and
  frictionless daily-rebalancing assumption. Keep `validate_data`.

### Feature engineering
- Keep monthly returns + 12-2 momentum (unchanged math, place formula before code).
- Keep the "which returns enter May momentum" Q/A and `validate_features` (tau removed).

### EDA (concise, formation-time only)
- Momentum distribution per formation date, marking **q25, median, q75** (vertical lines /
  annotation) instead of the zero line.
- Counts of **long / neutral / short / unavailable** per formation month (quartile-based),
  replacing the positive/negative/zero table.
- Keep IVV momentum as market context.
- No signal-performance/IC/hit-rate/classification content.

### Single holdout (May 2026 → June 2026) — show once, inline
Show the full construction directly in the QMD (do **not** hide in `build_positions()` /
`add_weights()`), with each math definition immediately before its code block, computing in
sequence:
1. formation-date momentum cross-section;
2. `q25`, `q75`;
3. `median_mom`;
4. `signal`;
5. `position`;
6. `relative_strength`;
7. `raw_weight`;
8. `scaled_weight`;
9. `portfolio_weight`;
10. daily asset returns + `contribution = portfolio_weight * daily_return`;
11. daily portfolio returns.

**Instructional table** (choose representative long / neutral / short tickers): columns
`ticker, momentum, q25, median, q75, signal, position, relative_strength, raw_weight,
scaled_weight, portfolio_weight`.

Then June evaluation for the single portfolio: exposure reconciliation
(long 1.20 / short −0.20 / gross 1.40 / net 1.00), performance vs IVV, growth of $100,
security + sleeve attribution (using `portfolio_weight`, Carino-linked), and a June
reconciliation-checks table.

**Short interpretation questions** (callout Q/A) after key transformations:
- why the quartile rule is relative, not absolute (long need not have positive absolute
  momentum, short need not be negative);
- why `s_pos = s` here;
- how `raw_weight` differs from the signal (adds relative size);
- what information `scaled_weight` contains (within-sleeve allocation shares);
- why the 120/20 rule belongs in `portfolio_weight`;
- why the portfolio is 140% gross / 100% net.

### Multiple holdouts — abstract to a local function
- Immediately before the rolling section, define a **local function inside the QMD**
  (e.g. `build_portfolio_panel(momentum_df, settings)`) reproducing the inline logic
  exactly (steps 1–9). Map it over the schedule via the generic
  `build_complete_panel(momentum_panel, schedule, build_portfolio_panel)`.
- Add a validation check confirming the inline June result equals the rolling-function June
  result (portfolio weights per ticker, within tolerance).
- Continuous daily returns/contributions from `portfolio_weight`; performance summary
  (total & monthly returns, growth of $100, ann. vol, Sharpe, max DD, beta & active vs IVV,
  turnover, turnover-adjusted Sharpe), composition (long/short/gross/net exposures),
  drawdown/underwater plot, security + sleeve attribution tabset (All/Jun/Jul/Aug).
- **Remove** the entire signal-versus-strategy section, its plot/table, and the two obsolete
  COMMENT lines.

### Turnover section
- Compute **monthly target-weight (reconstitution) turnover** from changes in
  `portfolio_weight` between monthly constructions; label it carefully and note it does not
  capture intra-month daily-rebalancing trades.
- Add the **compact roll-forward example table** (addresses the existing COMMENT): for a
  handful of selected tickers, show `momentum`, `signal`, and `portfolio_weight` across the
  three formation dates so students see which names change as the formation month rolls.

### Final validation & Limitations
- One consolidated final `assert_checks` covering weights, returns, attribution, and the
  inline-vs-rolling equality.
- Limitations: retain static membership, balanced-panel selection, survivorship/look-ahead,
  short sample, and the imperfect comparability of a leveraged long/short portfolio with
  long-only IVV. Update the frictionless-trading limitation to note constant daily target
  weights imply daily rebalancing whose costs/maintenance turnover are omitted.

---

## Step 4 — README rewrite

Update `MomentumStrategy2/README.md`: project structure, the pipeline summary
(feature→signal→position→raw→scaled→portfolio, single relative-momentum 120/20 vs IVV),
frozen-data convention (unchanged), render instructions, and limitations. Remove references
to the Gross-Normalized Signal Portfolio, signal-vs-strategy attribution, and `tau`.

---

## Step 5 — Verification

1. Remove obsolete objects/dead references from the QMD; confirm all renamed columns
   propagate through retained functions.
2. Render `MomentumStrategy2/SP500-Momentum-Strategy-Instructor.qmd` to HTML via
   `quarto render` (executeCode / terminal), from the repo so `here()` and
   `Supporting/ProjectSetup.R` resolve.
3. Resolve all execution, validation, and rendering errors (iterate).
4. Inspect rendered HTML for broken tables, plots, equations, callouts, duplicated or
   contradictory explanations.
5. Confirm inline June and rolling-function June produce identical portfolio weights (the
   dedicated validation check must pass).
6. Report which files changed and what verification was completed.

Only one strategy variant in the final lesson: the relative-momentum 120/20 portfolio + IVV.

## Risks / notes
- The balanced-panel / static universe should still yield non-empty long & short quartile
  sleeves each month; if any formation cross-section fails, validation fails loudly (by
  design) rather than producing invalid weights.
- `quantile(type = 7)` with strict `<`/`>` and ties-neutral must be applied consistently in
  both the inline block and the local function to keep the equality check exact.
- Attribution reconciles geometrically (Carino linking) to the compounded portfolio return;
  do not compare a raw arithmetic contribution sum to a compounded return.
