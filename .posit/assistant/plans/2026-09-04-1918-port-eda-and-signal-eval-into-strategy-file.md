# Port EDA & Signal Evaluation into the Strategy file (quartile signal)

## Objective

Bring the **EDA** and **Signal Evaluation** content from
`Topics/Momentum/SP500_Mthly_Momentum_Signal-Instructor.qmd` (the "Signal file")
into `Topics/MomentumStrategy2/SP500-Momentum-Strategy-Instructor.qmd` (the
"Strategy file"), **evaluating the Strategy's own relative quartile signal**
(long = top momentum quartile, short = bottom quartile, neutral otherwise) rather
than the Signal file's absolute-threshold signal.

The Strategy file is the destination and its data backbone stays authoritative.
The ported content is re-expressed on the Strategy's objects, not copy-pasted.

## Why this is a port, not a copy-paste

The two documents run on different data. Every ported chunk must be rewired:

| Concept | Signal file (source) | Strategy file (destination) |
|---|---|---|
| Constituent returns | `SP500_Mthly_SimpleReturns`, `Train.Returns.Constituents` | `monthly_returns` (`date`, `ticker`, `monthly_return`) |
| Momentum (formation) | `Train.Momentum.Constituents` | `momentum_panel` (`month`, `ticker`, `momentum`) |
| Momentum (full history) | `SP500_Mthly_Momentum` | `momentum_all` (`month`, `ticker`, `momentum`, ...) |
| Signal / position | `Signal_Position`, `Signal_Panel` (abs. threshold, tau=0) | `june_panel$signal`, `panel$signal` (quartile) |
| Split framing | `Train`/`Test`, `last_train_date`, `test_date` | `schedule` (formation -> evaluation) |
| Index / benchmark | `"SP500"` (`^GSPC`) | `"IVV"` via `asset_role == "benchmark"` |
| Date column | `Date` (month-end) | `date` / `month` / `evaluation_month` |

## Design decisions (defaults chosen — tell me to change any)

1. **Signal evaluated = the quartile signal** (per request). `return_threshold = 0`
   throughout; a benchmark-relative (excess-return) variant is added, mirroring the
   Signal file. Because the quartile signal is neutral in the middle two quartiles,
   **coverage will be ~50%**, so "All" vs "Acted Only" will differ meaningfully
   (unlike the Signal file, where tau=0 gave 100% coverage). **All instructor Q&A
   answers must be rewritten** to reflect the new numbers and the coverage gap.

2. **EDA framing recast to formation/holdout, not train/test.** The Signal file's
   EDA is built around a single Jan-2026 holdout ("last train date" vs "test",
   plus "full train history"). The Strategy file has no such split. Default: recast
   the same *pedagogical* comparisons onto the Strategy's framing:
   - momentum: **formation-date cross-section** (e.g. May 2026) vs **full momentum
     history** (`momentum_all`);
   - returns: **evaluation-month returns** (June 2026) vs **full monthly-return
     history**;
   - per-stock z-scores of the formation momentum / evaluation-month return against
     each stock's own history;
   - momentum -> next-month-return scatter (formation momentum vs evaluation return).

   Alternative (heavier, not recommended): introduce an explicit train/test split
   object into the Strategy file. This conflicts with the schedule-based design.

3. **Augment the existing EDA section, do not duplicate.** The Strategy file already
   has an `# EDA` section (cutoffs, momentum histograms with quartile lines, quartile
   counts, IVV momentum). New EDA goes in as **subsections under the existing header**;
   the momentum-distribution plot is *not* re-added.

4. **Placement of the evaluations:**
   - Single-holdout evaluation fills the **empty `## Signal Efficacy` placeholder**
     (`Single HoldOut` section, ~line 576), evaluating the **June holdout** (May
     formation) — consistent with the surrounding inline single-holdout narrative.
   - Multiple-holdout evaluation becomes a **new subsection under `# Multiple Hold
     outs`**, evaluating all three holdouts (Jun/Jul/Aug 2026).

5. **Statistics backend.** The evaluation functions use `e1071`. The Signal file's
   EDA helpers (`compute_summary_stats`, `summary_table`, `single_summary_table`)
   use `moments`. Default: **rewrite those helpers to use `e1071`** so the document
   has a single skew/kurtosis dependency and matches the evaluator. (Alternative:
   keep `moments` and add it to the package load — one extra dependency.)

## Prerequisites / infrastructure changes

In the Strategy file's `setup` chunk (currently sources `ProjectSetup.R` and the
four `R/` files):

1. Source the two evaluation scripts:
   ```r
   source(here("Supporting", "Signal_Evaluation_SingleHoldOut.R"))
   source(here("Supporting", "Signal_Evaluation_MultipleHoldOut.R"))
   ```
2. Confirm `e1071` (and `rlang`) are attached by `ProjectSetup.R`; if not, add
   `e1071` to the package list. `rlang` comes with tidyverse.
3. Port the EDA summary-stat helpers (`compute_summary_stats`, `metric_order`,
   `summary_table`, `single_summary_table`) into a folded helper chunk in the EDA
   section, rewritten to use `e1071::skewness` / `e1071::kurtosis`.
4. Define the shared evaluation inputs once (near the single-holdout section):
   ```r
   Meta <- list(assetname = "asset", signalname = "Relative-Momentum (quartile)")
   return_threshold <- 0
   ```
   Note: `signal_position` values are already integers in `panel$signal` /
   `june_panel$signal`, so no coercion needed.

## Data-object mapping to the evaluator contracts

**Single-holdout (June holdout = May formation, evaluated over June):**

- `signal_position` (needs `ticker`, `signal_position`):
  ```r
  june_signal_position <- june_panel |>
    filter(!is.na(signal)) |>
    transmute(ticker, signal_position = signal)
  ```
- `test_returns` (needs `ticker`, `test_returns`) = June constituent monthly returns:
  ```r
  june_test_returns <- monthly_returns |>
    filter(floor_date(date, "month") == as.Date("2026-06-01")) |>
    transmute(ticker, test_returns = monthly_return)
  ```
- Benchmark-relative variant: subtract IVV's June monthly return (from
  `monthly |> filter(asset_role == "benchmark") |> compute_monthly_returns()`) from
  each constituent's June return, then re-run with `return_threshold = 0`.

**Multiple-holdout (Jun/Jul/Aug):**

- `signal_position_ts` (needs `date`, `ticker`, `signal_position`):
  ```r
  signal_position_ts <- panel |>
    filter(!is.na(signal)) |>
    transmute(date = evaluation_month, ticker, signal_position = signal)
  ```
- `returns_ts` (needs `date`, `ticker`, `test_returns`):
  ```r
  returns_ts <- monthly_returns |>
    mutate(eval_month = floor_date(date, "month")) |>
    filter(eval_month %in% unique(panel$evaluation_month)) |>
    transmute(date = eval_month, ticker, test_returns = monthly_return)
  ```
- **Date alignment is the main correctness risk.** The evaluator joins signals to
  returns on an exact `date` match. Standardize both sides to first-of-month via
  `floor_date(..., "month")` so `evaluation_month` (already first-of-month) lines up
  with the constituent return dates. **Must verify** the actual date convention of
  `monthly_returns$date` during implementation and confirm 3 overlapping dates.
- Benchmark-relative variant: build an index monthly-return series over the three
  eval months, join, subtract, re-run.

## Section-by-section porting plan

### A. EDA augmentation (under existing `# EDA`)

Add, as folded subsections (recast per Decision 2, using the ported helpers):

1. **Helper: summary statistics** — folded chunk defining the ported helpers.
2. **Returns** — summary table contrasting evaluation-month (June) returns vs
   full monthly-return history; Welch t-test; overlapping density plot; per-stock
   z-scores of the June return vs each stock's own history.
3. **Momentum** — summary table contrasting the formation cross-section (May)
   momentum vs full momentum history; t-test; density plot; per-stock z-scores.
4. **Connecting momentum and returns** — scatter of May-formation momentum vs June
   return with an `lm` line; identify highest/lowest-momentum names; indexed-price
   plot for those names using `monthly` adjusted prices (last 12 formation months
   through the evaluation month, indexed to 100 at formation).

Each carries a short Task line and (rewritten) instructor Q&A in the Strategy file's
prevailing style. Skip anything that would duplicate the existing histogram/cutoff
content.

### B. Single-holdout signal evaluation (fills `## Signal Efficacy`)

Mirror the Signal file's single-holdout display flow, on the quartile signal:

1. Build `june_signal_position` and `june_test_returns` (mapping above).
2. `SignalEval <- signal_evaluation_singleholdout(june_test_returns, Meta, return_threshold, june_signal_position)`.
3. Display `TradeResults_Percent`, `hit_table`, `summary_ic_tbl`, `confusion_report`
   (each folded, with rewritten Q&A — emphasize that coverage is now ~50% and that
   "All" vs "Acted Only" now differ).
4. **Evaluate relative to benchmark** subsection: excess-return variant
   (`SignalEvalBenchmark`), same four displays.

### C. Multiple-holdout signal evaluation (new subsection under `# Multiple Hold outs`)

1. Build `signal_position_ts` and `returns_ts` (mapping above); show the count of
   overlapping evaluation dates (expect 3: Jun/Jul/Aug).
2. `MultiEval <- signal_evaluation_multipleholdout(signal_position_ts, returns_ts, Meta, return_threshold)`.
3. Display `by_date` and `ts_summary`; plot `ic_all` over the holdout window.
4. **Evaluate relative to benchmark**: excess-return variant (`MultiEvalBenchmark`),
   same displays.
5. Rewritten Q&A, explicitly noting the **small-sample caveat (only 3 holdout
   months)** — even weaker evidence than the Signal file's 7 months.

### D. Background / scope-statement edit

The Strategy file's Background currently states it does *not* re-establish whether
momentum predicts returns ("comprehensive signal evaluation is done elsewhere",
~line 22) and the Architecture bullet says EDA is "brief, formation-time views
only" (~line 49). Both must be reworded to reflect that signal evaluation now lives
in this document. Add a one-line pointer in the Architecture list.

## Things to verify during implementation

- `e1071` availability (add to `ProjectSetup.R` package list if missing).
- Exact date convention of `monthly_returns$date` vs `evaluation_month`; confirm the
  `floor_date` alignment yields the intended 3 overlapping dates.
- The global re-points `count <- dplyr::count`, `filter <- dplyr::filter`,
  `select <- dplyr::select` remain in effect so the evaluator's internal dplyr verbs
  resolve correctly.
- Constituent count consistency (`monthly_returns` vs `momentum_panel`) so the
  `inner_join` in the evaluator doesn't silently drop tickers.
- Whether the June evaluation should use the **quartile signal** (Decision 1) —
  confirm this is the intended object rather than `position` (identical here, since
  `position = signal`).

## Validation / done criteria

- Render the Strategy file end-to-end (`quarto render`) with no errors.
- Single-holdout tables/plots populate; coverage ≈ 50%; "All" vs "Acted Only" differ.
- Multiple-holdout `by_date` has 3 rows; `ts_summary` and the IC plot render.
- Benchmark-relative variants render for both single and multiple holdouts.
- Existing Strategy content (portfolio construction, attribution, reconciliation
  checks) still renders unchanged.

## Out of scope

- No changes to the Signal file.
- No changes to the frozen data or the `R/` engine files.
- Not re-deriving momentum or returns — reuse the Strategy's existing objects.
- Not adding an absolute-threshold signal evaluation (quartile signal only).
