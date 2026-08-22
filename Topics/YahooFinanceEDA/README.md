# U.S. Equity Universe EDA Pipeline

This project separates the analysis into three stages so students can see the difference between **sample definition**, **market measurement**, and **exploratory analysis**.

```text
01_build_universe.R
        |
        | fixed exchange listings and S&P 500 membership
        v
data/universe/*_latest.rds
        |
        v
02_update_market_data.R
        |
        | fresh quotes, fundamentals, and recent price/volume history
        v
data/market/*_latest.rds
        |
        v
03_eda.qmd
        |
        v
03_eda.html
```

## Stage 1 — Define the static universe

Run `01_build_universe.R` when you intentionally want to define or replace the semester's security universe.

The script:

- downloads the NASDAQ, NYSE, and AMEX exchange directories;
- downloads the current S&P 500 constituent, sector, and weight snapshot;
- preserves original symbols and creates Yahoo-compatible symbols;
- applies a documented heuristic security-type classification;
- creates the broad listed-security universe used in Module 1;
- creates a one-row-per-symbol Common Equity/ADR ticker master used by Stage 2;
- writes dated archives and stable `*_latest` files in both CSV and RDS formats.

Exchange-directory price and market-cap fields are retained only as universe-build-date source fields. They are not treated as current measurements in Stages 2 or 3.

## Stage 2 — Retrieve fresh market data

Run `02_update_market_data.R` whenever current measurements are needed.

The script:

- reads `data/universe/ticker_master_latest.rds`;
- always attempts fresh Yahoo quote and historical-price requests;
- retrieves price, market capitalization, shares outstanding, trailing P/E, price-to-book, and provider-reported volume fields where available;
- retrieves recent daily prices and volume;
- calculates 30-trading-day average share volume and dollar volume;
- keeps failed symbols in the output with explicit status and exclusion fields;
- writes diagnostic logs;
- promotes the new run to `*_latest` only after minimum quality checks pass.

Current market capitalization uses the fresh Yahoo market-cap field when available. If that field is missing, Stage 2 uses fresh shares outstanding multiplied by the current reference price. It never substitutes the static exchange-directory market cap.

## Stage 3 — Render the EDA

Render `03_eda.qmd` after a successful Stage 2 run.

The document:

- reads only the stable `*_latest.rds` files;
- verifies that all files share the same universe build ID, symbol hash, and market snapshot ID;
- makes no internet requests;
- warns when the latest market snapshot is more than seven calendar days old;
- produces all required summaries and EDA figures;
- records the data provenance and R session information.

## Recommended run order

From the project directory:

```bash
Rscript 01_build_universe.R
Rscript 02_update_market_data.R
quarto render 03_eda.qmd
```

For routine updates after the semester universe has been defined, run only:

```bash
Rscript 02_update_market_data.R
quarto render 03_eda.qmd
```

In RStudio, the equivalent workflow is to source the first two scripts in order and then render the Quarto document.

## Output directories

The scripts create these folders automatically:

```text
data/
├── universe/
│   ├── universe_<timestamp>.csv
│   ├── universe_<timestamp>.rds
│   ├── universe_latest.csv
│   ├── universe_latest.rds
│   ├── ticker_master_<timestamp>.csv
│   ├── ticker_master_<timestamp>.rds
│   ├── ticker_master_latest.csv
│   ├── ticker_master_latest.rds
│   ├── sp500_constituents_<timestamp>.csv
│   ├── sp500_constituents_<timestamp>.rds
│   └── metadata and quality files
├── market/
│   ├── market_snapshot_<timestamp>.csv
│   ├── market_snapshot_<timestamp>.rds
│   ├── market_snapshot_latest.csv
│   ├── market_snapshot_latest.rds
│   ├── price_history_<timestamp>.csv
│   ├── price_history_<timestamp>.rds
│   ├── price_history_latest.csv
│   ├── price_history_latest.rds
│   └── metadata and quality files
└── logs/
    ├── quote logs
    ├── historical-price logs
    └── failed-symbol files
```

The dated files preserve an audit trail. The `*_latest` aliases allow the downstream code to remain unchanged after a successful update.

## Required software and packages

Required software:

- R
- Quarto

Required R packages:

```r
install.packages(c(
  "tidyverse",
  "tidyquant",
  "quantmod",
  "patchwork",
  "scales",
  "knitr"
))
```

The scripts check for missing packages but do not install them automatically.

## Important interpretation

A Stage 2 update changes the **measurements**, not the **sample definition**. Therefore, changes between two market snapshots reflect updated market data for the same fixed ticker master, subject to temporary differences in provider availability. To change exchange membership or fixed S&P 500 membership, deliberately rerun Stage 1 and then rerun Stage 2.
