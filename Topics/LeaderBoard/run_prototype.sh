#!/usr/bin/env bash
set -euo pipefail
Rscript R/run_prototype.R
quarto render index.qmd --output-dir docs
printf 'Rendered dashboard: docs/index.html\n'
