# Purpose: Load packages and instantiates other global functions as needed.

# Structure: 1) checks if installed. 2) if not, then installs, then 3) calls library.
#           Can also add helper functions as needed.

# Usage:
## Add to the pkgs list as needed
## source("ProjectSetup.R") within other files as needed.

# Author
## Mike Aguilar (mike.aguilar@duke.edu)

# Define the packages you need
# Packages used in this course
# Packages used in this course
pkgs <- c(
  # -----------------------------
  # Data manipulation & graphics
  # -----------------------------
  "tidyverse", # Core data science packages
  "data.table", # Fast data manipulation
  "lubridate", # Date and time handling
  "dplyr", # Data wrangling
  "tidyr", # Data reshaping
  "ggplot2", # Data visualization
  "scales", # Axis and color formatting
  "gridExtra", # Arrange multiple plots
  "patchwork", # Combine ggplot figures
  "reshape2", # Reshape data (legacy)
  "readxl", # Read Excel files
  "readr", # Read delimited files
  "plotly", # Interactive plots

  # -----------------------------
  # Time series
  # -----------------------------
  "xts", # Extensible time series
  "zoo", # Indexed and rolling data
  "slider", # Rolling/window calculations
  "TTR", # Technical trading indicators
  "forecast", # Classical forecasting methods

  # -----------------------------
  # Finance
  # -----------------------------
  "quantmod", # Market data and charting
  "tidyquant", # Tidy finance workflows
  "PerformanceAnalytics", # Portfolio performance measures
  "jrvFinance", # Financial mathematics
  #"PortfolioAnalytics", # Portfolio optimization
  "bizdays", # Business day calendars

  # -----------------------------
  # Statistics & ML
  # -----------------------------
  #"MASS", # Statistical methods
  "GGally", # computing pairs plots
  "moments", # Skewness and kurtosis
  "rugarch", # GARCH volatility models
  "caret", # Machine learning framework
  "tidymodels", # Modern ML framework
  "glmnet", # LASSO & elastic net
  "randomForest", # Random forests
  "xgboost", # Gradient boosting
  "pROC", # ROC analysis
  "survival", # Survival analysis
  "cmprsk", # Competing risks analysis
  "mclust", # clustering
  "cluster", # clustering

  # -----------------------------
  # Correlation & visualization
  # -----------------------------
  "corrplot", # Correlation matrices
  "ggcorrplot", # ggplot correlation plots
  "patchwork", # Compose plots together

  # -----------------------------
  # Reporting & reproducibility
  # -----------------------------
  "here", # Project-relative paths
  "janitor", # Clean variable names
  "skimr", # Data summaries
  "glue", # String interpolation
  "knitr", # Dynamic reports
  "rmarkdown", # R Markdown documents
  "quarto", # Quarto publishing
  "gt", # Display tables

  # -----------------------------
  # APIs & web data
  # -----------------------------
  "httr2", # Modern HTTP client
  "jsonlite", # JSON parsing
  "rvest", # Web scraping
  "fredr", # FRED API access

  # -----------------------------
  # Deployment
  # -----------------------------
  "shiny", # Interactive web apps
  "bslib", # Shiny themes
  "DT" # Interactive data tables
)

failed_pkgs <- character()

for (p in pkgs) {
  # Install if missing
  if (!requireNamespace(p, quietly = TRUE)) {
    ok_install <- tryCatch(
      {
        install.packages(p, dependencies = TRUE)
        TRUE
      },
      warning = function(w) {
        warning(sprintf(
          "Package '%s' produced a warning during install: %s",
          p,
          w$message
        ))
        TRUE # warning != failure
      },
      error = function(e) {
        warning(sprintf("Package '%s' failed to install: %s", p, e$message))
        FALSE
      }
    )
    if (!ok_install) {
      failed_pkgs <- c(failed_pkgs, p)
      next
    }
  }

  # Load package
  ok_load <- tryCatch(
    {
      suppressPackageStartupMessages(library(p, character.only = TRUE))
      message(sprintf("Loaded package: %s", p))
      TRUE
    },
    error = function(e) {
      warning(sprintf(
        "Package '%s' is installed but failed to load: %s",
        p,
        e$message
      ))
      FALSE
    }
  )

  if (!ok_load) failed_pkgs <- c(failed_pkgs, p)
}

failed_pkgs <- unique(failed_pkgs)

if (length(failed_pkgs) > 0) {
  message(
    "The following packages failed: ",
    paste(failed_pkgs, collapse = ", ")
  )
} else {
  message("All packages loaded successfully.")
}

# MASS (loaded above) masks dplyr::select() with an unrelated S3 generic.
# Re-assert dplyr's version so bare `select()` calls behave as expected
# throughout the rest of the session (console and chunks alike).
select <- dplyr::select

# Coefficient Table after regression output
coefficient_table <- function(model) {
  estimates <- coef(model)
  standard_errors <- sqrt(diag(model$var.coef))

  tibble(
    term = names(estimates),
    estimate = as.numeric(estimates),
    standard_error = as.numeric(standard_errors),
    z_statistic = estimate / standard_error,
    p_value = 2 * pnorm(abs(z_statistic), lower.tail = FALSE)
  )
}




# Load other helper functions
source(here("Supporting","Signal_Evaluation_SingleHoldOut.R"))
#source(here("Supporting","Signal_Evaluation_MultipleHoldOut.R"))
#source(here("Supporting","ConstructPortfolio.R"))
#source(here("Supporting","DailyPerformanceStats.R"))