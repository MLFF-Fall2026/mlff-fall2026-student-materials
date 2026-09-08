# Purpose:
## Generate simulated data for the PCA exercise (country-equity version).

# Background:
## You are an investment analyst at a global asset-management firm.
## The firm allocates capital across country equity markets using country equity ETFs.
## You monitor many macroeconomic and equity-market indicators for each country.
## The investment team wants a small number of measures summarizing each
## country's macroeconomic and equity-market conditions.
## Can we reduce many correlated indicators into only a few informative dimensions?

# Settings:
## 100 fictional investable countries with naming convention CTRY001, CTRY002, etc.
## 11 economic / equity-market indicators driven by 3 underlying latent factors

# Architecture:
## Define Latent Factors
### For each country i define latent factors f_i = [F1i, F2i, F3i]
### Assume f_i ~ N(0, I)

## Specific factor loadings
### Define xki as indicator k for country i
### Define xi = [x1i, x2i, ..., x11i]
### Set a loading matrix Lambda such that groups of indicators load
### most heavily on one of the three underlying factors.

## Define idiosyncratic noise
### epsilon_i ~ N(0, Phi), where Phi is diagonal.

## Generate the observed indicators
### xik = lambda_k1 F_1i + lambda_k2 F_2i + lambda_k3 F_3i + epsilon_i

## Rescale the indicators directly to realistic economic / market units
## Add country codes ala CTRY001, CTRY002, CTRY003, ..., CTRY100
## Add outliers

# -----------------------------------------------------------------------
# Simulated Country Equity-Market Indicators for PCA
# Financial Data Science
# -----------------------------------------------------------------------

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}

suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(writexl))

source(here("Supporting", "ProjectSetup.R"))

set.seed(2026)


# -----------------------------------------------------------------------
# 1. Basic setup
# -----------------------------------------------------------------------

n_country <- 100

country_code <- sprintf("CTRY%03d", 1:n_country)


# -----------------------------------------------------------------------
# 2. Three latent factors
#
# F_growth : Economic growth and equity momentum
# F_value  : Equity valuation (expensive vs. inexpensive)
# F_risk   : Market and macroeconomic risk
# -----------------------------------------------------------------------

F_growth <- rnorm(n_country)
F_value <- rnorm(n_country)
F_risk <- rnorm(n_country)

# The three factors are independent in expectation, but a finite sample of
# 100 countries produces small accidental correlations between them. Those
# accidental correlations would blend two economic dimensions into the same
# principal component. We remove them transparently by residualizing each
# factor on the previously retained factors (Gram-Schmidt) and rescaling to
# unit standard deviation, so the three latent dimensions are orthogonal
# in-sample before we inject the outliers.
F_growth <- scale(F_growth)[, 1]
F_value <- scale(residuals(lm(F_value ~ F_growth)))[, 1]
F_risk <- scale(residuals(lm(F_risk ~ F_growth + F_value)))[, 1]

factors <- cbind(
  F_growth,
  F_value,
  F_risk
)

colnames(factors) <- c(
  "F_growth",
  "F_value",
  "F_risk"
)


# -----------------------------------------------------------------------
# 3. Create two unusual countries
#
# One exceptionally strong growth/momentum country (CTRY042).
# One exceptionally high-risk country (CTRY022).
#
# Each outlier is extreme on only ITS OWN latent factor so that it
# remains economically coherent; the other two factors are left at
# ordinary (randomly drawn) levels.
# -----------------------------------------------------------------------

growth_idx <- 42
risk_idx <- 22

growth_level <- 3.5
risk_level <- 3.5

factors[growth_idx, "F_growth"] <- growth_level + rnorm(1, 0, 0.25)
factors[risk_idx, "F_risk"] <- risk_level + rnorm(1, 0, 0.25)

outlier_type <- rep("normal", n_country)

outlier_type[growth_idx] <- "growth_outlier"
outlier_type[risk_idx] <- "risk_outlier"


# -----------------------------------------------------------------------
# 4. Indicator loadings
#
# Indicators are measured in very different units intentionally.
#
# This creates a realistic setting in which PCA on the covariance matrix
# behaves very differently from PCA on the correlation matrix.
#
# Columns: F_growth, F_value, F_risk
# -----------------------------------------------------------------------

loadings <- rbind(
  GDP_Growth_Rate = c(.95, .00, .00),
  Industrial_Production_Growth = c(.95, .00, .00),
  Earnings_Growth_Rate = c(.90, .05, .00),
  Equity_Momentum_12M = c(.90, .00, -.05),

  Forward_PE_Ratio = c(.00, .95, .00),
  Price_to_Book_Ratio = c(.05, .90, .00),
  Dividend_Yield = c(.00, -.90, .00),

  Equity_Volatility = c(.00, .00, .95),
  FX_Volatility = c(.00, .00, .90),
  Inflation_Rate = c(.00, .00, .88),
  Sovereign_Risk_Spread_Bps = c(.00, .00, .92)
)

colnames(loadings) <- c(
  "F_growth",
  "F_value",
  "F_risk"
)

indicator_names <- rownames(loadings)

n_indicators <- nrow(loadings)


# -----------------------------------------------------------------------
# 5. Idiosyncratic indicator noise
#
# Each observed indicator equals
#
#     latent factor contribution + idiosyncratic noise
#
# The noise is intentionally small so the latent factors remain visible.
# -----------------------------------------------------------------------

idio_sd <- setNames(
  rep(0.10, n_indicators),
  indicator_names
)

idio_noise <- sapply(
  idio_sd,
  function(s) rnorm(n_country, 0, s)
)


# -----------------------------------------------------------------------
# 6. Generate latent indicator values
#
# X = Factors x Loadings' + Noise
# -----------------------------------------------------------------------

X <- factors %*% t(loadings) + idio_noise

colnames(X) <- indicator_names


# -----------------------------------------------------------------------
# 7. Convert latent values into realistic economic / market units
#
# The latent values have arbitrary means and variances.
#
# We map each indicator onto plausible country-level magnitudes while
# preserving the latent correlation structure.
#
# Scaling is performed using ONLY the normal countries so the growth and
# risk outliers remain genuine outliers.
# -----------------------------------------------------------------------

normal_rows <- outlier_type == "normal"

x_mean <- colMeans(X[normal_rows, ])

x_sd <- apply(
  X[normal_rows, ],
  2,
  sd
)


# -----------------------------------------------------------------------
# Target means
# -----------------------------------------------------------------------

target_mean <- c(
  GDP_Growth_Rate = 2.5, # %

  Industrial_Production_Growth = 2.0, # %

  Earnings_Growth_Rate = 6.0, # %

  Equity_Momentum_12M = 5.0, # % (trailing 12-month price return)

  Forward_PE_Ratio = 15.0, # ratio

  Price_to_Book_Ratio = 2.0, # ratio

  Dividend_Yield = 2.8, # %

  Equity_Volatility = 18.0, # % (annualized)

  FX_Volatility = 9.0, # % (annualized)

  Inflation_Rate = 3.0, # %

  Sovereign_Risk_Spread_Bps = 250.0 # basis points
)


# -----------------------------------------------------------------------
# Target standard deviations
#
# Sovereign_Risk_Spread_Bps intentionally has a much larger numerical
# scale than every other indicator.
#
# This produces a dramatic difference between covariance-based PCA and
# correlation-based PCA.
# -----------------------------------------------------------------------

target_sd <- c(
  GDP_Growth_Rate = 2.5,

  Industrial_Production_Growth = 3.5,

  Earnings_Growth_Rate = 8.0,

  Equity_Momentum_12M = 15.0,

  Forward_PE_Ratio = 4.0,

  Price_to_Book_Ratio = 0.8,

  Dividend_Yield = 1.3,

  Equity_Volatility = 5.0,

  FX_Volatility = 3.0,

  Inflation_Rate = 2.0,

  Sovereign_Risk_Spread_Bps = 150.0
)


# Ensure ordering matches the loading matrix

target_mean <- target_mean[indicator_names]

target_sd <- target_sd[indicator_names]


# -----------------------------------------------------------------------
# Rescale into economic / market units
# -----------------------------------------------------------------------

indicators <- sweep(
  X,
  2,
  x_mean,
  "-"
)

indicators <- sweep(
  indicators,
  2,
  x_sd,
  "/"
)

indicators <- sweep(
  indicators,
  2,
  target_sd,
  "*"
)

indicators <- sweep(
  indicators,
  2,
  target_mean,
  "+"
)

indicators <- as.data.frame(indicators)

colnames(indicators) <- indicator_names


# -----------------------------------------------------------------------
# Constrain economically impossible values.
#
# The linear rescaling can push the low tail of some strictly positive
# indicators below zero. We apply transparent floors AFTER rescaling.
# These floors only bite on rare extreme-low draws and therefore leave
# the intended correlation structure essentially intact. Growth,
# earnings, momentum, industrial production, and inflation are left
# unconstrained because negative values (recessions, deflation) are
# economically plausible.
# -----------------------------------------------------------------------

floors <- c(
  Forward_PE_Ratio = 3.0,
  Price_to_Book_Ratio = 0.2,
  Dividend_Yield = 0.0,
  Equity_Volatility = 3.0,
  FX_Volatility = 1.0,
  Sovereign_Risk_Spread_Bps = 5.0
)

for (nm in names(floors)) {
  indicators[[nm]] <- pmax(indicators[[nm]], floors[[nm]])
}


# -----------------------------------------------------------------------
# 8. Assemble final datasets
# -----------------------------------------------------------------------

full_data <- data.frame(
  country_code = country_code,

  outlier_type = outlier_type,

  F_growth = factors[, "F_growth"],

  F_value = factors[, "F_value"],

  F_risk = factors[, "F_risk"],

  indicators,

  row.names = NULL,

  check.names = FALSE
)


# -----------------------------------------------------------------------
# Dataset distributed to students
#
# Students receive only the observed indicators.
#
# The latent factors and outlier labels are retained only for instructor
# validation and demonstration.
# -----------------------------------------------------------------------

country_data <- data.frame(
  country_code = country_code,

  indicators,

  row.names = NULL,

  check.names = FALSE
)


# -----------------------------------------------------------------------
# 9. Create student data dictionary
#
# The dictionary describes the student-facing variables.
#
# It intentionally does NOT reveal:
#
#   - latent factors
#   - factor loadings
#   - outlier labels
#
# Students must infer the underlying correlation structure using PCA.
# -----------------------------------------------------------------------

data_dictionary <- tibble::tibble(
  variable = c(
    "country_code",
    "GDP_Growth_Rate",
    "Industrial_Production_Growth",
    "Earnings_Growth_Rate",
    "Equity_Momentum_12M",
    "Forward_PE_Ratio",
    "Price_to_Book_Ratio",
    "Dividend_Yield",
    "Equity_Volatility",
    "FX_Volatility",
    "Inflation_Rate",
    "Sovereign_Risk_Spread_Bps"
  ),

  description = c(
    "Fictional country identifier",
    "Annual real GDP growth rate",
    "Annual growth in industrial production",
    "Trailing 12-month aggregate corporate earnings growth",
    "Trailing 12-month equity index price return (momentum)",
    "Forward price-to-earnings ratio of the country equity index",
    "Price-to-book ratio of the country equity index",
    "Dividend yield of the country equity index",
    "Annualized volatility of the country equity index",
    "Annualized volatility of the country's currency vs. USD",
    "Annual consumer price inflation rate",
    "Sovereign credit spread over the risk-free rate"
  ),

  units = c(
    "Identifier",
    "Percent",
    "Percent",
    "Percent",
    "Percent",
    "Ratio",
    "Ratio",
    "Percent",
    "Percent",
    "Percent",
    "Percent",
    "Basis points"
  ),

  type = c(
    "Character",
    rep("Numeric", 11)
  ),

  category = c(
    "Identifier",
    "Growth & Momentum",
    "Growth & Momentum",
    "Growth & Momentum",
    "Growth & Momentum",
    "Valuation",
    "Valuation",
    "Valuation",
    "Risk",
    "Risk",
    "Risk",
    "Risk"
  ),

  source = c(
    "Simulated for Financial Data Science PCA exercise",
    rep(
      "Simulated for Financial Data Science PCA exercise",
      11
    )
  )
)


# -----------------------------------------------------------------------
# 10. Sanity checks
# -----------------------------------------------------------------------

cat("Dimensions:\n")
print(dim(full_data))

cat("\n")


# -----------------------------------------------------------------------
# Display the two unusual countries
# -----------------------------------------------------------------------

cat("Growth and Risk Outlier Countries:\n")

print(
  full_data[
    full_data$outlier_type != "normal",
    c("country_code", "outlier_type", indicator_names)
  ]
)


# -----------------------------------------------------------------------
# Correlation matrix (normal countries only)
# -----------------------------------------------------------------------

cat("\nCorrelation Matrix (Normal Countries Only):\n")

print(
  round(
    cor(
      full_data[
        normal_rows,
        indicator_names
      ]
    ),
    2
  )
)


# -----------------------------------------------------------------------
# Variable standard deviations
#
# These demonstrate why covariance-based PCA is dominated by the
# large-scale variable (Sovereign_Risk_Spread_Bps).
# -----------------------------------------------------------------------

cat("\nIndicator Standard Deviations:\n")

print(
  sort(
    apply(
      full_data[, indicator_names],
      2,
      sd
    ),
    decreasing = TRUE
  )
)


# -----------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------

cat("\nSummary Statistics:\n")

print(
  summary(
    full_data[,
      indicator_names
    ]
  )
)


# -----------------------------------------------------------------------
# Display data dictionary
# -----------------------------------------------------------------------

cat("\nData Dictionary:\n")

print(data_dictionary)


# -----------------------------------------------------------------------
# 11. Save student dataset and data dictionary to Excel
#
# The workbook contains two sheets:
#
#   Data
#   Data_Dictionary
# -----------------------------------------------------------------------

output_file <- here(
  "data",
  "country_equity_indicators.xlsx"
)

write_xlsx(
  list(
    Data = country_data,
    Data_Dictionary = data_dictionary
  ),
  path = output_file
)

cat("\nWorkbook written to:\n")
cat(
  output_file,
  "\n"
)
