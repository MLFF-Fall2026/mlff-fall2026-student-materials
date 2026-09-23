# Synthetic daily asset returns for the Gaussian-versus-KDE exercise
#
# This script creates five years (1,260 trading days) of daily returns for four
# anonymous assets and writes them to data/KDE-returns.csv in long format
# (columns: date, asset_id, return).
#
# There is exactly ONE asset per data-generating family. The asset labels in the
# CSV are deliberately anonymous ("Asset 1"..."Asset 4") so that the lesson can
# ask students to diagnose each asset from its returns alone. The true family of
# each asset is recorded ONLY here, in this script, and is never written to the
# CSV or revealed in the exercise.
#
# TRUE FAMILY MAPPING (instructor reference only):
#   Asset 1 -> Negatively skewed
#   Asset 2 -> Gaussian
#   Asset 3 -> Regime mixture
#   Asset 4 -> Heavy-tailed

set.seed(20260905)

n_days <- 1260L

# Anonymous label, hidden true family, and target annual moments for each asset.
asset_spec <- data.frame(
  asset_id = c("Asset 1", "Asset 2", "Asset 3", "Asset 4"),
  true_family = c(
    "Negatively skewed",
    "Gaussian",
    "Regime mixture",
    "Heavy-tailed"
  ),
  annual_mean = c(0.10, 0.09, 0.06, 0.07),
  annual_volatility = c(0.22, 0.18, 0.28, 0.25),
  stringsAsFactors = FALSE
)

# Five consecutive blocks of 252 weekdays. The dates are labels for a synthetic
# sample; exchange holidays are intentionally outside the scope.
calendar <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = 1800L)
trading_dates <- calendar[as.POSIXlt(calendar)$wday %in% 1:5][seq_len(n_days)]

# Standardized (mean 0, variance 1) innovations for each family. Scaling to unit
# variance lets us impose each asset's target volatility separately.
standardized_innovation <- function(family, n) {
  if (family == "Gaussian") {
    return(rnorm(n))
  }

  if (family == "Heavy-tailed") {
    degrees_freedom <- 4.5
    return(
      rt(n, df = degrees_freedom) *
        sqrt((degrees_freedom - 2) / degrees_freedom)
    )
  }

  if (family == "Negatively skewed") {
    crash_probability <- 0.04
    crash <- rbinom(n, size = 1L, prob = crash_probability)
    regular_mean <- 0
    regular_sd <- 0.72
    crash_mean <- -7.0
    crash_sd <- 1.5

    raw <- ifelse(
      crash == 1L,
      rnorm(n, mean = crash_mean, sd = crash_sd),
      rnorm(n, mean = regular_mean, sd = regular_sd)
    )

    mixture_mean <- (1 - crash_probability) *
      regular_mean +
      crash_probability * crash_mean
    mixture_second_moment <-
      (1 - crash_probability) *
      (regular_sd^2 + regular_mean^2) +
      crash_probability * (crash_sd^2 + crash_mean^2)
    mixture_sd <- sqrt(mixture_second_moment - mixture_mean^2)
    return((raw - mixture_mean) / mixture_sd)
  }

  if (family == "Regime mixture") {
    stress_probability <- 0.14
    stressed <- rbinom(n, size = 1L, prob = stress_probability)
    calm_mean <- 0.12
    calm_sd <- 0.52
    stress_mean <- -0.74
    stress_sd <- 1.70

    raw <- ifelse(
      stressed == 1L,
      rnorm(n, mean = stress_mean, sd = stress_sd),
      rnorm(n, mean = calm_mean, sd = calm_sd)
    )

    mixture_mean <- (1 - stress_probability) *
      calm_mean +
      stress_probability * stress_mean
    mixture_second_moment <-
      (1 - stress_probability) *
      (calm_sd^2 + calm_mean^2) +
      stress_probability * (stress_sd^2 + stress_mean^2)
    mixture_sd <- sqrt(mixture_second_moment - mixture_mean^2)
    return((raw - mixture_mean) / mixture_sd)
  }

  stop("Unknown family: ", family)
}

return_blocks <- vector("list", nrow(asset_spec))

for (i in seq_len(nrow(asset_spec))) {
  daily_mean <- asset_spec$annual_mean[i] / 252
  daily_volatility <- asset_spec$annual_volatility[i] / sqrt(252)
  z <- standardized_innovation(asset_spec$true_family[i], n_days)
  return_blocks[[i]] <- data.frame(
    date = trading_dates,
    asset_id = asset_spec$asset_id[i],
    return = daily_mean + daily_volatility * z,
    stringsAsFactors = FALSE
  )
}

asset_returns <- do.call(rbind, return_blocks)
row.names(asset_returns) <- NULL

# Write to the project-level data directory. The CSV intentionally omits the
# true family so that the anonymized labels carry no distributional hint.
dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(asset_returns, "data/KDE-returns.csv", row.names = FALSE)

message(
  "Created ",
  format(nrow(asset_returns), big.mark = ","),
  " daily returns for ",
  nrow(asset_spec),
  " assets in data/KDE-returns.csv."
)
