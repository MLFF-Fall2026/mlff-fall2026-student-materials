# Helper functions for Generative-Return-Simulation-Instructor.qmd

sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3L) {
    return(NA_real_)
  }
  z <- (x - mean(x)) / stats::sd(x)
  n / ((n - 1) * (n - 2)) * sum(z^3)
}

sample_excess_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4L) {
    return(NA_real_)
  }
  z <- (x - mean(x)) / stats::sd(x)
  term_1 <- n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(z^4)
  term_2 <- 3 * (n - 1)^2 / ((n - 2) * (n - 3))
  term_1 - term_2
}

distribution_summary <- function(x) {
  q <- stats::quantile(
    x,
    probs = c(0.01, 0.05, 0.50, 0.95, 0.99),
    names = FALSE
  )
  data.frame(
    mean = mean(x),
    sd = stats::sd(x),
    skewness = sample_skewness(x),
    excess_kurtosis = sample_excess_kurtosis(x),
    q01 = q[1],
    q05 = q[2],
    median = q[3],
    q95 = q[4],
    q99 = q[5]
  )
}

fit_marginal_models <- function(x) {
  data.frame(
    mu = mean(x),
    sigma = stats::sd(x),
    bw_nrd0 = stats::bw.nrd0(x),
    bw_sj = tryCatch(stats::bw.SJ(x), error = function(e) stats::bw.nrd0(x))
  )
}

draw_gaussian <- function(n, mu, sigma) {
  stats::rnorm(n, mean = mu, sd = sigma)
}

# Exact smoothed-bootstrap representation of a Gaussian-kernel KDE.
draw_kde <- function(n, x, bandwidth, lower_bound = -1) {
  if (!is.finite(bandwidth) || bandwidth <= 0) {
    stop("bandwidth must be positive")
  }

  draws <- sample(x, size = n, replace = TRUE) + stats::rnorm(n, sd = bandwidth)
  invalid <- !is.finite(draws) | draws <= lower_bound
  attempts <- 0L

  while (any(invalid)) {
    attempts <- attempts + 1L
    if (attempts > 100L) {
      stop("Unable to produce KDE draws above the lower bound")
    }
    n_bad <- sum(invalid)
    draws[invalid] <- sample(x, size = n_bad, replace = TRUE) +
      stats::rnorm(n_bad, sd = bandwidth)
    invalid <- !is.finite(draws) | draws <= lower_bound
  }

  draws
}

kde_density_at <- function(points, x, bandwidth, floor = 1e-12) {
  values <- vapply(
    points,
    function(point) mean(stats::dnorm((point - x) / bandwidth)) / bandwidth,
    numeric(1)
  )
  pmax(values, floor)
}

maximum_drawdown <- function(wealth_matrix) {
  apply(wealth_matrix, 2, function(path) {
    wealth_with_origin <- c(1, path)
    drawdowns <- wealth_with_origin / cummax(wealth_with_origin) - 1
    -min(drawdowns)
  })
}

simulate_paths <- function(
  x,
  model = c("Gaussian", "KDE"),
  n_paths = 1000L,
  horizon = 252L,
  bandwidth = stats::bw.nrd0(x),
  keep_paths = FALSE
) {
  model <- match.arg(model)
  n_draws <- n_paths * horizon

  simulated_returns <- if (model == "Gaussian") {
    draw_gaussian(n_draws, mean(x), stats::sd(x))
  } else {
    draw_kde(n_draws, x, bandwidth)
  }

  return_matrix <- matrix(simulated_returns, nrow = horizon, ncol = n_paths)
  wealth_matrix <- apply(1 + return_matrix, 2, cumprod)
  if (!is.matrix(wealth_matrix)) {
    wealth_matrix <- matrix(wealth_matrix, ncol = n_paths)
  }

  terminal_wealth <- wealth_matrix[horizon, ]
  terminal_return <- terminal_wealth - 1
  lower_cutoff <- unname(stats::quantile(terminal_return, 0.05))
  drawdown <- maximum_drawdown(wealth_matrix)

  summary <- data.frame(
    model = model,
    median_terminal_wealth = stats::median(terminal_wealth),
    terminal_wealth_q05 = unname(stats::quantile(terminal_wealth, 0.05)),
    terminal_wealth_q95 = unname(stats::quantile(terminal_wealth, 0.95)),
    probability_below_one = mean(terminal_wealth < 1),
    return_q05 = lower_cutoff,
    loss_var_95 = -lower_cutoff,
    loss_es_95 = -mean(terminal_return[terminal_return <= lower_cutoff]),
    median_max_drawdown = stats::median(drawdown),
    max_drawdown_q95 = unname(stats::quantile(drawdown, 0.95))
  )

  result <- list(summary = summary, terminal_wealth = terminal_wealth)

  if (keep_paths) {
    probabilities <- c(0.025, 0.05, 0.25, 0.50, 0.75, 0.95, 0.975)
    bands <- t(apply(wealth_matrix, 1, stats::quantile, probs = probabilities))
    colnames(bands) <- c("q025", "q05", "q25", "q50", "q75", "q95", "q975")
    result$wealth_matrix <- wealth_matrix
    result$bands <- data.frame(
      day = seq_len(horizon),
      bands,
      check.names = FALSE
    )
  }

  result
}

validation_metrics <- function(
  train,
  validation,
  bandwidth = stats::bw.nrd0(train),
  kde_quantile_draws = 20000L
) {
  mu <- mean(train)
  sigma <- stats::sd(train)
  levels <- c(0.90, 0.95, 0.98)

  gaussian_log_score <- mean(stats::dnorm(validation, mu, sigma, log = TRUE))
  kde_log_score <- mean(log(kde_density_at(validation, train, bandwidth)))

  kde_reference <- draw_kde(kde_quantile_draws, train, bandwidth)

  coverage_for <- function(model, level) {
    alpha <- 1 - level
    interval <- if (model == "Gaussian") {
      stats::qnorm(c(alpha / 2, 1 - alpha / 2), mu, sigma)
    } else {
      stats::quantile(kde_reference, c(alpha / 2, 1 - alpha / 2), names = FALSE)
    }
    mean(validation >= interval[1] & validation <= interval[2])
  }


#The KDE log score is evaluated directly from the fitted KDE at each validationreturn. The small positive density floor prevents numerical underflow from
#producing `log(0)`; it is a numerical safeguard, not an additional fitted
#parameter.

  gaussian_tail_cutoff <- stats::qnorm(0.05, mu, sigma)
  kde_tail_cutoff <- unname(stats::quantile(kde_reference, 0.05))

  rbind(
    data.frame(
      model = "Gaussian",
      log_score = gaussian_log_score,
      coverage_90 = coverage_for("Gaussian", levels[1]),
      coverage_95 = coverage_for("Gaussian", levels[2]),
      coverage_98 = coverage_for("Gaussian", levels[3]),
      predicted_q05 = gaussian_tail_cutoff,
      realized_below_q05 = mean(validation < gaussian_tail_cutoff)
    ),
    data.frame(
      model = "KDE",
      log_score = kde_log_score,
      coverage_90 = coverage_for("KDE", levels[1]),
      coverage_95 = coverage_for("KDE", levels[2]),
      coverage_98 = coverage_for("KDE", levels[3]),
      predicted_q05 = kde_tail_cutoff,
      realized_below_q05 = mean(validation < kde_tail_cutoff)
    )
  )
}
