# Purpose: Evaluate the efficacy of a trading signal over a single hold out period


signal_evaluation_singleholdout <- function(test_returns,
                                             Meta,
                                             return_threshold,
                                             signal_position) {
  
  # -----------------------
  # Local helper functions
  # -----------------------
  summarise_stats <- function(df, group_vars, value_col = "test_returns") {
    value_sym <- rlang::sym(value_col)
    
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        n    = sum(!is.na(!!value_sym)),
        mean = mean(!!value_sym, na.rm = TRUE),
        sd   = sd(!!value_sym, na.rm = TRUE),
        skew = ifelse(n > 2, e1071::skewness(!!value_sym, na.rm = TRUE), NA_real_),
        kurt = ifelse(n > 3, e1071::kurtosis(!!value_sym, na.rm = TRUE), NA_real_),
        min  = min(!!value_sym, na.rm = TRUE),
        q.05 = quantile(!!value_sym, 0.05, na.rm = TRUE, names = FALSE),
        q.10 = quantile(!!value_sym, 0.10, na.rm = TRUE, names = FALSE),
        q.25 = quantile(!!value_sym, 0.25, na.rm = TRUE, names = FALSE),
        q.50 = quantile(!!value_sym, 0.50, na.rm = TRUE, names = FALSE),
        q.75 = quantile(!!value_sym, 0.75, na.rm = TRUE, names = FALSE),
        q.90 = quantile(!!value_sym, 0.90, na.rm = TRUE, names = FALSE),
        q.95 = quantile(!!value_sym, 0.95, na.rm = TRUE, names = FALSE),
        max  = max(!!value_sym, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        group = if (length(group_vars) == 1) {
          as.character(!!rlang::sym(group_vars))
        } else {
          apply(select(cur_data(), all_of(group_vars)), 1, paste, collapse = " | ")
        }
      ) %>%
      select(group, everything(), -all_of(group_vars))
  }
  
  calculate_ic <- function(signal_position, ret) {
    valid <- is.finite(signal_position) & is.finite(ret)
    s <- as.numeric(signal_position[valid])
    r <- as.numeric(ret[valid])
    n <- length(s)
    
    if (n < 3) {
      return(list(ic = NA, t_stat = NA, p_value = NA, n = n,
                  mean_s = NA, mean_r = NA, cov_sr = NA, sigma_s = NA, sigma_r = NA))
    }
    
    mean_s <- mean(s); mean_r <- mean(r)
    cov_sr <- sum((s - mean_s) * (r - mean_r)) / n
    sigma_s <- sqrt(sum((s - mean_s)^2) / n)
    sigma_r <- sqrt(sum((r - mean_r)^2) / n)
    
    ic <- if (sigma_s > 0 && sigma_r > 0) cov_sr / (sigma_s * sigma_r) else NA_real_
    
    if (!is.na(ic) && abs(ic) < 1) {
      t_stat <- ic * sqrt(n - 2) / sqrt(1 - ic^2)
      p_val  <- 2 * pt(-abs(t_stat), df = n - 2)
    } else {
      t_stat <- NA_real_
      p_val  <- NA_real_
    }
    
    list(ic = ic, t_stat = t_stat, p_value = p_val, n = n,
         mean_s = mean_s, mean_r = mean_r, cov_sr = cov_sr,
         sigma_s = sigma_s, sigma_r = sigma_r)
  }
  
  calculate_ic_acted_only <- function(signal_position, ret) {
    valid <- is.finite(signal_position) & is.finite(ret)
    s <- as.numeric(signal_position[valid])
    r <- as.numeric(ret[valid])
    
    acted <- (s != 0)
    if (sum(acted) < 3) {
      return(list(ic = NA, t_stat = NA, p_value = NA,
                  n_acted = sum(acted), n_plus = 0, n_minus = 0,
                  mean_plus = NA, mean_minus = NA, sd_acted = NA))
    }
    
    s_a <- s[acted]
    r_a <- r[acted]
    
    n_plus  <- sum(s_a ==  1)
    n_minus <- sum(s_a == -1)
    n_acted <- n_plus + n_minus
    p <- n_plus / n_acted
    
    mean_plus  <- mean(r_a[s_a ==  1], na.rm = TRUE)
    mean_minus <- mean(r_a[s_a == -1], na.rm = TRUE)
    sd_acted   <- sd(r_a, na.rm = TRUE)
    
    ic <- if (sd_acted > 0 && p > 0 && p < 1) {
      (mean_plus - mean_minus) / sd_acted * sqrt(p * (1 - p))
    } else NA_real_
    
    if (!is.na(ic) && abs(ic) < 1 && n_acted > 2) {
      t_stat <- ic * sqrt(n_acted - 2) / sqrt(1 - ic^2)
      p_val  <- 2 * pt(-abs(t_stat), df = n_acted - 2)
    } else {
      t_stat <- NA_real_
      p_val  <- NA_real_
    }
    
    list(ic = ic, t_stat = t_stat, p_value = p_val,
         n_acted = n_acted, n_plus = n_plus, n_minus = n_minus,
         mean_plus = mean_plus, mean_minus = mean_minus, sd_acted = sd_acted)
  }
  
  calculate_hit_rate <- function(signal_position, ret, p0 = 0.5, one_sided = FALSE) {
    valid <- is.finite(signal_position) & is.finite(ret)
    s <- as.numeric(signal_position[valid])
    r <- as.numeric(ret[valid])
    n <- length(s)
    
    if (n == 0) {
      return(list(hit_rate_overall = NA, overall_hits = NA, overall_n = 0,
                  z_overall = NA, p_overall = NA,
                  hit_rate_acted = NA, acted_hits = NA, acted_n = 0,
                  z_acted = NA, p_acted = NA, coverage = NA))
    }
    
    r_sign <- ifelse(r > 0, 1, -1)
    hits_overall <- sum(s * r_sign > 0, na.rm = TRUE)
    p_hat <- hits_overall / n
    
    se0 <- sqrt(p0 * (1 - p0) / n)
    z  <- (p_hat - p0) / se0
    p_val <- if (one_sided) (1 - pnorm(z)) else (2 * pnorm(-abs(z)))
    
    acted <- (s != 0)
    n_acted <- sum(acted)
    if (n_acted > 0) {
      s_a <- s[acted]
      r_a <- r[acted]
      r_a_sign <- ifelse(r_a > 0, 1, -1)
      hits_acted <- sum(s_a * r_a_sign > 0, na.rm = TRUE)
      
      p_hat_a <- hits_acted / n_acted
      se0_a <- sqrt(p0 * (1 - p0) / n_acted)
      z_a <- (p_hat_a - p0) / se0_a
      p_a <- if (one_sided) (1 - pnorm(z_a)) else (2 * pnorm(-abs(z_a)))
    } else {
      hits_acted <- NA; p_hat_a <- NA; z_a <- NA; p_a <- NA
    }
    
    list(
      hit_rate_overall = p_hat, overall_hits = hits_overall, overall_n = n,
      z_overall = z, p_overall = p_val,
      hit_rate_acted = p_hat_a, acted_hits = hits_acted, acted_n = n_acted,
      z_acted = z_a, p_acted = p_a,
      coverage = n_acted / n
    )
  }
  
  
  # safer build_contingency: explicit, named, and robust to factor/numeric inputs
  build_contingency <- function(pred_pos, actual_pos, na_to_false = TRUE) {
    # Try to coerce to logical in a predictable way
    pred_pos_l <- as.logical(pred_pos)
    actual_pos_l <- as.logical(actual_pos)
    
    if (na_to_false) {
      pred_pos_l[is.na(pred_pos_l)] <- FALSE
      actual_pos_l[is.na(actual_pos_l)] <- FALSE
    }
    
    tab <- table(
      predicted = factor(pred_pos_l, levels = c("TRUE", "FALSE")),
      actual    = factor(actual_pos_l, levels = c("TRUE", "FALSE"))
    )
    
    mat <- matrix(as.integer(tab), nrow = 2, byrow = FALSE)
    rownames(mat) <- c("pred_TRUE", "pred_FALSE")
    colnames(mat) <- c("act_TRUE", "act_FALSE")
    mat
  }
  
  confusion_stats_from_table <- function(tab) {
    get_cell <- function(mat, r, c) {
      if (r %in% rownames(mat) && c %in% colnames(mat)) {
        as.numeric(mat[r, c])
      } else {
        0
      }
    }
    
    tp <- get_cell(tab, "pred_TRUE", "act_TRUE")
    fp <- get_cell(tab, "pred_TRUE", "act_FALSE")
    fn <- get_cell(tab, "pred_FALSE", "act_TRUE")
    tn <- get_cell(tab, "pred_FALSE", "act_FALSE")
    
    n <- tp + tn + fp + fn
    accuracy <- if (n > 0) (tp + tn) / n else NA_real_
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall    <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    fpr <- 1 - specificity
    fdr <- ifelse((tp + fp) > 0, fp / (tp + fp), NA_real_)
    f1 <- ifelse(!is.na(precision) && !is.na(recall) && (precision + recall) > 0,
                 2 * precision * recall / (precision + recall), NA_real_)
    denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
    mcc <- ifelse(denom > 0, (tp * tn - fp * fn) / denom, NA_real_)
    
    tibble(
      n = n,
      TP = tp, FP = fp, FN = fn, TN = tn,
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      specificity = specificity,
      fpr = fpr,
      fdr = fdr,
      f1 = f1,
      mcc = mcc
    )
  }
  
  # ---- REPLACE existing eval_confusionmatrix with this robust version ----
  eval_confusionmatrix <- function(df, pos_outcome_value, prediction_signal_value) {
    predicted_pos <- df$signal_position == prediction_signal_value
    
    if (pos_outcome_value ==  1L) {
      actual_pos <- df$success ==  1L
      actual_neg <- df$success == -1L
    } else if (pos_outcome_value == -1L) {
      actual_pos <- df$success == -1L
      actual_neg <- df$success ==  1L
    } else {
      actual_pos <- df$success ==  0L
      actual_neg <- df$success !=  0L
    }
    
    tp <- sum(predicted_pos & actual_pos, na.rm = TRUE)
    fp <- sum(predicted_pos & actual_neg, na.rm = TRUE)
    fn <- sum((!predicted_pos) & actual_pos, na.rm = TRUE)
    tn <- sum((!predicted_pos) & actual_neg, na.rm = TRUE)
    
    mat <- matrix(c(tp, fp, fn, tn), nrow = 2, byrow = TRUE)
    rownames(mat) <- c("pred_TRUE", "pred_FALSE")
    colnames(mat) <- c("act_TRUE", "act_FALSE")
    
    stats <- confusion_stats_from_table(mat)
    list(table = mat, stats = stats)
  }
  
  tidy_case <- function(name, case_obj) {
    stats <- case_obj$stats %>% mutate(case = name) %>% relocate(case)
    list(name = name, stats = stats, cm = case_obj$table)
  }
  
  # -----------------------
  # Core evaluation flow
  # -----------------------
  
  test_returns_success <- test_returns %>%
    transmute(
      ticker,
      success = case_when(
        test_returns >  return_threshold ~  1L,
        test_returns < -return_threshold ~ -1L,
        TRUE                             ~  0L
      )
    )
  
  evaluation_data <- signal_position %>%
    inner_join(test_returns_success, by = "ticker") %>%
    left_join(test_returns, by = "ticker") %>%
    mutate(
      return_bucket = case_when(
        success ==  1 ~ "r > threshold",
        success ==  0 ~ "-threshold <= r <= threshold",
        success == -1 ~ "r < -threshold"
      ),
      signal = case_when(
        signal_position ==  1 ~ "Long",
        signal_position ==  0 ~ "Neutral",
        signal_position == -1 ~ "Short"
      )
    ) %>%
    mutate(
      return_bucket = factor(return_bucket,
                             levels = c("r > threshold",
                                        "-threshold <= r <= threshold",
                                        "r < -threshold")),
      signal = factor(signal, levels = c("Long", "Neutral", "Short"))
    ) %>%
    mutate(combo = interaction(signal, return_bucket, sep = " | "))
  
  # -----------------------
  # Trade results
  # -----------------------
  TradeResults <- evaluation_data %>%
    mutate(return_category = return_bucket) %>%
    group_by(return_category, signal_position) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(
      names_from  = signal_position,
      values_from = count,
      values_fill = 0
    )
  
  map_names <- c("-1" = "Short", "0" = "Neutral", "1" = "Long")
  for (old in names(map_names)) {
    if (old %in% names(TradeResults)) {
      names(TradeResults)[names(TradeResults) == old] <- map_names[[old]]
    }
  }
  
  if ("return_category" %in% names(TradeResults)) {
    names(TradeResults)[names(TradeResults) == "return_category"] <- "Test Period Returns"
  }
  
  for (col in c("Long", "Neutral", "Short")) {
    if (!col %in% names(TradeResults)) {
      TradeResults[[col]] <- 0L
    }
    TradeResults[[col]] <- as.integer(TradeResults[[col]])
  }
  
  TradeResults <- TradeResults %>%
    mutate(Sum = Long + Neutral + Short) %>%
    select(`Test Period Returns`, Long, Neutral, Short, Sum) %>%
    arrange(`Test Period Returns`)
  
  totals_row <- TradeResults %>%
    summarise(
      `Test Period Returns` = "Sum",
      Long    = sum(Long, na.rm = TRUE),
      Neutral = sum(Neutral, na.rm = TRUE),
      Short   = sum(Short, na.rm = TRUE),
      Sum     = sum(Sum, na.rm = TRUE)
    )
  
  TradeResults <- bind_rows(TradeResults, totals_row)
  
  total_sum <- TradeResults$Sum[TradeResults$`Test Period Returns` == "Sum"]
  TradeResults_Percent <- TradeResults %>%
    mutate(
      Long = Long / total_sum,
      Neutral = Neutral / total_sum,
      Short = Short / total_sum, 
      Sum = Sum / total_sum
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # -----------------------
  # Summary statistics
  # -----------------------
  tab_overall <- evaluation_data %>%
    summarise(
      n    = sum(!is.na(test_returns)),
      mean = mean(test_returns, na.rm = TRUE),
      sd   = sd(test_returns, na.rm = TRUE),
      skew = e1071::skewness(test_returns, na.rm = TRUE),
      kurt = e1071::kurtosis(test_returns, na.rm = TRUE),
      min  = min(test_returns, na.rm = TRUE),
      q.05 = quantile(test_returns, 0.05, na.rm = TRUE, names = FALSE),
      q.10 = quantile(test_returns, 0.10, na.rm = TRUE, names = FALSE),
      q.25 = quantile(test_returns, 0.25, na.rm = TRUE, names = FALSE),
      q.50 = quantile(test_returns, 0.50, na.rm = TRUE, names = FALSE),
      q.75 = quantile(test_returns, 0.75, na.rm = TRUE, names = FALSE),
      q.90 = quantile(test_returns, 0.90, na.rm = TRUE, names = FALSE),
      q.95 = quantile(test_returns, 0.95, na.rm = TRUE, names = FALSE),
      max  = max(test_returns, na.rm = TRUE)
    ) %>%
    mutate(group = "overall") %>%
    relocate(group)
  
  tab_signal <- summarise_stats(evaluation_data, "signal")
  tab_bucket <- summarise_stats(evaluation_data, "return_bucket")
  tab_combo  <- summarise_stats(evaluation_data, "combo")
  
  summary_table <- bind_rows(tab_overall, tab_signal, tab_bucket, tab_combo) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
    mutate(ordering = dplyr::case_when(
      group == "overall" ~ 0,
      group %in% c("Long","Neutral","Short") ~ 1,
      group %in% c("r > threshold","-threshold <= r <= threshold","r < -threshold") ~ 2,
      TRUE ~ 3
    )) %>%
    arrange(ordering, group) %>%
    select(-ordering)
  
  # -----------------------
  # Information Coefficient
  # -----------------------
  ic_all <- calculate_ic(evaluation_data$signal_position, evaluation_data$test_returns)
  ic_act <- calculate_ic_acted_only(evaluation_data$signal_position, evaluation_data$test_returns)
  
  summary_ic_tbl <- tibble(
    Metric = c("IC", "t-stat", "p-value", "N"),
    All = c(ic_all$ic, ic_all$t_stat, ic_all$p_value, ic_all$n),
    Acted_Only = c(ic_act$ic, ic_act$t_stat, ic_act$p_value, ic_act$n_acted)
  ) %>% mutate(across(where(is.numeric), ~ round(.x, 4)))
  
  # -----------------------
  # Hit rate
  # -----------------------
  hit <- calculate_hit_rate(evaluation_data$signal_position, evaluation_data$test_returns)
  
  tbl_tidy <- tibble(
    Metric = c("Hit rate", "Hits", "N", "z", "p-value", "Coverage"),
    Overall = c(hit$hit_rate_overall, hit$overall_hits, hit$overall_n,
                hit$z_overall, hit$p_overall, hit$coverage),
    ActedOnly = c(hit$hit_rate_acted, hit$acted_hits, hit$acted_n,
                  hit$z_acted, hit$p_acted, hit$coverage)
  )
  tbl_tidy <- tbl_tidy %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # -----------------------
  # Confusion matrices & stats
  # -----------------------
  long_case <- eval_confusionmatrix(evaluation_data, pos_outcome_value = 1L, prediction_signal_value = 1L)
  short_case <- eval_confusionmatrix(evaluation_data, pos_outcome_value = -1L, prediction_signal_value = -1L)
  neutral_case <- eval_confusionmatrix(evaluation_data, pos_outcome_value = 0L, prediction_signal_value = 0L)
  
  long_t <- tidy_case("Long vs Not-Long", long_case)
  short_t <- tidy_case("Short vs Not-Short", short_case)
  neutral_t <- tidy_case("Neutral vs Not-Neutral", neutral_case)
  
  report_stats <- bind_rows(long_t$stats, short_t$stats, neutral_t$stats)
  
  report_stats_pretty <- report_stats %>%
    mutate(
      type1_error = fpr,
      type2_error = 1 - recall
    ) %>%
    mutate(across(c(n, TP, FP, FN, TN), as.integer)) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    select(
      case, n, TP, FP, FN, TN,
      accuracy,
      precision,
      recall,
      specificity,
      type1_error,
      type2_error,
      f1,
      mcc
    ) %>%
    pivot_longer(cols = -case, names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric = factor(
        Metric,
        levels = c(
          "TP",
          "FP",
          "FN",
          "TN",
          #"n",
          "accuracy",
          "precision",
          "recall",
          "specificity",
          "type1_error",
          "type2_error",
          "f1",
          "mcc"
        )
      )
    ) %>%
    pivot_wider(names_from = case, values_from = Value) %>%
    arrange(Metric) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  
  # -----------------------
  # New: Return comparison table (4 rows)
  # -----------------------
  compute_return_comparison_table <- function(df, ret_col = "test_returns", sig_col = "signal_position") {
    # helper to extract numeric vector for a group
    vec <- function(condition) {
      x <- df[[ret_col]][which(condition)]
      as.numeric(x[!is.na(x)])
    }
    
    compute_row <- function(name, condA, condB, alt = "greater") {
      x <- vec(condA)
      y <- vec(condB)
      
      mean_x <- if (length(x) > 0) mean(x) else NA_real_
      mean_y <- if (length(y) > 0) mean(y) else NA_real_
      avg_diff <- mean_x - mean_y
      
      if (length(x) >= 2 && length(y) >= 2) {
        tt <- t.test(x, y, alternative = alt, var.equal = FALSE)
        tstat <- as.numeric(tt$statistic)
        pval  <- as.numeric(tt$p.value)
      } else {
        tstat <- NA_real_
        pval  <- NA_real_
      }
      
      tibble::tibble(
        comparison = name,
        Avg_Ret = avg_diff,
        t_stat  = tstat,
        p_one_sided = pval,
        n_A = length(x),
        n_B = length(y)
      )
    }
    
    is_long    <- df[[sig_col]] ==  1L
    is_short   <- df[[sig_col]] == -1L
    is_neutral <- df[[sig_col]] ==  0L
    
    rows <- list(
      compute_row("Long vs Not-Long",     is_long,            !is_long),
      compute_row("Short vs Not-Short",   is_short,           !is_short),
      compute_row("Neutral vs Not-Neutral", is_neutral,       !is_neutral),
      compute_row("Long vs Short",         is_long,            is_short)
    )
    
    res <- do.call(rbind, rows)
    res <- res[, c("comparison", "Avg_Ret", "t_stat", "p_one_sided", "n_A", "n_B")]
    return(res)
  }
  
  # -----------------------
  # Plots (ggplot objects returned)
  # -----------------------
  p_signal <- ggplot(evaluation_data, aes(x = signal, y = test_returns)) +
    geom_violin(trim = FALSE, alpha = 0.4) +
    geom_boxplot(width = 0.15, outlier.shape = NA) +
    labs(title = "Return Distribution by Signal", y = "Test returns", x = NULL) +
    theme_minimal()
  
  p_facet <- ggplot(evaluation_data, aes(x = signal, y = test_returns)) +
    geom_violin(trim = FALSE, alpha = 0.4) +
    geom_boxplot(width = 0.15, outlier.shape = NA) +
    facet_wrap(~ return_bucket) +
    labs(title = "Signal performance conditional on return bucket", y = "Test returns", x = NULL) +
    theme_minimal()
  
  p_combo <- ggplot(evaluation_data, aes(x = combo, y = test_returns)) +
    geom_violin(trim = FALSE, alpha = 0.4) +
    geom_boxplot(width = 0.12, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 21, fill = "white", size = 2) +
    labs(title = "Return distributions for each Signal × Outcome intersection",
         x = NULL, y = "Test returns") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # -----------------------
  # Return structured output
  # -----------------------
  # compute return comparison table and include in output
  return_comp <- compute_return_comparison_table(evaluation_data)
  
  out <- list(
    Meta = Meta,
    return_threshold = return_threshold,
    evaluation_data = evaluation_data,
    TradeResults = TradeResults,
    TradeResults_Percent = TradeResults_Percent,
    summary_table = summary_table,
    summary_ic_tbl = summary_ic_tbl,
    hit_table = tbl_tidy,
    confusion = list(long = long_t, short = short_t, neutral = neutral_t),
    confusion_report = report_stats_pretty,
    return_comparison = return_comp,
    plots = list(signal = p_signal, facet = p_facet, combo = p_combo)
  )
  
  return(out)
}
