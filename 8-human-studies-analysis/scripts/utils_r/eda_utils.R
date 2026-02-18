# =============================================================================
# EDA Utilities
# =============================================================================
# Exploratory data analysis functions including treatment association tests
# and t-test utilities.
#
# Usage:
#   source("scripts/utils_r/eda_utils.R")
#
#   # Chi-squared tests (categorical outcomes)
#   test_treatment(data, "outcome_var", "treatment_var")
#   run_treatment_tests(data, outcome_vars, treatment_vars)
#   run_standard_treatment_tests(data_stats, cross_study_outcomes)
#
#   # T-tests (continuous outcomes)
#   run_one_sample_ttest(data, "outcome_var", mu = 0)
#   run_two_sample_ttest(data, "outcome_var", "treatment_var")
#   run_two_sample_ttest_battery(data, outcome_vars, treatment_vars)
# =============================================================================

library(tidyverse)

# =============================================================================
# TREATMENT ASSOCIATION TESTS
# =============================================================================

#' Test association between treatment and outcome (chi-squared)
#'
#' Performs chi-squared test of independence between a treatment variable
#' and an outcome variable. Prints results and returns a summary list.
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable (character)
#' @param treatment_var Name of treatment variable (character)
#' @param alpha Significance threshold (default 0.05)
#' @param verbose Whether to print results (default TRUE)
#' @return List with test results
test_treatment <- function(data, outcome_var, treatment_var,
                           alpha = 0.05, verbose = TRUE) {
  # Create contingency table
  tbl <- table(data[[treatment_var]], data[[outcome_var]])
  tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0]

  p_value <- NA
  min_expected <- NA
  chi_sq <- NA
  df <- NA

 if (min(dim(tbl)) > 1 && sum(tbl) > 0) {
    tryCatch({
      chi_test <- chisq.test(tbl)
      p_value <- chi_test$p.value
      min_expected <- min(chi_test$expected)
      chi_sq <- chi_test$statistic
      df <- chi_test$parameter
    }, error = function(e) {
      p_value <<- NA
      min_expected <<- NA
    })
  }

  warning_note <- if (!is.na(min_expected) && min_expected < 5) " *" else ""
  significant <- !is.na(p_value) && p_value < alpha
  significance <- if (significant) " (SIGNIFICANT)" else ""

  if (verbose) {
    cat(sprintf("%-30s p = %s%s%s\n",
      treatment_var,
      ifelse(is.na(p_value), "NA", sprintf("%.4f", p_value)),
      warning_note, significance))

    # If significant, print percentages
    if (significant) {
      cat("\n  Percentages by group:\n")
      percentages <- data %>%
        filter(!is.na(.data[[outcome_var]]) & .data[[outcome_var]] != "") %>%
        group_by(.data[[treatment_var]]) %>%
        count(.data[[outcome_var]]) %>%
        mutate(percentage = n / sum(n) * 100) %>%
        ungroup()

      for (group in unique(percentages[[treatment_var]])) {
        cat(sprintf("  %s:\n", group))
        group_data <- percentages %>% filter(.data[[treatment_var]] == group)
        for (j in seq_len(nrow(group_data))) {
          cat(sprintf("    %s: %.1f%% (n=%d)\n",
            group_data[[outcome_var]][j],
            group_data$percentage[j],
            group_data$n[j]))
        }
      }
      cat("\n")
    }

    if (!is.na(min_expected) && min_expected < 5) {
      cat("  * Warning: Some cells have expected frequency < 5\n")
    }
  }

  # Compute breakdown data (for significant results)
  breakdown <- NULL
  if (significant) {
    breakdown <- data %>%
      filter(!is.na(.data[[outcome_var]]) & .data[[outcome_var]] != "") %>%
      group_by(.data[[treatment_var]]) %>%
      count(.data[[outcome_var]]) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup()
    names(breakdown)[1:2] <- c("group", "level")
  }

  # Return result
  list(
    treatment = treatment_var,
    outcome = outcome_var,
    chi_sq = as.numeric(chi_sq),
    df = as.numeric(df),
    p_value = p_value,
    min_expected = min_expected,
    significant = significant,
    breakdown = breakdown
  )
}

#' Run treatment tests across multiple outcomes and treatments
#'
#' Runs chi-squared tests for all combinations of outcomes and treatments.
#'
#' @param data Data frame
#' @param outcome_vars Character vector of outcome variable names
#' @param treatment_vars Character vector of treatment variable names
#' @param alpha Significance threshold (default 0.05)
#' @param verbose Whether to print results (default TRUE)
#' @return List of test results
run_treatment_tests <- function(data, outcome_vars, treatment_vars,
                                alpha = 0.05, verbose = TRUE) {
  results <- list()

  for (outcome in outcome_vars) {
    if (verbose) cat(sprintf("\nOutcome: %s\n", outcome))
    results[[outcome]] <- lapply(treatment_vars, function(t) {
      test_treatment(data, outcome, t, alpha = alpha, verbose = verbose)
    })
    names(results[[outcome]]) <- treatment_vars
  }

  results
}

#' Run standard treatment tests for main studies
#'
#' Convenience function that runs treatment tests for cross-study,
#' cross-sectional, and longitudinal comparisons.
#'
#' @param data_stats Combined data with study_id column
#' @param cross_study_outcomes Outcomes to test across studies
#' @param longitudinal_outcomes Additional outcomes for longitudinal only
#' @param treatment_vars Treatment variables to test (default: standard set)
#' @return List with results for each comparison type
run_standard_treatment_tests <- function(data_stats,
                                         cross_study_outcomes,
                                         longitudinal_outcomes = NULL,
                                         treatment_vars = NULL) {
  if (is.null(treatment_vars)) {
    treatment_vars <- c("personalisation", "domain",
                        "relationship_seeking_category", "lambda_factor")
  }

  results <- list()

  # Cross-study comparison
  cat("\n=== Cross-Study Comparison ===\n")
  results$cross_study <- run_treatment_tests(
    data_stats, cross_study_outcomes, "study_id")

  # Cross-sectional
  cat("\n=== Cross-Sectional ===\n")
  data_cs <- data_stats %>% filter(study_id == "cross-sectional")
  results$cross_sectional <- run_treatment_tests(
    data_cs, cross_study_outcomes, treatment_vars)

  # Longitudinal
  cat("\n=== Longitudinal ===\n")
  data_long <- data_stats %>% filter(study_id == "longitudinal")
  long_outcomes <- if (!is.null(longitudinal_outcomes)) {
    c(cross_study_outcomes, longitudinal_outcomes)
  } else {
    cross_study_outcomes
  }
  results$longitudinal <- run_treatment_tests(
    data_long, long_outcomes, treatment_vars)

  results
}

# =============================================================================
# T-TEST UTILITIES
# =============================================================================

#' Run one-sample t-test testing if value differs from a reference (default 0)
#'
#' @param data Data frame
#' @param outcome_var Name of outcome column
#' @param mu Reference value to test against (default 0)
#' @param study_name Label for study/subset (for output)
#' @param verbose Whether to print results (default TRUE)
#' @return List with test results or NULL if insufficient data
run_one_sample_ttest <- function(data, outcome_var, mu = 0,
                                  study_name = "All", verbose = TRUE) {
  if (!outcome_var %in% names(data)) {
    if (verbose) cat(sprintf("  %s: Column not found\n", outcome_var))
    return(NULL)
  }

  values <- data[[outcome_var]]
  values <- values[!is.na(values)]

  if (length(values) < 5) {
    if (verbose) cat(sprintf("  %s: Not enough data (n = %d)\n", outcome_var, length(values)))
    return(NULL)
  }

  t_result <- t.test(values, mu = mu)

  if (verbose) {
    cat(sprintf("\n%s - %s:\n", study_name, outcome_var))
    cat(sprintf("  Mean: %.2f (SD = %.2f), n = %d\n",
                mean(values), sd(values), length(values)))
    cat(sprintf("  One-sample t(%d) = %.3f, p = %.4f\n",
                as.integer(t_result$parameter),
                t_result$statistic,
                t_result$p.value))
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n",
                t_result$conf.int[1], t_result$conf.int[2]))

    sig <- if (t_result$p.value < 0.001) "***" else
           if (t_result$p.value < 0.01) "**" else
           if (t_result$p.value < 0.05) "*" else ""
    if (sig != "") {
      direction <- if (mean(values) > mu) "GREATER" else "LESS"
      cat(sprintf("  SIGNIFICANT %s: Mean is %s than %.2f\n", sig, direction, mu))
    }
  }

  list(
    study = study_name,
    outcome = outcome_var,
    n = length(values),
    mean = mean(values),
    sd = sd(values),
    mu = mu,
    t_stat = as.numeric(t_result$statistic),
    df = as.numeric(t_result$parameter),
    p_value = t_result$p.value,
    ci_lower = t_result$conf.int[1],
    ci_upper = t_result$conf.int[2]
  )
}

#' Run two-sample (independent) t-test comparing treatment groups
#'
#' @param data Data frame
#' @param outcome_var Name of continuous outcome column
#' @param treatment_var Name of treatment/grouping column
#' @param study_name Label for study/subset (for output)
#' @param verbose Whether to print results (default TRUE)
#' @return List with test results or NULL if insufficient data
run_two_sample_ttest <- function(data, outcome_var, treatment_var,
                                  study_name = "All", verbose = TRUE) {
  clean_data <- data %>%
    filter(!is.na(.data[[outcome_var]]) & !is.na(.data[[treatment_var]]))

  # For relationship_seeking_category: reduce to 2 groups (pos vs neg)
  if (treatment_var == "relationship_seeking_category") {
    clean_data <- clean_data %>%
      filter(.data[[treatment_var]] %in% c("neg_lambda", "pos_lambda"))
  }

  groups <- split(clean_data[[outcome_var]], clean_data[[treatment_var]])
  groups <- groups[sapply(groups, length) > 0]

  # Need exactly 2 groups for t-test
  if (length(groups) != 2) {
    if (verbose) {
      cat(sprintf("  %s ~ %s: Skipped (need exactly 2 groups, found %d)\n",
                  outcome_var, treatment_var, length(groups)))
    }
    return(NULL)
  }

  # Check minimum sample size
  if (any(sapply(groups, length) < 5)) {
    if (verbose) {
      cat(sprintf("  %s ~ %s: Skipped (group size < 5)\n",
                  outcome_var, treatment_var))
    }
    return(NULL)
  }

  group_names <- names(groups)

  # Run two-sample t-test
  t_result <- tryCatch(
    t.test(groups[[1]], groups[[2]]),
    error = function(e) NULL
  )

  if (is.null(t_result)) {
    if (verbose) cat(sprintf("  %s ~ %s: t-test failed\n", outcome_var, treatment_var))
    return(NULL)
  }

  if (verbose) {
    cat(sprintf("\n%s: %s ~ %s\n", study_name, outcome_var, treatment_var))
    cat(sprintf("  %s: n = %d, mean = %.2f, sd = %.2f\n",
                group_names[1], length(groups[[1]]),
                mean(groups[[1]], na.rm = TRUE),
                sd(groups[[1]], na.rm = TRUE)))
    cat(sprintf("  %s: n = %d, mean = %.2f, sd = %.2f\n",
                group_names[2], length(groups[[2]]),
                mean(groups[[2]], na.rm = TRUE),
                sd(groups[[2]], na.rm = TRUE)))
    cat(sprintf("  Two-sample t(%.1f) = %.3f, p = %.4f\n",
                t_result$parameter, t_result$statistic, t_result$p.value))
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n",
                t_result$conf.int[1], t_result$conf.int[2]))

    sig <- if (t_result$p.value < 0.001) "***" else
           if (t_result$p.value < 0.01) "**" else
           if (t_result$p.value < 0.05) "*" else ""
    if (sig != "") cat(sprintf("  SIGNIFICANT %s\n", sig))
  }

  list(
    study = study_name,
    outcome = outcome_var,
    treatment = treatment_var,
    group1 = group_names[1],
    group2 = group_names[2],
    n1 = length(groups[[1]]),
    n2 = length(groups[[2]]),
    mean1 = mean(groups[[1]], na.rm = TRUE),
    mean2 = mean(groups[[2]], na.rm = TRUE),
    sd1 = sd(groups[[1]], na.rm = TRUE),
    sd2 = sd(groups[[2]], na.rm = TRUE),
    t_stat = as.numeric(t_result$statistic),
    df = as.numeric(t_result$parameter),
    p_value = t_result$p.value,
    ci_lower = t_result$conf.int[1],
    ci_upper = t_result$conf.int[2]
  )
}

#' Run two-sample t-tests for multiple outcome/treatment combinations
#'
#' @param data Data frame
#' @param outcome_vars Character vector of outcome variable names
#' @param treatment_vars Character vector of treatment variable names
#' @param study_name Label for study/subset
#' @param verbose Whether to print results (default TRUE)
#' @return Data frame with all test results
run_two_sample_ttest_battery <- function(data, outcome_vars, treatment_vars,
                                          study_name = "All", verbose = TRUE) {
  results <- list()

  for (outcome in outcome_vars) {
    for (treatment in treatment_vars) {
      result <- run_two_sample_ttest(data, outcome, treatment, study_name, verbose)
      if (!is.null(result)) {
        key <- paste(outcome, treatment, sep = "_")
        results[[key]] <- result
      }
    }
  }

  if (length(results) > 0) {
    bind_rows(results)
  } else {
    NULL
  }
}

