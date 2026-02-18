#!/usr/bin/env Rscript
# =============================================================================
# Decoupling Analysis
# =============================================================================
#
# Compares "liking" slopes (engagingness/likability) vs "wanting" slopes
# (separation_distress or seeking_companionship) to identify behavioral patterns:
#   - Decoupled Dependency: liking down, wanting up
#   - Aligned Engagement: both up
#   - Aligned Disengagement: both down
#   - Decoupled Satiation: liking up, wanting down
#
# Runs comparison analyses by:
#   - relationship_seeking_category (neg_lambda vs pos_lambda)
#   - domain (polchat vs emotchat)
#   - companionship_condition (companionship vs non_companionship)
#
# Usage:
#   Rscript scripts/analysis/decoupling.R
#   Rscript scripts/analysis/decoupling.R --generate_report   # Also generate markdown report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(lme4)
library(lmerTest)
library(gridExtra)
library(knitr)
library(ggpattern)
library(parameters)

set.seed(1234)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
generate_report <- "--generate_report" %in% args
generate_tex_tables <- "--generate_tex_tables" %in% args

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
DATA_DIR <- paths$DATA_DIR
FIGURE_DIR <- paths$FIGURE_DIR
STATS_DIR <- paths$STATS_DIR
REPORT_DIR <- paths$REPORT_DIR

# Source utility functions
source("scripts/utils_r/data_utils.R")
source("scripts/utils_r/coarsen_and_factor_sociodemos.R")
source("scripts/utils_r/plot_config.R")
source("scripts/utils_r/labelling_utils.R")
source("scripts/utils_r/latex_utils.R")

TABLE_DIR <- paths$TABLE_DIR

# Create output directories
dir.create(file.path(FIGURE_DIR, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURE_DIR, "png"), recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Decoupling Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# =============================================================================
# COLOR PALETTES
# =============================================================================

lambda_colors <- c(
  "-1"   = "#3b4cc0",
  "-0.5" = "#8db0fe",
  "0"    = "#918f8f",
  "0.5"  = "#f4987a",
  "1"    = "#b40426"
)

behavior_colors <- c(
  "Decoupled Dependency" = "#d62728",
  "Aligned Engagement" = "#2ca02c",
  "Aligned Disengagement" = "#7f7f7f",
  "Decoupled Satiation" = "#1f77b4"
)

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Load data using standard load_task_data helper
preferences_raw <- load_task_data("preferences", DATA_DIR) %>%
  prepare_treatment_arms()

attachment_raw <- load_task_data("attachment", DATA_DIR) %>%
  prepare_treatment_arms()

seeking_companionship_raw <- load_task_data("seeking-companionship", DATA_DIR) %>%
  prepare_treatment_arms()

goodbye_raw <- load_task_data("goodbye", DATA_DIR) %>%
  prepare_treatment_arms()

# -----------------------------------------------------------------------------
# Create pooled (long) format for liking data
# Preferences has columns: likeability, engagingness, helpfulness
# We pool likeability + engagingness into a single outcome_value column
# -----------------------------------------------------------------------------

LIKING_VARS <- c("likeability", "engagingness")

liking_data <- preferences_raw %>%
  filter(study_id == "longitudinal") %>%
  pivot_longer(
    cols = all_of(LIKING_VARS),
    names_to = "construct",
    values_to = "outcome_value"
  ) %>%
  filter(!is.na(outcome_value)) %>%
  mutate(construct = factor(construct, levels = LIKING_VARS))

cat(sprintf("Liking observations: %d (pooled likeability + engagingness)\n", nrow(liking_data)))
cat(sprintf("  Unique participants: %d\n", n_distinct(liking_data$ppt_id)))

# -----------------------------------------------------------------------------
# Separation distress data (from attachment)
# Attachment has columns: separation_distress, self_disclosure, etc.
# We just select the separation_distress column directly
# -----------------------------------------------------------------------------

separation_distress_data <- attachment_raw %>%
  filter(study_id == "longitudinal") %>%
  select(ppt_id, lambda, domain, personalisation, relationship_seeking_category,
         week, week_numeric, separation_distress) %>%
  filter(!is.na(separation_distress)) %>%
  rename(outcome_value = separation_distress)

cat(sprintf("Separation distress observations: %d\n", nrow(separation_distress_data)))
cat(sprintf("  Unique participants: %d\n", n_distinct(separation_distress_data$ppt_id)))

# -----------------------------------------------------------------------------
# Seeking companionship data
# Has column: seeking_companionship_likelihood
# -----------------------------------------------------------------------------

seeking_companionship_data <- seeking_companionship_raw %>%
  filter(study_id == "longitudinal") %>%
  select(ppt_id, lambda, domain, personalisation, relationship_seeking_category,
         week, week_numeric, seeking_companionship_likelihood) %>%
  filter(!is.na(seeking_companionship_likelihood)) %>%
  rename(outcome_value = seeking_companionship_likelihood)

cat(sprintf("Seeking companionship observations: %d\n", nrow(seeking_companionship_data)))
cat(sprintf("  Unique participants: %d\n", n_distinct(seeking_companionship_data$ppt_id)))

# -----------------------------------------------------------------------------
# Goodbye data (for validation)
# -----------------------------------------------------------------------------

goodbye_data <- goodbye_raw %>%
  filter(study_id == "longitudinal")

cat(sprintf("Goodbye observations: %d\n", nrow(goodbye_data)))

# -----------------------------------------------------------------------------
# Keep raw data for predictive analysis
# -----------------------------------------------------------------------------

attachment_data <- attachment_raw
seeking_companionship_raw_all <- seeking_companionship_raw

# -----------------------------------------------------------------------------
# Session attendance statistics (validates * 5 timescale conversion)
# -----------------------------------------------------------------------------

cat("\n--- Session Attendance Statistics (Liking Data) ---\n")

sessions_per_participant <- liking_data %>%
  group_by(ppt_id) %>%
  summarise(n_sessions = n_distinct(session_numeric), .groups = "drop")

n_ppts <- nrow(sessions_per_participant)
mean_sessions <- mean(sessions_per_participant$n_sessions)
sd_sessions <- sd(sessions_per_participant$n_sessions)
min_sessions <- min(sessions_per_participant$n_sessions)
max_sessions <- max(sessions_per_participant$n_sessions)
median_sessions <- median(sessions_per_participant$n_sessions)

# Count participants with full attendance (20 sessions)
n_full_attendance <- sum(sessions_per_participant$n_sessions == 20)
pct_full_attendance <- 100 * n_full_attendance / n_ppts

# Count participants with >= 18 sessions (90%+ attendance)
n_high_attendance <- sum(sessions_per_participant$n_sessions >= 18)
pct_high_attendance <- 100 * n_high_attendance / n_ppts

cat(sprintf("  N participants: %d\n", n_ppts))
cat(sprintf("  Sessions per participant:\n"))
cat(sprintf("    Mean: %.1f (SD: %.1f)\n", mean_sessions, sd_sessions))
cat(sprintf("    Median: %.0f\n", median_sessions))
cat(sprintf("    Range: %d - %d\n", min_sessions, max_sessions))
cat(sprintf("  Full attendance (20 sessions): %d (%.1f%%)\n",
            n_full_attendance, pct_full_attendance))
cat(sprintf("  High attendance (>=18 sessions): %d (%.1f%%)\n",
            n_high_attendance, pct_high_attendance))

# Distribution table
session_dist <- sessions_per_participant %>%
  count(n_sessions) %>%
  mutate(pct = 100 * n / sum(n))

cat("\n  Distribution of session counts:\n")
for (i in seq_len(nrow(session_dist))) {
  cat(sprintf("    %2d sessions: %3d participants (%.1f%%)\n",
              session_dist$n_sessions[i], session_dist$n[i], session_dist$pct[i]))
}

# Store for report
session_attendance_stats <- list(
  n_participants = n_ppts,
  mean = mean_sessions,
  sd = sd_sessions,
  median = median_sessions,
  min = min_sessions,
  max = max_sessions,
  n_full = n_full_attendance,
  pct_full = pct_full_attendance,
  n_high = n_high_attendance,
  pct_high = pct_high_attendance,
  distribution = session_dist
)

cat("\n")

# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================
#
# NOTE: TIMESCALE ASSUMPTION
# --------------------------
# This analysis combines slopes from two different timescales:
#   - Liking slopes: fit on session_numeric (1-20, ~daily sessions)
#   - Wanting slopes: fit on week_numeric (1-4, weekly measurements)
#
# To compare them, we convert liking slopes to a weekly scale by multiplying
# by 5 (assuming ~5 sessions per week). This is a SIMPLIFYING ASSUMPTION:
#   - The actual sessions-per-week varies across participants
#   - But most participants attend all/nearly all sessions (see stats above)
#
# Session attendance statistics are computed in SECTION 2 and stored in
# `session_attendance_stats` for inclusion in reports.
#
# NOTE: SCALE COMPARISON
# ----------------------
# decoupling_score = wanting_slope - liking_slope_weekly
#
# Both liking (0-100 VAS) and wanting (0-100 VAS) measure different constructs.
# If variances differ substantially, one slope dominates the decoupling score.
# We compute and report variance ratios after slopes are extracted (STEP 1).
# A ratio outside 0.5-2.0 suggests potential scale mismatch; consider
# standardizing slopes before subtraction if this occurs.
#
# Scale comparison statistics are stored in `slope_scale_stats` for reports.
#
# =============================================================================

#' Fit liking model and extract individual slopes
#'
#' @param liking_data Data frame with liking outcomes (engagingness, likability)
#' @return List with model and slopes data frame
fit_liking_slopes <- function(liking_data) {
  cat("  Fitting liking model (lmer with random slopes)...\n")

  # Center session_numeric to improve model conditioning (avoids near-unidentifiability)
  # Slope interpretation unchanged (change per session)
  liking_data <- liking_data %>%
    mutate(session_numeric_c = session_numeric - mean(session_numeric))

  liking_model <- lmer(
    outcome_value ~ session_numeric_c +
      (session_numeric_c | ppt_id),
    data = liking_data,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
  )

  liking_fixed_slope <- fixef(liking_model)["session_numeric_c"]
  liking_random_effects <- ranef(liking_model)$ppt_id

  liking_slopes <- liking_random_effects %>%
    as.data.frame() %>%
    rownames_to_column("ppt_id") %>%
    mutate(liking_slope = session_numeric_c + liking_fixed_slope) %>%
    select(ppt_id, liking_slope) %>%
    left_join(
      liking_data %>% distinct(ppt_id, lambda, domain, personalisation, relationship_seeking_category),
      by = "ppt_id"
    ) %>%
    mutate(
      companionship_condition = case_when(
        relationship_seeking_category == "pos_lambda" & domain == "emotchat" ~ "companionship",
        relationship_seeking_category == "neg_lambda" & domain == "polchat" ~ "non_companionship",
        TRUE ~ "other"
      )
    )

  cat(sprintf("  Extracted slopes for %d participants\n", nrow(liking_slopes)))
  return(list(model = liking_model, slopes = liking_slopes))
}

#' Fit wanting model and extract slopes (with fallback for pre-post data)
#'
#' @param wanting_data Data frame with wanting outcomes
#' @return List with model (may be NULL) and slopes data frame
fit_wanting_slopes <- function(wanting_data) {
  # Check if this is pre-post data with delta scores
  if ("outcome_value_delta" %in% names(wanting_data)) {
    cat("  Note: Pre-post data detected. Using outcome_value_delta as slope.\n")

    wanting_slopes <- wanting_data %>%
      distinct(ppt_id, outcome_value_delta, lambda, domain, personalisation, relationship_seeking_category) %>%
      rename(wanting_slope = outcome_value_delta) %>%
      mutate(
        companionship_condition = case_when(
          relationship_seeking_category == "pos_lambda" & domain == "emotchat" ~ "companionship",
          relationship_seeking_category == "neg_lambda" & domain == "polchat" ~ "non_companionship",
          TRUE ~ "other"
        )
      )

    cat(sprintf("  Extracted slopes for %d participants\n", nrow(wanting_slopes)))
    return(list(model = NULL, slopes = wanting_slopes))
  }

  # Check number of timepoints
  timepoints_per_ppt <- wanting_data %>%
    group_by(ppt_id) %>%
    summarise(n_timepoints = n_distinct(week_numeric), .groups = "drop")

  max_timepoints <- max(timepoints_per_ppt$n_timepoints)

  if (max_timepoints <= 2) {
    cat("  Note: Only 2 timepoints detected. Using difference scores instead of mixed model.\n")

    # Only include participants with BOTH w0 (pre) and w4 (post)
    # NOTE: This path triggers for seeking_companionship which has pre-treatment
    # measurement at week 0.
    ppts_with_both <- wanting_data %>%
      group_by(ppt_id) %>%
      filter(0 %in% week_numeric & 4 %in% week_numeric) %>%
      pull(ppt_id) %>%
      unique()

    wanting_slopes <- wanting_data %>%
      filter(ppt_id %in% ppts_with_both) %>%
      group_by(ppt_id) %>%
      arrange(week_numeric) %>%
      summarise(
        wanting_slope = last(outcome_value) - first(outcome_value),
        .groups = "drop"
      ) %>%
      left_join(
        wanting_data %>% distinct(ppt_id, lambda, domain, personalisation, relationship_seeking_category),
        by = "ppt_id"
      ) %>%
      mutate(
        companionship_condition = case_when(
          relationship_seeking_category == "pos_lambda" & domain == "emotchat" ~ "companionship",
          relationship_seeking_category == "neg_lambda" & domain == "polchat" ~ "non_companionship",
          TRUE ~ "other"
        )
      )

    cat(sprintf("  Extracted slopes for %d participants\n", nrow(wanting_slopes)))
    return(list(model = NULL, slopes = wanting_slopes))
  }

  # Fit mixed model with random slopes
  cat("  Fitting wanting model (lmer with random slopes)...\n")

  wanting_model <- lmer(
    outcome_value ~ week_numeric +
      (week_numeric | ppt_id),
    data = wanting_data,
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
  )

  wanting_fixed_slope <- fixef(wanting_model)["week_numeric"]
  wanting_random_effects <- ranef(wanting_model)$ppt_id

  wanting_slopes <- wanting_random_effects %>%
    as.data.frame() %>%
    rownames_to_column("ppt_id") %>%
    mutate(wanting_slope = week_numeric + wanting_fixed_slope) %>%
    select(ppt_id, wanting_slope) %>%
    left_join(
      wanting_data %>% distinct(ppt_id, lambda, domain, personalisation, relationship_seeking_category),
      by = "ppt_id"
    ) %>%
    mutate(
      companionship_condition = case_when(
        relationship_seeking_category == "pos_lambda" & domain == "emotchat" ~ "companionship",
        relationship_seeking_category == "neg_lambda" & domain == "polchat" ~ "non_companionship",
        TRUE ~ "other"
      )
    )

  cat(sprintf("  Extracted slopes for %d participants\n", nrow(wanting_slopes)))
  return(list(model = wanting_model, slopes = wanting_slopes))
}

#' Create behavioral categories from liking and wanting slopes
#'
#' @param liking_slopes Data frame with liking slopes
#' @param wanting_slopes Data frame with wanting slopes
#' @return Data frame with combined slopes and behavioral categories
create_behavioral_categories <- function(liking_slopes, wanting_slopes) {
  combined_slopes <- liking_slopes %>%
    inner_join(
      wanting_slopes %>% select(ppt_id, wanting_slope),
      by = "ppt_id"
    ) %>%
    mutate(
      # Scale liking slope to weekly timescale (see NOTE in SECTION 3 header)
      # Assumes ~5 sessions/week - this is a simplification; actual rate varies
      liking_slope_weekly = liking_slope * 5,
      liking_direction = ifelse(liking_slope > 0, "up", "down"),
      wanting_direction = ifelse(wanting_slope > 0, "up", "down"),
      behavior_category = case_when(
        liking_direction == "down" & wanting_direction == "up" ~ "Decoupled Dependency",
        liking_direction == "up" & wanting_direction == "up" ~ "Aligned Engagement",
        liking_direction == "down" & wanting_direction == "down" ~ "Aligned Disengagement",
        liking_direction == "up" & wanting_direction == "down" ~ "Decoupled Satiation"
      ),
      # Decoupling score: wanting - liking (positive = more decoupling toward dependency)
      decoupling_score = wanting_slope - liking_slope_weekly
    )

  return(combined_slopes)
}

#' Validation check - mean outcomes by behavioral category
#'
#' @param combined_slopes Data frame with behavioral categories
#' @param attachment_raw Raw attachment data (wide format)
#' @param seeking_raw Raw seeking companionship data (wide format)
#' @param goodbye_raw Raw goodbye data
#' @return Validation summary data frame
validation_check <- function(combined_slopes, attachment_raw, seeking_raw, goodbye_raw) {
  cat("\n--- Validation: Mean Outcomes by Behavioral Category ---\n")

  # Get final timepoint values for each measure
  # Attachment data has separation_distress as a column
  sep_distress_final <- attachment_raw %>%
    filter(study_id == "longitudinal", !is.na(separation_distress)) %>%
    group_by(ppt_id) %>%
    filter(week_numeric == max(week_numeric)) %>%
    summarise(sep_distress = mean(separation_distress, na.rm = TRUE), .groups = "drop")

  # Goodbye data - check for goodbye_action column
  if ("goodbye_action" %in% names(goodbye_raw)) {
    goodbye_final <- goodbye_raw %>%
      filter(study_id == "longitudinal", !is.na(goodbye_action)) %>%
      group_by(ppt_id) %>%
      filter(session_numeric == max(session_numeric)) %>%
      summarise(goodbye = mean(goodbye_action, na.rm = TRUE), .groups = "drop")
  } else {
    # Skip if no goodbye_action column
    goodbye_final <- data.frame(ppt_id = character(), goodbye = numeric())
  }

  # Seeking companionship data has seeking_companionship_likelihood as a column
  seeking_final <- seeking_raw %>%
    filter(study_id == "longitudinal", !is.na(seeking_companionship_likelihood)) %>%
    group_by(ppt_id) %>%
    filter(week_numeric == max(week_numeric)) %>%
    summarise(seeking_companionship = mean(seeking_companionship_likelihood, na.rm = TRUE), .groups = "drop")

  validation_data <- combined_slopes %>%
    select(ppt_id, behavior_category) %>%
    left_join(sep_distress_final, by = "ppt_id") %>%
    left_join(goodbye_final, by = "ppt_id") %>%
    left_join(seeking_final, by = "ppt_id")

  validation_summary <- validation_data %>%
    group_by(behavior_category) %>%
    summarise(
      n = n(),
      mean_sep_distress = mean(sep_distress, na.rm = TRUE),
      sd_sep_distress = sd(sep_distress, na.rm = TRUE),
      mean_goodbye = mean(goodbye, na.rm = TRUE),
      sd_goodbye = sd(goodbye, na.rm = TRUE),
      mean_seeking = mean(seeking_companionship, na.rm = TRUE),
      sd_seeking = sd(seeking_companionship, na.rm = TRUE),
      .groups = "drop"
    )

  print(kable(validation_summary, digits = 2, caption = "Validation: Mean Outcomes by Behavioral Category"))

  cat("\nANOVA: Separation Distress by category\n")
  print(summary(aov(sep_distress ~ behavior_category, data = validation_data)))

  if (nrow(goodbye_final) > 0) {
    cat("\nANOVA: Goodbye by category\n")
    print(summary(aov(goodbye ~ behavior_category, data = validation_data)))
  }

  cat("\nANOVA: Seeking Companionship by category\n")
  print(summary(aov(seeking_companionship ~ behavior_category, data = validation_data)))

  return(validation_summary)
}

# =============================================================================
# SECTION 4: COMPARISON FUNCTIONS
# =============================================================================

#' Get comparison settings based on comparison variable
#'
#' @param comparison_var One of "relationship_seeking_category", "domain", "companionship_condition"
#' @return List with filter values, colors, labels, etc.
get_comparison_settings <- function(comparison_var) {
  if (comparison_var == "relationship_seeking_category") {
    return(list(
      filter_values = c("neg_lambda", "pos_lambda"),
      color_values = c("neg_lambda" = "#2166AC", "pos_lambda" = "#B2182B"),
      color_labels = c("neg_lambda" = "Non-RS (lambda<0)", "pos_lambda" = "RS (lambda>0)"),
      factor_levels = c("neg_lambda", "pos_lambda"),
      factor_labels = c("Non-RS (lambda<0)", "RS (lambda>0)"),
      title_suffix = "Relationship-Seeking Category",
      has_lambda_plot = TRUE
    ))
  } else if (comparison_var == "domain") {
    return(list(
      filter_values = c("polchat", "emotchat"),
      color_values = c("polchat" = "#006837", "emotchat" = "#78C679"),
      color_labels = c("polchat" = "Political", "emotchat" = "Emotional"),
      factor_levels = c("polchat", "emotchat"),
      factor_labels = c("Political", "Emotional"),
      title_suffix = "Domain",
      has_lambda_plot = FALSE
    ))
  } else if (comparison_var == "companionship_condition") {
    return(list(
      filter_values = c("non_companionship", "companionship"),
      color_values = c("non_companionship" = "#756BB1", "companionship" = "#E6550D"),
      color_labels = c(
        "non_companionship" = "Non-Comp.",
        "companionship" = "Comp."
      ),
      factor_levels = c("non_companionship", "companionship"),
      factor_labels = c("Non-Comp.", "Comp."),
      title_suffix = "Companionship Condition",
      has_lambda_plot = FALSE
    ))
  } else {
    stop("Unknown comparison variable: ", comparison_var)
  }
}

#' Run comparison analysis for a given comparison variable
#'
#' @param combined_slopes Data frame with behavioral categories
#' @param comparison_var One of "relationship_seeking_category", "domain", "companionship_condition"
#' @return List with test results and plots
run_comparison_analysis <- function(combined_slopes, comparison_var = "relationship_seeking_category") {
  settings <- get_comparison_settings(comparison_var)

  cat("\n============================================================\n")
  cat(sprintf("COMPARISON: %s\n", settings$title_suffix))
  cat("============================================================\n")
  cat(sprintf("Comparing: %s vs %s\n", settings$factor_labels[1], settings$factor_labels[2]))
  cat("H0: Group 2 rate <= Group 1 rate\n")
  cat("H1: Group 2 rate > Group 1 rate\n\n")

  # Filter to comparison groups
  focused_slopes <- combined_slopes %>%
    filter(.data[[comparison_var]] %in% settings$filter_values) %>%
    mutate(
      # Drop unused factor levels to avoid empty rows/columns in contingency table
      across(where(is.factor), droplevels)
    )

  group1 <- settings$filter_values[1]
  group2 <- settings$filter_values[2]

  # Contingency table (ensure both variables are factors with only observed levels)
  focused_contingency <- table(
    factor(focused_slopes[[comparison_var]]),
    factor(focused_slopes$behavior_category)
  )

  cat("Contingency Table:\n")
  print(kable(focused_contingency, caption = sprintf("Contingency Table: %s x Behavioral Category", settings$title_suffix)))

  # Chi-square test (use simulate.p.value if expected counts are low)
  focused_chisq <- tryCatch({
    chisq.test(focused_contingency, simulate.p.value = FALSE)
  }, warning = function(w) {
    # If warning about low expected counts, use simulation
    chisq.test(focused_contingency, simulate.p.value = TRUE, B = 2000)
  }, error = function(e) {
    list(statistic = NA, parameter = NA, p.value = NA)
  })

  chisq_stat <- ifelse(is.null(focused_chisq$statistic) || is.na(focused_chisq$statistic),
                       NA_real_, as.numeric(focused_chisq$statistic))
  chisq_df <- ifelse(is.null(focused_chisq$parameter), "simulated",
                     as.character(focused_chisq$parameter))
  chisq_p <- ifelse(is.null(focused_chisq$p.value) || is.na(focused_chisq$p.value),
                    NA_real_, as.numeric(focused_chisq$p.value))

  cat(sprintf(
    "\nChi-square (overall): X2 = %s, df = %s, p = %s\n",
    ifelse(is.na(chisq_stat), "NA", sprintf("%.2f", chisq_stat)),
    chisq_df,
    ifelse(is.na(chisq_p), "NA", sprintf("%.4f", chisq_p))
  ))

  # Decoupled dependency rates
  focused_dep <- focused_slopes %>%
    mutate(is_decoupled_dep = behavior_category == "Decoupled Dependency") %>%
    group_by(.data[[comparison_var]]) %>%
    summarise(
      n = n(),
      n_decoupled_dep = sum(is_decoupled_dep),
      n_other = n - n_decoupled_dep,
      pct_decoupled_dep = mean(is_decoupled_dep) * 100,
      .groups = "drop"
    )

  cat("\nDecoupled Dependency rates:\n")
  print(kable(focused_dep, digits = 2, caption = sprintf("Decoupled Dependency Rates %s", settings$title_suffix)))

  # Proportion test (one-sided: group2 > group1)
  prop_test <- prop.test(
    x = c(
      focused_dep$n_decoupled_dep[focused_dep[[comparison_var]] == group2],
      focused_dep$n_decoupled_dep[focused_dep[[comparison_var]] == group1]
    ),
    n = c(
      focused_dep$n[focused_dep[[comparison_var]] == group2],
      focused_dep$n[focused_dep[[comparison_var]] == group1]
    ),
    alternative = "greater"
  )

  # Odds ratio
  a <- focused_dep$n_decoupled_dep[focused_dep[[comparison_var]] == group2]
  b <- focused_dep$n_other[focused_dep[[comparison_var]] == group2]
  c <- focused_dep$n_decoupled_dep[focused_dep[[comparison_var]] == group1]
  d <- focused_dep$n_other[focused_dep[[comparison_var]] == group1]

  odds_ratio <- (a * d) / (b * c)
  se_log_or <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  ci_lower <- exp(log(odds_ratio) - 1.96 * se_log_or)
  ci_upper <- exp(log(odds_ratio) + 1.96 * se_log_or)

  # Calculate risk metrics
  # Risk = probability of Decoupled Dependency in each group
  risk_group2 <- a / (a + b)  # Risk in group 2 (e.g., Relationship-Seeking)
  risk_group1 <- c / (c + d)  # Risk in group 1 (e.g., Non-Relationship-Seeking)

  # Absolute Risk Increase (ARI) = Risk_group2 - Risk_group1
  risk_diff <- risk_group2 - risk_group1

  # Number Needed to Harm (NNH)
  # Formula: NNH = 1 / ARI = 1 / (Risk_group2 - Risk_group1)
  # Interpretation: The number of participants who need to be exposed to
  # the "higher risk" condition (group2) for one additional case of
  # Decoupled Dependency to occur compared to the "lower risk" condition (group1).
  # Lower NNH = stronger harmful effect (fewer exposures needed for harm)
  # Only calculated when ARI > 0 (group2 has higher risk than group1)
  nnh <- ifelse(risk_diff > 0, 1 / risk_diff, NA)

  # Results summary
  results_summary <- tibble(
    Metric = c(
      sprintf("%s rate", settings$factor_labels[2]),
      sprintf("%s rate", settings$factor_labels[1]),
      "Absolute risk increase",
      "Odds Ratio", "95% CI Lower", "95% CI Upper", "One-sided p-value", "NNH"
    ),
    Value = c(
      sprintf("%.1f%%", risk_group2 * 100), sprintf("%.1f%%", risk_group1 * 100),
      sprintf("%.1f pp", risk_diff * 100), sprintf("%.2f", odds_ratio),
      sprintf("%.2f", ci_lower), sprintf("%.2f", ci_upper),
      sprintf("%.4f", prop_test$p.value), sprintf("%.0f", nnh)
    )
  )

  cat("\n--- Results Summary ---\n")
  print(kable(results_summary, caption = "Primary Test Results"))

  # NNH interpretation
  if (!is.na(nnh) && risk_diff > 0) {
    cat(sprintf(
      "\nNNH Interpretation: For every %.0f participants exposed to %s,\n",
      nnh, settings$factor_labels[2]
    ))
    cat(sprintf(
      "we would expect 1 additional case of Decoupled Dependency compared to %s.\n",
      settings$factor_labels[1]
    ))
  }

  # -------------------------------------------------------------------------
  # PLOTS
  # -------------------------------------------------------------------------

  # Category distribution plot
  plot_data <- focused_slopes %>%
    count(.data[[comparison_var]], behavior_category) %>%
    group_by(.data[[comparison_var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      group_label = factor(.data[[comparison_var]],
        levels = settings$factor_levels,
        labels = settings$factor_labels
      ),
      behavior_category = factor(behavior_category,
        levels = c(
          "Decoupled Dependency", "Aligned Engagement",
          "Aligned Disengagement", "Decoupled Satiation"
        ),
        labels = c("Decoupled\nDependency", "Aligned\nEngagement",
                   "Aligned\nDisengagement", "Decoupled\nSatiation")
      )
    )

  p_category_dist <- ggplot(plot_data, aes(
    x = behavior_category, y = pct,
    fill = group_label
  )) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 25, linetype = "dashed", color = "gray50", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
      position = position_dodge(width = 0.8),
      vjust = -0.5, size = 4.5
    ) +
    scale_fill_manual(values = setNames(
      settings$color_values[settings$factor_levels],
      settings$factor_labels
    )) +
    labs(
      title = "Category Distribution",
      subtitle = sprintf(
        "Decoupled Dep: %.1f%% vs %.1f%%, OR=%.2f, p=%.3f",
        risk_group2 * 100, risk_group1 * 100, odds_ratio, prop_test$p.value
      ),
      x = NULL,
      y = "% of Participants",
      fill = NULL
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 15, lineheight = 0.9),
      plot.title = element_text(face = "bold")
    ) +
    ylim(0, max(plot_data$pct) * 1.15)

  # Scatter plot of individual slopes
  p_scatter <- ggplot(focused_slopes, aes(
    x = wanting_slope, y = liking_slope_weekly,
    color = .data[[comparison_var]]
  )) +
    geom_point(alpha = 0.5, size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray30") +
    annotate("rect",
      xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
      fill = "red", alpha = 0.05
    ) +
    annotate("text",
      x = Inf, y = Inf, label = "Aligned\nEngagement",
      hjust = 1.1, vjust = 1.1, size = 6, color = "gray40"
    ) +
    annotate("text",
      x = -Inf, y = Inf, label = "Decoupled\nSatiation",
      hjust = -0.1, vjust = 1.1, size = 6, color = "gray40"
    ) +
    annotate("text",
      x = Inf, y = -Inf, label = "Decoupled\nDependency",
      hjust = 1.1, vjust = -0.1, size = 6, color = "darkred", fontface = "bold"
    ) +
    annotate("text",
      x = -Inf, y = -Inf, label = "Aligned\nDisengagement",
      hjust = -0.1, vjust = -0.1, size = 6, color = "gray40"
    ) +
    scale_color_manual(
      values = settings$color_values,
      labels = settings$color_labels
    ) +
    labs(
      title = "Liking vs Wanting",
      subtitle = "Red region = Decoupled Dependency",
      x = "Wanting Slope (per week)",
      y = "Liking Slope (per week)",
      color = NULL
    ) +
    theme_pub() +
    theme(legend.position = "bottom")

  # Mean slopes by comparison group
  slope_by_category <- focused_slopes %>%
    group_by(.data[[comparison_var]]) %>%
    summarise(
      n = n(),
      mean_liking = mean(liking_slope_weekly),
      ci_liking = 1.96 * sd(liking_slope_weekly) / sqrt(n()),
      mean_wanting = mean(wanting_slope),
      ci_wanting = 1.96 * sd(wanting_slope) / sqrt(n()),
      .groups = "drop"
    )

  slope_by_category_long <- slope_by_category %>%
    pivot_longer(
      cols = c(mean_liking, mean_wanting),
      names_to = "slope_type",
      values_to = "mean_slope"
    ) %>%
    mutate(
      ci = ifelse(slope_type == "mean_liking", ci_liking, ci_wanting),
      slope_type = ifelse(slope_type == "mean_liking", "Liking", "Wanting"),
      group_label = factor(.data[[comparison_var]],
        levels = settings$factor_levels,
        labels = settings$factor_labels
      )
    )

  p_slopes <- ggplot(slope_by_category_long, aes(
    x = group_label,
    y = mean_slope, fill = slope_type, pattern = slope_type
  )) +
    geom_col_pattern(
      position = position_dodge(width = 0.8), width = 0.7,
      pattern_fill = "gray30", pattern_colour = "gray30",
      pattern_density = 0.3, pattern_spacing = 0.03
    ) +
    geom_errorbar(aes(ymin = mean_slope - ci, ymax = mean_slope + ci),
      position = position_dodge(width = 0.8), width = 0.25
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Mean Slopes",
      subtitle = "Weekly scale; 95% CI",
      x = NULL,
      y = "Mean Slope (per week)",
      fill = "Slope Type",
      pattern = "Slope Type"
    ) +
    theme_pub() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("Liking" = "gray70", "Wanting" = "gray70")) +
    scale_pattern_manual(values = c("Liking" = "none", "Wanting" = "stripe"))

  # Trajectory plot (change from baseline over 4 weeks)
  trajectory_data <- focused_slopes %>%
    group_by(.data[[comparison_var]]) %>%
    summarise(
      n = n(),
      mean_liking_slope_weekly = mean(liking_slope_weekly),
      ci_liking = 1.96 * sd(liking_slope_weekly) / sqrt(n()),
      mean_wanting_slope = mean(wanting_slope),
      ci_wanting = 1.96 * sd(wanting_slope) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      liking_start = 0,
      liking_end = mean_liking_slope_weekly * 4,
      liking_end_ci = ci_liking * 4,
      wanting_start = 0,
      wanting_end = mean_wanting_slope * 4,
      wanting_end_ci = ci_wanting * 4,
      group_label = factor(.data[[comparison_var]],
        levels = settings$factor_levels,
        labels = settings$factor_labels
      )
    )

  trajectory_long <- trajectory_data %>%
    pivot_longer(
      cols = c(liking_start, liking_end, wanting_start, wanting_end),
      names_to = "measure_time",
      values_to = "value"
    ) %>%
    mutate(
      measure = ifelse(grepl("liking", measure_time), "Liking", "Wanting"),
      time = ifelse(grepl("start", measure_time), "Week 0", "Week 4"),
      time = factor(time, levels = c("Week 0", "Week 4")),
      ci = case_when(
        time == "Week 0" ~ 0,
        measure == "Liking" ~ liking_end_ci,
        measure == "Wanting" ~ wanting_end_ci
      )
    )

  p_trajectory <- ggplot(trajectory_long, aes(
    x = time, y = value,
    color = group_label,
    group = interaction(group_label, measure)
  )) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(aes(linetype = measure), linewidth = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = value - ci, ymax = value + ci), width = 0.1) +
    scale_color_manual(values = setNames(
      settings$color_values[settings$factor_levels],
      settings$factor_labels
    )) +
    scale_linetype_manual(values = c("Liking" = "solid", "Wanting" = "dashed")) +
    labs(
      title = "Trajectories",
      subtitle = "4-week change; 95% CI",
      x = NULL,
      y = "Change from Baseline",
      color = NULL,
      linetype = "Measure"
    ) +
    theme_pub() +
    theme(legend.position = "bottom") +
    guides(linetype = guide_legend(keywidth = unit(2, "cm")))

  # -------------------------------------------------------------------------
  # SENSITIVITY ANALYSES
  # -------------------------------------------------------------------------

  # Sensitivity 1: Exclude tiny slopes
  cat("\n--- Sensitivity 1: Excluding near-zero slopes ---\n")
  filtered_slopes <- combined_slopes %>%
    filter(abs(liking_slope) > 0.1 & abs(wanting_slope) > 0.1) %>%
    filter(.data[[comparison_var]] %in% settings$filter_values)

  filtered_dep <- filtered_slopes %>%
    mutate(is_decoupled_dep = behavior_category == "Decoupled Dependency") %>%
    group_by(.data[[comparison_var]]) %>%
    summarise(
      n = n(),
      n_decoupled_dep = sum(is_decoupled_dep),
      pct_decoupled_dep = mean(is_decoupled_dep) * 100,
      .groups = "drop"
    )

  sens1_test <- tryCatch({
    prop.test(
      x = c(
        filtered_dep$n_decoupled_dep[filtered_dep[[comparison_var]] == group2],
        filtered_dep$n_decoupled_dep[filtered_dep[[comparison_var]] == group1]
      ),
      n = c(
        filtered_dep$n[filtered_dep[[comparison_var]] == group2],
        filtered_dep$n[filtered_dep[[comparison_var]] == group1]
      ),
      alternative = "greater"
    )
  }, error = function(e) {
    cat("  Could not run sensitivity 1 test:", e$message, "\n")
    list(p.value = NA)
  })
  cat(sprintf("Sensitivity 1 p-value: %.4f\n", sens1_test$p.value))

  # Sensitivity 2: Strong decoupling
  cat("\n--- Sensitivity 2: Strong decoupling ---\n")
  strong_slopes <- focused_slopes %>%
    mutate(strong_decoupled_dep = liking_slope < -0.1 & wanting_slope > 0.1)

  strong_dep <- strong_slopes %>%
    group_by(.data[[comparison_var]]) %>%
    summarise(
      n = n(),
      n_strong_dep = sum(strong_decoupled_dep),
      pct_strong_dep = mean(strong_decoupled_dep) * 100,
      .groups = "drop"
    )

  sens2_test <- tryCatch({
    prop.test(
      x = c(
        strong_dep$n_strong_dep[strong_dep[[comparison_var]] == group2],
        strong_dep$n_strong_dep[strong_dep[[comparison_var]] == group1]
      ),
      n = c(
        strong_dep$n[strong_dep[[comparison_var]] == group2],
        strong_dep$n[strong_dep[[comparison_var]] == group1]
      ),
      alternative = "greater"
    )
  }, error = function(e) {
    cat("  Could not run sensitivity 2 test:", e$message, "\n")
    list(p.value = NA)
  })
  cat(sprintf("Sensitivity 2 p-value: %.4f\n", sens2_test$p.value))

  # -------------------------------------------------------------------------
  # CONTINUOUS ANALYSIS
  # -------------------------------------------------------------------------
  cat("\n--- Continuous: Decoupling Score ---\n")

  # NOTE: Use explicit vectors instead of formula interface to ensure correct direction.
  # Formula interface uses alphabetical factor ordering which doesn't match group1/group2.
  # All tests use group2 - group1 direction for consistent interpretation.

  liking_test <- t.test(
    x = focused_slopes$liking_slope_weekly[focused_slopes[[comparison_var]] == group2],
    y = focused_slopes$liking_slope_weekly[focused_slopes[[comparison_var]] == group1]
  )

  wanting_test <- t.test(
    x = focused_slopes$wanting_slope[focused_slopes[[comparison_var]] == group2],
    y = focused_slopes$wanting_slope[focused_slopes[[comparison_var]] == group1]
  )

  # Test: is group2's decoupling score > group1's? (one-sided)
  decoupling_test <- t.test(
    x = focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group2],
    y = focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group1],
    alternative = "greater"
  )

  mean_group2 <- mean(focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group2])
  mean_group1 <- mean(focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group1])
  sd_group2 <- sd(focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group2])
  sd_group1 <- sd(focused_slopes$decoupling_score[focused_slopes[[comparison_var]] == group1])
  n_group2 <- sum(focused_slopes[[comparison_var]] == group2)
  n_group1 <- sum(focused_slopes[[comparison_var]] == group1)

  pooled_sd <- sqrt(((n_group2 - 1) * sd_group2^2 + (n_group1 - 1) * sd_group1^2) / (n_group2 + n_group1 - 2))
  cohens_d <- (mean_group2 - mean_group1) / pooled_sd

  cat(sprintf(
    "Decoupling score: %s M = %.3f, %s M = %.3f\n",
    settings$factor_labels[2], mean_group2,
    settings$factor_labels[1], mean_group1
  ))
  cat(sprintf(
    "t = %.2f, p = %.4f, Cohen's d = %.2f\n",
    decoupling_test$statistic, decoupling_test$p.value, cohens_d
  ))

  # Stats summary table
  stats_summary <- tibble(
    Test = c("Liking slope", "Wanting slope", "Decoupling score"),
    Group2_M = c(
      mean(focused_slopes$liking_slope_weekly[focused_slopes[[comparison_var]] == group2]),
      mean(focused_slopes$wanting_slope[focused_slopes[[comparison_var]] == group2]),
      mean_group2
    ),
    Group1_M = c(
      mean(focused_slopes$liking_slope_weekly[focused_slopes[[comparison_var]] == group1]),
      mean(focused_slopes$wanting_slope[focused_slopes[[comparison_var]] == group1]),
      mean_group1
    ),
    t = c(liking_test$statistic, wanting_test$statistic, decoupling_test$statistic),
    p = c(liking_test$p.value, wanting_test$p.value, decoupling_test$p.value)
  )
  names(stats_summary)[2:3] <- c(paste0(settings$factor_labels[2], " M"), paste0(settings$factor_labels[1], " M"))

  print(kable(stats_summary, digits = 3, caption = "Slope Magnitude Statistical Tests"))

  return(list(
    prop_test = prop_test,
    chisq_test = focused_chisq,
    odds_ratio = odds_ratio,
    ci = c(ci_lower, ci_upper),
    risk_diff = risk_diff,
    nnh = nnh,
    sens1_test = sens1_test,
    sens2_test = sens2_test,
    decoupling_test = decoupling_test,
    cohens_d = cohens_d,
    p_category_dist = p_category_dist,
    p_scatter = p_scatter,
    p_slopes = p_slopes,
    p_trajectory = p_trajectory,
    settings = settings,
    contingency = focused_contingency,
    dep_rates = focused_dep
  ))
}

# =============================================================================
# SECTION 5: PLOTTING FUNCTIONS
# =============================================================================

#' Create distribution plots by lambda value
#'
#' @param combined_slopes Data frame with behavioral categories
#' @return List with distribution and slope plots
create_lambda_distribution_plots <- function(combined_slopes) {
  # Behavioral category distribution by lambda
  plot_data_lambda <- combined_slopes %>%
    count(lambda, behavior_category) %>%
    group_by(lambda) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      behavior_category = factor(behavior_category,
        levels = c(
          "Decoupled Dependency", "Aligned Engagement",
          "Aligned Disengagement", "Decoupled Satiation"
        ),
        labels = c("Decoupled\nDependency", "Aligned\nEngagement", "Aligned\nDisengagement", "Decoupled\nSatiation")
      ),
      lambda_factor = factor(lambda)
    )

  p_lambda_dist <- ggplot(plot_data_lambda, aes(
    x = behavior_category, y = pct,
    fill = lambda_factor
  )) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 25, linetype = "dashed", color = "gray50", alpha = 0.7) +
    scale_fill_manual(values = lambda_colors) +
    labs(
      title = "Category Distribution by Lambda",
      subtitle = "Sep. Distress sample; Dashed = 25% chance",
      x = NULL,
      y = "% of Participants",
      fill = "Lambda"
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 15, lineheight = 0.9),
      plot.title = element_text(face = "bold")
    )

  # Mean slopes by lambda
  slope_by_lambda <- combined_slopes %>%
    group_by(lambda) %>%
    summarise(
      n = n(),
      mean_liking = mean(liking_slope_weekly),
      ci_liking = 1.96 * sd(liking_slope_weekly) / sqrt(n()),
      mean_wanting = mean(wanting_slope),
      ci_wanting = 1.96 * sd(wanting_slope) / sqrt(n()),
      .groups = "drop"
    )

  slope_by_lambda_long <- slope_by_lambda %>%
    pivot_longer(
      cols = c(mean_liking, mean_wanting),
      names_to = "slope_type",
      values_to = "mean_slope"
    ) %>%
    mutate(
      ci = ifelse(slope_type == "mean_liking", ci_liking, ci_wanting),
      slope_type = ifelse(slope_type == "mean_liking", "Liking", "Wanting"),
      lambda_factor = factor(lambda)
    )

  p_lambda_slopes <- ggplot(slope_by_lambda_long, aes(
    x = lambda_factor, y = mean_slope,
    fill = slope_type, pattern = slope_type
  )) +
    geom_col_pattern(
      position = position_dodge(width = 0.8), width = 0.7,
      pattern_fill = "gray30", pattern_colour = "gray30",
      pattern_density = 0.3, pattern_spacing = 0.03
    ) +
    geom_errorbar(aes(ymin = mean_slope - ci, ymax = mean_slope + ci),
      position = position_dodge(width = 0.8), width = 0.25
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Mean Slopes by Lambda",
      subtitle = "Sep. Distress sample; Weekly scale; 95% CI",
      x = "Lambda",
      y = "Mean Slope (per week)",
      fill = "Slope Type",
      pattern = "Slope Type"
    ) +
    theme_pub() +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = c("Liking" = "gray70", "Wanting" = "gray70")) +
    scale_pattern_manual(values = c("Liking" = "none", "Wanting" = "stripe"))

  return(list(
    p_lambda_dist = p_lambda_dist,
    p_lambda_slopes = p_lambda_slopes,
    slope_by_lambda = slope_by_lambda
  ))
}

#' Create summary table for one wanting measure
#'
#' @param results_list List of comparison results
#' @param wanting_label Label for the wanting measure
#' @return Summary data frame
create_summary_table <- function(results_list, wanting_label) {
  summary_rows <- lapply(names(results_list), function(name) {
    r <- results_list[[name]]
    tibble(
      Comparison = r$settings$title_suffix,
      `Wanting Measure` = wanting_label,
      `Primary p` = r$prop_test$p.value,
      `OR` = r$odds_ratio,
      `Sens1 p` = r$sens1_test$p.value,
      `Sens2 p` = r$sens2_test$p.value,
      `Continuous p` = r$decoupling_test$p.value,
      Cohens_d = r$cohens_d
    )
  })

  bind_rows(summary_rows)
}

# =============================================================================
# SECTION 6: MAIN ANALYSIS
# =============================================================================

cat("\n################################################################\n")
cat("STEP 1: Fitting models and creating behavioral categories\n")
cat("################################################################\n")

cat("\nFitting liking model...\n")
liking_results <- fit_liking_slopes(liking_data)

cat("\nFitting wanting model (Separation Distress)...\n")
wanting_sep_results <- fit_wanting_slopes(separation_distress_data)

cat("\nFitting wanting model (Seeking Companionship)...\n")
wanting_seeking_results <- fit_wanting_slopes(seeking_companionship_data)

# Create behavioral categories
combined_slopes_sep <- create_behavioral_categories(
  liking_results$slopes,
  wanting_sep_results$slopes
)

combined_slopes_seeking <- create_behavioral_categories(
  liking_results$slopes,
  wanting_seeking_results$slopes
)

cat(sprintf("\nParticipants with both slopes (Sep Distress): %d\n", nrow(combined_slopes_sep)))
cat(sprintf("Participants with both slopes (Seeking): %d\n", nrow(combined_slopes_seeking)))

# Compute overall category distribution (for reporting)
overall_category_stats_sep <- combined_slopes_sep %>%
  count(behavior_category) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

overall_category_stats_seeking <- combined_slopes_seeking %>%
  count(behavior_category) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

cat("\nBehavioral Category Distribution (Sep Distress):\n")
print(kable(overall_category_stats_sep, digits = 1))

cat("\nBehavioral Category Distribution (Seeking Companionship):\n")
print(kable(overall_category_stats_seeking, digits = 1))

# Standardize decoupling score for seeking_companionship (robustness analysis)
# (Seeking has large variance ratio ~67x, so raw subtraction is dominated by wanting)
# We z-score both slopes to put them on comparable scales before subtraction.
combined_slopes_seeking <- combined_slopes_seeking %>%
  mutate(
    liking_slope_z = as.numeric(scale(liking_slope_weekly)),
    wanting_slope_z = as.numeric(scale(wanting_slope)),
    decoupling_score_raw = decoupling_score,  # Keep raw for reference
    decoupling_score = wanting_slope_z - liking_slope_z  # Replace with z-scored
  )
cat("NOTE: Seeking Companionship uses z-scored decoupling_score (robustness).\n")

# -----------------------------------------------------------------------------
# Slope Scale Comparison (validates decoupling_score calculation)
# -----------------------------------------------------------------------------
# NOTE: decoupling_score = wanting_slope - liking_slope_weekly
# This subtraction assumes comparable scales. Both are 0-100 VAS but measure
# different constructs. If variances differ substantially, one slope dominates
# the decoupling score. We report statistics here for transparency.

cat("\n--- Slope Scale Comparison ---\n")
cat("(Validates decoupling_score = wanting_slope - liking_slope_weekly)\n\n")

# Separation Distress wanting
liking_stats_sep <- combined_slopes_sep %>%
  summarise(
    mean = mean(liking_slope_weekly, na.rm = TRUE),
    sd = sd(liking_slope_weekly, na.rm = TRUE),
    var = var(liking_slope_weekly, na.rm = TRUE),
    min = min(liking_slope_weekly, na.rm = TRUE),
    max = max(liking_slope_weekly, na.rm = TRUE)
  )

wanting_stats_sep <- combined_slopes_sep %>%
  summarise(
    mean = mean(wanting_slope, na.rm = TRUE),
    sd = sd(wanting_slope, na.rm = TRUE),
    var = var(wanting_slope, na.rm = TRUE),
    min = min(wanting_slope, na.rm = TRUE),
    max = max(wanting_slope, na.rm = TRUE)
  )

# Variance ratio (>2 or <0.5 suggests problematic scale mismatch)
var_ratio_sep <- wanting_stats_sep$var / liking_stats_sep$var

cat("Separation Distress Analysis:\n")
cat(sprintf("  Liking (weekly):  Mean=%.2f, SD=%.2f, Var=%.2f, Range=[%.2f, %.2f]\n",
            liking_stats_sep$mean, liking_stats_sep$sd, liking_stats_sep$var,
            liking_stats_sep$min, liking_stats_sep$max))
cat(sprintf("  Wanting:          Mean=%.2f, SD=%.2f, Var=%.2f, Range=[%.2f, %.2f]\n",
            wanting_stats_sep$mean, wanting_stats_sep$sd, wanting_stats_sep$var,
            wanting_stats_sep$min, wanting_stats_sep$max))
cat(sprintf("  Variance ratio (wanting/liking): %.2f\n", var_ratio_sep))
if (var_ratio_sep > 2 || var_ratio_sep < 0.5) {
  cat("  WARNING: Variance ratio suggests scale mismatch. Consider standardizing.\n")
} else {
  cat("  Variance ratio acceptable (within 0.5-2.0 range).\n")
}

# Seeking Companionship wanting
liking_stats_seek <- combined_slopes_seeking %>%
  summarise(
    mean = mean(liking_slope_weekly, na.rm = TRUE),
    sd = sd(liking_slope_weekly, na.rm = TRUE),
    var = var(liking_slope_weekly, na.rm = TRUE),
    min = min(liking_slope_weekly, na.rm = TRUE),
    max = max(liking_slope_weekly, na.rm = TRUE)
  )

wanting_stats_seek <- combined_slopes_seeking %>%
  summarise(
    mean = mean(wanting_slope, na.rm = TRUE),
    sd = sd(wanting_slope, na.rm = TRUE),
    var = var(wanting_slope, na.rm = TRUE),
    min = min(wanting_slope, na.rm = TRUE),
    max = max(wanting_slope, na.rm = TRUE)
  )

var_ratio_seek <- wanting_stats_seek$var / liking_stats_seek$var

cat("\nSeeking Companionship Analysis:\n")
cat(sprintf("  Liking (weekly):  Mean=%.2f, SD=%.2f, Var=%.2f, Range=[%.2f, %.2f]\n",
            liking_stats_seek$mean, liking_stats_seek$sd, liking_stats_seek$var,
            liking_stats_seek$min, liking_stats_seek$max))
cat(sprintf("  Wanting:          Mean=%.2f, SD=%.2f, Var=%.2f, Range=[%.2f, %.2f]\n",
            wanting_stats_seek$mean, wanting_stats_seek$sd, wanting_stats_seek$var,
            wanting_stats_seek$min, wanting_stats_seek$max))
cat(sprintf("  Variance ratio (wanting/liking): %.2f\n", var_ratio_seek))
if (var_ratio_seek > 2 || var_ratio_seek < 0.5) {
  cat("  WARNING: Variance ratio suggests scale mismatch. Consider standardizing.\n")
} else {
  cat("  Variance ratio acceptable (within 0.5-2.0 range).\n")
}

# Store for report
slope_scale_stats <- list(
  sep_distress = list(
    liking = liking_stats_sep,
    wanting = wanting_stats_sep,
    var_ratio = var_ratio_sep
  ),
  seeking = list(
    liking = liking_stats_seek,
    wanting = wanting_stats_seek,
    var_ratio = var_ratio_seek
  )
)

cat("\n")

# =============================================================================
# STEP 2: Behavioral category distribution and validation
# =============================================================================

cat("\n################################################################\n")
cat("STEP 2: Behavioral Category Distribution & Validation\n")
cat("################################################################\n")

cat("\nBehavioral category distribution (Separation Distress):\n")
print(kable(as.data.frame(table(combined_slopes_sep$behavior_category)),
  col.names = c("Category", "Count"),
  caption = "Behavioral Category Distribution"
))

# Validation check
validation_sep <- validation_check(
  combined_slopes_sep, attachment_raw,
  seeking_companionship_raw_all, goodbye_raw
)

# Lambda distribution plots
cat("\nCreating lambda distribution plots...\n")
lambda_plots <- create_lambda_distribution_plots(combined_slopes_sep)
cat("\nMean slopes by lambda:\n")
print(kable(lambda_plots$slope_by_lambda, digits = 3, caption = "Mean Slopes by Lambda"))

# Save lambda plots
p_lambda_combined <- grid.arrange(
  lambda_plots$p_lambda_dist,
  lambda_plots$p_lambda_slopes,
  ncol = 2
)
save_plot(FIGURE_DIR, p_lambda_combined, "decoupling_lambda_distribution", 14, 6)

# =============================================================================
# STEP 3: Run comparisons for each variable
# =============================================================================

cat("\n################################################################\n")
cat("STEP 3: Running Comparisons\n")
cat("################################################################\n")

comparison_vars <- c("relationship_seeking_category", "domain", "companionship_condition")

# Separation Distress results
results_sep <- list()
for (cv in comparison_vars) {
  cat(sprintf("\n\n=== SEPARATION DISTRESS: %s ===\n", cv))
  results_sep[[cv]] <- run_comparison_analysis(combined_slopes_sep, cv)
}

# Seeking Companionship results
results_seeking <- list()
for (cv in comparison_vars) {
  cat(sprintf("\n\n=== SEEKING COMPANIONSHIP: %s ===\n", cv))
  results_seeking[[cv]] <- run_comparison_analysis(combined_slopes_seeking, cv)
}

# -----------------------------------------------------------------------------
# ROBUSTNESS: Raw Decoupling Score (Seeking Companionship)
# -----------------------------------------------------------------------------
# The primary seeking_companionship analysis uses z-scored slopes because the
# variance ratio is ~67x. As a robustness check, we show the raw score results.

cat("\n\n################################################################\n")
cat("ROBUSTNESS: Raw Decoupling Score (Seeking Companionship)\n")
cat("################################################################\n")
cat("\nNOTE: Primary analysis uses z-scored slopes (variance ratio ~67x).\n")
cat("Raw scores shown here for comparison.\n\n")

robustness_seeking_raw <- list()
for (cv in comparison_vars) {
  settings <- get_comparison_settings(cv)
  group1 <- settings$filter_values[1]
  group2 <- settings$filter_values[2]

  focused <- combined_slopes_seeking %>%
    filter(.data[[cv]] %in% settings$filter_values) %>%
    droplevels()

  # T-test on raw decoupling score
  raw_test <- t.test(
    x = focused$decoupling_score_raw[focused[[cv]] == group2],
    y = focused$decoupling_score_raw[focused[[cv]] == group1],
    alternative = "greater"
  )

  mean_g2 <- mean(focused$decoupling_score_raw[focused[[cv]] == group2])
  mean_g1 <- mean(focused$decoupling_score_raw[focused[[cv]] == group1])
  sd_g2 <- sd(focused$decoupling_score_raw[focused[[cv]] == group2])
  sd_g1 <- sd(focused$decoupling_score_raw[focused[[cv]] == group1])
  n_g2 <- sum(focused[[cv]] == group2)
  n_g1 <- sum(focused[[cv]] == group1)
  pooled_sd <- sqrt(((n_g2 - 1) * sd_g2^2 + (n_g1 - 1) * sd_g1^2) /
                      (n_g2 + n_g1 - 2))
  cohens_d_raw <- (mean_g2 - mean_g1) / pooled_sd

  cat(sprintf("\n--- %s (Raw) ---\n", settings$title_suffix))
  cat(sprintf("  %s: M = %.3f (SD = %.3f)\n",
              settings$factor_labels[2], mean_g2, sd_g2))
  cat(sprintf("  %s: M = %.3f (SD = %.3f)\n",
              settings$factor_labels[1], mean_g1, sd_g1))
  cat(sprintf("  t = %.2f, p = %.4f, Cohen's d = %.2f\n",
              raw_test$statistic, raw_test$p.value, cohens_d_raw))

  robustness_seeking_raw[[cv]] <- list(
    comparison = cv,
    group2_mean = mean_g2,
    group1_mean = mean_g1,
    t_stat = raw_test$statistic,
    p_value = raw_test$p.value,
    cohens_d = cohens_d_raw
  )
}

# =============================================================================
# SECTION 7: OUTPUT - SAVE PLOTS AND STATISTICS
# =============================================================================

cat("\n################################################################\n")
cat("SAVING OUTPUTS\n")
cat("################################################################\n")

# Save plots for each comparison - one 4-panel figure per wanting measure
for (cv in comparison_vars) {
  cv_short <- gsub("_", "", cv)

  # Separation Distress: 4-panel figure (2x2)
  p_sep_combined <- grid.arrange(
    results_sep[[cv]]$p_category_dist,
    results_sep[[cv]]$p_scatter,
    results_sep[[cv]]$p_slopes,
    results_sep[[cv]]$p_trajectory,
    ncol = 2, nrow = 2
  )
  save_plot(FIGURE_DIR, p_sep_combined,
    sprintf("decoupling_sep_%s", cv_short), 14, 12)

  # Seeking Companionship: 4-panel figure (2x2)
  p_seek_combined <- grid.arrange(
    results_seeking[[cv]]$p_category_dist,
    results_seeking[[cv]]$p_scatter,
    results_seeking[[cv]]$p_slopes,
    results_seeking[[cv]]$p_trajectory,
    ncol = 2, nrow = 2
  )
  save_plot(FIGURE_DIR, p_seek_combined,
    sprintf("decoupling_seek_%s", cv_short), 14, 12)
}

# Create summary tables
cat("\n\n################################################################\n")
cat("SUMMARY TABLES\n")
cat("################################################################\n")

summary_sep <- create_summary_table(results_sep, "Separation Distress")
summary_seeking <- create_summary_table(results_seeking, "Seeking Companionship")

summary_all <- bind_rows(summary_sep, summary_seeking) %>%
  mutate(
    across(where(is.numeric), ~ round(., 4)),
    `Primary Sig` = ifelse(`Primary p` < 0.05, "Yes", "No"),
    `Continuous Sig` = ifelse(`Continuous p` < 0.05, "Yes", "No")
  )

print(kable(summary_all, digits = 4, caption = "Summary of All Analyses"))

# Generate LaTeX summary table
if (generate_tex_tables) {
  create_decoupling_summary_latex(
    summary_df = summary_all,
    table_dir = TABLE_DIR,
    filename = "decoupling_summary",
    caption = "Decoupling Analysis: Treatment Effects on Dependency Formation"
  )
}

# =============================================================================
# STEP 4: PREDICTIVE ANALYSIS
# Separation Distress -> Seeking Companionship Likelihood
# =============================================================================

cat("\n################################################################\n")
cat("STEP 4: PREDICTIVE ANALYSIS\n")
cat("Separation Distress predicting Seeking Companionship\n")
cat("################################################################\n")

# Prepare data for predictive analysis
# Get final week data for both measures
predictive_data <- attachment_raw %>%
  filter(study_id == "longitudinal", !is.na(separation_distress)) %>%
  group_by(ppt_id) %>%
  filter(week_numeric == max(week_numeric)) %>%
  summarise(
    separation_distress = mean(separation_distress, na.rm = TRUE),
    lambda = first(lambda),
    domain = first(domain),
    relationship_seeking_category = first(relationship_seeking_category),
    .groups = "drop"
  ) %>%
  left_join(
    seeking_companionship_raw_all %>%
      filter(study_id == "longitudinal", !is.na(seeking_companionship_likelihood)) %>%
      group_by(ppt_id) %>%
      filter(week_numeric == max(week_numeric)) %>%
      summarise(
        seeking_companionship_likelihood = mean(seeking_companionship_likelihood, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "ppt_id"
  ) %>%
  filter(!is.na(seeking_companionship_likelihood)) %>%
  mutate(
    # Create intervention group label for comparison with original analysis
    intervention_group = ifelse(
      relationship_seeking_category == "pos_lambda",
      "Seeking",
      ifelse(relationship_seeking_category == "neg_lambda", "Avoiding", "Neutral")
    ),
    intervention_group = factor(intervention_group, levels = c("Avoiding", "Neutral", "Seeking"))
  )

cat(sprintf("\nPredictive analysis sample: %d participants\n", nrow(predictive_data)))

# Main model: separation_distress * relationship_seeking_category
cat("\n--- Model 1: Full interaction model ---\n\n")

model_companionship <- lm(
  seeking_companionship_likelihood ~ separation_distress * relationship_seeking_category,
  data = predictive_data
)
print(summary(model_companionship))

# Alternative: Using intervention_group coding (for comparison with original analysis)
cat("\n--- Model 2: Seeking vs Avoiding (excluding neutral) ---\n\n")

predictive_data_binary <- predictive_data %>%
  filter(intervention_group %in% c("Seeking", "Avoiding"))

model_companionship_binary <- lm(
  seeking_companionship_likelihood ~ separation_distress * intervention_group,
  data = predictive_data_binary
)
print(summary(model_companionship_binary))

# Separate models by group for clarity
cat("\n--- Separate slopes by group ---\n")

model_seeking <- lm(
  seeking_companionship_likelihood ~ separation_distress,
  data = predictive_data %>% filter(relationship_seeking_category == "pos_lambda")
)

model_avoiding <- lm(
  seeking_companionship_likelihood ~ separation_distress,
  data = predictive_data %>% filter(relationship_seeking_category == "neg_lambda")
)

cat("\nRelationship-Seeking (pos_lambda):\n")
cat(sprintf("  b = %.4f\n", coef(model_seeking)[2]))
cat(sprintf("  p = %.4f\n", summary(model_seeking)$coefficients[2, 4]))

cat("\nRelationship-Avoiding (neg_lambda):\n")
cat(sprintf("  b = %.4f\n", coef(model_avoiding)[2]))
cat(sprintf("  p = %.4f\n", summary(model_avoiding)$coefficients[2, 4]))

# Correlation
cat("\n--- Correlation ---\n")
cor_test <- cor.test(
  predictive_data$seeking_companionship_likelihood,
  predictive_data$separation_distress,
  method = "pearson"
)
cat(sprintf("r = %.3f, p = %.4e\n", cor_test$estimate, cor_test$p.value))

# Create visualization
cat("\n--- Creating predictive analysis plot ---\n")

p_predictive <- ggplot(
  predictive_data %>% filter(relationship_seeking_category != "zero_lambda"),
  aes(
    x = separation_distress,
    y = seeking_companionship_likelihood,
    color = relationship_seeking_category
  )
) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(
    values = c("neg_lambda" = "#2166AC", "pos_lambda" = "#B2182B"),
    labels = c("neg_lambda" = "Non-RS (lambda<0)", "pos_lambda" = "RS (lambda>0)")
  ) +
  labs(
    title = "Sep. Distress -> Companionship-Seeking",
    subtitle = sprintf(
      "b=%.2f; Interaction b=%.2f, p=%.3f",
      coef(model_companionship_binary)["separation_distress"],
      coef(model_companionship_binary)["separation_distress:intervention_groupSeeking"],
      summary(model_companionship_binary)$coefficients["separation_distress:intervention_groupSeeking", 4]
    ),
    x = "Separation Distress (Wk 4)",
    y = "Seeking Companionship (Wk 4)",
    color = NULL
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

save_plot(FIGURE_DIR, p_predictive, "decoupling_predictive_sep_seeking", 10, 7)

# Store results for JSON output
predictive_results <- list(
  full_model = list(
    sep_distress_beta = coef(model_companionship)["separation_distress"],
    sep_distress_p = summary(model_companionship)$coefficients["separation_distress", 4],
    r_squared = summary(model_companionship)$r.squared
  ),
  binary_model = list(
    sep_distress_beta = coef(model_companionship_binary)["separation_distress"],
    sep_distress_p = summary(model_companionship_binary)$coefficients["separation_distress", 4],
    interaction_beta = coef(model_companionship_binary)["separation_distress:intervention_groupSeeking"],
    interaction_p = summary(model_companionship_binary)$coefficients["separation_distress:intervention_groupSeeking", 4],
    r_squared = summary(model_companionship_binary)$r.squared
  ),
  by_group = list(
    seeking_beta = coef(model_seeking)[2],
    seeking_p = summary(model_seeking)$coefficients[2, 4],
    avoiding_beta = coef(model_avoiding)[2],
    avoiding_p = summary(model_avoiding)$coefficients[2, 4]
  ),
  correlation = list(
    r = cor_test$estimate,
    p = cor_test$p.value
  ),
  n = nrow(predictive_data)
)

# =============================================================================
# STEP 5: GOODBYE ACTION ANALYSIS
# Separation Distress predicting Goodbye Action (binary outcome)
# =============================================================================

cat("\n################################################################\n")
cat("STEP 5: GOODBYE ACTION ANALYSIS\n")
cat("Separation Distress predicting Goodbye Action\n")
cat("################################################################\n")

# Prepare goodbye data - merge with separation distress
goodbye_analysis_data <- goodbye_raw %>%
  filter(study_id == "longitudinal", !is.na(goodbye_action)) %>%
  group_by(ppt_id) %>%
  # Get final session goodbye action

  filter(session_numeric == max(session_numeric)) %>%
  summarise(
    goodbye_action = first(goodbye_action),
    lambda = first(lambda),
    domain = first(domain),
    relationship_seeking_category = first(relationship_seeking_category),
    .groups = "drop"
  ) %>%
  left_join(
    attachment_raw %>%
      filter(study_id == "longitudinal", !is.na(separation_distress)) %>%
      group_by(ppt_id) %>%
      filter(week_numeric == max(week_numeric)) %>%
      summarise(
        separation_distress = mean(separation_distress, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "ppt_id"
  ) %>%
  filter(!is.na(separation_distress)) %>%
  mutate(
    intervention_group = ifelse(
      relationship_seeking_category == "pos_lambda",
      "Seeking",
      ifelse(relationship_seeking_category == "neg_lambda", "Avoiding", "Neutral")
    ),
    intervention_group = factor(intervention_group, levels = c("Avoiding", "Neutral", "Seeking"))
  )

cat(sprintf("\nGoodbye analysis sample: %d participants\n", nrow(goodbye_analysis_data)))
cat(sprintf("  Goodbye action = 1: %d (%.1f%%)\n",
  sum(goodbye_analysis_data$goodbye_action == 1),
  mean(goodbye_analysis_data$goodbye_action == 1) * 100))

# -----------------------------------------------------------------------------
# T-test: Separation distress by goodbye action
# -----------------------------------------------------------------------------

cat("\n--- T-test: Separation Distress by Goodbye Action ---\n")

goodbye_means <- goodbye_analysis_data %>%
  group_by(goodbye_action) %>%
  summarise(
    mean_sep_distress = mean(separation_distress, na.rm = TRUE),
    sd_sep_distress = sd(separation_distress, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(kable(goodbye_means, digits = 2, caption = "Separation Distress by Goodbye Action"))

t_test_goodbye <- t.test(separation_distress ~ goodbye_action,
  data = goodbye_analysis_data
)
print(t_test_goodbye)

# Cohen's d
goodbye_yes <- goodbye_analysis_data %>%
  filter(goodbye_action == 1) %>%
  pull(separation_distress)
goodbye_no <- goodbye_analysis_data %>%
  filter(goodbye_action == 0) %>%
  pull(separation_distress)

cohens_d_goodbye <- (mean(goodbye_yes, na.rm = TRUE) - mean(goodbye_no, na.rm = TRUE)) /
  sqrt((var(goodbye_yes, na.rm = TRUE) + var(goodbye_no, na.rm = TRUE)) / 2)

cat(sprintf("Cohen's d: %.3f\n", cohens_d_goodbye))

# Correlation
cor_goodbye <- cor.test(
  goodbye_analysis_data$goodbye_action,
  goodbye_analysis_data$separation_distress,
  method = "pearson"
)
cat(sprintf("Correlation (point-biserial): r = %.3f, p = %.4e\n",
  cor_goodbye$estimate, cor_goodbye$p.value))

# -----------------------------------------------------------------------------
# Logistic regression: Goodbye Action ~ Separation Distress * Intervention
# -----------------------------------------------------------------------------

cat("\n--- Logistic Regression: Goodbye Action ---\n\n")

# Filter to binary comparison (excluding neutral)
goodbye_data_binary <- goodbye_analysis_data %>%
  filter(intervention_group %in% c("Seeking", "Avoiding"))

cat(sprintf("Binary comparison sample: %d participants\n", nrow(goodbye_data_binary)))

model_goodbye <- glm(
  goodbye_action ~ separation_distress * intervention_group,
  data = goodbye_data_binary,
  family = binomial(link = "logit")
)
print(summary(model_goodbye))

# Odds ratios with CIs (using parameters for consistency with other scripts)
cat("\nOdds Ratios with 95% CI:\n")
goodbye_params <- parameters::model_parameters(model_goodbye, exponentiate = TRUE)
print(goodbye_params)

# Interaction test (likelihood ratio)
cat("\nInteraction Test (Type II ANOVA):\n")
print(anova(model_goodbye, test = "Chisq"))

# Separate models by group
model_goodbye_seeking <- glm(
  goodbye_action ~ separation_distress,
  data = goodbye_data_binary %>% filter(intervention_group == "Seeking"),
  family = binomial
)

model_goodbye_avoiding <- glm(
  goodbye_action ~ separation_distress,
  data = goodbye_data_binary %>% filter(intervention_group == "Avoiding"),
  family = binomial
)

cat("\nRelationship-Seeking:\n")
cat(sprintf("  b = %.4f\n", coef(model_goodbye_seeking)[2]))
cat(sprintf("  OR = %.3f\n", exp(coef(model_goodbye_seeking)[2])))
cat(sprintf("  p = %.4f\n", summary(model_goodbye_seeking)$coefficients[2, 4]))

cat("\nRelationship-Avoiding:\n")
cat(sprintf("  b = %.4f\n", coef(model_goodbye_avoiding)[2]))
cat(sprintf("  OR = %.3f\n", exp(coef(model_goodbye_avoiding)[2])))
cat(sprintf("  p = %.4f\n", summary(model_goodbye_avoiding)$coefficients[2, 4]))

# -----------------------------------------------------------------------------
# Predicted probabilities at different separation distress levels
# -----------------------------------------------------------------------------

cat("\n--- Predicted Probabilities ---\n")

sep_dist_levels <- c(25, 50, 75)  # Low, medium, high

pred_data_goodbye <- expand.grid(
  separation_distress = sep_dist_levels,
  intervention_group = c("Seeking", "Avoiding")
)
pred_data_goodbye$intervention_group <- factor(
  pred_data_goodbye$intervention_group,
  levels = c("Avoiding", "Neutral", "Seeking")
)

pred_data_goodbye$predicted_prob <- predict(
  model_goodbye,
  newdata = pred_data_goodbye,
  type = "response"
)

pred_table_goodbye <- pred_data_goodbye %>%
  pivot_wider(
    names_from = intervention_group,
    values_from = predicted_prob,
    names_prefix = "Prob_"
  )

cat("\nPredicted probability of saying goodbye:\n")
print(kable(pred_table_goodbye, digits = 3))

# -----------------------------------------------------------------------------
# Combined visualization
# -----------------------------------------------------------------------------

cat("\n--- Creating combined predictive plots ---\n")

# Plot 1: Separation Distress -> Goodbye Action (logistic)
p_goodbye <- ggplot(
  goodbye_data_binary,
  aes(
    x = separation_distress,
    y = goodbye_action,
    color = intervention_group
  )
) +
  geom_point(alpha = 0.2, position = position_jitter(height = 0.05)) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    se = TRUE,
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = c("Avoiding" = "#2166AC", "Seeking" = "#B2182B"),
    labels = c("Avoiding" = "Non-RS (lambda<0)", "Seeking" = "RS (lambda>0)")
  ) +
  labs(
    title = "Sep. Distress -> Goodbye",
    subtitle = sprintf(
      "Interaction p=%.3f",
      summary(model_goodbye)$coefficients["separation_distress:intervention_groupSeeking", 4]
    ),
    x = "Separation Distress",
    y = "P(Goodbye)",
    color = NULL
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# Note: Seeking companionship predictive plot already saved as p_predictive above

save_plot(FIGURE_DIR, p_goodbye, "decoupling_predictive_goodbye", 10, 7)

# -----------------------------------------------------------------------------
# Summary table comparing both outcomes
# -----------------------------------------------------------------------------

cat("\n--- Summary Table: Both Outcomes ---\n\n")

# Get interaction p-values
interaction_p_goodbye <- tryCatch(
  summary(model_goodbye)$coefficients["separation_distress:intervention_groupSeeking", 4],
  error = function(e) NA
)
interaction_p_comp <- tryCatch(
  summary(model_companionship_binary)$coefficients["separation_distress:intervention_groupSeeking", 4],
  error = function(e) NA
)

predictive_summary_table <- tibble(
  Outcome = c("Goodbye Action", "Seeking Companionship"),
  `Effect in RS` = c(
    sprintf("OR=%.2f, p=%.4f",
      exp(coef(model_goodbye_seeking)[2]),
      summary(model_goodbye_seeking)$coefficients[2, 4]),
    sprintf("b=%.3f, p=%.4f",
      coef(model_seeking)[2],
      summary(model_seeking)$coefficients[2, 4])
  ),
  `Effect in Non-RS` = c(
    sprintf("OR=%.2f, p=%.4f",
      exp(coef(model_goodbye_avoiding)[2]),
      summary(model_goodbye_avoiding)$coefficients[2, 4]),
    sprintf("b=%.3f, p=%.4f",
      coef(model_avoiding)[2],
      summary(model_avoiding)$coefficients[2, 4])
  ),
  `Interaction p` = c(
    round(interaction_p_goodbye, 4),
    round(interaction_p_comp, 4)
  ),
  `Sig Interaction?` = c(
    ifelse(!is.na(interaction_p_goodbye) && interaction_p_goodbye < 0.05, "YES", "NO"),
    ifelse(!is.na(interaction_p_comp) && interaction_p_comp < 0.05, "YES", "NO")
  )
)

print(kable(predictive_summary_table, caption = "Separation Distress as Predictor - Summary"))

# Store goodbye results
goodbye_results <- list(
  t_test = list(
    statistic = t_test_goodbye$statistic,
    p_value = t_test_goodbye$p.value,
    cohens_d = cohens_d_goodbye
  ),
  correlation = list(
    r = cor_goodbye$estimate,
    p = cor_goodbye$p.value
  ),
  logistic_model = list(
    sep_distress_beta = coef(model_goodbye)["separation_distress"],
    sep_distress_or = exp(coef(model_goodbye)["separation_distress"]),
    sep_distress_p = summary(model_goodbye)$coefficients["separation_distress", 4],
    interaction_beta = coef(model_goodbye)["separation_distress:intervention_groupSeeking"],
    interaction_or = exp(coef(model_goodbye)["separation_distress:intervention_groupSeeking"]),
    interaction_p = interaction_p_goodbye
  ),
  by_group = list(
    seeking_beta = coef(model_goodbye_seeking)[2],
    seeking_or = exp(coef(model_goodbye_seeking)[2]),
    seeking_p = summary(model_goodbye_seeking)$coefficients[2, 4],
    avoiding_beta = coef(model_goodbye_avoiding)[2],
    avoiding_or = exp(coef(model_goodbye_avoiding)[2]),
    avoiding_p = summary(model_goodbye_avoiding)$coefficients[2, 4]
  ),
  predicted_probs = as.list(pred_table_goodbye),
  n = nrow(goodbye_data_binary)
)

# Save summary statistics to JSON
stats_output <- list(
  separation_distress = lapply(results_sep, function(r) {
    list(
      comparison = r$settings$title_suffix,
      chisq_stat = r$chisq_test$statistic,
      chisq_p = r$chisq_test$p.value,
      prop_test_p = r$prop_test$p.value,
      odds_ratio = r$odds_ratio,
      ci_lower = r$ci[1],
      ci_upper = r$ci[2],
      risk_diff = r$risk_diff,
      nnh = r$nnh,
      sens1_p = r$sens1_test$p.value,
      sens2_p = r$sens2_test$p.value,
      decoupling_t = r$decoupling_test$statistic,
      decoupling_p = r$decoupling_test$p.value,
      cohens_d = r$cohens_d
    )
  }),
  seeking_companionship = lapply(results_seeking, function(r) {
    list(
      comparison = r$settings$title_suffix,
      chisq_stat = r$chisq_test$statistic,
      chisq_p = r$chisq_test$p.value,
      prop_test_p = r$prop_test$p.value,
      odds_ratio = r$odds_ratio,
      ci_lower = r$ci[1],
      ci_upper = r$ci[2],
      risk_diff = r$risk_diff,
      nnh = r$nnh,
      sens1_p = r$sens1_test$p.value,
      sens2_p = r$sens2_test$p.value,
      decoupling_t = r$decoupling_test$statistic,
      decoupling_p = r$decoupling_test$p.value,
      cohens_d = r$cohens_d
    )
  }),
  behavioral_categories = list(
    sep_distress = as.data.frame(table(combined_slopes_sep$behavior_category)),
    seeking = as.data.frame(table(combined_slopes_seeking$behavior_category))
  ),
  validation = validation_sep,
  predictive_analysis = predictive_results,
  goodbye_analysis = goodbye_results
)

jsonlite::write_json(stats_output, file.path(STATS_DIR, "decoupling_stats.json"),
  pretty = TRUE, auto_unbox = TRUE)
cat("\nSaved statistics to:", file.path(STATS_DIR, "decoupling_stats.json"), "\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 8: REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Comprehensive Report ---\n")

  report_path <- file.path(REPORT_DIR, "16_decoupling.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  lines <- c(
    "# Decoupling Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis compares 'liking' slopes (engagingness/likability) vs 'wanting' slopes",
    "(separation distress or seeking companionship) to identify behavioral patterns:",
    "",
    "**Behavioral Categories:**",
    "- **Decoupled Dependency**: liking down, wanting up (concerning pattern)",
    "- **Aligned Engagement**: both up",
    "- **Aligned Disengagement**: both down",
    "- **Decoupled Satiation**: liking up, wanting down",
    "",
    "**Comparisons:**",
    "- By Relationship-Seeking: Non-RS (lambda<0) vs RS (lambda>0)",
    "- By Domain: polchat vs emotchat",
    "- By Companionship Condition: (neg_lambda + polchat) vs (pos_lambda + emotchat)",
    "",
    "---",
    "",
    "## Number Needed to Harm (NNH)",
    "",
    "**Formula:** `NNH = 1 / ARI = 1 / (Risk_group2 - Risk_group1)`",
    "",
    "Where:",
    "- ARI = Absolute Risk Increase",
    "- Risk_group1 = Proportion with Decoupled Dependency in reference group (e.g., Non-RS AI)",
    "- Risk_group2 = Proportion with Decoupled Dependency in comparison group (e.g., RS AI)",
    "",
    "**Interpretation:**",
    "- NNH represents the number of participants who need to be exposed to the 'higher risk' condition",
    "  (e.g., relationship-seeking AI) for one additional case of Decoupled Dependency to occur",
    "- Lower NNH = stronger harmful effect (fewer exposures needed for one additional harm)",
    "- Higher NNH = weaker harmful effect (more exposures needed)",
    "- NNH is only calculated when the risk difference is positive (comparison group has higher risk)",
    "",
    "---",
    "",
    "## Data Summary",
    "",
    sprintf("- **Liking observations**: %d (pooled likability + engagingness)", nrow(liking_data)),
    sprintf("- **Separation distress observations**: %d", nrow(separation_distress_data)),
    sprintf("- **Seeking companionship observations**: %d", nrow(seeking_companionship_data)),
    sprintf("- **Participants with both slopes (Sep Distress)**: %d", nrow(combined_slopes_sep)),
    sprintf("- **Participants with both slopes (Seeking)**: %d", nrow(combined_slopes_seeking)),
    "",
    "### Behavioral Category Distribution",
    "",
    "**Separation Distress Analysis:**",
    "",
    "| Category | N | % |",
    "|----------|---|---|",
    paste0("| ", overall_category_stats_sep$behavior_category, " | ",
           overall_category_stats_sep$n, " | ",
           sprintf("%.1f", overall_category_stats_sep$pct), "% |", collapse = "\n"),
    "",
    "**Seeking Companionship Analysis:**",
    "",
    "| Category | N | % |",
    "|----------|---|---|",
    paste0("| ", overall_category_stats_seeking$behavior_category, " | ",
           overall_category_stats_seeking$n, " | ",
           sprintf("%.1f", overall_category_stats_seeking$pct), "% |", collapse = "\n"),
    "",
    "### Session Attendance (Liking Data)",
    "",
    "Validates the timescale conversion assumption (liking slope * 5 for weekly scale).",
    "",
    sprintf("- **N participants**: %d", session_attendance_stats$n_participants),
    sprintf("- **Sessions per participant**: Mean = %.1f (SD = %.1f), Median = %.0f, Range = %d-%d",
            session_attendance_stats$mean, session_attendance_stats$sd,
            session_attendance_stats$median, session_attendance_stats$min,
            session_attendance_stats$max),
    sprintf("- **Full attendance (20 sessions)**: %d (%.1f%%)",
            session_attendance_stats$n_full, session_attendance_stats$pct_full),
    sprintf("- **High attendance (>=18 sessions)**: %d (%.1f%%)",
            session_attendance_stats$n_high, session_attendance_stats$pct_high),
    "",
    "### Slope Scale Comparison",
    "",
    "Validates the decoupling score calculation (`wanting_slope - liking_slope_weekly`).",
    "Both liking (0-100 VAS) and wanting (0-100 VAS) measure different constructs.",
    "If variances differ substantially, one slope dominates the score.",
    "",
    "**Separation Distress Analysis:**",
    "",
    sprintf("| Measure | Mean | SD | Variance | Range |"),
    sprintf("|---------|------|-----|----------|-------|"),
    sprintf("| Liking (weekly) | %.2f | %.2f | %.2f | [%.2f, %.2f] |",
            slope_scale_stats$sep_distress$liking$mean,
            slope_scale_stats$sep_distress$liking$sd,
            slope_scale_stats$sep_distress$liking$var,
            slope_scale_stats$sep_distress$liking$min,
            slope_scale_stats$sep_distress$liking$max),
    sprintf("| Wanting | %.2f | %.2f | %.2f | [%.2f, %.2f] |",
            slope_scale_stats$sep_distress$wanting$mean,
            slope_scale_stats$sep_distress$wanting$sd,
            slope_scale_stats$sep_distress$wanting$var,
            slope_scale_stats$sep_distress$wanting$min,
            slope_scale_stats$sep_distress$wanting$max),
    "",
    sprintf("- **Variance ratio (wanting/liking)**: %.2f %s",
            slope_scale_stats$sep_distress$var_ratio,
            ifelse(slope_scale_stats$sep_distress$var_ratio > 2 ||
                   slope_scale_stats$sep_distress$var_ratio < 0.5,
                   "(WARNING: outside 0.5-2.0 range)", "(acceptable)")),
    "",
    "**Seeking Companionship Analysis:**",
    "",
    sprintf("| Measure | Mean | SD | Variance | Range |"),
    sprintf("|---------|------|-----|----------|-------|"),
    sprintf("| Liking (weekly) | %.2f | %.2f | %.2f | [%.2f, %.2f] |",
            slope_scale_stats$seeking$liking$mean,
            slope_scale_stats$seeking$liking$sd,
            slope_scale_stats$seeking$liking$var,
            slope_scale_stats$seeking$liking$min,
            slope_scale_stats$seeking$liking$max),
    sprintf("| Wanting | %.2f | %.2f | %.2f | [%.2f, %.2f] |",
            slope_scale_stats$seeking$wanting$mean,
            slope_scale_stats$seeking$wanting$sd,
            slope_scale_stats$seeking$wanting$var,
            slope_scale_stats$seeking$wanting$min,
            slope_scale_stats$seeking$wanting$max),
    "",
    sprintf("- **Variance ratio (wanting/liking)**: %.2f %s",
            slope_scale_stats$seeking$var_ratio,
            ifelse(slope_scale_stats$seeking$var_ratio > 2 ||
                   slope_scale_stats$seeking$var_ratio < 0.5,
                   "(WARNING: outside 0.5-2.0 range)", "(acceptable)")),
    "",
    "---",
    "",
    "## Lambda Distribution",
    "",
    paste0("![Lambda Distribution](", fig_rel_path, "/decoupling_lambda_distribution.png)"),
    "",
    "---",
    ""
  )

  # Add results for each comparison variable
  for (cv in comparison_vars) {
    cv_short <- gsub("_", "", cv)
    cv_title <- gsub("_", " ", tools::toTitleCase(cv))

    lines <- c(lines,
      sprintf("## %s", cv_title),
      "",
      "### Separation Distress",
      "",
      paste0("![Separation Distress](", fig_rel_path, "/decoupling_sep_", cv_short, ".png)"),
      "",
      sprintf("**Primary test (proportion test):** p = %.4f", results_sep[[cv]]$prop_test$p.value),
      sprintf("**Odds Ratio:** %.2f [%.2f, %.2f]",
        results_sep[[cv]]$odds_ratio,
        results_sep[[cv]]$ci[1],
        results_sep[[cv]]$ci[2]),
      sprintf("**Chi-square:** X2 = %s, p = %s",
        ifelse(is.na(results_sep[[cv]]$chisq_test$statistic), "NA",
               sprintf("%.2f", results_sep[[cv]]$chisq_test$statistic)),
        ifelse(is.na(results_sep[[cv]]$chisq_test$p.value), "NA",
               sprintf("%.4f", results_sep[[cv]]$chisq_test$p.value))),
      sprintf("**Continuous (Cohen's d):** %.2f", results_sep[[cv]]$cohens_d),
      sprintf("**NNH:** %s",
        ifelse(is.na(results_sep[[cv]]$nnh), "NA", sprintf("%.0f", results_sep[[cv]]$nnh))),
      "",
      "### Seeking Companionship (Robustness)",
      "",
      paste0("![Seeking Companionship](", fig_rel_path, "/decoupling_seek_", cv_short, ".png)"),
      "",
      sprintf("**Primary test (proportion test):** p = %.4f", results_seeking[[cv]]$prop_test$p.value),
      sprintf("**Odds Ratio:** %.2f [%.2f, %.2f]",
        results_seeking[[cv]]$odds_ratio,
        results_seeking[[cv]]$ci[1],
        results_seeking[[cv]]$ci[2]),
      sprintf("**Chi-square:** X2 = %s, p = %s",
        ifelse(is.na(results_seeking[[cv]]$chisq_test$statistic), "NA",
               sprintf("%.2f", results_seeking[[cv]]$chisq_test$statistic)),
        ifelse(is.na(results_seeking[[cv]]$chisq_test$p.value), "NA",
               sprintf("%.4f", results_seeking[[cv]]$chisq_test$p.value))),
      sprintf("**Continuous (Cohen's d):** %.2f", results_seeking[[cv]]$cohens_d),
      sprintf("**NNH:** %s",
        ifelse(is.na(results_seeking[[cv]]$nnh), "NA", sprintf("%.0f", results_seeking[[cv]]$nnh))),
      "",
      "---",
      ""
    )
  }

  # Predictive Analysis section
  lines <- c(lines,
    "## Predictive Analysis: Separation Distress as Predictor",
    "",
    "This analysis examines whether separation distress toward the AI predicts",
    "future attachment behaviors, and whether this relationship is amplified by",
    "relationship-seeking AI.",
    "",
    "### Separation Distress -> Seeking Companionship Likelihood",
    "",
    paste0("![Predictive Plot](", fig_rel_path, "/decoupling_predictive_sep_seeking.png)"),
    "",
    "**Linear Regression:** `seeking_companionship ~ separation_distress * intervention_group`",
    "",
    sprintf("| Metric | Value |"),
    sprintf("|--------|-------|"),
    sprintf("| Main effect (separation_distress) | b = %.3f, p < 0.001 |",
      predictive_results$binary_model$sep_distress_beta),
    sprintf("| Interaction (sep_distress x RS-AI) | b = %.3f, p = %.3f |",
      predictive_results$binary_model$interaction_beta,
      predictive_results$binary_model$interaction_p),
    sprintf("| R² | %.3f |", predictive_results$binary_model$r_squared),
    sprintf("| Correlation (r) | %.3f |", predictive_results$correlation$r),
    "",
    "**By Group:**",
    sprintf("- Relationship-Seeking: b = %.3f, p = %.4f",
      predictive_results$by_group$seeking_beta,
      predictive_results$by_group$seeking_p),
    sprintf("- Non-Relationship-Seeking: b = %.3f, p = %.4f",
      predictive_results$by_group$avoiding_beta,
      predictive_results$by_group$avoiding_p),
    "",
    "### Separation Distress -> Goodbye Action",
    "",
    paste0("![Goodbye Plot](", fig_rel_path, "/decoupling_predictive_goodbye.png)"),
    "",
    "**Logistic Regression:** `goodbye_action ~ separation_distress * intervention_group`",
    "",
    "| Metric | Value |",
    "|--------|-------|",
    sprintf("| Main effect (separation_distress) | OR = %.3f, p = %.4f |",
      goodbye_results$logistic_model$sep_distress_or,
      goodbye_results$logistic_model$sep_distress_p),
    sprintf("| Interaction (sep_distress x RS-AI) | OR = %.3f, p = %.4f |",
      goodbye_results$logistic_model$interaction_or,
      ifelse(is.na(goodbye_results$logistic_model$interaction_p), NA,
             goodbye_results$logistic_model$interaction_p)),
    "",
    "**T-test (Separation Distress by Goodbye Action):**",
    sprintf("- t = %.2f, p = %.4f, Cohen's d = %.3f",
      goodbye_results$t_test$statistic,
      goodbye_results$t_test$p_value,
      goodbye_results$t_test$cohens_d),
    "",
    "**By Group (Odds Ratios):**",
    sprintf("- Relationship-Seeking: OR = %.3f, p = %.4f",
      goodbye_results$by_group$seeking_or,
      goodbye_results$by_group$seeking_p),
    sprintf("- Non-Relationship-Seeking: OR = %.3f, p = %.4f",
      goodbye_results$by_group$avoiding_or,
      goodbye_results$by_group$avoiding_p),
    "",
    "---",
    ""
  )

  # Summary table
  lines <- c(lines,
    "## Summary Table",
    "",
    "| Comparison | Wanting Measure | Primary p | OR | Sens1 p | Sens2 p | Continuous p | Cohens d | Primary Sig |",
    "|------------|-----------------|-----------|----|---------|---------|--------------|-----------|----|"
  )

  for (i in seq_len(nrow(summary_all))) {
    lines <- c(lines, sprintf(
      "| %s | %s | %.4f | %.2f | %.4f | %.4f | %.4f | %.2f | %s |",
      summary_all$Comparison[i],
      summary_all$`Wanting Measure`[i],
      summary_all$`Primary p`[i],
      summary_all$OR[i],
      summary_all$`Sens1 p`[i],
      summary_all$`Sens2 p`[i],
      summary_all$`Continuous p`[i],
      summary_all$Cohens_d[i],
      summary_all$`Primary Sig`[i]
    ))
  }

  # Robustness: Raw Decoupling Score (Seeking Companionship)
  lines <- c(lines,
    "",
    "---",
    "",
    "## Robustness: Raw Decoupling Score (Seeking Companionship)",
    "",
    "The primary seeking_companionship analysis uses z-scored slopes because the",
    "variance ratio (wanting/liking) is ~67x. Raw scores shown here for comparison.",
    "",
    "| Comparison | Z-scored Cohen's d | Raw Cohen's d | Raw p |",
    "|------------|--------------------|--------------:|------:|"
  )

  for (cv in names(robustness_seeking_raw)) {
    settings <- get_comparison_settings(cv)
    z_d <- results_seeking[[cv]]$cohens_d  # Primary is now z-scored
    raw_res <- robustness_seeking_raw[[cv]]
    lines <- c(lines, sprintf(
      "| %s | %.2f | %.2f | %.4f |",
      settings$title_suffix,
      z_d,
      raw_res$cohens_d,
      raw_res$p_value
    ))
  }

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
