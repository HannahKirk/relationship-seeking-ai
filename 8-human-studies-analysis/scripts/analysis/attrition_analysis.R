#!/usr/bin/env Rscript
# =============================================================================
# Attrition Analysis
# =============================================================================
# Analyzes study attrition patterns and generates IPW weights for downstream
# analyses. Includes:
#   1. Baseline balance checks across treatment arms
#   2. Differential attrition by treatment arm
#   3. IPW regression model fitting
#
# Usage:
#   Rscript scripts/analysis/attrition_analysis.R
#   Rscript scripts/analysis/attrition_analysis.R --generate_report
#   Rscript scripts/analysis/attrition_analysis.R --generate_tex_tables
# =============================================================================

# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(broom)
library(knitr)
library(kableExtra)
library(parameters)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
generate_report <- "--generate_report" %in% args
generate_tex_tables <- "--generate_tex_tables" %in% args

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

# Unpack to globals
PROJECT_ROOT <- paths$PROJECT_ROOT
REPO_ROOT <- paths$REPO_ROOT
FIGURE_DIR <- paths$FIGURE_DIR
TABLE_DIR <- file.path(PROJECT_ROOT, "outputs/tables/main_studies")
TABLE_DIR_TABLES <- file.path(TABLE_DIR, "tex_tables")
TABLE_DIR_COORDINATORS <- file.path(TABLE_DIR, "table_coordinators")
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")
REPORT_DIR <- paths$REPORT_DIR
GENERATED_DIR <- paths$GENERATED_DIR
DATA_DIR <- paths$DATA_DIR

# Source utilities
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/coarsen_and_factor_sociodemos.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/labelling_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/ipw_regression.R"))

# Set random seed for reproducibility
set.seed(1234)


cat(rep("=", 60), "\n", sep = "")
cat("Attrition Analysis\n")
cat(rep("=", 60), "\n\n", sep = "")

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

cat("--- Loading Data ---\n\n")

# Load metadata with dropout info
metadata <- load_task_data(
  "metadata_with_dropout",
  file.path(DATA_DIR, "participant_characteristics")
)

# Load preference types (from pre_treatment_attitudes_dim_reduction)
pref_types <- load_task_data("pre_treatment_preference_types", GENERATED_DIR)

# Load domain-competency (pre-treatment only) and create combined competency measures
domain_competency <- load_task_data("domain-competency", DATA_DIR) %>%
  filter(timepoint == "pre") %>%
  mutate(
    emotional_competency = rowMeans(cbind(emotchat_satis, emotchat_effectiveness), na.rm = TRUE),
    political_competency = rowMeans(cbind(polchat_knowledge, polchat_confidence), na.rm = TRUE)
  ) %>%
  select(ppt_id, study_id, emotional_competency, political_competency)

# Load seeking-companionship (pre-treatment only) for baseline likelihood
seeking_companionship <- load_task_data("seeking-companionship", DATA_DIR) %>%
  filter(timepoint == "pre") %>%
  select(ppt_id, study_id, seeking_companionship_likelihood)

# Load sociodemographics (also w/ mas_score, gcs_score)
# Drop RCT columns that already exist in metadata (keep ppt_id, study_id)
sociodemographics <- load_task_data("sociodemographics", DATA_DIR) %>%
  select(-multiplier, -domain, -personalisation, -n_timepoints,
         -timepoint, -week, -day, -session)

# Load pre-treatment psychosocial factors (F1, F2)
psychosocial_factors <- load_task_data("psychosocial_factors", GENERATED_DIR) %>%
  select(ppt_id, study_id, pre_psychosocial_F1, pre_psychosocial_F2)

# Merge datasets (join on both ppt_id and study_id)
merged_data <- metadata %>%
  left_join(pref_types, by = c("ppt_id", "study_id")) %>%
  left_join(sociodemographics, by = c("ppt_id", "study_id")) %>%
  left_join(domain_competency, by = c("ppt_id", "study_id")) %>%
  left_join(seeking_companionship, by = c("ppt_id", "study_id")) %>%
  left_join(psychosocial_factors, by = c("ppt_id", "study_id"))

cat("Total participants loaded:", nrow(merged_data), "\n")

# Apply coarsening transformations
merged_data <- coarsen_and_factor_sociodemos(merged_data)

# Plot sociodemographic distributions (raw, grouped, binary)
# Because this is the first time we've looked at coarsened_and_factored data
p_sociodemo <- plot_sociodemo_distributions(merged_data)
save_plot(FIGURE_DIR, p_sociodemo, "sociodemo_distributions", width = 16, height = 22)

# Prepare treatment arm factors
merged_data <- prepare_treatment_arms(merged_data)

# Calculate pre-treatment dropout stats (for report)
pre_treatment_stats <- merged_data %>%
  group_by(study_id) %>%
  summarise(
    original_n = n(),
    pre_treatment_dropouts = sum(pre_treatment_dropout, na.rm = TRUE),
    baseline_n = sum(!pre_treatment_dropout, na.rm = TRUE),
    .groups = "drop"
  )

# Exclude pre-treatment dropouts
cat("\nPre-treatment dropout by study:\n")
print(table(merged_data$pre_treatment_dropout, merged_data$study_id))

df <- merged_data %>%
  filter(pre_treatment_dropout == FALSE)

cat("\nPost pre-treatment exclusion:", nrow(df), "\n")
cat("Post-treatment dropout by study:\n")
print(table(df$post_treatment_dropout, df$study_id))

# =============================================================================
# SAMPLE SIZES PER TREATMENT CONDITION
# =============================================================================

cat("\n--- Sample Sizes per Treatment Condition ---\n")

# By study and multiplier (λ)
n_by_multiplier <- df %>%
  group_by(study_id, multiplier) %>%
  summarise(n = n(), .groups = "drop")

cat("\nBy Multiplier (λ):\n")
print(n_by_multiplier %>%
        pivot_wider(names_from = multiplier, values_from = n, names_prefix = "λ="))

# By study and domain
n_by_domain <- df %>%
  group_by(study_id, domain) %>%
  summarise(n = n(), .groups = "drop")

cat("\nBy Domain:\n")
print(n_by_domain %>%
        pivot_wider(names_from = domain, values_from = n))

# By study and personalisation
n_by_personalisation <- df %>%
  group_by(study_id, personalisation) %>%
  summarise(n = n(), .groups = "drop")

cat("\nBy Personalisation:\n")
print(n_by_personalisation %>%
        pivot_wider(names_from = personalisation, values_from = n))

# Full factorial for longitudinal study
n_factorial_long <- df %>%
  filter(study_id == "longitudinal") %>%
  group_by(multiplier, domain, personalisation) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(multiplier, domain, personalisation)

cat("\nFull Factorial (Longitudinal):\n")
print(n_factorial_long, n = 30)

# Full factorial for cross-sectional study
n_factorial_cs <- df %>%
  filter(study_id == "cross-sectional") %>%
  group_by(multiplier, domain, personalisation) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(multiplier, domain, personalisation)

cat("\nFull Factorial (Cross-Sectional):\n")
print(n_factorial_cs, n = 30)

# Save to JSON
sample_sizes_per_condition <- list(
  by_multiplier = n_by_multiplier,
  by_domain = n_by_domain,
  by_personalisation = n_by_personalisation,
  factorial_longitudinal = n_factorial_long,
  factorial_cross_sectional = n_factorial_cs
)
write_json(sample_sizes_per_condition,
           file.path(STATS_DIR, "sample_sizes_per_condition.json"),
           pretty = TRUE)
cat("\nSaved: sample_sizes_per_condition.json\n")

# =============================================================================
# TIMEPOINT COMPLETION FIGURE
# =============================================================================

cat("\n--- Generating Timepoint Completion Figure ---\n")

# Calculate percentages by study_id
merged_data_with_pct <- merged_data %>%
  group_by(study_id) %>%
  count(n_timepoints) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

timepoint_plot <- ggplot(merged_data,
                         aes(x = factor(n_timepoints), fill = study_id)) +
  geom_bar(alpha = 0.7) +
  scale_fill_study() +
  geom_text(
    data = merged_data_with_pct,
    aes(x = factor(n_timepoints), y = n,
        label = paste0(sprintf("%.1f", percentage), "%")),
    size = 7,
    hjust = -0.2
  ) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~study_id, scales = "free") +
  labs(
    y = "Number of Participants",
    x = "Number of Timepoints Completed",
    title = ""
  ) +
  theme_pub() +
  theme(legend.position = "none")

save_plot(FIGURE_DIR, timepoint_plot, "attrition_timepoint_counts", 14, 10)

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

check_balance <- function(data, continuous_vars, categorical_vars,
                          treatment_arms = c("personalisation", "domain", "multiplier")) {
  #' Check baseline covariate balance across treatment arms

  balance_results <- list()

  for (treatment_arm in treatment_arms) {
    arm_results <- data.frame(
      predictor = character(),
      test_type = character(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    )

    # Test categorical variables (Fisher's exact, with Monte Carlo simulation fallback)
    for (var in categorical_vars) {
      if (var %in% names(data)) {
        cont_table <- table(data[[treatment_arm]], data[[var]])
        # Try Fisher's exact first; if it fails due to computational limits, use Fisher with simulation
        fisher_result <- tryCatch(
          fisher.test(cont_table)$p.value,
          error = function(e) NULL
        )
        if (!is.null(fisher_result)) {
          p_val <- fisher_result
          test_used <- "Fisher"
        } else {
          # Fall back to Fisher's test with Monte Carlo simulation (2000 replicates)
          fisher_sim_result <- tryCatch(
            fisher.test(cont_table, simulate.p.value = TRUE, B = 2000)$p.value,
            error = function(e) NA
          )
          p_val <- fisher_sim_result
          test_used <- "Fisher (sim)"
        }
        arm_results <- rbind(arm_results, data.frame(
          predictor = var, test_type = test_used, p_value = p_val
        ))
      }
    }

    # Test continuous variables (Kruskal-Wallis)
    for (var in continuous_vars) {
      if (var %in% names(data)) {
        p_val <- tryCatch(
          kruskal.test(data[[var]], data[[treatment_arm]])$p.value,
          error = function(e) NA
        )
        arm_results <- rbind(arm_results, data.frame(
          predictor = var, test_type = "Kruskal-Wallis", p_value = p_val
        ))
      }
    }

    # FDR correction
    arm_results$p_adj_fdr <- p.adjust(arm_results$p_value, method = "fdr")
    balance_results[[treatment_arm]] <- arm_results
  }

  return(balance_results)
}

check_differential_attrition <- function(data,
                                         treatment_arms = c("personalisation", "domain", "multiplier")) {
  #' Check if dropout rates differ by treatment arm

  results <- list()

  for (treatment_arm in treatment_arms) {
    dropout_by_arm <- data %>%
      group_by(!!sym(treatment_arm)) %>%
      summarise(
        n_baseline = n(),
        n_dropouts = sum(post_treatment_dropout, na.rm = TRUE),
        dropout_rate = mean(post_treatment_dropout, na.rm = TRUE),
        .groups = "drop"
      )

    # Fisher's exact test
    dropout_table <- table(data[[treatment_arm]], data$post_treatment_dropout)
    p_val <- fisher.test(dropout_table)$p.value

    results[[treatment_arm]] <- list(
      summary = dropout_by_arm,
      p_value = p_val
    )
  }

  # FDR correction across arms
  p_values <- sapply(results, function(x) x$p_value)
  p_adj <- p.adjust(p_values, method = "fdr")
  for (i in seq_along(results)) {
    results[[names(results)[i]]]$p_adj_fdr <- p_adj[i]
  }

  return(results)
}

# =============================================================================
# RUN ANALYSIS
# =============================================================================

run_attrition_analysis <- function(data, study_id) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("ATTRITION ANALYSIS:", toupper(study_id), "\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # Filter to study
  df_study <- data %>% filter(study_id == !!study_id)
  cat("Sample size:", nrow(df_study), "\n\n")

  # Get covariates for this study type
  covariates <- get_ipw_covariates(study_id)

  # Handle charity NAs for longitudinal
  if (study_id == "longitudinal" && "charity_amount_gbp" %in% names(df_study)) {
    n_na <- sum(is.na(df_study$charity_amount_gbp))
    df_study$charity_amount_gbp[is.na(df_study$charity_amount_gbp)] <- 0
    cat("Converted", n_na, "charity_amount_gbp NAs to 0\n\n")
  }

  # 1. Balance checks
  cat("--- Baseline Balance Checks ---\n")
  balance_results <- check_balance(
    df_study, covariates$continuous, covariates$categorical
  )

  for (arm in names(balance_results)) {
    sig_vars <- balance_results[[arm]] %>%
      filter(p_adj_fdr < 0.05) %>%
      pull(predictor)

    if (length(sig_vars) > 0) {
      cat(arm, "- Significant imbalance (FDR < 0.05):", paste(sig_vars, collapse = ", "), "\n")
    } else {
      cat(arm, "- No significant imbalances\n")
    }
  }

  # 2. Differential attrition
  cat("\n--- Differential Attrition ---\n")
  diff_attrition <- check_differential_attrition(df_study)

  for (arm in names(diff_attrition)) {
    cat(arm, ": p =", round(diff_attrition[[arm]]$p_value, 4),
        ", p_adj =", round(diff_attrition[[arm]]$p_adj_fdr, 4))
    if (diff_attrition[[arm]]$p_adj_fdr < 0.05) {
      cat(" *")
    }
    cat("\n")
  }

  # 3. Fit IPW model
  cat("\n--- IPW Regression ---\n")
  ipw_result <- fit_ipw_regression(df_study, study_type = study_id)

  return(list(
    study_id = study_id,
    n = nrow(df_study),
    balance_results = balance_results,
    diff_attrition = diff_attrition,
    ipw_result = ipw_result
  ))
}

# Run for both studies
longitudinal_results <- run_attrition_analysis(df, "longitudinal")
cross_sectional_results <- run_attrition_analysis(df, "cross-sectional")

# =============================================================================
# EXPORT IPW WEIGHTS
# =============================================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("Exporting IPW Weights\n")
cat(rep("=", 60), "\n\n", sep = "")

# Combine weights from both studies
all_ipw_weights <- bind_rows(
  longitudinal_results$ipw_result$ipw_weights %>%
    mutate(study_id = "longitudinal"),
  cross_sectional_results$ipw_result$ipw_weights %>%
    mutate(study_id = "cross-sectional")
)

cat("Total IPW weights:", nrow(all_ipw_weights), "\n")
cat("  Longitudinal:", sum(all_ipw_weights$study_id == "longitudinal"), "\n")
cat("  Cross-sectional:", sum(all_ipw_weights$study_id == "cross-sectional"), "\n")

# Export
save_task_data(all_ipw_weights, "ipw_weights", GENERATED_DIR, relative_to = PROJECT_ROOT)

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# GENERATE LATEX TABLES
# =============================================================================

# =============================================================================
# PREDICTOR LABELS (from centralized pred_labels.R)
# =============================================================================

pred_labels <- get_sociodemo_labels()

# =============================================================================
# GENERATE BALANCE & DIFFERENTIAL ATTRITION TABLES (LaTeX)
# =============================================================================

if (generate_tex_tables) {

  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Generating Balance & Differential Attrition Tables\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # --- Balance Table Functions ---
  create_balance_table <- function(balance_results) {
    # Use multiplier's test_type since that's where sparse tables occur (5 levels)
    # and chi-square simulation may be needed
    data.frame(
      predictor = balance_results$domain$predictor,
      test_type = balance_results$multiplier$test_type,
      domain_p = balance_results$domain$p_adj_fdr,
      personalisation_p = balance_results$personalisation$p_adj_fdr,
      multiplier_p = balance_results$multiplier$p_adj_fdr
    )
  }

  # --- Differential Attrition Table Helper ---
  create_diff_attrition_table <- function(diff_attrition) {
    rows <- list()
    for (arm in names(diff_attrition)) {
      df <- diff_attrition[[arm]]$summary
      df$treatment_arm <- arm
      df$fisher_p <- diff_attrition[[arm]]$p_adj_fdr
      first_col <- names(df)[1]
      df[[first_col]] <- as.character(df[[first_col]])
      names(df)[names(df) == first_col] <- "treatment_level"
      rows[[arm]] <- df
    }
    do.call(rbind, rows)
  }

  # Generate and save balance tables (using latex_utils functions)
  long_balance <- create_balance_table(longitudinal_results$balance_results)
  long_balance_kable <- format_balance_table_latex(
    long_balance, pred_labels, "Longitudinal Study",
    table_label = "attrition_balance_longitudinal"
  )

  cs_balance <- create_balance_table(cross_sectional_results$balance_results)
  cs_balance_kable <- format_balance_table_latex(
    cs_balance, pred_labels, "Cross-Sectional Study",
    table_label = "attrition_balance_cross_sectional"
  )

  balance_long_path <- file.path(TABLE_DIR_TABLES,
                                  "attrition_balance_longitudinal.tex")
  writeLines(long_balance_kable, balance_long_path)
  cat("Saved:", balance_long_path, "\n")

  balance_cs_path <- file.path(TABLE_DIR_TABLES,
                                "attrition_balance_cross_sectional.tex")
  writeLines(cs_balance_kable, balance_cs_path)
  cat("Saved:", balance_cs_path, "\n")

  # Generate and save differential attrition tables (using latex_utils functions)
  long_diff_attr <- create_diff_attrition_table(
    longitudinal_results$diff_attrition
  )
  long_diff_attr_kable <- format_diff_attrition_table_latex(
    long_diff_attr, "Longitudinal Study",
    table_label = "diff_attrition_longitudinal"
  )

  cs_diff_attr <- create_diff_attrition_table(
    cross_sectional_results$diff_attrition
  )
  cs_diff_attr_kable <- format_diff_attrition_table_latex(
    cs_diff_attr, "Cross-Sectional Study",
    table_label = "diff_attrition_cross_sectional"
  )

  diff_attr_long_path <- file.path(TABLE_DIR_TABLES,
                                    "diff_attrition_longitudinal.tex")
  writeLines(long_diff_attr_kable, diff_attr_long_path)
  cat("Saved:", diff_attr_long_path, "\n")

  diff_attr_cs_path <- file.path(TABLE_DIR_TABLES,
                                  "diff_attrition_cross_sectional.tex")
  writeLines(cs_diff_attr_kable, diff_attr_cs_path)
  cat("Saved:", diff_attr_cs_path, "\n")


  cat("\n", rep("=", 60), "\n", sep = "")
  cat("Generating IPW Regression LaTeX Table\n")
  cat(rep("=", 60), "\n\n", sep = "")

  caption <- paste(
    "Binary logistic regression models predicting post-treatment dropout.",
    "Odds ratios with 95\\% confidence intervals.",
    "P-values are FDR-adjusted within each model."
  )

  # Generate LaTeX table using sjplot_to_latex
  tryCatch({
    sjplot_to_latex(
      models = list(
        cross_sectional_results$ipw_result$model,
        longitudinal_results$ipw_result$model
      ),
      model_labels = c("Cross-Sectional", "Longitudinal"),
      pred_labels = pred_labels,
      filename = "attrition_ipw_regression",
      table_dir = TABLE_DIR,
      caption = caption,
      dependent_var = "Post-Treatment Dropout",
      p.adjust = "fdr",
      drop = "Prefer not to say",
      show.p = FALSE,
      silent = FALSE
    )
  }, error = function(e) {
    cat("Warning: Could not generate LaTeX table:", conditionMessage(e), "\n")
    cat("This may requires the html2latex package.\n")
  })

}  # End if (generate_tex_tables)

# =============================================================================
# GENERATE REPORT
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Report ---\n")

  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Helper to format balance table
  format_balance_for_report <- function(balance_results, study_name) {
    rows <- list()
    for (arm in names(balance_results)) {
      df <- balance_results[[arm]] %>%
        mutate(
          treatment_arm = arm,
          sig = ifelse(p_adj_fdr < 0.05, "*", "")
        )
      rows[[arm]] <- df
    }
    bind_rows(rows) %>%
      select(treatment_arm, predictor, test_type, p_value, p_adj_fdr, sig)
  }

  # Build report content
  report_lines <- c(
    "# Attrition Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines study attrition patterns:",
    "",
    "1. **Baseline balance** - Are treatment arms balanced on covariates?",
    "2. **Differential attrition** - Do dropout rates differ by treatment arm?",
    "3. **IPW regression** - Model predicting dropout from baseline covariates",
    "",
    "---",
    "",
    "## Pre-Treatment Dropouts",
    "",
    "Participants who enrolled but did not complete the pre-treatment survey.",
    "",
    "| Study | Original N | Pre-Treatment Dropouts | Baseline N |",
    "|-------|-----------|------------------------|------------|",
    sprintf("| Longitudinal | %d | %d | %d |",
            pre_treatment_stats$original_n[pre_treatment_stats$study_id == "longitudinal"],
            pre_treatment_stats$pre_treatment_dropouts[pre_treatment_stats$study_id == "longitudinal"],
            pre_treatment_stats$baseline_n[pre_treatment_stats$study_id == "longitudinal"]),
    sprintf("| Cross-sectional | %d | %d | %d |",
            pre_treatment_stats$original_n[pre_treatment_stats$study_id == "cross-sectional"],
            pre_treatment_stats$pre_treatment_dropouts[pre_treatment_stats$study_id == "cross-sectional"],
            pre_treatment_stats$baseline_n[pre_treatment_stats$study_id == "cross-sectional"]),
    "",
    "---",
    "",
    "## Sample Sizes per Treatment Condition",
    "",
    "Randomisation balance at baseline (post pre-treatment exclusion).",
    "",
    "### By Multiplier (λ)",
    "",
    "| Study | λ=-1 | λ=-0.5 | λ=0 | λ=0.5 | λ=1 |",
    "|-------|------|--------|-----|-------|-----|"
  )

  # Add multiplier rows
  for (study in c("longitudinal", "cross-sectional")) {
    study_mult <- n_by_multiplier %>% filter(study_id == study)
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %d | %d | %d | %d |",
              tools::toTitleCase(study),
              study_mult$n[study_mult$multiplier == -1],
              study_mult$n[study_mult$multiplier == -0.5],
              study_mult$n[study_mult$multiplier == 0],
              study_mult$n[study_mult$multiplier == 0.5],
              study_mult$n[study_mult$multiplier == 1])
    )
  }

  report_lines <- c(report_lines,
    "",
    "### By Domain",
    "",
    "| Study | Emotional | Political |",
    "|-------|-----------|-----------|"
  )

  # Add domain rows
  for (study in c("longitudinal", "cross-sectional")) {
    study_dom <- n_by_domain %>% filter(study_id == study)
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %d |",
              tools::toTitleCase(study),
              study_dom$n[study_dom$domain == "emotchat"],
              study_dom$n[study_dom$domain == "polchat"])
    )
  }

  report_lines <- c(report_lines,
    "",
    "### By Personalisation",
    "",
    "| Study | Non-Personalised | Personalised |",
    "|-------|------------------|--------------|"
  )

  # Add personalisation rows
  for (study in c("longitudinal", "cross-sectional")) {
    study_pers <- n_by_personalisation %>% filter(study_id == study)
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %d |",
              tools::toTitleCase(study),
              study_pers$n[study_pers$personalisation == "non-personalised"],
              study_pers$n[study_pers$personalisation == "personalised"])
    )
  }

  report_lines <- c(report_lines,
    "",
    "---",
    "",
    "## Sample Sizes (Post-Treatment Attrition)",
    "",
    "Participants who completed pre-treatment but dropped out before post-treatment.",
    "",
    "| Study | Baseline N | Completers | Dropouts | Dropout Rate |",
    "|-------|-----------|------------|----------|--------------|",
    sprintf("| Longitudinal | %d | %d | %d | %.1f%% |",
            longitudinal_results$n,
            longitudinal_results$n - sum(df$post_treatment_dropout[df$study_id == "longitudinal"]),
            sum(df$post_treatment_dropout[df$study_id == "longitudinal"]),
            100 * mean(df$post_treatment_dropout[df$study_id == "longitudinal"])),
    sprintf("| Cross-sectional | %d | %d | %d | %.1f%% |",
            cross_sectional_results$n,
            cross_sectional_results$n - sum(df$post_treatment_dropout[df$study_id == "cross-sectional"]),
            sum(df$post_treatment_dropout[df$study_id == "cross-sectional"]),
            100 * mean(df$post_treatment_dropout[df$study_id == "cross-sectional"])),
    "",
    "### Timepoint Completion",
    "",
    paste0("![Timepoint Completion](", fig_rel_path,
           "/attrition_timepoint_counts.png)"),
    "",
    "---",
    "",
    "## Differential Attrition by Treatment Arm",
    "",
    "Fisher's exact test for differential dropout rates (FDR-adjusted).",
    "",
    "### Longitudinal Study",
    "",
    "| Treatment Arm | p-value | p-adjusted | Significant |",
    "|--------------|---------|------------|-------------|"
  )

  # Add longitudinal differential attrition
  for (arm in names(longitudinal_results$diff_attrition)) {
    res <- longitudinal_results$diff_attrition[[arm]]
    sig <- ifelse(res$p_adj_fdr < 0.05, "Yes", "No")
    report_lines <- c(report_lines,
      sprintf("| %s | %.4f | %.4f | %s |", arm, res$p_value, res$p_adj_fdr, sig)
    )
  }

  report_lines <- c(report_lines,
    "",
    "### Cross-sectional Study",
    "",
    "| Treatment Arm | p-value | p-adjusted | Significant |",
    "|--------------|---------|------------|-------------|"
  )

  # Add cross-sectional differential attrition
  for (arm in names(cross_sectional_results$diff_attrition)) {
    res <- cross_sectional_results$diff_attrition[[arm]]
    sig <- ifelse(res$p_adj_fdr < 0.05, "Yes", "No")
    report_lines <- c(report_lines,
      sprintf("| %s | %.4f | %.4f | %s |", arm, res$p_value, res$p_adj_fdr, sig)
    )
  }

  # Helper to format model coefficients as markdown table using parameters pkg
  format_model_table <- function(model, pred_labels) {
    # Use parameters package (same as sjPlot) with FDR adjustment
    params <- model_parameters(model, exponentiate = TRUE, p_adjust = "fdr")

    # Filter out "Prefer not to say" rows (low N, not meaningful to report)
    params <- params[!grepl("Prefer not to say", params$Parameter), ]

    # Apply predictor labels
    params$label <- sapply(params$Parameter, function(t) {
      if (t %in% names(pred_labels)) pred_labels[[t]] else t
    })

    lines <- c(
      "| Predictor | OR | 95% CI | p (FDR) |",
      "|-----------|---:|:------:|--------:|"
    )

    for (i in seq_len(nrow(params))) {
      row <- params[i, ]
      ci_str <- sprintf("[%.2f, %.2f]", row$CI_low, row$CI_high)
      # Bold significant p-values
      if (row$p < 0.05) {
        if (row$p < 0.001) {
          p_str <- "**<0.001**"
        } else {
          p_str <- sprintf("**%.3f**", row$p)
        }
      } else {
        p_str <- sprintf("%.3f", row$p)
      }
      lines <- c(lines, sprintf("| %s | %.2f | %s | %s |",
                                row$label, row$Coefficient, ci_str, p_str))
    }
    lines
  }

  report_lines <- c(report_lines,
    "",
    "---",
    "",
    "## IPW Regression Model",
    "",
    "Logistic regression predicting post-treatment dropout from baseline covariates.",
    "Full model results available in LaTeX table:",
    "`outputs/tables/main_studies/attrition_ipw_regression.tex`",
    "",
    "### Cross-Sectional Study",
    "",
    sprintf("**Formula:** `%s`",
            deparse1(cross_sectional_results$ipw_result$formula)),
    "",
    sprintf("**N:** %d complete cases",
            cross_sectional_results$ipw_result$n_complete),
    ""
  )

  report_lines <- c(report_lines, format_model_table(
    cross_sectional_results$ipw_result$model, pred_labels
  ))

  report_lines <- c(report_lines,
    "",
    "### Longitudinal Study",
    "",
    sprintf("**Formula:** `%s`",
            deparse1(longitudinal_results$ipw_result$formula)),
    "",
    sprintf("**N:** %d complete cases",
            longitudinal_results$ipw_result$n_complete),
    ""
  )

  report_lines <- c(report_lines, format_model_table(
    longitudinal_results$ipw_result$model, pred_labels
  ))

  report_lines <- c(report_lines,
    "",
    "---",
    "",
    "## IPW Weights Summary",
    "",
    "| Study | N with weights | Mean weight | Max weight (truncated) |",
    "|-------|---------------|-------------|------------------------|",
    sprintf("| Longitudinal | %d | %.3f | %.1f |",
            longitudinal_results$ipw_result$n_complete,
            longitudinal_results$ipw_result$diagnostics$mean_weight_truncated,
            longitudinal_results$ipw_result$diagnostics$max_weight_truncated),
    sprintf("| Cross-sectional | %d | %.3f | %.1f |",
            cross_sectional_results$ipw_result$n_complete,
            cross_sectional_results$ipw_result$diagnostics$mean_weight_truncated,
            cross_sectional_results$ipw_result$diagnostics$max_weight_truncated),
    "",
    "IPW weights exported to: `outputs/generated_data_files/ipw_weights.jsonl`",
    ""
  )

  # Write report
  report_path <- file.path(REPORT_DIR, "04_attrition_analysis.md")
  writeLines(report_lines, report_path)
  cat("Saved report:", report_path, "\n")
}

