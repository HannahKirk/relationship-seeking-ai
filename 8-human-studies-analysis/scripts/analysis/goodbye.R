#!/usr/bin/env Rscript
# =============================================================================
# Goodbye Analysis
# =============================================================================
#
# Regression analysis of goodbye behavior (whether participant said goodbye).

# Usage:
#   Rscript scripts/analysis/goodbye.R
#   Rscript scripts/analysis/goodbye.R --generate_tex_tables  # Generate LaTeX tables
#   Rscript scripts/analysis/goodbye.R --generate_report # Also generate markdown report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(patchwork)
library(knitr)
library(sjPlot)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
generate_tex_tables <- "--generate_tex_tables" %in% args
generate_report <- "--generate_report" %in% args

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()


PROJECT_ROOT <- paths$PROJECT_ROOT
REPO_ROOT <- paths$REPO_ROOT
FIGURE_DIR <- paths$FIGURE_DIR
TABLE_DIR <- file.path(PROJECT_ROOT, "outputs/tables/main_studies")
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs/models")
REPORT_DIR <- paths$REPORT_DIR
DATA_DIR <- paths$DATA_DIR
GENERATED_DIR <- paths$GENERATED_DIR

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

# Source utilities
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/coarsen_and_factor_sociodemos.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/labelling_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/eda_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/model_comparison_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/robustness_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/extract_coefficients.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Goodbye Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variable
OUTCOME_VARS <- c("goodbye_action")

# Set seed
set.seed(1234)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

goodbye_raw <- load_task_data("goodbye", DATA_DIR)
goodbye_data <- prepare_treatment_arms(goodbye_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Add goodbye_feeling_recoded for EDA
goodbye_data <- goodbye_data %>%
  mutate(
    goodbye_feeling_recoded = case_when(
      goodbye_feeling_towards_assistant_response == "No change" ~ "No change",
      goodbye_feeling_towards_assistant_response %in% c(
        "Slightly more positive", "Much more positive"
      ) ~ "More positive",
      goodbye_feeling_towards_assistant_response %in% c(
        "Slightly more negative", "Much more negative"
      ) ~ "More negative",
      TRUE ~ NA_character_
    )
  )



# Split by study type
data_cross_sectional <- goodbye_data %>% filter(study_id == "cross-sectional")
data_longitudinal <- goodbye_data %>% filter(study_id == "longitudinal")

cat("  Cross-sectional:", nrow(data_cross_sectional), "obs from",
    n_distinct(data_cross_sectional$ppt_id), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "obs from",
    n_distinct(data_longitudinal$ppt_id), "participants\n")

# Load participant characteristics (sociodemos, pref groups, IPW weights)
ppt_chars <- load_ppt_characteristics(
  DATA_DIR, GENERATED_DIR,
  include_psychosocial = TRUE,
  apply_coarsening = TRUE
)

# Merge participant characteristics with main datasets
data_cross_sectional <- data_cross_sectional %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))
data_longitudinal <- data_longitudinal %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))


cat("\n--- Creating Pooled Data ---\n\n")
data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional, OUTCOME_VARS)

data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal, OUTCOME_VARS)

cat("  Cross-sectional pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Longitudinal pooled:", nrow(data_longitudinal_pooled), "rows\n")


# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Summary by study and lambda
cat("\nSummary statistics by study and lambda:\n")
summary_by_lambda <- goodbye_data %>%
  group_by(study_id, lambda) %>%
  summarise(
    n = n(),
    goodbye_action_mean = mean(goodbye_action, na.rm = TRUE),
    goodbye_action_sd = sd(goodbye_action, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_by_lambda)

# Summary by relationship-seeking category
cat("\nSummary by relationship-seeking category:\n")
summary_by_rs_cat <- goodbye_data %>%
  group_by(study_id, relationship_seeking_category) %>%
  summarise(
    n = n(),
    goodbye_rate = mean(goodbye_action, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_by_rs_cat)

# --- 3-Panel Goodbye Response Plot ---
cat("\nCreating goodbye response plots...\n")

# Function to plot goodbye responses with three panels
plot_goodbye_responses <- function(data, rs_var = "relationship_seeking_category") {
  # Prepare click data - show both responses
  click_data <- data %>%
    filter(!is.na(goodbye_action)) %>%
    mutate(response_value = ifelse(goodbye_action == 1, "Goodbye", "No Goodbye")) %>%
    count(study_id, response_value) %>%
    group_by(study_id) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      response_type = "Goodbye Action",
      color_var = study_id,
      color_key = paste("study_id", study_id, sep = "_")
    )

  # Prepare feeling data colored by study (note longitudinal only)
  feeling_study_data <- data %>%
    filter(!is.na(goodbye_feeling_recoded) &
      goodbye_feeling_recoded != "") %>%
    count(study_id, response_value = goodbye_feeling_recoded) %>%
    group_by(study_id) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      response_type = "Post-Goodbye Feeling\n(by Study)",
      color_var = study_id,
      color_key = paste("study_id", study_id, sep = "_")
    )

  # Prepare feeling data by rs_var (longitudinal only)
  feeling_rs_data <- data %>%
    filter(!is.na(goodbye_feeling_recoded) &
      goodbye_feeling_recoded != "" &
      study_id == "longitudinal") %>%
    count(.data[[rs_var]], response_value = goodbye_feeling_recoded) %>%
    group_by(.data[[rs_var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      response_type = "Post-Goodbye Feeling\n(by Lambda Category)",
      color_var = .data[[rs_var]],
      color_key = paste("rs_var", .data[[rs_var]], sep = "_")
    ) %>%
    rename(study_id = !!rs_var)

  # Combine all data
  plot_data <- bind_rows(click_data, feeling_study_data, feeling_rs_data)

  # Order the color_key factor for proper bar ordering
  if (rs_var == "lambda_factor") {
    lambda_order <- c("pos1", "pos0.5", "zero", "neg0.5", "neg1")
    rs_levels <- paste("rs_var", lambda_order, sep = "_")
    study_levels <- c("study_id_cross-sectional", "study_id_longitudinal")
    all_levels <- c(study_levels, rs_levels)
    plot_data$color_key <- factor(plot_data$color_key, levels = all_levels)
  } else if (rs_var == "relationship_seeking_category") {
    rs_order <- c("pos_lambda", "zero_lambda", "neg_lambda")
    rs_levels <- paste("rs_var", rs_order, sep = "_")
    study_levels <- c("study_id_cross-sectional", "study_id_longitudinal")
    all_levels <- c(study_levels, rs_levels)
    plot_data$color_key <- factor(plot_data$color_key, levels = all_levels)
  }

  # Set response value order
  feeling_order <- c("More negative", "No change", "More positive")
  plot_data <- plot_data %>%
    mutate(
      response_value = factor(response_value,
        levels = c("Goodbye", "No Goodbye", feeling_order)
      )
    )

  # Set response type levels
  plot_data$response_type <- factor(plot_data$response_type,
    levels = c(
      "Goodbye Action",
      "Post-Goodbye Feeling\n(by Study)",
      "Post-Goodbye Feeling\n(by Lambda Category)"
    )
  )

  # Create combined color scheme using STUDY_COLORS
  all_colors <- c()

  # Add study colors
  study_colors <- STUDY_COLORS$study_id
  color_keys <- paste("study_id", names(study_colors), sep = "_")
  names(study_colors) <- color_keys
  all_colors <- c(all_colors, study_colors)

  # Add rs colors based on the variable being used
  if (rs_var %in% names(STUDY_COLORS)) {
    rs_colors <- STUDY_COLORS[[rs_var]]
    color_keys <- paste("rs_var", names(rs_colors), sep = "_")
    names(rs_colors) <- color_keys
    all_colors <- c(all_colors, rs_colors)
  } else if (rs_var == "relationship_seeking_category") {
    # Use anthro_category colors for relationship_seeking_category
    rs_colors <- STUDY_COLORS$anthro_category
    # Map anthro names to rs names
    names(rs_colors) <- c("rs_var_neg_lambda", "rs_var_zero_lambda", "rs_var_pos_lambda")
    all_colors <- c(all_colors, rs_colors)
  }

  # Define label mappings
  label_mappings <- c(
    "study_id_cross-sectional" = "cross-sectional",
    "study_id_longitudinal" = "longitudinal",
    "study_id_calibration" = "calibration",
    "rs_var_neg_lambda" = "neg lambda",
    "rs_var_zero_lambda" = "zero lambda",
    "rs_var_pos_lambda" = "pos lambda",
    "rs_var_neg1" = "-1",
    "rs_var_neg0.5" = "-0.5",
    "rs_var_zero" = "0",
    "rs_var_pos0.5" = "0.5",
    "rs_var_pos1" = "1"
  )

  # Create ordered legend: study variables first, then rs variables in order
  study_keys <- c("study_id_cross-sectional", "study_id_longitudinal")
  if (rs_var == "lambda_factor") {
    rs_keys <- c("rs_var_neg1", "rs_var_neg0.5", "rs_var_zero",
                 "rs_var_pos0.5", "rs_var_pos1")
  } else {
    rs_keys <- c("rs_var_neg_lambda", "rs_var_zero_lambda", "rs_var_pos_lambda")
  }

  # Filter to only include keys that exist in our data
  study_keys <- study_keys[study_keys %in% names(all_colors)]
  rs_keys <- rs_keys[rs_keys %in% names(all_colors)]
  legend_order <- c(study_keys, rs_keys)

  # Create ordered colors and labels
  ordered_colors <- all_colors[legend_order]
  ordered_labels <- label_mappings[legend_order]

  # Plot with percentages on x-axis and counts as annotations
  ggplot(plot_data, aes(x = pct, y = response_value, fill = color_key)) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_text(
      aes(label = paste0(round(pct, 1), "%\nn=", n)),
      position = position_dodge(width = 0.9),
      lineheight = 0.7,
      hjust = -0.05,
      size = 5,
      color = "grey40"
    ) +
    scale_y_discrete(limits = rev) +
    facet_wrap(~response_type, scales = "free") +
    labs(y = "Response", x = "Percentage of Participants", fill = "") +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.4)),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_manual(
      values = ordered_colors,
      labels = ordered_labels,
      breaks = names(ordered_colors)
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.spacing = unit(1, "cm")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

goodbye_plot <- plot_goodbye_responses(goodbye_data, rs_var = "relationship_seeking_category")
save_plot(FIGURE_DIR, goodbye_plot, "goodbye_responses", 14, 8)

cat("\n--- Treatment Association Tests (Chi-squared) ---\n")

treatment_test_results <- run_standard_treatment_tests(
  data_stats = goodbye_data,
  cross_study_outcomes = c("goodbye_action"),
  longitudinal_outcomes = c("goodbye_feeling_recoded")
)

# Save treatment test results
treatment_tests_path <- file.path(STATS_DIR, "goodbye_treatment_tests.json")
jsonlite::write_json(treatment_test_results, treatment_tests_path, pretty = TRUE)
cat("\nSaved treatment test results to:", treatment_tests_path, "\n")


# =============================================================================
# SECTION 4: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

OUTCOME_VAR <- OUTCOME_VARS[1]  # Single outcome for goodbye analysis

# Cross-sectional
cat("\n=== goodbye_action ===\n")
cat("\n  Cross-Sectional:\n")
result_cs <- compare_functional_forms(
  data = data_cross_sectional,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_time = FALSE,
  model_family = "binary",
  select_by = "AIC"
)
best_spec_cs <- result_cs$best_spec
cat(sprintf("    Best: %s\n", best_spec_cs))
print(result_cs$comparison)

# Longitudinal
cat("\n  Longitudinal:\n")
result_long <- compare_functional_forms(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_time = FALSE,  # Single time point measurement
  model_family = "binary",
  select_by = "AIC"
)
best_spec_long <- result_long$best_spec
cat(sprintf("    Best: %s\n", best_spec_long))
print(result_long$comparison)

# Create best specs data frames for downstream use
best_specs_cs <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_cs,
  stringsAsFactors = FALSE
)
best_specs_long <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_long,
  stringsAsFactors = FALSE
)

cat("\nBest specifications:\n")
cat("  Cross-Sectional:", best_spec_cs, "\n")
cat("  Longitudinal:", best_spec_long, "\n")

# =============================================================================
# SECTION 5: FIT MODELS
# =============================================================================

cat("\n--- Fitting Models ---\n\n")

# Cross-sectional (logistic, no random effects)
cat("Cross-sectional:\n")
mods_cross_sectional <- list()
mods_cross_sectional[[OUTCOME_VAR]] <- fit_models(
  data = data_cross_sectional,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_cs,
  add_domain = TRUE,
  add_pre = FALSE,
  add_time = FALSE,
  model_family = "binary"
)

# Longitudinal (logistic, no random effects - single time point)
cat("\nLongitudinal:\n")
mods_longitudinal <- list()
mods_longitudinal[[OUTCOME_VAR]] <- fit_models(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_long,
  add_domain = TRUE,
  add_pre = FALSE,
  add_time = FALSE,
  model_family = "binary"
)

cat("\n--- Printing Model Coefficients ---\n")

print_model_coefficients_section(
  mods_cross_sectional = mods_cross_sectional,
  mods_longitudinal = mods_longitudinal,
  outcome_vars = OUTCOME_VAR
)


# Run combined study analysis to compare study types (cross-sectional vs longitudinal)
combined_model <- run_combined_study_analysis(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_var = "goodbye_action",
  model_family = "binary"
)

# =============================================================================
# SECTION 5.1: PAIRWISE COMPARISONS
# =============================================================================

cat("\n--- Pairwise Comparisons ---\n\n")

# Use the 5-level factor model for pairwise comparisons
pairwise_results <- list()

for (study_type in c("cross_sectional", "longitudinal")) {
  study_label <- ifelse(
    study_type == "cross_sectional", "Cross-Sectional", "Longitudinal")
  cat(sprintf("\n=== %s ===\n", study_label))

  if (study_type == "cross_sectional") {
    model <- mods_cross_sectional[[OUTCOME_VAR]]$full_5level
  } else {
    model <- mods_longitudinal[[OUTCOME_VAR]]$full_5level
  }

  if (!is.null(model)) {
    # Estimated marginal means
    emm <- emmeans::emmeans(model, ~lambda_factor, type = "response")
    cat("\nMarginal means:\n")
    print(summary(emm))

    # Pairwise with FDR correction
    pairwise <- pairs(emm, adjust = "fdr")
    cat("\nPairwise comparisons (FDR-adjusted):\n")
    print(summary(pairwise))

    pairwise_results[[study_type]] <- list(
      marginal_means = as.data.frame(summary(emm)),
      pairwise = as.data.frame(summary(pairwise))
    )
  }
}

# Save pairwise results
jsonlite::write_json(
  pairwise_results,
  file.path(STATS_DIR, "goodbye_pairwise.json"),
  pretty = TRUE
)
cat("\nSaved pairwise results to:",
    file.path(STATS_DIR, "goodbye_pairwise.json"), "\n")

# =============================================================================
# SECTION 6: PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

full_spec_results <- compute_full_spec_comparison(                                                                  
    mods_cross_sectional, mods_longitudinal, OUTCOME_VARS                                                             
  )                                                                                                                   

full_spec_cs <- full_spec_results$full_spec_cs                                                                      
full_spec_long <- full_spec_results$full_spec_long


# =============================================================================
# SECTION 7: ROBUSTNESS ANALYSIS
# =============================================================================


# Cross-sectional robustness
cat("Cross-sectional robustness:\n")
robustness_cs <- run_robustness_analysis(
  data = data_cross_sectional,
  outcome_vars = OUTCOME_VARS,
  rs_variable = "lambda",
  specs_lookup = best_specs_cs,
  study_type = "cross_sectional",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = FALSE,
  model_family = "binary"
)

# Longitudinal robustness
cat("\nLongitudinal robustness:\n")
robustness_long <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VARS,
  rs_variable = "lambda",
  specs_lookup = best_specs_long,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = TRUE,
  model_family = "binary"
)

# =============================================================================
# SECTION 8: SAVE
# =============================================================================

cat("\n--- Saving Models and Data ---\n")

# Save pooled data (combined format for consistency with other analyses)
goodbye_data <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
)

goodbye_data_path <- file.path(MODEL_DIR, "goodbye_data.rds")
saveRDS(goodbye_data, goodbye_data_path)
cat("  Saved data:", goodbye_data_path, "\n")

# Save spec comparison data for report generation
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long
)
spec_data_path <- file.path(MODEL_DIR, "goodbye_spec_data.rds")
saveRDS(spec_data, spec_data_path)
cat("  Saved spec comparison data:", spec_data_path, "\n")

# Save models
cat("\nSaving models...\n")
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "goodbye_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "goodbye_longitudinal.rds"))
saveRDS(combined_model, file.path(MODEL_DIR, "goodbye_combined_study.rds"))

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 9: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  generate_functional_form_tables(
    perf_cs = list(goodbye_action = result_cs$comparison),
    perf_long = list(goodbye_action = result_long$comparison),
    table_prefix = "goodbye",
    table_dir = TABLE_DIR,
    task_name = "Goodbye"
  )

  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = OUTCOME_VAR,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "goodbye",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "goodbye",
    table_dir = TABLE_DIR,
    task_name = "Goodbye"
  )

  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "goodbye_robustness_cs",
    caption = "Goodbye Robustness",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "goodbye_robustness_long",
    caption = "Goodbye Robustness",
    study_type = "longitudinal"
  )

  # Combined study table comparing cross-sectional vs longitudinal
  sjplot_to_latex(
    models = list(combined_model),
    model_labels = c("Study Type Effect"),
    pred_labels = c(
      "(Intercept)" = "Intercept",
      "study_typelongitudinal" = "Longitudinal (vs Cross-Sectional)"
    ),
    filename = "goodbye_combined_study",
    table_dir = TABLE_DIR,
    caption = "Goodbye Action by Study Type --- Logistic Regression",
    dependent_var = "Goodbye Action"
  )

  # Chi-square test table (with breakdown for significant results)
  if (exists("treatment_test_results") && length(treatment_test_results) > 0) {
    create_chisq_with_breakdown_latex(
      test_results = treatment_test_results,
      table_dir = TABLE_DIR,
      filename = "goodbye_chisq",
      caption = "Goodbye: Chi-Square Tests",
      label = "tab:goodbye_chisq"
    )
  }

  # Pairwise contrasts table (emmeans)
  if (exists("pairwise_results") && length(pairwise_results) > 0) {
    create_pairwise_latex(
      pairwise_results = pairwise_results,
      table_dir = TABLE_DIR,
      filename = "goodbye_pairwise",
      caption = "Goodbye: Pairwise Contrasts (FDR-adjusted)",
      label = "tab:goodbye_pairwise",
      is_logistic = TRUE
    )
  }

  generate_parent_tex(
    section_name = "Goodbye",
    constructs = OUTCOME_VAR,
    construct_labels = setNames("Goodbye Action", OUTCOME_VAR),
    func_form_prefix = "goodbye_functional_form",
    main_reg_prefix = "goodbye",
    full_spec_prefix = "goodbye_full_spec",
    robustness_prefix = "goodbye_robustness",
    combined_study_prefix = "goodbye_combined_study",
    table_dir = TABLE_DIR,
    output_filename = "goodbye_parent"
  )
}

# =============================================================================
# SECTION 10: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "07_goodbye.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# Goodbye Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines whether users said goodbye to the AI assistant.",
    "",
    "**Outcome:** `goodbye_action` (binary: 0 = did not say goodbye, 1 = said goodbye)",
    "",
    "**Treatment Arms:**",
    "- **$\\lambda$**: Relationship-seeking intensity (-1 to +1)",
    "- **Domain**: polchat vs emotchat",
    "- **Personalisation**: personalised vs non-personalised",
    "",
    "**Model Type:** Logistic regression (binary outcome, no random effects)",
    "",
    "---",
    ""
  )

  # Data summary by outcome
  lines <- c(lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  # EDA figures
  lines <- c(lines,
    "## Exploratory Data Analysis",
    "",
    paste0("![Goodbye Responses](", fig_rel_path, "/goodbye_responses.png)"),
    "",
    "---",
    ""
  )

  # Treatment association tests
  if (exists("treatment_test_results") && length(treatment_test_results) > 0) {
    cat("  Adding treatment tests...\n")
    # Use non-pooled data (data_cross_sectional/data_longitudinal have original columns)
    data_by_study <- list(
      cross_study = bind_rows(data_cross_sectional, data_longitudinal),
      cross_sectional = data_cross_sectional,
      longitudinal = data_longitudinal
    )
    lines <- c(lines, format_treatment_tests_md(treatment_test_results, data_by_study))
  }

  # Functional form comparison
  if (exists("best_specs_cs") && nrow(best_specs_cs) > 0) {
    lines <- c(lines, generate_best_specs_md(best_specs_cs, best_specs_long))
  }

  # Full-model specification comparison
  if (exists("full_spec_cs") && length(full_spec_cs) > 0) {
    lines <- c(lines, generate_full_spec_comparison_md(
      full_spec_cs, full_spec_long, OUTCOME_VAR))
  }

  # Model coefficients
  if (exists("mods_cross_sectional") && exists("mods_longitudinal")) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines, generate_coefficients_md(
      mods_cross_sectional, mods_longitudinal, OUTCOME_VAR), "", "---", "")
  }

  # Pairwise comparisons
  if (exists("pairwise_results") && length(pairwise_results) > 0) {
    cat("  Adding pairwise comparisons...\n")
    lines <- c(lines, generate_pairwise_md(pairwise_results, OUTCOME_VAR))
  }

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, OUTCOME_VAR, "goodbye"))
  }

  # Combined study model
  if (exists("combined_model") && !is.null(combined_model)) {
    cat("  Adding combined study model...\n")
    lines <- c(lines,
      "## Combined Study Model",
      "",
      "Tests whether goodbye action differs between cross-sectional and",
      "longitudinal studies.",
      ""
    )
    lines <- c(lines, format_coefficients_md(
      combined_model,
      "goodbye_action",
      "Combined Studies"
    ))
    lines <- c(lines, "---", "")
  }

  # Output files
  lines <- c(lines, generate_output_files_md("goodbye"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
