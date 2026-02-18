# =============================================================================
# Seeking Companionship Analysis
# =============================================================================
#
# Analyzes pre-treatment AI companionship-seeking behavior:
# 1. Pre-treatment survey: frequency and products used
# 2. Logistic regression predicting companionship users from demographics
#
# Usage:
#   Rscript scripts/analysis/seeking_companionship.R
#   Rscript scripts/analysis/seeking_companionship.R --generate_tex_tables  # Generate LaTeX tables
#   Rscript scripts/analysis/seeking_companionship.R --report               # Also generate markdown report
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

set.seed(1234)

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

# For backward compatibility with existing code
FIGURES_DIR <- FIGURE_DIR
TABLES_DIR <- TABLE_DIR
REPORTS_DIR <- REPORT_DIR
OUTPUTS_DIR <- file.path(PROJECT_ROOT, "outputs")
GENERATED_DATA_DIR <- GENERATED_DIR

dir.create(file.path(FIGURE_DIR, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURE_DIR, "png"), recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

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
cat("Seeking Companionship Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# =============================================================================
# SECTION 2: DATA LOADING & FORMATTING
# =============================================================================
#
# Creates three types of datasets:
#
# 1. data_merged: Full data with ALL timepoints + ppt_chars
#    - Used for pre-treatment survey analysis (filter to timepoint == "pre")
#    - Multiple rows per participant (up to 2 timepoints)
#
# 2. data_cross_sectional / data_longitudinal: ONE ROW PER PARTICIPANT
#    - Post timepoint values only
#    - {outcome}_pre column for baseline (NA for cross-sectional)
#    - Used for regressions with add_pre = TRUE/FALSE
#
# 3. data_*_pooled: Long format for multi-outcome analyses
#    - outcome_value (post) and outcome_value_pre columns
#
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Define outcome variables
OUTCOME_VARS <- c("seeking_companionship_likelihood")

# -----------------------------------------------------------------------------
# Load raw data and merge participant characteristics
# -----------------------------------------------------------------------------

data_raw <- load_task_data("seeking-companionship", DATA_DIR)

# ppt_chars includes: sociodemos, pref groups, IPW weights, psychosocial factors
ppt_chars <- load_ppt_characteristics(
  DATA_DIR, GENERATED_DIR,
  include_psychosocial = TRUE,
  apply_coarsening = TRUE
)

# data_merged: Full data with all timepoints + ppt_chars (includes IPW weights)
# Used for pre-survey analysis (filter to timepoint == "pre")
data_merged <- prepare_treatment_arms(data_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal")) %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))

cat("Loaded", nrow(data_merged), "rows\n")
cat("Studies:", paste(unique(data_merged$study_id), collapse = ", "), "\n")
cat("Timepoints:", paste(unique(data_merged$timepoint), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# CROSS-SECTIONAL: One row per ppt, post only, _pre = NA
# -----------------------------------------------------------------------------

data_cross_sectional <- data_merged %>%
  filter(study_id == "cross-sectional", timepoint == "post") %>%
  mutate(seeking_companionship_likelihood_pre = NA_real_)

# -----------------------------------------------------------------------------
# LONGITUDINAL: One row per ppt, post values with _pre column
# -----------------------------------------------------------------------------

# Get pre values to join
data_long_pre <- data_merged %>%
  filter(study_id == "longitudinal", timepoint == "pre") %>%
  select(ppt_id, all_of(OUTCOME_VARS)) %>%
  rename_with(~ paste0(.x, "_pre"), all_of(OUTCOME_VARS))

# Post values (one row per ppt) + join pre values
data_longitudinal <- data_merged %>%
  filter(study_id == "longitudinal", timepoint == "post") %>%
  left_join(data_long_pre, by = "ppt_id")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

cat("\nFormatted datasets (one row per participant):\n")
cat("  Cross-sectional:", nrow(data_cross_sectional), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "participants\n")
cat("  Longitudinal with pre values:",
    sum(!is.na(data_longitudinal$seeking_companionship_likelihood_pre)), "\n")

# -----------------------------------------------------------------------------
# POOLED data (long format for multi-outcome analyses)
# -----------------------------------------------------------------------------

cat("\n--- Creating Pooled Data ---\n\n")

data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional, OUTCOME_VARS, include_pre = TRUE
)

data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal, OUTCOME_VARS, include_pre = TRUE
)

cat("  Cross-sectional pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Longitudinal pooled:", nrow(data_longitudinal_pooled), "rows\n")

# =============================================================================
# SECTION 3: PRE-TREATMENT SURVEY ANALYSIS
# =============================================================================

cat("--- Pre-Treatment Survey Analysis ---\n")

# Filter to pre-treatment longitudinal data (from data_merged which has all timepoints)
# We only conduct this extended pre-treatment survey for the longitudinal study.
pre_data <- data_merged %>%
  filter(study_id == "longitudinal", timepoint == "pre")

cat("Pre-treatment longitudinal participants:", n_distinct(pre_data$ppt_id), "\n")

# Define custom ordering for companionship frequency
freq_order <- c(
  "Never", "Less than once a month", "Once a month",
  "More than once a month", "Every week", "Every day"
)

# Process frequency data
freq_data <- pre_data$companionship_freq[!is.na(pre_data$companionship_freq)]
freq_counts <- table(freq_data)
cat("\nCompanionship Frequency Counts:\n")
print(freq_counts)

# Reorder according to custom order (only include categories that exist)
ordered_freq <- freq_order[freq_order %in% names(freq_counts)]
freq_counts_ordered <- freq_counts[ordered_freq]

# Generate colors for frequency bars - grey for "Never", red gradient for others
freq_colors <- c()
for (i in seq_along(freq_counts_ordered)) {
  category <- names(freq_counts_ordered)[i]
  if (category == "Never") {
    freq_colors <- c(freq_colors, "grey")
  } else {
    non_never_categories <- names(freq_counts_ordered)[
      names(freq_counts_ordered) != "Never"
    ]
    non_never_count <- length(non_never_categories)
    if (non_never_count > 1) {
      red_index <- which(non_never_categories == category) - 1
      red_intensity <- 0.4 + 0.6 * red_index / max(1, non_never_count - 1)
      freq_colors <- c(freq_colors, rgb(red_intensity, 0, 0))
    } else {
      freq_colors <- c(freq_colors, rgb(0.7, 0, 0))
    }
  }
}

# Create data frame for frequency plot
freq_df <- data.frame(
  category = factor(names(freq_counts_ordered),
                    levels = rev(names(freq_counts_ordered))),
  count = as.numeric(freq_counts_ordered),
  color = rev(freq_colors)
)

total_unique_ppts <- length(unique(pre_data$ppt_id))

# Create frequency plot
p1 <- ggplot(freq_df, aes(x = count, y = category)) +
  geom_col(fill = freq_colors, alpha = 0.8, width = 0.8) +
  geom_text(aes(label = paste0(round(count / length(freq_data) * 100), "%")),
            hjust = -0.1, size = 6, fontface = "bold") +
  labs(
    title = paste0(
      "Frequency of AI use for companionship, emotional support,\n",
      "or social interaction in the past year (N = ",
      format(total_unique_ppts, big.mark = ","), ")"
    ),
    x = "Number of Participants",
    y = ""
  ) +
  theme_pub() +
  theme(plot.title = element_text(size = rel(1.05))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)))

# Filter for participants who did NOT respond "Never"
non_never_data <- pre_data[
  pre_data$companionship_freq != "Never" &
    !is.na(pre_data$companionship_freq),
]

cat("\nParticipants who use AI for companionship:",
    length(unique(non_never_data$ppt_id)), "\n")

# Handle pipe-separated strings for products
products_df <- non_never_data %>%
  select(companionship_products) %>%
  filter(!is.na(companionship_products)) %>%
  separate_rows(companionship_products, sep = " \\| ") %>%
  mutate(companionship_products = trimws(companionship_products)) %>%
  filter(!is.na(companionship_products) & companionship_products != "")

products_counts <- table(products_df$companionship_products)
cat("\nCompanionship Products Counts (excluding 'Never' respondents):\n")
print(products_counts)

# Sort products by frequency (descending)
products_counts_sorted <- sort(products_counts, decreasing = TRUE)

# Generate red colors for product bars
n_products <- length(products_counts_sorted)
product_colors <- sapply(0:(n_products - 1), function(i) {
  red_intensity <- 0.4 + 0.6 * i / max(1, n_products - 1)
  rgb(red_intensity, 0, 0)
})

# Clean up product labels
product_labels <- names(products_counts_sorted)
product_labels <- gsub("__other", "Other", product_labels)
product_labels <- gsub(
  "AI companions designed specifically for relationships/companionship",
  "AI designed specifically for relationships/companionship",
  product_labels
)
product_labels_formatted <- sapply(product_labels, function(label) {
  if (grepl("\\(", label)) {
    gsub("\\s*\\(", "\n(", label)
  } else {
    label
  }
})

# Create products plot data frame
products_df_plot <- data.frame(
  category = factor(product_labels_formatted, levels = product_labels_formatted),
  count = as.numeric(products_counts_sorted),
  color = rev(product_colors)
)

non_never_unique_ppts <- length(unique(non_never_data$ppt_id))

# Create products plot
p2 <- ggplot(products_df_plot, aes(x = count, y = category)) +
  geom_col(fill = rev(product_colors), alpha = 0.8, width = 0.8) +
  geom_text(aes(label = paste0(round(count / nrow(products_df) * 100), "%")),
            hjust = -0.1, size = 6, fontface = "bold") +
  labs(
    title = paste0(
      "Types of AI used for companionship, emotional support,\n",
      "or social interaction over the past year (N = ",
      format(non_never_unique_ppts, big.mark = ","), "*)"
    ),
    x = "Number of Participants",
    y = ""
  ) +
  theme_pub() +
  theme(plot.title = element_text(size = rel(1.05))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)))

cat("\n--- Saving Pre-Treatment Survey Figure ---\n")

# Combine plots
combined_plot <- p1 / plot_spacer() / p2 + plot_layout(heights = c(1, 0.0, 1))

# Save figure
ggsave(file.path(FIGURES_DIR, "pdf/seeking_companionship_pre_survey.pdf"),
       combined_plot, width = 16, height = 9)
ggsave(file.path(FIGURES_DIR, "png/seeking_companionship_pre_survey.png"),
       combined_plot, width = 16, height = 9, dpi = 300)

cat("Saved: seeking_companionship_pre_survey.pdf/png\n")

# =============================================================================
# SECTION 3B: DUMBBELL PLOT - LIKELIHOOD BY FREQUENCY
# =============================================================================

cat("\n--- Creating Dumbbell Plot ---\n")

# Calculate means and standard errors for each frequency group
freq_summary <- pre_data %>%
  filter(!is.na(companionship_freq) & !is.na(seeking_companionship_likelihood)) %>%
  group_by(companionship_freq) %>%
  summarise(
    n = n(),
    mean_likelihood = mean(seeking_companionship_likelihood, na.rm = TRUE),
    se_likelihood = sd(seeking_companionship_likelihood, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    lower_ci = mean_likelihood - 1.96 * se_likelihood,
    upper_ci = mean_likelihood + 1.96 * se_likelihood
  )

# Order by the custom frequency order
freq_summary <- freq_summary %>%
  filter(companionship_freq %in% freq_order) %>%
  mutate(companionship_freq = factor(companionship_freq, levels = rev(freq_order)))

# Create the dumbbell plot
# Calculate x position for n labels (outside aes to avoid warning)
label_x_pos <- max(freq_summary$upper_ci) * 1.05

p_dumbbell <- ggplot(freq_summary, aes(y = companionship_freq)) +
  geom_segment(
    aes(x = lower_ci, xend = upper_ci,
        y = companionship_freq, yend = companionship_freq),
    color = "darkgray", linewidth = 1
  ) +
  geom_point(aes(x = lower_ci), color = "red", size = 3, alpha = 0.8) +
  geom_point(aes(x = upper_ci), color = "red", size = 3, alpha = 0.8) +
  geom_point(aes(x = mean_likelihood), color = "darkred", size = 4) +
  geom_text(
    aes(x = mean_likelihood, label = sprintf("%.1f", mean_likelihood)),
    vjust = -0.8, size = 5, fontface = "bold"
  ) +
  geom_text(
    aes(label = paste0("n=", n)), x = label_x_pos,
    hjust = 0, size = 6, color = "gray50"
  ) +
  labs(
    x = "Pre-Treatment Companionship-Seeking Likelihood (Mean ± 95% CI)",
    y = "Current AI\nCompanionship Frequency"
  ) +
  theme_pub() +
  theme(
    plot.margin = margin(t = 40, l = 20, b = 10),
    axis.title.y = element_text(margin = margin(r = 35))
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))

ggsave(file.path(FIGURES_DIR, "pdf/seeking_companionship_by_freq_pre_survey.pdf"),
       p_dumbbell, width = 18, height = 5)
ggsave(file.path(FIGURES_DIR, "png/seeking_companionship_by_freq_pre_survey.png"),
       p_dumbbell, width = 18, height = 5, dpi = 300)

cat("Saved: seeking_companionship_by_freq_pre_survey.pdf/png\n")

# =============================================================================
# SECTION 3C: RAW LAMBDA PLOT
# =============================================================================

cat("\n--- Creating Raw Lambda Plot ---\n")

p_lambda <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  height = 4, width = 8
)
save_plot(FIGURES_DIR, p_lambda, "seeking_companionship_by_lambda", 8, 4)
cat("Saved: seeking_companionship_by_lambda.pdf/png\n")

# =============================================================================
# SECTION 3D: PRE-POST CHANGE PLOT
# =============================================================================

cat("\n--- Creating Pre-Post Change Plot ---\n")

# data_longitudinal already has wide format with _pre column from Section 2
p_pre_post <- plot_pre_post_change_combined(
  data = data_longitudinal,
  outcome_vars = "seeking_companionship_likelihood",
  facet_var = "study_id",
  total_height = 6,
  total_width = 14,
  figure_dir = FIGURES_DIR,
  save_pdf = TRUE,
  filename_prefix = "seeking_companionship"
)

cat("Saved: seeking_companionship_pre_post_combined.pdf/png\n")

# --- Pre-Post Correlation Scatter Plot ---
cat("\n--- Creating Pre-Post Correlation Scatter Plot ---\n")

p_corr <- plot_pre_post_correlations(
  data = data_longitudinal,
  outcome_vars = "seeking_companionship_likelihood",
  total_height = 6,
  total_width = 8,
  figure_dir = FIGURES_DIR,
  save_pdf = TRUE,
  filename_prefix = "seeking_companionship"
)

cat("Saved: seeking_companionship_pre_post_corr.pdf/png\n")


# =============================================================================
# SECTION 3E: LOGISTIC REGRESSION ANALYSIS (ON PRE-TREATMENT SURVEY)
# =============================================================================

cat("\n--- Logistic Regression Analysis (On Pre-Treatment Survey) ---\n")

reg_data <- pre_data %>%
  mutate(companionship_users = ifelse(companionship_freq == "Never", 0, 1))

cat("Companionship users distribution:\n")
print(table(reg_data$companionship_users))
cat("Proportion:", round(mean(reg_data$companionship_users, na.rm = TRUE), 3), "\n\n")

candidate_predictors <- c(
  "age", "education_years", "gender_binary", "disability_binary",
  "ethnicity_binary", "income_binary", "religion_binary",
  "pre_psychosocial_F1", "pre_psychosocial_F2",
  "ai_frequency_coarsened", "cluster_name"
)

# Check all predictors are available
missing_predictors <- setdiff(candidate_predictors, names(reg_data))
if (length(missing_predictors) > 0) {
  stop("Missing predictors for logistic regression: ",
       paste(missing_predictors, collapse = ", "))
}

formula_str <- paste("companionship_users ~",
                     paste(candidate_predictors, collapse = " + "))

pre_treatment_survey_logistic <- glm(
  as.formula(formula_str),
  family = binomial(link = "logit"),
  data = reg_data
)

cat("Model summary:\n")
print(summary(pre_treatment_survey_logistic))

# Filter out intercept and "Prefer not to say" for display
model_params <- model_parameters(pre_treatment_survey_logistic) %>%
  as.data.frame() %>%
  filter(Parameter != "(Intercept)",
         !grepl("Prefer not to say", Parameter, ignore.case = TRUE)) %>%
  mutate(
    p_adjusted = p.adjust(p, method = "fdr"),
    significance = case_when(
      p_adjusted < 0.001 ~ "***",
      p_adjusted < 0.01 ~ "**",
      p_adjusted < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

cat("\nFDR-adjusted results:\n")
print(kable(model_params %>%
              select(Parameter, Coefficient, SE, z, p, p_adjusted, significance),
            digits = 3))

# Get terms to display (exclude intercept and "Prefer not to say")
all_terms <- names(coef(pre_treatment_survey_logistic))
display_terms <- all_terms[
  all_terms != "(Intercept)" &
  !grepl("Prefer not to say", all_terms, ignore.case = TRUE)
]

# Get standardized labels for forest plot axis
# sjPlot's plot_model expects labels in reverse order (bottom to top)
sociodemo_labels <- get_sociodemo_labels()
axis_labels <- sapply(display_terms, function(t) {
  if (t %in% names(sociodemo_labels)) sociodemo_labels[[t]] else t
}, USE.NAMES = FALSE)
axis_labels_rev <- rev(axis_labels)

p_forest <- plot_model(
  pre_treatment_survey_logistic,
  type = "est",
  terms = display_terms,
  axis.labels = axis_labels_rev,
  show.values = TRUE,
  value.offset = 0.3,
  title = "Predictors of AI Companionship Use (Odds Ratios)",
  colors = "bw",
  vline.color = "grey50"
) +
  theme_pub() +
  theme(plot.title = element_text(size = 12, face = "bold"))

ggsave(file.path(FIGURES_DIR, "pdf/seeking_companionship_forest.pdf"),
       p_forest, width = 10, height = 8)
ggsave(file.path(FIGURES_DIR, "png/seeking_companionship_forest.png"),
       p_forest, width = 10, height = 8, dpi = 300)
cat("Saved: seeking_companionship_forest.pdf/png\n")

# Save model
saveRDS(pre_treatment_survey_logistic,
        file.path(MODEL_DIR, "seeking_companionship_pre_survey_model.rds"))
cat("Saved: seeking_companionship_pre_survey_model.rds\n")

# =============================================================================
# SECTION 4: POST-TREATMENT SURVEY PLOT (2 PANELS)
# =============================================================================

cat("\n--- Creating Post-Treatment Survey Plot ---\n")

# Get post-treatment data for both studies
post_data <- data_merged %>%
  filter(timepoint == "post")

cat("Post-treatment participants:", n_distinct(post_data$ppt_id), "\n")

# Perceived change in seeking companionship likelihood
change_var <- "perceived_change_seeking_companionship_likelihood"
if (change_var %in% names(post_data)) {

  # Define order for change categories (short labels)
  change_order <- c(
    "Much less likely than before",
    "Slightly less likely than before",
    "No change from before",
    "Slightly more likely than before",
    "Much more likely than before"
  )

  # Recode to shorter labels
  post_data <- post_data %>%
    mutate(
      change_recoded = case_when(
        .data[[change_var]] == "No change from before" ~ "No change",
        .data[[change_var]] %in% c(
          "Slightly more likely than before",
          "Much more likely than before"
        ) ~ "More likely",
        .data[[change_var]] %in% c(
          "Slightly less likely than before",
          "Much less likely than before"
        ) ~ "Less likely",
        TRUE ~ NA_character_
      )
    )

  change_levels <- c("Less likely", "No change", "More likely")

  # --- Panel 1: By Study ---
  p_by_study <- plot_categorical_by_group(
    data = post_data,
    response_var = "change_recoded",
    group_var = "study_id",
    response_levels = change_levels,
    title = "Perceived Change in Companionship-Seeking\n(by Study)"
  ) +
    labs(x = "Percentage of Participants") +
    theme(plot.title = element_text(size = rel(1.0)))

  # --- Panel 2: By RS Category (longitudinal only) ---
  rs_colors <- c(
    "pos_lambda" = "#b40426",
    "zero_lambda" = "#918f8f",
    "neg_lambda" = "#3b4cc0"
  )
  rs_labels <- c(
    "pos_lambda" = "pos lambda",
    "zero_lambda" = "zero lambda",
    "neg_lambda" = "neg lambda"
  )

  # Filter to longitudinal only and valid RS categories
  post_data_long_rs <- post_data %>%
    filter(study_id == "longitudinal") %>%
    filter(relationship_seeking_category %in% c("pos_lambda", "zero_lambda", "neg_lambda")) %>%
    mutate(relationship_seeking_category = factor(
      relationship_seeking_category,
      levels = c("pos_lambda", "zero_lambda", "neg_lambda")
    ))

  p_by_rs <- plot_categorical_by_group(
    data = post_data_long_rs,
    response_var = "change_recoded",
    group_var = "relationship_seeking_category",
    response_levels = change_levels,
    title = "Perceived Change in Companionship-Seeking\n(by Lambda Category)",
    subtitle = "Longitudinal study only",
    colors = rs_colors,
    labels = rs_labels
  ) +
    labs(x = "Percentage of Participants") +
    theme(plot.title = element_text(size = rel(1.0)))

  # Combine panels
  p_post_combined <- p_by_study | p_by_rs

  ggsave(
    file.path(FIGURES_DIR, "pdf/seeking_companionship_post_survey.pdf"),
    p_post_combined, width = 16, height = 6
  )
  ggsave(
    file.path(FIGURES_DIR, "png/seeking_companionship_post_survey.png"),
    p_post_combined, width = 16, height = 6, dpi = 300
  )

  cat("Saved: seeking_companionship_post_survey.pdf/png\n")

  # --- Treatment Tests for Categorical Variable ---
  cat("\nRunning treatment tests for perceived change variable...\n")

  # Run comprehensive treatment tests
  post_categorical_test_results <- run_standard_treatment_tests(
    data_stats = post_data,
    cross_study_outcomes = "change_recoded",
    longitudinal_outcomes = NULL
  )

  # Save results
  write_json(post_categorical_test_results,
          file.path(STATS_DIR, "seeking_companionship_categorical_tests.json"), pretty = TRUE)
  cat("Saved: seeking_companionship_categorical_tests.json\n")
}

# =============================================================================
# SECTION 5: PRE_POST ANALYSIS (MAIN ANALYSIS)
# =============================================================================
#
# The PRE_POST analysis (longitudinal only, controlling for baseline) is our
# KEY measure in the paper. data_longitudinal already has the required format
# one row per ppt with _pre column, ppt_chars, IPW weights).
#
# =============================================================================

cat("\n--- PRE_POST Analysis (Main) ---\n\n")

OUTCOME_VAR <- "seeking_companionship_likelihood"

cat("PRE_POST sample size:", nrow(data_longitudinal), "participants\n")
cat("Pre values available:",
    sum(!is.na(data_longitudinal$seeking_companionship_likelihood_pre)), "\n")
cat("Post values available:",
    sum(!is.na(data_longitudinal$seeking_companionship_likelihood)), "\n")

# =============================================================================
# SECTION 5A: PRE_POST FUNCTIONAL FORM COMPARISON (MAIN ANALYSIS)
# =============================================================================

cat("\n--- PRE_POST Functional Form Comparison ---\n\n")

ff_result_prepost <- compare_functional_forms(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_pre = TRUE,
  add_time = FALSE,
  model_family = "ols",
  select_by = "AIC"
)
best_spec_prepost <- ff_result_prepost$best_spec
cat(sprintf("  Best specification: %s\n", best_spec_prepost))
print(ff_result_prepost$comparison)

# Save functional form comparison for later table generation
perf_prepost <- list()
perf_prepost[[OUTCOME_VAR]] <- ff_result_prepost$comparison

# =============================================================================
# SECTION 5B: PRE_POST MODEL FITTING & REGRESSION TABLES (MAIN ANALYSIS)
# =============================================================================

cat("\n--- PRE_POST Model Fitting ---\n\n")

mods_prepost <- fit_models(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_prepost,
  add_domain = TRUE,
  add_pre = TRUE, # add pre values as covariate
  add_time = FALSE,
  model_family = "ols"
)

# Create list structure for table generation
mods_prepost_list <- list()
mods_prepost_list[[OUTCOME_VAR]] <- mods_prepost

best_specs_prepost_df <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_prepost,
  stringsAsFactors = FALSE
)

# Print coefficients
cat("\n--- PRE_POST Model Coefficients ---\n")
print_model_coefficients_section(
  mods_cross_sectional = NULL,
  mods_longitudinal = mods_prepost_list,
  outcome_vars = OUTCOME_VAR
)

# =============================================================================
# SECTION 5D: PRE_POST ROBUSTNESS ANALYSIS (MAIN ANALYSIS)
# =============================================================================

cat("\n--- PRE_POST Robustness Analysis ---\n\n")

robustness_prepost <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VAR,
  rs_variable = "lambda",
  continuous_spec = best_spec_prepost,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_pre = TRUE,  # Control for pre-treatment value
  add_time = FALSE,
  use_weights = TRUE,
  model_family = "ols"
)

# =============================================================================
# SECTION 5E: PRE_POST PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- PRE_POST Full-Model Performance Comparison ---\n\n")

full_spec_results_prepost <- compute_full_spec_comparison(
  mods_cs = NULL,
  mods_long = mods_prepost_list,
  outcome_vars = OUTCOME_VAR
)
full_spec_prepost <- full_spec_results_prepost$full_spec_long

# =============================================================================
# SECTION 6: POST_ONLY ANALYSIS (SUPPLEMENTARY)
# =============================================================================
#
# Supplementary analysis using post-treatment values only (no baseline control).
# Includes both cross-sectional and longitudinal studies.
#
# =============================================================================

cat("\n--- POST_ONLY Analysis (Supplementary) ---\n\n")
# data_cross_sectional and data_longitudinal are already post-only from Section 2
# For POST_ONLY analysis, we use these directly with add_pre = FALSE

cat("POST_ONLY sample sizes:\n")
cat("  Cross-sectional:", nrow(data_cross_sectional), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "participants\n")

# Cross-sectional functional form comparison
cat("\nCross-sectional functional form comparison (POST_ONLY):\n")
ff_result_cs <- compare_functional_forms(
  data = data_cross_sectional,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_time = FALSE,
  model_family = "ols",
  select_by = "AIC"
)
best_spec_cs <- ff_result_cs$best_spec
cat(sprintf("  Best specification: %s\n", best_spec_cs))

# Longitudinal functional form comparison (POST_ONLY)
cat("\nLongitudinal functional form comparison (POST_ONLY):\n")
ff_result_long_postonly <- compare_functional_forms(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_time = FALSE,
  model_family = "ols",
  select_by = "AIC"
)
best_spec_long_postonly <- ff_result_long_postonly$best_spec
cat(sprintf("  Best specification: %s\n", best_spec_long_postonly))

# Save POST_ONLY functional form results for later table generation
perf_cs_postonly <- list()
perf_cs_postonly[[OUTCOME_VAR]] <- ff_result_cs$comparison
perf_long_postonly <- list()
perf_long_postonly[[OUTCOME_VAR]] <- ff_result_long_postonly$comparison

# POST_ONLY model fitting
cat("\nPOST_ONLY model fitting:\n")

# Cross-sectional models
mods_cross_sectional <- fit_models(
  data = data_cross_sectional,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_cs,
  add_domain = TRUE,
  add_pre = FALSE,
  add_time = FALSE,
  model_family = "ols"
)

# Longitudinal models (POST_ONLY)
mods_longitudinal_postonly <- fit_models(
  data = data_longitudinal,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_long_postonly,
  add_domain = TRUE,
  add_pre = FALSE,
  add_time = FALSE,
  model_family = "ols"
)

# Create list structures
mods_cs_list <- list()
mods_cs_list[[OUTCOME_VAR]] <- mods_cross_sectional

mods_long_postonly_list <- list()
mods_long_postonly_list[[OUTCOME_VAR]] <- mods_longitudinal_postonly

best_specs_cs_df <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_cs,
  stringsAsFactors = FALSE
)

best_specs_long_postonly_df <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_long_postonly,
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# POST_ONLY Robustness Analysis
# -----------------------------------------------------------------------------

cat("\nPOST_ONLY robustness analysis:\n")

cat("  Cross-sectional:\n")
robustness_cs <- run_robustness_analysis(
  data = data_cross_sectional,
  outcome_vars = OUTCOME_VAR,
  rs_variable = "lambda",
  continuous_spec = best_spec_cs,
  study_type = "cross_sectional",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = TRUE, # Seeking companionship is measured in exit study
  model_family = "ols"
)

cat("  Longitudinal:\n")
robustness_long_postonly <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VAR,
  rs_variable = "lambda",
  continuous_spec = best_spec_long_postonly,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = TRUE,
  model_family = "ols"
)

# -----------------------------------------------------------------------------
# POST_ONLY Performance Comparison
# -----------------------------------------------------------------------------

cat("\nPOST_ONLY full-model performance comparison:\n")

full_spec_results_postonly <- compute_full_spec_comparison(
  mods_cs = mods_cs_list,
  mods_long = mods_long_postonly_list,
  outcome_vars = OUTCOME_VAR
)
full_spec_cs_postonly <- full_spec_results_postonly$full_spec_cs
full_spec_long_postonly <- full_spec_results_postonly$full_spec_long

# =============================================================================
# SECTION 7: COMBINED STUDY MODEL (POST-ONLY)
# =============================================================================

combined_model <- run_combined_study_analysis(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_var = "seeking_companionship_likelihood",
  model_family = "ols"
)


# =============================================================================
# SECTION 8: SAVE
# =============================================================================

cat("\n--- Saving Models ---\n")

# Save main PRE_POST model (primary analysis)
saveRDS(mods_prepost_list, file.path(MODEL_DIR, "seeking_companionship_longitudinal.rds"))
cat("  Saved: seeking_companionship_longitudinal.rds (PRE_POST, main analysis)\n")

# Save POST_ONLY models (supplementary)
saveRDS(mods_cs_list, file.path(MODEL_DIR, "seeking_companionship_postonly_cross_sectional.rds"))
saveRDS(mods_long_postonly_list, file.path(MODEL_DIR, "seeking_companionship_postonly_longitudinal.rds"))
cat("  Saved: seeking_companionship_postonly_cross_sectional.rds (supplementary)\n")
cat("  Saved: seeking_companionship_postonly_longitudinal.rds (supplementary)\n")

# Save combined study model
saveRDS(combined_model, file.path(MODEL_DIR, "seeking_companionship_combined_study.rds"))
cat("  Saved: seeking_companionship_combined_study.rds\n")

# Save pooled data for contrast analysis (has construct column from create_pooled_data)
seeking_companionship_data <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
)
saveRDS(seeking_companionship_data, file.path(MODEL_DIR, "seeking_companionship_data.rds"))
cat("  Saved: seeking_companionship_data.rds\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 9: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n")

  # -------------------------------------------------------------------------
  # PRE_POST Tables (Main Analysis)
  # -------------------------------------------------------------------------

  cat("\n  PRE_POST functional form table...\n")
  generate_functional_form_tables(
    perf_cs = NULL,
    perf_long = perf_prepost,
    table_prefix = "seeking_companionship",
    table_dir = TABLES_DIR,
    task_name = "Seeking Companionship (Pre-Post)"
  )

  cat("  PRE_POST regression tables...\n")
  generate_regression_tables(
    mods_cs = NULL,
    mods_long = mods_prepost_list,
    outcome_vars = OUTCOME_VAR,
    best_specs_cs = NULL,
    best_specs_long = best_specs_prepost_df,
    table_prefix = "seeking_companionship",
    table_dir = TABLES_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  cat("  PRE_POST full-spec comparison table...\n")
  generate_full_spec_tables(
    full_spec_cs = NULL,
    full_spec_long = full_spec_prepost,
    table_prefix = "seeking_companionship",
    table_dir = TABLES_DIR,
    task_name = "Seeking Companionship (Pre-Post)"
  )

  cat("  PRE_POST robustness table...\n")
  create_robustness_latex_table(
    wide_df = robustness_prepost$coeffs_wide,
    table_dir = TABLES_DIR,
    filename_prefix = "seeking_companionship_robustness_long",
    caption = "Seeking Companionship Robustness (Pre-Post)",
    study_type = "longitudinal"
  )

  # -------------------------------------------------------------------------
  # POST_ONLY Tables (Supplementary)
  # -------------------------------------------------------------------------

  cat("\n  POST_ONLY functional form table...\n")
  generate_functional_form_tables(
    perf_cs = perf_cs_postonly,
    perf_long = perf_long_postonly,
    table_prefix = "seeking_companionship_postonly",
    table_dir = TABLES_DIR,
    task_name = "Seeking Companionship (Post-Only)"
  )

  cat("  POST_ONLY regression tables...\n")
  generate_regression_tables(
    mods_cs = mods_cs_list,
    mods_long = mods_long_postonly_list,
    outcome_vars = OUTCOME_VAR,
    best_specs_cs = best_specs_cs_df,
    best_specs_long = best_specs_long_postonly_df,
    table_prefix = "seeking_companionship_postonly",
    table_dir = TABLES_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  cat("  POST_ONLY full-spec comparison table...\n")
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs_postonly,
    full_spec_long = full_spec_long_postonly,
    table_prefix = "seeking_companionship_postonly",
    table_dir = TABLES_DIR,
    task_name = "Seeking Companionship (Post-Only)"
  )

  cat("  POST_ONLY robustness tables...\n")
  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLES_DIR,
    filename_prefix = "seeking_companionship_postonly_robustness_cs",
    caption = "Seeking Companionship Robustness (Post-Only)",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long_postonly$coeffs_wide,
    table_dir = TABLES_DIR,
    filename_prefix = "seeking_companionship_postonly_robustness_long",
    caption = "Seeking Companionship Robustness (Post-Only)",
    study_type = "longitudinal"
  )

  # -------------------------------------------------------------------------
  # Pre-Treatment Survey Regression Table
  # -------------------------------------------------------------------------

  if (exists("pre_treatment_survey_logistic")) {
    cat("  Pre-treatment survey regression table...\n")
    sjplot_to_latex(
      models = list(pre_treatment_survey_logistic),
      model_labels = c("Odds Ratio"),
      pred_labels = get_sociodemo_labels(),
      filename = "seeking_companionship_pre_survey_regression",
      table_dir = TABLES_DIR,
      caption = paste0(
        "Logistic Regression: Predictors of AI Companionship Use (Pre-Treatment)"
      ),
      dependent_var = "Companionship User (1 = Any use, 0 = Never)",
      fontsize = "footnotesize",
      p.adjust = "fdr",
      show.p = TRUE,
      drop = "Prefer not to say",
      silent = FALSE
    )
  }

  # -------------------------------------------------------------------------
  # Combined Study Table
  # -------------------------------------------------------------------------

  cat("  Combined study table...\n")
  sjplot_to_latex(
    models = list(combined_model),
    model_labels = c("Study Type Effect"),
    pred_labels = c(
      "(Intercept)" = "Intercept",
      "study_typelongitudinal" = "Longitudinal (vs Cross-Sectional)"
    ),
    filename = "seeking_companionship_combined_study",
    table_dir = TABLES_DIR,
    caption = paste0(
      "Seeking Companionship Likelihood by Study Type --- ",
      "OLS Regression (Post-Only)"
    ),
    dependent_var = "Seeking Companionship Likelihood"
  )

  # -------------------------------------------------------------------------
  # Chi-Square Test Table
  # -------------------------------------------------------------------------

  # Chi-square test table (with breakdown for significant results)
  if (exists("post_categorical_test_results") &&
      length(post_categorical_test_results) > 0) {
    create_chisq_with_breakdown_latex(
      test_results = post_categorical_test_results,
      table_dir = TABLES_DIR,
      filename = "seeking_companionship_chisq",
      caption = "Seeking Companionship: Chi-Square Tests",
      label = "tab:seeking_companionship_chisq"
    )
  }

  # -------------------------------------------------------------------------
  # Parent TeX File
  # -------------------------------------------------------------------------

  cat("  Parent TeX file...\n")
  generate_parent_tex(
    section_name = "Seeking Companionship",
    constructs = OUTCOME_VAR,
    construct_labels = setNames("Seeking Companionship Likelihood", OUTCOME_VAR),
    func_form_prefix = "seeking_companionship_functional_form",
    main_reg_prefix = "seeking_companionship",
    full_spec_prefix = "seeking_companionship_full_spec",
    robustness_prefix = "seeking_companionship_robustness",
    postonly_prefix = "seeking_companionship_postonly",
    combined_study_prefix = "seeking_companionship_combined_study",
    main_includes_cs = FALSE,  # Main analysis is PRE_POST (longitudinal only)
    table_dir = TABLES_DIR,
    output_filename = "seeking_companionship_parent"
  )

  cat("\n  All LaTeX tables generated.\n")
}

# =============================================================================
# SECTION 12: GENERATE REPORT (if --report flag)
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Markdown Report ---\n")

  # Create frequency summary for report
  freq_report_df <- data.frame(
    category = names(freq_counts_ordered),
    count = as.numeric(freq_counts_ordered)
  ) %>%
    mutate(percentage = count / sum(count) * 100)

  report_lines <- c(
    "# Seeking Companionship Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines whether exposure to relationship-seeking AI affects",
    "users' likelihood of seeking AI companionship in the future.",
    "",
    "**Outcome:** `seeking_companionship_likelihood` (0-100 scale)",
    "",
    "**Treatment Arms:**",
    "- **$\\lambda$**: Relationship-seeking intensity (-1 to +1)",
    "- **Domain**: polchat vs emotchat",
    "- **Personalisation**: personalised vs non-personalised",
    "",
    "---",
    ""
  )

  # Data summary
  report_lines <- c(report_lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  report_lines <- c(report_lines,
    "## Pre-Treatment Survey: AI Companionship Use",
    "",
    "This analysis examines pre-treatment patterns of AI use for companionship, emotional support, or social interaction among longitudinal study participants.",
    "",
    "### Frequency of AI Companionship Use",
    "",
    sprintf("**Total participants:** %d", sum(freq_report_df$count)),
    "",
    "| Frequency | N | % |",
    "|-----------|---|---|"
  )

  for (i in seq_len(nrow(freq_report_df))) {
    report_lines <- c(report_lines, sprintf(
      "| %s | %d | %.1f%% |",
      freq_report_df$category[i],
      freq_report_df$count[i],
      freq_report_df$percentage[i]
    ))
  }

  report_lines <- c(report_lines, "", "### Products Used for AI Companionship", "")

  if (length(products_counts_sorted) > 0) {
    # Create products report dataframe
    products_report_df <- data.frame(
      product = names(products_counts_sorted),
      count = as.numeric(products_counts_sorted)
    ) %>%
      mutate(
        product = gsub("__other", "Other", product),
        percentage = count / sum(count) * 100
      )

    report_lines <- c(report_lines,
      sprintf("**Participants with any AI companionship use:** %d",
              non_never_unique_ppts),
      "",
      "| Product Type | N | % |",
      "|--------------|---|---|"
    )

    for (i in seq_len(nrow(products_report_df))) {
      report_lines <- c(report_lines, sprintf(
        "| %s | %d | %.1f%% |",
        products_report_df$product[i],
        products_report_df$count[i],
        products_report_df$percentage[i]
      ))
    }
  }

  report_lines <- c(report_lines,
    "",
    paste0(
      "![Pre-treatment survey]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_pre_survey.png)"
    ),
    "",
    "### Likelihood by Current Frequency",
    "",
    paste0(
      "![Dumbbell plot]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_by_freq_pre_survey.png)"
    ),
    ""
  )

  # Add regression section within Pre-Treatment Survey if available
  if (exists("pre_treatment_survey_logistic")) {
    report_lines <- c(report_lines,
      "### Predictors of AI Companionship Use",
      "",
      "Logistic regression predicting any AI companionship use (vs Never).",
      ""
    )

    # Add formula and odds ratios table
    or_lines <- format_logistic_or_md(pre_treatment_survey_logistic, model_params)
    report_lines <- c(report_lines, or_lines)

    report_lines <- c(report_lines,
      "#### Forest Plot",
      "",
      paste0(
        "![Forest plot]",
        "(../../outputs/figures/main_studies/png/",
        "seeking_companionship_forest.png)"
      ),
      ""
    )
  }

  report_lines <- c(report_lines,
    "---",
    "",
    "## EDA",
    "",
    "### Outcome by Lambda",
    "",
    paste0(
      "![By Lambda]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_by_lambda.png)"
    ),
    "",
    "### Pre-Post Change",
    "",
    paste0(
      "![Pre-post change]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_pre_post_combined.png)"
    ),
    "",
    "### Pre-Post Correlation",
    "",
    paste0(
      "![Pre-post correlation]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_pre_post_corr.png)"
    ),
    "",
    "### Post-Treatment Survey",
    "",
    paste0(
      "![Post-treatment survey]",
      "(../../outputs/figures/main_studies/png/",
      "seeking_companionship_post_survey.png)"
    ),
    ""
  )

  # Add categorical test results if they exist
  if (exists("post_categorical_test_results")) {
    report_lines <- c(report_lines,
      "### Chi-Square Tests for Perceived Change",
      ""
    )
    data_by_study <- list(
      cross_study = post_data,
      cross_sectional = post_data %>% filter(study_id == "cross-sectional"),
      longitudinal = post_data %>% filter(study_id == "longitudinal")
    )
    report_lines <- c(report_lines,
      format_treatment_tests_md(post_categorical_test_results, data_by_study))
  }

  # --- Treatment Effect Analysis Section ---
  report_lines <- c(report_lines,
    "---",
    "",
    "## Treatment Effect Analysis",
    "",
    "### Functional Form Comparison",
    "",
    "**PRE_POST Analysis** (Main - longitudinal only, controlling for pre):",
    "",
    sprintf("- **Best spec:** %s", best_spec_prepost),
    "",
    "**POST_ONLY Analysis** (Supplementary - post-treatment values only):",
    "",
    sprintf("- **Cross-sectional best spec:** %s", best_spec_cs),
    sprintf("- **Longitudinal best spec:** %s", best_spec_long_postonly),
    ""
  )

  # PRE_POST coefficients (Main Analysis)
  # Use full_continuous model (functional form is baked into continuous_spec)
  report_lines <- c(report_lines,
    "### PRE_POST Model Results (Main Analysis)",
    "",
    sprintf("Best specification: %s. Controlling for pre-treatment value.",
            best_spec_prepost),
    ""
  )

  if (!is.null(mods_prepost$full_continuous)) {
    report_lines <- c(report_lines,
      format_coefficients_md(
        mods_prepost$full_continuous, OUTCOME_VAR, "Longitudinal (PRE_POST)"
      )
    )
  }

  # POST_ONLY coefficients (Supplementary)
  report_lines <- c(report_lines,
    "### POST_ONLY Model Results (Supplementary)",
    "",
    "Post-treatment values only (no baseline control).",
    ""
  )

  # Cross-sectional
  if (!is.null(mods_cross_sectional$full_continuous)) {
    report_lines <- c(report_lines,
      sprintf("**Cross-sectional** (best spec: %s):", best_spec_cs),
      "",
      format_coefficients_md(
        mods_cross_sectional$full_continuous, OUTCOME_VAR, "Cross-sectional"
      )
    )
  }

  # Longitudinal POST_ONLY
  if (!is.null(mods_longitudinal_postonly$full_continuous)) {
    report_lines <- c(report_lines,
      sprintf("**Longitudinal** (best spec: %s):", best_spec_long_postonly),
      "",
      format_coefficients_md(
        mods_longitudinal_postonly$full_continuous, OUTCOME_VAR,
        "Longitudinal (POST_ONLY)"
      )
    )
  }

  # Combined study model
  if (exists("combined_model") && !is.null(combined_model)) {
    report_lines <- c(report_lines,
      "### Combined Study Model",
      "",
      "Tests whether seeking companionship likelihood differs between",
      "cross-sectional and longitudinal studies.",
      ""
    )
    report_lines <- c(report_lines, format_coefficients_md(
      combined_model,
      "seeking_companionship_likelihood",
      "Combined Studies"
    ))
  }

  report_lines <- c(report_lines, "---", "")

  # Output files
  report_lines <- c(report_lines,
    generate_output_files_md("seeking_companionship")
  )

  writeLines(report_lines, file.path(REPORTS_DIR, "08_seeking_companionship.md"))
  cat("Saved: reports/main_studies/08_seeking_companionship.md\n")
}