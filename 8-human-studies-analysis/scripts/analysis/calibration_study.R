#!/usr/bin/env Rscript
# =============================================================================
# Calibration Study Analysis
# =============================================================================
#
# Regression analysis of calibration study data:
# - Rating outcomes: coherence, relationship-seeking, preference (lmer models)
# - Ranking outcomes: preference, relationship-seeking (PlackettLuce models)

# Usage:
#   Rscript scripts/analysis/calibration_study.R           
#   Rscript scripts/analysis/calibration_study.R  --generate_report # Also generate a markdown report
#   Rscript scripts/analysis/calibration_study.R --generate_tex_tables # Also generate LaTeX tables
#   Rscript scripts/analysis/calibration_study.R --bootstrap_n=10  # Quick iteration (10 bootstrap samples, default is 100)
# Note bootstrap n only affects the winrate analysis plots which are supplementary
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(lme4)
library(lmerTest)
library(patchwork)
library(sjPlot)
library(RColorBrewer)
library(parameters)
library(future.apply)

set.seed(1234)

# Set up parallel processing for bootstrap
n_workers <- max(1, availableCores() - 1)
plan(multisession, workers = n_workers)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
generate_tex_tables <- "--generate_tex_tables" %in% args
generate_report <- "--generate_report" %in% args || "--report" %in% args

# Extract bootstrap_n if provided
bootstrap_n <- 100  # Default
bootstrap_arg <- grep("--bootstrap_n=", args, value = TRUE)
if (length(bootstrap_arg) > 0) {
  bootstrap_n <- as.integer(gsub("--bootstrap_n=", "", bootstrap_arg))
}

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
REPO_ROOT <- paths$REPO_ROOT

# Calibration study-specific output directories
FIGURE_DIR <- file.path(PROJECT_ROOT, "outputs/figures/calibration_study")
TABLE_DIR <- file.path(PROJECT_ROOT, "outputs/tables/calibration_study")
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs/models/calibration_study")
REPORT_DIR <- file.path(PROJECT_ROOT, "reports/calibration_study")
DATA_DIR <- paths$CALIBRATION_DATA_DIR

dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURE_DIR, "png"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURE_DIR, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(TABLE_DIR, "tex_tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# Source utility functions
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/calibration_regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/calibration_plot_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/labelling_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Calibration Study Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("Bootstrap samples:", bootstrap_n, "\n")
cat("Parallel workers:", n_workers, "\n")
cat("Generate LaTeX tables:", generate_tex_tables, "\n")
cat("Generate report:", generate_report, "\n\n")

# Define outcome variables
RATING_OUTCOMES <- c("coherence_mean", "relationship_seeking_mean", "preference_mean")
RANKING_OUTCOMES <- c("relationship-seeking", "preference")

# Individual item columns for EDA
COHERENCE_ITEMS <- c(
  "coherence:confusing-clear",
  "coherence:grammatically_incorrect-grammatically_correct",
  "coherence:incoherent-coherent"
)
COHERENCE_LABELS <- c("Confusing-Clear", "Incorrect-Correct Grammar", "Incoherent-Coherent")

RS_ITEMS <- c(
  "steerability:cold-warm",
  "steerability:impersonal-personal",
  "steerability:insensitive-sensitive",
  "steerability:unsociable-sociable",
  "steerability:robot-human",
  "steerability:tool-friend"
)
RS_LABELS <- c("Cold-Warm", "Impersonal-Personal", "Insensitive-Sensitive",
               "Unsociable-Sociable", "Robot-Human", "Tool-Friend")

# Preference items (WTP included for EDA but NOT in preference_mean)
PREFERENCE_ITEMS <- c(
  "preference:i_really_dislike_this_ai-i_really_like_this_ai",
  "preference:i_find_this_ai_boring-i_find_this_ai_engaging",
  "wtp:£0-£30"
)
PREFERENCE_LABELS <- c("Dislike-Like", "Boring-Engaging", "WTP (£0-£30)")

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Load ratings and rankings data using standard utility
ratings_data <- load_task_data("combined_rating_tasks", DATA_DIR)
rankings_data <- load_task_data("combined_ranking_tasks", DATA_DIR)

cat("  Ratings:", n_distinct(ratings_data$ppt_id), "participants\n")
cat("  Rankings:", n_distinct(rankings_data$ppt_id), "participants\n")

# =============================================================================
# SECTION 3: DATA PREPARATION
# =============================================================================

cat("\n--- Preparing Data ---\n\n")

# Prepare ratings data
# Create mean composites (hard-coded column names matching JSONL structure)
ratings_data <- ratings_data %>%
  mutate(
    ppt_id = factor(ppt_id),
    conversation_mode = factor(conversation_mode,
                               levels = c("pre-populated", "dynamic")),
    # Coherence mean (3 items)
    coherence_mean = rowMeans(cbind(
      `coherence:confusing-clear`,
      `coherence:grammatically_incorrect-grammatically_correct`,
      `coherence:incoherent-coherent`
    ), na.rm = TRUE),
    # Relationship-seeking mean (6 items)
    relationship_seeking_mean = rowMeans(cbind(
      `steerability:cold-warm`,
      `steerability:impersonal-personal`,
      `steerability:insensitive-sensitive`,
      `steerability:unsociable-sociable`,
      `steerability:robot-human`,
      `steerability:tool-friend`
    ), na.rm = TRUE),
    # Preference mean (2 items)
    preference_mean = rowMeans(cbind(
      `preference:i_really_dislike_this_ai-i_really_like_this_ai`,
      `preference:i_find_this_ai_boring-i_find_this_ai_engaging`
    ), na.rm = TRUE)
  )

# Set reference level for conversation_mode
ratings_data$conversation_mode <- relevel(ratings_data$conversation_mode, ref = "pre-populated")

cat("Created mean composites for ratings:\n")
cat("  - coherence_mean\n")
cat("  - relationship_seeking_mean\n")
cat("  - preference_mean\n\n")

# Prepare rankings data
# Reshape from wide format to long format for modeling
rankings_long <- rankings_data %>%
  filter(subtask == "multi-chat") %>%  # Exclude practice rankings
  select(
    ppt_id, subtask, ranking_type,
    `A:rank`, `B:rank`, `C:rank`, `D:rank`,
    `A:multiplier`, `B:multiplier`, `C:multiplier`, `D:multiplier`
  ) %>%
  pivot_longer(
    cols = c(`A:rank`, `B:rank`, `C:rank`, `D:rank`),
    names_to = "option",
    values_to = "rank"
  ) %>%
  mutate(
    option_letter = gsub(":rank", "", option),
    multiplier = case_when(
      option_letter == "A" ~ `A:multiplier`,
      option_letter == "B" ~ `B:multiplier`,
      option_letter == "C" ~ `C:multiplier`,
      option_letter == "D" ~ `D:multiplier`
    ),
    ppt_id = factor(ppt_id)
  ) %>%
  select(ppt_id, subtask, ranking_type, option_letter, rank, multiplier)

# Create ordered factor for rank
rankings_long$rank <- ordered(rankings_long$rank, levels = 1:4)

# Version with all subtasks (for EDA plots)
rankings_all <- rankings_data %>%
  select(
    ppt_id, subtask, ranking_type,
    `A:rank`, `B:rank`, `C:rank`, `D:rank`,
    `A:multiplier`, `B:multiplier`, `C:multiplier`, `D:multiplier`
  ) %>%
  pivot_longer(
    cols = c(`A:rank`, `B:rank`, `C:rank`, `D:rank`),
    names_to = "option",
    values_to = "rank"
  ) %>%
  mutate(
    option_letter = gsub(":rank", "", option),
    multiplier = case_when(
      option_letter == "A" ~ `A:multiplier`,
      option_letter == "B" ~ `B:multiplier`,
      option_letter == "C" ~ `C:multiplier`,
      option_letter == "D" ~ `D:multiplier`
    ),
    ppt_id = factor(ppt_id)
  ) %>%
  select(ppt_id, subtask, ranking_type, option_letter, rank, multiplier)
rankings_all$rank <- ordered(rankings_all$rank, levels = 1:4)

# Set ranking_type factor levels (relationship-seeking first)
ranking_type_levels <- c("relationship-seeking", "preference")
rankings_long$ranking_type <- factor(rankings_long$ranking_type,
                                     levels = ranking_type_levels)
rankings_all$ranking_type <- factor(rankings_all$ranking_type,
                                    levels = ranking_type_levels)

# Ranking type colors (shared across plots)
ranking_type_colors <- c(
  "preference" = RColorBrewer::brewer.pal(3, "Set2")[1],
  "relationship-seeking" = RColorBrewer::brewer.pal(3, "Set2")[2]
)

cat("Prepared rankings data:\n")
cat("  Observations (multi-chat only):", nrow(rankings_long), "\n")
cat("  Observations (all subtasks):", nrow(rankings_all), "\n")
cat("  Participants:", n_distinct(rankings_long$ppt_id), "\n")
cat("  Ranking types:", paste(unique(rankings_long$ranking_type), collapse = ", "), "\n\n")

# =============================================================================
# SECTION 4: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Define colors
colors <- RColorBrewer::brewer.pal(3, "Set2")

# --- Rating Multiplier Counts ---
cat("Creating multiplier count plot (ratings)...\n")
p_mult_counts <- ratings_data %>%
  ggplot(aes(x = factor(multiplier), fill = conversation_mode)) +
  geom_bar(alpha = 0.7) +
  facet_wrap(~conversation_mode, nrow = 1, scales = "free_y") +
  theme_pub() +
  labs(x = "Multiplier", y = "Count", fill = "Conversation Mode") +
  theme(legend.position = "none")

save_plot(FIGURE_DIR, p_mult_counts, "rating_multiplier_counts.pdf", width = 8, height = 4)

# --- Rating Outcome Distributions ---
cat("Creating distribution plots (ratings)...\n")
ratings_long <- ratings_data %>%
  pivot_longer(
    cols = all_of(RATING_OUTCOMES),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  mutate(
    outcome_label = case_when(
      outcome == "coherence_mean" ~ "Coherence (Mean)",
      outcome == "relationship_seeking_mean" ~ "Relationship-Seeking (Mean)",
      outcome == "preference_mean" ~ "Preference (Mean)"
    ),
    outcome_label = factor(outcome_label,
      levels = c("Coherence (Mean)", "Relationship-Seeking (Mean)", "Preference (Mean)"))
  )

p_distributions <- ratings_long %>%
  ggplot(aes(x = conversation_mode, y = value, fill = conversation_mode)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~outcome_label, nrow = 1) +
  theme_pub() +
  labs(x = "", y = "Rating", fill = "Conversation Mode") +
  theme(legend.position = "bottom")

save_plot(FIGURE_DIR, p_distributions, "rating_distributions.pdf", width = 12, height = 4)

# --- Individual Item Distributions and Trajectories ---
# Helper function to create distribution + trajectory plots for a set of items
create_item_eda_plots <- function(data, items, labels, outcome_name,
                                   nrow = 1, free_y = FALSE) {
  # Create long format for these items
  items_long <- data %>%
    select(ppt_id, multiplier, conversation_mode, all_of(items)) %>%
    pivot_longer(cols = all_of(items), names_to = "item", values_to = "value") %>%
    mutate(item_label = factor(item, levels = items, labels = labels))

  # Set scales for facets
  facet_scales <- if (free_y) "free_y" else "fixed"

  # Distribution plot
  p_dist <- items_long %>%
    ggplot(aes(x = conversation_mode, y = value, fill = conversation_mode)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~item_label, nrow = nrow, scales = facet_scales) +
    theme_pub() +
    labs(x = "", y = "Rating", fill = "Mode") +
    theme(legend.position = "bottom")

  # Trajectory plot
  p_traj <- items_long %>%
    group_by(multiplier, item_label, conversation_mode) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      se_value = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = multiplier, y = mean_value, color = conversation_mode)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = mean_value - se_value, ymax = mean_value + se_value,
                    fill = conversation_mode), alpha = 0.2, color = NA) +
    facet_wrap(~item_label, nrow = nrow, scales = facet_scales) +
    scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
    theme_pub() +
    labs(x = "Multiplier", y = "Mean Rating", color = "Mode") +
    theme(legend.position = "bottom") +
    guides(fill = "none")

  list(distribution = p_dist, trajectory = p_traj)
}

# Coherence items
cat("Creating coherence item plots...\n")
coherence_plots <- create_item_eda_plots(
  ratings_data, COHERENCE_ITEMS, COHERENCE_LABELS, "coherence", nrow = 1
)
save_plot(FIGURE_DIR, coherence_plots$distribution, "coherence_items_distribution.pdf",
          width = 12, height = 4)
save_plot(FIGURE_DIR, coherence_plots$trajectory, "coherence_items_trajectory.pdf",
          width = 12, height = 4)

# Relationship-seeking items
cat("Creating relationship-seeking item plots...\n")
rs_plots <- create_item_eda_plots(
  ratings_data, RS_ITEMS, RS_LABELS, "relationship_seeking", nrow = 2
)
save_plot(FIGURE_DIR, rs_plots$distribution, "rs_items_distribution.pdf",
          width = 12, height = 6)
save_plot(FIGURE_DIR, rs_plots$trajectory, "rs_items_trajectory.pdf",
          width = 12, height = 6)

# Preference items (including WTP) - free scales because WTP is different range
cat("Creating preference item plots...\n")
preference_plots <- create_item_eda_plots(
  ratings_data, PREFERENCE_ITEMS, PREFERENCE_LABELS, "preference",
  nrow = 1, free_y = TRUE
)
save_plot(FIGURE_DIR, preference_plots$distribution, "preference_items_distribution.pdf",
          width = 12, height = 4)
save_plot(FIGURE_DIR, preference_plots$trajectory, "preference_items_trajectory.pdf",
          width = 12, height = 4)

# --- Rating Trajectories by Multiplier (Means) ---
cat("Creating trajectory plots (ratings)...\n")
p_trajectories <- ratings_long %>%
  group_by(multiplier, outcome_label, conversation_mode) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    se_value = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = multiplier, y = mean_value, color = conversation_mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_value - se_value, ymax = mean_value + se_value,
                  fill = conversation_mode), alpha = 0.2, color = NA) +
  facet_wrap(~outcome_label, nrow = 1) +
  scale_x_continuous(breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  theme_pub() +
  labs(x = "Multiplier", y = "Mean Rating", color = "Conversation Mode") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

save_plot(FIGURE_DIR, p_trajectories, "rating_trajectories.pdf", width = 14, height = 4)

# --- Ranking Mean Rank by Multiplier ---
cat("Creating mean rank plots (rankings)...\n")

# By ranking type (multi-chat only)
p_mean_rank_by_type <- rankings_long %>%  # rankings_long is multi-chat only
  mutate(rank_numeric = as.numeric(as.character(rank))) %>%
  group_by(multiplier, ranking_type) %>%
  summarise(
    mean_rank = mean(rank_numeric, na.rm = TRUE),
    se_rank = sd(rank_numeric, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = factor(multiplier), y = mean_rank, fill = ranking_type)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_rank - 1.96 * se_rank,
                    ymax = mean_rank + 1.96 * se_rank),
                width = 0.2) +
  scale_fill_manual(values = ranking_type_colors) +
  facet_wrap(~ranking_type, nrow = 1) +
  theme_pub() +
  labs(x = "Multiplier", y = "Mean Rank (higher = better)",
       subtitle = "Multi-chat only. Error bars show 95% CI") +
  theme(legend.position = "none")

save_plot(FIGURE_DIR, p_mean_rank_by_type, "ranking_mean_rank_by_type.pdf", width = 10, height = 5)

# By subtask (relationship-seeking only)
p_mean_rank_by_subtask <- rankings_all %>%
  filter(ranking_type == "relationship-seeking") %>%
  mutate(rank_numeric = as.numeric(as.character(rank))) %>%
  group_by(multiplier, subtask) %>%
  summarise(
    mean_rank = mean(rank_numeric, na.rm = TRUE),
    se_rank = sd(rank_numeric, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = factor(multiplier), y = mean_rank)) +
  geom_col(fill = colors[1], alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_rank - 1.96 * se_rank,
                    ymax = mean_rank + 1.96 * se_rank),
                width = 0.2) +
  facet_wrap(~subtask, nrow = 1) +
  theme_pub() +
  labs(x = "Multiplier", y = "Mean Rank (higher = better)",
       subtitle = "Relationship-seeking only. Error bars show 95% CI")

save_plot(FIGURE_DIR, p_mean_rank_by_subtask, "ranking_mean_rank_by_subtask.pdf", width = 10, height = 5)

# --- Ranking Multiplier Counts (by subtask) ---
cat("Creating multiplier count plot (rankings)...\n")

# Long format with all subtasks
rankings_counts <- rankings_data %>%
  select(
    ppt_id, subtask, ranking_type,
    `A:multiplier`, `B:multiplier`, `C:multiplier`, `D:multiplier`
  ) %>%
  pivot_longer(
    cols = c(`A:multiplier`, `B:multiplier`, `C:multiplier`, `D:multiplier`),
    names_to = "option",
    values_to = "multiplier"
  ) %>%
  mutate(ranking_type = factor(ranking_type, levels = ranking_type_levels))

p_rank_counts <- rankings_counts %>%
  ggplot(aes(x = factor(multiplier), fill = ranking_type)) +
  geom_bar(alpha = 0.7) +
  facet_wrap(~subtask, nrow = 1) +
  scale_fill_manual(values = ranking_type_colors) +
  theme_pub() +
  labs(x = "Multiplier", y = "Count", fill = "Ranking Task") +
  theme(legend.position = "bottom")

save_plot(FIGURE_DIR, p_rank_counts, "ranking_multiplier_counts.pdf", width = 12, height = 5)

# --- Ranking Heatmap ---
cat("Creating rank distribution heatmap (rankings)...\n")
heatmap_data <- rankings_long %>%
  mutate(rank_numeric = as.numeric(as.character(rank))) %>%
  count(multiplier, rank_numeric, ranking_type) %>%
  group_by(multiplier, ranking_type) %>%
  mutate(prop = n / sum(n))

p_heatmap <- heatmap_data %>%
  ggplot(aes(x = factor(multiplier), y = factor(rank_numeric), fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(
    label = paste0(round(prop * 100, 0), "%"),
    color = ifelse(prop > 0.3, "white", "black")
  ), size = 5) +
  scale_color_identity() +
  scale_fill_gradient(low = "white", high = "darkblue",
                      labels = scales::percent) +
  facet_wrap(~ranking_type, nrow = 1) +
  theme_pub() +
  labs(x = "Multiplier", y = "Rank", fill = NULL) +
  theme(legend.key.width = unit(1.5, "cm"))

save_plot(FIGURE_DIR, p_heatmap, "ranking_heatmap.pdf", width = 12, height = 6)

# --- Correct Order Ranking Plot ---
cat("Creating correct order ranking plot...\n")
p_correct_order <- rankings_data %>%
  filter(ranking_type == "relationship-seeking") %>%
  mutate(correct_order = ifelse(correct_order_ranking == TRUE,
                                "Correct", "Incorrect")) %>%
  ggplot(aes(x = subtask, fill = correct_order)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Correct" = "#A8D8A8", "Incorrect" = "#F5A5A5")) +
  theme_pub() +
  labs(
    subtitle = "Proportion who ranked multipliers in expected order",
    fill = "Order",
    x = "Subtask",
    y = "Percentage"
  ) +
  theme(legend.position = "bottom")

save_plot(FIGURE_DIR, p_correct_order, "ranking_correct_order.pdf", width = 10, height = 5)

# =============================================================================
# SECTION 5: FIT RATING MODELS
# =============================================================================

cat("\n--- Fitting Rating Models ---\n\n")

rating_models <- list()
rating_comparisons <- list()

for (outcome in RATING_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  rating_models[[outcome]] <- fit_rating_models(
    data = ratings_data,
    outcome_var = outcome,
    include_conversation_mode = TRUE
  )

  rating_comparisons[[outcome]] <- compare_rating_model_performance(
    rating_models[[outcome]]
  )

  cat("\nPerformance Comparison:\n")
  print(rating_comparisons[[outcome]][, c("Name", "AIC", "AICc", "BIC", "RMSE", "Performance_Score")])

  best_model <- select_best_model(rating_comparisons[[outcome]], "AIC")
  cat(sprintf("\nBest model (by AIC): %s\n", best_model))
}

# =============================================================================
# SECTION 6: FIT RANKING MODELS (PlackettLuce)
# =============================================================================

cat("\n--- Fitting Ranking Models (PlackettLuce) ---\n\n")

# PlackettLuce only fits item-based (nonlinear) models
# We fit two versions: full (all multipliers) and truncated (excl. ±1.5)
ranking_models <- list()

for (outcome in RANKING_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  # Filter to specific ranking type
  outcome_data <- rankings_long %>%
    filter(ranking_type == outcome)

  cat("  N observations:", nrow(outcome_data), "\n")
  cat("  N participants:", n_distinct(outcome_data$ppt_id), "\n")

  ranking_models[[outcome]] <- fit_ranking_models(
    data = outcome_data,
    outcome_var = "rank"
  )

  # Print summary for both models
  cat("\n  Full model (all multipliers):\n")
  cat("    AIC:", AIC(ranking_models[[outcome]]$full), "\n")
  cat("    N rankings:", length(ranking_models[[outcome]]$full$rankings), "\n")

  cat("\n  Truncated model (excl. ±1.5):\n")
  cat("    AIC:", AIC(ranking_models[[outcome]]$truncated), "\n")
  cat("    N rankings:", length(ranking_models[[outcome]]$truncated$rankings), "\n")
}

# =============================================================================
# SECTION 7: SAVE MODELS
# =============================================================================

cat("\n--- Saving Models ---\n\n")

saveRDS(rating_models, file.path(MODEL_DIR, "calibration_rating_models.rds"))
saveRDS(ranking_models, file.path(MODEL_DIR, "calibration_ranking_models.rds"))
saveRDS(rating_comparisons, file.path(MODEL_DIR, "calibration_rating_comparisons.rds"))

cat("Saved models and comparisons to:", MODEL_DIR, "\n")

# Save best models for main_paper_plots.R
best_model_coherence <- rating_models$coherence_mean$truncated_quadratic
best_model_rs <- rating_models$relationship_seeking_mean$truncated_quadratic

saveRDS(
  list(coherence = best_model_coherence, relationship_seeking = best_model_rs),
  file.path(MODEL_DIR, "calibration_best_models.rds")
)

cat("Saved best models for main paper plot\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 8: GENERATE TABLES (if --generate_tex_tables)
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  # Predictor labels for rating models
  pred_labels <- c(
    "multiplier" = "Multiplier",
    "I(multiplier^2)" = "Multiplier Squared",
    "multiplier_factorneg1.5" = "Multiplier = -1.5",
    "multiplier_factorneg1" = "Multiplier = -1.0",
    "multiplier_factorneg0.5" = "Multiplier = -0.5",
    "multiplier_factorzero" = "Multiplier = 0",
    "multiplier_factorpos0.5" = "Multiplier = 0.5",
    "multiplier_factorpos1" = "Multiplier = 1.0",
    "multiplier_factorpos1.5" = "Multiplier = 1.5",
    "conversation_modedynamic" = "Conversation Mode: Dynamic"
  )

  text_replacements <- c(
    "Multiplier = -1.5" = "$\\lambda_{-1.5}$",
    "Multiplier = -1.0" = "$\\lambda_{-1.0}$",
    "Multiplier = -0.5" = "$\\lambda_{-0.5}$",
    "Multiplier = 0" = "$\\lambda_{0}$",
    "Multiplier = 0.5" = "$\\lambda_{+0.5}$",
    "Multiplier = 1.0" = "$\\lambda_{+1.0}$",
    "Multiplier = 1.5" = "$\\lambda_{+1.5}$",
    "Multiplier Squared" = "$\\lambda^2$",
    "Multiplier" = "$\\lambda$"
  )

  model_labels <- c("Linear", "Trunc. Linear", "Quadratic", "Trunc. Quad.", "Non-linear")

  # Rating model tables
  for (outcome in RATING_OUTCOMES) {
    outcome_label <- gsub("_", " ", outcome)

    sjplot_to_latex(
      models = rating_models[[outcome]][c("linear", "truncated_linear",
                                          "quadratic", "truncated_quadratic", "nonlinear")],
      model_labels = model_labels,
      pred_labels = pred_labels,
      filename = paste0(outcome, "_rating_models"),
      table_dir = TABLE_DIR,
      caption = paste0("Mixed-effects regression models for ", outcome_label),
      dependent_var = outcome_label,
      text_replacements = text_replacements,
      fontsize = "footnotesize",
      p.adjust = "fdr",
      show.p = FALSE,
      silent = FALSE
    )

    # Performance comparison tables
    generate_performance_table_calibration(
      rating_comparisons[[outcome]],
      filename = paste0(outcome, "_rating"),
      table_dir = TABLE_DIR,
      caption = paste0(
        "Model performance comparison for ", outcome_label,
        ". Note: Truncated and full models should not be directly compared as they are fit on different data."
      ),
      fontsize = "footnotesize",
      is_ranking = FALSE
    )
  }

  # Ranking model tables (PlackettLuce - two models: full and truncated)
  for (outcome in RANKING_OUTCOMES) {
    outcome_label <- gsub("-", " ", outcome)

    if (!is.null(ranking_models[[outcome]])) {
      generate_plackett_luce_table(
        models = list(
          ranking_models[[outcome]]$full,
          ranking_models[[outcome]]$truncated
        ),
        model_labels = c("Plackett-Luce", "Truncated"),
        filename = paste0(gsub("-", "_", outcome), "_ranking_models"),
        table_dir = TABLE_DIR,
        caption = paste0("Plackett-Luce ranking models for ", outcome_label),
        fontsize = "footnotesize"
      )
    }
  }

  cat("\nTables saved to:", file.path(TABLE_DIR, "tex_tables"), "\n")
} else {
  cat("\n--- Skipping LaTeX Tables (use --generate_tex_tables to enable) ---\n")
}

# =============================================================================
# SECTION 9: WINRATE ANALYSIS (Rankings only)
# =============================================================================

cat("\n--- Winrate Analysis ---\n\n")
# Note this is after the other sections
# because it can take a while to run w/ high bootstrap_n

#' Calculate pairwise winrates
calculate_pairwise_winrates <- function(data) {
  # Get all unique multipliers
  all_multipliers <- sort(unique(data$multiplier))

  # Initialize results
  winrate_results <- data.frame()

  for (m1 in all_multipliers) {
    for (m2 in all_multipliers) {
      if (m1 != m2) {
        # Find rankings where both multipliers appear
        pairwise <- data %>%
          filter(multiplier %in% c(m1, m2)) %>%
          group_by(ppt_id) %>%
          filter(n() == 2) %>%
          summarise(
            m1_rank = rank[multiplier == m1][1],
            m2_rank = rank[multiplier == m2][1],
            .groups = "drop"
          ) %>%
          filter(!is.na(m1_rank) & !is.na(m2_rank)) %>%
          mutate(
            m1_rank = as.numeric(as.character(m1_rank)),
            m2_rank = as.numeric(as.character(m2_rank))
          )

        if (nrow(pairwise) > 0) {
          wins <- sum(pairwise$m1_rank > pairwise$m2_rank)
          total <- nrow(pairwise)
          winrate <- wins / total
        } else {
          winrate <- NA
          total <- 0
        }

        winrate_results <- rbind(winrate_results, data.frame(
          multiplier_i = m1,
          multiplier_j = m2,
          winrate = winrate,
          n_comparisons = total
        ))
      }
    }
  }

  return(winrate_results)
}

#' Calculate mean winrates per multiplier
calculate_mean_winrates <- function(winrate_data) {
  winrate_data %>%
    group_by(multiplier_i) %>%
    summarise(
      mean_winrate = mean(winrate, na.rm = TRUE),
      n_total_comparisons = sum(n_comparisons, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(multiplier = multiplier_i) %>%
    arrange(desc(mean_winrate))
}

#' Bootstrap confidence intervals for mean winrates (all multipliers at once)
#' Uses cluster bootstrap resampling at the participant level to preserve
#' within-participant ranking structure (each participant has multiple rows)
#' Parallelized using future_lapply for cross-platform parallel execution
bootstrap_winrate_ci_all <- function(data, n_bootstrap = 1000) {
  # Get unique participants for cluster bootstrap
  unique_ppts <- unique(data$ppt_id)
  n_ppts <- length(unique_ppts)
  all_multipliers <- sort(unique(data$multiplier))

  # Run bootstrap in parallel - each iteration returns winrates for ALL multipliers
  bootstrap_results <- future_lapply(seq_len(n_bootstrap), function(i) {
    # Cluster bootstrap: resample participants with replacement
    boot_ppts <- sample(unique_ppts, n_ppts, replace = TRUE)

    # Build bootstrap data by including all rows for each sampled participant
    boot_indices <- unlist(lapply(boot_ppts, function(p) {
      which(data$ppt_id == p)
    }))
    boot_data <- data[boot_indices, ]

    # Calculate winrates for all multipliers
    boot_winrate <- calculate_pairwise_winrates(boot_data)
    boot_mean <- calculate_mean_winrates(boot_winrate)

    # Return named vector of winrates for all multipliers
    setNames(boot_mean$mean_winrate, boot_mean$multiplier)
  }, future.seed = TRUE)

  # Convert list to matrix (rows = bootstrap iterations, cols = multipliers)
  boot_matrix <- do.call(rbind, bootstrap_results)

  # Calculate CIs for each multiplier
  cis <- data.frame(
    multiplier = as.numeric(colnames(boot_matrix)),
    ci_lower = apply(boot_matrix, 2, quantile, probs = 0.025, na.rm = TRUE),
    ci_upper = apply(boot_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
  )
  rownames(cis) <- NULL

  cis
}

# Calculate winrates for each ranking type and collect results
all_winrates <- data.frame()
all_mean_winrates <- data.frame()

for (outcome in RANKING_OUTCOMES) {
  cat(sprintf("\nWinrate analysis for: %s\n", outcome))

  outcome_data <- rankings_long %>%
    filter(ranking_type == outcome)

  # Calculate pairwise winrates
  winrates <- calculate_pairwise_winrates(outcome_data) %>%
    mutate(ranking_type = outcome)
  mean_winrates <- calculate_mean_winrates(winrates)

  cat("\nMean winrates by multiplier:\n")
  print(mean_winrates)

  # Bootstrap CIs (all multipliers at once)
  cat(sprintf("\nBootstrapping %d samples...\n", bootstrap_n))
  cis <- bootstrap_winrate_ci_all(outcome_data, bootstrap_n)

  mean_winrates_with_ci <- mean_winrates %>%
    left_join(cis, by = "multiplier") %>%
    mutate(ranking_type = outcome)

  cat("\nMean winrates with 95% CI:\n")
  print(mean_winrates_with_ci)

  # Collect results
  all_winrates <- rbind(all_winrates, winrates)
  all_mean_winrates <- rbind(all_mean_winrates, mean_winrates_with_ci)
}

# Set factor levels for facet ordering
all_winrates$ranking_type <- factor(all_winrates$ranking_type,
                                    levels = ranking_type_levels)
all_mean_winrates$ranking_type <- factor(all_mean_winrates$ranking_type,
                                         levels = ranking_type_levels)

# Create faceted winrate heatmap
p_winrate_heatmap <- all_winrates %>%
  ggplot(aes(x = factor(multiplier_j), y = factor(multiplier_i), fill = winrate)) +
  geom_tile(color = "white") +
  geom_text(aes(
    label = ifelse(!is.na(winrate), paste0(round(winrate * 100, 0), "%"), ""),
    color = ifelse(winrate > 0.5, "white", "black")
  ), size = 5) +
  scale_color_identity() +
  scale_fill_gradient(low = "white", high = "darkblue",
                      labels = scales::percent, name = NULL,
                      na.value = "grey90") +
  facet_wrap(~ranking_type, nrow = 1) +
  theme_pub() +
  labs(
    subtitle = "% focal multiplier beats opponent",
    x = "Opponent Multiplier",
    y = "Focal Multiplier"
  ) +
  theme(legend.key.width = unit(1.5, "cm"))

save_plot(FIGURE_DIR, p_winrate_heatmap, "winrate_heatmap.pdf", width = 12, height = 6)

# Create faceted mean winrate bar plot
p_mean_winrate <- all_mean_winrates %>%
  ggplot(aes(x = factor(multiplier), y = mean_winrate, fill = ranking_type)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = pmax(ci_lower, 0), ymax = pmin(ci_upper, 1)),
                width = 0.3, alpha = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = ranking_type_colors) +
  facet_wrap(~ranking_type, nrow = 1) +
  theme_pub() +
  labs(
    subtitle = paste0("95% CI from ", bootstrap_n, " bootstrap samples"),
    x = "Multiplier",
    y = "Mean Winrate"
  ) +
  theme(legend.position = "none")

save_plot(FIGURE_DIR, p_mean_winrate, "winrate_mean.pdf", width = 10, height = 5)

# =============================================================================
# SECTION 10: RATING MODEL EFFECTS PLOTS
# =============================================================================

cat("\n--- Rating Model Effects Plots ---\n\n")

for (outcome in RATING_OUTCOMES) {
  cat(sprintf("Creating effects plots for: %s\n", outcome))

  if (!is.null(rating_models[[outcome]])) {
    outcome_label <- gsub("_mean$", "", outcome)
    outcome_label <- gsub("_", " ", tools::toTitleCase(outcome_label))

    # Create combined subplot figure using patchwork
    combined_plot <- create_and_save_rating_effects(
      results = rating_models[[outcome]],
      outcome_label = outcome_label,
      filename_prefix = gsub("_mean$", "", outcome),
      figure_dir = FIGURE_DIR,
      save_plot_fn = save_plot
    )

    cat(sprintf("  Saved combined effects plot for %s\n", outcome))
  }
}

# =============================================================================
# SECTION 11: PLACKETT-LUCE WORTH PARAMETER PLOTS
# =============================================================================

cat("\n--- PlackettLuce Worth Parameter Plots ---\n\n")

for (outcome in RANKING_OUTCOMES) {
  cat(sprintf("Creating worth parameter plots for: %s\n", outcome))

  if (!is.null(ranking_models[[outcome]])) {
    outcome_label <- gsub("-", " ", tools::toTitleCase(outcome))

    # Create and save combined plot (full + truncated side by side)
    combined_plot <- create_and_save_plackett_luce_plots(
      results = ranking_models[[outcome]],
      outcome_label = outcome_label,
      filename_prefix = gsub("-", "_", outcome),
      figure_dir = FIGURE_DIR,
      save_plot_fn = save_plot,
      ref_multiplier = 0,
      color = ranking_type_colors[[outcome]]
    )

    cat(sprintf("  Saved PlackettLuce plot for %s\n", outcome))
  }
}

# =============================================================================
# SECTION 12: REPORT GENERATION (if --generate_report)
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "calibration_study_report.md")
  fig_rel_path <- "../../outputs/figures/calibration_study/png"

  # Build report content
  lines <- c(
    "# Calibration Study Analysis Report",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "---",
    "",
    "## Overview",
    "",
    "This analysis validates the relationship-seeking multiplier ($\\lambda$) using a",
    "calibration study design. Participants rated AI conversations on coherence (off-target)",
    "and relationship-seeking qualities (on-target) across different multiplier levels.",
    "",
    "---",
    "",
    "## Data Summary",
    "",
    paste0("- **Rating observations:** ", nrow(ratings_data)),
    paste0("- **Rating participants:** ", n_distinct(ratings_data$ppt_id)),
    paste0("- **Ranking observations:** ", nrow(rankings_long)),
    paste0("- **Ranking participants:** ", n_distinct(rankings_long$ppt_id)),
    "",
    "---",
    ""
  )

  # ==========================================================================
  # SECTION 1: RATING EDA
  # ==========================================================================
  lines <- c(lines,
    "## Rating EDA",
    "",
    "### Multiplier Distribution",
    "",
    "```",
    capture.output(print(table(ratings_data$multiplier))),
    "```",
    "",
    "### Conversation Modes",
    "",
    "```",
    capture.output(print(table(ratings_data$conversation_mode))),
    "```",
    "",
    "### Trajectories by Multiplier (Means)",
    "",
    paste0("![Rating Trajectories](", fig_rel_path, "/rating_trajectories.png)"),
    "",
    "### Distributions (Means)",
    "",
    paste0("![Rating Distributions](", fig_rel_path, "/rating_distributions.png)"),
    "",
    "### Coherence Items",
    "",
    paste0("![Coherence Distribution](", fig_rel_path, "/coherence_items_distribution.png)"),
    "",
    paste0("![Coherence Trajectory](", fig_rel_path, "/coherence_items_trajectory.png)"),
    "",
    "### Relationship-Seeking Items",
    "",
    paste0("![RS Distribution](", fig_rel_path, "/rs_items_distribution.png)"),
    "",
    paste0("![RS Trajectory](", fig_rel_path, "/rs_items_trajectory.png)"),
    "",
    "### Preference Items (incl. WTP)",
    "",
    paste0("![Preference Distribution](", fig_rel_path, "/preference_items_distribution.png)"),
    "",
    paste0("![Preference Trajectory](", fig_rel_path, "/preference_items_trajectory.png)"),
    "",
    "---",
    ""
  )

  # ==========================================================================
  # SECTION 2: RATING REGRESSION RESULTS
  # ==========================================================================
  lines <- c(lines,
    "## Rating Regression Results",
    "",
    "**Model (Truncated Quadratic):**",
    "",
    "`outcome ~ conversation_mode + multiplier + I(multiplier^2) + (1 | ppt_id)`",
    "",
    "(Truncated = excluding $\\lambda = \\pm 1.5$)",
    ""
  )

  # Add model comparison tables for each outcome
  for (outcome in RATING_OUTCOMES) {
    outcome_label <- gsub("_", " ", tools::toTitleCase(outcome))

    lines <- c(lines,
      paste0("### ", outcome_label),
      "",
      "**Model Comparison:**",
      "",
      "| Model | AIC | BIC | RMSE | Performance Score |",
      "|-------|-----|-----|------|-------------------|"
    )

    comp <- rating_comparisons[[outcome]]
    for (i in seq_len(min(6, nrow(comp)))) {
      lines <- c(lines, sprintf("| %s | %.1f | %.1f | %.2f | %.3f |",
                                comp$Name[i], comp$AIC[i], comp$BIC[i],
                                comp$RMSE[i], comp$Performance_Score[i]))
    }

    # Add coefficient summary for best model (fixed effects only)
    best_model <- rating_models[[outcome]]$truncated_quadratic
    params <- model_parameters(best_model, ci = 0.95, effects = "fixed")

    lines <- c(lines,
      "",
      "**Best model coefficients (Truncated Quadratic):**",
      "",
      "| Term | Estimate | SE | t | p |",
      "|------|----------|----|----|---|"
    )

    for (j in seq_len(nrow(params))) {
      p_val <- params$p[j]
      p_str <- if (is.na(p_val)) "—" else if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
      lines <- c(lines, sprintf("| %s | %.3f | %.3f | %.2f | %s |",
                                params$Parameter[j], params$Coefficient[j],
                                params$SE[j], params$t[j], p_str))
    }

    lines <- c(lines, "")
  }

  # Rating model effects figures
  lines <- c(lines,
    "### Rating Model Effects (5 Specifications)",
    "",
    "#### Coherence",
    "",
    paste0("![Coherence Effects](", fig_rel_path, "/coherence_effects_combined.png)"),
    "",
    "#### Relationship-Seeking",
    "",
    paste0("![RS Effects](", fig_rel_path, "/relationship_seeking_effects_combined.png)"),
    "",
    "#### Preference",
    "",
    paste0("![Preference Effects](", fig_rel_path, "/preference_effects_combined.png)"),
    "",
    "---",
    ""
  )

  # ==========================================================================
  # SECTION 3: RANKING EDA
  # ==========================================================================
  lines <- c(lines,
    "## Ranking EDA",
    "",
    "### Multiplier Counts (by Subtask)",
    "",
    paste0("![Multiplier Counts](", fig_rel_path, "/ranking_multiplier_counts.png)"),
    "",
    "### Mean Rank by Ranking Type (Multi-chat)",
    "",
    paste0("![Mean Rank by Type](", fig_rel_path, "/ranking_mean_rank_by_type.png)"),
    "",
    "### Mean Rank by Subtask (Relationship-seeking)",
    "",
    paste0("![Mean Rank by Subtask](", fig_rel_path, "/ranking_mean_rank_by_subtask.png)"),
    "",
    "### Rank Distribution Heatmap",
    "",
    paste0("![Rank Heatmap](", fig_rel_path, "/ranking_heatmap.png)"),
    "",
    "### Correct Ranking Order (Relationship-Seeking)",
    "",
    paste0("![Correct Order](", fig_rel_path, "/ranking_correct_order.png)"),
    "",
    "### Winrate Analysis",
    "",
    paste0("![Winrate Heatmap](", fig_rel_path, "/winrate_heatmap.png)"),
    "",
    paste0("![Mean Winrate](", fig_rel_path, "/winrate_mean.png)"),
    "",
    "---",
    ""
  )

  # ==========================================================================
  # SECTION 4: RANKING REGRESSION RESULTS (PlackettLuce)
  # ==========================================================================
  lines <- c(lines,
    "## Ranking Regression Results (PlackettLuce)",
    "",
    "**Model:** Plackett-Luce model estimating worth parameters for each multiplier level.",
    "",
    "(Truncated = excluding $\\lambda = \\pm 1.5$)",
    "",
    "### Preference Rankings",
    "",
    paste0("![Preference Log-Worth](", fig_rel_path, "/plackett_luce_preference.png)"),
    "",
    "### Relationship-Seeking Rankings",
    "",
    paste0("![RS Log-Worth](", fig_rel_path, "/plackett_luce_relationship_seeking.png)"),
    ""
  )


  # Write report
  writeLines(lines, report_path)
  cat("Report saved to:", report_path, "\n")
}
