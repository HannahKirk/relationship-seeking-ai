#!/usr/bin/env Rscript
# =============================================================================
# Pre-Treatment Attitudes
# =============================================================================
#
# This script performs EDA on three pre-treatment measures:
# 1. Societal Attitudes to Anthropomorphism Survey
# 2. Topic Preferences (use case rankings)
# 3. Stated Preferences (AI characteristics)
#
# Usage:
#   Rscript scripts/analysis/pre_treatment_attitudes.R
#   Rscript scripts/analysis/pre_treatment_attitudes.R --generate_report
#   Rscript scripts/analysis/pre_treatment_attitudes.R --generate_tex_tables
#
# =============================================================================

# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(patchwork)
library(pheatmap)
library(RColorBrewer)
library(knitr)

set.seed(1234)

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
TABLE_DIR <- paths$TABLE_DIR
TABLE_DIR_TABLES <- file.path(TABLE_DIR, "tex_tables")
REPORT_DIR <- paths$REPORT_DIR
DATA_DIR <- paths$DATA_DIR
GENERATED_DIR <- paths$GENERATED_DIR

# Source utility functions
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_utils.R"))

# Create directories (save_plot creates pdf/png subdirs automatically)
dir.create(FIGURE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(GENERATED_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOCAL HELPER FUNCTIONS
# =============================================================================

#' Calculate summary statistics for outcome variables by group
calc_summary_stats <- function(data, outcome_vars, group_var = "study_id") {
  map_dfr(outcome_vars, function(var) {
    data %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(.data[[group_var]]) %>%
      summarise(
        variable = var,
        n = n(),
        mean = mean(.data[[var]], na.rm = TRUE),
        sd = sd(.data[[var]], na.rm = TRUE),
        se = sd / sqrt(n),
        ci_lower = mean - 1.96 * se,
        ci_upper = mean + 1.96 * se,
        .groups = "drop"
      )
  })
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Pre-Treatment Attitudes\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# =============================================================================
# 1. SOCIETAL ATTITUDES TO ANTHROPOMORPHISM
# =============================================================================

cat("\n--- 1. Societal Attitudes to Anthropomorphism ---\n\n")

# Load data
attitudes_raw <- load_task_data("anthro-attitudes", DATA_DIR)
attitudes_data <- prepare_treatment_arms(attitudes_raw) %>%
  filter_main_studies() %>%
  filter_pre_treatment()

cat("Loaded", n_distinct(attitudes_data$ppt_id), "participants\n")

# Create category mapping for cleaner labels
CATEGORY_MAPPING <- c(
  "mental_states" = "Mental States",
  "tone" = "Tone",
  "relationships" = "Relationships"
)

# Summary statistics by category
cat("\nSummary by category:\n")
attitudes_summary <- attitudes_data %>%
  group_by(slider_category) %>%
  summarise(
    n_items = n_distinct(slider_name),
    n_responses = n(),
    mean_rating = mean(slider_response, na.rm = TRUE),
    sd_rating = sd(slider_response, na.rm = TRUE),
    .groups = "drop"
  )
print(attitudes_summary)

# Create survey item plot
attitudes_plot <- plot_survey_items(attitudes_data, CATEGORY_MAPPING)
save_plot(FIGURE_DIR,attitudes_plot, "anthro_attitudes_survey", 10, 10)

# =============================================================================
# 2. TOPIC PREFERENCES
# =============================================================================

cat("\n--- 2. Topic Preferences ---\n\n")

# Load data
topics_raw <- load_task_data("usecase-preferences", DATA_DIR)
topics_data <- prepare_treatment_arms(topics_raw) %>%
  filter_main_studies()

cat("Loaded", n_distinct(topics_data$ppt_id), "participants\n")

# Reshape to long format
topics_long <- topics_data %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "rank_position",
    values_to = "topic",
    names_prefix = "rank_"
  ) %>%
  mutate(
    rank = as.numeric(str_extract(rank_position, "\\d+")),
    topic = str_wrap(topic, 30)
  ) %>%
  filter(!is.na(topic), topic != "")

# Reverse ranking so higher is better
topics_long <- topics_long %>%
  mutate(rank = 6 - rank)

# Summary of mean ranks
cat("\nMean ranks by topic:\n")
topic_means <- topics_long %>%
  group_by(topic) %>%
  summarise(
    mean_rank = mean(rank, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_rank))
print(topic_means)

# Create ranking plot
topics_plot <- plot_topic_rankings(topics_long)
save_plot(FIGURE_DIR,topics_plot, "topic_preferences", 14, 6)

# -----------------------------------------------------------------------------
# 2b. Topic Preference Matching
# -----------------------------------------------------------------------------

cat("\n--- 2b. Topic Preference Matching ---\n\n")

# Calculate relative preference between polchat and emotchat topics
relative_preferences <- topics_raw %>%
  filter(study_id %in% c("cross-sectional", "longitudinal")) %>%
  pivot_longer(
    cols = starts_with("rank_"),
    names_to = "rank_position",
    values_to = "topic"
  ) %>%
  mutate(rank = as.numeric(str_extract(rank_position, "\\d+"))) %>%
  filter(topic %in% c(
    "Discussions about current events and political topics",
    "Personal conversations about daily life and emotional wellbeing"
  )) %>%
  select(ppt_id, study_id, domain, topic, rank) %>%
  pivot_wider(names_from = topic, values_from = rank) %>%
  rename(
    rank_polchat = `Discussions about current events and political topics`,
    rank_emotchat = `Personal conversations about daily life and emotional wellbeing`
  ) %>%
  mutate(
    relative_preference = case_when(
      rank_polchat < rank_emotchat ~ "prefers_polchat",
      rank_emotchat < rank_polchat ~ "prefers_emotchat",
      rank_polchat == rank_emotchat ~ "tie",
      TRUE ~ NA_character_
    )
  )

# Find absolute most preferred topic for each participant
absolute_preferences <- topics_raw %>%
  filter(study_id %in% c("cross-sectional", "longitudinal")) %>%
  select(ppt_id, study_id, domain, most_preferred_topic = rank_1_topic)

# Combine data
topic_match_data <- relative_preferences %>%
  left_join(absolute_preferences, by = c("ppt_id", "study_id", "domain"))

# Analysis 1: % who received their ABSOLUTE most preferred topic
absolute_match <- topic_match_data %>%
  mutate(
    got_absolute_preference = case_when(
      domain == "emotchat" & most_preferred_topic ==
        "Personal conversations about daily life and emotional wellbeing" ~ TRUE,
      domain == "polchat" & most_preferred_topic ==
        "Discussions about current events and political topics" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(study_id, domain) %>%
  summarise(
    n_total = n(),
    n_got_absolute = sum(got_absolute_preference, na.rm = TRUE),
    pct_got_absolute = round((n_got_absolute / n_total) * 100, 1),
    .groups = "drop"
  )

# Analysis 2: % who received their RELATIVE preferred topic
relative_match <- topic_match_data %>%
  mutate(
    got_relative_preference = case_when(
      domain == "emotchat" & relative_preference == "prefers_emotchat" ~ TRUE,
      domain == "polchat" & relative_preference == "prefers_polchat" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(study_id, domain) %>%
  summarise(
    n_total = n(),
    n_got_relative = sum(got_relative_preference, na.rm = TRUE),
    pct_got_relative = round((n_got_relative / n_total) * 100, 1),
    .groups = "drop"
  )

# Combine summaries
topic_match_summary <- absolute_match %>%
  left_join(relative_match, by = c("study_id", "domain")) %>%
  select(study_id, domain, n = n_total.x, pct_got_absolute, pct_got_relative)

cat("\nTopic Preference Matching Summary:\n")
print(topic_match_summary)

# Generate LaTeX table for topic preference matching
if (generate_tex_tables) {
  # Format domain names
  topic_match_tex <- topic_match_summary %>%
    mutate(
      domain_clean = case_when(
        domain == "emotchat" ~ "EmotChat",
        domain == "polchat" ~ "PolChat",
        TRUE ~ domain
      )
    )

  tex_lines <- c(
    "\\begin{table}[!h]",
    "\\footnotesize",
    "\\centering",
    "\\caption{Topic Preference Matching by Study and Domain. Absolute: \\% who received their top-ranked topic overall. Relative: \\% who preferred assigned domain over the alternative.}",
    "\\label{tab:topic_preference_matching}",
    "\\begin{tabular}[t]{llccc}",
    "\\toprule",
    "\\multicolumn{3}{c}{ } & \\multicolumn{2}{c}{Match Percentage} \\\\",
    "\\cmidrule(l{3pt}r{3pt}){4-5}",
    "Study & Domain & N & Absolute (\\%) & Relative (\\%)\\\\",
    "\\midrule"
  )

  # Add data rows, grouping by study
  current_study <- ""
  for (i in seq_len(nrow(topic_match_tex))) {
    row <- topic_match_tex[i, ]
    if (row$study_id != current_study) {
      study_label <- tools::toTitleCase(row$study_id)
      current_study <- row$study_id
    } else {
      study_label <- ""
    }
    tex_lines <- c(tex_lines, sprintf(
      "%s & %s & %d & %.1f & %.1f\\\\",
      study_label, row$domain_clean, row$n, row$pct_got_absolute, row$pct_got_relative
    ))
  }

  tex_lines <- c(tex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  tex_path <- file.path(TABLE_DIR_TABLES, "topic_preference_matching.tex")
  writeLines(tex_lines, tex_path)
  cat("Saved LaTeX table:", tex_path, "\n")
}

# Export topic matching data
topic_export <- topic_match_data %>%
  mutate(
    received_absolute_pref_topic_match = case_when(
      domain == "emotchat" & most_preferred_topic ==
        "Personal conversations about daily life and emotional wellbeing" ~ TRUE,
      domain == "polchat" & most_preferred_topic ==
        "Discussions about current events and political topics" ~ TRUE,
      TRUE ~ FALSE
    ),
    received_pref_topic_relative_match = case_when(
      domain == "emotchat" & relative_preference == "prefers_emotchat" ~ TRUE,
      domain == "polchat" & relative_preference == "prefers_polchat" ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  select(ppt_id, domain, received_absolute_pref_topic_match,
         received_pref_topic_relative_match)

save_task_data(topic_export, "topic_groups", GENERATED_DIR, relative_to = PROJECT_ROOT)

# =============================================================================
# 3. STATED PREFERENCES
# =============================================================================

cat("\n--- 3. Stated Preferences ---\n\n")

# Load data
prefs_raw <- load_task_data("stated-preferences", DATA_DIR)
prefs_data <- prepare_treatment_arms(prefs_raw) %>%
  filter_main_studies()

cat("Loaded", n_distinct(prefs_data$ppt_id), "participants\n")

# Define outcome variables
PREF_VARS <- c(
  "cold_warm", "impersonal_personal", "insensitive_sensitive",
  "unsociable_sociable", "robot_human", "tool_friend", "personalisation_agreement"
)

# Filter to pre-treatment
prefs_pre <- prefs_data %>%
  filter(timepoint == "pre")

# Summary statistics
cat("\nSummary statistics (pre-treatment):\n")
prefs_summary <- calc_summary_stats(prefs_pre, PREF_VARS, "study_id")
print(prefs_summary)

# 3a. Boxplot by study
cat("\nCreating distribution boxplot...\n")
boxplot_plot <- boxplot_by_study(prefs_pre, PREF_VARS, nrow = 1)
save_plot(FIGURE_DIR,boxplot_plot, "stated_prefs_distributions_by_study", 18, 6)

# 3b. Correlation heatmaps (pre-treatment only)
cat("\nCreating correlation heatmaps...\n")
prefs_pre_cs <- prefs_pre %>% filter(study_id == "cross-sectional")
prefs_pre_long <- prefs_pre %>% filter(study_id == "longitudinal")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = prefs_pre_cs,
  data_long = prefs_pre_long,
  outcome_vars = PREF_VARS
)
save_plot(FIGURE_DIR, corr_plot, "stated_prefs_corr_by_study", 12, 6)

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Report ---\n")

  report_path <- file.path(REPORT_DIR, "01_pre_treatment_attitudes.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  lines <- c(
    "# Pre-Treatment Attitudes",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis explores pre-treatment attitudes across three measures:",
    "",
    "1. **Societal Attitudes to Anthropomorphism Survey** - 12 items across 3 categories",
    "2. **Topic Preferences** - Ranking of 5 AI use cases",
    "3. **Stated Preferences** - 7 items on ideal AI characteristics",
    "",
    "---",
    "",
    "## 1. Societal Attitudes to Anthropomorphism",
    "",
    paste0("**N = ", n_distinct(attitudes_data$ppt_id), "** participants"),
    "",
    "### Survey Item Responses",
    "",
    paste0("![Anthro Attitudes](", fig_rel_path, "/anthro_attitudes_survey.png)"),
    "",
    "### Summary by Category",
    "",
    "| Category | Items | Mean | SD |",
    "|----------|-------|------|-----|"
  )

  for (i in seq_len(nrow(attitudes_summary))) {
    lines <- c(lines, sprintf(
      "| %s | %d | %.1f | %.1f |",
      CATEGORY_MAPPING[attitudes_summary$slider_category[i]],
      attitudes_summary$n_items[i],
      attitudes_summary$mean_rating[i],
      attitudes_summary$sd_rating[i]
    ))
  }

  lines <- c(
    lines,
    "",
    "---",
    "",
    "## 2. Topic Preferences",
    "",
    paste0("**N = ", n_distinct(topics_data$ppt_id), "** participants"),
    "",
    "Participants ranked 5 AI use cases. Higher rank = more preferred.",
    "",
    "### Ranking Summary",
    "",
    paste0("![Topic Preferences](", fig_rel_path, "/topic_preferences.png)"),
    "",
    "### Mean Ranks",
    "",
    "| Topic | Mean Rank |",
    "|-------|-----------|"
  )

  for (i in seq_len(nrow(topic_means))) {
    lines <- c(lines, sprintf(
      "| %s | %.2f |",
      gsub("\n", " ", topic_means$topic[i]),
      topic_means$mean_rank[i]
    ))
  }

  lines <- c(
    lines,
    "",
    "### Topic Preference Matching",
    "",
    "How well did assigned domains match participant preferences?",
    "",
    "- **Absolute Match**: % who received their #1 most preferred topic overall",
    "- **Relative Match**: % who preferred their assigned domain over the alternative",
    "",
    "| Study | Domain | N | Absolute Match (%) | Relative Match (%) |",
    "|-------|--------|---|--------------------|--------------------|"
  )

  for (i in seq_len(nrow(topic_match_summary))) {
    lines <- c(lines, sprintf(
      "| %s | %s | %d | %.1f | %.1f |",
      topic_match_summary$study_id[i],
      tools::toTitleCase(topic_match_summary$domain[i]),
      topic_match_summary$n[i],
      topic_match_summary$pct_got_absolute[i],
      topic_match_summary$pct_got_relative[i]
    ))
  }

  lines <- c(
    lines,
    "",
    "---",
    "",
    "## 3. Stated Preferences (Pre-Treatment)",
    "",
    paste0("**N = ", n_distinct(prefs_pre$ppt_id), "** participants"),
    "",
    "Pre-treatment ratings of ideal AI characteristics on 7 semantic differential scales.",
    "Higher values indicate preference for warmer, more personal, more human-like AI.",
    "",
    "### Distribution by Study",
    "",
    paste0("![Stated Prefs Distributions](", fig_rel_path, "/stated_prefs_distributions_by_study.png)"),
    "",
    "### Correlation Heatmaps",
    "",
    paste0("![Stated Prefs Correlations](", fig_rel_path, "/stated_prefs_corr_by_study.png)"),
    "",
    "---",
    "",
    "### Summary Statistics (Stated Preferences)",
    "",
    "| Variable | Study | N | Mean | SD |",
    "|----------|-------|---|------|-----|"
  )

  for (i in seq_len(nrow(prefs_summary))) {
    lines <- c(lines, sprintf(
      "| %s | %s | %d | %.1f | %.1f |",
      clean_var_name(prefs_summary$variable[i]),
      prefs_summary$study_id[i],
      prefs_summary$n[i],
      prefs_summary$mean[i],
      prefs_summary$sd[i]
    ))
  }

  # Write report
  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
