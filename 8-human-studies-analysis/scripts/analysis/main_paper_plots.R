# =============================================================================
# Main Paper Plots
# =============================================================================
#
# This script generates the main paper plots.
#
# Usage: Rscript scripts/analysis/main_paper_plots.R [--p-value p_global|p_local|p_raw]
#
# =============================================================================

library(tidyverse)
library(jsonlite)
library(cowplot)
library(ggeffects)
library(ggthemes)
library(parameters)

# =============================================================================
# Setup Paths
# =============================================================================

source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
STATS_DIR <- paths$STATS_DIR
MODEL_DIR <- paths$MODEL_DIR
FIGURE_DIR <- file.path(PROJECT_ROOT, "outputs/figures/paper_plots")

# Create output directories
dir.create(file.path(FIGURE_DIR, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURE_DIR, "png"), recursive = TRUE, showWarnings = FALSE)

# Source utilities
source("scripts/utils_r/plot_config.R")
source("scripts/plotting/main_plotting_utils.R")

# =============================================================================
# Parse Command Line Arguments
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
P_VALUE_COL <- "p_global"  # Default

if ("--p-value" %in% args) {
  idx <- which(args == "--p-value")
  if (length(args) > idx) {
    P_VALUE_COL <- args[idx + 1]
    if (!P_VALUE_COL %in% c("p_global", "p_local", "p_raw")) {
      stop("Invalid p-value column. Must be one of: p_global, p_local, p_raw")
    }
  }
}

cat("Using p-value column:", P_VALUE_COL, "\n\n")

# =============================================================================
# PLOT_CONFIG: Global to Local Family Mapping
# =============================================================================

PLOT_CONFIG <- list(
  preferences = list(
    global_family = "preferences",
    local_families = list(
      preferences = list(
        outcomes = c("likeability", "engagingness", "helpfulness"),
        model_prefix = "preferences",
        time_var = "session_numeric"
      )
    ),
    contrasts_file = "preferences_contrasts.json",
    contrasts_file_cs = "preferences_contrasts_cs.json",
    has_temporal = TRUE,
    output_file = "preferences_combined"
  ),

  attachment = list(
    global_family = "attachment",
    local_families = list(
      attachment_core = list(
        outcomes = c("reliance", "perceived_understanding", "self_disclosure", "separation_distress"),
        model_prefix = "attachment",
        time_var = "week_numeric"
      )
    ),
    contrasts_file = "attachment_contrasts.json",
    contrasts_file_cs = "attachment_contrasts_cs.json",
    has_temporal = TRUE,
    output_file = "attachment_combined"
  ),

  attachment_behavioural = list(
    global_family = "attachment",  # Same contrasts file as attachment
    local_families = list(
      goodbye = list(
        outcomes = c("goodbye_action"),
        model_prefix = "goodbye",
        time_var = NULL  # post-only
      ),
      seeking_companionship = list(
        outcomes = c("seeking_companionship_likelihood"),
        model_prefix = "seeking_companionship",
        time_var = NULL  # post-only
      )
    ),
    contrasts_file = "attachment_contrasts.json",
    contrasts_file_cs = "attachment_contrasts_cs.json",
    has_temporal = FALSE,
    output_file = "attachment_behavioural_combined"
  ),

  wellbeing = list(
    global_family = "wellbeing",
    local_families = list(
      psychosocial = list(
        outcomes = c("psychosocial_F1", "psychosocial_F2"),
        model_prefix = "psychosocial",
        time_var = NULL,
        contrasts_file = "psychosocial_contrasts.json",
        contrasts_file_cs = "psychosocial_contrasts_cs.json"
      ),
      momentary_affect = list(
        outcomes = c("valence", "arousal"),
        model_prefix = "mood",
        time_var = "session_numeric",
        contrasts_file = "momentary_affect_contrasts.json",
        contrasts_file_cs = "momentary_affect_contrasts_cs.json"
      )
    ),
    has_temporal = TRUE,  # momentary_affect has temporal
    output_file = "wellbeing_combined"
  ),

  perceptions = list(
    global_family = "perceptions",
    local_families = list(
      relational = list(
        outcomes = c("tool_friend"),
        model_prefix = "relational",
        time_var = "week_numeric"
      ),
      sentience = list(
        outcomes = c("ontological_sentience", "perceived_sentience"),
        model_prefix = "sentience",
        time_var = "week_numeric"
      )
    ),
    contrasts_file = "perceptions_contrasts.json",
    contrasts_file_cs = "perceptions_contrasts_cs.json",
    has_temporal = TRUE,
    output_file = "perceptions_combined"
  )
)

# =============================================================================
# Helper Functions
# =============================================================================

#' Load models for a local family
#' @param model_prefix The prefix for model files (e.g., "preferences")
#' @return List with longitudinal and cross_sectional models, plus data
load_models_for_family <- function(model_prefix) {
  long_path <- file.path(MODEL_DIR, paste0(model_prefix, "_longitudinal.rds"))
  cs_path <- file.path(MODEL_DIR, paste0(model_prefix, "_cross_sectional.rds"))
  data_path <- file.path(MODEL_DIR, paste0(model_prefix, "_data.rds"))

  result <- list()

  if (file.exists(long_path)) {
    result$longitudinal <- readRDS(long_path)
    cat("  Loaded:", long_path, "\n")
  }

  if (file.exists(cs_path)) {
    result$cross_sectional <- readRDS(cs_path)
    cat("  Loaded:", cs_path, "\n")
  }

  if (file.exists(data_path)) {
    result$data <- readRDS(data_path)
    cat("  Loaded:", data_path, "\n")
  }

  result
}

#' Load all models for a plot config
#' @param config A PLOT_CONFIG entry
#' @return List of models by local family name
load_all_models <- function(config) {
  cat("Loading models for", config$global_family, "...\n")
  models <- list()

  for (local_name in names(config$local_families)) {
    local <- config$local_families[[local_name]]
    models[[local_name]] <- load_models_for_family(local$model_prefix)
  }

  models
}

#' Get all outcomes from a config
#' @param config A PLOT_CONFIG entry
#' @return Character vector of all outcome names
get_all_outcomes <- function(config) {
  unlist(lapply(config$local_families, function(x) x$outcomes))
}

# =============================================================================
# Generate Preferences Plots
# =============================================================================

generate_preferences_plots <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating PREFERENCES plots\n")
  cat(strrep("=", 60), "\n\n")

  config <- PLOT_CONFIG$preferences
  # Use the specific order from the original notebook
  outcomes <- c("engagingness", "likeability", "helpfulness")

  # Load JSON contrasts
  json_path <- file.path(STATS_DIR, config$contrasts_file)
  json_data <- load_contrasts_json(json_path)

  # Load models
  models <- load_all_models(config)
  prefs_models <- models$preferences

  # Extract results from JSON
  rs_results <- extract_rs(json_data, outcomes, "longitudinal", P_VALUE_COL)
  domain_results <- extract_domain(json_data, outcomes, "longitudinal", P_VALUE_COL)
  pers_results <- extract_pers(json_data, outcomes, "longitudinal", P_VALUE_COL)
  temporal_results <- extract_temporal(json_data, outcomes, "session_numeric", P_VALUE_COL)
  polynomial_results <- extract_polynomial(json_data, outcomes, P_VALUE_COL)
  moderation_results <- extract_moderation(json_data, outcomes, P_VALUE_COL)

  label_size <- 30

  # Plot 1: Lambda predictions (Panel B)
  cat("  Creating lambda prediction plot...\n")
  p_lambda <- plot_lambda_means_with_predictions(
    mods_longitudinal = prefs_models$longitudinal,
    data_longitudinal = prefs_models$data %>% filter(study_id == "longitudinal"),
    construct_names = outcomes,
    custom_order = outcomes,
    time_var = "session_numeric",
    family_results = wrap_polynomial(polynomial_results),
    ncol = 3,
    width = 14, height = 6,
    free_y = FALSE,
    legend_position = "none"
  )

  # Plot 2: Treatment effects forest (Panel A)
  cat("  Creating treatment effects forest plot...\n")
  p_forest <- plot_treatment_effects_forest(
    rs_results_long = rs_results,
    pers_results_long = pers_results,
    domain_results_long = domain_results,
    custom_order = outcomes,
    legend_position = "none"
  )

  # Plot 3: Time coefficients forest (Panel C) - with legend for extraction
  cat("  Creating time coefficients forest plot...\n")
  p_time_forest_with_legend <- NULL
  p_time_forest <- NULL
  if (!is.null(temporal_results)) {
    p_time_forest_with_legend <- plot_time_coeffs_forest(
      temporal_results_long = temporal_results,
      time_var = "session_numeric",
      custom_order = outcomes,
      legend_position = "bottom",
      legend_nrow = 2
    )
    forest_legend <- get_legend(p_time_forest_with_legend)

    # Now create without legend for layout
    p_time_forest <- plot_time_coeffs_forest(
      temporal_results_long = temporal_results,
      time_var = "session_numeric",
      custom_order = outcomes,
      legend_position = "none"
    )
  }

  # Plot 4: Temporal trends (Panel D) - with legend for extraction
  cat("  Creating temporal trends plot...\n")
  p_trends <- tryCatch({
    # First create with legend to extract it
    p_trends_with_legend <- plot_temporal_predictions_colored(
      mods_longitudinal = prefs_models$longitudinal,
      data_longitudinal = prefs_models$data %>% filter(study_id == "longitudinal"),
      slope_results = json_data,  # Pass JSON directly for slope annotations
      terms = c("lambda"),
      custom_order = outcomes,
      time_var = "session_numeric",
      add_lines = TRUE,
      legend_position = "bottom",
      legend_nrow = 2
    )
    trends_legend <<- get_legend(p_trends_with_legend)

    # Now create without legend for layout
    plot_temporal_predictions_colored(
      mods_longitudinal = prefs_models$longitudinal,
      data_longitudinal = prefs_models$data %>% filter(study_id == "longitudinal"),
      slope_results = json_data,  # Pass JSON directly for slope annotations
      terms = c("lambda"),
      custom_order = outcomes,
      time_var = "session_numeric",
      add_lines = TRUE,
      legend_position = "none"
    )
  }, error = function(e) {
    cat("    Note: Skipping temporal trends plot:", conditionMessage(e), "\n")
    trends_legend <<- NULL
    NULL
  })

  # Plot 5: Domain moderation (separate figure)
  cat("  Creating domain moderation plot...\n")
  p_moderation <- tryCatch({
    plot_domain_moderation_specific(
      models_list_long = prefs_models$longitudinal,
      data_long = prefs_models$data %>% filter(study_id == "longitudinal"),
      moderation_results_long = wrap_moderation(moderation_results)
    )
  }, error = function(e) {
    cat("    Note: Skipping domain moderation plot:", conditionMessage(e), "\n")
    NULL
  })

  # Save moderation plot separately
  if (!is.null(p_moderation)) {
    save_plot(FIGURE_DIR, p_moderation, "preferences_domain_moderation", width = 12, height = 6)
  }

  # Combine plots using cowplot - matching original layout
  cat("  Combining plots...\n")

  # Top row: Forest (A) + Lambda (B) with rel_widths = c(1, 3)
  top_row <- plot_grid(
    p_forest, p_lambda,
    ncol = 2,
    labels = c("A", "B"),
    rel_widths = c(1, 3),
    label_size = label_size
  )

  # Legend row: forest_legend + trends_legend
  if (!is.null(forest_legend) && exists("trends_legend") && !is.null(trends_legend)) {
    legend_row <- plot_grid(
      NULL, forest_legend, trends_legend, NULL,
      ncol = 4,
      rel_widths = c(0.1, 1, 1, 0.1)
    )
  } else if (!is.null(forest_legend)) {
    legend_row <- plot_grid(forest_legend, ncol = 1)
  } else {
    legend_row <- NULL
  }

  # Bottom row: Time forest (C) + Trends (D) with rel_widths = c(1, 3)
  bottom_row <- plot_grid(
    p_time_forest, p_trends,
    ncol = 2,
    rel_widths = c(1, 3),
    labels = c("C", "D"),
    label_size = label_size
  )

  # Combined figure
  combined_figure <- plot_grid(
    top_row,
    legend_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1.5, 0.2, 1.5)
  )

  # Add arrow annotation for "Session 1" (commented out - using endpoint labels instead)
  # combined_figure_with_arrow <- ggdraw(combined_figure) +
  #   draw_line(
  #     x = c(0.513, 0.513),
  #     y = c(0.97, 0.91),
  #     color = "grey60",
  #     size = 1,
  #     arrow = arrow(length = unit(0.3, "cm"), type = "closed")
  #   ) +
  #   draw_label(
  #     "Session 1",
  #     x = 0.513,
  #     y = 0.98,
  #     color = "grey60",
  #     size = 16,
  #     hjust = 0.5
  #   )

  # Save
  save_plot(FIGURE_DIR, combined_figure, config$output_file, width = 16, height = 13)

  cat("  Done!\n")
  return(combined_figure)
}

# =============================================================================
# Generate Attachment Core Plots
# =============================================================================

generate_attachment_plots <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating ATTACHMENT (core) plots\n")
  cat(strrep("=", 60), "\n\n")

  config <- PLOT_CONFIG$attachment
  # All 4 outcomes for forest plots
  all_outcomes <- c("separation_distress", "perceived_understanding", "reliance", "self_disclosure")
  # Only 2 outcomes for lambda and trends plots
  lambda_trends_outcomes <- c("separation_distress", "perceived_understanding")

  label_size <- 30

  # Load JSON contrasts
  json_path <- file.path(STATS_DIR, config$contrasts_file)
  json_data <- load_contrasts_json(json_path)

  # Load models
  models <- load_all_models(config)
  att_models <- models$attachment_core

  # Extract results from JSON (all outcomes for forest plots)
  rs_results <- extract_rs(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  domain_results <- extract_domain(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  pers_results <- extract_pers(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  temporal_results <- extract_temporal(json_data, all_outcomes, "week_numeric", P_VALUE_COL)
  polynomial_results <- extract_polynomial(json_data, lambda_trends_outcomes, P_VALUE_COL)

  # Plot 1: Lambda predictions (only 2 outcomes: separation_distress, perceived_understanding)
  cat("  Creating lambda prediction plot...\n")
  p_lambda <- plot_lambda_means_with_predictions(
    mods_longitudinal = att_models$longitudinal,
    data_longitudinal = att_models$data %>% filter(study_id == "longitudinal"),
    construct_names = lambda_trends_outcomes,
    custom_order = lambda_trends_outcomes,
    time_var = "week_numeric",
    family_results = wrap_polynomial(polynomial_results),
    free_y = FALSE,
    ncol = 2,
    jitter_amount = 0.05,
    legend_position = "none"
  )

  # Plot 2: Treatment effects forest (all 4 outcomes)
  cat("  Creating treatment effects forest plot...\n")
  p_forest <- plot_treatment_effects_forest(
    rs_results_long = rs_results,
    pers_results_long = pers_results,
    domain_results_long = domain_results,
    custom_order = all_outcomes,
    legend_position = "none"
  )

  # Plot 3: Time coefficients forest (all 4 outcomes) - with legend for extraction
  cat("  Creating time coefficients forest plot...\n")
  forest_legend <- NULL
  p_time_forest <- NULL
  if (!is.null(temporal_results)) {
    p_time_forest_with_legend <- plot_time_coeffs_forest(
      temporal_results_long = temporal_results,
      time_var = "week_numeric",
      custom_order = all_outcomes,
      legend_position = "bottom",
      legend_nrow = 2
    )
    forest_legend <- get_legend(p_time_forest_with_legend)

    # Now create without legend for layout
    p_time_forest <- plot_time_coeffs_forest(
      temporal_results_long = temporal_results,
      time_var = "week_numeric",
      custom_order = all_outcomes,
      legend_position = "none"
    )
  }

  # Plot 4: Temporal trends (only 2 outcomes) - with legend for extraction
  cat("  Creating temporal trends plot...\n")
  p_trends <- tryCatch({
    # First create with legend to extract it
    p_trends_with_legend <- plot_temporal_predictions_colored(
      mods_longitudinal = att_models$longitudinal,
      data_longitudinal = att_models$data %>% filter(study_id == "longitudinal"),
      slope_results = json_data,  # Pass JSON directly for slope annotations
      terms = c("lambda"),
      custom_order = lambda_trends_outcomes,
      time_var = "week_numeric",
      add_lines = TRUE,
      legend_position = "bottom",
      legend_nrow = 2
    )
    trends_legend <<- get_legend(p_trends_with_legend)

    # Now create without legend for layout
    plot_temporal_predictions_colored(
      mods_longitudinal = att_models$longitudinal,
      data_longitudinal = att_models$data %>% filter(study_id == "longitudinal"),
      slope_results = json_data,
      terms = c("lambda"),
      custom_order = lambda_trends_outcomes,
      time_var = "week_numeric",
      add_lines = TRUE,
      legend_position = "none"
    )
  }, error = function(e) {
    cat("    Note: Skipping temporal trends plot:", conditionMessage(e), "\n")
    trends_legend <<- NULL
    NULL
  })

  # Combine plots using cowplot - matching original layout
  cat("  Combining plots...\n")

  # Top row: Forest (A) + Lambda (B) with rel_widths = c(2, 3)
  top_row <- plot_grid(
    p_forest, p_lambda,
    ncol = 2,
    labels = c("A", "B"),
    rel_widths = c(2, 3),
    label_size = label_size
  )

  # Legend row: forest_legend + trends_legend
  if (!is.null(forest_legend) && exists("trends_legend") && !is.null(trends_legend)) {
    legend_row <- plot_grid(
      NULL, forest_legend, trends_legend, NULL,
      ncol = 4,
      rel_widths = c(0.1, 1, 1, 0.1)
    )
  } else if (!is.null(forest_legend)) {
    legend_row <- plot_grid(forest_legend, ncol = 1)
  } else {
    legend_row <- NULL
  }

  # Middle row: Time forest (C) + Trends (D) with rel_widths = c(2, 3)
  middle_row <- plot_grid(
    p_time_forest, p_trends,
    ncol = 2,
    rel_widths = c(2, 3),
    labels = c("C", "D"),
    label_size = label_size
  )

  # Combined figure
  combined_figure <- plot_grid(
    top_row,
    legend_row,
    middle_row,
    ncol = 1,
    rel_heights = c(2.3, 0.2, 1.9)
  )

  # Save
  save_plot(FIGURE_DIR, combined_figure, config$output_file, width = 16, height = 12)

  cat("  Done!\n")
  return(combined_figure)
}

# =============================================================================
# Generate Attachment Behavioural Plots
# =============================================================================

generate_attachment_behavioural_plots <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating ATTACHMENT BEHAVIOURAL plots\n")
  cat(strrep("=", 60), "\n\n")

  config <- PLOT_CONFIG$attachment_behavioural
  label_size <- 30

  # Load JSON contrasts
  json_path <- file.path(STATS_DIR, config$contrasts_file)
  json_data <- load_contrasts_json(json_path)

  # Load models for each local family
  models <- load_all_models(config)
  goodbye_models <- models$goodbye
  seeking_models <- models$seeking_companionship

  # Load combined study models for study comparison plots
  goodbye_combined_path <- file.path(MODEL_DIR, "goodbye_combined_study.rds")
  seeking_combined_path <- file.path(MODEL_DIR, "seeking_companionship_combined_study.rds")

  # GOODBYE plots
  goodbye_outcomes <- c("goodbye_action")

  goodbye_polynomial <- extract_polynomial(json_data, goodbye_outcomes, P_VALUE_COL)
  goodbye_rs <- extract_rs(json_data, goodbye_outcomes, "longitudinal", P_VALUE_COL)
  goodbye_domain <- extract_domain(json_data, goodbye_outcomes, "longitudinal", P_VALUE_COL)
  goodbye_pers <- extract_pers(json_data, goodbye_outcomes, "longitudinal", P_VALUE_COL)

  # Goodbye lambda plot
  cat("  Creating goodbye lambda prediction plot...\n")
  p_goodbye_lambda <- tryCatch({
    goodbye_long <- goodbye_models$data %>%
      filter(study_id == "longitudinal")

    plot_lambda_means_with_predictions(
      mods_longitudinal = goodbye_models$longitudinal,
      data_longitudinal = goodbye_long,
      construct_names = goodbye_outcomes,
      custom_order = goodbye_outcomes,
      time_var = "session_numeric",
      family_results = wrap_polynomial(goodbye_polynomial),
      free_y = FALSE,
      ncol = 1,
      jitter_amount = 0,
      y_label = "Predicted Probability",
      legend_position = "none"
    )
  }, error = function(e) {
    cat("    Note: Skipping goodbye lambda plot:", conditionMessage(e), "\n")
    NULL
  })

  # Goodbye treatment forest
  cat("  Creating goodbye treatment effects plot...\n")
  p_goodbye_forest <- plot_treatment_effects_forest(
    rs_results_long = goodbye_rs,
    pers_results_long = goodbye_pers,
    domain_results_long = goodbye_domain,
    custom_order = goodbye_outcomes,
    legend_position = "none",
    convert_odds_ratios = TRUE
  )

  # Goodbye study comparison
  cat("  Creating goodbye study comparison plot...\n")
  p_goodbye_study <- tryCatch({
    if (file.exists(goodbye_combined_path)) {
      goodbye_combined_model <- readRDS(goodbye_combined_path)
      result <- plot_group_comparison(
        model = goodbye_combined_model,
        group_var = "study_type",
        group_labels = c(
          "cross-sectional" = "Single Exposure\nParticipants",
          "longitudinal" = "Month Exposure\nParticipants"
        ),
        title = "Goodbye Rates by\nStudy Type",
        xlab = "Study Type",
        ylab = "Predicted Probability",
        use_response_scale = TRUE
      )
      result$plot
    } else {
      cat("    Note: Combined study model not found\n")
      NULL
    }
  }, error = function(e) {
    cat("    Note: Skipping goodbye study comparison:", conditionMessage(e), "\n")
    NULL
  })

  # SEEKING COMPANIONSHIP plots
  seeking_outcomes <- c("seeking_companionship_likelihood")

  seeking_polynomial <- extract_polynomial(json_data, seeking_outcomes, P_VALUE_COL)
  seeking_rs <- extract_rs(json_data, seeking_outcomes, "longitudinal", P_VALUE_COL)
  seeking_domain <- extract_domain(json_data, seeking_outcomes, "longitudinal", P_VALUE_COL)
  seeking_pers <- extract_pers(json_data, seeking_outcomes, "longitudinal", P_VALUE_COL)

  # Seeking lambda plot
  cat("  Creating seeking companionship lambda prediction plot...\n")
  p_seeking_lambda <- tryCatch({
    seeking_long <- seeking_models$data %>%
      filter(study_id == "longitudinal")

    plot_lambda_means_with_predictions(
      mods_longitudinal = seeking_models$longitudinal,
      data_longitudinal = seeking_long,
      construct_names = seeking_outcomes,
      custom_order = seeking_outcomes,
      time_var = "session_numeric",
      family_results = wrap_polynomial(seeking_polynomial),
      free_y = TRUE,
      ncol = 1,
      jitter_amount = 0,
      y_label = "Predicted Mean",
      legend_position = "none"
    )
  }, error = function(e) {
    cat("    Note: Skipping seeking companionship lambda plot:", conditionMessage(e), "\n")
    NULL
  })

  # Seeking treatment forest
  cat("  Creating seeking companionship treatment effects plot...\n")
  p_seeking_forest <- plot_treatment_effects_forest(
    rs_results_long = seeking_rs,
    pers_results_long = seeking_pers,
    domain_results_long = seeking_domain,
    custom_order = seeking_outcomes,
    legend_position = "none"
  )

  # Seeking study comparison
  cat("  Creating seeking companionship study comparison plot...\n")
  p_seeking_study <- tryCatch({
    if (file.exists(seeking_combined_path)) {
      seeking_combined_model <- readRDS(seeking_combined_path)
      result <- plot_group_comparison(
        model = seeking_combined_model,
        group_var = "study_type",
        group_labels = c(
          "cross-sectional" = "Single Exposure\nParticipants",
          "longitudinal" = "Month Exposure\nParticipants"
        ),
        title = "Future Companionship Desire by\nStudy Type",
        xlab = "Study Type",
        ylab = "Predicted Mean",
        use_response_scale = FALSE
      )
      result$plot
    } else {
      cat("    Note: Combined study model not found\n")
      NULL
    }
  }, error = function(e) {
    cat("    Note: Skipping seeking study comparison:", conditionMessage(e), "\n")
    NULL
  })

  # Create legend from seeking forest (with legend)
  treatment_legend <- get_legend(
    plot_treatment_effects_forest(
      rs_results_long = seeking_rs,
      pers_results_long = seeking_pers,
      domain_results_long = seeking_domain,
      custom_order = seeking_outcomes,
      legend_position = "bottom",
      legend_nrow = 1
    )
  )

  # Combine in layout matching old notebook
  cat("  Combining plots...\n")

  # Top row: Goodbye (forest, lambda, study comparison) - order matters!
  top_row <- plot_grid(
    p_goodbye_forest, p_goodbye_lambda, p_goodbye_study,
    ncol = 3, rel_widths = c(1, 1.25, 1),
    labels = c("A", "B", "C"),
    label_size = label_size
  )

  # Legend row
  legend_row <- plot_grid(
    NULL,
    treatment_legend,
    NULL,
    ncol = 3,
    rel_widths = c(0.1, 1, 0.1)
  )

  # Bottom row: Seeking companionship (forest, lambda, study comparison)
  bottom_row <- plot_grid(
    p_seeking_forest, p_seeking_lambda, p_seeking_study,
    ncol = 3, rel_widths = c(1, 1.25, 1.05),
    labels = c("D", "E", "F"),
    label_size = label_size
  )

  # Combined figure
  combined_figure <- plot_grid(
    top_row,
    legend_row,
    bottom_row,
    ncol = 1, rel_heights = c(2, 0.2, 2)
  )

  # Save
  save_plot(FIGURE_DIR, combined_figure, config$output_file, width = 16, height = 10)

  cat("  Done!\n")
  return(combined_figure)
}

# =============================================================================
# Generate Wellbeing Plots
# =============================================================================

generate_wellbeing_plots <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating WELLBEING plots\n")
  cat(strrep("=", 60), "\n\n")

  config <- PLOT_CONFIG$wellbeing
  label_size <- 30

  # Load JSON contrasts from separate files (psychosocial and momentary_affect have separate FDR families)
  json_psych <- load_contrasts_json(file.path(STATS_DIR, config$local_families$psychosocial$contrasts_file))
  json_affect <- load_contrasts_json(file.path(STATS_DIR, config$local_families$momentary_affect$contrasts_file))

  # Load models
  models <- load_all_models(config)
  psychosocial_models <- models$psychosocial
  mood_models <- models$momentary_affect

  # PSYCHOSOCIAL plots
  psychosocial_outcomes <- c("psychosocial_F1", "psychosocial_F2")

  rs_results_psych <- extract_rs(json_psych, psychosocial_outcomes, "longitudinal", P_VALUE_COL)
  domain_results_psych <- extract_domain(json_psych, psychosocial_outcomes, "longitudinal", P_VALUE_COL)
  pers_results_psych <- extract_pers(json_psych, psychosocial_outcomes, "longitudinal", P_VALUE_COL)

  cat("  Creating psychosocial treatment effects plot...\n")
  p_psychosocial_forest <- plot_treatment_effects_forest(
    rs_results_long = rs_results_psych,
    pers_results_long = pers_results_psych,
    domain_results_long = domain_results_psych,
    custom_order = psychosocial_outcomes,
    legend_position = "none"
  )

  # Psychosocial domain × study ANOVA plot
  cat("  Creating psychosocial domain × study ANOVA plot...\n")
  p_psychosocial_study <- tryCatch({
    # Combine longitudinal and cross-sectional data
    # NOTE: This data transformation should eventually be moved to psychosocial.R
    # to output data in the format expected by plot_anova_study_by_domain
    psych_data <- psychosocial_models$data %>%
      mutate(
        construct = outcome_measure,  # Rename to expected column name
        # Create outcome_value_pre by selecting the appropriate pre column
        outcome_value_pre = case_when(
          outcome_measure == "psychosocial_F1" ~ psychosocial_F1_pre,
          outcome_measure == "psychosocial_F2" ~ psychosocial_F2_pre,
          TRUE ~ NA_real_
        )
      )

    data_combined <- bind_rows(
      psych_data %>% filter(study_id == "longitudinal") %>% mutate(study_type = "longitudinal"),
      psych_data %>% filter(study_id == "cross-sectional") %>% mutate(study_type = "cross-sectional")
    )

    results <- plot_anova_study_by_domain(
      data_combined,
      construct_filter = psychosocial_outcomes,
      plot_overall = FALSE
    )
    results$plot
  }, error = function(e) {
    cat("    Note: Skipping psychosocial study plot:", conditionMessage(e), "\n")
    NULL
  })

  # MOMENTARY AFFECT plots
  mood_outcomes <- c("arousal", "valence")  # Order: arousal first

  rs_results_mood <- extract_rs(json_affect, mood_outcomes, "longitudinal", P_VALUE_COL)
  pers_results_mood <- extract_pers(json_affect, mood_outcomes, "longitudinal", P_VALUE_COL)
  temporal_results_mood <- extract_temporal(json_affect, mood_outcomes, "session_numeric", P_VALUE_COL)

  cat("  Creating mood treatment effects plot...\n")
  p_mood_forest <- plot_treatment_effects_forest(
    rs_results_long = rs_results_mood,
    pers_results_long = pers_results_mood,
    custom_order = mood_outcomes,
    legend_position = "none"
  )

  cat("  Creating mood time coefficients plot...\n")
  p_mood_time <- NULL
  if (!is.null(temporal_results_mood)) {
    p_mood_time <- plot_time_coeffs_forest(
      temporal_results_long = temporal_results_mood,
      time_var = "session_numeric",
      custom_order = mood_outcomes,
      legend_position = "none"
    )
  }

  # Mood trends - only valence with delta from S1 baseline
  cat("  Creating mood temporal trends plot (valence only, delta from S1)...\n")
  p_mood_trends <- tryCatch({
    mood_data_long <- mood_models$data %>% filter(study_id == "longitudinal")
    plot_temporal_predictions_colored(
      mods_longitudinal = mood_models$longitudinal,
      data_longitudinal = mood_data_long,
      slope_results = json_affect,
      terms = c("lambda"),
      custom_order = c("valence"),  # Only valence
      time_var = "session_numeric",
      add_lines = TRUE,
      ncol = 1,
      legend_position = "none",
      plot_delta_from_s1_baseline = TRUE  # Delta from S1 baseline
    )
  }, error = function(e) {
    cat("    Note: Skipping mood trends plot:", conditionMessage(e), "\n")
    NULL
  })

  # Create legends (2 rows to match old notebook)
  # Get forest legend from attachment time forest (which has domain AND time)
  # Load attachment JSON for legend
  attachment_json <- load_contrasts_json(file.path(STATS_DIR, "attachment_contrasts.json"))
  attachment_outcomes <- c("perceived_understanding", "separation_distress", "reliance", "self_disclosure")
  attachment_temporal <- extract_temporal(attachment_json, attachment_outcomes, "week_numeric", P_VALUE_COL)

  forest_legend <- get_legend(
    plot_time_coeffs_forest(
      temporal_results_long = attachment_temporal,
      time_var = "week_numeric",
      custom_order = attachment_outcomes,
      legend_position = "bottom",
      legend_nrow = 2
    )
  )

  # Trends legend from temporal predictions (also 2 rows)
  trends_legend <- tryCatch({
    get_legend(
      plot_temporal_predictions_colored(
        mods_longitudinal = mood_models$longitudinal,
        data_longitudinal = mood_models$data %>% filter(study_id == "longitudinal"),
        slope_results = json_affect,
        terms = c("lambda"),
        custom_order = c("valence"),
        time_var = "session_numeric",
        add_lines = TRUE,
        ncol = 1,
        legend_position = "bottom",
        legend_nrow = 2
      )
    )
  }, error = function(e) NULL)

  # Study type legend
  study_type_legend <- tryCatch({
    get_legend(study_legend())
  }, error = function(e) NULL)

  # Combine plots - matching old notebook layout
  cat("  Combining plots...\n")

  # Top row: psychosocial forest + domain × study ANOVA
  top_row <- plot_grid(
    p_psychosocial_forest,
    p_psychosocial_study,
    ncol = 2,
    labels = c("A", "B"),
    rel_widths = c(0.75, 1.25),
    label_size = label_size
  )

  # Bottom row: mood forest, time coeffs, trends
  bottom_row <- plot_grid(
    p_mood_forest,
    p_mood_time,
    p_mood_trends,
    ncol = 3,
    rel_widths = c(1, 1, 1.5),
    labels = c("C", "D", "E"),
    label_size = label_size
  )

  # Legend row
  legend_row <- plot_grid(
    NULL,
    forest_legend,
    trends_legend,
    NULL,
    ncol = 4,
    rel_widths = c(0.2, 1.2, 0.9, 0.0)
  )

  # Combined figure
  combined_figure <- plot_grid(
    top_row,
    bottom_row,
    legend_row,
    nrow = 3, rel_heights = c(1.2, 0.9, 0.15)
  )

  # Add study type legend annotation
  if (!is.null(study_type_legend)) {
    combined_figure <- combined_figure +
      annotation_custom(study_type_legend, xmin = 0.7, xmax = 1.0, ymin = 0.85, ymax = 1.0)
  }

  # Save
  save_plot(FIGURE_DIR, combined_figure, config$output_file, width = 16, height = 8.5)

  cat("  Done!\n")
  return(combined_figure)
}

# =============================================================================
# Generate Perceptions Plots
# =============================================================================

generate_perceptions_plots <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating PERCEPTIONS plots\n")
  cat(strrep("=", 60), "\n\n")

  config <- PLOT_CONFIG$perceptions
  all_outcomes <- get_all_outcomes(config)

  # Load JSON contrasts
  json_path <- file.path(STATS_DIR, config$contrasts_file)
  json_data <- load_contrasts_json(json_path)

  # Load models
  models <- load_all_models(config)
  relational_models <- models$relational
  sentience_models <- models$sentience

  # Extract results from JSON (combined for all outcomes)
  rs_results <- extract_rs(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  domain_results <- extract_domain(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  pers_results <- extract_pers(json_data, all_outcomes, "longitudinal", P_VALUE_COL)
  temporal_results <- extract_temporal(json_data, all_outcomes, "week_numeric", P_VALUE_COL)

  # RELATIONAL plots
  relational_outcomes <- c("tool_friend")
  polynomial_relational <- extract_polynomial(json_data, relational_outcomes, P_VALUE_COL)

  cat("  Creating tool-friend lambda prediction plot...\n")
  p_relational_lambda <- plot_lambda_means_with_predictions(
    mods_longitudinal = relational_models$longitudinal,
    data_longitudinal = relational_models$data %>% filter(study_id == "longitudinal"),
    construct_names = relational_outcomes,
    custom_order = relational_outcomes,
    time_var = "week_numeric",
    family_results = wrap_polynomial(polynomial_relational),
    ncol = 1,
    width = 5, height = 5,
    jitter_amount = 0.05,
    label_endpoints = FALSE,
    legend_position = "none"
  )

  # SENTIENCE plots (perceived first, then ontological to match old notebook)
  sentience_outcomes <- c("perceived_sentience", "ontological_sentience")
  polynomial_sentience <- extract_polynomial(json_data, sentience_outcomes, P_VALUE_COL)

  cat("  Creating sentience lambda prediction plot...\n")
  p_sentience_lambda <- plot_lambda_means_with_predictions(
    mods_longitudinal = sentience_models$longitudinal,
    data_longitudinal = sentience_models$data %>% filter(study_id == "longitudinal"),
    construct_names = sentience_outcomes,
    custom_order = sentience_outcomes,
    time_var = "week_numeric",
    family_results = wrap_polynomial(polynomial_sentience),
    ncol = 2,
    width = 10, height = 5,
    jitter_amount = 0,
    label_endpoints = FALSE,
    legend_position = "none"
  )

  # Combined forest plot (all 3 outcomes)
  cat("  Creating combined treatment effects plot...\n")
  p_forest <- plot_treatment_effects_forest(
    rs_results_long = rs_results,
    pers_results_long = pers_results,
    domain_results_long = domain_results,
    custom_order = c("tool_friend", "perceived_sentience", "ontological_sentience"),
    legend_position = "none"
  )

  # Create legend from forest (with legend)
  forest_legend <- get_legend(
    plot_treatment_effects_forest(
      rs_results_long = rs_results,
      pers_results_long = pers_results,
      domain_results_long = domain_results,
      custom_order = c("tool_friend", "perceived_sentience", "ontological_sentience"),
      legend_position = "bottom",
      legend_nrow = 1
    )
  )

  # Combine plots - matching old notebook layout
  # Top row: forest (A) + tool_friend_lambda (B) + sentience_lambda (C)
  cat("  Combining plots...\n")
  label_size <- 30

  top_row <- plot_grid(
    p_forest,
    p_relational_lambda,
    p_sentience_lambda,
    ncol = 3,
    labels = c("A", "B", "C"),
    rel_widths = c(1.2, 1, 2),
    label_size = label_size
  )

  # Legend row
  legend_row <- plot_grid(
    NULL,
    forest_legend,
    NULL,
    ncol = 3,
    rel_widths = c(0.1, 1, 0.1)
  )

  # Combined figure
  combined_figure <- plot_grid(
    top_row,
    legend_row,
    ncol = 1, rel_heights = c(1, 0.1)
  )

  # Save
  save_plot(FIGURE_DIR, combined_figure, config$output_file, width = 16, height = 6)

  cat("  Done!\n")
  return(combined_figure)
}

# =============================================================================
# Generate Markdown Report
# =============================================================================

generate_report <- function() {
  report_dir <- file.path(PROJECT_ROOT, "reports/main_studies")
  report_path <- file.path(report_dir, "paper_plots.md")

  # Define plot order
  plot_order <- c(
    "calibration_study.png",
    "preferences_combined.png",
    "preferences_domain_moderation.png",
    "attachment_combined.png",
    "attachment_behavioural_combined.png",
    "wellbeing_combined.png",
    "perceptions_combined.png"
  )

  # Filter to only existing files
  png_files <- plot_order[file.exists(file.path(FIGURE_DIR, "png", plot_order))]

  # Start building report
  lines <- c(
    "# Main Paper Plots Report",
    "",
    paste0("Generated: ", Sys.time()),
    "",
    paste0("P-value column: `", P_VALUE_COL, "`"),
    "",
    "---",
    ""
  )

  # Add each plot to report
  plot_info <- list(
    calibration_study = list(
      title = "Calibration Study",
      description = "Validation of relationship-seeking multiplier on coherence (off-target) vs relationship-seeking (on-target)",
      panels = c("Marginal predictions showing on-target effect on relationship-seeking ratings")
    ),
    preferences_combined = list(
      title = "Preferences",
      description = "Outcomes: engagingness, likeability, helpfulness",
      panels = c("A: Treatment effects forest", "B: Lambda predictions", "C: Time coefficients forest", "D: Temporal trends")
    ),
    preferences_domain_moderation = list(
      title = "Preferences Domain Moderation",
      description = "Domain × Relationship-Seeking interaction for helpfulness",
      panels = c("Domain moderation effect on lambda predictions")
    ),
    attachment_combined = list(
      title = "Attachment (Core)",
      description = "Outcomes: reliance, perceived_understanding, self_disclosure, separation_distress",
      panels = c("A: Lambda predictions", "B: Temporal trends", "C: Treatment effects forest", "D: Time coefficients forest")
    ),
    attachment_behavioural_combined = list(
      title = "Attachment (Behavioural)",
      description = "Outcomes: goodbye_action, seeking_companionship_likelihood",
      panels = c("A: Goodbye lambda predictions", "B: Goodbye treatment effects", "C: Seeking companionship lambda", "D: Seeking companionship treatment effects")
    ),
    wellbeing_combined = list(
      title = "Wellbeing",
      description = "Outcomes: psychosocial_F1, psychosocial_F2, valence, arousal",
      panels = c("A: Psychosocial treatment effects", "B: Mood treatment effects", "C: Mood time coefficients", "D: Mood temporal trends")
    ),
    perceptions_combined = list(
      title = "Perceptions",
      description = "Outcomes: tool_friend, ontological_sentience, perceived_sentience",
      panels = c("A: Lambda predictions", "B: Treatment effects forest", "C: Time coefficients forest")
    )
  )

  for (png_file in png_files) {
    base_name <- sub("\\.png$", "", png_file)
    info <- plot_info[[base_name]]

    if (!is.null(info)) {
      lines <- c(lines,
        paste0("## ", info$title),
        "",
        paste0("**", info$description, "**"),
        "",
        "Panels:",
        paste0("- ", info$panels),
        "",
        paste0("![", info$title, "](../../outputs/figures/paper_plots/png/", png_file, ")"),
        "",
        "---",
        ""
      )
    }
  }

  # Write report
  writeLines(lines, report_path)
  cat("\nReport saved to:", report_path, "\n")
}

# =============================================================================
# Generate Calibration Study Plot
# =============================================================================

generate_calibration_study_plot <- function() {
  cat("\n", strrep("=", 60), "\n")
  cat("Generating CALIBRATION STUDY plot\n")
  cat(strrep("=", 60), "\n\n")

  # Load calibration best models
  calibration_model_path <- file.path(MODEL_DIR, "calibration_study/calibration_best_models.rds")
  calibration_effects_path <- file.path(MODEL_DIR, "calibration_study/calibration_combined_effects.rds")
  calibration_data_path <- file.path(PROJECT_ROOT, "analysis_code/stats_data/calibration_study/single-chat.csv")

  if (!file.exists(calibration_model_path) && !file.exists(calibration_effects_path)) {
    cat("  Note: Calibration study models not found. Run calibration_study.R first.\n")
    cat("  Expected path:", calibration_model_path, "\n")
    return(NULL)
  }

  # Load models to extract beta coefficients
  calibration_models <- readRDS(calibration_model_path)
  best_model_coherence <- calibration_models$coherence
  best_model_rs <- calibration_models$relationship_seeking

  # Extract beta coefficients and p-values (fixed effects only)
  params_coherence <- model_parameters(best_model_coherence, ci = 0.95, effects = "fixed")
  params_rs <- model_parameters(best_model_rs, ci = 0.95, effects = "fixed")

  coef_coherence <- params_coherence$Coefficient[params_coherence$Parameter == "multiplier"]
  coef_rs <- params_rs$Coefficient[params_rs$Parameter == "multiplier"]
  p_coherence <- params_coherence$p[params_coherence$Parameter == "multiplier"]
  p_rs <- params_rs$p[params_rs$Parameter == "multiplier"]

  # Format significance
  format_sig <- function(p) {
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    return(" (ns)")
  }

  beta_coherence_label <- sprintf("%.2f%s", coef_coherence, format_sig(p_coherence))
  beta_rs_label <- sprintf("%.2f%s", coef_rs, format_sig(p_rs))

  cat("  Beta coefficients:\n")
  cat("    Coherence:", beta_coherence_label, "\n")
  cat("    Relationship-Seeking:", beta_rs_label, "\n")

  # Load combined effects (pre-computed by calibration_study.R)
  if (file.exists(calibration_effects_path)) {
    combined_effects <- readRDS(calibration_effects_path)
  } else {
    effects_coherence <- ggeffects::ggpredict(best_model_coherence, terms = "multiplier [all]") %>%
      data.frame() %>%
      mutate(model = "Coherence (Off-Target)")

    effects_rs <- ggeffects::ggpredict(best_model_rs, terms = "multiplier [all]") %>%
      data.frame() %>%
      mutate(model = "Relationship-Seeking (On-Target)")

    combined_effects <- bind_rows(effects_coherence, effects_rs) %>%
      filter(x >= -1 & x <= 1)
  }

  # Load raw data and compute summary statistics
  calibration_summary <- NULL
  if (file.exists(calibration_data_path)) {
    cat("  Loading raw calibration data for overlay...\n")
    calibration_data <- read.csv(calibration_data_path)

    calibration_summary <- calibration_data %>%
      rowwise() %>%
      mutate(
        individual_coherence = mean(c_across(starts_with("coherence.")), na.rm = TRUE),
        individual_steerability = mean(c_across(starts_with("steerability.")), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(multiplier >= -1 & multiplier <= 1) %>%
      group_by(multiplier) %>%
      summarise(
        mean_coherence = mean(individual_coherence, na.rm = TRUE),
        se_coherence = sd(individual_coherence, na.rm = TRUE) / sqrt(n()),
        mean_steerability = mean(individual_steerability, na.rm = TRUE),
        se_steerability = sd(individual_steerability, na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
  }

  # Create smooth gradient data for relationship-seeking line
  rs_data <- combined_effects %>% filter(model == "Relationship-Seeking (On-Target)")
  smooth_rs <- data.frame(x = seq(min(rs_data$x), max(rs_data$x), length.out = 100))
  smooth_rs$predicted <- approx(rs_data$x, rs_data$predicted, smooth_rs$x)$y

  # Colors: purple for RS, gray for coherence
  colors <- c("#6f00b9", "#535252")

  # Create plot
  p_calibration <- ggplot() +
    # Confidence ribbon for coherence (gray)
    geom_ribbon(
      data = combined_effects %>% filter(model == "Coherence (Off-Target)"),
      aes(x = x, ymin = conf.low, ymax = conf.high),
      alpha = 0.2,
      fill = colors[2]
    ) +
    # Confidence ribbon for relationship-seeking (purple/plum)
    geom_ribbon(
      data = combined_effects %>% filter(model == "Relationship-Seeking (On-Target)"),
      aes(x = x, ymin = conf.low, ymax = conf.high),
      alpha = 0.3,
      fill = "plum"
    )

  # Add raw data points if available
  if (!is.null(calibration_summary)) {
    p_calibration <- p_calibration +
      # Error bars for coherence (±1 SE)
      geom_errorbar(
        data = calibration_summary,
        aes(x = multiplier,
            ymin = mean_coherence - se_coherence,
            ymax = mean_coherence + se_coherence),
        color = "black",
        width = 0.05
      ) +
      # Error bars for steerability (±1 SE)
      geom_errorbar(
        data = calibration_summary,
        aes(x = multiplier,
            ymin = mean_steerability - se_steerability,
            ymax = mean_steerability + se_steerability),
        color = "black",
        width = 0.05
      ) +
      # Data points for coherence
      geom_point(
        data = calibration_summary,
        aes(x = multiplier, y = mean_coherence),
        color = "black",
        size = 4
      ) +
      # Data points for steerability
      geom_point(
        data = calibration_summary,
        aes(x = multiplier, y = mean_steerability),
        color = "black",
        size = 4
      )
  }

  p_calibration <- p_calibration +
    # Coherence line (gray, dashed)
    geom_line(
      data = combined_effects %>% filter(model == "Coherence (Off-Target)"),
      aes(x = x, y = predicted),
      linewidth = 1.5,
      linetype = "dashed",
      color = colors[2],
      alpha = 0.9
    ) +
    # Relationship-seeking line with color gradient
    geom_line(
      data = smooth_rs,
      aes(x = x, y = predicted, color = x),
      linewidth = 2.5
    ) +
    # Color scale for gradient line
    scale_color_gradientn(
      colors = c("#0000FF", "#4040FF", "#808080", "#808080", "#808080", "#FF4040", "#FF0000"),
      values = scales::rescale(c(-1.0, -0.3, -0.1, 0.0, 0.1, 0.3, 1.0)),
      guide = "none"
    ) +
    # Scales
    scale_x_continuous(
      breaks = c(-1, -0.5, 0, 0.5, 1),
      labels = c("-1.0", "-0.5", "0.0", "+0.5", "+1.0")
    ) +
    scale_y_continuous(
      limits = c(20, 100),
      breaks = seq(20, 100, by = 20)
    ) +
    # Text annotations with beta coefficients
    annotate("text",
      x = 0, y = 98,
      label = "Coherence (Selectivity)",
      color = colors[2], size = 8, fontface = "bold"
    ) +
    annotate("text",
      x = 0, y = 93,
      label = paste0("beta[lambda] == '", beta_coherence_label, "'"),
      parse = TRUE,
      color = colors[2], size = 8, fontface = "bold"
    ) +
    annotate("text",
      x = 0, y = 34,
      label = "Relationship-Seeking (Efficacy)",
      color = colors[1], size = 8, fontface = "bold"
    ) +
    annotate("text",
      x = 0, y = 29,
      label = paste0("beta[lambda] == '", beta_rs_label, "'"),
      parse = TRUE,
      color = colors[1], size = 8, fontface = "bold"
    ) +
    theme_minimal(base_size = 14) +
    theme_pub() +
    theme(
      axis.text = element_text(size = 22),
      axis.title.x = element_text(size = 28),
      axis.title.y = element_text(size = 24),
      axis.line.x = element_line(colour = "black")
    ) +
    labs(
      x = expression(lambda),
      y = "Predicted Mean (0-100 scale)"
    )

  # Save
  save_plot(FIGURE_DIR, p_calibration, "calibration_study", width = 7.5, height = 10)

  cat("  Done!\n")
  return(p_calibration)
}

# =============================================================================
# Main Execution
# =============================================================================

main <- function() {
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("MAIN PAPER PLOTS GENERATION\n")
  cat("Loading from JSON contrasts files\n")
  cat(strrep("=", 70), "\n")

  # Check that required directories exist
  if (!dir.exists(STATS_DIR)) {
    stop(paste("JSON directory not found:", STATS_DIR))
  }
  if (!dir.exists(MODEL_DIR)) {
    stop(paste("Model directory not found:", MODEL_DIR))
  }

  # Generate all plots
  tryCatch({
    generate_preferences_plots()
  }, error = function(e) {
    cat("ERROR in preferences plots:", conditionMessage(e), "\n")
  })

  tryCatch({
    generate_attachment_plots()
  }, error = function(e) {
    cat("ERROR in attachment plots:", conditionMessage(e), "\n")
  })

  tryCatch({
    generate_attachment_behavioural_plots()
  }, error = function(e) {
    cat("ERROR in attachment behavioural plots:", conditionMessage(e), "\n")
  })

  tryCatch({
    generate_wellbeing_plots()
  }, error = function(e) {
    cat("ERROR in wellbeing plots:", conditionMessage(e), "\n")
  })

  tryCatch({
    generate_perceptions_plots()
  }, error = function(e) {
    cat("ERROR in perceptions plots:", conditionMessage(e), "\n")
  })

  tryCatch({
    generate_calibration_study_plot()
  }, error = function(e) {
    cat("ERROR in calibration study plot:", conditionMessage(e), "\n")
  })

  # Generate markdown report
  generate_report()

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("DONE! Plots saved to:", FIGURE_DIR, "\n")
  cat(strrep("=", 70), "\n")
}

# Run if executed directly
if (!interactive()) {
  main()
}
