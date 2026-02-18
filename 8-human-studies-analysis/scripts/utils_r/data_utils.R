# data_utils.R
# Data loading and preparation functions for R analyses

library(tidyverse)
library(jsonlite)

# =============================================================================
# DATA LOADING
# =============================================================================

#' Load task data from DATA_DIR as JSONL
#'
#' @param task_name Name of the task (e.g., "anthro-attitudes")
#' @param data_dir Data directory path (e.g., DATA_DIR from path_utils.R)
#' @return data.frame
load_task_data <- function(task_name, data_dir) {
  file_path <- file.path(data_dir, paste0(task_name, ".jsonl"))

  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  con <- file(file_path, "r")
  on.exit(close(con))
  data <- jsonlite::stream_in(con, verbose = FALSE)

  # Sort by available identifiers (not all may be present)
  # Use numeric sorting for ppt_id
  if ("ppt_id" %in% names(data)) {
    data <- data %>%
      mutate(.ppt_num = as.numeric(gsub("[^0-9]", "", ppt_id)))
  }

  sort_cols <- c("study_id", ".ppt_num", "week", "day", "session")
  available_sort_cols <- sort_cols[sort_cols %in% names(data)]
  if (length(available_sort_cols) > 0) {
    data <- data %>%
      arrange(across(all_of(available_sort_cols)))
  }

  # Remove helper column

  if (".ppt_num" %in% names(data)) {
    data <- data %>% select(-.ppt_num)
  }

  cat("Loaded:", task_name, "with", nrow(data), "rows\n")
  data
}

#' Save data as JSONL and print relative path
#'
#' @param data Data frame to save
#' @param filename Name of file (without extension)
#' @param output_dir Directory to save to
#' @param relative_to Base path for relative path display (default: output_dir)
#' @return Full path to saved file (invisible)
save_task_data <- function(data, filename, output_dir, relative_to = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(output_dir, paste0(filename, ".jsonl"))

  con <- file(file_path, "w")
  on.exit(close(con))
  jsonlite::stream_out(data, con, verbose = FALSE)

  # Print relative path
  if (is.null(relative_to)) {
    rel_path <- file_path
  } else {
    rel_path <- sub(
      paste0("^", normalizePath(relative_to), "/?"), "",
      normalizePath(file_path))
  }
  cat("Saved:", rel_path, "\n")

  invisible(file_path)
}


#' Simple load JSONL data file
#'
#' @param file_path Path to JSONL file
#' @return Data frame
load_jsonl <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  con <- file(file_path, "r")
  on.exit(close(con))
  data <- jsonlite::stream_in(con, verbose = FALSE)
  cat("Loaded:", basename(file_path), "with", nrow(data), "rows\n")
  return(data)
}

# =============================================================================
# DATA PREPARATION
# =============================================================================

#' Prepare treatment arms with proper factor levels
#'
#' Creates properly typed treatment arm variables including:
#' - lambda (numeric: -1 to 1)
#' - lambda_factor (factor: neg1, neg0.5, zero, pos0.5, pos1)
#' - relationship_seeking_category (factor: neg_lambda, zero_lambda, pos_lambda)
#' - session_numeric (numeric from session)
#'
#' @param data_raw Raw data frame
#' @return data.frame with factor columns
prepare_treatment_arms <- function(data_raw) {
  data <- data_raw

  # Convert to factors where appropriate
  if ("ppt_id" %in% names(data)) {
    data$ppt_id <- factor(data$ppt_id)
  }

  if ("study_id" %in% names(data)) {
    data$study_id <- factor(data$study_id)
  }

  if ("timepoint" %in% names(data)) {
    data$timepoint <- factor(data$timepoint, levels = c("pre", "post"))
  }

  if ("week" %in% names(data)) {
    data$week_numeric <- as.numeric(gsub("w", "", data$week))
    # Warn if conversion produced NAs (malformed data)
    n_na <- sum(is.na(data$week_numeric) & !is.na(data$week))
    if (n_na > 0) {
      warning(sprintf("week_numeric: %d values became NA during conversion", n_na))
    }
  }

  if ("day" %in% names(data)) {
    data$day <- factor(data$day)
  }

  if ("session" %in% names(data)) {
    data$session <- factor(data$session)
    # Create numeric session variable for random slopes
    data$session_numeric <- as.numeric(gsub("s", "", data$session))
    # Warn if conversion produced NAs (malformed data)
    n_na <- sum(is.na(data$session_numeric) & !is.na(data$session))
    if (n_na > 0) {
      warning(sprintf("session_numeric: %d values became NA during conversion", n_na))
    }
  }

  if ("domain" %in% names(data)) {
    data$domain <- factor(data$domain, levels = c("polchat", "emotchat"))
  }

  if ("personalisation" %in% names(data)) {
    data$personalisation <- factor(data$personalisation, levels = c("non-personalised", "personalised"))
  }

  if ("multiplier" %in% names(data)) {
    # Create 5-level factor with descriptive labels
    data$lambda_factor <- factor(
      data$multiplier,
      levels = c("-1", "-0.5", "0", "0.5", "1"),
      labels = c("neg1", "neg0.5", "zero", "pos0.5", "pos1")
    )

    # Create numeric lambda
    data$lambda <- as.numeric(data$multiplier)

    # Create 3-category relationship-seeking variable (pre-registered)
    data$relationship_seeking_category <- dplyr::case_when(
      data$lambda_factor %in% c("neg1", "neg0.5") ~ "neg_lambda",
      data$lambda_factor == "zero" ~ "zero_lambda",
      data$lambda_factor %in% c("pos0.5", "pos1") ~ "pos_lambda"
    )
    data$relationship_seeking_category <- factor(
      data$relationship_seeking_category,
      levels = c("neg_lambda", "zero_lambda", "pos_lambda")
    )
  }

  return(data)
}

#' Filter to main studies only (cross-sectional and longitudinal)
#'
#' @param data Data frame with study_id column
#' @return Filtered data frame
filter_main_studies <- function(data) {
  data %>%
    filter(study_id %in% c("cross-sectional", "longitudinal")) %>%
    mutate(study_id = droplevels(study_id))
}

#' Filter to pre-treatment timepoint only
#'
#' @param data Data frame with timepoint column
#' @return Filtered data frame
filter_pre_treatment <- function(data) {
  data %>%
    filter(timepoint == "pre")
}

# =============================================================================
# POOLED DATA FUNCTIONS
# =============================================================================

#' Create pooled data format (long format for multiple outcomes)
#'
#' Stacks multiple outcome variables into a single 'outcome_value' column,
#' with 'construct' indicating which construct it belongs to.
#'
#' @param data Data frame with multiple outcome columns
#' @param outcome_vars Character vector of outcome variable names to stack
#' @param construct_mapping Optional named vector mapping outcome_vars to
#'   construct names. Names are outcome_vars, values are construct names.
#'   If NULL (default), uses 1:1 mapping (outcome_var name = construct name).
#'   Example: c(behavioural_reliance = "reliance",
#'              cognitive_reliance = "reliance",
#'              self_disclosure = "self_disclosure")
#' @param include_pre If TRUE, also pivots pre-treatment columns (outcome_var_pre)
#'   into outcome_value_pre. Default FALSE.
#' @return Data frame in long format with construct, outcome_measure, and
#'   outcome_value columns (and outcome_value_pre if include_pre = TRUE)
create_pooled_data <- function(data, outcome_vars, construct_mapping = NULL,
                                include_pre = FALSE) {
  # Check for pre columns if requested
  if (include_pre) {
    pre_vars <- paste0(outcome_vars, "_pre")
    has_pre <- all(pre_vars %in% names(data))
    if (!has_pre) {
      # Check which are missing
      missing <- pre_vars[!pre_vars %in% names(data)]
      warning(sprintf("Pre columns not found: %s. Setting outcome_value_pre to NA.",
                      paste(missing, collapse = ", ")))
    }
  }

  # Pivot outcome values to long format
  pooled <- data %>%
    tidyr::pivot_longer(
      cols = all_of(outcome_vars),
      names_to = "outcome_measure",
      values_to = "outcome_value"
    )

  # If include_pre, add outcome_value_pre by matching on outcome_measure

  if (include_pre) {
    pre_vars <- paste0(outcome_vars, "_pre")
    if (all(pre_vars %in% names(data))) {
      # Determine join keys (include study_id if present to avoid cross-matching)
      # In practice ppt_ids are unique across studies, but include for safety
      join_keys <- if ("study_id" %in% names(data)) {
        c("ppt_id", "study_id", "outcome_measure")
      } else {
        c("ppt_id", "outcome_measure")
      }

      # Create a lookup from the original data
      pre_lookup <- data %>%
        tidyr::pivot_longer(
          cols = all_of(pre_vars),
          names_to = "outcome_measure_pre",
          values_to = "outcome_value_pre"
        ) %>%
        mutate(
          # Map pre column name to outcome_measure name
          outcome_measure = gsub("_pre$", "", outcome_measure_pre)
        ) %>%
        select(all_of(c(join_keys, "outcome_value_pre")))

      pooled <- pooled %>%
        left_join(pre_lookup, by = join_keys)
    } else {
      # No pre columns - set to NA
      pooled <- pooled %>%
        mutate(outcome_value_pre = NA_real_)
    }
  }

  # Warn about NA values before filtering
  n_na <- sum(is.na(pooled$outcome_value))
  if (n_na > 0) {
    warning(sprintf("Dropping %d rows with NA outcome values", n_na))
  }
  pooled <- pooled %>% filter(!is.na(outcome_value))

  # Apply construct mapping
  if (is.null(construct_mapping)) {
    # 1:1 mapping: construct = outcome_measure
    pooled <- pooled %>%
      mutate(construct = outcome_measure)
    construct_levels <- outcome_vars
  } else {
    # Custom mapping: look up construct from outcome_measure
    pooled <- pooled %>%
      mutate(construct = construct_mapping[outcome_measure])
    construct_levels <- unique(construct_mapping[outcome_vars])
  }

  # Factor construct with proper ordering
  pooled <- pooled %>%
    mutate(
      outcome_measure = factor(outcome_measure, levels = outcome_vars),
      construct = factor(construct, levels = construct_levels)
    )

  # Sort by available identifiers (not all may be present)
  # Use numeric sorting for ppt_id
  if ("ppt_id" %in% names(pooled)) {
    pooled <- pooled %>%
      mutate(.ppt_num = as.numeric(gsub("[^0-9]", "", ppt_id)))
  }

  sort_cols <- c("study_id", ".ppt_num", "week", "day", "session", "outcome_measure")
  available_sort_cols <- sort_cols[sort_cols %in% names(pooled)]
  pooled <- pooled %>%
    arrange(across(all_of(available_sort_cols)))

  # Remove helper column
  if (".ppt_num" %in% names(pooled)) {
    pooled <- pooled %>% select(-.ppt_num)
  }

  pooled
}

#' Print dataset summary (participants and observations)
#'
#' Prints a summary of the dataset including participant counts and
#' observation counts per outcome variable by study.
#'
#' @param data Data frame with ppt_id and study_id columns
#' @param outcome_vars Character vector of outcome variable names
#' @return NULL (prints to console)
print_dataset_summary <- function(data, outcome_vars) {
  cat("\nDataset Summary:\n")

  # Participants by study
  ppt_summary <- data %>%
    distinct(ppt_id, study_id) %>%
    group_by(study_id) %>%
    summarise(n_participants = n(), .groups = "drop")
  cat("\nParticipants by study:\n")
  print(knitr::kable(ppt_summary))

  # Observations per outcome per study
  cat("\nObservations per outcome:\n")
  obs_summary <- data %>%
    group_by(study_id) %>%
    summarise(
      across(all_of(outcome_vars), ~sum(!is.na(.)), .names = "n_obs_{col}"),
      .groups = "drop"
    )
  print(knitr::kable(obs_summary))

  invisible(NULL)
}

# =============================================================================
# PARTICIPANT CHARACTERISTICS (COMBINED LOADING)
# =============================================================================

#' Load all participant characteristics for analysis
#'
#' Loads and merges all participant-level data:
#'   - Sociodemographics (from raw data)
#'   - Preference types (from generated data)
#'   - IPW weights (from generated data)
#'   - Psychosocial factors (from generated data, optional)
#'
#' Returns one unique row per (ppt_id, study_id).
#'
#' @param data_dir Path to raw data directory
#' @param generated_data_dir Path to outputs/generated_data_files directory
#' @param include_psychosocial Whether to include psychosocial factors
#' @param apply_coarsening Whether to apply coarsen_and_factor_sociodemos
#' @return Data frame with one row per participant
load_ppt_characteristics <- function(data_dir,
                                      generated_data_dir,
                                      include_psychosocial = TRUE,
                                      apply_coarsening = TRUE) {
  cat("\n--- Loading Participant Characteristics ---\n")

  # Treatment variables to exclude from sociodemographics
  # (these are already saved in outcome data files)
  treatment_vars <- c(
    "multiplier", "domain", "personalisation", "timepoint",
    "n_timepoints", "week", "day", "session"
  )

  # Helper to assert one row per (ppt_id, study_id)
  assert_unique <- function(df, name) {
    n_rows <- nrow(df)
    n_unique <- nrow(distinct(df, ppt_id, study_id))
    if (n_rows != n_unique) {
      stop(name, " has duplicates: ", n_rows, " rows but ",
           n_unique, " unique (ppt_id, study_id)")
    }
    df
  }

  # 1. Load sociodemographics (base table)
  sociodemos <- load_task_data("sociodemographics", data_dir) %>%
    dplyr::select(-any_of(treatment_vars)) %>%
    assert_unique("sociodemographics")

  # 2. Load preference types
  pref_types <- load_task_data(
    "pre_treatment_preference_types", generated_data_dir) %>%
    dplyr::select(ppt_id, study_id, cluster_name, factor_1_score) %>%
    assert_unique("pre_treatment_preference_types")

  # 3. Load IPW weights
  ipw_weights <- load_task_data("ipw_weights", generated_data_dir) %>%
    dplyr::select(ppt_id, study_id, ipw_weight, ipw_weight_truncated) %>%
    assert_unique("ipw_weights")

  # 4. Merge all data sources
  ppt_chars <- sociodemos %>%
    left_join(pref_types, by = c("ppt_id", "study_id")) %>%
    left_join(ipw_weights, by = c("ppt_id", "study_id"))

  # 5. Optionally load psychosocial factors
  if (include_psychosocial) {
    psychosocial_path <- file.path(
      generated_data_dir, "psychosocial_factors.jsonl")
    if (file.exists(psychosocial_path)) {
      psychosocial <- load_task_data(
        "psychosocial_factors", generated_data_dir) %>%
        dplyr::select(ppt_id, study_id,
                      pre_psychosocial_F1, pre_psychosocial_F2) %>%
        assert_unique("psychosocial_factors")

      ppt_chars <- ppt_chars %>%
        left_join(psychosocial, by = c("ppt_id", "study_id"))
    } else {
      cat("  Note: psychosocial_factors.jsonl not found - skipping\n")
    }
  }

  # 6. Final uniqueness check after merges
  ppt_chars <- assert_unique(ppt_chars, "merged ppt_chars")

  # 7. Apply coarsening transformations if requested
  if (apply_coarsening) {
    ppt_chars <- coarsen_and_factor_sociodemos(ppt_chars)
  }

  cat("  Final:", n_distinct(ppt_chars$ppt_id), "participants,",
      nrow(ppt_chars), "rows\n")

  return(ppt_chars)
}
