#!/usr/bin/env Rscript
# =============================================================================
# Unified Hypothesis Report Generator
# =============================================================================
#
# Generates a unified hypothesis report:
#   Part 1: Main Hypothesis Tests (organized by paper framework)
#   Part 2: Summary Table
#   Appendix: Original Pre-Registration (robustness check)
#
# Report Structure:
#   - Outcomes organized by domain (Attachment, Wellbeing, Perceptions, etc.)
#   - Each domain shows results for Relationship-Seeking, Personalisation, Domain effects
#   - Uses p_local and p_global from existing contrast JSONs
#
# Usage:
#   Rscript scripts/analysis/generate_hypothesis_report.R
#   Rscript scripts/analysis/generate_hypothesis_report.R --generate_tex_tables
#
# =============================================================================

library(tidyverse)
library(jsonlite)

set.seed(1234)

# =============================================================================
# COMMAND LINE ARGUMENTS
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
generate_tex_tables <- "--generate_tex_tables" %in% args

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

cat("=", strrep("=", 59), "\n", sep = "")
cat("Unified Hypothesis Report Generator\n")
cat("=", strrep("=", 59), "\n\n", sep = "")

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")
REPORT_DIR <- paths$REPORT_DIR

dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SECTION 2: LOAD CONTRAST DATA
# =============================================================================

cat("--- Loading Contrast Data ---\n\n")

#' Load a contrast JSON file
load_contrast_json <- function(filename, stats_dir) {
  path <- file.path(stats_dir, filename)
  if (!file.exists(path)) {
    cat("  WARNING: Missing", filename, "\n")
    return(NULL)
  }
  cat("  Loaded:", filename, "\n")
  fromJSON(path, simplifyVector = FALSE)
}

# Load all contrast files (longitudinal + cross-sectional)
CONTRAST_FILES_LONG <- c(
  attachment = "attachment_contrasts.json",
  perceptions = "perceptions_contrasts.json",
  psychosocial = "psychosocial_contrasts.json",
  preferences = "preferences_contrasts.json",
  momentary_affect = "momentary_affect_contrasts.json"
)

CONTRAST_FILES_CS <- c(
  attachment = "attachment_contrasts_cs.json",
  perceptions = "perceptions_contrasts_cs.json",
  psychosocial = "psychosocial_contrasts_cs.json",
  preferences = "preferences_contrasts_cs.json",
  momentary_affect = "momentary_affect_contrasts_cs.json"
)

# Load longitudinal contrasts
long_contrasts <- lapply(CONTRAST_FILES_LONG, load_contrast_json, stats_dir = STATS_DIR)
names(long_contrasts) <- names(CONTRAST_FILES_LONG)

# Load cross-sectional contrasts
cs_contrasts <- lapply(CONTRAST_FILES_CS, load_contrast_json, stats_dir = STATS_DIR)
names(cs_contrasts) <- names(CONTRAST_FILES_CS)

# Combine longitudinal and cross-sectional contrasts for each family
all_contrasts <- lapply(names(CONTRAST_FILES_LONG), function(family) {
  long_data <- long_contrasts[[family]]
  cs_data <- cs_contrasts[[family]]

  if (is.null(long_data) && is.null(cs_data)) return(NULL)
  if (is.null(cs_data)) return(long_data)
  if (is.null(long_data)) return(cs_data)

  # Combine contrasts from both study types
  combined <- long_data
  combined$contrasts <- c(long_data$contrasts, cs_data$contrasts)
  combined
})
names(all_contrasts) <- names(CONTRAST_FILES_LONG)

# Load pre-reg results
prereg_path <- file.path(STATS_DIR, "original_prereg_fdr_results.json")
if (file.exists(prereg_path)) {
  prereg_results <- fromJSON(prereg_path, simplifyVector = FALSE)
  cat("  Loaded: original_prereg_fdr_results.json\n")
} else {
  prereg_results <- NULL
  cat("  WARNING: original_prereg_fdr_results.json not found\n")
}

# =============================================================================
# SECTION 3: HELPER FUNCTIONS
# =============================================================================

#' Format p-value for display
format_p <- function(p) {
  if (is.null(p) || is.na(p)) return("--")
  if (p < 0.001) return("<0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  sprintf("%.3f", p)
}

#' Format estimate with CI
format_estimate <- function(est, lower, upper) {
  if (is.null(est) || is.na(est)) return("--")
  sprintf("%.2f [%.2f, %.2f]", est, lower, upper)
}

#' Add significance star
add_star <- function(p, threshold = 0.05) {
  if (is.null(p) || is.na(p)) return("")
  if (p < threshold) return("*")
  ""
}

#' Extract contrasts matching criteria
extract_contrasts <- function(json_data, test_pattern, test_type, study_type = "longitudinal") {
  if (is.null(json_data)) return(list())

  matches <- list()
  for (c in json_data$contrasts) {
    if (grepl(test_pattern, c$test, perl = TRUE) &&
        c$test_type == test_type &&
        c$study_type == study_type) {
      matches[[length(matches) + 1]] <- c
    }
  }
  matches
}

# =============================================================================
# SECTION 4: DEFINE OUTCOME DOMAINS (Paper Framework)
# =============================================================================

OUTCOME_DOMAINS <- list(
  # Order: Preferences, Attachment, Psychosocial, Affect, Perceptions
  preferences = list(
    name = "Preferences",
    description = "User preferences for AI chatbot",
    outcomes = list(
      list(outcome = "likeability", source = "preferences", label = "Likeability"),
      list(outcome = "engagingness", source = "preferences", label = "Engagingness"),
      list(outcome = "helpfulness", source = "preferences", label = "Helpfulness")
    )
  ),
  attachment = list(
    name = "Attachment",
    description = "Behavioral and psychological attachment indicators",
    outcomes = list(
      list(outcome = "separation_distress", source = "attachment", label = "Separation Distress"),
      list(outcome = "goodbye_action", source = "attachment", label = "Goodbye Action"),
      list(outcome = "seeking_companionship_likelihood", source = "attachment", label = "Seeking Companionship"),
      list(outcome = "reliance", source = "attachment", label = "Reliance"),
      list(outcome = "perceived_understanding", source = "attachment", label = "Perceived Understanding"),
      list(outcome = "self_disclosure", source = "attachment", label = "Self Disclosure")
    )
  ),
  psychosocial = list(
    name = "Psychosocial Wellbeing",
    description = "Psychosocial wellbeing factor scores",
    outcomes = list(
      list(outcome = "psychosocial_F1", source = "psychosocial", label = "Emotional Health (F1)"),
      list(outcome = "psychosocial_F2", source = "psychosocial", label = "Social Health (F2)")
    )
  ),
  momentary_affect = list(
    name = "Momentary Affect",
    description = "Immediate emotional responses (valence, arousal)",
    outcomes = list(
      list(outcome = "valence", source = "momentary_affect", label = "Valence"),
      list(outcome = "arousal", source = "momentary_affect", label = "Arousal")
    )
  ),
  perceptions = list(
    name = "Perceptions",
    description = "Perceptions of AI (relational/tool-friend, sentience)",
    outcomes = list(
      list(outcome = "tool_friend", source = "perceptions", label = "Tool-Friend"),
      list(outcome = "ontological_sentience", source = "perceptions", label = "Ontological Sentience"),
      list(outcome = "perceived_sentience", source = "perceptions", label = "Perceived Sentience")
    )
  )
)

# Treatments (ordered: RS, Domain, Pers)
TREATMENTS <- list(
  RS = list(name = "Relationship-Seeking (RS)"),
  Domain = list(name = "Domain"),
  Pers = list(name = "Personalisation")
)

# =============================================================================
# SECTION 5: EXTRACT MAIN EFFECT RESULTS
# =============================================================================

cat("\n--- Extracting Main Effect Results ---\n\n")

#' Get main effect for specific outcome and treatment
#' @param source_name Name of the contrast JSON source
#' @param outcome_val Outcome variable name
#' @param treatment_id One of "RS", "Domain", "Pers" - used to construct test_type
get_main_effect <- function(source_name, outcome_val, treatment_id) {
  json_data <- all_contrasts[[source_name]]
  if (is.null(json_data)) return(NULL)

  # Hard-coded test_type from treatment_id
  expected_test_type <- paste0("main_effect_", treatment_id)

  for (c in json_data$contrasts) {
    if (c$test_type == expected_test_type &&
        c$outcome == outcome_val &&
        c$study_type == "longitudinal") {
      return(c)
    }
  }
  NULL
}

# Build results table for each domain
domain_results <- list()

for (domain_id in names(OUTCOME_DOMAINS)) {
  domain <- OUTCOME_DOMAINS[[domain_id]]
  domain_results[[domain_id]] <- list()

  for (treatment_id in names(TREATMENTS)) {
    treatment <- TREATMENTS[[treatment_id]]
    treatment_rows <- list()

    for (outcome_spec in domain$outcomes) {
      result <- get_main_effect(
        outcome_spec$source,
        outcome_spec$outcome,
        treatment_id
      )

      if (!is.null(result)) {
        primary <- result$primary
        treatment_rows[[length(treatment_rows) + 1]] <- list(
          outcome = outcome_spec$outcome,
          label = outcome_spec$label,
          estimate = primary$estimate,
          lower_cl = primary$lower_cl,
          upper_cl = primary$upper_cl,
          p_raw = primary$p_raw,
          p_local = primary$p_local,
          p_global = primary$p_global
        )
      }
    }

    domain_results[[domain_id]][[treatment_id]] <- treatment_rows
  }
}

# =============================================================================
# SECTION 6: GENERATE PART 1 - MAIN HYPOTHESIS TESTS
# =============================================================================

cat("--- Generating Part 1: Main Hypothesis Tests ---\n\n")

lines <- c(
  "# Hypothesis Test Results",
  "",
  paste0("*Generated: ", Sys.time(), "*"),
  "",
  "---",
  "",
  "## Part 1: Main Hypothesis Tests (Paper Framework)",
  "",
  "Tests organized by outcome domain, showing effects of each treatment.",
  "p_local: FDR-corrected within local family; p_global: FDR-corrected across global family.",
  ""
)

# Generate tables for each domain
for (domain_id in names(OUTCOME_DOMAINS)) {
  domain <- OUTCOME_DOMAINS[[domain_id]]

  lines <- c(lines,
    sprintf("### 1.%d %s", which(names(OUTCOME_DOMAINS) == domain_id), domain$name),
    "",
    domain$description,
    ""
  )

  # Generate table for each treatment
  for (treatment_id in names(TREATMENTS)) {
    treatment <- TREATMENTS[[treatment_id]]
    rows <- domain_results[[domain_id]][[treatment_id]]

    if (length(rows) == 0) {
      lines <- c(lines,
        sprintf("#### Does %s affect %s?", treatment$name, tolower(domain$name)),
        "",
        "*No data available*",
        ""
      )
      next
    }

    lines <- c(lines,
      sprintf("#### Does %s affect %s?", treatment$name, tolower(domain$name)),
      "",
      "| Outcome | Estimate [95% CI] | p_raw | p_local | p_global |",
      "|---------|-------------------|-------|---------|----------|"
    )

    for (row in rows) {
      est_ci <- format_estimate(row$estimate, row$lower_cl, row$upper_cl)
      sig_star <- add_star(row$p_global)

      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s%s |",
        row$label,
        est_ci,
        format_p(row$p_raw),
        format_p(row$p_local),
        format_p(row$p_global),
        sig_star
      ))
    }

    lines <- c(lines, "")
  }
}

# =============================================================================
# SECTION 7: GENERATE PART 2 - SUMMARY TABLE
# =============================================================================

cat("--- Generating Part 2: Summary ---\n\n")

lines <- c(lines,
  "",
  "---",
  "",
  "## Part 2: Summary",
  "",
  "Overview of significant effects by outcome domain and treatment.",
  "",
  "| Outcome Domain | RS | Pers | Domain |",
  "|----------------|:--:|:----:|:------:|"
)

# Compute summary
for (domain_id in names(OUTCOME_DOMAINS)) {
  domain <- OUTCOME_DOMAINS[[domain_id]]

  rs_sig <- any(sapply(domain_results[[domain_id]][["RS"]], function(r) {
    !is.null(r$p_global) && !is.na(r$p_global) && r$p_global < 0.05
  }))
  pers_sig <- any(sapply(domain_results[[domain_id]][["Pers"]], function(r) {
    !is.null(r$p_global) && !is.na(r$p_global) && r$p_global < 0.05
  }))
  domain_sig <- any(sapply(domain_results[[domain_id]][["Domain"]], function(r) {
    !is.null(r$p_global) && !is.na(r$p_global) && r$p_global < 0.05
  }))

  lines <- c(lines, sprintf(
    "| %s | %s | %s | %s |",
    domain$name,
    ifelse(rs_sig, "Yes", "--"),
    ifelse(pers_sig, "Yes", "--"),
    ifelse(domain_sig, "Yes", "--")
  ))
}

# =============================================================================
# SECTION 8: GENERATE APPENDIX - ORIGINAL PRE-REG
# =============================================================================

cat("--- Generating Appendix: Original Pre-Registration ---\n\n")

lines <- c(lines,
  "",
  "---",
  "",
  "## Appendix: Original Pre-Registration (Robustness)",
  "",
  "### What Changed",
  "",
  "The pre-registration specified hypotheses organized by Research Question (RQ1-RQ4).",
  "The paper reorganizes around measure families for clarity. Key differences:",
  "",
  "- **tool_friend**: Moved from \"attachment\" (pre-reg) to \"perceptions\" (paper)",
  "- **Outcome additions**: Paper includes additional outcomes not in pre-reg",
  "  (momentary affect, preferences, sentience measures)",
  "- **FDR structure**: Pre-reg grouped by RQ; paper groups by measure family x test type",
  ""
)

if (!is.null(prereg_results)) {
  lines <- c(lines,
    "### Pre-Registration Results",
    "",
    "For robustness, we run the original pre-registration exactly as specified,",
    "with treatment-specific FDR families. All tests are two-sided (p_fdr < 0.05).",
    ""
  )

  # Group hypotheses by FDR family
  # Convert all numeric fields with as.numeric to handle mixed "NA" strings and numeric values
  hypotheses_df <- bind_rows(lapply(prereg_results$hypotheses, function(h) {
    data.frame(
      id = as.character(h$id %||% NA_character_),
      rq = as.character(h$rq %||% NA_character_),
      description = as.character(h$description %||% NA_character_),
      outcome = as.character(h$outcome %||% NA_character_),
      fdr_family = as.character(h$fdr_family %||% NA_character_),
      estimate = as.numeric(h$estimate %||% NA_real_),
      lower_cl = as.numeric(h$lower_cl %||% NA_real_),
      upper_cl = as.numeric(h$upper_cl %||% NA_real_),
      p_raw = as.numeric(h$p_raw %||% NA_real_),
      p_fdr = as.numeric(h$p_fdr %||% NA_real_),
      significant = as.logical(h$significant %||% FALSE),
      stringsAsFactors = FALSE
    )
  }))

  # Order FDR families: RS, Domain, Pers for each outcome group
  fdr_family_order <- c(
    "attachment_RS", "attachment_Domain", "attachment_Pers",
    "wellbeing_RS", "wellbeing_Domain", "wellbeing_Pers",
    "wellbeing_individual_RS", "wellbeing_individual_Domain", "wellbeing_individual_Pers",
    "ai_treatment"
  )
  fdr_families <- unique(hypotheses_df$fdr_family)
  fdr_families <- fdr_family_order[fdr_family_order %in% fdr_families]

  for (fam in fdr_families) {
    fam_data <- hypotheses_df %>% filter(fdr_family == fam)
    n_tests <- nrow(fam_data)

    lines <- c(lines,
      sprintf("#### %s (%d tests, FDR-corrected)", fam, n_tests),
      "",
      "| Hypothesis | Outcome | Estimate [95% CI] | p_raw | p_fdr |",
      "|------------|---------|-------------------|-------|-------|"
    )

    for (i in seq_len(nrow(fam_data))) {
      row <- fam_data[i, ]
      sig_star <- if (!is.na(row$p_fdr) && row$p_fdr < 0.05) "*" else ""

      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %s%s |",
        row$description,
        row$outcome,
        format_estimate(row$estimate, row$lower_cl, row$upper_cl),
        format_p(row$p_raw),
        format_p(row$p_fdr),
        sig_star
      ))
    }

    lines <- c(lines, "")
  }

  # Pre-reg summary table
  lines <- c(lines,
    "### Pre-Reg Summary",
    "",
    "*Note: The pre-registered FDR families covered a subset of outcomes. For attachment,",
    "only tool_friend, separation_distress, will_miss_ai, and goodbye_action were specified;",
    "additional attachment measures (reliance, perceived_understanding, self_disclosure,",
    "seeking_companionship) were analysed using the pre-registered model specification but",
    "not included in the original FDR structure. Preferences and perceptions outcomes were",
    "similarly not included in the original FDR families.*",
    "",
    "| Pre-Reg Family | N Tests | Significant | Result |",
    "|----------------|---------|-------------|--------|"
  )

  prereg_summary <- hypotheses_df %>%
    group_by(fdr_family) %>%
    summarise(
      n_tests = n(),
      n_sig = sum(significant, na.rm = TRUE),
      .groups = "drop"
    )

  # Order: RS, Domain, Pers for each outcome group
  prereg_order <- c(
    "attachment_RS", "attachment_Domain", "attachment_Pers",
    "wellbeing_RS", "wellbeing_Domain", "wellbeing_Pers",
    "wellbeing_individual_RS", "wellbeing_individual_Domain", "wellbeing_individual_Pers",
    "ai_treatment"
  )
  prereg_summary <- prereg_summary %>%
    mutate(fdr_family = factor(fdr_family, levels = prereg_order)) %>%
    arrange(fdr_family)

  for (i in seq_len(nrow(prereg_summary))) {
    row <- prereg_summary[i, ]
    result <- if (row$n_sig > 0) "Yes" else "--"
    lines <- c(lines, sprintf(
      "| %s | %d | %d | %s |",
      row$fdr_family, row$n_tests, row$n_sig, result
    ))
  }

} else {
  lines <- c(lines,
    "### Pre-Registration Results",
    "",
    "*Pre-registration results not available. Run original_prereg_fdr_check.R first.*",
    ""
  )
}


report_path <- file.path(REPORT_DIR, "hypothesis_report.md")
writeLines(lines, report_path)
cat("Saved:", report_path, "\n")

# =============================================================================
# SECTION 9: LATEX TABLES (Optional)
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating Contrast LaTeX Tables ---\n\n")

  # Source contrast table utilities (which sources latex_utils.R)
  source("scripts/utils_r/latex_utils.R")
  source("scripts/utils_r/latex_contrast_table_utils.R")

  # Output directory structure
  TABLE_DIR <- file.path(PROJECT_ROOT, "outputs/tables/main_studies")
  TABLE_DIR_CONTRASTS <- file.path(TABLE_DIR, "tex_contrast_tables")
  COORDINATORS_DIR <- file.path(TABLE_DIR, "table_coordinators")
  dir.create(TABLE_DIR_CONTRASTS, recursive = TRUE, showWarnings = FALSE)
  dir.create(COORDINATORS_DIR, recursive = TRUE, showWarnings = FALSE)

  # Time configuration per family
  TIME_CONFIG <- list(
    attachment = list(prefix = "W", points = c(1, 4)),
    perceptions = list(prefix = "W", points = c(1, 4)),
    psychosocial = list(prefix = "W", points = c(1, 4)),
    preferences = list(prefix = "S", points = c(1, 20)),
    momentary_affect = list(prefix = "S", points = c(1, 20))
  )

  # Generate tables for each measure family - separately for each study type
  for (family_name in names(all_contrasts)) {
    if (!is.null(all_contrasts[[family_name]])) {
      time_cfg <- TIME_CONFIG[[family_name]]
      if (is.null(time_cfg)) {
        time_cfg <- list(prefix = "S", points = c(1, 20))
      }

      # Longitudinal: full tables (X1-X5)
      process_contrast_measure_family(
        measure_family = family_name,
        contrasts_data = all_contrasts[[family_name]],
        table_dir = TABLE_DIR_CONTRASTS,
        time_prefix = time_cfg$prefix,
        time_points = time_cfg$points,
        tables_to_include = c("X1", "X2", "X3", "X4a", "X4b", "X5"),
        study_filter = "longitudinal",
        file_suffix = "_long"
      )

      # Cross-sectional: main effects and dose-response (X1, X2)
      process_contrast_measure_family(
        measure_family = family_name,
        contrasts_data = all_contrasts[[family_name]],
        table_dir = TABLE_DIR_CONTRASTS,
        time_prefix = time_cfg$prefix,
        time_points = time_cfg$points,
        tables_to_include = c("X1", "X2"),
        study_filter = "cross_sectional",
        file_suffix = "_cs"
      )
    }
  }

  # Generate coordinator file
  generate_contrast_coordinator(
    table_dir = TABLE_DIR_CONTRASTS,
    output_dir = COORDINATORS_DIR,
    table_subdir = "tables/main_studies/tex_contrast_tables"
  )

  cat("\n--- LaTeX Table Generation Complete ---\n")
  cat("Tables saved to:", TABLE_DIR_CONTRASTS, "\n")
  cat("Coordinator saved to:", COORDINATORS_DIR, "\n")
}

cat("\n", strrep("=", 59), "\n", sep = "")
cat("DONE\n")
cat(strrep("=", 59), "\n", sep = "")