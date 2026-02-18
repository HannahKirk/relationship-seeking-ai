#!/usr/bin/env Rscript
# =============================================================================
# Psychosocial Dimensionality Reduction
# =============================================================================
#
# Exploratory Factor Analysis (EFA) of psychosocial measures:
#   - WHO-5 (wellbeing/quality of life)
#   - PHQ-GAD-4 (anxiety/depression)
#   - UCLA-8 (loneliness)
#   - LUBBEN-6 (social connection)
#
# Outputs:
#   - Factor scores (F1, F2) saved for downstream analysis
#   - EFA diagnostics and visualizations
#   - Factor loadings tables
#
# Usage:
#   Rscript scripts/analysis/psychosocial_dim_reduction.R
#   Rscript scripts/analysis/psychosocial_dim_reduction.R --generate_report
#   Rscript scripts/analysis/psychosocial_dim_reduction.R --generate_tex_tables
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(psych)
library(knitr)
library(reshape2)
library(pheatmap)
library(RColorBrewer)

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
FIGURES_DIR <- paths$FIGURE_DIR
TABLES_DIR <- file.path(PROJECT_ROOT, "outputs/tables/main_studies")
TABLES_DIR_TABLES <- file.path(TABLES_DIR, "tex_tables")
TABLES_DIR_COORDINATORS <- file.path(TABLES_DIR, "table_coordinators")
MODEL_DIR <- paths$MODEL_DIR
STATS_DIR <- paths$STATS_DIR
REPORT_DIR <- paths$REPORT_DIR
GENERATED_DIR <- paths$GENERATED_DIR
DATA_DIR <- paths$DATA_DIR

# Source utilities
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))

# Create output directories
dir.create(file.path(FIGURES_DIR, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FIGURES_DIR, "png"), recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLES_DIR_COORDINATORS, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(GENERATED_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=" , rep("=", 59), "\n", sep = "")
cat("Psychosocial Dimensionality Reduction\n")
cat("=" , rep("=", 59), "\n", sep = "")
cat("Project root:", PROJECT_ROOT, "\n")
cat("Data directory:", DATA_DIR, "\n\n")

# Set random seed for reproducibility
random_seed <- 1234
set.seed(random_seed)

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("--- Loading Data ---\n")

# Load psychosocial data
data_raw <- load_task_data("psychosocial", DATA_DIR)

# Prepare treatment arms
data_prepped <- prepare_treatment_arms(data_raw)

# Filter to main studies
# Note: Include ALL participants (including dropouts) so PRE factor scores
# are available for attrition analysis
data_stats <- data_prepped %>%
 filter(study_id %in% c("cross-sectional", "longitudinal"))

cat("After filtering:", nrow(data_stats), "rows\n")
print_counts <- function(df) {
  df %>%
    group_by(study_id, timepoint, week_numeric) %>%
    summarise(n_ppts = n_distinct(ppt_id), n_rows = n(), .groups = "drop") %>%
    print()
}
print_counts(data_stats)

# =============================================================================
# SECTION 3: CONVERT ITEMS TO NUMERIC (HIGHER = BETTER)
# =============================================================================

cat("--- Converting Items to Numeric ---\n")

# Identify item columns
all_cols <- colnames(data_stats)

who_items <- all_cols[grepl("^who_", all_cols) & all_cols != "who_score"]
ucla_items <- all_cols[grepl("^ucla_", all_cols) & all_cols != "ucla_score"]
phq_items <- all_cols[grepl("^phq_gad_", all_cols) & all_cols != "phq_gad_score"]
lubben_items <- all_cols[grepl("^lubben_", all_cols) & all_cols != "lubben_score"]

cat("Found", length(who_items), "WHO items\n")
cat("Found", length(ucla_items), "UCLA items\n")
cat("Found", length(phq_items), "PHQ-GAD items\n")
cat("Found", length(lubben_items), "LUBBEN items\n")

stopifnot(
 length(who_items) == 5, # WHO-5 should have 5 items
 length(ucla_items) == 8, # UCLA-8 should have 8 items
 length(phq_items) == 4, # PHQ-GAD-4 should have 4 items
 length(lubben_items) == 6 # LUBBEN-6 should have 6 items
)

# Validate all response values before converting
expected_who <- c("At no time", "Some of the time", "Less than half of the time",
                  "More than half of the time", "Most of the time", "All of the time")
expected_ucla <- c("Never", "Rarely", "Sometimes", "Often")
expected_phq <- c("Not at all", "Several days", "More than half the days", "Nearly every day")
expected_lubben <- c("None", "One", "Two", "Three or Four", "Five to Eight", "Nine or More")

check_values <- function(df, cols, expected, scale_name) {
  vals <- unique(unlist(df[cols], use.names = FALSE))
  vals <- vals[!is.na(vals)]
  unexpected <- setdiff(vals, expected)
  if (length(unexpected) > 0) {
    stop(sprintf("%s has unexpected values: %s", scale_name, paste(unexpected, collapse = ", ")))
  }
}

check_values(data_stats, who_items, expected_who, "WHO")
check_values(data_stats, ucla_items, expected_ucla, "UCLA")
check_values(data_stats, phq_items, expected_phq, "PHQ-GAD")
check_values(data_stats, lubben_items, expected_lubben, "LUBBEN")

# Convert to numeric
data_numeric <- data_stats %>%
 mutate(
   across(all_of(who_items),
     ~ case_when(
       . == "At no time" ~ 0,
       . == "Some of the time" ~ 1,
       . == "Less than half of the time" ~ 2,
       . == "More than half of the time" ~ 3,
       . == "Most of the time" ~ 4,
       . == "All of the time" ~ 5,
       TRUE ~ NA_real_
     ),
     .names = "{.col}_num"
   ),
   across(all_of(ucla_items),
     ~ case_when(
       . == "Never" ~ 1,
       . == "Rarely" ~ 2,
       . == "Sometimes" ~ 3,
       . == "Often" ~ 4,
       TRUE ~ NA_real_
     ),
     .names = "{.col}_num"
   ),
   across(all_of(phq_items),
     ~ case_when(
       . == "Not at all" ~ 0,
       . == "Several days" ~ 1,
       . == "More than half the days" ~ 2,
       . == "Nearly every day" ~ 3,
       TRUE ~ NA_real_
     ),
     .names = "{.col}_num"
   ),
   across(all_of(lubben_items),
     ~ case_when(
       . == "One" ~ 1,
       . == "Two" ~ 2,
       . == "Three or Four" ~ 3,
       . == "Five to Eight" ~ 4,
       . == "Nine or More" ~ 5,
       TRUE ~ NA_real_
     ),
     .names = "{.col}_num"
   )
 )

all_items_num <- c(
 paste0(who_items, "_num"),
 paste0(ucla_items, "_num"),
 paste0(phq_items, "_num"),
 paste0(lubben_items, "_num")
)
stopifnot(all(all_items_num %in% names(data_numeric)))
cat("Items converted to numeric. Total:", length(all_items_num), "\n")

# Define which UCLA items are positive (keep) vs negative (reverse)
# Positive UCLA items: higher raw score = better (outgoing, can find companionship)
ucla_positive <- c(
  "ucla_i_am_an_outgoing_person_num",
  "ucla_i_can_find_companionship_when_i_want_it_num"
)
ucla_negative <- setdiff(paste0(ucla_items, "_num"), ucla_positive)

cat("UCLA positive items:", length(ucla_positive), "\n")
cat("UCLA negative items:", length(ucla_negative), "\n")

stopifnot(length(ucla_positive) + length(ucla_negative) == length(ucla_items))

# Helper to reverse scales
rev_ucla <- function(x) 5 - x # UCLA has 1-4 range, so reverse by subtracting from 5
rev_phq  <- function(x) 3 - x # Has 0-3 range

# Create *_pos columns where HIGHER = BETTER WELLBEING
data_pos <- data_numeric %>%
 mutate(
   # WHO: keep (already higher = better)
   across(all_of(paste0(who_items, "_num")), ~., .names = "{.col}_pos"),
   # Lubben: keep (already higher = better)
   across(all_of(paste0(lubben_items, "_num")), ~., .names = "{.col}_pos"),
   # UCLA positive: keep
   across(all_of(ucla_positive), ~., .names = "{.col}_pos"),
   # UCLA negative: reverse
   across(all_of(ucla_negative), rev_ucla, .names = "{.col}_pos"),
   # PHQ/GAD: reverse to make higher = better (less symptoms)
   across(all_of(paste0(phq_items, "_num")), rev_phq, .names = "{.col}_pos")
 )

all_items_pos <- paste0(all_items_num, "_pos")
stopifnot(all(all_items_pos %in% names(data_pos)))
cat("Re-coded so HIGHER = BETTER WELLBEING; created *_pos columns.\n\n")

# =============================================================================
# SECTION 4: SPLIT PRE/POST
# =============================================================================

cat("--- Splitting PRE/POST ---\n")

data_pre_pos <- data_pos %>%
 filter(timepoint == "pre") %>%
 select(ppt_id, study_id, timepoint, week_numeric, all_of(all_items_pos))

data_post_pos <- data_pos %>%
 filter(timepoint == "post") %>%
 select(ppt_id, study_id, timepoint, week_numeric, all_of(all_items_pos))

cat("PRE:\n")
print_counts(data_pre_pos)
cat("POST:\n")
print_counts(data_post_pos)

# =============================================================================
# SECTION 5: POLYCHORIC CORRELATIONS + DIAGNOSTICS (PRE)
# =============================================================================

cat("--- Polychoric Correlations + Diagnostics (PRE) ---\n")

# Check for missing data in PRE
na_counts <- colSums(is.na(data_pre_pos[all_items_pos]))
if (any(na_counts > 0)) {
  cat("NAs per item:\n")
  print(na_counts[na_counts > 0])
} else {
  cat("No missing values in PRE items\n")
}

# Exclude any all-NA items (note pre-data has no NA but we include this check for confirmation)
na_only_items_pre <- names(na_counts[na_counts == nrow(data_pre_pos)])
if (length(na_only_items_pre) > 0) {
  cat("Items all-NA in PRE (excluded):", paste(na_only_items_pre, collapse = ", "), "\n")
}

# Clean analysis set
items_pos_clean <- setdiff(all_items_pos, na_only_items_pre)
X_pre <- data_pre_pos %>% select(all_of(items_pos_clean))

# Effective N
n_obs_pre <- sum(rowSums(!is.na(X_pre)) > 1)
cat("Effective N (PRE):", n_obs_pre, "\n")
cat("N items:", length(items_pos_clean), "\n")

# Compute polychoric correlations
# Note some cross-tabs might be empty so a continuity correction is applied by psych::polychoric
cat("Computing polychoric correlations...\n")
pc_pre_obj <- psych::polychoric(X_pre)
pc_pre <- pc_pre_obj$rho

# Also compute Pearson for comparison
pearson_pre <- cor(X_pre, use = "pairwise.complete.obs")
diff_pre <- pc_pre - pearson_pre
mean_diff <- mean(abs(diff_pre), na.rm = TRUE)
cat("Mean |Polychoric - Pearson|:", round(mean_diff, 4), "\n")

# KMO test
kmo_obj <- try(psych::KMO(pc_pre), silent = TRUE)
if (!inherits(kmo_obj, "try-error")) {
 cat("Overall KMO:", round(kmo_obj$MSA, 3), "\n")
}

# Bartlett's test
bart_obj <- try(psych::cortest.bartlett(pc_pre, n = n_obs_pre), silent = TRUE)
if (!inherits(bart_obj, "try-error")) {
 cat("Bartlett's test: chi-sq =", round(bart_obj$chisq, 2),
     ", df =", bart_obj$df, ", p =", format.pval(bart_obj$p.value), "\n")
}

# Eigenvalues
eig_vals <- eigen(pc_pre, symmetric = TRUE, only.values = TRUE)$values
eig_tbl <- data.frame(
 Factor = seq_along(eig_vals),
 Eigenvalue = eig_vals,
 Proportion = eig_vals / sum(eig_vals),
 Cumulative = cumsum(eig_vals) / sum(eig_vals)
)
cat("\nEigenvalues (first 5):\n")
print(head(eig_tbl, 5))

# Parallel analysis
cat("\nRunning parallel analysis...\n")
pa <- psych::fa.parallel(pc_pre, n.obs = n_obs_pre, fm = "uls", fa = "fa", plot = FALSE)

cat("Parallel analysis suggests", pa$nfact, "factors\n\n")
map_result <- psych::VSS(pc_pre, n.obs = n_obs_pre, plot = FALSE)
# VSS complexity 1: best k assuming each item loads on exactly 1 factor
cat("VSS complexity 1 suggests:", which.max(map_result$vss.stats$cfit.1), "factors\n")
# VSS complexity 2: best k assuming each item can cross-load on up to 2 factors
cat("VSS complexity 2 suggests:", which.max(map_result$vss.stats$cfit.2), "factors\n")

# =============================================================================
# SECTION 6: EXPLORATORY FACTOR ANALYSIS (k=2)
# =============================================================================

cat("--- EFA (k=2, oblimin rotation) ---\n")
# We select 2 factors for interpretability
# PA suggests 6 but tends to over-extract with large N and ordinal items.
# Factors will serve as covariates in downstream analyses (vulnerability analysis)
# So we prioritize parsimony and interpretability.
k <- 2

efa_pre <- psych::fa(
 r = pc_pre,
 nfactors = k,
 fm = "uls",
 rotate = "oblimin",
 n.obs = n_obs_pre
)

# Fit indices
fit_df <- data.frame(
 nfactors = k,
 dof = efa_pre$dof,
 RMSR = efa_pre$rms,
 RMSEA = efa_pre$RMSEA[1],
 TLI = efa_pre$TLI,
 BIC = efa_pre$BIC
)
cat("Fit indices:\n")
print(fit_df)

# Pattern loadings
L <- as.matrix(unclass(efa_pre$loadings))
colnames(L) <- paste0("F", seq_len(ncol(L)))

load_df <- as.data.frame(L)
load_df$item <- rownames(efa_pre$loadings)
load_df$h2 <- efa_pre$communality
load_df$u2 <- efa_pre$uniquenesses

# Sort by strongest loading
load_df <- load_df %>%
 mutate(max_loading = pmax(abs(F1), abs(F2))) %>%
 arrange(desc(max_loading)) %>%
 select(item, F1, F2, h2, u2)

cat("\nTop 10 loadings:\n")
print(head(load_df, 10))

# Factor correlation
phi <- efa_pre$Phi
cat("\nFactor correlation (Phi):\n")
print(round(phi, 3))

# Variance explained
var_accounted <- efa_pre$Vaccounted
cat("\nVariance accounted for:\n")
print(round(var_accounted, 3))

# Extract proportion of variance for each factor
prop_var_f1 <- var_accounted["Proportion Var", 1]
prop_var_f2 <- var_accounted["Proportion Var", 2]
cum_var <- var_accounted["Cumulative Var", 2]

cat(sprintf("\nF1 explains %.1f%% of variance\n", prop_var_f1 * 100))
cat(sprintf("F2 explains %.1f%% of variance\n", prop_var_f2 * 100))
cat(sprintf("Total: %.1f%% of variance\n", cum_var * 100))

# Save EFA model
efa_pre_model <- efa_pre
items_for_scoring <- rownames(efa_pre$loadings)

cat("\n")

# =============================================================================
# SECTION 7: COMPUTE ANCHORED FACTOR SCORES
# =============================================================================

cat("--- Computing Anchored Factor Scores ---\n")
# -------------------------------------------------------------------------
# Factor score weights from psych (regression method)
# Using psych::factor.scores to avoid manual matrix algebra errors
# -------------------------------------------------------------------------

Xpre_raw <- data_pre_pos %>% select(all_of(items_for_scoring))
pre_means <- sapply(Xpre_raw, mean, na.rm = TRUE)
pre_sds   <- sapply(Xpre_raw, sd,   na.rm = TRUE)
pre_sds[pre_sds == 0 | is.na(pre_sds)] <- 1

# Helper: z-standardize using PRE parameters
z_with_pre_params <- function(df) {
  Z <- sweep(df, 2, pre_means, "-")
  Z <- sweep(Z, 2, pre_sds,   "/")
  Z[is.na(Z)] <- 0
  as.matrix(Z)
}

# Extract scoring weights from psych
Zpre <- z_with_pre_params(Xpre_raw)
weights <- psych::factor.scores(Zpre, efa_pre_model)$weights
colnames(weights) <- paste0("F", seq_len(ncol(weights)))

# PRE scores
Scr_pre <- Zpre %*% weights

scores_pre <- data.frame(
  ppt_id = data_pre_pos$ppt_id,
  study_id = data_pre_pos$study_id,
  Scr_pre,
  week_numeric = 0,
  timepoint = "pre"
)

# Assert we have PRE scores for all participants in data_stats (including dropouts)
ppts_in_data <- data_stats %>% distinct(ppt_id) %>% pull(ppt_id)
ppts_with_pre <- scores_pre %>% pull(ppt_id)

missing <- setdiff(ppts_in_data, ppts_with_pre)
duplicated <- scores_pre %>% count(ppt_id) %>% filter(n > 1)

if (length(missing) > 0) cat("WARNING:", length(missing), "participants missing PRE scores\n")
if (nrow(duplicated) > 0) cat("WARNING:", nrow(duplicated), "participants with duplicate PRE scores\n")
if (length(missing) == 0 & nrow(duplicated) == 0) {
  cat("All", length(ppts_in_data), "participants have exactly one PRE score\n")
}

# POST scores (anchored to PRE parameters, same weights)
Xpost_raw <- data_post_pos %>% select(all_of(items_for_scoring))
Zpost <- z_with_pre_params(Xpost_raw)
Scr_post <- Zpost %*% weights

scores_post <- data.frame(
  ppt_id = data_post_pos$ppt_id,
  study_id = data_post_pos$study_id,
  Scr_post,
  week_numeric = 4,
  timepoint = "post"
)

# Assert POST scores are present for completing participants)
ppts_with_post <- scores_post %>% pull(ppt_id)

missing_post <- setdiff(ppts_in_data, ppts_with_post)
duplicated_post <- scores_post %>% count(ppt_id) %>% filter(n > 1)

cat("POST scores:", length(ppts_with_post), "of", length(ppts_in_data), "participants\n")
cat("Missing POST (dropout):", length(missing_post), "\n")
if (nrow(duplicated_post) > 0) cat("WARNING:", nrow(duplicated_post), "participants with duplicate POST scores\n")
if (nrow(duplicated_post) == 0) {
  cat("All", length(ppts_with_post), "POST participants have exactly one POST score\n")
}

cat("\nPRE F1: mean =", round(mean(scores_pre$F1), 3),
    ", sd =", round(sd(scores_pre$F1), 3), "\n")
cat("PRE F2: mean =", round(mean(scores_pre$F2), 3),
    ", sd =", round(sd(scores_pre$F2), 3), "\n")
cat("POST F1: mean =", round(mean(scores_post$F1), 3),
    ", sd =", round(sd(scores_post$F1), 3), "\n")
cat("POST F2: mean =", round(mean(scores_post$F2), 3),
    ", sd =", round(sd(scores_post$F2), 3), "\n\n")

# =============================================================================
# SECTION 8: CREATE AND SAVE FACTOR SCORES DATA
# =============================================================================

cat("--- Creating Factor Scores Data ---\n")

# Combine pre and post scores
factor_scores_all <- bind_rows(scores_pre, scores_post) %>%
 rename(
   psychosocial_F1 = F1,
   psychosocial_F2 = F2
 )


# Pivot to wide format: pre_psychosocial_F1, post_psychosocial_F1, etc.
factor_scores_wide <- factor_scores_all %>%
  select(-week_numeric) %>%
  pivot_wider(
    id_cols = c(ppt_id, study_id),
    names_from = timepoint,
    values_from = c(psychosocial_F1, psychosocial_F2),
    names_glue = "{timepoint}_{.value}"
  )

# Save as JSONL
save_task_data(factor_scores_wide, "psychosocial_factors", GENERATED_DIR, relative_to = PROJECT_ROOT)

# =============================================================================
# SECTION 9: HELPER FUNCTIONS FOR OUTPUTS
# =============================================================================

# Helper to clean item names for display
clean_item_name <- function(x) {
  x %>%
    gsub("_num_pos$", "", .) %>%
    gsub("^who_", "", .) %>%
    gsub("^phq_gad_", "", .) %>%
    gsub("^ucla_", "", .) %>%
    gsub("^lubben_", "", .) %>%
    gsub("_", " ", .) %>%
    tools::toTitleCase(.)
}

# Helper to infer questionnaire from item name
infer_questionnaire <- function(x) {
  case_when(
    grepl("^who_", x, ignore.case = TRUE) ~ "WHO",
    grepl("^phq_", x, ignore.case = TRUE) ~ "PHQ",
    grepl("^ucla_", x, ignore.case = TRUE) ~ "UCLA",
    grepl("^lubben_", x, ignore.case = TRUE) ~ "LUBBEN",
    TRUE ~ "Other"
  )
}

# Prepare loadings data with short names (used by both plots and tables)
load_plot_df <- load_df %>%
  mutate(questionnaire = infer_questionnaire(item)) %>%
  group_by(questionnaire) %>%
  mutate(short_name = paste0(questionnaire, "_", row_number())) %>%
  ungroup()

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 10: LATEX TABLES (conditional)
# =============================================================================

if (generate_tex_tables) {
  cat("--- Generating LaTeX Tables ---\n")

  # Helper to escape LaTeX special characters

  escape_latex <- function(x) {
    gsub("_", "\\\\_", x)
  }

  loadings_latex <- load_plot_df %>%
    mutate(
      Scale = case_when(
        grepl("^who_", item, ignore.case = TRUE) ~ "WHO-5",
        grepl("^phq_", item, ignore.case = TRUE) ~ "PHQ-GAD-4",
        grepl("^ucla_", item, ignore.case = TRUE) ~ "UCLA-8",
        grepl("^lubben_", item, ignore.case = TRUE) ~ "LUBBEN-6",
        TRUE ~ "Other"
      ),
      Item = escape_latex(tolower(clean_item_name(item))),
      ID = escape_latex(short_name),
      F1 = round(F1, 3),
      F2 = round(F2, 3),
      h2 = round(h2, 3)
    ) %>%
    arrange(Scale, short_name) %>%
    select(Scale, ID, Item, F1, F2, h2)

  # Generate LaTeX table
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\caption{Psychosocial Factor Loadings (PRE, Oblimin Rotation)}",
    "\\label{tab:psychosocial_loadings}",
    "\\footnotesize",
    "\\begin{tabular}{lllrrr}",
    "\\toprule",
    "Scale & ID & Item & F1 & F2 & $h^2$ \\\\",
    "\\midrule"
  )

  current_scale <- ""
  for (i in seq_len(nrow(loadings_latex))) {
    row <- loadings_latex[i, ]
    # Add scale header if new scale
    if (row$Scale != current_scale) {
      if (current_scale != "") {
        latex_lines <- c(latex_lines, "\\addlinespace[0.5em]")
      }
      current_scale <- row$Scale
    }
    # Format row
    scale_col <- ifelse(i == 1 || loadings_latex$Scale[i] != loadings_latex$Scale[i-1],
                        row$Scale, "")
    latex_lines <- c(latex_lines, sprintf(
      "%s & %s & %s & %.3f & %.3f & %.3f \\\\",
      scale_col, row$ID, row$Item, row$F1, row$F2, row$h2
    ))
  }

  latex_lines <- c(latex_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(latex_lines, file.path(TABLES_DIR_TABLES, "psychosocial_loadings.tex"))
  cat("Saved: psychosocial_loadings.tex\n")

  # Scale intercorrelation table
  pre_scores <- data_stats %>%
    filter(timepoint == "pre") %>%
    select(lubben_score, ucla_score, who_score, phq_gad_score) %>%
    rename(
      Lubben = lubben_score,
      UCLA = ucla_score,
      WHO = who_score,
      `PHQ-GAD` = phq_gad_score
    )

  cor_mat <- cor(pre_scores, use = "pairwise.complete.obs")

  # Format as lower triangle with 1.00 on diagonal
  format_cor <- function(r) {
    if (r < 0) sprintf("$-%.2f$", abs(r)) else sprintf("%.2f", r)
  }

  cor_latex <- c(
    "\\begin{table}[H]",
    "\\footnotesize",
    "\\centering",
    "\\caption{Intercorrelations among psychosocial scales (pre-treatment)}",
    "\\label{tab:psychosocial_cors}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    " & Lubben & UCLA & WHO & PHQ-GAD \\\\",
    "\\midrule",
    "Lubben (social support) & 1.00 & & & \\\\",
    sprintf("UCLA (loneliness) & %s & 1.00 & & \\\\",
            format_cor(cor_mat["UCLA", "Lubben"])),
    sprintf("WHO (wellbeing) & %s & %s & 1.00 & \\\\",
            format_cor(cor_mat["WHO", "Lubben"]),
            format_cor(cor_mat["WHO", "UCLA"])),
    sprintf("PHQ-GAD (anxiety/depression) & %s & %s & %s & 1.00 \\\\",
            format_cor(cor_mat["PHQ-GAD", "Lubben"]),
            format_cor(cor_mat["PHQ-GAD", "UCLA"]),
            format_cor(cor_mat["PHQ-GAD", "WHO"])),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  writeLines(cor_latex, file.path(TABLES_DIR_TABLES, "psychosocial_cors.tex"))
  cat("Saved: psychosocial_cors.tex\n")
}

# =============================================================================
# SECTION 11: VISUALIZATIONS
# =============================================================================

cat("\n--- Creating Visualizations ---\n")

# Source plot utilities for save_plot function
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_utils.R"))

questionnaire_colors <- c(
 "WHO" = "#808080",
 "PHQ" = "#CC79A7",
 "UCLA" = "#E69F00",
 "LUBBEN" = "#009E73"
)

# Combined factor loadings plot (F1 and F2 as facets)
# Pivot to long format for faceting
load_long <- load_plot_df %>%
  pivot_longer(cols = c(F1, F2), names_to = "Factor", values_to = "Loading") %>%
  mutate(Factor = factor(Factor, levels = c("F1", "F2")))

# Order items by F1 loading (consistent across both panels)
item_order <- load_plot_df %>%
  arrange(F1) %>%
  pull(short_name)

load_long <- load_long %>%
  mutate(short_name = factor(short_name, levels = item_order))

p_loadings <- ggplot(load_long, aes(x = short_name, y = Loading, fill = questionnaire)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
  geom_hline(yintercept = c(-0.30, 0.30), linetype = "dashed", color = "gray55") +
  scale_fill_manual(values = questionnaire_colors) +
  facet_wrap(~Factor, ncol = 1) +
  labs(
    x = "Items",
    y = "Loading",
    fill = "Scale"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14)) +
  coord_cartesian(ylim = c(-0.5, 1.0))

save_plot(FIGURES_DIR, p_loadings, "psychosocial_loadings", width = 14, height = 7)

# Polychoric correlation heatmap
pal <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(101)
brks <- seq(-1, 1, length.out = 102)

# Use short names for heatmap labels
heatmap_labels <- load_plot_df$short_name
names(heatmap_labels) <- load_plot_df$item
rownames(pc_pre) <- heatmap_labels[rownames(pc_pre)]
colnames(pc_pre) <- heatmap_labels[colnames(pc_pre)]

pdf(file.path(FIGURES_DIR, "pdf/psychosocial_polychoric_heatmap.pdf"), width = 14, height = 12)
pheatmap(
 pc_pre,
 cluster_rows = TRUE,
 cluster_cols = TRUE,
 color = pal,
 breaks = brks,
 display_numbers = FALSE,
 legend_breaks = seq(-1, 1, by = 0.5),
 fontsize = 18,
 fontsize_row = 16,
 fontsize_col = 16,
 treeheight_row = 50,
 treeheight_col = 50,
 main = "Polychoric Correlations (PRE)"
)
dev.off()

png(file.path(FIGURES_DIR, "png/psychosocial_polychoric_heatmap.png"), width = 1400, height = 1200)
pheatmap(
 pc_pre,
 cluster_rows = TRUE,
 cluster_cols = TRUE,
 color = pal,
 breaks = brks,
 display_numbers = FALSE,
 legend_breaks = seq(-1, 1, by = 0.5),
 fontsize = 18,
 fontsize_row = 16,
 fontsize_col = 16,
 treeheight_row = 50,
 treeheight_col = 50,
 main = "Polychoric Correlations (PRE)"
)
dev.off()
cat("Saved: psychosocial_polychoric_heatmap.pdf/png\n")

# =============================================================================
# SECTION 12: REPORT GENERATION (conditional)
# =============================================================================

if (generate_report) {
 cat("\n--- Generating Markdown Report ---\n")

 report_lines <- c(
   "# Psychosocial Dimensionality Reduction",
   "",
   paste0("*Generated: ", Sys.time(), "*"),
   "",
   "---",
   "",
   "## Overview",
   "",
   "Exploratory Factor Analysis (EFA) of psychosocial wellbeing measures:",
   "",
   "- **WHO-5**: Wellbeing (5 items)",
   "- **PHQ-GAD-4**: Anxiety/Depression (4 items)",
   "- **UCLA-8**: Loneliness (8 items)",
   "- **LUBBEN-6**: Social connection (6 items)",
   "",
   "All items recoded so **higher = better** before analysis.",
   "",
   "---",
   "",
   "## Data Summary",
   "",
   sprintf("- **Total items**: %d", length(items_pos_clean)),
   sprintf("- **PRE observations**: %d", nrow(data_pre_pos)),
   sprintf("- **POST observations**: %d", nrow(data_post_pos)),
   sprintf("- **Effective N (PRE)**: %d", n_obs_pre),
   "",
   "---",
   "",
   "## Suitability Tests",
   ""
 )

 if (!inherits(kmo_obj, "try-error")) {
   report_lines <- c(report_lines,
     sprintf("- **KMO Overall**: %.3f", kmo_obj$MSA)
   )
 }
 if (!inherits(bart_obj, "try-error")) {
   report_lines <- c(report_lines,
     sprintf("- **Bartlett's Test**: chi-sq = %.2f, df = %d, p %s",
             bart_obj$chisq, bart_obj$df,
             ifelse(bart_obj$p.value < 0.001, "< 0.001", sprintf("= %.3f", bart_obj$p.value)))
   )
 }

 report_lines <- c(report_lines,
   "",
   "---",
   "",
   "## Factor Selection Criteria",
   "",
   sprintf("- **Parallel analysis**: %d factors", pa$nfact),
   sprintf("- **VSS complexity 1**: %d factors", which.max(map_result$vss.stats$cfit.1)),
   sprintf("- **VSS complexity 2**: %d factors", which.max(map_result$vss.stats$cfit.2)),
   "",
   "---",
   "",
   "## EFA Results (k=2)",
   "",
   "### Fit Indices",
   "",
   "| Metric | Value |",
   "|--------|-------|",
   sprintf("| Factors | %d |", k),
   sprintf("| RMSR | %.3f |", efa_pre$rms),
   sprintf("| RMSEA | %.3f |", efa_pre$RMSEA[1]),
   sprintf("| TLI | %.3f |", efa_pre$TLI),
   sprintf("| BIC | %.1f |", efa_pre$BIC),
   "",
   "### Variance Explained",
   "",
   sprintf("- **Factor 1**: %.1f%%", prop_var_f1 * 100),
   sprintf("- **Factor 2**: %.1f%%", prop_var_f2 * 100),
   sprintf("- **Total**: %.1f%%", cum_var * 100),
   "",
   sprintf("### Factor Correlation: r = %.3f", phi[1, 2]),
   "",
   "### Factor Loadings",
   "",
   "![Factor Loadings](../../outputs/figures/main_studies/png/psychosocial_loadings.png)",
   "",
   "### Polychoric Correlation Matrix",
   "",
   "![Heatmap](../../outputs/figures/main_studies/png/psychosocial_polychoric_heatmap.png)",
   "",
   "---",
   "",
   "## Factor Score Summary",
   "",
   "| Timepoint | Factor | Mean | SD |",
   "|-----------|--------|------|-----|",
   sprintf("| PRE | F1 | %.3f | %.3f |", mean(scores_pre$F1), sd(scores_pre$F1)),
   sprintf("| PRE | F2 | %.3f | %.3f |", mean(scores_pre$F2), sd(scores_pre$F2)),
   sprintf("| POST | F1 | %.3f | %.3f |", mean(scores_post$F1), sd(scores_post$F1)),
   sprintf("| POST | F2 | %.3f | %.3f |", mean(scores_post$F2), sd(scores_post$F2)),
   "",
   "---",
   "",
   "## Output Files",
   "",
   "### Tables",
   "- `outputs/tables/main_studies/tex_tables/psychosocial_loadings.tex`",
   "- `outputs/tables/main_studies/tex_tables/psychosocial_cors.tex`",
   "",
   "### Generated Data",
   "- `outputs/generated_data_files/psychosocial_factors.jsonl`",
   "",
   "### Figures",
   "- `outputs/figures/main_studies/pdf/psychosocial_loadings.pdf`",
   "- `outputs/figures/main_studies/pdf/psychosocial_polychoric_heatmap.pdf`",
   ""
 )

 writeLines(report_lines, file.path(REPORT_DIR, "03_psychosocial_dim_reduction.md"))
 cat("Saved: reports/main_studies/03_psychosocial_dim_reduction.md\n")
}

