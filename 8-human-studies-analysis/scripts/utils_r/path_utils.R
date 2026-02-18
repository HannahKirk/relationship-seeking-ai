# =============================================================================
# Path Utilities
# =============================================================================
#
# Helper function to consistently set project paths across all analysis scripts.
#
# Usage:
#   source("scripts/utils_r/path_utils.R")
#   paths <- setup_project_paths()
#   # Then use: paths$PROJECT_ROOT, paths$REPO_ROOT, paths$DATA_DIR, etc.
#
# =============================================================================

#' Set up project paths consistently across scripts
#'
#' Detects whether running from Rscript or interactive R session and sets
#' paths accordingly. Returns a list of commonly used directories.
#'
#' @return List with PROJECT_ROOT, REPO_ROOT, and common output directories
setup_project_paths <- function() {
  # Try to get script directory from different contexts
  script_dir <- tryCatch({
    # When sourced from another script
    dirname(sys.frame(1)$ofile)
  }, error = function(e) {
    tryCatch({
      # When run via Rscript
      args <- commandArgs(trailingOnly = FALSE)
      file_arg <- args[grepl("--file=", args)]
      if (length(file_arg) > 0) {
        dirname(normalizePath(sub("--file=", "", file_arg)))
      } else {
        NULL
      }
    }, error = function(e) NULL)
  })

  # Fallback: assume we're in project root

if (is.null(script_dir) || script_dir == "" || script_dir == ".") {
    project_root <- normalizePath(getwd(), mustWork = FALSE)
  } else {
    # Script is in scripts/analysis/ or scripts/utils_r/, so go up 2 levels
    project_root <- normalizePath(file.path(script_dir, "../.."), mustWork = FALSE)
  }

  repo_root <- normalizePath(file.path(project_root, ".."), mustWork = FALSE)

  list(
    PROJECT_ROOT = project_root,
    REPO_ROOT = repo_root,
    # Output directories
    FIGURE_DIR = file.path(project_root, "outputs/figures/main_studies"),
    TABLE_DIR = file.path(project_root, "outputs/tables/main_studies"),
    STATS_DIR = file.path(project_root, "outputs/stats"),
    MODEL_DIR = file.path(project_root, "outputs/models"),
    REPORT_DIR = file.path(project_root, "reports/main_studies"),
    GENERATED_DIR = file.path(project_root, "outputs/generated_data_files"),
    # Data directories
    DATA_DIR = file.path(repo_root, "data/human_study/main_studies"),
    CALIBRATION_DATA_DIR = file.path(repo_root, "data/human_study/calibration_study")
  )
}
