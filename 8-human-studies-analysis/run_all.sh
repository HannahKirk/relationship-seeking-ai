#!/bin/bash
# =============================================================================
# Run All Analysis Scripts
# =============================================================================
# Generates all figures, models, tables, and reports for human studies analysis.
#
# Usage:
#   ./run_all.sh                      # Run all scripts (no reports/tables)
#   ./run_all.sh --generate_report    # Also generate markdown reports
#   ./run_all.sh --generate_tex_tables # Also generate LaTeX tables
#   ./run_all.sh --bootstrap_n=100    # Set bootstrap samples (for calibration study)
#   ./run_all.sh --clean              # Clear outputs before running
#   ./run_all.sh --contrasts-only     # Only run contrast computation and tables
#   ./run_all.sh --clean --generate_report --generate_tex_tables  # Full run with clean
#   ./run_all.sh --bootstrap_n=1      # Quick test run with minimal bootstrap
#
# Script order:
#   00 - Calibration study (separate validation study)
#   01 - Sociodemographics, Pre-treatment dimensionality reduction
#   02 - Pre-treatment attitudes
#   03 - Psychosocial dimensionality reduction
#   04 - Attrition analysis
#   05 - Preferences
#   06 - Attachment
#   07 - Goodbye
#   08 - Seeking companionship
#   09 - Psychosocial
#   10 - Mood
#   11 - Relational
#   12 - Sentience
#   13 - Post-survey relational
#   14 - Domain competency
#   15 - Vulnerability
#   16 - Decoupling
#   -- - Compute contrasts, FDR check, and hypothesis report
#   -- - Main paper plots
# =============================================================================

set -e  # Exit on error

# Change to script directory
cd "$(dirname "$0")"

# Parse arguments (opt-in for reports and tables)
REPORT_FLAG=""
TEX_FLAG=""
BOOTSTRAP_FLAG=""
CLEAN=false
CONTRASTS_ONLY=false

for arg in "$@"; do
    case $arg in
        --generate_report)
            REPORT_FLAG="--generate_report"
            echo "Will generate markdown reports..."
            ;;
        --generate_tex_tables)
            TEX_FLAG="--generate_tex_tables"
            echo "Will generate LaTeX tables..."
            ;;
        --bootstrap_n=*)
            BOOTSTRAP_FLAG="$arg"
            echo "Bootstrap samples: ${arg#--bootstrap_n=}"
            ;;
        --clean)
            CLEAN=true
            ;;
        --contrasts-only)
            CONTRASTS_ONLY=true
            ;;
    esac
done

# Clean outputs if requested
if [ "$CLEAN" = true ]; then
    echo "Clearing output folders..."
    rm -rf outputs/figures/*
    rm -rf outputs/tables/*
    rm -rf outputs/stats/*
    rm -rf outputs/models/*
    rm -rf outputs/generated_data_files/*
    echo "Output folders cleared."
    echo ""
fi

echo "============================================================"
echo "Human Studies Analysis - Running All Scripts"
echo "============================================================"
echo ""

# Skip to contrasts if --contrasts-only flag is set
if [ "$CONTRASTS_ONLY" = false ]; then

    # ==========================================================================
    # STAGE 0: Calibration Study (separate from main studies)
    # ==========================================================================
    echo ">>> STAGE 0: Calibration Study"
    echo "------------------------------------------------------------"

    echo ">>> [00] Running calibration_study.R..."
    Rscript scripts/analysis/calibration_study.R $REPORT_FLAG $TEX_FLAG $BOOTSTRAP_FLAG
    echo ""

    # ==========================================================================
    # STAGE 1: Data Preparation & Dimensionality Reduction
    # ==========================================================================
    echo ">>> STAGE 1: Data Preparation & Dimensionality Reduction"
    echo "------------------------------------------------------------"

    # 01. Sociodemographics (Python)
    echo ">>> [01] Running sociodemographics.py..."
    python scripts/analysis/sociodemographics.py $REPORT_FLAG $TEX_FLAG
    echo ""

    # 01. Pre-Treatment Dimensionality Reduction (Python)
    echo ">>> [01] Running pre_treatment_dim_reduction.py..."
    python scripts/analysis/pre_treatment_dim_reduction.py $REPORT_FLAG $TEX_FLAG
    echo ""

    # 02. Pre-Treatment Attitudes (R)
    echo ">>> [02] Running pre_treatment_attitudes.R..."
    Rscript scripts/analysis/pre_treatment_attitudes.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 03. Psychosocial Dimensionality Reduction (R)
    echo ">>> [03] Running psychosocial_dim_reduction.R..."
    Rscript scripts/analysis/psychosocial_dim_reduction.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 04. Attrition Analysis (R)
    echo ">>> [04] Running attrition_analysis.R..."
    Rscript scripts/analysis/attrition_analysis.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # ==========================================================================
    # STAGE 2: Primary Outcome Analysis (generates models for contrasts)
    # ==========================================================================
    echo ">>> STAGE 2: Primary Outcome Analysis"
    echo "------------------------------------------------------------"

    # 05. Preferences (R) - generates preferences_*.rds
    echo ">>> [05] Running preferences.R..."
    Rscript scripts/analysis/preferences.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 06. Attachment (R) - generates attachment_*.rds
    echo ">>> [06] Running attachment.R..."
    Rscript scripts/analysis/attachment.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 07. Goodbye (R) - generates goodbye_*.rds
    echo ">>> [07] Running goodbye.R..."
    Rscript scripts/analysis/goodbye.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 08. Seeking Companionship (R) - generates seeking_companionship_*.rds
    echo ">>> [08] Running seeking_companionship.R..."
    Rscript scripts/analysis/seeking_companionship.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 09. Psychosocial (R) - generates psychosocial_*.rds
    echo ">>> [09] Running psychosocial.R..."
    Rscript scripts/analysis/psychosocial.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 10. Mood (R) - generates mood_*.rds
    echo ">>> [10] Running mood.R..."
    Rscript scripts/analysis/mood.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 11. Relational (R) - generates relational_*.rds
    echo ">>> [11] Running relational.R..."
    Rscript scripts/analysis/relational.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 12. Sentience (R) - generates sentience_*.rds
    echo ">>> [12] Running sentience.R..."
    Rscript scripts/analysis/sentience.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # ==========================================================================
    # STAGE 3: Secondary Analyses
    # ==========================================================================
    echo ">>> STAGE 3: Secondary Analyses"
    echo "------------------------------------------------------------"

    # 13. Post-Survey Relational (R)
    echo ">>> [13] Running post_survey_relational.R..."
    Rscript scripts/analysis/post_survey_relational.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 14. Domain Competency (R)
    echo ">>> [14] Running domain_competency.R..."
    Rscript scripts/analysis/domain_competency.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 15. Vulnerability (R)
    echo ">>> [15] Running vulnerability.R..."
    Rscript scripts/analysis/vulnerability.R $REPORT_FLAG $TEX_FLAG
    echo ""

    # 16. Decoupling (R)
    echo ">>> [16] Running decoupling.R..."
    Rscript scripts/analysis/decoupling.R $REPORT_FLAG $TEX_FLAG
    echo ""

fi  # End of --contrasts-only skip

# ==========================================================================
# STAGE 4: Statistical Contrasts & Tables
# ==========================================================================
echo ">>> STAGE 4: Statistical Contrasts & Tables"
echo "------------------------------------------------------------"

# Compute contrasts for LONGITUDINAL (all tables: X1-X5)
echo ">>> Computing LONGITUDINAL contrasts (all tables)..."
Rscript scripts/analysis/compute_contrasts.R --all
echo ""

# Compute contrasts for CROSS-SECTIONAL (only X1 main effects)
echo ">>> Computing CROSS-SECTIONAL contrasts (main effects only)..."
Rscript scripts/analysis/compute_contrasts.R --all --cross-sectional
echo ""

# Run original pre-registration FDR check (robustness)
echo ">>> Running original pre-registration FDR check..."
Rscript scripts/analysis/original_prereg_fdr_check.R
echo ""

# Generate hypothesis report (includes LaTeX contrast tables with --generate_tex_tables)
echo ">>> Generating hypothesis report and contrast tables..."
Rscript scripts/analysis/generate_hypothesis_report.R $TEX_FLAG
echo ""

# ==========================================================================
# STAGE 5: Paper Plots
# ==========================================================================
echo ">>> STAGE 5: Paper Plots"
echo "------------------------------------------------------------"

# Generate main paper plots from JSON contrasts
echo ">>> Generating main paper plots..."
Rscript scripts/analysis/main_paper_plots.R
echo ""

# ==========================================================================
# DONE
# ==========================================================================
echo "============================================================"
echo "All scripts completed successfully!"
echo "============================================================"
echo ""
echo "Outputs:"
echo "  Figures:       outputs/figures/"
echo "  Paper Plots:   outputs/figures/paper_plots/"
echo "  Models:        outputs/models/"
echo "  Stats:         outputs/stats/"
echo "  Tables:        outputs/tables/main_studies/"
echo "    - tex_tables/:         Individual LaTeX tables"
echo "    - table_coordinators/: Parent files that organize table inputs"
echo ""
echo "Key files:"
echo "  - outputs/stats/*_contrasts.json (contrast results)"
echo "  - outputs/stats/original_prereg_fdr_results.json (pre-reg robustness)"
echo "  - outputs/tables/main_studies/table_coordinators/contrasts_parent.tex (unified contrasts)"
echo "  - reports/main_studies/hypothesis_report.md (hypothesis test summary)"
echo "  - outputs/figures/paper_plots/*.pdf (combined figures)"
echo ""
