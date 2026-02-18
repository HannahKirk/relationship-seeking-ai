#!/usr/bin/env python3
"""
Sociodemographics Analysis

This script analyzes participant demographics across the three studies:
- Calibration study
- Cross-sectional study
- Longitudinal study

Outputs:
- Demographic summary tables (HTML and LaTeX)
- Geographic distribution maps
- Markdown report with embedded figures

Usage:
    python scripts/analysis/sociodemographics.py                      # Run analysis only
    python scripts/analysis/sociodemographics.py --generate_report    # Run + generate report
    python scripts/analysis/sociodemographics.py --generate_tex_tables # Run + generate LaTeX tables
    python scripts/analysis/sociodemographics.py --report-only        # Report from prior run
"""

import argparse
import json
from datetime import datetime
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

# Add scripts to path for imports
import sys

SCRIPT_DIR = Path(__file__).parent
UTILS_DIR = SCRIPT_DIR.parent / "utils"
sys.path.insert(0, str(UTILS_DIR))
sys.path.insert(0, str(SCRIPT_DIR.parent))

from utils.plotting_utils import (
    setup_plotting,
    load_uk_geometries,
    plot_geographic_triangle,
    save_plot,
)
from utils.table_utils import (
    create_multi_study_demographic_table,
    style_demographic_table,
)

# Table caption
TABLE_CAPTION = (
    "Sociodemographic characteristics of participants across the three studies. "
    "Percentages shown within each study."
)


# =============================================================================
# PATH CONFIGURATION
# =============================================================================

PROJECT_ROOT = Path(__file__).parent.parent.parent
REPO_ROOT = PROJECT_ROOT.parent
DATA_DIR = REPO_ROOT / "data" / "human_study"

OUTPUT_DIR = PROJECT_ROOT / "outputs"
FIGURE_DIR = OUTPUT_DIR / "figures" / "main_studies"
TABLE_DIR = OUTPUT_DIR / "tables"
STATS_DIR = OUTPUT_DIR / "stats"
REPORT_DIR = PROJECT_ROOT / "reports" / "shared"

# GeoJSON files
GEO_DIR = UTILS_DIR / "geo_jsons"
COUNTRIES_FILE = GEO_DIR / "Countries_December_2021_UK_BUC_2022_5161029914231685949.geojson"
REGIONS_FILE = GEO_DIR / "Regions_December_2021_EN_BUC_2022_-5302211111801702970.geojson"

# Create directories (save_plot creates pdf/png subdirs automatically)
for d in [FIGURE_DIR, TABLE_DIR, STATS_DIR, REPORT_DIR]:
    d.mkdir(parents=True, exist_ok=True)


# =============================================================================
# CONFIGURATION
# =============================================================================

# Categorical variables to include in demographic table
CATEGORICAL_VARS = {
    "age_category": "Age",
    "gender": "Gender",
    "ethnicity": "Self-Reported Ethnicity",
    "religion": "Self-Reported Religion",
    "education": "Education",
    "income": "Income",
    "disability": "Disability",
    "ai_frequency": "Frequency of AI Use",
}

# Label cleaning mappings
LABEL_MAPPINGS = {
    "education": {
        "Vocational qualifications": "Vocational qualifications",
        "No qualifications": "No qualifications",
        "GCSEs or equivalent": "GCSEs",
        "A levels or equivalent": "A levels",
        "Undergraduate degree or equivalent": "Undergraduate degree",
        "Graduate study": "Graduate study",
    },
    "income": {
        "less than £10K": "<£10K",
    },
    "disability": {
        "I do not have a disability": "No disability",
        "I have minor health issues or a mild disability": "Minor disability",
        "I have major health issues or a major disability that significantly affects my day-to-day activities, but I am not registered disabled": "Non-registered major disability",
        "Yes, and I am registered disabled": "Registered disabled",
    },
    "ai_frequency": {
        "I have never used a chatbot": "Never",
    },
    "ethnicity": {
        "White": "White",
        "Asian and Asian British": "Asian",
        "Mixed and multiple ethnic groups": "Mixed",
        "Black, African, Caribbean or Black British": "Black",
    },
}


# =============================================================================
# DATA LOADING
# =============================================================================


def load_data():
    """Load and combine sociodemographic data from all studies."""
    print("Loading data...")

    # Calibration study
    calibration_df = pd.read_json(
        DATA_DIR / "calibration_study" / "sociodemographics.jsonl", lines=True
    )
    calibration_df["study_id"] = "calibration"

    # Main study
    main_df = pd.read_json(
        DATA_DIR / "main_studies" / "sociodemographics.jsonl", lines=True
    )

    # Combine
    combined_df = pd.concat([calibration_df, main_df], axis=0)

    print(f"  Combined shape: {combined_df.shape}")
    print(f"  Study IDs: {combined_df['study_id'].value_counts().to_dict()}")

    return combined_df


def clean_labels(df):
    """Apply label cleaning mappings to demographic columns."""
    print("Cleaning labels...")
    df = df.copy()

    for col, mapping in LABEL_MAPPINGS.items():
        if col in df.columns:
            df[col] = df[col].replace(mapping)

    return df


# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================


def create_demographic_tables(df, generate_tex_tables=False):
    """Create demographic summary table for LaTeX output."""
    print("Creating demographic table...")

    # Create the base table
    table_df, headers, count_cols, bar_cols = create_multi_study_demographic_table(
        df, CATEGORICAL_VARS, study_col="study_id"
    )

    if generate_tex_tables:
        # Define custom column spec and multicolumn headers for clean LaTeX output
        column_spec = "p{4cm}p{1cm}p{2.3cm}p{1cm}p{2.3cm}p{1cm}p{2.3cm}"
        multicolumn_headers = (
            " & \\multicolumn{2}{c}{\\textbf{Calibration}} "
            "& \\multicolumn{2}{c}{\\textbf{Cross-sectional}} "
            "& \\multicolumn{2}{c}{\\textbf{Longitudinal}}"
        )

        # Style for LaTeX
        print("  Styling LaTeX table...")
        latex_str = style_demographic_table(
            table_df,
            headers,
            category_cols=["Category"],
            count_cols=count_cols,
            bar_cols=bar_cols,
            format="latex",
            highlight_mode="rows",
            environment="longtable",
            caption=TABLE_CAPTION,
            label="tab:sociodemographics",
            column_spec=column_spec,
            multicolumn_headers=multicolumn_headers,
        )

        # Save LaTeX
        latex_path = TABLE_DIR / "sociodemographics.tex"
        with open(latex_path, "w") as f:
            f.write(latex_str)
        print(f"  Saved: {latex_path}")

    return table_df, headers, count_cols, bar_cols


def create_geographic_plot(df):
    """Create geographic distribution choropleth maps."""
    print("Creating geographic plot...")

    # Load UK geometries
    base_gdf, xlim, ylim = load_uk_geometries(COUNTRIES_FILE, REGIONS_FILE)

    # Create triangle layout
    fig = plot_geographic_triangle(
        df,
        base_gdf,
        xlim,
        ylim,
        study_col="study_id",
        geo_col="geoglocation",
    )

    # Save figures
    save_plot(FIGURE_DIR, fig, "geographic_distribution")
    plt.close(fig)

    return fig


def compute_summary_stats(df):
    """Compute summary statistics for the demographic data."""
    print("Computing summary statistics...")

    stats = {
        "n_total": len(df),
        "study_sizes": df["study_id"].value_counts().to_dict(),
    }

    # Compute statistics for each categorical variable
    for col, label in CATEGORICAL_VARS.items():
        if col in df.columns:
            value_counts = df[col].value_counts(dropna=False).to_dict()
            # Convert any NaN keys to string
            value_counts = {
                str(k) if pd.isna(k) else k: v for k, v in value_counts.items()
            }
            stats[f"{col}_distribution"] = value_counts

    return stats


def save_stats_json(stats, path):
    """Save statistics to JSON file."""
    with open(path, "w") as f:
        json.dump(stats, f, indent=2, default=str)
    print(f"  Saved stats: {path}")


# =============================================================================
# REPORT GENERATION
# =============================================================================


def generate_markdown_report(stats):
    """Generate a markdown report with embedded figures."""
    print("Generating markdown report...")

    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    lines = [
        "# Sociodemographics Analysis",
        "",
        f"*Generated: {timestamp}*",
        "",
        "## Overview",
        "",
        "This analysis summarizes participant demographics across the three studies.",
        "",
        f"**Total participants**: {stats['n_total']:,}",
        "",
        "### Study Sample Sizes",
        "",
        "| Study | N |",
        "|-------|---|",
    ]

    # Add study sizes in order
    study_order = ["calibration", "cross-sectional", "longitudinal"]
    for study in study_order:
        n = stats["study_sizes"].get(study, 0)
        lines.append(f"| {study.title()} | {n:,} |")

    lines.extend([
        "",
        "---",
        "",
        "## Demographic Summary Table",
        "",
        "See `outputs/tables/sociodemographics.tex` for LaTeX table.",
        "",
        "---",
        "",
        "## Geographic Distribution",
        "",
        "![Geographic Distribution](../../outputs/figures/main_studies/png/geographic_distribution.png)",
        "",
        "---",
        "",
        "## Detailed Breakdowns",
        "",
    ])

    # Add detailed breakdowns for each demographic variable
    for col, label in CATEGORICAL_VARS.items():
        dist_key = f"{col}_distribution"
        if dist_key in stats:
            lines.extend([
                f"### {label}",
                "",
                "| Category | Count | % |",
                "|----------|-------|---|",
            ])

            dist = stats[dist_key]
            total = sum(dist.values())

            # Sort by count descending
            sorted_items = sorted(dist.items(), key=lambda x: x[1], reverse=True)

            for category, count in sorted_items:
                pct = (count / total * 100) if total > 0 else 0
                cat_str = str(category) if category != "nan" else "Missing"
                lines.append(f"| {cat_str} | {count:,} | {pct:.1f}% |")

            lines.extend(["", ""])

    # Write report
    report_path = REPORT_DIR / "01_sociodemographics.md"
    with open(report_path, "w") as f:
        f.write("\n".join(lines))

    print(f"  Saved report: {report_path}")


# =============================================================================
# MAIN
# =============================================================================


def main(generate_report=False, generate_tex_tables=False, report_only=False):
    """Run the full sociodemographics analysis pipeline."""
    setup_plotting()

    stats_path = STATS_DIR / "sociodemographics_participant_counts.json"

    if report_only:
        # Load existing stats and generate report
        print("Loading existing stats for report generation...")
        with open(stats_path, "r") as f:
            stats = json.load(f)
        generate_markdown_report(stats)
        return

    # Load and prepare data
    combined_df = load_data()
    combined_df = clean_labels(combined_df)

    # Create demographic tables
    create_demographic_tables(combined_df, generate_tex_tables=generate_tex_tables)

    # Create geographic plot
    create_geographic_plot(combined_df)

    # Compute and save summary stats
    stats = compute_summary_stats(combined_df)
    save_stats_json(stats, stats_path)

    # Generate report if requested
    if generate_report:
        generate_markdown_report(stats)

    print("\n" + "=" * 60)
    print("ANALYSIS COMPLETE")
    print("=" * 60)
    print(f"\nTotal participants: {stats['n_total']:,}")
    print(f"Study sizes: {stats['study_sizes']}")
    print(f"\nOutputs:")
    print(f"  - Tables: {TABLE_DIR}")
    print(f"  - Figures: {FIGURE_DIR}")
    print(f"  - Stats: {stats_path}")
    if generate_report:
        print(f"  - Report: {REPORT_DIR / '01_sociodemographics.md'}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Sociodemographics Analysis")
    parser.add_argument(
        "--generate_report",
        action="store_true",
        help="Generate markdown report after analysis",
    )
    parser.add_argument(
        "--generate_tex_tables",
        action="store_true",
        help="Generate LaTeX tables",
    )
    parser.add_argument(
        "--report-only",
        action="store_true",
        help="Only generate report (requires prior analysis run)",
    )

    args = parser.parse_args()

    main(
        generate_report=args.generate_report,
        generate_tex_tables=args.generate_tex_tables,
        report_only=args.report_only,
    )
