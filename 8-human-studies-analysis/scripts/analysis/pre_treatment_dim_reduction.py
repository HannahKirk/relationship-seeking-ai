#!/usr/bin/env python3
"""
Pre-Treatment Attitudes Analysis

Performs cluster analysis and factor analysis on pre-treatment attitudes
toward AI anthropomorphism and relationship-seeking.

Usage:
    python scripts/analysis/pre_treatment_dim_reduction.py
    python scripts/analysis/pre_treatment_dim_reduction.py --generate_report
    python scripts/analysis/pre_treatment_dim_reduction.py --generate_tex_tables
"""

# =============================================================================
# SETUP
# =============================================================================

import argparse
import json
import sys
import warnings
from datetime import datetime
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from scipy.stats import pearsonr, ttest_ind, skew, kurtosis

# Suppress sklearn deprecation warning from factor_analyzer
warnings.filterwarnings("ignore", message="'force_all_finite' was renamed")

# Path setup
SCRIPT_DIR = Path(__file__).resolve().parent
SCRIPTS_DIR = SCRIPT_DIR.parent
PROJECT_ROOT = SCRIPTS_DIR.parent  # 8-human-studies-analysis/
REPO_ROOT = PROJECT_ROOT.parent  # relationship-seeking-ai/
sys.path.insert(0, str(SCRIPTS_DIR))

# Relative imports
from utils.data_utils import load_study_data, save_data
from utils.stats_utils import (
    find_optimal_k,
    run_kmeans,
    label_clusters_by_mean,
    run_anova_with_fdr,
    impute_with_mean,
    standardize_data,
    get_factor_eigenvalues,
    run_factor_analysis,
    get_factor_loadings,
    get_factor_scores,
)
from utils.plotting_utils import (
    setup_plotting,
    apply_theme,
    save_plot,
    plot_scree,
    plot_factor_distribution,
    plot_correlation_clustermap,
    plot_kmeans_diagnostics,
    plot_cluster_pca,
    plot_cluster_means_by_category,
    CLUSTER_COLORS,
    SOURCE_COLORS,
)


# Output directories
FIGURE_DIR = PROJECT_ROOT / "outputs" / "figures" / "main_studies"
TABLE_DIR = PROJECT_ROOT / "outputs" / "tables" / "main_studies" / "tex_tables"
STATS_DIR = PROJECT_ROOT / "outputs" / "stats"
REPORT_DIR = PROJECT_ROOT / "reports"
GENERATED_DIR = PROJECT_ROOT / "outputs" / "generated_data_files"
REPORT_DIR_MAIN = REPORT_DIR / "main_studies"

# Create directories (save_plot function creates pdf/png subdirs automatically)
for d in [FIGURE_DIR, TABLE_DIR, STATS_DIR, REPORT_DIR, REPORT_DIR_MAIN, GENERATED_DIR]:
    d.mkdir(parents=True, exist_ok=True)

# Reproducibility seed
RANDOM_SEED = 42
np.random.seed(RANDOM_SEED)

# =============================================================================
# CONFIGURATION
# =============================================================================

CLUSTER_NAMES = {0: "Anthro Skeptic", 1: "Anthro Enthusiast"}

ANTHRO_CATEGORIES = {
    "Anthropomorphic AI Preferences": [
        "cold_warm",
        "impersonal_personal",
        "unsociable_sociable",
        "robot_human",
        "insensitive_sensitive",
    ],
    "Ascribing Mental States": [
        "mental_states:think_believe",
        "mental_states:happy_to_help",
        "mental_states:has_emotions",
        "mental_states:express_preferences",
    ],
    "Accepting Relationships": [
        "relationships:ai_as_friend",
        "relationships:express_love",
        "relationships:meaningful_over_humans",
    ],
    "Casual Interaction Style": [
        "tone:swear_slang",
        "tone:funny_offbeat",
        "tone:give_opinions",
    ],
    "Tool vs Friend": ["tool_friend"],
    "Seeking Companionship": ["seeking_companionship_likelihood_pre"],
}

PREF_COLS = [
    "cold_warm",
    "impersonal_personal",
    "insensitive_sensitive",
    "personalisation_agreement",
    "robot_human",
    "tool_friend",
    "unsociable_sociable",
]


# =============================================================================
# 2. DATA LOADING AND PREPARATION
# =============================================================================


def load_data():
    """Load all required datasets."""
    print("Loading data...")
    attitudes_df = load_study_data("anthro-attitudes")
    prefs_df = load_study_data("stated-preferences")
    companionship_df = load_study_data("seeking-companionship")

    return attitudes_df, prefs_df, companionship_df


def prepare_attitudes(attitudes_df):
    """Prepare attitudes data with reverse coding for single item."""
    df = attitudes_df.copy()

    # Assert all timepoints are pre
    assert (
        df["timepoint"].nunique() == 1 and df["timepoint"].unique()[0] == "pre"
    ), "Expected only 'pre' timepoint in attitudes data"

    # Flip "rude_to_ai" to "polite_to_ai" so all items point in pro-anthro direction
    # previously slider_response = "agreement with positive valence statement"
    # For rude_to_ai: positive valence = "OK to be rude" = which we interpret as anti-anthro
    # So we flip the direction of the response
    rude_mask = df["slider_name"] == "rude_to_ai"
    df.loc[rude_mask, "slider_name"] = "polite_to_ai"
    df.loc[rude_mask, "slider_response"] = 100 - df.loc[rude_mask, "slider_response"]

    # Create full item name: category:name
    df["item"] = df["slider_category"] + ":" + df["slider_name"]
    df["item_source"] = "Societal Attitudes to Anthropomorphism Survey"

    df = df[["ppt_id", "study_id", "item", "item_source", "slider_response", "week"]]

    # Print number of participants per study
    print("Number of participants per study (soc attitudes):")
    print(df.groupby("study_id")["ppt_id"].nunique().to_string())

    # Print unique items stats
    print(f"Unique items (soc attitudes): {df['item'].nunique()}")
    return df


def prepare_preferences(prefs_df):
    """Prepare stated preferences data."""
    df = prefs_df.copy()

    id_cols = ["ppt_id", "week", "day", "study_id", "timepoint"]

    # Melt takes the wide format df and turns it into long format to match attitudes_df, with one row per ppt_id x item
    melted_df = df.melt(
        id_vars=id_cols,
        value_vars=PREF_COLS,
        var_name="item",
        value_name="slider_response",
    )
    melted_df = melted_df.sort_values(["ppt_id", "item"]).reset_index(drop=True)

    # Assert all timepoints are pre
    assert (
        melted_df["timepoint"].nunique() == 1
        and melted_df["timepoint"].unique()[0] == "pre"
    ), "Expected only 'pre' timepoint in preferences data"
    melted_df["item_source"] = "Ideal AI Preferences Survey"
    melted_df = melted_df[
        ["ppt_id", "study_id", "item", "item_source", "slider_response", "week"]
    ]

    # Print number of participants per study
    print("Number of participants per study (preferences):")
    print(melted_df.groupby("study_id")["ppt_id"].nunique().to_string())

    # Print unique items stats
    print(f"Unique items (preferences): {melted_df['item'].nunique()}")
    return melted_df


def prepare_companionship(companionship_df):
    """Prepare seeking companionship baseline measure."""
    df = companionship_df.copy()

    # Print pre and post timepoints to check
    print("Timepoints in companionship data:")
    print(
        df.groupby(["study_id", "timepoint"])["ppt_id"].nunique().unstack().to_string()
    )
    # Filter to pre-treatment only
    df = df[df["timepoint"] == "pre"]
    assert (
        df["timepoint"].nunique() == 1 and df["timepoint"].unique()[0] == "pre"
    ), "Expected only 'pre' timepoint in companionship data"

    df = df[["ppt_id", "study_id", "seeking_companionship_likelihood", "week"]]

    # Rename the response column to match format of other datasets
    df = df.rename(columns={"seeking_companionship_likelihood": "slider_response"})

    # Metadata for consistent merge
    df["item"] = "seeking_companionship_likelihood_pre"
    df["item_source"] = "Seeking Companionship Survey"

    df = df[["ppt_id", "study_id", "item", "item_source", "slider_response", "week"]]

    # Print number of participants per study
    print("Number of participants per study (companionship):")
    print(df.groupby("study_id")["ppt_id"].nunique().to_string())
    # Print unique items stats
    print(f"Unique items (companionship): {df['item'].nunique()}")
    return df


def combine_and_pivot(attitudes_df_long, prefs_df_long, companionship_df_long):
    """Combine all sources and pivot to wide format."""
    # First combine all datasets in long format
    combined_df = pd.concat(
        [attitudes_df_long, prefs_df_long, companionship_df_long], ignore_index=True
    )

    # Store item -> source mapping for later reference
    item_source_map = (
        combined_df[["item", "item_source"]]
        .drop_duplicates()
        .set_index("item")["item_source"]
        .to_dict()
    )

    print("Number of participants per study (combined):")
    print(combined_df.groupby("study_id")["ppt_id"].nunique().to_string())

    # Pivot to wide format
    wide_df = combined_df.pivot_table(
        index=["ppt_id", "study_id"],
        columns="item",
        values="slider_response",
    )
    wide_df.reset_index(inplace=True)

    non_id_cols = [col for col in wide_df.columns if col not in ["ppt_id", "study_id"]]

    print(f"Wide format: {len(wide_df)} participants x {len(non_id_cols)} items")

    # Report missing data
    missing = wide_df.isnull().sum()
    missing_items = missing[missing > 0]
    if len(missing_items) > 0:
        print(f"Missing data: {missing_items.to_dict()}")

    # Assert we have one row per participant
    assert wide_df[
        "ppt_id"
    ].is_unique, "Expected one row per participant in wide format"

    return wide_df, item_source_map


# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================


def run_correlation_analysis(wide_df_imputed, item_source_map):
    """Compute and plot correlation matrix."""
    print("\n--- Correlation Analysis ---")

    corr_matrix = wide_df_imputed.corr(method="pearson")

    g = plot_correlation_clustermap(
        corr_matrix,
        item_source_map=item_source_map,
        source_palette=SOURCE_COLORS,
        annot_fontsize=10,
        tick_fontsize=14,
    )

    # Save clustermap (it's a ClusterGrid, not a Figure)
    (FIGURE_DIR / "pdf").mkdir(parents=True, exist_ok=True)
    (FIGURE_DIR / "png").mkdir(parents=True, exist_ok=True)
    g.savefig(FIGURE_DIR / "pdf" / "pre_attitudes_correlation_clustermap.pdf", bbox_inches="tight")
    g.savefig(
        FIGURE_DIR / "png" / "pre_attitudes_correlation_clustermap.png", bbox_inches="tight", dpi=150
    )
    plt.close()
    print(f"Saved: pre_attitudes_correlation_clustermap.pdf/png")

    return corr_matrix


def run_cluster_analysis(wide_df_imputed, data_scaled, feature_cols):
    """Run K-means clustering analysis."""
    print("\n--- Cluster Analysis ---")

    # Find optimal k
    k_results = find_optimal_k(data_scaled, k_range=range(2, 8))

    fig = plot_kmeans_diagnostics(k_results)
    save_plot(FIGURE_DIR, fig, "pre_attitudes_kmeans_diagnostics")
    plt.close(fig)

    # Print results
    print("\nCluster Optimization Results:")
    print(f"{'k':>3} {'Inertia':>12} {'Silhouette':>12} {'CH Index':>12}")
    print("-" * 45)
    for i, k in enumerate(k_results["k"]):
        print(
            f"{k:>3} {k_results['inertia'][i]:>12.0f} "
            f"{k_results['silhouette'][i]:>12.3f} "
            f"{k_results['calinski_harabasz'][i]:>12.1f}"
        )

    best_k_silhouette = k_results["k"][np.argmax(k_results["silhouette"])]
    print(f"\nBest silhouette: k={best_k_silhouette}")

    # Run with optimal k
    kmeans, cluster_labels = run_kmeans(data_scaled, n_clusters=best_k_silhouette)

    print(f"\nK-means with k={best_k_silhouette}")
    print(f"Cluster sizes: {np.bincount(cluster_labels)}")

    # Create analysis dataframe (impute missing data)
    cluster_df = wide_df_imputed.copy()
    cluster_df["cluster"] = cluster_labels

    # Lower mean score = more skeptical/less pro-anthropomorphic
    # So we assign that to cluster 0, and the more enthusiastic cluster gets index 1 after mean sorting
    mapping, cluster_df = label_clusters_by_mean(
        cluster_df, "cluster", feature_cols, CLUSTER_NAMES
    )
    print(f"Cluster mapping (old -> new): {mapping}")

    # Update labels to match new mapping
    cluster_labels = cluster_df["cluster"].values

    # Plot clusters
    fig = plot_cluster_pca(
        data_scaled,
        cluster_labels,
        kmeans,
        CLUSTER_NAMES,
    )
    save_plot(FIGURE_DIR, fig, "pre_attitudes_cluster_pca")
    plt.close(fig)

    # Plot cluster means by category
    fig = plot_cluster_means_by_category(
        cluster_df,
        "cluster",
        ANTHRO_CATEGORIES,
        CLUSTER_NAMES,
    )
    save_plot(FIGURE_DIR, fig, "pre_attitudes_cluster_means")
    plt.close(fig)

    # Run ANOVA
    anova_results = run_anova_with_fdr(cluster_df, "cluster", feature_cols)

    print("\nANOVA Results (top 5):")
    print(anova_results.head().to_string())

    n_sig = anova_results["significant"].sum()
    print(
        f"\nSignificant after FDR correction: {n_sig} / {len(feature_cols)} variables"
    )

    # Store optimal k
    optimal_k = best_k_silhouette
    return cluster_df, kmeans, k_results, anova_results, optimal_k


def run_factor_analysis_pipeline(data_scaled, feature_cols, wide_df_imputed):
    """Run factor analysis."""
    print("\n--- Factor Analysis ---")

    # Scree plot
    eigenvalues = get_factor_eigenvalues(data_scaled)
    n_kaiser = sum(eigenvalues > 1)
    print(f"Factors with eigenvalue > 1 (Kaiser): {n_kaiser}")

    fig = plot_scree(eigenvalues)
    save_plot(FIGURE_DIR, fig, "pre_attitudes_factor_scree")
    plt.close(fig)

    # Run factor analysis
    fa, variance = run_factor_analysis(data_scaled, n_kaiser)

    print("\nVariance Explained:")
    total_var = 0
    for i, var in enumerate(variance[1]):
        print(f"  Factor {i+1}: {var:.3f} ({var*100:.1f}%)")
        total_var += var
    print(f"  Total: {total_var:.3f} ({total_var*100:.1f}%)")

    # Get loadings
    loadings = get_factor_loadings(fa, feature_cols, n_kaiser)

    print("\nTop 5 items per factor:")
    for i in range(n_kaiser):
        col = f"Factor_{i+1}"
        print(f"\n{col}:")
        top_items = loadings[col].abs().sort_values(ascending=False).head(5)
        for item in top_items.index:
            print(f"  {item}: {loadings.loc[item, col]:.3f}")

    # Get scores
    factor_scores = get_factor_scores(fa, data_scaled, wide_df_imputed.index, n_kaiser)

    # Factor 1 distribution statistics
    f1_scores = factor_scores["Factor_1_Score"]
    f1_stats = {
        "mean": f1_scores.mean(),
        "std": f1_scores.std(),
        "skewness": skew(f1_scores),
        "kurtosis": kurtosis(f1_scores),  # excess kurtosis (Fisher's definition)
    }
    print(f"\nFactor 1 distribution: M = {f1_stats['mean']:.2f}, "
          f"SD = {f1_stats['std']:.2f}, skewness = {f1_stats['skewness']:.3f}, "
          f"kurtosis = {f1_stats['kurtosis']:.3f}")

    # Plot Factor 1 distribution
    fig = plot_factor_distribution(
        factor_scores["Factor_1_Score"],
        "Factor 1",
    )
    save_plot(FIGURE_DIR, fig, "pre_attitudes_factor1_distribution")
    plt.close(fig)

    # Store number of factors
    n_factors = n_kaiser
    return fa, loadings, factor_scores, variance, eigenvalues, n_factors, f1_stats


def save_factor_loadings_latex(loadings, n_factors, output_path):
    """Save factor loadings as LaTeX table with top 5 per factor highlighted."""
    # Identify top 5 items per factor by absolute loading
    top_items = {}
    for i in range(n_factors):
        col = f"Factor_{i+1}"
        top_5 = loadings[col].abs().sort_values(ascending=False).head(5).index.tolist()
        top_items[col] = top_5

    # Build LaTeX table
    lines = [
        "\\begin{table}[!htbp]",
        "\\footnotesize",
        "\\centering",
        "\\caption{Factor loadings from exploratory factor analysis of pre-treatment attitude items. "
        "Top 5 loadings per factor are highlighted in gray.}",
        "\\label{tab:factor_loadings}",
        "\\begin{tabular}{l" + "r" * n_factors + "}",
        "\\toprule",
        "Item & " + " & ".join(f"Factor {i+1}" for i in range(n_factors)) + " \\\\",
        "\\midrule",
    ]

    for item in loadings.index:
        # Clean item name for LaTeX (escape underscores, etc.)
        item_clean = item.replace("_", "\\_").replace(":", ": ")
        row_vals = []
        for i in range(n_factors):
            col = f"Factor_{i+1}"
            val = loadings.loc[item, col]
            val_str = f"{val:.3f}"
            # Highlight top 5 items
            if item in top_items[col]:
                val_str = f"\\cellcolor{{gray!25}}{val_str}"
            row_vals.append(val_str)
        lines.append(f"{item_clean} & " + " & ".join(row_vals) + " \\\\")

    lines.extend([
        "\\bottomrule",
        "\\end{tabular}",
        "\\end{table}",
    ])

    with open(output_path, "w") as f:
        f.write("\n".join(lines))

    print(f"Saved: {output_path.name}")


def compare_cluster_factor(cluster_df, factor_scores):
    """Compare cluster assignments with factor scores."""
    print("\n--- Cluster vs Factor Comparison ---")

    comparison_df = cluster_df.reset_index().merge(
        factor_scores.reset_index()[["ppt_id", "study_id", "Factor_1_Score"]],
        on=["ppt_id", "study_id"],
        how="left",
    )

    # Calculate group stats
    group_stats = comparison_df.groupby("cluster_name")["Factor_1_Score"].agg(
        ["mean", "std", "count"]
    )
    print("\nFactor 1 scores by cluster:")
    print(group_stats)

    # T-test
    enthusiasts = comparison_df[comparison_df["cluster_name"] == "Anthro Enthusiast"][
        "Factor_1_Score"
    ]
    skeptics = comparison_df[comparison_df["cluster_name"] == "Anthro Skeptic"][
        "Factor_1_Score"
    ]

    t_stat, p_val = ttest_ind(enthusiasts, skeptics)

    print(f"\nT-test: t = {t_stat:.2f}, p = {p_val:.2e}")

    # Format for paper
    enth_m, enth_sd = group_stats.loc["Anthro Enthusiast", ["mean", "std"]]
    skep_m, skep_sd = group_stats.loc["Anthro Skeptic", ["mean", "std"]]
    print(f"\nFor paper: Enthusiasts (M = {enth_m:.2f}, SD = {enth_sd:.2f}) vs "
          f"Skeptics (M = {skep_m:.2f}, SD = {skep_sd:.2f}), p = {p_val:.2e}")

    comparison_stats = {
        "t_stat": t_stat,
        "p_value": p_val,
        "enthusiast_mean": enth_m,
        "enthusiast_sd": enth_sd,
        "skeptic_mean": skep_m,
        "skeptic_sd": skep_sd,
    }
    return comparison_df, comparison_stats


# =============================================================================
# EXPORT AND REPORT GENERATION
# =============================================================================


def save_stats_json(stats_dict, filename):
    """Save statistics to JSON file."""
    output_path = STATS_DIR / f"{filename}.json"

    # Convert numpy types to Python types
    def convert(obj):
        if isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        elif isinstance(obj, pd.DataFrame):
            return obj.to_dict()
        elif isinstance(obj, pd.Series):
            return obj.to_dict()
        return obj

    converted = {k: convert(v) for k, v in stats_dict.items()}

    with open(output_path, "w") as f:
        json.dump(converted, f, indent=2, default=str)

    print(f"Saved: {output_path}")


def generate_markdown_report(
    cluster_df,
    k_results,
    anova_results,
    loadings,
    variance,
    comparison_stats,
    item_source_map,
    optimal_k,
    n_factors,
    f1_stats=None,
):
    """Generate markdown report with embedded figures."""
    report_path = REPORT_DIR_MAIN / "02_pre_treatment_dim_reduction.md"

    # Relative path from report to PNG figures
    fig_rel_path = "../../outputs/figures/main_studies/png"

    # Get cluster counts
    cluster_counts = cluster_df["cluster_name"].value_counts()

    # Build report
    lines = [
        "# Pre-Treatment Attitudes: Dimensionality Reduction",
        "",
        f"*Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*",
        "",
        "## Overview",
        "",
        "This analysis performs cluster analysis and factor analysis on pre-treatment attitudes",
        "toward anthropomorphic and relationship-seeking AI, combining:",
        "",
        "- **Societal attitude to anthropomorphism survey**: 12 items",
        "- **Stated preferences for ideal AI assistant survey**: 7 items",
        "- **Seeking companionship**: 1 item (longitudinal only)",
        "",
        f"**Total participants**: {len(cluster_df)}",
        "",
        "---",
        "",
        "## Correlation Analysis",
        "",
        f"![Correlation Clustermap]({fig_rel_path}/pre_attitudes_correlation_clustermap.png)",
        "",
        "---",
        "",
        "## Cluster Analysis",
        "",
        f"K-means clustering with k={optimal_k}:",
        "",
    ]

    for name in CLUSTER_NAMES.values():
        count = cluster_counts.get(name, 0)
        pct = count / len(cluster_df) * 100
        lines.append(f"- **{name}**: {count} participants ({pct:.1f}%)")

    lines.extend(
        [
            "",
            "### Cluster Optimization",
            "",
            f"![K-means Diagnostics]({fig_rel_path}/pre_attitudes_kmeans_diagnostics.png)",
            "",
            "| k | Silhouette | Calinski-Harabasz |",
            "|---|------------|-------------------|",
        ]
    )

    for i, k in enumerate(k_results["k"]):
        lines.append(
            f"| {k} | {k_results['silhouette'][i]:.3f} | {k_results['calinski_harabasz'][i]:.1f} |"
        )

    lines.extend(
        [
            "",
            "### Cluster Visualization",
            "",
            f"![Cluster PCA]({fig_rel_path}/pre_attitudes_cluster_pca.png)",
            "",
            "### Cluster Profiles by Category",
            "",
            f"![Cluster Means]({fig_rel_path}/pre_attitudes_cluster_means.png)",
            "",
            "### ANOVA Results",
            "",
            "Significant differences between clusters (FDR-corrected p < 0.05).",
            "",
            "| Variable | F-stat | p-value | p-FDR | Sig |",
            "|----------|--------|---------|-------|-----|",
        ]
    )

    for _, row in anova_results.iterrows():
        sig = (
            "***"
            if row["p_fdr"] < 0.001
            else ("**" if row["p_fdr"] < 0.01 else ("*" if row["p_fdr"] < 0.05 else ""))
        )
        lines.append(
            f"| {row['variable']} | {row['f_stat']:.1f} | {row['p_value']:.2e} | {row['p_fdr']:.2e} | {sig} |"
        )

    # Factor analysis section
    lines.extend(
        [
            "",
            "---",
            "",
            "## Factor Analysis",
            "",
            f"Extracted {n_factors} factors using varimax rotation.",
            "",
            "### Scree Plot",
            "",
            f"![Scree Plot]({fig_rel_path}/pre_attitudes_factor_scree.png)",
            "",
            "### Variance Explained",
            "",
            "| Factor | Variance | Cumulative |",
            "|--------|----------|------------|",
        ]
    )

    cumulative = 0
    for i, var in enumerate(variance[1]):
        cumulative += var
        lines.append(f"| Factor {i+1} | {var*100:.1f}% | {cumulative*100:.1f}% |")

    lines.extend(
        [
            "",
            "### Factor Loadings (Top 5 per factor)",
            "",
        ]
    )

    for i in range(n_factors):
        col = f"Factor_{i+1}"
        lines.append(f"**{col}:**")
        top_items = loadings[col].abs().sort_values(ascending=False).head(5)
        for item in top_items.index:
            loading = loadings.loc[item, col]
            lines.append(f"- {item}: {loading:.3f}")
        lines.append("")

    # Full loadings table
    header = (
        "| Variable | " + " | ".join(f"Factor {i+1}" for i in range(n_factors)) + " |"
    )
    separator = "|" + "|".join("----------" for _ in range(n_factors + 1)) + "|"
    lines.extend(
        [
            "### Full Factor Loadings Table",
            "",
            header,
            separator,
        ]
    )

    for var in loadings.index:
        row_vals = [
            f"{loadings.loc[var, f'Factor_{i+1}']:.3f}" for i in range(n_factors)
        ]
        lines.append(f"| {var} | {' | '.join(row_vals)} |")

    # Use f1_stats if provided
    if f1_stats is None:
        f1_stats = {}

    lines.extend(
        [
            "",
            "### Factor 1 Distribution",
            "",
            f"![Factor 1 Distribution]({fig_rel_path}/pre_attitudes_factor1_distribution.png)",
            "",
        ]
    )

    if f1_stats:
        lines.extend([
            f"- **Mean**: {f1_stats.get('mean', 0):.2f}",
            f"- **SD**: {f1_stats.get('std', 0):.2f}",
            f"- **Skewness**: {f1_stats.get('skewness', 0):.3f}",
            f"- **Kurtosis**: {f1_stats.get('kurtosis', 0):.3f}",
            "",
        ])

    # Comparison section
    lines.extend(
        [
            "---",
            "",
            "## Cluster vs Factor Comparison",
            "",
            "Factor 1 scores by cluster:",
            "",
            f"- **Anthro Enthusiast**: M = {comparison_stats.get('enthusiast_mean', 0):.2f}, "
            f"SD = {comparison_stats.get('enthusiast_sd', 0):.2f}",
            f"- **Anthro Skeptic**: M = {comparison_stats.get('skeptic_mean', 0):.2f}, "
            f"SD = {comparison_stats.get('skeptic_sd', 0):.2f}",
            f"- **T-test**: t = {comparison_stats['t_stat']:.2f}, p = {comparison_stats['p_value']:.2e}",
            "",
            "---",
            "",
            "## Output Data",
            "",
            "- `generated/pre_treatment_preference_types.jsonl` - Participant groupings for downstream analysis saved by ppt_id and study_id",
        ]
    )

    # Write report
    with open(report_path, "w") as f:
        f.write("\n".join(lines))

    print(f"\nSaved report: {report_path}")


def export_participant_data(comparison_df):
    """Export participant groupings for downstream analysis."""
    print("\n--- Exporting Data ---")

    # Prepare export dataframe
    export_df = comparison_df[
        ["ppt_id", "study_id", "cluster_name", "Factor_1_Score"]
    ].copy()
    export_df.columns = export_df.columns.str.lower()

    # Verify one row per (ppt_id, study_id) pair
    assert (
        export_df.duplicated(subset=["ppt_id", "study_id"]).sum() == 0
    ), "Expected one row per (ppt_id, study_id) in export"

    save_data(
        export_df,
        "pre_treatment_preference_types",
        GENERATED_DIR,
        fmt="jsonl",
        relative_to=SCRIPT_DIR,
    )


# =============================================================================
# MAIN
# =============================================================================


def run_analysis(generate_tex_tables=False):
    """Run the full analysis pipeline (without report)."""
    print("=" * 60)
    print("Pre-Treatment Attitudes Analysis")
    print("=" * 60)

    setup_plotting()

    # Load and prepare data
    attitudes_df, prefs_df, companionship_df = load_data()
    attitudes_df_long = prepare_attitudes(attitudes_df)
    prefs_df_long = prepare_preferences(prefs_df)
    companionship_df_long = prepare_companionship(companionship_df)
    wide_df, item_source_map = combine_and_pivot(
        attitudes_df_long, prefs_df_long, companionship_df_long
    )

    # Impute and standardize
    id_cols = ["ppt_id", "study_id"]
    wide_df_imputed = impute_with_mean(wide_df, exclude_cols=id_cols)
    wide_df_imputed.set_index(id_cols, inplace=True)

    feature_cols = list(wide_df_imputed.columns)
    data_scaled, scaler = standardize_data(wide_df_imputed)

    # Run analyses
    corr_matrix = run_correlation_analysis(wide_df_imputed, item_source_map)

    cluster_df, kmeans, k_results, anova_results, optimal_k = run_cluster_analysis(
        wide_df_imputed, data_scaled, feature_cols
    )

    fa, loadings, factor_scores, variance, eigenvalues, n_factors, f1_stats = (
        run_factor_analysis_pipeline(data_scaled, feature_cols, wide_df_imputed)
    )

    # Save factor loadings as LaTeX
    if generate_tex_tables:
        save_factor_loadings_latex(
            loadings, n_factors, TABLE_DIR / "pre_attitudes_factor_loadings.tex"
        )

    comparison_df, comparison_stats = compare_cluster_factor(cluster_df, factor_scores)

    # Save stats (for report generation)
    stats_dict = {
        "n_participants": len(cluster_df),
        "n_items": len(feature_cols),
        "optimal_k": optimal_k,
        "n_factors": n_factors,
        "cluster_sizes": cluster_df["cluster_name"].value_counts().to_dict(),
        "variance_explained": list(variance[1]),
        "comparison_stats": comparison_stats,
        "k_results": k_results,
        "anova_results": anova_results.to_dict(),
        "loadings": loadings.to_dict(),
        "item_source_map": item_source_map,
        "f1_stats": f1_stats,
    }
    save_stats_json(stats_dict, "pre_attitudes_factor_clustering_stats")

    # Export participant data
    export_participant_data(comparison_df)

    print("\n" + "=" * 60)
    print("Analysis complete!")
    print("=" * 60)

    return stats_dict


def run_report(stats_dict):
    """Generate markdown report from saved or provided stats."""
    print("\n--- Generating Report ---")

    # Convert back to proper types
    k_results = stats_dict.get("k_results", {})
    anova_results = pd.DataFrame(stats_dict.get("anova_results", {}))
    loadings = pd.DataFrame(stats_dict.get("loadings", {}))
    variance = (None, stats_dict.get("variance_explained", []))
    comparison_stats = stats_dict.get("comparison_stats", {})
    item_source_map = stats_dict.get("item_source_map", {})
    optimal_k = stats_dict.get("optimal_k", None)
    n_factors = stats_dict.get("n_factors", None)
    f1_stats = stats_dict.get("f1_stats", {})

    # Create minimal cluster_df for report
    cluster_sizes = stats_dict.get("cluster_sizes", {})
    cluster_df = pd.DataFrame(
        {"cluster_name": list(cluster_sizes.keys()) * 100}  # Dummy for value_counts
    )
    # Override with actual counts
    cluster_df = pd.DataFrame({"cluster_name": []})
    for name, count in cluster_sizes.items():
        cluster_df = pd.concat(
            [cluster_df, pd.DataFrame({"cluster_name": [name] * count})]
        )

    generate_markdown_report(
        cluster_df,
        k_results,
        anova_results,
        loadings,
        variance,
        comparison_stats,
        item_source_map,
        optimal_k,
        n_factors,
        f1_stats,
    )


def main():
    """Main entry point with CLI argument parsing."""
    parser = argparse.ArgumentParser(
        description="Pre-Treatment Attitudes Analysis",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python pre_treatment_dim_reduction.py              # Run analysis only
  python pre_treatment_dim_reduction.py --generate_report     # Run analysis + report
  python pre_treatment_dim_reduction.py --generate_tex_tables # Run analysis + LaTeX tables
        """,
    )
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

    args = parser.parse_args()

    stats_dict = run_analysis(generate_tex_tables=args.generate_tex_tables)
    if args.generate_report:
        run_report(stats_dict)


if __name__ == "__main__":
    main()
