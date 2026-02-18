#!/usr/bin/env python3
"""
Statistical utility functions for human studies analysis.

Includes functions for:
- Clustering (K-means optimization, labeling)
- Factor analysis
- Statistical testing (ANOVA with FDR correction)
"""

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.cluster import KMeans
from sklearn.metrics import calinski_harabasz_score, silhouette_score
from sklearn.preprocessing import StandardScaler
from statsmodels.stats.multitest import multipletests


# =============================================================================
# DATA PREPARATION
# =============================================================================


def standardize_data(df, exclude_cols=None):
    """
    Standardize numeric columns in a dataframe.

    Args:
        df: DataFrame with numeric columns
        exclude_cols: List of column names to exclude from standardization

    Returns:
        Tuple of (scaled_array, scaler, feature_columns)
    """
    if exclude_cols is None:
        exclude_cols = []

    feature_cols = [c for c in df.columns if c not in exclude_cols]
    scaler = StandardScaler()
    scaled_data = scaler.fit_transform(df[feature_cols])

    return scaled_data, scaler


def impute_with_mean(df, exclude_cols=None):
    """
    Impute missing values with column means.

    Args:
        df: DataFrame with possible missing values
        exclude_cols: List of column names to exclude from imputation

    Returns:
        DataFrame with imputed values
    """
    if exclude_cols is None:
        exclude_cols = []

    df_out = df.copy()
    item_cols = [c for c in df_out.columns if c not in exclude_cols]

    for col in item_cols:
        df_out[col] = df_out[col].fillna(df_out[col].mean())

    return df_out


# =============================================================================
# CLUSTERING
# =============================================================================


def find_optimal_k(data_scaled, k_range=range(2, 8), random_state=42):
    """
    Evaluate clustering metrics for different values of k.

    Args:
        data_scaled: Standardized data array
        k_range: Range of k values to test
        random_state: Random seed for reproducibility

    Returns:
        Dict with k values and metrics (inertia, silhouette, calinski_harabasz)
    """
    results = {"k": [], "inertia": [], "silhouette": [], "calinski_harabasz": []}

    for k in k_range:
        kmeans = KMeans(n_clusters=k, random_state=random_state, n_init=10)
        labels = kmeans.fit_predict(data_scaled)

        results["k"].append(k)
        results["inertia"].append(kmeans.inertia_)
        results["silhouette"].append(silhouette_score(data_scaled, labels))
        results["calinski_harabasz"].append(
            calinski_harabasz_score(data_scaled, labels)
        )

    return results


def run_kmeans(data_scaled, n_clusters, random_state=42):
    """
    Run K-means clustering.

    Args:
        data_scaled: Standardized data array
        n_clusters: Number of clusters
        random_state: Random seed

    Returns:
        Tuple of (kmeans_model, cluster_labels)
    """
    kmeans = KMeans(n_clusters=n_clusters, random_state=random_state, n_init=10)
    labels = kmeans.fit_predict(data_scaled)
    return kmeans, labels


def label_clusters_by_mean(df, cluster_col, value_cols, cluster_names=None):
    """
    Relabel clusters so that lower index = lower mean score.

    Args:
        df: DataFrame with cluster labels and value columns
        cluster_col: Name of cluster column
        value_cols: List of columns to compute mean over
        cluster_names: Optional dict mapping cluster index to name

    Returns:
        Tuple of (mapping_dict, labeled_df)
    """
    # Compute overall mean per cluster
    cluster_means = df.groupby(cluster_col)[value_cols].mean().mean(axis=1)
    sorted_clusters = cluster_means.sort_values().index.tolist()

    # Create mapping: old -> new (sorted by mean)
    mapping = {old: new for new, old in enumerate(sorted_clusters)}

    df_out = df.copy()
    df_out[cluster_col] = df_out[cluster_col].map(mapping)

    if cluster_names:
        df_out["cluster_name"] = df_out[cluster_col].map(cluster_names)

    return mapping, df_out


def compute_cluster_stats(df, cluster_col, value_cols):
    """
    Compute statistics for each cluster.

    Args:
        df: DataFrame with cluster labels
        cluster_col: Name of cluster column
        value_cols: Columns to compute stats for

    Returns:
        Dict with cluster statistics
    """
    stats_dict = {
        "cluster_sizes": df[cluster_col].value_counts().to_dict(),
        "cluster_means": df.groupby(cluster_col)[value_cols].mean().to_dict(),
        "overall_means": df.groupby(cluster_col)[value_cols]
        .mean()
        .mean(axis=1)
        .to_dict(),
    }
    return stats_dict


# =============================================================================
# STATISTICAL TESTING
# =============================================================================


def run_anova_with_fdr(df, group_col, value_cols, alpha=0.05):
    """
    Run one-way ANOVA for each variable with FDR correction.

    Args:
        df: DataFrame with group labels and values
        group_col: Column name for grouping variable
        value_cols: List of columns to test
        alpha: Significance level

    Returns:
        DataFrame with ANOVA results including FDR-corrected p-values
    """
    results = []

    for var in value_cols:
        groups = [df[df[group_col] == g][var].dropna() for g in df[group_col].unique()]
        f_stat, p_val = stats.f_oneway(*groups)
        results.append({"variable": var, "f_stat": f_stat, "p_value": p_val})

    results_df = pd.DataFrame(results)

    # Apply FDR correction (Benjamini-Hochberg)
    _, p_fdr, _, _ = multipletests(results_df["p_value"], method="fdr_bh")
    results_df["p_fdr"] = p_fdr
    results_df["significant"] = results_df["p_fdr"] < alpha

    return results_df.sort_values("f_stat", ascending=False)


def compute_confidence_interval(values, confidence=0.95):
    """
    Compute confidence interval for mean.

    Args:
        values: Array-like of values
        confidence: Confidence level

    Returns:
        Tuple of (mean, ci_lower, ci_upper)
    """
    n = len(values)
    mean = values.mean()
    se = values.std() / np.sqrt(n)
    ci = stats.t.interval(confidence, n - 1, loc=mean, scale=se)
    return mean, ci[0], ci[1]


# =============================================================================
# FACTOR ANALYSIS
# =============================================================================


def get_factor_eigenvalues(data_scaled):
    """
    Compute eigenvalues for factor analysis scree plot.

    Args:
        data_scaled: Standardized data array

    Returns:
        Array of eigenvalues
    """
    from factor_analyzer import FactorAnalyzer

    fa = FactorAnalyzer(rotation=None)
    fa.fit(data_scaled)
    eigenvalues = fa.get_eigenvalues()[0]
    return eigenvalues


def run_factor_analysis(data_scaled, n_factors, rotation="varimax"):
    """
    Run factor analysis.

    Args:
        data_scaled: Standardized data array
        n_factors: Number of factors to extract
        rotation: Rotation method

    Returns:
        Tuple of (factor_analyzer, loadings_df, variance_explained)
    """
    from factor_analyzer import FactorAnalyzer

    fa = FactorAnalyzer(n_factors=n_factors, rotation=rotation)
    fa.fit(data_scaled)

    variance = fa.get_factor_variance()

    return fa, variance


def get_factor_loadings(fa, feature_names, n_factors):
    """
    Get factor loadings as a DataFrame.

    Args:
        fa: Fitted FactorAnalyzer
        feature_names: List of feature names
        n_factors: Number of factors

    Returns:
        DataFrame with factor loadings
    """
    loadings = pd.DataFrame(
        fa.loadings_,
        index=feature_names,
        columns=[f"Factor_{i+1}" for i in range(n_factors)],
    )
    return loadings


def get_factor_scores(fa, data_scaled, index, n_factors):
    """
    Compute factor scores for each observation.

    Args:
        fa: Fitted FactorAnalyzer
        data_scaled: Standardized data array
        index: Index for output DataFrame
        n_factors: Number of factors

    Returns:
        DataFrame with factor scores
    """
    scores = fa.transform(data_scaled)
    scores_df = pd.DataFrame(
        scores,
        index=index,
        columns=[f"Factor_{i+1}_Score" for i in range(n_factors)],
    )
    return scores_df
