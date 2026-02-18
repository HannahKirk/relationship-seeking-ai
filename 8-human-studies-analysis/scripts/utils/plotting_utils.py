#!/usr/bin/env python3
"""
Plotting utility functions for human studies analysis.

Includes functions for:
- Correlation heatmaps/clustermaps
- Clustering visualizations (PCA, elbow plots, cluster profiles)
- Factor analysis plots (scree, loadings)
- Geographic choropleth maps
"""

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import numpy as np
import pandas as pd
import seaborn as sns
from scipy import stats
from sklearn.decomposition import PCA
from matplotlib.patches import Rectangle
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


# =============================================================================
# PLOT CONFIGURATION
# =============================================================================

# Default color palettes
CLUSTER_COLORS = {
    0: "#d95f02",  # Orange
    1: "#7570b3",  # Purple
}

SOURCE_COLORS = {
    "Ideal AI Preferences Survey": "#e41a1c",
    "Societal Attitudes to Anthropomorphism Survey": "#377eb8",
    "Seeking Companionship Survey": "#4daf4a",
}


def apply_theme(ax, base_size=14):
    """Apply consistent styling to matplotlib axes."""
    ax.set_title(ax.get_title(), fontsize=base_size * 1.2, fontweight="bold")
    ax.set_xlabel(ax.get_xlabel(), fontsize=base_size, fontweight="bold")
    ax.set_ylabel(ax.get_ylabel(), fontsize=base_size, fontweight="bold")
    ax.tick_params(axis="both", labelsize=base_size * 0.9)
    ax.grid(True, alpha=0.3)
    ax.set_facecolor("white")
    for spine in ax.spines.values():
        spine.set_linewidth(0.8)


def setup_plotting():
    """Set up default plotting configuration."""
    plt.rcParams.update(
        {
            "figure.facecolor": "white",
            "axes.facecolor": "white",
            "font.family": "sans-serif",
        }
    )


# =============================================================================
# SAVE PLOT UTILITY
# =============================================================================


def save_plot(
    figure_dir,
    fig,
    filename,
    width=None,
    height=None,
    dpi=300,
    save_pdf=True,
    save_png=True,
):
    """
    Save a matplotlib figure to both PDF and PNG formats.

    Args:
        figure_dir: Base figure directory (will create pdf/ and png/ subdirs)
        fig: matplotlib Figure object
        filename: Filename without extension (e.g., "my_plot")
        width: Figure width in inches (optional, uses existing if None)
        height: Figure height in inches (optional, uses existing if None)
        dpi: Resolution for PNG (default 300)
        save_pdf: Whether to save PDF (default True)
        save_png: Whether to save PNG (default True)
    """
    from pathlib import Path

    figure_dir = Path(figure_dir)

    # Remove extension if provided
    base_name = filename.replace(".pdf", "").replace(".png", "")

    # Resize figure if dimensions provided
    if width is not None and height is not None:
        fig.set_size_inches(width, height)

    if save_pdf:
        pdf_dir = figure_dir / "pdf"
        pdf_dir.mkdir(parents=True, exist_ok=True)
        pdf_path = pdf_dir / f"{base_name}.pdf"
        fig.savefig(pdf_path, bbox_inches="tight", dpi=dpi)
        print(f"Saved: {pdf_path}")

    if save_png:
        png_dir = figure_dir / "png"
        png_dir.mkdir(parents=True, exist_ok=True)
        png_path = png_dir / f"{base_name}.png"
        fig.savefig(png_path, bbox_inches="tight", dpi=dpi)
        print(f"Saved: {png_path}")


# =============================================================================
# CORRELATION PLOTS
# =============================================================================


def plot_correlation_clustermap(
    corr_matrix,
    item_source_map=None,
    source_palette=None,
    figsize=(14, 14),
    save_path=None,
    annot_fontsize=8,
    tick_fontsize=12,
):
    """
    Plot clustered correlation heatmap with optional source coloring.

    Args:
        corr_matrix: Correlation matrix (DataFrame)
        item_source_map: Dict mapping item names to source labels
        source_palette: Dict mapping source labels to colors
        figsize: Figure size tuple
        save_path: Optional path to save figure
        annot_fontsize: Font size for cell annotations
        tick_fontsize: Font size for axis tick labels

    Returns:
        ClusterGrid object
    """
    # Prepare row/col colors if source mapping provided
    row_colors = None
    col_colors = None

    if item_source_map is not None and source_palette is not None:
        item_colors = pd.DataFrame(
            {
                "Source": pd.Series(corr_matrix.columns)
                .map(item_source_map)
                .map(source_palette)
                .values
            },
            index=corr_matrix.columns,
        )
        row_colors = item_colors
        col_colors = item_colors

    g = sns.clustermap(
        corr_matrix,
        annot=True,
        cmap="RdBu_r",
        center=0,
        fmt=".2f",
        figsize=figsize,
        dendrogram_ratio=0.15,
        linewidths=0.1,
        method="ward",
        metric="euclidean",
        annot_kws={"size": annot_fontsize},
        cbar_pos=None,
        row_colors=row_colors,
        col_colors=col_colors,
    )

    g.ax_heatmap.set_xlabel("")
    g.ax_heatmap.set_ylabel("")

    # Set tick label font sizes
    g.ax_heatmap.tick_params(axis='both', labelsize=tick_fontsize)
    g.ax_heatmap.set_xticklabels(g.ax_heatmap.get_xticklabels(), fontsize=tick_fontsize)
    g.ax_heatmap.set_yticklabels(g.ax_heatmap.get_yticklabels(), fontsize=tick_fontsize)

    # Add legend if sources provided
    if item_source_map is not None and source_palette is not None:
        for source, color in source_palette.items():
            g.ax_col_dendrogram.bar(0, 0, color=color, label=source, linewidth=0)
        g.ax_col_dendrogram.legend(
            loc="upper left", ncol=1, fontsize=10, bbox_to_anchor=(1, 1)
        )

    if save_path:
        g.savefig(save_path, bbox_inches="tight")

    return g


# =============================================================================
# CLUSTERING PLOTS
# =============================================================================


def plot_kmeans_diagnostics(k_results, save_path=None):
    """
    Plot K-means diagnostic plots (elbow, silhouette, CH index).

    Args:
        k_results: Dict from find_optimal_k()
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))

    # Elbow plot
    axes[0].plot(k_results["k"], k_results["inertia"], "bo-", linewidth=2, markersize=8)
    axes[0].set_title("Elbow Method")
    axes[0].set_xlabel("Number of Clusters")
    axes[0].set_ylabel("Inertia")

    # Silhouette plot
    axes[1].plot(
        k_results["k"], k_results["silhouette"], "ro-", linewidth=2, markersize=8
    )
    axes[1].set_title("Silhouette Score")
    axes[1].set_xlabel("Number of Clusters")
    axes[1].set_ylabel("Score")

    # Calinski-Harabasz plot
    axes[2].plot(
        k_results["k"], k_results["calinski_harabasz"], "go-", linewidth=2, markersize=8
    )
    axes[2].set_title("Calinski-Harabasz Index")
    axes[2].set_xlabel("Number of Clusters")
    axes[2].set_ylabel("Index")

    for ax in axes:
        apply_theme(ax)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig


def plot_cluster_pca(
    data_scaled,
    cluster_labels,
    kmeans_model,
    cluster_names,
    cluster_colors=None,
    save_path=None,
):
    """
    Plot clusters in PCA space with summary statistics.

    Args:
        data_scaled: Standardized data array
        cluster_labels: Cluster assignments
        kmeans_model: Fitted KMeans model
        cluster_names: Dict mapping cluster index to name
        cluster_colors: Dict mapping cluster index to color
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    if cluster_colors is None:
        cluster_colors = CLUSTER_COLORS

    n_clusters = len(cluster_names)

    # PCA transformation
    pca = PCA(n_components=2)
    data_pca = pca.fit_transform(data_scaled)

    # Prepare ordered lists
    ordered_clusters = list(range(n_clusters))
    ordered_names = [cluster_names[i] for i in ordered_clusters]
    ordered_sizes = [np.sum(cluster_labels == i) for i in ordered_clusters]
    ordered_colors = [cluster_colors[i] for i in ordered_clusters]

    fig = plt.figure(figsize=(14, 6))

    # Plot 1: PCA scatter
    ax1 = plt.subplot2grid((2, 2), (0, 0), rowspan=2, colspan=1)

    for cluster_id in range(n_clusters):
        mask = cluster_labels == cluster_id
        ax1.scatter(
            data_pca[mask, 0],
            data_pca[mask, 1],
            c=cluster_colors[cluster_id],
            alpha=0.6,
            s=40,
            label=cluster_names[cluster_id],
        )

    # Add cluster centers
    centers_pca = pca.transform(kmeans_model.cluster_centers_)
    ax1.scatter(
        centers_pca[:, 0],
        centers_pca[:, 1],
        c="black",
        marker="x",
        s=200,
        linewidths=3,
        label="Centers",
    )

    ax1.set_xlabel(f"PC1 ({pca.explained_variance_ratio_[0]:.1%} variance)")
    ax1.set_ylabel(f"PC2 ({pca.explained_variance_ratio_[1]:.1%} variance)")
    ax1.set_title("K-means Clusters in PCA Space")
    ax1.legend()
    apply_theme(ax1)

    # Plot 2: Cluster sizes
    ax2 = plt.subplot2grid((2, 2), (0, 1))
    bars = ax2.bar(
        range(len(ordered_clusters)), ordered_sizes, color=ordered_colors, alpha=0.7
    )
    ax2.set_ylabel("N Participants")
    ax2.set_title("Cluster Sizes")
    ax2.set_xticks(range(len(ordered_clusters)))
    ax2.set_xticklabels(ordered_names)
    apply_theme(ax2)

    # Plot 3: Within-cluster sum of squares
    ax3 = plt.subplot2grid((2, 2), (1, 1))
    wcss = []
    for i in ordered_clusters:
        mask = cluster_labels == i
        points = data_scaled[mask]
        center = points.mean(axis=0)
        wcss.append(np.sum((points - center) ** 2))

    bars = ax3.bar(range(len(ordered_clusters)), wcss, color=ordered_colors, alpha=0.7)
    ax3.set_ylabel("Within-Cluster SS")
    ax3.set_title("Cluster Cohesion")
    ax3.set_xticks(range(len(ordered_clusters)))
    ax3.set_xticklabels(ordered_names)
    apply_theme(ax3)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig


def plot_cluster_means_by_category(
    df,
    cluster_col,
    categories,
    cluster_names,
    cluster_colors=None,
    save_path=None,
):
    """
    Plot cluster means for each category of variables.

    Args:
        df: DataFrame with cluster labels and values
        cluster_col: Name of cluster column
        categories: Dict mapping category names to list of column names
        cluster_names: Dict mapping cluster index to name
        cluster_colors: Dict mapping cluster index to color
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    if cluster_colors is None:
        cluster_colors = CLUSTER_COLORS

    n_categories = len(categories)
    n_cols = 3
    n_rows = (n_categories + n_cols - 1) // n_cols

    ordered_clusters = sorted(cluster_names.keys())
    ordered_names = [cluster_names[i] for i in ordered_clusters]
    ordered_colors = [cluster_colors[i] for i in ordered_clusters]

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(15, 4 * n_rows))
    axes = axes.flatten()

    for idx, (category, variables) in enumerate(categories.items()):
        available_vars = [v for v in variables if v in df.columns]
        if not available_vars:
            continue

        means, cis = [], []
        for cluster_id in ordered_clusters:
            subset = df[df[cluster_col] == cluster_id]

            if len(available_vars) == 1:
                values = subset[available_vars[0]]
            else:
                values = subset[available_vars].mean(axis=1)

            mean = values.mean()
            se = values.std() / np.sqrt(len(values))
            ci = stats.t.interval(0.95, len(values) - 1, loc=mean, scale=se)

            means.append(mean)
            cis.append(ci)

        # Plot
        x = range(len(ordered_clusters))
        bars = axes[idx].bar(x, means, color=ordered_colors, alpha=0.7)

        yerr_lower = [means[i] - cis[i][0] for i in range(len(means))]
        yerr_upper = [cis[i][1] - means[i] for i in range(len(means))]
        axes[idx].errorbar(
            x,
            means,
            yerr=[yerr_lower, yerr_upper],
            fmt="none",
            color="black",
            capsize=5,
        )

        axes[idx].set_title(
            f"{category}\n({len(available_vars)} item{'s' if len(available_vars) > 1 else ''})"
        )
        axes[idx].set_ylabel("Mean Score")
        axes[idx].set_xticks(x)
        axes[idx].set_xticklabels(ordered_names, fontsize=10)

        for i, v in enumerate(means):
            axes[idx].text(
                i, v * 0.7, f"{v:.1f}", ha="center", fontsize=12, fontweight="bold"
            )

        apply_theme(axes[idx], base_size=11)

    # Hide unused axes
    for idx in range(len(categories), len(axes)):
        axes[idx].set_visible(False)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig


# =============================================================================
# FACTOR ANALYSIS PLOTS
# =============================================================================


def plot_scree(eigenvalues, save_path=None):
    """
    Plot scree plot for factor analysis.

    Args:
        eigenvalues: Array of eigenvalues
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    fig, ax = plt.subplots(figsize=(8, 4))

    ax.plot(
        range(1, len(eigenvalues) + 1),
        eigenvalues,
        "o-",
        color="#2166ac",
        linewidth=2,
        markersize=8,
    )
    ax.axhline(
        y=1, color="#d73027", linestyle="--", linewidth=2, label="Kaiser criterion"
    )
    ax.set_xlabel("Factor Number")
    ax.set_ylabel("Eigenvalue")
    ax.set_title("Scree Plot")
    ax.legend()
    apply_theme(ax)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig


def plot_factor_distribution(factor_scores, factor_name, save_path=None):
    """
    Plot distribution diagnostics for a factor score.

    Args:
        factor_scores: Series or array of factor scores
        factor_name: Name for labeling
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    from scipy.stats import jarque_bera

    fig, axes = plt.subplots(2, 2, figsize=(12, 10))

    # Histogram
    axes[0, 0].hist(factor_scores, bins=30, alpha=0.7, density=True, color="steelblue")
    axes[0, 0].axvline(factor_scores.mean(), color="red", linestyle="--", label="Mean")
    axes[0, 0].axvline(
        np.median(factor_scores), color="orange", linestyle="--", label="Median"
    )
    axes[0, 0].set_title(f"{factor_name} Distribution")
    axes[0, 0].set_xlabel("Score")
    axes[0, 0].legend()

    # Q-Q plot
    stats.probplot(factor_scores, dist="norm", plot=axes[0, 1])
    axes[0, 1].set_title("Q-Q Plot")

    # Box plot
    axes[1, 0].boxplot(factor_scores, vert=True)
    axes[1, 0].set_ylabel("Score")
    axes[1, 0].set_title("Box Plot")

    # Kernel density
    pd.Series(factor_scores).plot.density(ax=axes[1, 1], color="darkblue", linewidth=2)
    axes[1, 1].set_title("Kernel Density")
    axes[1, 1].set_xlabel("Score")

    for ax in axes.flatten():
        apply_theme(ax)

    plt.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig


# =============================================================================
# GEOGRAPHIC PLOTS
# =============================================================================

# Study colors for geographic plots
STUDY_COLORS = {
    "calibration": "#fbbd05",
    "cross-sectional": "#32bcdd",
    "longitudinal": "#ff8a67",
}

# Study display names
STUDY_TITLES = {
    "calibration": "Calibration",
    "cross-sectional": "Cross-Sectional",
    "longitudinal": "Longitudinal",
}


def create_colormap_from_color(base_color, name):
    """
    Create a colormap from a base color (light -> base -> dark).

    Args:
        base_color: Hex color string
        name: Name for the colormap

    Returns:
        LinearSegmentedColormap
    """
    base_rgb = mcolors.hex2color(base_color)
    light_rgb = tuple(min(1.0, c + 0.6) for c in base_rgb)
    dark_rgb = tuple(max(0.0, c * 0.7) for c in base_rgb)
    colors = [mcolors.rgb2hex(light_rgb), base_color, mcolors.rgb2hex(dark_rgb)]
    return mcolors.LinearSegmentedColormap.from_list(name, colors, N=100)


def load_uk_geometries(countries_file, regions_file, crs="EPSG:27700"):
    """
    Load and prepare UK country and region geometries.

    Args:
        countries_file: Path to countries GeoJSON
        regions_file: Path to regions GeoJSON
        crs: Coordinate reference system (default: British National Grid)

    Returns:
        tuple: (combined GeoDataFrame, xlim, ylim)
    """
    import geopandas as gpd

    countries_gdf = gpd.read_file(countries_file)
    regions_gdf = gpd.read_file(regions_file)

    non_england = countries_gdf[
        countries_gdf["CTRY21NM"].isin(["Scotland", "Wales", "Northern Ireland"])
    ].copy()
    english = regions_gdf.copy()

    non_england = non_england.to_crs(crs)
    english = english.to_crs(crs)

    non_england["region_name"] = non_england["CTRY21NM"]
    english["region_name"] = english["RGN21NM"]

    cols = ["region_name", "geometry"]
    base_gdf = pd.concat([non_england[cols], english[cols]], ignore_index=True)

    xmin, ymin, xmax, ymax = base_gdf.total_bounds
    return base_gdf, (xmin, xmax), (ymin, ymax)


def plot_choropleth_panel(
    df,
    ax,
    base_gdf,
    xlim,
    ylim,
    study_type,
    colormap,
    geo_col="geoglocation",
    show_labels=True,
):
    """
    Plot a single choropleth panel for one study.

    Args:
        df: DataFrame with geographic data
        ax: Matplotlib axis
        base_gdf: Base GeoDataFrame with region geometries
        xlim: X-axis limits tuple
        ylim: Y-axis limits tuple
        study_type: Study type name for title
        colormap: Colormap for the choropleth
        geo_col: Column name for geographic location
        show_labels: Whether to show region labels
    """
    # Prep counts
    region_counts = df[geo_col].value_counts().reset_index()
    region_counts.columns = [geo_col, "count"]
    region_counts[geo_col] = region_counts[geo_col].replace(
        "Yorkshire and the Humber", "Yorkshire and The Humber"
    )
    region_counts = region_counts[
        ~region_counts[geo_col].isin(["Prefer not to say", "__other"])
    ]

    # Merge with geometries
    final_gdf = base_gdf.merge(
        region_counts, left_on="region_name", right_on=geo_col, how="left"
    )

    # Plot
    final_gdf.plot(
        column="count",
        ax=ax,
        legend=False,
        cmap=colormap,
        missing_kwds={"color": "lightgray", "edgecolor": "black"},
        edgecolor="grey",
        linewidth=0.8,
    )

    # Lock extent + aspect
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_aspect("equal", adjustable="datalim")
    ax.margins(0)
    ax.axis("off")

    # Colorbar
    mappable = ax.collections[0]
    cb = inset_axes(
        ax,
        width="2.5%",
        height="38%",
        bbox_to_anchor=(0.75, 0.5, 1, 1),
        bbox_transform=ax.transAxes,
        loc="lower left",
    )
    cbar = plt.colorbar(mappable, cax=cb, orientation="vertical")
    cbar.ax.tick_params(labelsize=10)
    cbar.set_label("Count", fontsize=10, rotation=90, labelpad=6)

    # Title strip
    total_n = int(df.shape[0])
    title = STUDY_TITLES.get(study_type, study_type.title())
    title_text = f"{title} (N = {total_n:,})"
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    rect_h = (y1 - y0) * 0.06
    rect_w = (x1 - x0) * 0.80
    rx = x0 + (x1 - x0) * 0.10
    ry = y1 - rect_h
    strip = Rectangle(
        (rx, ry),
        rect_w,
        rect_h,
        facecolor="#f0f0f0",
        edgecolor="#f0f0f0",
        zorder=10,
        clip_on=False,
    )
    ax.add_patch(strip)
    ax.text(
        rx + rect_w / 2,
        ry + rect_h / 2,
        title_text,
        ha="center",
        va="center",
        fontsize=16,
        fontweight="bold",
        zorder=11,
    )

    # Region labels
    if show_labels:
        span_x = xlim[1] - xlim[0]
        span_y = ylim[1] - ylim[0]
        dx = 0.02 * span_x
        dy = 0.02 * span_y

        # Offset adjustments for specific regions
        region_offsets = {
            "Yorkshire and The Humber": (1.5 * dx, -dy, "Yorkshire and\nThe Humber"),
            "East Midlands": (0, 1.2 * dy, None),
            "North East": (0, 1.25 * dy, None),
            "North West": (0, 3 * dy, None),
            "South East": (0, -1 * dy, None),
            "Wales": (0, -1.7 * dy, None),
            "West Midlands": (0, 0.5 * dy, None),
            "London": (0, 0.75 * dy, None),
        }

        for _, row in final_gdf.iterrows():
            if pd.notna(row["count"]):
                centroid = row["geometry"].centroid
                name = row["region_name"]
                x, y = centroid.x, centroid.y

                if name in region_offsets:
                    x_off, y_off, display_name = region_offsets[name]
                    x += x_off
                    y += y_off
                    if display_name:
                        name = display_name

                pct = int((row["count"] / total_n) * 100) if total_n > 0 else 0
                ax.annotate(
                    f"{name}\n({pct}%)",
                    xy=(x, y),
                    ha="center",
                    va="center",
                    fontsize=10,
                    bbox=dict(boxstyle="round,pad=0.3", facecolor="white", alpha=0.8),
                    fontweight="bold",
                )


def plot_geographic_triangle(
    combined_df,
    base_gdf,
    xlim,
    ylim,
    study_col="study_id",
    geo_col="geoglocation",
    study_colors=None,
    save_path=None,
):
    """
    Create a triangle layout of three geographic choropleth maps.

    Args:
        combined_df: Combined DataFrame with all studies
        base_gdf: Base GeoDataFrame with region geometries
        xlim: X-axis limits tuple
        ylim: Y-axis limits tuple
        study_col: Column name for study identifier
        geo_col: Column name for geographic location
        study_colors: Dict mapping study names to colors
        save_path: Optional path to save figure

    Returns:
        Figure object
    """
    if study_colors is None:
        study_colors = STUDY_COLORS

    fig = plt.figure(figsize=(10, 20), facecolor="white")

    # Bottom row (touching, no whitespace)
    ax_cross_sectional = fig.add_axes([0.00, 0.08, 0.50, 0.40])
    ax_longitudinal = fig.add_axes([0.50, 0.08, 0.50, 0.40])

    # Top (centered)
    ax_calibration = fig.add_axes([0.25, 0.50, 0.50, 0.40])

    study_axes = {
        "calibration": ax_calibration,
        "cross-sectional": ax_cross_sectional,
        "longitudinal": ax_longitudinal,
    }

    ordered_ids = ["calibration", "cross-sectional", "longitudinal"]
    for sid in ordered_ids:
        if sid not in combined_df[study_col].unique():
            continue
        df = combined_df[combined_df[study_col] == sid]
        cmap = create_colormap_from_color(study_colors[sid], f"{sid}_cmap")
        plot_choropleth_panel(
            df, study_axes[sid], base_gdf, xlim, ylim, sid, cmap, geo_col
        )

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")

    return fig
