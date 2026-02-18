# Human Studies Processing

This folder contains the data processing pipeline for converting raw human study data into analysis-ready datasets.

## Not Included in Public Release

This folder's contents (scripts and data) are **not included** in the public release to protect participant privacy. The intermediary data files contain identifiable information that could compromise anonymity.

## What This Folder Does

The processing pipeline:
1. Loads raw survey responses and chat logs from Gorilla data export
2. Cleans and validates participant responses e.g., maps free-text ``Other'' responses for ethnicity.
3. Maps Prolific IDs to anonymized participant IDs
4. Standardises format of conversation logs
5. Produces analysis-ready datasets split by task.

## Output

The analysis-ready datasets produced by this pipeline are available in `data/human_study/` and are used by `8-human-studies-analysis/`. This is the data to reproduce the results in the paper.
