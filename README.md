# Mindfulness @ Work — R Analysis Package

A production-ready R workflow to evaluate pre/post **Perceived Stress Scale (PSS)** outcomes,
dose–response with mindfulness practice, adherence tiers, robustness checks, and publication-quality visuals.

## Contents
- `R/analysis.R` — Main end-to-end analysis script (run this).
- `R/install_packages.R` — Installs required CRAN packages.
- `config.yml` — Configuration for file paths, column names, reverse-coded items, etc.
- `data/` — Put your raw data here (e.g., `mindfulness_survey.xlsx` with `pre` and `post` sheets).
- `outputs/` — Tables and metrics will be written here (timestamped run folder).
- `figures/` — (Optional) You can redirect figures here if desired.
- `docs/` — Executive summary template.

## Quickstart
1. Install R (>= 4.1). Open R / RStudio in this folder.
2. Run the installer once:
   ```r
   source("R/install_packages.R")
   ```
3. Update **config.yml** with your column names and file paths.
4. Run the analysis:
   ```r
   source("R/analysis.R")
   ```
5. Check the `outputs/run_YYYYMMDD_HHMMSS/` folder for CSV tables and figures.

## Data expectations
- Two sheets in an Excel workbook (default names: `pre`, `post`) or adapt loader for CSV.
- PSS-10 items must be present for pre and post (see `config.yml` for names and reverse-coded items).

## Notes
- Primary estimands: ΔPSS (post − pre), effect sizes (Cohen's d_z), dose–response slope.
- Robustness: bootstrap CIs, Wilcoxon, trimmed means option, sensitivity for adherence.
- Optional: TOST equivalence using MCID or a specified SESOI.
