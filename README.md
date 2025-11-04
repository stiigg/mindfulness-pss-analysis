# Mindfulness @ Work — R Analysis Package

A production-ready R workflow to evaluate pre/post **Perceived Stress Scale (PSS)** outcomes,
dose–response with mindfulness practice, adherence tiers, robustness checks, and publication-quality visuals.

## Contents
- `R/analysis.R` — Main end-to-end analysis script (run this).
- `R/install_packages.R` — Installs required CRAN packages.
- `config.yml` — Configuration for file paths, column names, reverse-coded items, etc.
- `report_config.yml` — Thresholds for the executive RAG verdict.
- `reports/executive_summary.Rmd` — Executive-ready HTML summary template.
- `demo_run.R` — One-click synthetic demo (generates data, runs analysis, renders report).
- `data/` — Put your raw data here (e.g., `mindfulness_survey.xlsx` with `pre` and `post` sheets).
- `outputs/` — Tables, manifests, and figures saved per run (timestamped folder).

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
5. Check the `outputs/run_YYYYMMDD_HHMMSS/` folder for manifests, CSV tables, and PNG figures.
6. Render the executive summary:
   ```r
   rmarkdown::render("reports/executive_summary.Rmd")
   ```

## Quick demo
Generate synthetic data, execute the full pipeline, and render the report:
```bash
Rscript demo_run.R
open outputs/<latest>/executive_summary.html
```

## Data expectations
- Two sheets in an Excel workbook (default names: `pre`, `post`) or adapt loader for CSV.
- PSS-10 items must be present for pre and post (see `config.yml` for names and reverse-coded items).

## What you get in each run
- **Audit-grade manifest**: config snapshot, package inventory, warnings, seeds.
- **Robustness suite**: permutation test, bootstrap dz CI, Wilcoxon, MCID-based equivalence.
- **RAG executive verdict**: headline status card saved to `status_tile.png`.
- **DEI segmentation**: subgroup effect sizes with fairness gap flags.
- **Adversarial review checklist**: prompts to falsify or stress-test the findings.

## How to falsify our claims
Each run emits `adversarial_checklist.md`. Start there to challenge the analysis assumptions and surface missing diagnostics.
