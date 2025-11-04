adversarial_checklist <- function(out_dir, flags) {
  lines <- c(
    "# Adversarial Review Checklist",
    "- Randomization / quasi-experiment: documented? (Y/N)",
    "- Baseline balance (pre PSS, covariates) assessed? (Y/N)",
    "- Missingness mechanism explored (MCAR/MAR/MNAR) + sensitivity? (Y/N)",
    "- Adherence/dropout bias quantified? (Y/N)",
    "- Robustness: permutation test, bootstrap CI, Wilcoxon? (Y/N)",
    "- Practical significance vs MCID/SESOI reported? (Y/N)",
    "- Multiple comparisons guarded (if subgroups)? (Y/N)",
    "- Confounds (seasonality, policy changes) discussed? (Y/N)",
    sprintf("- DEI segmentation checked (see %s)? (Y/N)", "dei_report.csv"),
    "- Reproducibility: config snapshot, session info, seeds saved? (Y/N)"
  )
  writeLines(lines, file.path(out_dir, "adversarial_checklist.md"))
}
