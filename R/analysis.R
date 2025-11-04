###############################################
# Mindfulness @ Work: Impact Analysis (R)
# Author: <you>
# Last updated: Sys.Date()
###############################################

# -------- PACKAGES --------
need <- c(
  "tidyverse","readxl","janitor","lubridate","broom","broom.mixed","stringr",
  "skimr","naniar","effectsize","parameters","performance","see","patchwork",
  "mice","lme4","lmerTest","splines","ggeffects","marginaleffects","TOSTER",
  "ggdist","cowplot","scales","modelsummary","yaml","boot","gt","writexl"
)
inst <- setdiff(need, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# -------- HELPERS --------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

safe_source <- function(path) {
  if (file.exists(path)) source(path)
}

safe_source("R/logging.R")
safe_source("R/config_validate.R")
safe_source("R/robustness.R")
safe_source("R/dei_audit.R")
safe_source("R/adversarial.R")

# -------- CONFIG (YAML or defaults) --------
cfg_path <- "config.yml"
if (file.exists(cfg_path)) {
  cfg <- yaml::read_yaml(cfg_path)
} else {
  cfg <- list()
}

cfg <- modifyList(list(
  input_file = "data/mindfulness_survey.xlsx",
  sheet_pre  = "pre",
  sheet_post = "post",
  id_col     = "participant_id",
  time_col   = NULL,
  pss_items_pre  = c("pss1_pre","pss2_pre","pss3_pre","pss4_pre","pss5_pre","pss6_pre","pss7_pre","pss8_pre","pss9_pre","pss10_pre"),
  pss_items_post = c("pss1_post","pss2_post","pss3_post","pss4_post","pss5_post","pss6_post","pss7_post","pss8_post","pss9_post","pss10_post"),
  pss_reverse_idx = c(4,5,7,8),
  pss_min = 0,
  pss_max = 4,
  dose_minutes_day = "minutes_per_day",
  dose_days_week   = "days_per_week",
  covars = c("age","gender","role","prior_mindfulness"),
  group_col = NULL,
  mcid_sd = 0.5,
  outdir_root = "outputs",
  runtime = list(seed = 20251104),
  analysis = list(
    alpha = 0.05,
    perm_iters = 5000,
    do_tost = TRUE,
    mcid_frac_sd = 0.5
  ),
  dose = list(minutes_col = "minutes_total"),
  dei = list(groups = c("gender","age_band","department"), gap_threshold = 0.2),
  flags = list(render_report = TRUE, save_png = TRUE, run_tests = FALSE)
), cfg)

# -------- OUTPUT DIR + LOGGING --------
out_dir <- file.path(cfg$outdir_root, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

t0 <- if (exists("manifest_start")) manifest_start(out_dir, cfg) else Sys.time()
on.exit({
  if (exists("manifest_end")) {
    manifest_end(out_dir, t0, warnings_vec = getOption("analysis_warnings", character(0)))
  }
}, add = TRUE)

options(analysis_warnings = character())
register_warning <- function(msg) {
  current <- getOption("analysis_warnings", character())
  options(analysis_warnings = unique(c(current, msg)))
}

# -------- FUNCTIONS --------
save_plot <- function(p, name, width = 8, height = 5) {
  if (isTRUE(cfg$flags$save_png)) {
    ggsave(file.path(out_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 300)
  }
}

score_pss <- function(df, items, reverse_idx, minv, maxv) {
  stopifnot(all(items %in% names(df)))
  mat <- as.data.frame(df[items])
  mat <- mat |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  if (length(reverse_idx)) {
    mat[reverse_idx] <- lapply(mat[reverse_idx], function(x) ifelse(is.na(x), NA_real_, (maxv + minv - x)))
  }
  score <- rowSums(mat, na.rm = FALSE)
  attr(score, "n_missing") <- rowSums(is.na(mat))
  score
}

boot_ci_mean <- function(x, R = 10000, conf = 0.95, seed = 42) {
  set.seed(seed)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2) {
    return(tibble(mean = mean(x), lwr = NA, upr = NA))
  }
  means <- replicate(R, mean(sample(x, size = n, replace = TRUE)))
  q <- quantile(means, probs = c((1 - conf) / 2, 1 - (1 - conf) / 2), na.rm = TRUE)
  tibble(mean = mean(x), lwr = as.numeric(q[1]), upr = as.numeric(q[2]))
}

dedupe_latest <- function(df, id_col, time_col) {
  if (!is.null(time_col) && time_col %in% names(df)) {
    df |> arrange(.data[[id_col]], desc(.data[[time_col]])) |> distinct(.data[[id_col]], .keep_all = TRUE)
  } else {
    df |> distinct(.data[[id_col]], .keep_all = TRUE)
  }
}

capture_analysis <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    register_warning(conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}

capture_analysis({
  # -------- LOAD DATA --------
  if (stringr::str_ends(cfg$input_file, regex("\\.xlsx$", ignore_case = TRUE))) {
    pre <- readxl::read_excel(cfg$input_file, sheet = cfg$sheet_pre) |> janitor::clean_names()
    post <- readxl::read_excel(cfg$input_file, sheet = cfg$sheet_post) |> janitor::clean_names()
  } else {
    stop("For CSV, split pre/post sheets or adapt the loader in analysis.R")
  }

  stopifnot(cfg$id_col %in% names(pre), cfg$id_col %in% names(post))
  pre <- dedupe_latest(pre, cfg$id_col, cfg$time_col)
  post <- dedupe_latest(post, cfg$id_col, cfg$time_col)

  dat <- pre |>
    dplyr::select(dplyr::any_of(c(cfg$id_col, cfg$covars, cfg$pss_items_pre,
      cfg$dose_minutes_day, cfg$dose_days_week, cfg$group_col))) |>
    dplyr::full_join(
      post |> dplyr::select(dplyr::any_of(c(cfg$id_col, cfg$covars, cfg$pss_items_post,
        cfg$dose_minutes_day, cfg$dose_days_week, cfg$group_col))),
      by = cfg$id_col,
      suffix = c("_preR", "_postR")
    ) |> janitor::clean_names()

  # -------- QA --------
  qa <- list()
  qa$n_pre <- nrow(pre)
  qa$n_post <- nrow(post)
  qa$n_merged <- nrow(dat)
  qa$dup_ids_pre <- pre |> count(.data[[cfg$id_col]]) |> filter(n > 1) |> nrow()
  qa$dup_ids_post <- post |> count(.data[[cfg$id_col]]) |> filter(n > 1) |> nrow()
  writeLines(c(capture.output(str(utils::head(dat))), ""), con = file.path(out_dir, "data_glimpse.txt"))
  writeLines(utils::capture_output(qa), con = file.path(out_dir, "qa_counts.txt"))

  # -------- SCORE PSS --------
  dat <- dat |>
    mutate(
      pre_pss = score_pss(cur_data_all(), cfg$pss_items_pre, cfg$pss_reverse_idx, cfg$pss_min, cfg$pss_max),
      post_pss = score_pss(cur_data_all(), cfg$pss_items_post, cfg$pss_reverse_idx, cfg$pss_min, cfg$pss_max)
    )

  dat_joined <- dat |>
    mutate(
      dose_min_per_day = suppressWarnings(as.numeric(.data[[cfg$dose_minutes_day]])),
      dose_days_week = suppressWarnings(as.numeric(.data[[cfg$dose_days_week]])),
      dose_min_week = dose_min_per_day * dose_days_week
    )

  analysis_set <- dat_joined |> filter(!is.na(pre_pss) & !is.na(post_pss))

  mcid_thresh <- sd(analysis_set$pre_pss, na.rm = TRUE) * (cfg$analysis$mcid_frac_sd %||% cfg$mcid_sd)

  analysis_set <- analysis_set |>
    mutate(
      delta_pss = post_pss - pre_pss,
      improved = if_else((pre_pss - post_pss) >= mcid_thresh, 1, 0, missing = NA_real_),
      adherence_tier = case_when(
        is.na(dose_min_per_day) ~ NA_character_,
        dose_min_per_day < 2 ~ "<2 min/day",
        dose_min_per_day < 5 ~ "2–5 min/day",
        TRUE ~ "≥5 min/day"
      )
    )

  minutes_col <- cfg$dose$minutes_col %||% "minutes_total"
  if (!minutes_col %in% names(analysis_set) && "dose_min_week" %in% names(analysis_set)) {
    analysis_set[[minutes_col]] <- analysis_set$dose_min_week
  }

  readr::write_csv(analysis_set, file.path(out_dir, "analysis_set.csv"))

  # -------- DESCRIPTIVES --------
  desc <- analysis_set |>
    summarise(
      n = n(),
      mean_pre = mean(pre_pss, na.rm = TRUE),
      sd_pre = sd(pre_pss, na.rm = TRUE),
      mean_post = mean(post_pss, na.rm = TRUE),
      sd_post = sd(post_pss, na.rm = TRUE),
      mean_delta = mean(delta_pss, na.rm = TRUE),
      sd_delta = sd(delta_pss, na.rm = TRUE),
      mcid_thresh = mcid_thresh,
      mcid_rate = mean(improved, na.rm = TRUE)
    )
  readr::write_csv(desc, file.path(out_dir, "descriptives_pss.csv"))

  # -------- PRIMARY TESTS --------
  t_res <- t.test(analysis_set$delta_pss, mu = 0)
  dz <- effectsize::cohens_d(analysis_set$post_pss, analysis_set$pre_pss, paired = TRUE)
  boot <- boot_ci_mean(analysis_set$delta_pss, R = 10000)

  primary_tbl <- tibble(
    test = c("paired_t", "bootstrap_mean_delta"),
    estimate = c(unname(t_res$estimate), boot$mean),
    lwr = c(unname(t_res$conf.int[1]), boot$lwr),
    upr = c(unname(t_res$conf.int[2]), boot$upr),
    p_value = c(t_res$p.value, NA_real_),
    cohens_dz = c(unname(dz$Cohens_d), NA_real_)
  )
  readr::write_csv(primary_tbl, file.path(out_dir, "primary_tests.csv"))

  # Wilcoxon
  w_res <- suppressWarnings(wilcox.test(analysis_set$post_pss, analysis_set$pre_pss, paired = TRUE, exact = FALSE))
  readr::write_csv(broom::tidy(w_res), file.path(out_dir, "wilcoxon_signed_rank.csv"))

  # -------- ROBUSTNESS --------
  if (exists("cohen_dz")) {
    delta <- analysis_set$post_pss - analysis_set$pre_pss
    dz_val <- cohen_dz(delta)
    ci <- boot_ci(delta)
    p_perm <- perm_test_p(analysis_set$pre_pss, analysis_set$post_pss, iters = cfg$analysis$perm_iters %||% 5000)
    mcid <- if (!is.null(cfg$analysis$mcid_abs)) cfg$analysis$mcid_abs else mcid_thresh
    tost_ok <- if (isTRUE(cfg$analysis$do_tost)) noninferiority_tost(delta, mcid, alpha = cfg$analysis$alpha %||% 0.05) else NA

    exec <- tibble::tibble(
      metric = "PSS Δ (post - pre)",
      dz = dz_val,
      ci_low = ci[1],
      ci_high = ci[2],
      p = p_perm,
      mcid = mcid,
      tost = tost_ok
    )
    readr::write_csv(exec, file.path(out_dir, "exec_metrics.csv"))

    sens <- adherence_sensitivity(analysis_set, minutes_col = cfg$dose$minutes_col %||% "minutes_total")
    readr::write_csv(sens, file.path(out_dir, "adherence_sensitivity.csv"))
  }

  # -------- ASSUMPTION TESTS --------
  shapiro <- tryCatch(shapiro.test(analysis_set$delta_pss), error = function(e) NULL)
  ad_test <- tryCatch(performance::check_normality(lm(delta_pss ~ 1, data = analysis_set)), error = function(e) NULL)
  ass_tbl <- tibble(
    test = c("Shapiro-Wilk", "Performance::check_normality"),
    statistic = c(if (!is.null(shapiro)) unname(shapiro$statistic) else NA_real_, NA_real_),
    p_value = c(if (!is.null(shapiro)) shapiro$p.value else NA_real_, if (!is.null(ad_test)) ad_test$p else NA_real_)
  )
  readr::write_csv(ass_tbl, file.path(out_dir, "assumption_tests.csv"))

  qq_df <- data.frame(sample = stats::na.omit(analysis_set$delta_pss))
  qq_plot <- ggplot(qq_df, aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "#2D7FF9") +
    labs(title = "QQ plot of ΔPSS", x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal(base_size = 12)
  save_plot(qq_plot, "qq_deltaPSS")

  # -------- MODELLING --------
  covars_present <- cfg$covars[cfg$covars %in% names(analysis_set)]
  form_base <- paste0("post_pss ~ pre_pss + ns(dose_min_per_day, df=3)",
                      if (length(covars_present)) paste0(" + ", paste(covars_present, collapse = " + ")) else "")
  use_mixed <- (!is.null(cfg$group_col) && cfg$group_col %in% names(analysis_set))

  if (use_mixed) {
    form_m <- as.formula(paste0(form_base, " + (1|", cfg$group_col, ")"))
    fit <- lmer(form_m, data = analysis_set)
    summ <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
  } else {
    fit <- lm(as.formula(form_base), data = analysis_set)
    summ <- broom::tidy(fit, conf.int = TRUE)
  }
  readr::write_csv(summ, file.path(out_dir, "ancova_summary.csv"))
  checks <- performance::model_performance(fit)
  capture.output(checks, file = file.path(out_dir, "ancova_model_checks.txt"))

  me_dose <- ggeffects::ggpredict(fit, terms = "dose_min_per_day [all]")
  dose_plot <- ggplot(me_dose, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    geom_line(size = 1) +
    geom_hline(yintercept = mean(analysis_set$pre_pss, na.rm = TRUE) - mcid_thresh, linetype = "dashed", alpha = .5) +
    labs(x = "Minutes per day", y = "Adjusted post-PSS", title = "Dose–response (spline) with 95% CI",
         subtitle = "Dashed line ≈ pre-mean − MCID threshold") +
    theme_minimal(base_size = 12)
  save_plot(dose_plot, "dose_spline")

  # -------- ADHERENCE --------
  adherence_levels <- c("<2 min/day", "2–5 min/day", "≥5 min/day")
  ad_agg <- analysis_set |>
    mutate(adherence_tier = factor(adherence_tier, levels = adherence_levels)) |>
    group_by(adherence_tier) |>
    summarise(
      n = n(),
      mean_delta = mean(delta_pss, na.rm = TRUE),
      sd_delta = sd(delta_pss, na.rm = TRUE),
      mcid_rate = mean(improved, na.rm = TRUE),
      .groups = "drop"
    )
  readr::write_csv(ad_agg, file.path(out_dir, "adherence_summary.csv"))

  tier_ci <- analysis_set |>
    mutate(adherence_tier = factor(adherence_tier, levels = adherence_levels)) |>
    group_by(adherence_tier) |>
    summarise(boot_ci_mean(delta_pss), .groups = "drop")

  forest_tier <- ggplot(tier_ci, aes(x = adherence_tier, y = mean)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = .1) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    coord_flip() +
    labs(x = "", y = "ΔPSS (post–pre) mean with 95% boot CI", title = "Change in PSS by adherence tier") +
    theme_minimal(base_size = 12)
  save_plot(forest_tier, "forest_adherence")

  # -------- DISTRIBUTIONS / SLOPEGRAPH --------
  rain <- analysis_set |>
    ggplot(aes(x = 1, y = delta_pss)) +
    ggdist::stat_halfeye(adjust = 0.5, width = 0.7, .width = 0.95, point_interval = "mean_qi") +
    ggdist::stat_dots(side = "left", dotsize = 0.5, alpha = .4) +
    scale_x_continuous(breaks = NULL) +
    labs(y = "ΔPSS (post–pre)", title = "Distribution of change in PSS with 95% interval") +
    theme_minimal(base_size = 12)
  save_plot(rain, "raincloud_deltaPSS")

  samp_n <- min(400, nrow(analysis_set))
  set.seed(1)
  slope_df <- analysis_set |> dplyr::sample_n(samp_n) |>
    dplyr::select(all_of(c(cfg$id_col, "pre_pss", "post_pss"))) |>
    tidyr::pivot_longer(cols = c(pre_pss, post_pss), names_to = "time", values_to = "pss") |>
    dplyr::mutate(time = factor(time, levels = c("pre_pss", "post_pss"), labels = c("Pre", "Post")))
  slope <- ggplot(slope_df, aes(x = time, y = pss, group = .data[[cfg$id_col]])) +
    geom_line(alpha = .15) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.2, color = "black") +
    stat_summary(aes(group = 1), fun.data = mean_cl_normal, geom = "errorbar", width = .1) +
    labs(x = "", y = "PSS", title = "Individual pre→post trajectories (mean ± CI in bold)") +
    theme_minimal(base_size = 12)
  save_plot(slope, "slopegraph_pre_post")

  # -------- SUBGROUP (example) --------
  subgroups <- c("gender", "role", "prior_mindfulness")
  subgroups <- intersect(subgroups, names(analysis_set))
  if (length(subgroups)) {
    make_sub_forest <- function(var) {
      f <- as.formula(paste0("post_pss ~ pre_pss + ns(dose_min_per_day,3) + ", var))
      mf <- lm(f, data = analysis_set)
      est <- modelsummary::tidy(mf, conf.level = 0.95) |>
        dplyr::filter(stringr::str_detect(term, paste0("^", var))) |>
        dplyr::mutate(var = var)
      est
    }
    est_all <- dplyr::bind_rows(lapply(subgroups, make_sub_forest))
    readr::write_csv(est_all, file.path(out_dir, "subgroup_estimates.csv"))
  }

  # -------- MISSINGNESS --------
  miss_profile <- naniar::miss_var_summary(analysis_set)
  readr::write_csv(miss_profile, file.path(out_dir, "missingness_summary.csv"))

  covar_vars <- intersect(cfg$covars, names(analysis_set))
  if (length(covar_vars) >= 1) {
    imp_df <- analysis_set |> dplyr::select(all_of(c(cfg$id_col, "pre_pss", "post_pss", "dose_min_per_day", covar_vars)))
    meth <- mice::make.method(imp_df)
    meth[c("pre_pss", "post_pss")] <- ""
    imp <- mice::mice(imp_df, m = 5, method = meth, maxit = 10, seed = 101)
    fit_m <- with(imp, lm(post_pss ~ pre_pss + splines::ns(dose_min_per_day, 3) + !!as.name(covar_vars[1])))
    pool_res <- mice::pool(fit_m)
    capture.output(summary(pool_res), file = file.path(out_dir, "mice_pooled_summary.txt"))
  }

  # -------- EXEC SUMMARY METRICS --------
  exec_summary <- tibble(
    metric = c(
      "N (complete pre+post)",
      "Mean ΔPSS",
      "ΔPSS 95% boot CI low",
      "ΔPSS 95% boot CI high",
      "Cohen's dz",
      "MCID threshold (points)",
      "% achieving MCID"
    ),
    value = c(
      nrow(analysis_set),
      round(primary_tbl$estimate[primary_tbl$test == "bootstrap_mean_delta"], 2),
      round(primary_tbl$lwr[primary_tbl$test == "bootstrap_mean_delta"], 2),
      round(primary_tbl$upr[primary_tbl$test == "bootstrap_mean_delta"], 2),
      round(primary_tbl$cohens_dz[!is.na(primary_tbl$cohens_dz)], 2),
      round(mcid_thresh, 2),
      round(mean(analysis_set$improved, na.rm = TRUE) * 100, 1)
    )
  )
  readr::write_csv(exec_summary, file.path(out_dir, "executive_summary_metrics.csv"))

  if (exists("dei_audit")) {
    dei_tbl <- dei_audit(analysis_set, cfg$dei$groups %||% c("gender", "age_band", "department"))
    if (nrow(dei_tbl)) {
      readr::write_csv(dei_tbl, file.path(out_dir, "dei_report.csv"))
      readr::write_csv(flag_disparities(dei_tbl, cfg$dei$gap_threshold %||% 0.2), file.path(out_dir, "dei_flags.csv"))
    }
  }

  if (exists("adversarial_checklist")) {
    adversarial_checklist(out_dir, cfg$flags)
  }

  if (exists("config_diff") && file.exists(cfg_path)) {
    readr::write_csv(config_diff(cfg_path, file.path(out_dir, "config_snapshot.yml")),
      file.path(out_dir, "config_diff.csv"))
  }

  writeLines(capture.output(sessionInfo()), con = file.path(out_dir, "sessionInfo.txt"))
  message("Done. Outputs written to: ", out_dir)
})
