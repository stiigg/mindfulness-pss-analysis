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
  "ggdist","cowplot","scales","modelsummary","yaml"
)
inst <- setdiff(need, rownames(installed.packages()))
if(length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# -------- CONFIG (YAML or defaults) --------
cfg_path <- "config.yml"
if (file.exists(cfg_path)) {
  cfg <- yaml::read_yaml(cfg_path)
} else {
  cfg <- list(
    input_file = "data/mindfulness_survey.xlsx",
    sheet_pre  = "pre",
    sheet_post = "post",
    id_col     = "participant_id",
    time_col   = NULL,
    pss_items_pre  = c("pss1_pre","pss2_pre","pss3_pre","pss4_pre","pss5_pre","pss6_pre","pss7_pre","pss8_pre","pss9_pre","pss10_pre"),
    pss_items_post = c("pss1_post","pss2_post","pss3_post","pss4_post","pss5_post","pss6_post","pss7_post","pss8_post","pss9_post","pss10_post"),
    pss_reverse_idx = c(4,5,7,8),
    pss_min = 0, pss_max = 4,
    dose_minutes_day = "minutes_per_day",
    dose_days_week   = "days_per_week",
    covars = c("age","gender","role","prior_mindfulness"),
    group_col = NULL,
    mcid_sd = 0.5,
    outdir_root = "outputs"
  )
}

# -------- UTILS --------
ts_outdir <- file.path(cfg$outdir_root, paste0("run_", format(Sys.time(),"%Y%m%d_%H%M%S")))
dir.create(ts_outdir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(p, name, width=8, height=5){
  ggsave(file.path(ts_outdir, paste0(name, ".png")), p, width=width, height=height, dpi=300)
}

score_pss <- function(df, items, reverse_idx, minv, maxv) {
  stopifnot(all(items %in% names(df)))
  mat <- as.data.frame(df[items])
  mat <- mat |> dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))
  if(length(reverse_idx)){
    mat[reverse_idx] <- lapply(mat[reverse_idx], function(x) ifelse(is.na(x), NA_real_, (maxv + minv - x)))
  }
  score <- rowSums(mat, na.rm = FALSE)
  attr(score, "n_missing") <- rowSums(is.na(mat))
  score
}

boot_ci_mean <- function(x, R=10000, conf=0.95, seed=42){
  set.seed(seed)
  x <- x[is.finite(x)]
  n <- length(x)
  if(n < 2) return(tibble(mean=mean(x), lwr=NA, upr=NA))
  means <- replicate(R, mean(sample(x, size=n, replace=TRUE)))
  q <- quantile(means, probs=c((1-conf)/2, 1-(1-conf)/2), na.rm=TRUE)
  tibble(mean=mean(x), lwr=as.numeric(q[1]), upr=as.numeric(q[2]))
}

dedupe_latest <- function(df, id_col, time_col){
  if(!is.null(time_col) && time_col %in% names(df)){
    df |> arrange(.data[[id_col]], desc(.data[[time_col]])) |> distinct(.data[[id_col]], .keep_all = TRUE)
  } else {
    df |> distinct(.data[[id_col]], .keep_all = TRUE)
  }
}

# -------- LOAD DATA --------
if(stringr::str_ends(cfg$input_file, regex("\\.xlsx$", ignore_case=TRUE))){
  pre  <- readxl::read_excel(cfg$input_file, sheet = cfg$sheet_pre)  |> janitor::clean_names()
  post <- readxl::read_excel(cfg$input_file, sheet = cfg$sheet_post) |> janitor::clean_names()
} else {
  stop("For CSV, split pre/post sheets or adapt the loader in analysis.R")
}

stopifnot(cfg$id_col %in% names(pre), cfg$id_col %in% names(post))
pre  <- dedupe_latest(pre,  cfg$id_col, cfg$time_col)
post <- dedupe_latest(post, cfg$id_col, cfg$time_col)

dat <- pre |>
  dplyr::select(dplyr::any_of(c(cfg$id_col, cfg$covars, cfg$pss_items_pre,
                  cfg$dose_minutes_day, cfg$dose_days_week, cfg$group_col))) |>
  dplyr::full_join(
    post |> dplyr::select(dplyr::any_of(c(cfg$id_col, cfg$covars, cfg$pss_items_post,
                            cfg$dose_minutes_day, cfg$dose_days_week, cfg$group_col))),
    by = cfg$id_col,
    suffix = c("_preR","_postR")
  ) |> janitor::clean_names()

# -------- QA --------
qa <- list()
qa$n_pre  <- nrow(pre);  qa$n_post <- nrow(post); qa$n_merged <- nrow(dat)
qa$dup_ids_pre  <- pre  |> count(.data[[cfg$id_col]]) |> filter(n>1) |> nrow()
qa$dup_ids_post <- post |> count(.data[[cfg$id_col]]) |> filter(n>1) |> nrow()
writeLines(c(capture.output(str(utils::head(dat))), ""), con = file.path(ts_outdir,"data_glimpse.txt"))
writeLines(utils::capture.output(qa), con=file.path(ts_outdir,"qa_counts.txt"))

# -------- SCORE PSS --------
dat <- dat |>
  mutate(
    pss_pre  = score_pss(cur_data_all(), cfg$pss_items_pre,  cfg$pss_reverse_idx, cfg$pss_min, cfg$pss_max),
    pss_post = score_pss(cur_data_all(), cfg$pss_items_post, cfg$pss_reverse_idx, cfg$pss_min, cfg$pss_max)
  )

analysis_set <- dat |> filter(!is.na(pss_pre) & !is.na(pss_post))

# Dose & adherence
analysis_set <- analysis_set |>
  mutate(
    dose_min_per_day = suppressWarnings(as.numeric(.data[[cfg$dose_minutes_day]])),
    dose_days_week   = suppressWarnings(as.numeric(.data[[cfg$dose_days_week]])),
    dose_min_week    = dose_min_per_day * dose_days_week,
    adherence_tier   = case_when(
      is.na(dose_min_per_day) ~ NA_character_,
      dose_min_per_day < 2 ~ "<2 min/day",
      dose_min_per_day < 5 ~ "2–5 min/day",
      TRUE ~ "≥5 min/day"
    )
  )

mcid_thresh <- sd(analysis_set$pss_pre, na.rm=TRUE) * cfg$mcid_sd

analysis_set <- analysis_set |>
  mutate(
    delta_pss = pss_post - pss_pre,
    improved  = if_else((pss_pre - pss_post) >= mcid_thresh, 1, 0, missing = NA_real_)
  )

readr::write_csv(analysis_set, file.path(ts_outdir, "analysis_set.csv"))

# -------- DESCRIPTIVES --------
desc <- analysis_set |>
  summarise(
    n = n(),
    mean_pre  = mean(pss_pre, na.rm=TRUE),
    sd_pre    = sd(pss_pre, na.rm=TRUE),
    mean_post = mean(pss_post, na.rm=TRUE),
    sd_post   = sd(pss_post, na.rm=TRUE),
    mean_delta = mean(delta_pss, na.rm=TRUE),
    sd_delta   = sd(delta_pss, na.rm=TRUE),
    mcid_thresh = mcid_thresh,
    mcid_rate = mean(improved, na.rm=TRUE)
  )
readr::write_csv(desc, file.path(ts_outdir,"descriptives_pss.csv"))

# -------- PRIMARY TESTS --------
t_res <- t.test(analysis_set$delta_pss, mu=0)
dz <- effectsize::cohens_d(analysis_set$pss_post, analysis_set$pss_pre, paired=TRUE)
boot <- boot_ci_mean(analysis_set$delta_pss, R=10000)

primary_tbl <- tibble(
  test = c("paired_t", "bootstrap_mean_delta"),
  estimate = c(unname(t_res$estimate), boot$mean),
  lwr = c(unname(t_res$conf.int[1]), boot$lwr),
  upr = c(unname(t_res$conf.int[2]), boot$upr),
  p_value = c(t_res$p.value, NA_real_),
  cohens_dz = c(unname(dz$Cohens_d), NA_real_)
)
readr::write_csv(primary_tbl, file.path(ts_outdir,"primary_tests.csv"))

# Wilcoxon
w_res <- wilcox.test(analysis_set$pss_post, analysis_set$pss_pre, paired=TRUE, exact=FALSE)
readr::write_csv(broom::tidy(w_res), file.path(ts_outdir,"wilcoxon_signed_rank.csv"))

# -------- EQUIVALENCE (TOST) --------
sesoi <- mcid_thresh
tost <- TOSTER::TOSTone(m = mean(analysis_set$delta_pss, na.rm=TRUE),
                        mu = 0,
                        sd = sd(analysis_set$delta_pss, na.rm=TRUE),
                        n = sum(is.finite(analysis_set$delta_pss)),
                        low_eqbound = -sesoi, high_eqbound = sesoi, alpha=0.05)
capture.output(tost, file=file.path(ts_outdir,"tost_equivalence.txt"))

# -------- ANCOVA / MIXED --------
covars_present <- cfg$covars[cfg$covars %in% names(analysis_set)]
form_base <- paste0("pss_post ~ pss_pre + ns(dose_min_per_day, df=3)",
                    if(length(covars_present)) paste0(" + ", paste(covars_present, collapse=" + ")) else "")
use_mixed <- (!is.null(cfg$group_col) && cfg$group_col %in% names(analysis_set))

if(use_mixed){
  form_m <- as.formula(paste0(form_base, " + (1|", cfg$group_col, ")"))
  fit <- lmer(form_m, data = analysis_set)
  summ <- broom.mixed::tidy(fit, effects="fixed", conf.int=TRUE)
} else {
  fit <- lm(as.formula(form_base), data = analysis_set)
  summ <- broom::tidy(fit, conf.int=TRUE)
}
readr::write_csv(summ, file.path(ts_outdir,"ancova_summary.csv"))
checks <- performance::model_performance(fit)
capture.output(checks, file=file.path(ts_outdir,"ancova_model_checks.txt"))

# Marginal effects for dose
me_dose <- ggeffects::ggpredict(fit, terms = "dose_min_per_day [all]")
dose_plot <- ggplot(me_dose, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.15) +
  geom_line(size=1) +
  geom_hline(yintercept = mean(analysis_set$pss_pre, na.rm=TRUE) - mcid_thresh, linetype="dashed", alpha=.5) +
  labs(x="Minutes per day", y="Adjusted post-PSS", title="Dose–response (spline) with 95% CI",
       subtitle="Dashed line ≈ pre-mean − MCID threshold") +
  theme_minimal(base_size = 12)
save_plot(dose_plot, "fig_dose_response_spline")

# -------- ADHERENCE --------
adherence_levels <- c("<2 min/day","2–5 min/day","≥5 min/day")
ad_agg <- analysis_set |>
  mutate(adherence_tier = factor(adherence_tier, levels = adherence_levels)) |>
  group_by(adherence_tier) |>
  summarise(
    n = n(),
    mean_delta = mean(delta_pss, na.rm=TRUE),
    sd_delta = sd(delta_pss, na.rm=TRUE),
    mcid_rate = mean(improved, na.rm=TRUE),
    .groups="drop"
  )
readr::write_csv(ad_agg, file.path(ts_outdir,"adherence_summary.csv"))

tier_ci <- analysis_set |>
  mutate(adherence_tier = factor(adherence_tier, levels = adherence_levels)) |>
  group_by(adherence_tier) |>
  summarise(boot_ci_mean(delta_pss), .groups="drop")

forest_tier <- ggplot(tier_ci, aes(x=adherence_tier, y=mean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.1) +
  geom_hline(yintercept=0, linetype="dotted") +
  coord_flip() +
  labs(x="", y="ΔPSS (post–pre) mean with 95% boot CI", title="Change in PSS by adherence tier") +
  theme_minimal(base_size = 12)
save_plot(forest_tier, "fig_forest_adherence")

# -------- DISTRIBUTIONS / SLOPEGRAPH --------
rain <- analysis_set |>
  ggplot(aes(x = 1, y = delta_pss)) +
  ggdist::stat_halfeye(adjust=0.5, width=0.7, .width=0.95, point_interval = "mean_qi") +
  ggdist::stat_dots(side="left", dotsize=0.5, alpha=.4) +
  scale_x_continuous(breaks=NULL) +
  labs(y="ΔPSS (post–pre)", title="Distribution of change in PSS with 95% interval") +
  theme_minimal(base_size = 12)
save_plot(rain, "fig_raincloud_delta")

samp_n <- min(400, nrow(analysis_set))
set.seed(1)
slope_df <- analysis_set |> dplyr::sample_n(samp_n) |>
  dplyr::select(all_of(c(cfg$id_col, "pss_pre","pss_post"))) |>
  tidyr::pivot_longer(cols=c(pss_pre, pss_post), names_to="time", values_to="pss") |>
  dplyr::mutate(time = factor(time, levels=c("pss_pre","pss_post"), labels=c("Pre","Post")))
slope <- ggplot(slope_df, aes(x=time, y=pss, group=.data[[cfg$id_col]])) +
  geom_line(alpha=.15) +
  stat_summary(aes(group=1), fun=mean, geom="line", size=1.2, color="black") +
  stat_summary(aes(group=1), fun.data=mean_cl_normal, geom="errorbar", width=.1) +
  labs(x="", y="PSS", title="Individual pre→post trajectories (mean ± CI in bold)") +
  theme_minimal(base_size = 12)
save_plot(slope, "fig_slopegraph_pre_post")

# -------- SUBGROUP (example) --------
subgroups <- c("gender","role","prior_mindfulness")
subgroups <- intersect(subgroups, names(analysis_set))
if(length(subgroups)){
  make_sub_forest <- function(var){
    f <- as.formula(paste0("pss_post ~ pss_pre + ns(dose_min_per_day,3) + ", var))
    mf <- lm(f, data = analysis_set)
    est <- modelsummary::tidy(mf, conf.level = 0.95) |>
      dplyr::filter(stringr::str_detect(term, paste0("^", var))) |>
      dplyr::mutate(var = var)
    est
  }
  est_all <- dplyr::bind_rows(lapply(subgroups, make_sub_forest))
  readr::write_csv(est_all, file.path(ts_outdir,"subgroup_estimates.csv"))
}

# -------- MISSINGNESS --------
miss_profile <- naniar::miss_var_summary(analysis_set)
readr::write_csv(miss_profile, file.path(ts_outdir,"missingness_summary.csv"))

covar_vars <- intersect(cfg$covars, names(analysis_set))
if(length(covar_vars) >= 1){
  imp_df <- analysis_set |> dplyr::select(all_of(c(cfg$id_col,"pss_pre","pss_post","dose_min_per_day",covar_vars)))
  meth <- mice::make.method(imp_df)
  meth[c("pss_pre","pss_post")] <- ""
  imp <- mice::mice(imp_df, m=5, method=meth, maxit=10, seed=101)
  fit_m <- with(imp, lm(pss_post ~ pss_pre + splines::ns(dose_min_per_day,3) + !!as.name(covar_vars[1])))
  pool_res <- mice::pool(fit_m)
  capture.output(summary(pool_res), file=file.path(ts_outdir,"mice_pooled_summary.txt"))
}

# -------- EXEC SUMMARY METRICS --------
exec <- tibble(
  metric = c("N (complete pre+post)","Mean ΔPSS","ΔPSS 95% boot CI low","ΔPSS 95% boot CI high","Cohen's dz","MCID threshold (points)","% achieving MCID"),
  value  = c(nrow(analysis_set),
             round(primary_tbl$estimate[primary_tbl$test=="bootstrap_mean_delta"],2),
             round(primary_tbl$lwr[primary_tbl$test=="bootstrap_mean_delta"],2),
             round(primary_tbl$upr[primary_tbl$test=="bootstrap_mean_delta"],2),
             round(primary_tbl$cohens_dz[!is.na(primary_tbl$cohens_dz)],2),
             round(mcid_thresh,2),
             round(mean(analysis_set$improved, na.rm=TRUE)*100,1))
)
readr::write_csv(exec, file.path(ts_outdir,"executive_summary_metrics.csv"))

writeLines(capture.output(sessionInfo()), con=file.path(ts_outdir,"sessionInfo.txt"))
message("Done. Outputs written to: ", ts_outdir)
