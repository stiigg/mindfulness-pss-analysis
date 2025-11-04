suppressPackageStartupMessages({
  library(dplyr)
  library(boot)
})

cohen_dz <- function(delta) {
  mean(delta, na.rm = TRUE) / stats::sd(delta, na.rm = TRUE)
}

boot_ci <- function(delta, R = 2000) {
  b <- boot::boot(delta, statistic = function(d, i) cohen_dz(d[i]), R = R)
  boot::boot.ci(b, type = "perc")$percent[4:5]
}

perm_test_p <- function(pre, post, iters = 5000) {
  delta <- post - pre
  obs <- mean(delta, na.rm = TRUE)
  signs <- matrix(sample(c(-1, 1), length(delta) * iters, TRUE), ncol = iters)
  sims <- colMeans(delta * signs, na.rm = TRUE)
  (sum(abs(sims) >= abs(obs)) + 1) / (iters + 1)
}

adherence_sensitivity <- function(df, minutes_col, quantiles = c(0, 0.33, 0.66, 1)) {
  if (!minutes_col %in% names(df)) {
    return(tibble::tibble())
  }
  qs <- stats::quantile(df[[minutes_col]], probs = quantiles, na.rm = TRUE)
  df$bin <- cut(df[[minutes_col]], include.lowest = TRUE, breaks = qs, dig.lab = 4)
  df |>
    group_by(bin) |>
    summarise(
      n = dplyr::n(),
      dz = cohen_dz(post_pss - pre_pss),
      .groups = "drop"
    )
}

noninferiority_tost <- function(delta, sesoi, alpha = 0.05) {
  res <- tryCatch(
    TOSTER::TOSTone.raw(
      m = mean(delta, na.rm = TRUE),
      sd = stats::sd(delta, na.rm = TRUE),
      n = sum(!is.na(delta)),
      low_eqbound = -sesoi,
      high_eqbound = sesoi,
      alpha = alpha
    ),
    error = function(e) NULL
  )
  !is.null(res) && res$TOST_outcome == "significant"
}
