testthat::test_that("DEI audit returns gaps and flags", {
  source("R/dei_audit.R")
  df <- data.frame(pre_pss = rnorm(60, 18, 6), post_pss = rnorm(60, 16, 6), gender = rep(c("F", "M"), 30))
  res <- dei_audit(df, "gender")
  testthat::expect_true(all(c("dz", "n") %in% names(res)))
  flags <- flag_disparities(res, threshold = 0.1)
  testthat::expect_true(all(c("gap", "flag") %in% names(flags)))
})
