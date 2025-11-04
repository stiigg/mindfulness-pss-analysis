testthat::test_that("Permutation p-value within [0,1]", {
  testthat::skip_if_not_installed("boot")
  source("R/robustness.R")
  set.seed(1)
  pre <- rnorm(50, 18, 6)
  post <- pre - rnorm(50, 1.5, 2.5)
  p <- perm_test_p(pre, post, iters = 2000)
  testthat::expect_true(is.finite(p) && p >= 0 && p <= 1)
})
