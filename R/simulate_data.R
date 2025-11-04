simulate_pss <- function(n = 120, mu_pre = 18, sd_pre = 6, true_delta = -2, adherence = "minutes_total") {
  set.seed(123)
  id <- sprintf("P%04d", 1:n)
  pre <- tibble::tibble(id = id, matrix(pmax(0, pmin(4, round(rnorm(n * 10, 2, 1)))), ncol = 10))
  names(pre)[2:11] <- paste0("pss10_", 1:10)
  dose <- tibble::tibble(id = id, minutes_total = rgamma(n, 3, 0.01))
  eff <- scales::rescale(dose$minutes_total, to = c(0, 1))
  post_bias <- rnorm(n, true_delta * eff, 2)
  post <- pre
  post[2:11] <- lapply(pre[2:11], function(x) pmax(0, pmin(4, x + round(post_bias / 10, 0))))
  list(pre = pre, post = post, dose = dose)
}
