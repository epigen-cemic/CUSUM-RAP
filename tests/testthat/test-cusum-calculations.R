testthat::test_that("CUSUM parameter helpers return expected values", {
  testthat::expect_equal(calculate_k_from_rr(rr = 2, mu0_mean = 4), 1)
  testthat::expect_error(calculate_k_from_rr(rr = 2, mu0_mean = -1), "mu0_mean cannot be negative")

  h <- recommend_h(arl0 = 100, k = 1)
  testthat::expect_true(is.numeric(h))
  testthat::expect_true(is.finite(h))
  testthat::expect_gt(h, 0)

  k <- recommend_k(arl0 = 100, h = h)
  testthat::expect_true(is.numeric(k))
  testthat::expect_true(is.finite(k))
  testthat::expect_gt(k, 0)
})

testthat::test_that("CUSUM core raises alarms for large sustained increases", {
  y <- c(rep(1, 5), rep(10, 5))
  mu <- rep(1, length(y))

  res <- cusum_core(y = y, mu = mu, k = 1, h = 3, reset = FALSE)

  testthat::expect_equal(length(res$cusum), length(y))
  testthat::expect_equal(length(res$alarm), length(y))
  testthat::expect_true(any(res$alarm))
  testthat::expect_true(all(res$cusum >= 0))
})

testthat::test_that("CUSUM all-units runner preserves unit coverage and result columns", {
  df <- data.frame(
    analysis_unit_id = rep(c("A", "B"), each = 10),
    time_index = rep(1:10, 2),
    n_cases = c(rep(1, 5), rep(8, 5), rep(2, 10))
  )

  out <- run_cusum_all_units(
    df = df,
    unit_var = "analysis_unit_id",
    baseline_filter = function(d) d$time_index <= 5,
    detect_filter = function(d) d$time_index > 5,
    k = 1,
    h = 3,
    fixed_mu = 1,
    reset = TRUE
  )

  testthat::expect_equal(nrow(out), nrow(df))
  testthat::expect_true(all(c("mu_hat", "cusum", "alarm", "z") %in% names(out)))
  testthat::expect_true(any(out$alarm[out$analysis_unit_id == "A"]))
})
