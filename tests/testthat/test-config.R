testthat::test_that("CUSUM config is valid and exposes expected settings", {
  testthat::expect_true(is.list(config))
  testthat::expect_true("cusum" %in% names(config))

  required_cusum_keys <- c(
    "default_detection_period",
    "minimum_prepared_weeks",
    "minimum_observed_coverage_stop",
    "minimum_observed_coverage_warn",
    "fill_missing_weeks_with_zero"
  )

  testthat::expect_true(all(required_cusum_keys %in% names(config$cusum)))
  testthat::expect_equal(length(rap_validate_cusum_config(config)), 0)

  testthat::expect_true(is.numeric(as.numeric(config$cusum$default_detection_period)))
  testthat::expect_gte(as.numeric(config$cusum$default_detection_period), 1)
  testthat::expect_lt(as.numeric(config$cusum$minimum_observed_coverage_stop),
                      as.numeric(config$cusum$minimum_observed_coverage_warn))
})

testthat::test_that("active country is read from config and has levels", {
  testthat::expect_true("active_country" %in% names(config))
  testthat::expect_true(active_country %in% names(config))
  testthat::expect_true(length(config[[active_country]]$levels) > 0)

  choices <- rap_cusum_geo_choices(config, active_country)
  testthat::expect_true(length(choices) > 0)
  testthat::expect_true(all(nzchar(names(choices))))
})
