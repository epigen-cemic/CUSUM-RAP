make_cusum_input <- function(weeks, cases = rep(1, length(weeks))) {
  data.frame(
    file_index = seq_along(weeks),
    country = "Argentina",
    level1 = "Buenos Aires",
    level2 = "La Plata",
    year = 2026L,
    week = as.integer(weeks),
    n_cases = as.numeric(cases),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("prepared data fills missing weeks with zero without modifying raw rows", {
  raw <- make_cusum_input(c(1, 3), c(5, 7))
  original_rows <- nrow(raw)

  prepared <- process_target_data(
    raw_df = raw,
    target_locations = character(0),
    target_col = "level2",
    req_cols = c("country", "level1", "level2"),
    overlap_method = "sum",
    hierarchy_levels = config$Argentina$levels,
    selected_level = "department"
  )

  testthat::expect_equal(original_rows, 2)
  testthat::expect_equal(nrow(prepared), 3)
  testthat::expect_true(2L %in% prepared$week)
  testthat::expect_equal(prepared$n_cases[prepared$week == 2L], 0)

  coverage <- attr(prepared, "cusum_coverage_summary", exact = TRUE)
  testthat::expect_false(is.null(coverage))
  testthat::expect_equal(coverage$observed_weeks, 2)
  testthat::expect_equal(coverage$prepared_weeks, 3)
  testthat::expect_equal(coverage$missing_weeks, 1)
  testthat::expect_equal(coverage$observed_coverage, 2 / 3)
})

testthat::test_that("empty location selection includes all locations", {
  raw <- rbind(
    make_cusum_input(c(1, 2), c(1, 2)),
    transform(make_cusum_input(c(1, 2), c(3, 4)), level2 = "Moron")
  )

  prepared <- process_target_data(
    raw_df = raw,
    target_locations = character(0),
    target_col = "level2",
    req_cols = c("country", "level1", "level2"),
    overlap_method = "sum",
    hierarchy_levels = config$Argentina$levels,
    selected_level = "department"
  )

  testthat::expect_equal(length(unique(prepared$analysis_unit_id)), 2)
})

testthat::test_that("overlap methods sum, old, and new behave as expected", {
  raw <- data.frame(
    file_index = c(1, 2),
    country = "Argentina",
    level1 = "Buenos Aires",
    level2 = "La Plata",
    year = 2026L,
    week = 1L,
    n_cases = c(5, 9),
    stringsAsFactors = FALSE
  )

  run_method <- function(method) {
    process_target_data(
      raw_df = raw,
      target_locations = character(0),
      target_col = "level2",
      req_cols = c("country", "level1", "level2"),
      overlap_method = method,
      hierarchy_levels = config$Argentina$levels,
      selected_level = "department"
    )$n_cases[1]
  }

  testthat::expect_equal(run_method("sum"), 14)
  testthat::expect_equal(run_method("old"), 5)
  testthat::expect_equal(run_method("new"), 9)
})

testthat::test_that("coverage validation stops when observed coverage is too low", {
  sparse <- make_cusum_input(c(seq(1, 51, by = 5), 52), rep(1, 12))

  prepared <- process_target_data(
    raw_df = sparse,
    target_locations = character(0),
    target_col = "level2",
    req_cols = c("country", "level1", "level2"),
    overlap_method = "sum",
    hierarchy_levels = config$Argentina$levels,
    selected_level = "department"
  )

  assessment <- cusum_assess_prepared_coverage(prepared, detection_period = 52, cfg = config)
  testthat::expect_equal(assessment$status, "stop")
  testthat::expect_lt(assessment$minimum_coverage, config$cusum$minimum_observed_coverage_stop)
})

testthat::test_that("coverage validation is OK for complete weekly data", {
  complete <- make_cusum_input(1:52, rep(1, 52))

  prepared <- process_target_data(
    raw_df = complete,
    target_locations = character(0),
    target_col = "level2",
    req_cols = c("country", "level1", "level2"),
    overlap_method = "sum",
    hierarchy_levels = config$Argentina$levels,
    selected_level = "department"
  )

  assessment <- cusum_assess_prepared_coverage(prepared, detection_period = 52, cfg = config)
  testthat::expect_equal(assessment$status, "ok")
  testthat::expect_equal(assessment$minimum_coverage, 1)
})
