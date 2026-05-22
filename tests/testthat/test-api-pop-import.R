testthat::test_that("API-POP import accepts common aliases and standardises types", {
  raw <- data.frame(
    Country = c("Argentina", "Argentina"),
    Province = c(" Buenos Aires ", "Buenos Aires"),
    Department = c("La Plata", "La Plata"),
    `Año` = c("2026", "2026"),
    SE = c("6", "7"),
    cases = c("12", "15"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  std <- api_pop_standardise_columns(
    raw,
    config = config,
    active_country = active_country,
    require_population = FALSE
  )

  testthat::expect_true(all(c("country", "level1", "level2", "year", "week", "n_cases") %in% names(std)))
  testthat::expect_type(std$year, "integer")
  testthat::expect_type(std$week, "integer")
  testthat::expect_type(std$n_cases, "double")
  testthat::expect_equal(std$n_cases, c(12, 15))
  testthat::expect_equal(std$level1, c("Buenos Aires", "Buenos Aires"))
  testthat::expect_true(length(api_pop_get_log(std)) > 0)
  testthat::expect_null(api_pop_validate_data(std, require_population = FALSE, target_col = "level2"))
})

testthat::test_that("CUSUM import does not require population", {
  raw <- data.frame(
    country = "Argentina",
    level1 = "Buenos Aires",
    level2 = "La Plata",
    year = 2026,
    week = 6,
    count = 12
  )

  std <- api_pop_standardise_columns(raw, require_population = FALSE)
  testthat::expect_true("n_cases" %in% names(std))
  testthat::expect_false("population" %in% names(std))
  testthat::expect_null(api_pop_validate_data(std, require_population = FALSE, target_col = "level2"))
})

testthat::test_that("missing case column fails with a user-facing message", {
  raw <- data.frame(
    country = "Argentina",
    level1 = "Buenos Aires",
    level2 = "La Plata",
    year = 2026,
    week = 6
  )

  testthat::expect_error(
    api_pop_standardise_columns(raw, require_population = FALSE),
    "Missing required column"
  )
})

testthat::test_that("CSV reader detects comma and semicolon delimiters", {
  comma_file <- tempfile(fileext = ".csv")
  semicolon_file <- tempfile(fileext = ".csv")

  writeLines(c("year,week,cases", "2026,6,12"), comma_file)
  writeLines(c("year;week;cases", "2026;7;15"), semicolon_file)

  comma_df <- api_pop_read_file(comma_file)
  semicolon_df <- api_pop_read_file(semicolon_file)

  testthat::expect_equal(nrow(comma_df), 1)
  testthat::expect_equal(nrow(semicolon_df), 1)
  testthat::expect_true("cases" %in% names(comma_df))
  testthat::expect_true("cases" %in% names(semicolon_df))
})

testthat::test_that("multiple uploaded files are combined with file_index and a preparation log", {
  file_one <- tempfile(fileext = ".csv")
  file_two <- tempfile(fileext = ".csv")

  writeLines(c(
    "country,level1,level2,year,week,cases",
    "Argentina,Buenos Aires,La Plata,2026,6,12"
  ), file_one)
  writeLines(c(
    "country,level1,level2,year,week,cases",
    "Argentina,Buenos Aires,La Plata,2026,7,15"
  ), file_two)

  combined <- api_pop_combine_files(
    c(file_one, file_two),
    config = config,
    active_country = active_country,
    require_population = FALSE
  )

  testthat::expect_equal(nrow(combined), 2)
  testthat::expect_true("file_index" %in% names(combined))
  testthat::expect_equal(sort(unique(combined$file_index)), c(1, 2))
  testthat::expect_true(any(grepl("Read 2 uploaded file", api_pop_get_log(combined))))
})
