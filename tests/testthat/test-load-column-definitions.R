sample_column_definitions <- function() {
  data.frame(
    index = c(1L, 2L),
    table = c("deployments", "deployments"),
    name = c("organization_code", "deployment_depth_m"),
    type = c("code", "float"),
    default = c(NA_character_, NA_character_),
    min = c(NA_real_, 0),
    max = c(NA_real_, 100),
    unique_by = c(NA_character_, NA_character_),
    required = c(TRUE, NA),
    required_unless = c(NA_character_, NA_character_),
    required_ncei = c(NA, TRUE),
    reference_table = c("organizations", NA_character_),
    notes = c(NA_character_, "Example note")
  )
}

write_column_definitions_csv <- function(path, data = sample_column_definitions()) {
  readr::write_csv(data, path)
  path
}

column_file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))
}

test_that("load_column_definitions parses local CSV files", {
  csv_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(csv_file)

  columns <- load_column_definitions(file = csv_file)

  expect_named(columns, "deployments")
  expect_s3_class(columns$deployments, "data.frame")
  expect_equal(columns$deployments$name, c("organization_code", "deployment_depth_m"))
  expect_type(columns$deployments$index, "integer")
  expect_type(columns$deployments$min, "double")
  expect_type(columns$deployments$required, "logical")
  expect_equal(columns$deployments$required, c(TRUE, FALSE))
  expect_equal(columns$deployments$required_ncei, c(FALSE, TRUE))
})

test_that("load_column_definitions fetches CSV URLs and writes the cache", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(source_file)

  columns <- load_column_definitions(
    url = column_file_url(source_file),
    cache_file = cache_file
  )

  expect_named(columns, "deployments")
  expect_true(file.exists(cache_file))
  expect_equal(
    paste(readLines(cache_file, warn = FALSE), collapse = "\n"),
    paste(readLines(source_file, warn = FALSE), collapse = "\n")
  )
})

test_that("load_column_definitions falls back to the cache on fetch failure", {
  missing_source <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(cache_file)

  expect_warning(
    columns <- load_column_definitions(
      url = column_file_url(missing_source),
      cache_file = cache_file
    ),
    "Using cached column definitions"
  )

  expect_named(columns, "deployments")
})

test_that("load_column_definitions falls back to the cache on invalid remote CSV", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      index = 1L,
      name = "missing_table",
      min = NA_real_,
      max = NA_real_,
      required = NA,
      required_ncei = NA
    ),
    source_file
  )
  write_column_definitions_csv(cache_file)

  expect_warning(
    columns <- load_column_definitions(
      url = column_file_url(source_file),
      cache_file = cache_file
    ),
    "Using cached column definitions"
  )

  expect_named(columns, "deployments")
})

test_that("load_column_definitions errors when fetch and cache are unavailable", {
  missing_source <- tempfile(fileext = ".csv")
  missing_cache <- tempfile(fileext = ".csv")

  expect_error(
    load_column_definitions(
      url = column_file_url(missing_source),
      cache_file = missing_cache
    ),
    "No cached column definitions were available"
  )
})

test_that("load_column_definitions validates CSV structure", {
  csv_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      index = 1L,
      name = "missing_table",
      min = NA_real_,
      max = NA_real_,
      required = NA,
      required_ncei = NA
    ),
    csv_file
  )

  expect_error(
    load_column_definitions(file = csv_file),
    "missing required columns"
  )
})

test_that("validate_submission uses column_definitions_file when columns is NULL", {
  csv_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(csv_file)

  expect_true(validate_submission(
    tempdir(),
    tables = character(0),
    column_definitions_file = csv_file,
    reference_tables = list(),
    manual_rules = list(),
    output_file = NULL,
    verbose = FALSE
  ))
})

test_that("validate_submission columns override column_definitions_file", {
  missing_file <- tempfile(fileext = ".csv")

  expect_true(validate_submission(
    tempdir(),
    tables = character(0),
    columns = list(),
    column_definitions_file = missing_file,
    reference_tables = list(),
    manual_rules = list(),
    output_file = NULL,
    verbose = FALSE
  ))
})
