sample_manual_rules <- function() {
  data.frame(
    table = c("deployments", "recordings", "recordings"),
    name = c(
      "start_after_end.deployment_datetime",
      "usable_before_start.recording_usable_start_datetime",
      "usable_after_end.recording_usable_end_datetime"
    ),
    label = c(
      "'deployment_datetime' cannot be after 'recovery_datetime'",
      "'recording_usable_start_datetime' cannot be before 'recording_start_datetime'",
      "'recording_usable_end_datetime' cannot be after 'recording_end_datetime'"
    ),
    rule = c(
      "is.na(deployment_datetime) | is.na(recovery_datetime)",
      "is.na(recording_usable_start_datetime) | is.na(recording_start_datetime)",
      "is.na(recording_usable_end_datetime) | is.na(recording_end_datetime)"
    )
  )
}

write_manual_rules_csv <- function(path, data = sample_manual_rules()) {
  readr::write_csv(data, path)
  path
}

manual_file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))
}

test_that("load_manual_rules parses local CSV files", {
  csv_file <- tempfile(fileext = ".csv")
  write_manual_rules_csv(csv_file)

  manual_rules <- load_manual_rules(file = csv_file)

  expect_named(manual_rules, c("deployments", "recordings"))
  expect_s3_class(manual_rules$deployments, "data.frame")
  expect_equal(manual_rules$deployments$name, "start_after_end.deployment_datetime")
  expect_equal(nrow(manual_rules$recordings), 2)
})

test_that("load_manual_rules fetches CSV URLs and writes the cache", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  write_manual_rules_csv(source_file)

  manual_rules <- load_manual_rules(
    url = manual_file_url(source_file),
    cache_file = cache_file
  )

  expect_named(manual_rules, c("deployments", "recordings"))
  expect_true(file.exists(cache_file))
  expect_equal(
    paste(readLines(cache_file, warn = FALSE), collapse = "\n"),
    paste(readLines(source_file, warn = FALSE), collapse = "\n")
  )
})

test_that("load_manual_rules falls back to the cache on fetch failure", {
  missing_source <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  write_manual_rules_csv(cache_file)

  expect_warning(
    manual_rules <- load_manual_rules(
      url = manual_file_url(missing_source),
      cache_file = cache_file
    ),
    "Using cached manual rules"
  )

  expect_named(manual_rules, c("deployments", "recordings"))
})

test_that("load_manual_rules falls back to the cache on invalid remote CSV", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(table = "deployments", name = "missing_rule"), source_file)
  write_manual_rules_csv(cache_file)

  expect_warning(
    manual_rules <- load_manual_rules(
      url = manual_file_url(source_file),
      cache_file = cache_file
    ),
    "Using cached manual rules"
  )

  expect_named(manual_rules, c("deployments", "recordings"))
})

test_that("load_manual_rules errors when fetch and cache are unavailable", {
  missing_source <- tempfile(fileext = ".csv")
  missing_cache <- tempfile(fileext = ".csv")

  expect_error(
    load_manual_rules(
      url = manual_file_url(missing_source),
      cache_file = missing_cache
    ),
    "No cached manual rules were available"
  )
})

test_that("load_manual_rules validates CSV structure", {
  csv_file <- tempfile(fileext = ".csv")
  readr::write_csv(data.frame(table = "deployments", name = "missing_rule"), csv_file)

  expect_error(
    load_manual_rules(file = csv_file),
    "missing required columns"
  )
})

test_that("validate_submission uses manual_rules_file when manual_rules is NULL", {
  csv_file <- tempfile(fileext = ".csv")
  write_manual_rules_csv(csv_file)

  expect_true(validate_submission(
    tempdir(),
    tables = character(0),
    columns = list(),
    reference_tables = list(),
    manual_rules_file = csv_file,
    output_file = NULL,
    verbose = FALSE
  ))
})

test_that("validate_submission manual_rules override manual_rules_file", {
  missing_file <- tempfile(fileext = ".csv")

  expect_true(validate_submission(
    tempdir(),
    tables = character(0),
    columns = list(),
    reference_tables = list(),
    manual_rules = list(),
    manual_rules_file = missing_file,
    output_file = NULL,
    verbose = FALSE
  ))
})
