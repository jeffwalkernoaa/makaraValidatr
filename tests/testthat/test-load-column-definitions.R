sample_column_definitions <- function() {
  data.frame(
    table = c("deployments", "deployments", "projects"),
    name = c("organization_code", "deployment_depth_m", "project_code"),
    type = c("code", "float", "code"),
    default = c(NA_character_, NA_character_, NA_character_),
    min = c(NA_real_, 0, NA_real_),
    max = c(NA_real_, 100, NA_real_),
    unique_by = c(NA_character_, NA_character_, "organization_code"),
    required = c(TRUE, NA, TRUE),
    required_unless = c(NA_character_, NA_character_, NA_character_),
    required_ncei = c(NA, TRUE, NA),
    reference_table = c("organizations", NA_character_, NA_character_),
    relation = c(NA_character_, NA_character_, NA_character_),
    org_prefix_allowed = c(NA, NA, NA),
    notes = c(NA_character_, "Example note", NA_character_)
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

  expect_named(columns, c("deployments", "projects"))
  expect_s3_class(columns$deployments, "data.frame")
  expect_equal(columns$deployments$name, c("organization_code", "deployment_depth_m"))
  expect_type(columns$deployments$min, "double")
  expect_type(columns$deployments$required, "logical")
  expect_equal(columns$deployments$required, c(TRUE, FALSE))
  expect_equal(columns$deployments$required_ncei, c(FALSE, TRUE))
  expect_equal(columns$deployments$org_prefix_allowed, c(FALSE, FALSE))
})

test_that("load_column_definitions retains an extra 'index' column if present", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data$index <- c(1L, 2L, 3L)
  write_column_definitions_csv(csv_file, data)

  columns <- load_column_definitions(file = csv_file)

  expect_named(columns, c("deployments", "projects"))
  expect_true("index" %in% names(columns$deployments))
})

test_that("load_column_definitions works without an 'index' column", {
  csv_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(csv_file)

  columns <- load_column_definitions(file = csv_file)

  expect_named(columns, c("deployments", "projects"))
  expect_false("index" %in% names(columns$deployments))
})

test_that("load_column_definitions drops PARS-only rows with empty table/name", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  pars_row <- data[1, ]
  pars_row$table <- NA_character_
  pars_row$name <- "monitoring_start_datetime"
  data <- rbind(data, pars_row)
  write_column_definitions_csv(csv_file, data)

  columns <- load_column_definitions(file = csv_file)

  expect_named(columns, c("deployments", "projects"))
  expect_false("monitoring_start_datetime" %in% columns$deployments$name)
})

test_that("load_column_definitions fetches CSV URLs and writes the cache", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  write_column_definitions_csv(source_file)

  columns <- load_column_definitions(
    url = column_file_url(source_file),
    cache_file = cache_file
  )

  expect_named(columns, c("deployments", "projects"))
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

  expect_named(columns, c("deployments", "projects"))
})

test_that("load_column_definitions falls back to the cache on invalid remote CSV", {
  source_file <- tempfile(fileext = ".csv")
  cache_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
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

  expect_named(columns, c("deployments", "projects"))
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

test_that("load_column_definitions requires the 'relation' column", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data$relation <- NULL
  write_column_definitions_csv(csv_file, data)

  expect_error(
    load_column_definitions(file = csv_file),
    "missing required columns"
  )
})

test_that("load_column_definitions requires the 'org_prefix_allowed' column", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data$org_prefix_allowed <- NULL
  write_column_definitions_csv(csv_file, data)

  expect_error(
    load_column_definitions(file = csv_file),
    "missing required columns"
  )
})

test_that("load_column_definitions fails fast on a relation to an unknown table", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data$relation[data$name == "project_code"] <- "not_a_table"
  write_column_definitions_csv(csv_file, data)

  expect_error(
    load_column_definitions(file = csv_file),
    "unknown table"
  )
})

test_that("load_column_definitions fails fast on a relation target without a unique_by key", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data$relation[data$name == "project_code"] <- "deployments"
  write_column_definitions_csv(csv_file, data)

  expect_error(
    load_column_definitions(file = csv_file),
    "unique_by"
  )
})

test_that("load_column_definitions fails fast when the child is missing organization_code", {
  csv_file <- tempfile(fileext = ".csv")
  data <- sample_column_definitions()
  data <- rbind(
    data,
    data.frame(
      table = "sites",
      name = "site_code",
      type = "code",
      default = NA_character_,
      min = NA_real_,
      max = NA_real_,
      unique_by = "organization_code",
      required = TRUE,
      required_unless = NA_character_,
      required_ncei = NA,
      reference_table = NA_character_,
      relation = NA_character_,
      org_prefix_allowed = NA,
      notes = NA_character_
    ),
    data.frame(
      table = "recordings",
      name = "site_code",
      type = "code",
      default = NA_character_,
      min = NA_real_,
      max = NA_real_,
      unique_by = NA_character_,
      required = TRUE,
      required_unless = NA_character_,
      required_ncei = NA,
      reference_table = NA_character_,
      relation = "sites",
      org_prefix_allowed = TRUE,
      notes = NA_character_
    )
  )
  write_column_definitions_csv(csv_file, data)

  expect_error(
    load_column_definitions(file = csv_file),
    "organization_code"
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
