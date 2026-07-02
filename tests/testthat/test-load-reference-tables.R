sample_reference_json <- function() {
  jsonlite::toJSON(
    list(
      list(
        table = "animals",
        data = list(
          list(code = "WHALE", name = "Whale"),
          list(code = "DOLPHIN", name = "Dolphin")
        )
      ),
      list(
        table = "platform_types",
        data = list(
          list(code = "VESSEL", name = "Vessel")
        )
      )
    ),
    auto_unbox = TRUE
  )
}

write_reference_json <- function(path, json = sample_reference_json()) {
  writeLines(json, path, useBytes = TRUE)
  path
}

file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))
}

test_that("load_reference_tables parses local JSON files", {
  json_file <- tempfile(fileext = ".json")
  write_reference_json(json_file)

  reference_tables <- load_reference_tables(file = json_file)

  expect_named(reference_tables, c("animals", "platform_types"))
  expect_s3_class(reference_tables$animals, "data.frame")
  expect_equal(reference_tables$animals$code, c("WHALE", "DOLPHIN"))
  expect_equal(reference_tables$animals$name, c("Whale", "Dolphin"))
})

test_that("load_reference_tables keeps local CSV compatibility", {
  csv_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      table = c("animals", "animals", "platform_types"),
      code = c("WHALE", "DOLPHIN", "VESSEL"),
      name = c("Whale", "Dolphin", "Vessel")
    ),
    csv_file
  )

  reference_tables <- load_reference_tables(file = csv_file)

  expect_named(reference_tables, c("animals", "platform_types"))
  expect_equal(reference_tables$animals$code, c("WHALE", "DOLPHIN"))
})

test_that("load_reference_tables fetches JSON URLs and writes the cache", {
  source_file <- tempfile(fileext = ".json")
  cache_file <- tempfile(fileext = ".json")
  write_reference_json(source_file)

  reference_tables <- load_reference_tables(
    url = file_url(source_file),
    cache_file = cache_file
  )

  expect_named(reference_tables, c("animals", "platform_types"))
  expect_true(file.exists(cache_file))
  expect_equal(
    paste(readLines(cache_file, warn = FALSE), collapse = "\n"),
    as.character(sample_reference_json())
  )
})

test_that("load_reference_tables falls back to the cache on fetch failure", {
  missing_source <- tempfile(fileext = ".json")
  cache_file <- tempfile(fileext = ".json")
  write_reference_json(cache_file)

  expect_warning(
    reference_tables <- load_reference_tables(
      url = file_url(missing_source),
      cache_file = cache_file
    ),
    "Using cached reference tables"
  )

  expect_named(reference_tables, c("animals", "platform_types"))
  expect_equal(reference_tables$platform_types$code, "VESSEL")
})

test_that("load_reference_tables falls back to the cache on invalid remote JSON", {
  source_file <- tempfile(fileext = ".json")
  cache_file <- tempfile(fileext = ".json")
  write_reference_json(source_file, '{"not":"the expected shape"}')
  write_reference_json(cache_file)

  expect_warning(
    reference_tables <- load_reference_tables(
      url = file_url(source_file),
      cache_file = cache_file
    ),
    "Using cached reference tables"
  )

  expect_named(reference_tables, c("animals", "platform_types"))
})

test_that("load_reference_tables errors when fetch and cache are unavailable", {
  missing_source <- tempfile(fileext = ".json")
  missing_cache <- tempfile(fileext = ".json")

  expect_error(
    load_reference_tables(
      url = file_url(missing_source),
      cache_file = missing_cache
    ),
    "No cached reference tables were available"
  )
})

test_that("load_reference_tables validates JSON structure", {
  json_file <- tempfile(fileext = ".json")
  write_reference_json(
    json_file,
    '[{"table":"animals","data":[{"code":"WHALE"}]}]'
  )

  expect_error(
    load_reference_tables(file = json_file),
    "missing required columns: name"
  )
})
