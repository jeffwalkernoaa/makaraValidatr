withr_tempdir <- function() {
  dir <- tempfile("relations-")
  dir.create(dir)
  dir
}

test_relation_columns <- function() {
  list(
    projects = tibble::tibble(
      table = "projects",
      name = c("organization_code", "project_code"),
      type = c("code", "code"),
      unique_by = c(NA_character_, "organization_code"),
      relation = NA_character_,
      org_prefix_allowed = NA
    ),
    deployments = tibble::tibble(
      table = "deployments",
      name = c("organization_code", "deployment_code", "project_code"),
      type = c("code", "code", "code"),
      unique_by = c(NA_character_, "organization_code", NA_character_),
      relation = c(NA_character_, NA_character_, "projects"),
      org_prefix_allowed = c(NA, NA, TRUE)
    ),
    recordings = tibble::tibble(
      table = "recordings",
      name = c("organization_code", "deployment_code", "recording_code"),
      type = c("code", "code", "code"),
      unique_by = c(NA_character_, NA_character_, "organization_code,deployment_code"),
      relation = c(NA_character_, "deployments", NA_character_),
      org_prefix_allowed = c(NA, TRUE, NA)
    )
  )
}

write_csv_fixture <- function(dir, table, data) {
  readr::write_csv(data, file.path(dir, paste0(table, ".csv")), na = "")
}

test_that("validate_relations resolves submission-internal references without DB rows", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "projects", tibble::tibble(
    organization_code = "NEFSC",
    project_code = "PROJ1"
  ))
  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_relation_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("validate_relations resolves DB-only references", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_relation_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("validate_relations reports missing references with the exact error tibble shape", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "BADCODE"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_relation_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_named(result, c("table", "type", "row", "col", "value", "rule", "error"))
  expect_equal(result$table, "deployments")
  expect_equal(as.character(result$type), "relation")
  expect_equal(result$row, 1L)
  expect_equal(result$col, "project_code")
  expect_equal(result$value, "'BADCODE'")
  expect_equal(result$rule, "relation.project_code")
  expect_match(result$error, "unknown projects code 'BADCODE' in organization 'NEFSC'")
})

test_that("validate_relations resolves deployment-scoped references against the DB", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "recordings", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_code = "REC1"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("recordings"),
    columns = test_relation_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("validate_relations stops with an error when the DB connection fails", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  old_bq_project <- Sys.getenv("BQ_PROJECT", unset = NA)
  Sys.unsetenv("BQ_PROJECT")
  old_db_host <- Sys.getenv("DB_HOST", unset = NA)
  Sys.setenv(DB_HOST = "nonexistent-host-for-test")
  Sys.setenv(DB_PORT = "1")
  on.exit({
    if (is.na(old_bq_project)) Sys.unsetenv("BQ_PROJECT") else Sys.setenv("BQ_PROJECT" = old_bq_project)
    if (is.na(old_db_host)) Sys.unsetenv("DB_HOST") else Sys.setenv("DB_HOST" = old_db_host)
  })

  expect_error(
    validate_relations(
      data_dir,
      tables = c("deployments"),
      columns = test_relation_columns(),
      verbose = FALSE
    ),
    "Could not connect to the Makara database"
  )
})

test_that("validate_relations closes a connection it created itself but leaves an injected one open", {
  data_dir <- withr_tempdir()
  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_relation_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_true(DBI::dbIsValid(conn))
})

test_code_list_columns <- function() {
  list(
    devices = tibble::tibble(
      table = "devices",
      name = c("organization_code", "device_code"),
      type = c("code", "code"),
      unique_by = c(NA_character_, "organization_code"),
      relation = NA_character_,
      org_prefix_allowed = NA
    ),
    deployments = tibble::tibble(
      table = "deployments",
      name = c("organization_code", "deployment_code", "deployment_device_codes"),
      type = c("code", "code", "code_list"),
      unique_by = c(NA_character_, "organization_code", NA_character_),
      relation = c(NA_character_, NA_character_, "devices"),
      org_prefix_allowed = c(NA, NA, TRUE)
    )
  )
}

test_that("validate_relations resolves code_list references, including cross-org prefixes, against the DB", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    deployment_device_codes = "DEV1, SEFSC:DEV2"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_code_list_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})

test_that("validate_relations combines submission and DB keys for code_list references and flags unknowns", {
  data_dir <- withr_tempdir()

  write_csv_fixture(data_dir, "devices", tibble::tibble(
    organization_code = "NEFSC",
    device_code = "SUBDEV1"
  ))
  write_csv_fixture(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    deployment_device_codes = c("SUBDEV1,DEV1", "DEV1, BADDEV")
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- validate_relations(
    data_dir,
    tables = c("deployments"),
    columns = test_code_list_columns(),
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$table, "deployments")
  expect_equal(as.character(result$type), "relation")
  expect_equal(result$row, 2L)
  expect_equal(result$col, "deployment_device_codes")
  expect_equal(result$rule, "relation.deployment_device_codes")
  expect_match(result$error, "unknown devices codes \\('BADDEV'\\)")
})

test_that("validate_relations returns zero rows when no tables have relation columns", {
  data_dir <- withr_tempdir()
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  columns <- list(
    projects = tibble::tibble(
      table = "projects",
      name = c("organization_code", "project_code"),
      type = c("code", "code"),
      unique_by = c(NA_character_, "organization_code"),
      relation = NA_character_,
      org_prefix_allowed = NA
    )
  )

  result <- validate_relations(
    data_dir,
    columns = columns,
    conn = conn,
    verbose = FALSE
  )

  expect_equal(nrow(result), 0)
})
