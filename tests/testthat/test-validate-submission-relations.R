wiring_test_columns <- function() {
  list(
    deployments = tibble::tibble(
      table = "deployments",
      name = c("organization_code", "deployment_code", "project_code"),
      pars_table = NA_character_,
      pars_name = NA_character_,
      type = c("code", "code", "code"),
      default = NA_character_,
      min = NA_real_,
      max = NA_real_,
      unique_by = c(NA_character_, "organization_code", NA_character_),
      required = c(TRUE, TRUE, TRUE),
      required_unless = NA_character_,
      required_ncei = NA,
      reference_table = c("organizations", NA_character_, NA_character_),
      relation = c(NA_character_, NA_character_, "projects"),
      org_prefix_allowed = c(NA, NA, TRUE),
      notes = NA_character_
    ),
    projects = tibble::tibble(
      table = "projects",
      name = c("organization_code", "project_code"),
      pars_table = NA_character_,
      pars_name = NA_character_,
      type = c("code", "code"),
      default = NA_character_,
      min = NA_real_,
      max = NA_real_,
      unique_by = c(NA_character_, "organization_code"),
      required = c(TRUE, TRUE),
      required_unless = NA_character_,
      required_ncei = NA,
      reference_table = c("organizations", NA_character_),
      relation = NA_character_,
      org_prefix_allowed = NA,
      notes = NA_character_
    )
  )
}

write_wiring_csv <- function(dir, table, data) {
  readr::write_csv(data, file.path(dir, paste0(table, ".csv")), na = "")
}

test_that("validate_submission default behavior is unchanged when relations is FALSE", {
  data_dir <- tempfile("wiring-")
  dir.create(data_dir)
  write_wiring_csv(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "BADCODE"
  ))

  success <- validate_submission(
    data_dir,
    tables = c("deployments"),
    columns = wiring_test_columns(),
    reference_tables = list(organizations = data.frame(code = "NEFSC", name = "NEFSC")),
    manual_rules = list(),
    output_file = NULL,
    verbose = FALSE
  )

  # BADCODE is not checked by default (relations = FALSE): only the
  # relation-blind rules run, so this should pass.
  expect_true(success)
})

test_that("validate_submission(relations = TRUE) merges relation errors into the result", {
  data_dir <- tempfile("wiring-")
  dir.create(data_dir)
  write_wiring_csv(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "BADCODE"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  output_file <- file.path(data_dir, "validation-errors.csv")
  success <- validate_submission(
    data_dir,
    tables = c("deployments"),
    columns = wiring_test_columns(),
    reference_tables = list(organizations = data.frame(code = "NEFSC", name = "NEFSC")),
    manual_rules = list(),
    output_file = output_file,
    verbose = FALSE,
    relations = TRUE,
    conn = conn
  )

  expect_false(success)
  expect_true(file.exists(output_file))

  errors <- readr::read_csv(output_file, show_col_types = FALSE)
  relation_errors <- errors[errors$type == "relation", ]
  expect_equal(nrow(relation_errors), 1)
  expect_equal(relation_errors$col, "project_code")
  expect_match(relation_errors$error, "unknown projects code 'BADCODE'")
})

test_that("validate_relations errors clearly when a relation target's columns are missing", {
  data_dir <- tempfile("wiring-")
  dir.create(data_dir)
  write_wiring_csv(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  columns <- wiring_test_columns()
  columns$projects <- NULL

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  expect_error(
    validate_relations(
      data_dir,
      tables = c("deployments"),
      columns = columns,
      conn = conn,
      verbose = FALSE
    ),
    "no column definitions were provided"
  )
})

test_that("validate_submission(relations = TRUE) succeeds when relations resolve", {
  data_dir <- tempfile("wiring-")
  dir.create(data_dir)
  write_wiring_csv(data_dir, "deployments", tibble::tibble(
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    project_code = "PROJ1"
  ))

  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  success <- validate_submission(
    data_dir,
    tables = c("deployments"),
    columns = wiring_test_columns(),
    reference_tables = list(organizations = data.frame(code = "NEFSC", name = "NEFSC")),
    manual_rules = list(),
    output_file = NULL,
    verbose = FALSE,
    relations = TRUE,
    conn = conn
  )

  expect_true(success)
})
