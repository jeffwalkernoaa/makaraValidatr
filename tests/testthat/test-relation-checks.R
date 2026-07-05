deployments_columns <- function() {
  tibble::tibble(
    table = "deployments",
    name = c("organization_code", "deployment_code", "project_code"),
    type = c("code", "code", "code"),
    unique_by = c(NA_character_, "organization_code", NA_character_),
    relation = c(NA_character_, NA_character_, "projects"),
    org_prefix_allowed = c(NA, NA, TRUE)
  )
}

projects_columns <- function() {
  tibble::tibble(
    table = "projects",
    name = c("organization_code", "project_code"),
    type = c("code", "code"),
    unique_by = c(NA_character_, "organization_code"),
    relation = c(NA_character_, NA_character_),
    org_prefix_allowed = c(NA, NA)
  )
}

recordings_columns <- function() {
  tibble::tibble(
    table = "recordings",
    name = c("organization_code", "deployment_code", "recording_code"),
    type = c("code", "code", "code"),
    unique_by = c(NA_character_, NA_character_, "organization_code,deployment_code"),
    relation = c(NA_character_, "deployments", NA_character_),
    org_prefix_allowed = c(NA, TRUE, NA)
  )
}

analyses_columns <- function() {
  tibble::tibble(
    table = "analyses",
    name = c("organization_code", "deployment_code", "analysis_code", "recording_codes"),
    type = c("code", "code", "code", "code_list"),
    unique_by = c(NA_character_, NA_character_, "organization_code,deployment_code", NA_character_),
    relation = c(NA_character_, "deployments", NA_character_, "recordings"),
    org_prefix_allowed = c(NA, TRUE, NA, NA)
  )
}

test_that(".relation_columns returns only rows with a non-blank relation", {
  result <- .relation_columns(deployments_columns())

  expect_equal(result$name, "project_code")
})

test_that(".relation_scope derives org scope from unique_by arity 1", {
  scope <- .relation_scope(projects_columns(), "projects")

  expect_equal(scope$scope, "org")
  expect_equal(scope$key_col, "project_code")
})

test_that(".relation_scope derives deployment scope from unique_by arity 2", {
  scope <- .relation_scope(recordings_columns(), "recordings")

  expect_equal(scope$scope, "deployment")
  expect_equal(scope$key_col, "recording_code")
})

test_that(".relation_scope special-cases analyses regardless of arity", {
  scope <- .relation_scope(analyses_columns(), "analyses")

  expect_equal(scope$scope, "analyses")
  expect_equal(scope$key_col, "analysis_code")
})

test_that(".relation_scope errors when parent has no unique key column", {
  bad_columns <- projects_columns()
  bad_columns$unique_by <- NA_character_

  expect_error(.relation_scope(bad_columns, "projects"), "unique_by")
})

test_that(".build_submission_keys builds org-scoped keys", {
  data <- tibble::tibble(
    organization_code = c("NEFSC", "SEFSC"),
    project_code = c("PROJ1", "PROJ2")
  )

  keys <- .build_submission_keys(data, "org", "project_code")

  expect_setequal(keys, c("NEFSC|PROJ1", "SEFSC|PROJ2"))
})

test_that(".build_submission_keys builds deployment-scoped keys", {
  data <- tibble::tibble(
    organization_code = c("NEFSC", "NEFSC"),
    deployment_code = c("DEP1", "SEFSC:DEP2"),
    recording_code = c("REC1", "REC2")
  )

  keys <- .build_submission_keys(data, "deployment", "recording_code")

  expect_setequal(keys, c("NEFSC|DEP1|REC1", "SEFSC|DEP2|REC2"))
})

test_that(".build_submission_keys builds analyses composite keys", {
  data <- tibble::tibble(
    organization_code = c("NEFSC", "SEFSC"),
    deployment_code = c("DEP1", "NEFSC:DEP2"),
    analysis_code = c("AN1", "AN2")
  )

  keys <- .build_submission_keys(data, "analyses", "analysis_code")

  expect_setequal(keys, c("NEFSC|DEP1|NEFSC|AN1", "NEFSC|DEP2|SEFSC|AN2"))
})

test_that(".build_submission_keys returns empty for NULL/empty data", {
  expect_equal(.build_submission_keys(NULL, "org", "project_code"), character(0))
  expect_equal(
    .build_submission_keys(tibble::tibble(), "org", "project_code"),
    character(0)
  )
})

test_that(".combine_keys unions submission and DB keys", {
  result <- .combine_keys(c("A|1", "A|2"), c("A|2", "B|3"))

  expect_setequal(result, c("A|1", "A|2", "B|3"))
})

test_that(".collect_org_codes gathers org codes and deployment-code prefixes", {
  data_list <- list(
    deployments = tibble::tibble(
      organization_code = c("NEFSC", "SEFSC"),
      deployment_code = c("DEP1", "DEP2")
    ),
    recordings = tibble::tibble(
      organization_code = c("NEFSC"),
      deployment_code = c("SEFSC:DEP2")
    )
  )

  result <- .collect_org_codes(data_list)

  expect_setequal(result, c("NEFSC", "SEFSC"))
})

test_that(".collect_org_codes gathers ORG: prefixes from relation values, including code_list items", {
  data_list <- list(
    deployments = tibble::tibble(
      organization_code = "NEFSC",
      deployment_code = "DEP1",
      project_code = "SWFSC:PROJ9",
      deployment_device_codes = "DEV1, SEFSC:DEV2"
    )
  )
  relation_columns <- tibble::tibble(
    table = c("deployments", "deployments"),
    name = c("project_code", "deployment_device_codes"),
    type = c("code", "code_list")
  )

  result <- .collect_org_codes(data_list, relation_columns)

  expect_setequal(result, c("NEFSC", "SWFSC", "SEFSC"))
})

# --- check_relation_column: org scope --------------------------------------

test_that("check_relation_column finds a hit for a valid org-scoped bare code", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    project_code = "PROJ1"
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(
    data, "deployments", col_def, "org", keys = "NEFSC|PROJ1"
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags a miss for an org-scoped bare code", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    project_code = "BADCODE"
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(
    data, "deployments", col_def, "org", keys = "NEFSC|PROJ1"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$row, 1L)
  expect_equal(result$col, "project_code")
  expect_equal(result$rule, "relation.project_code")
  expect_match(result$error, "unknown projects code 'BADCODE' in organization 'NEFSC'")
})

test_that("check_relation_column resolves a cross-org ORG:CODE prefix", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    project_code = "SEFSC:PROJ1"
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(
    data, "deployments", col_def, "org", keys = "SEFSC|PROJ1"
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags a miss for a cross-org prefix pointing nowhere", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    project_code = "SEFSC:BADCODE"
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(
    data, "deployments", col_def, "org", keys = "SEFSC|PROJ1"
  )

  expect_equal(nrow(result), 1)
  expect_match(result$error, "unknown projects code 'BADCODE' in organization 'SEFSC'")
})

test_that("check_relation_column skips NA/blank values", {
  data <- tibble::tibble(
    row = c(1L, 2L),
    organization_code = c("NEFSC", "NEFSC"),
    project_code = c(NA_character_, "")
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(data, "deployments", col_def, "org", keys = character(0))

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags malformed org-scoped values as relation errors, not R errors", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    project_code = "A:B:C"
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(data, "deployments", col_def, "org", keys = character(0))

  expect_equal(nrow(result), 1)
  expect_match(result$error, "malformed")
})

# --- check_relation_column: deployment scope --------------------------------

test_that("check_relation_column resolves deployment-scoped children with a prefixed deployment_code", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "SEFSC:DEP1",
    recording_code = "REC1"
  )
  col_def <- list(name = "recording_code", type = "code", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment", keys = "SEFSC|DEP1|REC1"
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags a miss for deployment-scoped children", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_code = "BADCODE"
  )
  col_def <- list(name = "recording_code", type = "code", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment", keys = "NEFSC|DEP1|REC1"
  )

  expect_equal(nrow(result), 1)
  expect_match(result$error, "unknown recordings code 'BADCODE' in deployment 'DEP1' of organization 'NEFSC'")
})

test_that("check_relation_column rejects an org-prefixed deployment-scoped child code", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_code = "SEFSC:REC1"
  )
  col_def <- list(name = "recording_code", type = "code", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment", keys = "NEFSC|DEP1|REC1"
  )

  expect_equal(nrow(result), 1)
  expect_match(result$error, "must not carry an organization prefix")
})

test_that("check_relation_column: self-references are satisfied via the submission's own rows", {
  data <- tibble::tibble(
    row = c(1L, 2L),
    organization_code = c("NEFSC", "NEFSC"),
    deployment_code = c(NA_character_, NA_character_),
    parent_deployment_code = c(NA_character_, "DEP1")
  )
  parent_keys <- .build_submission_keys(
    tibble::tibble(organization_code = "NEFSC", deployment_code = "DEP1"),
    "org", "deployment_code"
  )
  col_def <- list(name = "parent_deployment_code", type = "code", relation = "deployments")

  result <- check_relation_column(data, "deployments", col_def, "org", keys = parent_keys)

  expect_equal(nrow(result), 0)
})

# --- check_relation_column: analyses composite scope ------------------------

test_that("check_relation_column resolves the analyses composite key", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    analysis_code = "AN1"
  )
  col_def <- list(name = "analysis_code", type = "code", relation = "analyses")

  result <- check_relation_column(
    data, "detections", col_def, "analyses", keys = "NEFSC|DEP1|NEFSC|AN1"
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags a miss for the analyses composite key", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    analysis_code = "BADCODE"
  )
  col_def <- list(name = "analysis_code", type = "code", relation = "analyses")

  result <- check_relation_column(
    data, "detections", col_def, "analyses", keys = "NEFSC|DEP1|NEFSC|AN1"
  )

  expect_equal(nrow(result), 1)
  expect_match(
    result$error,
    "unknown analyses analysis code 'BADCODE' for deployment 'DEP1' of organization 'NEFSC' from organization 'NEFSC'"
  )
})

test_that("check_relation_column distinguishes analysis org from deployment org", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "SEFSC",
    deployment_code = "NEFSC:DEP1",
    analysis_code = "AN1"
  )
  col_def <- list(name = "analysis_code", type = "code", relation = "analyses")

  # key exists for NEFSC deployment / NEFSC analysis org but this row's
  # analysis org is SEFSC, so it should not match
  result <- check_relation_column(
    data, "detections", col_def, "analyses", keys = "NEFSC|DEP1|NEFSC|AN1"
  )

  expect_equal(nrow(result), 1)
})

# --- check_relation_column: code_list ---------------------------------------

test_that("check_relation_column handles code_list with mixed valid/invalid codes", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_codes = "REC1, BADCODE, REC2"
  )
  col_def <- list(name = "recording_codes", type = "code_list", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment",
    keys = c("NEFSC|DEP1|REC1", "NEFSC|DEP1|REC2")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$row, 1L)
  expect_match(result$error, "unknown recordings codes \\('BADCODE'\\)")
})

test_that("check_relation_column code_list passes when all codes are valid", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_codes = "REC1,REC2"
  )
  col_def <- list(name = "recording_codes", type = "code_list", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment",
    keys = c("NEFSC|DEP1|REC1", "NEFSC|DEP1|REC2")
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column code_list lists all unknown codes in one error row", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_codes = "BAD1,BAD2"
  )
  col_def <- list(name = "recording_codes", type = "code_list", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment", keys = character(0)
  )

  expect_equal(nrow(result), 1)
  expect_match(result$error, "BAD1")
  expect_match(result$error, "BAD2")
})

test_that("check_relation_column resolves org-scoped code_list items with cross-org prefixes", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_device_codes = "DEV1, SEFSC:DEV2"
  )
  col_def <- list(name = "deployment_device_codes", type = "code_list", relation = "devices")

  result <- check_relation_column(
    data, "deployments", col_def, "org",
    keys = c("NEFSC|DEV1", "SEFSC|DEV2")
  )

  expect_equal(nrow(result), 0)
})

test_that("check_relation_column flags an unknown org-prefixed item in an org-scoped code_list", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_device_codes = "DEV1, SEFSC:BADDEV"
  )
  col_def <- list(name = "deployment_device_codes", type = "code_list", relation = "devices")

  result <- check_relation_column(
    data, "deployments", col_def, "org",
    keys = c("NEFSC|DEV1", "SEFSC|DEV2")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$rule, "relation.deployment_device_codes")
  expect_match(result$error, "unknown devices codes \\('BADDEV'\\)")
})

test_that("check_relation_column flags a malformed item in an org-scoped code_list without masking valid items", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_device_codes = "DEV1,A:B:C"
  )
  col_def <- list(name = "deployment_device_codes", type = "code_list", relation = "devices")

  result <- check_relation_column(
    data, "deployments", col_def, "org", keys = "NEFSC|DEV1"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$value, "'A:B:C'")
  expect_match(result$error, "malformed")
})

test_that("check_relation_column rejects org prefixes on items in a deployment-scoped code_list", {
  data <- tibble::tibble(
    row = 1L,
    organization_code = "NEFSC",
    deployment_code = "DEP1",
    recording_codes = "REC1,SEFSC:REC2"
  )
  col_def <- list(name = "recording_codes", type = "code_list", relation = "recordings")

  result <- check_relation_column(
    data, "analyses", col_def, "deployment",
    keys = c("NEFSC|DEP1|REC1", "SEFSC|DEP1|REC2")
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$value, "'SEFSC:REC2'")
  expect_match(result$error, "must not carry an organization prefix")
})

test_that("check_relation_column returns zero rows with zero DB/file I/O for a clean table", {
  data <- tibble::tibble(
    row = 1:3,
    organization_code = rep("NEFSC", 3),
    project_code = c("PROJ1", "PROJ2", "PROJ3")
  )
  col_def <- list(name = "project_code", type = "code", relation = "projects")

  result <- check_relation_column(
    data, "deployments", col_def, "org",
    keys = c("NEFSC|PROJ1", "NEFSC|PROJ2", "NEFSC|PROJ3")
  )

  expect_equal(nrow(result), 0)
})
