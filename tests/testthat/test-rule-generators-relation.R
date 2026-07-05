test_that("generate_format_rules relaxes code format for org_prefix_allowed columns", {
  columns <- tibble::tibble(
    table = "deployments",
    name = c("project_code", "site_code"),
    type = c("code", "code"),
    org_prefix_allowed = c(TRUE, FALSE)
  )

  rules <- generate_format_rules(columns)

  prefixed_rule <- rules$rule[rules$name == "format.project_code"]
  strict_rule <- rules$rule[rules$name == "format.site_code"]

  expect_match(prefixed_rule, "\\(ORG:\\)\\?|\\[A-Z0-9_-\\]\\+:\\)\\?", perl = TRUE)
  expect_match(strict_rule, 'formats\\[\\["code"\\]\\]', fixed = FALSE)
})

test_that("format.* rules accept an ORG: prefix on org_prefix_allowed columns and reject it otherwise", {
  columns <- tibble::tibble(
    table = "deployments",
    name = c("project_code", "site_code"),
    type = c("code", "code"),
    org_prefix_allowed = c(TRUE, FALSE)
  )
  rules_data <- generate_format_rules(columns)
  rules <- validate::validator(.data = rules_data)

  data <- tibble::tibble(
    project_code = "SEFSC:PROJ1",
    site_code = "SEFSC:SITE1"
  )

  results <- validate::confront(data, rules)
  summary <- validate::summary(results)

  expect_equal(summary$fails[summary$name == "format.project_code"], 0)
  expect_equal(summary$fails[summary$name == "format.site_code"], 1)
})

test_that("format.* rules still reject malformed codes on org_prefix_allowed columns", {
  columns <- tibble::tibble(
    table = "deployments",
    name = "project_code",
    type = "code",
    org_prefix_allowed = TRUE
  )
  rules_data <- generate_format_rules(columns)
  rules <- validate::validator(.data = rules_data)

  data <- tibble::tibble(project_code = "not a valid code!")

  results <- validate::confront(data, rules)
  summary <- validate::summary(results)

  expect_equal(summary$fails[summary$name == "format.project_code"], 1)
})

test_that("generate_format_rules relaxes code_list format for org_prefix_allowed columns", {
  columns <- tibble::tibble(
    table = "deployments",
    name = "deployment_device_codes",
    type = "code_list",
    org_prefix_allowed = TRUE
  )
  rules_data <- generate_format_rules(columns)
  rules <- validate::validator(.data = rules_data)

  data <- tibble::tibble(deployment_device_codes = "DEV1, SEFSC:DEV2")

  results <- validate::confront(data, rules)
  summary <- validate::summary(results)

  expect_equal(summary$fails[summary$name == "format.deployment_device_codes"], 0)
})
