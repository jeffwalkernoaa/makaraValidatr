test_that("split_org_code handles a bare code (no prefix)", {
  result <- split_org_code("UXS_MHI", "NEFSC")

  expect_equal(result$org, "NEFSC")
  expect_equal(result$code, "UXS_MHI")
  expect_true(is.na(result$malformed_reason))
})

test_that("split_org_code handles an ORG:CODE prefix", {
  result <- split_org_code("SEFSC:UXS_MHI", "NEFSC")

  expect_equal(result$org, "SEFSC")
  expect_equal(result$code, "UXS_MHI")
  expect_true(is.na(result$malformed_reason))
})

test_that("split_org_code trims whitespace around parts", {
  result <- split_org_code("  SEFSC : UXS_MHI  ", "NEFSC")

  expect_equal(result$org, "SEFSC")
  expect_equal(result$code, "UXS_MHI")
  expect_true(is.na(result$malformed_reason))
})

test_that("split_org_code treats NA as passthrough (not malformed)", {
  result <- split_org_code(NA_character_, "NEFSC")

  expect_true(is.na(result$org))
  expect_true(is.na(result$code))
  expect_true(is.na(result$malformed_reason))
})

test_that("split_org_code treats blank/whitespace-only as passthrough (not malformed)", {
  result <- split_org_code("   ", "NEFSC")

  expect_true(is.na(result$org))
  expect_true(is.na(result$code))
  expect_true(is.na(result$malformed_reason))

  result_empty <- split_org_code("", "NEFSC")
  expect_true(is.na(result_empty$org))
  expect_true(is.na(result_empty$code))
  expect_true(is.na(result_empty$malformed_reason))
})

test_that("split_org_code flags multiple colons as malformed", {
  result <- split_org_code("A:B:C", "NEFSC")

  expect_true(is.na(result$org))
  expect_true(is.na(result$code))
  expect_false(is.na(result$malformed_reason))
})

test_that("split_org_code flags an empty org prefix as malformed", {
  result <- split_org_code(":CODE", "NEFSC")

  expect_true(is.na(result$org))
  expect_true(is.na(result$code))
  expect_false(is.na(result$malformed_reason))
})

test_that("split_org_code flags an empty code after the colon as malformed", {
  result <- split_org_code("ORG:", "NEFSC")

  expect_true(is.na(result$org))
  expect_true(is.na(result$code))
  expect_false(is.na(result$malformed_reason))
})

test_that("split_org_code is vectorized and recycles default_org", {
  result <- split_org_code(
    c("UXS_MHI", "SEFSC:UXS_MHI", NA, "A:B:C"),
    "NEFSC"
  )

  expect_equal(nrow(result), 4)
  expect_equal(result$org, c("NEFSC", "SEFSC", NA, NA))
  expect_equal(result$code, c("UXS_MHI", "UXS_MHI", NA, NA))
  expect_equal(is.na(result$malformed_reason), c(TRUE, TRUE, TRUE, FALSE))
})

test_that("split_org_code recycles a vector of default_org values", {
  result <- split_org_code(
    c("CODE1", "CODE2"),
    c("NEFSC", "SEFSC")
  )

  expect_equal(result$org, c("NEFSC", "SEFSC"))
  expect_equal(result$code, c("CODE1", "CODE2"))
})
