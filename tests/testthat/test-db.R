test_that("makara_db_connect honors env var defaults", {
  withr_env <- c(
    DB_NAME = "test_db",
    DB_HOST = "test_host",
    DB_PORT = "5433",
    DB_USER = "test_user",
    DB_PASSWORD = "test_pass"
  )
  old <- Sys.getenv(names(withr_env), unset = NA)
  do.call(Sys.setenv, as.list(withr_env))
  on.exit({
    for (i in seq_along(old)) {
      if (is.na(old[i])) {
        Sys.unsetenv(names(old)[i])
      } else {
        do.call(Sys.setenv, stats::setNames(list(old[i]), names(old)[i]))
      }
    }
  })

  # We can't actually connect without a live Postgres server, but we can
  # confirm the function reads the env vars into its defaults and attempts
  # a connection with them (surfacing our informative error).
  expect_error(
    makara_db_connect(),
    "test_user@test_host:5433/test_db"
  )
})

test_that("makara_db_connect gives an informative error on failed connection", {
  expect_error(
    makara_db_connect(host = "nonexistent-host-for-test", port = "1"),
    "Could not connect to the Makara database"
  )
})

test_that(".fetch_db_relation_keys returns org-scoped keys filtered by organization", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "projects", "NEFSC")

  expect_equal(nrow(result), 1)
  expect_equal(result$org, "NEFSC")
  expect_equal(result$code, "PROJ1")
})

test_that(".fetch_db_relation_keys returns org-scoped keys for multiple organizations", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "projects", c("NEFSC", "SEFSC"))

  expect_setequal(paste(result$org, result$code), c("NEFSC PROJ1", "SEFSC PROJ2"))
})

test_that(".fetch_db_relation_keys works for all org-scoped parent tables", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  for (table in c("projects", "sites", "devices", "deployments", "citations")) {
    result <- .fetch_db_relation_keys(conn, table, c("NEFSC", "SEFSC"))
    expect_equal(nrow(result), 2, info = table)
    expect_named(result, c("org", "code"))
  }
})

test_that(".fetch_db_relation_keys returns deployment-scoped keys via deployments join", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "recordings", "NEFSC")

  expect_equal(nrow(result), 1)
  expect_equal(result$dep_org, "NEFSC")
  expect_equal(result$dep_code, "DEP1")
  expect_equal(result$code, "REC1")
})

test_that(".fetch_db_relation_keys works for all deployment-scoped parent tables", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  for (table in c("recordings", "tracks", "sensor_datasets")) {
    result <- .fetch_db_relation_keys(conn, table, c("NEFSC", "SEFSC"))
    expect_equal(nrow(result), 2, info = table)
    expect_named(result, c("dep_org", "dep_code", "code"))
  }
})

test_that(".fetch_db_relation_keys returns the analyses composite key", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "analyses", c("NEFSC", "SEFSC"))

  expect_named(result, c("dep_org", "dep_code", "analysis_org", "code"))
  combos <- paste(result$dep_org, result$dep_code, result$analysis_org, result$code)
  expect_setequal(
    combos,
    c("NEFSC DEP1 NEFSC AN1", "NEFSC DEP1 SEFSC AN2", "SEFSC DEP2 SEFSC AN3")
  )
})

test_that(".fetch_db_relation_keys never queries excluded tables", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  for (table in c("detections", "recording_intervals", "track_positions", "sensor_values")) {
    expect_error(.fetch_db_relation_keys(conn, table, "NEFSC"), "Unsupported relation parent table")
  }
})

test_that(".fetch_db_relation_keys returns an empty result for empty org_codes without querying", {
  conn <- seed_makara_test_db()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "projects", character(0))
  expect_equal(nrow(result), 0)
  expect_named(result, c("org", "code"))

  result_dep <- .fetch_db_relation_keys(conn, "recordings", character(0))
  expect_equal(nrow(result_dep), 0)
  expect_named(result_dep, c("dep_org", "dep_code", "code"))

  result_analyses <- .fetch_db_relation_keys(conn, "analyses", character(0))
  expect_equal(nrow(result_analyses), 0)
  expect_named(result_analyses, c("dep_org", "dep_code", "analysis_org", "code"))
})

test_that("live PostgreSQL integration test", {
  skip_if(Sys.getenv("MAKARA_TEST_DB") == "", "MAKARA_TEST_DB not set; skipping live DB test")

  conn <- makara_db_connect()
  on.exit(DBI::dbDisconnect(conn))

  expect_s4_class(conn, "PqConnection")

  result <- .fetch_db_relation_keys(conn, "projects", "NEFSC")
  expect_true(is.data.frame(result))
})
