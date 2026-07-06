test_that(".detect_backend selects BigQuery when BQ_PROJECT is set", {
  withr::local_envvar(BQ_PROJECT = "my-gcp-project")
  expect_equal(.detect_backend(), "bigquery")
})

test_that(".detect_backend falls back to PostgreSQL when BQ_PROJECT is unset", {
  withr::local_envvar(BQ_PROJECT = NA)
  expect_equal(.detect_backend(), "postgres")
})

test_that(".detect_backend treats an empty BQ_PROJECT as unset", {
  withr::local_envvar(BQ_PROJECT = "")
  expect_equal(.detect_backend(), "postgres")
})

test_that("makara_connect routes to PostgreSQL when BQ_PROJECT is unset", {
  # Point at an unreachable host so the (correct) backend surfaces its
  # informative Postgres connection error rather than actually connecting.
  withr::local_envvar(
    BQ_PROJECT = NA,
    DB_HOST = "nonexistent-host-for-test",
    DB_PORT = "1"
  )
  expect_error(makara_connect(), "Could not connect to the Makara database")
})

test_that("makara_connect routes to BigQuery when BQ_PROJECT is set", {
  # No dataset / no ADC, so the BigQuery path errors — but the error is
  # BigQuery-flavored, which is what confirms the dispatch (regardless of
  # whether 'bigrquery' happens to be installed).
  withr::local_envvar(BQ_PROJECT = "my-gcp-project", BQ_DATASET = NA)
  expect_error(makara_connect(), "BigQuery")
})

test_that("makara_connect rejects an unknown backend", {
  expect_error(makara_connect(backend = "sqlite"), "should be one of")
})

test_that("makara_bq_connect errors when no project is configured", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("bigrquery")
  expect_error(
    makara_bq_connect(project = "", dataset = "some_dataset"),
    "No BigQuery project specified"
  )
})

test_that("makara_bq_connect errors when no dataset is configured", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("bigrquery")
  expect_error(
    makara_bq_connect(project = "my-gcp-project", dataset = ""),
    "No BigQuery dataset specified"
  )
})

test_that("makara_bq_connect honors env var defaults", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("bigrquery")
  withr::local_envvar(
    BQ_PROJECT = "env-project",
    BQ_DATASET = "env_dataset",
    BQ_BILLING = NA
  )
  # bigrquery connections are lazy (no network until a query runs), so we can
  # confirm the function read the env vars into the connection without ADC.
  conn <- makara_bq_connect()
  # withr::defer (not on.exit) so we don't clobber local_envvar's own deferred
  # restoration registered on this frame.
  withr::defer(DBI::dbDisconnect(conn))

  expect_s4_class(conn, "BigQueryConnection")
  expect_equal(conn@project, "env-project")
  expect_equal(conn@dataset, "env_dataset")
  # billing defaults to project when BQ_BILLING is unset
  expect_equal(conn@billing, "env-project")
})

test_that("live BigQuery integration test", {
  skip_if(Sys.getenv("MAKARA_TEST_BQ") == "", "MAKARA_TEST_BQ not set; skipping live BigQuery test")
  skip_if_not_installed("bigrquery")

  conn <- makara_bq_connect()
  on.exit(DBI::dbDisconnect(conn))

  result <- .fetch_db_relation_keys(conn, "projects", "NEFSC")
  expect_true(is.data.frame(result))
  expect_named(result, c("org", "code"))
})
