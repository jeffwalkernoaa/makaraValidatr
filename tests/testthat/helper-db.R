#' Create an in-memory RSQLite connection seeded with a minimal Makara-like
#' schema, covering all relation parent tables:
#' projects, sites, devices, deployments, citations (org-scoped),
#' recordings, tracks, sensor_datasets (deployment-scoped), and analyses
#' (composite deployment + analysis-org scoped).
seed_makara_test_db <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbExecute(conn, "
    CREATE TABLE deployments (
      id INTEGER PRIMARY KEY,
      organization_code TEXT,
      deployment_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE projects (
      id INTEGER PRIMARY KEY,
      organization_code TEXT,
      project_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE sites (
      id INTEGER PRIMARY KEY,
      organization_code TEXT,
      site_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE devices (
      id INTEGER PRIMARY KEY,
      organization_code TEXT,
      device_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE citations (
      id INTEGER PRIMARY KEY,
      organization_code TEXT,
      citation_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE recordings (
      id INTEGER PRIMARY KEY,
      deployment_id INTEGER,
      recording_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE tracks (
      id INTEGER PRIMARY KEY,
      deployment_id INTEGER,
      track_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE sensor_datasets (
      id INTEGER PRIMARY KEY,
      deployment_id INTEGER,
      sensor_dataset_code TEXT
    )
  ")
  DBI::dbExecute(conn, "
    CREATE TABLE analyses (
      id INTEGER PRIMARY KEY,
      deployment_id INTEGER,
      organization_code TEXT,
      analysis_code TEXT
    )
  ")

  # deployments: NEFSC:DEP1 (id=1), SEFSC:DEP2 (id=2)
  DBI::dbExecute(conn, "
    INSERT INTO deployments (id, organization_code, deployment_code) VALUES
    (1, 'NEFSC', 'DEP1'),
    (2, 'SEFSC', 'DEP2')
  ")

  DBI::dbExecute(conn, "
    INSERT INTO projects (organization_code, project_code) VALUES
    ('NEFSC', 'PROJ1'),
    ('SEFSC', 'PROJ2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO sites (organization_code, site_code) VALUES
    ('NEFSC', 'SITE1'),
    ('SEFSC', 'SITE2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO devices (organization_code, device_code) VALUES
    ('NEFSC', 'DEV1'),
    ('SEFSC', 'DEV2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO citations (organization_code, citation_code) VALUES
    ('NEFSC', 'CIT1'),
    ('SEFSC', 'CIT2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO recordings (deployment_id, recording_code) VALUES
    (1, 'REC1'),
    (2, 'REC2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO tracks (deployment_id, track_code) VALUES
    (1, 'TRK1'),
    (2, 'TRK2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO sensor_datasets (deployment_id, sensor_dataset_code) VALUES
    (1, 'SDS1'),
    (2, 'SDS2')
  ")
  DBI::dbExecute(conn, "
    INSERT INTO analyses (deployment_id, organization_code, analysis_code) VALUES
    (1, 'NEFSC', 'AN1'),
    (1, 'SEFSC', 'AN2'),
    (2, 'SEFSC', 'AN3')
  ")

  conn
}
