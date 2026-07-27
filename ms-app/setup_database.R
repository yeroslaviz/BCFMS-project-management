#!/usr/bin/env Rscript

library(DBI)
library(RSQLite)
library(digest)

source("db_schema.R")

reset_requested <- Sys.getenv("MS_RESET_DB", "0") != "0"
restore_from <- trimws(Sys.getenv("MS_RESTORE_DB_FROM", ""))

if (nzchar(restore_from)) {
  if (!file.exists(restore_from)) {
    stop("MS_RESTORE_DB_FROM does not exist: ", restore_from)
  }
  target_path <- ms_db_path()
  if (identical(
    normalizePath(restore_from, mustWork = TRUE),
    normalizePath(target_path, mustWork = FALSE)
  )) {
    stop("MS_RESTORE_DB_FROM must point to a separate backup file, not the target database itself.")
  }
  if (file.exists(target_path) && !isTRUE(reset_requested)) {
    stop("The target database already exists. Set MS_RESET_DB=1 only after taking a verified backup.")
  }
  if (file.exists(target_path) && !file.remove(target_path)) {
    stop("Could not replace the existing target database: ", target_path)
  }
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  restore_database <- function() {
    source_con <- dbConnect(SQLite(), restore_from)
    on.exit(dbDisconnect(source_con), add = TRUE)
    target_con <- dbConnect(SQLite(), target_path)
    on.exit(dbDisconnect(target_con), add = TRUE)
    RSQLite::sqliteCopyDatabase(source_con, target_con)
  }
  restore_database()
  db_path <- ms_initialize_database(reset = FALSE)
  message("Database restored from verified backup: ", restore_from)
} else {
  db_path <- ms_initialize_database(reset = reset_requested)
}

message("Mass Spectrometry project database initialized: ", db_path)
message("Local test login: admin / ", Sys.getenv("MS_LOCAL_ADMIN_PASSWORD", "admin123"))
message("Facility email fallback: ", MS_FACILITY_EMAIL)
message("Upload pool root: ", MS_UPLOAD_ROOT)
message("MS_RESET_DB defaults to 0; existing data is preserved unless an explicit reset is requested.")
message("For an exact rebuild, set MS_RESTORE_DB_FROM to a validated SQLite backup and MS_RESET_DB=1.")
