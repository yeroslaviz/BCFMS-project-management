#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
source_db <- if (length(args) >= 1) args[[1]] else Sys.getenv("MS_DB_FILE", "ms-app/ms_projects.db")
backup_file <- if (length(args) >= 2) {
  args[[2]]
} else {
  file.path("backups", paste0("ms_projects_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite"))
}

if (!file.exists(source_db)) stop("Source database does not exist: ", source_db)
dir.create(dirname(backup_file), recursive = TRUE, showWarnings = FALSE)
if (file.exists(backup_file)) stop("Backup destination already exists: ", backup_file)

source_con <- dbConnect(SQLite(), source_db)
on.exit(dbDisconnect(source_con), add = TRUE)
backup_con <- dbConnect(SQLite(), backup_file)
on.exit(dbDisconnect(backup_con), add = TRUE)
RSQLite::sqliteCopyDatabase(source_con, backup_con)

integrity <- dbGetQuery(backup_con, "PRAGMA integrity_check")[[1]][[1]]
if (!identical(integrity, "ok")) stop("Backup integrity check failed: ", integrity)

message("Verified database backup created: ", normalizePath(backup_file))
