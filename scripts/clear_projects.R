#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
confirmed <- "--confirm" %in% args
positional_args <- args[!startsWith(args, "--")]

db_file <- if (length(positional_args) >= 1) {
  positional_args[[1]]
} else {
  Sys.getenv("MS_DB_FILE", "ms-app/ms_projects.db")
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_file <- if (length(positional_args) >= 2) {
  positional_args[[2]]
} else {
  file.path(
    dirname(db_file),
    paste0(tools::file_path_sans_ext(basename(db_file)), "_before_project_cleanup_", timestamp, ".sqlite")
  )
}

if (!file.exists(db_file)) {
  stop("Database does not exist: ", db_file)
}
if (file.exists(backup_file)) {
  stop("Backup destination already exists: ", backup_file)
}

con <- dbConnect(SQLite(), db_file)
on.exit(dbDisconnect(con), add = TRUE)
invisible(dbExecute(con, "PRAGMA foreign_keys = ON"))
invisible(dbExecute(con, "PRAGMA busy_timeout = 10000"))

integrity <- dbGetQuery(con, "PRAGMA integrity_check")[[1]][[1]]
if (!identical(integrity, "ok")) {
  stop("Source database integrity check failed: ", integrity)
}

project_tables <- c(
  "projects",
  "project_samples",
  "project_files",
  "project_status_history",
  "project_documents",
  "mail_delivery_queue"
)
missing_tables <- setdiff(c(project_tables, "email_logs"), dbListTables(con))
if (length(missing_tables) > 0) {
  stop("Required tables are missing: ", paste(missing_tables, collapse = ", "))
}

counts <- vapply(
  project_tables,
  function(table_name) {
    dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", dbQuoteIdentifier(con, table_name)))$n[[1]]
  },
  numeric(1)
)
counts <- c(
  counts,
  project_email_logs = dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM email_logs WHERE project_id IS NOT NULL"
  )$n[[1]]
)

message("Project-linked rows that would be removed:")
for (table_name in names(counts)) {
  message("  ", table_name, ": ", counts[[table_name]])
}

if (!confirmed) {
  stop(
    "Dry run only; no rows were changed. Stop the application, verify the counts, ",
    "then run again with --confirm."
  )
}

backup_con <- dbConnect(SQLite(), backup_file)
on.exit(dbDisconnect(backup_con), add = TRUE)
RSQLite::sqliteCopyDatabase(con, backup_con)

backup_integrity <- dbGetQuery(backup_con, "PRAGMA integrity_check")[[1]][[1]]
if (!identical(backup_integrity, "ok")) {
  stop("Backup integrity check failed: ", backup_integrity)
}
dbDisconnect(backup_con)

invisible(dbWithTransaction(con, {
  # email_logs has a non-cascading foreign key in migrated production databases.
  dbExecute(con, "DELETE FROM email_logs WHERE project_id IS NOT NULL")
  # Delete child rows explicitly as well as relying on cascades. This also
  # removes legacy orphans created while foreign-key enforcement was disabled.
  dbExecute(con, "DELETE FROM project_samples")
  dbExecute(con, "DELETE FROM project_files")
  dbExecute(con, "DELETE FROM project_status_history")
  dbExecute(con, "DELETE FROM project_documents")
  dbExecute(con, "DELETE FROM mail_delivery_queue")
  dbExecute(con, "DELETE FROM projects")

  sequence_tables <- c(project_tables, "email_logs")
  placeholders <- paste(rep("?", length(sequence_tables)), collapse = ", ")
  dbExecute(
    con,
    paste0("DELETE FROM sqlite_sequence WHERE name IN (", placeholders, ")"),
    params = as.list(sequence_tables)
  )
}))

foreign_key_violations <- dbGetQuery(con, "PRAGMA foreign_key_check")
if (nrow(foreign_key_violations) > 0) {
  stop("Cleanup finished with foreign-key violations; restore the verified backup.")
}

remaining_projects <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM projects")$n[[1]]
remaining_project_logs <- dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM email_logs WHERE project_id IS NOT NULL"
)$n[[1]]
remaining_child_rows <- sum(vapply(
  setdiff(project_tables, "projects"),
  function(table_name) {
    dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", dbQuoteIdentifier(con, table_name)))$n[[1]]
  },
  numeric(1)
))
if (remaining_projects != 0 || remaining_project_logs != 0 || remaining_child_rows != 0) {
  stop("Cleanup verification failed; restore the verified backup.")
}

message("Project cleanup completed successfully.")
message("Verified pre-cleanup backup: ", normalizePath(backup_file))
message("Users, budget holders, references, controlled options, costs, landing text, and email templates were preserved.")
