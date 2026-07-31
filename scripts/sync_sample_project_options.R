#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || length(args) > 2) {
  stop(
    "Usage: Rscript scripts/sync_sample_project_options.R ",
    "<database.sqlite> [backup.sqlite]"
  )
}

database_path <- normalizePath(args[[1]], mustWork = TRUE)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- if (length(args) == 2) {
  normalizePath(args[[2]], mustWork = FALSE)
} else {
  paste0(database_path, ".before_sample_project_sync_", timestamp, ".sqlite")
}

if (identical(database_path, backup_path)) {
  stop("The backup path must differ from the database path.")
}
if (file.exists(backup_path)) {
  stop("Backup destination already exists: ", backup_path)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]))
repository_root <- normalizePath(file.path(dirname(script_path), ".."))
app_dir <- file.path(repository_root, "ms-app")
old_working_directory <- getwd()
on.exit(setwd(old_working_directory), add = TRUE)
setwd(app_dir)
source("db_schema.R")

source_con <- dbConnect(SQLite(), database_path, flags = SQLITE_RO)
backup_con <- dbConnect(SQLite(), backup_path)
RSQLite::sqliteCopyDatabase(source_con, backup_con)
backup_integrity <- dbGetQuery(backup_con, "PRAGMA integrity_check")[[1]][[1]]
dbDisconnect(backup_con)
dbDisconnect(source_con)
if (!identical(backup_integrity, "ok")) {
  stop("Backup integrity check failed: ", backup_integrity)
}

con <- dbConnect(SQLite(), database_path)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA foreign_keys = ON")

operational_tables <- c(
  "projects",
  "project_samples",
  "project_files",
  "project_status_history",
  "email_logs"
)
row_counts <- function() {
  setNames(
    vapply(
      operational_tables,
      function(table_name) {
        dbGetQuery(
          con,
          paste0("SELECT COUNT(*) AS n FROM `", table_name, "`")
        )$n[[1]]
      },
      numeric(1)
    ),
    operational_tables
  )
}
before_counts <- row_counts()

before_options <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n FROM controlled_options WHERE option_group IN (",
    paste(rep("?", length(ms_authoritative_project_option_groups)), collapse = ","),
    ")"
  ),
  params = as.list(ms_authoritative_project_option_groups)
)$n[[1]]

ms_sync_authoritative_project_options(con, reset_costs = TRUE)

expected_options <- do.call(
  rbind,
  lapply(ms_authoritative_project_option_groups, function(group_name) {
    values <- trimws(ms_controlled_options[[group_name]])
    data.frame(
      option_group = group_name,
      value = values,
      display_order = seq_along(values),
      stringsAsFactors = FALSE
    )
  })
)
actual_options <- dbGetQuery(
  con,
  paste0(
    "SELECT option_group, value, display_order FROM controlled_options ",
    "WHERE option_group IN (",
    paste(rep("?", length(ms_authoritative_project_option_groups)), collapse = ","),
    ") ORDER BY option_group, display_order, value"
  ),
  params = as.list(ms_authoritative_project_option_groups)
)
expected_options <- expected_options[
  order(expected_options$option_group, expected_options$display_order),
]
row.names(expected_options) <- NULL
row.names(actual_options) <- NULL
if (!identical(actual_options, expected_options)) {
  stop("Synchronized controlled options do not match the workbook-backed schema.")
}

actual_costs <- dbGetQuery(
  con,
  paste(
    "SELECT co.option_group, co.value, oc.cost, oc.is_custom",
    "FROM controlled_options co",
    "JOIN option_costs oc ON oc.controlled_option_id = co.id",
    "WHERE co.option_group IN (",
    paste(rep("?", length(ms_priced_option_groups)), collapse = ","),
    ") ORDER BY co.option_group, co.display_order"
  ),
  params = as.list(ms_priced_option_groups)
)
expected_cost_count <- sum(vapply(ms_default_option_costs, length, integer(1)))
if (nrow(actual_costs) != expected_cost_count) {
  stop("Unexpected number of synchronized cost rows.")
}
for (i in seq_len(nrow(actual_costs))) {
  expected_cost <- ms_default_option_costs[[actual_costs$option_group[[i]]]][[
    actual_costs$value[[i]]
  ]]
  if (
    is.null(expected_cost) ||
      !isTRUE(all.equal(as.numeric(actual_costs$cost[[i]]), as.numeric(expected_cost))) ||
      actual_costs$is_custom[[i]] != 0
  ) {
    stop(
      "Cost validation failed for ",
      actual_costs$option_group[[i]],
      " / ",
      actual_costs$value[[i]]
    )
  }
}

after_counts <- row_counts()
if (!identical(before_counts, after_counts)) {
  stop("Operational project records changed during option synchronization.")
}

integrity <- dbGetQuery(con, "PRAGMA integrity_check")[[1]][[1]]
if (!identical(integrity, "ok")) {
  stop("Database integrity check failed after synchronization: ", integrity)
}

message("Verified pre-migration backup: ", backup_path)
message("Workbook-controlled option rows before: ", before_options)
message("Workbook-controlled option rows after: ", nrow(actual_options))
message("Verified cost rows after: ", nrow(actual_costs))
message("Operational project records preserved.")
message("Database integrity check: ok")
