#!/usr/bin/env Rscript

library(DBI)
library(RSQLite)
library(digest)

source("db_schema.R")

con <- ms_db_connect()
on.exit(dbDisconnect(con), add = TRUE)

updated <- ms_sync_landing_text_defaults(con)
message("Applied ", updated, " landing-page defaults to: ", ms_db_path())
message("Restart Shiny Server to load updated code constants such as MS_SUBMISSION_WARNING.")
