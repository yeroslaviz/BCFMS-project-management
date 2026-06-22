#!/usr/bin/env Rscript

library(DBI)
library(RSQLite)
library(digest)

source("db_schema.R")

reset_requested <- Sys.getenv("MS_RESET_DB", "1") != "0"
db_path <- ms_initialize_database(reset = reset_requested)

message("Mass Spectrometry project database initialized: ", db_path)
message("Local test login: admin / ", Sys.getenv("MS_LOCAL_ADMIN_PASSWORD", "admin123"))
message("Facility email fallback: ", MS_FACILITY_EMAIL)
message("Upload pool root: ", MS_UPLOAD_ROOT)
message("Set MS_RESET_DB=0 to run schema/seed checks without deleting an existing database.")
