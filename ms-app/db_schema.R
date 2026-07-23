library(DBI)
library(RSQLite)
library(digest)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

MS_DB_FILE <- Sys.getenv("MS_DB_FILE", "ms_projects.db")
MS_FACILITY_EMAIL <- Sys.getenv("MS_FACILITY_EMAIL", "ms-service@biochem.mpg.de")
MS_MAIL_FROM <- Sys.getenv("MS_MAIL_FROM", MS_FACILITY_EMAIL)
MS_POOL_FALLBACK_EMAIL <- Sys.getenv("MS_POOL_FALLBACK_EMAIL", "omicsdesk@biochem.mpg.de")
MS_UPLOAD_ROOT <- Sys.getenv("MS_UPLOAD_ROOT", "/fs/pool/pool-bcfngs-ms-projects")
MS_LOCAL_UPLOAD_FALLBACK <- Sys.getenv("MS_LOCAL_UPLOAD_FALLBACK", "uploads_pending_pool")
MS_INSTITUTE_ADDRESS <- paste(
  "Max Planck Institute of Biochemistry",
  "Am Klopferspitz 18",
  "82152 Martinsried",
  "Germany",
  sep = "\n"
)

MS_SUBMISSION_WARNING <- paste(
  "Samples are stored for a maximum of 2 months after analysis is completed,",
  "then discarded without notice."
)

MS_SAMPLE_TABLE_STATEMENT <- paste(
  "The sample table above is part of the submitted project request. Please label",
  "submitted samples exactly as listed here and inform the facility before making",
  "changes after submission."
)

ms_project_types <- data.frame(
  slug = c("intact_mass", "proteomics", "metabolomics"),
  name = c("Intact / Native Mass", "Proteomics", "Metabolomics"),
  color = c("#DDCC77", "#88CCEE", "#CC6677"),
  description = c(
    "Intact mass measurements for proteins or other biomolecules.",
    "Protein identification, quantification, PTM, AP-MS, XL-MS, or related proteomics work.",
    "Metabolomics submissions with species/sample metadata and optional target lists."
  ),
  display_order = c(1L, 2L, 3L),
  stringsAsFactors = FALSE
)

ms_status_options <- c(
  "Submitted",
  "Under review",
  "Awaiting samples",
  "Samples received",
  "Measurement in progress",
  "Data analysis",
  "Data released",
  "Completed",
  "On hold",
  "Cancelled",
  "Legacy project"
)

ms_controlled_options <- list(
  intact_sample_type = c("Protein (monomer)", "Protein complex / Native complex", "Peptide", "Oligonucleotide (DNA / RNA)", "Small molecule", "Other"),
  proteomics_project_type = c("Protein ID", "Protein coverage / PTMs", "Interaction proteomics / AP-MS", "Total proteome", "PTM omics (global phospho, ubiquitin, etc.)", "Crosslinking (XL-MS)", "Other"),
  proteomics_sample_type = c("In-solution digest", "Gel band / Gel lane", "On-beads (immunoprecipitation)", "Cell pellet", "Protein pellet", "Tissue", "Predigested peptides", "Ready-to-load (peptides in MS-compatible buffer)", "Other"),
  proteomics_acquisition_mode = c("DDA (Data-Dependent)", "DIA (Data-Independent)", "No preference", "Discuss with facility"),
  proteomics_quantification_strategy = c("Label-free (LFQ)", "SILAC", "No quantification", "Other"),
  silac_amino_acids = c("Arg6", "Arg10", "Lys4", "Lys6", "Lys8", "Other"),
  digestion_enzyme = c("Trypsin", "LysC", "GluC", "AspN", "Chymotrypsin", "Trypsin+LysC", "Other", "Performed by user"),
  ptm_variable_modifications = c("Phospho (Ser/Thr/Tyr)", "Acetylation (Lys/N-term)", "GlyGly (Lys, ubiquitin)", "Methylation", "Other"),
  crosslinker = c("PhoX", "DSS/BS3", "DSSO", "DMTMM/EDC", "Other"),
  metabolomics_analysis_type = c("Targeted (specific panel)", "Untargeted (global profiling)", "Lipidomics", "Other"),
  metabolomics_sample_type = c("Cell pellet (adherent)", "Cell pellet (suspension)", "Cell culture medium / supernatant", "Tissue", "Other"),
  concentration_determination = c("Yes", "No", "Not applicable"),
  concentration_method = c("UV A280", "BCA", "Bradford", "NanoDrop", "Estimated", "Other"),
  sample_amount_unit = c("ug", "mg", "pmol", "nmol", "cells", "uL", "mL", "other"),
  concentration_unit = c("ng/uL", "ug/uL", "mg/mL", "uM", "mM", "cells/mL", "other"),
  volume_unit = c("uL", "mL", "other"),
  column_type = c("PepSep15", "PepSep8", "Aurora15", "Aurora25", "F5", "C8", "C18 Polar", "C18", "NativePac_OBE", "MAbPac-RP", "C4", "Other"),
  ms_machine = c("Expl01", "QHF2", "TimstofHT", "Tims2_Mann", "Tims3", "Zeno1_Mann", "Other"),
  data_acquisition = c("DDA", "DIA", "DIA and DDA")
)

ms_reference_organisms <- c(
  "Homo sapiens",
  "Mus musculus",
  "Rattus norvegicus",
  "Drosophila melanogaster",
  "Caenorhabditis elegans",
  "Saccharomyces cerevisiae",
  "Escherichia coli",
  "Danio rerio",
  "Arabidopsis thaliana",
  "Other",
  "Mixed species"
)

ms_user_roles <- c("user", "technician", "admin")

ms_facility_billing_groups <- data.frame(
  ldap_group = c("b_ms", "b_ngs"),
  name = c("MS Facility", "NGS Facility"),
  surname = c("Core", "Core"),
  email = c("ms-service@biochem.mpg.de", "ngs@biochem.mpg.de"),
  cost_center = c("K350B", "K350F"),
  stringsAsFactors = FALSE
)

ms_normalize_user_role <- function(role, is_admin = 0L) {
  role <- tolower(trimws(as.character(role %||% "")))
  if (role %in% ms_user_roles) return(role)
  if (as.integer(is_admin %||% 0L) == 1L) "admin" else "user"
}

ms_landing_text_defaults <- data.frame(
  block_key = c(
    "facility_scope",
    "experiment_planning",
    "gel_samples",
    "fasta_uploads",
    "sample_timing",
    "xl_ms_buffer",
    "facility_contact"
  ),
  title = c(
    "Mass Spectrometry Core Facility",
    "Experiment Planning",
    "Gel Samples",
    "FASTA Uploads",
    "Sample Timing",
    "XL-MS Buffers",
    "Contact"
  ),
  body = c(
    "The facility supports intact/native mass, proteomics, crosslinking-MS, and metabolomics/lipidomics workflows.",
    "Discuss the experiment, goal, and expectations with the facility before sample submission.",
    "For in-gel samples, provide the entire gel and a gel image indicating the bands to analyze.",
    "For recombinant, tagged, mutated, or non-standard proteins, upload a FASTA file.",
    "For proteomics samples requiring preparation, avoid Friday submissions unless arranged with the facility.",
    "For XL-MS, amine-free buffers are important; avoid Tris and ammonium salts.",
    "ms-service@biochem.mpg.de"
  ),
  display_order = seq_len(7),
  stringsAsFactors = FALSE
)

ms_db_path <- function() {
  MS_DB_FILE
}

ms_db_connect <- function() {
  dbConnect(RSQLite::SQLite(), ms_db_path())
}

ms_hash_password <- function(password) {
  digest::digest(password, algo = "sha256")
}

ms_table_exists <- function(con, table_name) {
  dbExistsTable(con, table_name)
}

ms_column_exists <- function(con, table_name, column_name) {
  if (!ms_table_exists(con, table_name)) return(FALSE)
  column_name %in% dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))$name
}

ms_add_column_if_missing <- function(con, table_name, column_sql) {
  column_name <- strsplit(trimws(column_sql), "\\s+")[[1]][1]
  if (!ms_column_exists(con, table_name, column_name)) {
    dbExecute(con, paste("ALTER TABLE", table_name, "ADD COLUMN", column_sql))
  }
}

ms_budget_holder_exists <- function(con, name, surname, cost_center, email) {
  count <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM budget_holders
    WHERE lower(name) = lower(?)
      AND lower(surname) = lower(?)
      AND lower(cost_center) = lower(?)
      AND lower(email) = lower(?)
  ", params = list(name, surname, cost_center, email))$n[1]
  isTRUE(count > 0)
}

ms_insert_budget_holder_if_missing <- function(con, name, surname, cost_center, email) {
  if (!ms_budget_holder_exists(con, name, surname, cost_center, email)) {
    dbExecute(con, "
      INSERT INTO budget_holders (name, surname, cost_center, email)
      VALUES (?, ?, ?, ?)
    ", params = list(name, surname, cost_center, email))
  }
}

ms_upsert_facility_billing_holder <- function(con, name, surname, cost_center, email) {
  existing <- dbGetQuery(con, "
    SELECT id, cost_center
    FROM budget_holders
    WHERE lower(name) = lower(?) AND lower(surname) = lower(?)
    ORDER BY id
  ", params = list(name, surname))

  desired_key <- toupper(gsub("[^A-Z0-9]", "", cost_center))
  existing_keys <- toupper(gsub("[^A-Z0-9]", "", existing$cost_center %||% character()))
  if (desired_key %in% existing_keys) {
    dbExecute(con, "
      UPDATE budget_holders SET email = ?
      WHERE id = ?
    ", params = list(email, existing$id[[which(existing_keys == desired_key)[1]]]))
    return(invisible(TRUE))
  }

  placeholder <- which(existing_keys %in% c("", "NA"))
  if (length(placeholder) > 0) {
    dbExecute(con, "
      UPDATE budget_holders SET cost_center = ?, email = ?
      WHERE id = ?
    ", params = list(cost_center, email, existing$id[[placeholder[1]]]))
  } else {
    ms_insert_budget_holder_if_missing(con, name, surname, cost_center, email)
  }
  invisible(TRUE)
}

ms_cleanup_budget_holders <- function(con) {
  if (!ms_table_exists(con, "budget_holders")) return(invisible(FALSE))

  dbExecute(con, "
    DELETE FROM budget_holders
    WHERE lower(name) IN ('administrator', 'unknown')
       OR lower(surname) IN ('surname', 'user')
  ")

  dbExecute(con, "
    DELETE FROM budget_holders
    WHERE id NOT IN (
      SELECT MIN(id)
      FROM budget_holders
      GROUP BY lower(name), lower(surname), lower(cost_center), lower(email)
    )
  ")

  invisible(TRUE)
}

ms_create_schema <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      full_name TEXT,
      password TEXT NOT NULL,
      email TEXT NOT NULL,
      phone TEXT,
      research_group TEXT,
      cost_center TEXT,
      role TEXT NOT NULL DEFAULT 'user',
      is_admin INTEGER DEFAULT 0,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS budget_holders (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      surname TEXT NOT NULL,
      cost_center TEXT NOT NULL,
      email TEXT NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      slug TEXT UNIQUE NOT NULL,
      name TEXT UNIQUE NOT NULL,
      color TEXT NOT NULL,
      description TEXT,
      display_order INTEGER NOT NULL,
      is_active INTEGER NOT NULL DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reference_organisms (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      is_active INTEGER NOT NULL DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS controlled_options (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      option_group TEXT NOT NULL,
      value TEXT NOT NULL,
      display_order INTEGER NOT NULL DEFAULT 1,
      is_active INTEGER NOT NULL DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(option_group, value)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS landing_text_blocks (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      block_key TEXT UNIQUE NOT NULL,
      title TEXT NOT NULL,
      body TEXT NOT NULL,
      display_order INTEGER NOT NULL DEFAULT 1,
      is_active INTEGER NOT NULL DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, paste0("
    CREATE TABLE IF NOT EXISTS projects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_code TEXT UNIQUE,
      project_name TEXT NOT NULL,
      project_type TEXT NOT NULL,
      user_id INTEGER NOT NULL,
      responsible_user TEXT NOT NULL,
      submitter_name TEXT,
      submitter_email TEXT,
      submitter_phone TEXT,
      submitter_group TEXT,
      budget_id INTEGER NOT NULL,
      submission_date TEXT NOT NULL,
      num_samples INTEGER NOT NULL,
      technical_replicates INTEGER NOT NULL DEFAULT 0
        CHECK (technical_replicates >= 0 AND typeof(technical_replicates) = 'integer'),
      sample_material TEXT,
      sample_buffer TEXT,
      sample_amount TEXT,
      sample_amount_unit TEXT,
      sample_concentration TEXT,
      sample_concentration_unit TEXT,
      concentration_determination TEXT,
      concentration_method TEXT,
      sample_volume TEXT,
      sample_volume_unit TEXT,
      sample_notes TEXT,
      sample_delivery_notes TEXT,
      special_requirements TEXT,
      column_type TEXT,
      column_type_other TEXT,
      ms_machine TEXT,
      ms_machine_other TEXT,
      data_acquisition TEXT,
      intact_sample_type TEXT,
      intact_sample_type_other TEXT,
      intact_stoichiometry TEXT,
      intact_sequence_name_structure TEXT,
      intact_expected_mass_da TEXT,
      intact_tags_modifications TEXT,
      intact_analysis_goal TEXT,
      intact_analysis_goal_other TEXT,
      intact_measurement_conditions TEXT,
      intact_expected_mass TEXT,
      intact_modifications TEXT,
      intact_fasta_path TEXT,
      proteomics_project_type TEXT,
      proteomics_project_type_other TEXT,
      proteomics_sample_type TEXT,
      proteomics_sample_type_other TEXT,
      gel_image_confirmation INTEGER DEFAULT 0,
      proteomics_acquisition_mode TEXT,
      proteomics_quantification_strategy TEXT,
      proteomics_silac_amino_acids TEXT,
      proteomics_species TEXT,
      proteomics_expression_host TEXT,
      proteomics_digestion_enzyme TEXT,
      proteomics_ptms TEXT,
      proteomics_crosslinker TEXT,
      proteomics_strategy TEXT,
      proteomics_strategy_other TEXT,
      proteomics_sample_processing TEXT,
      proteomics_processing_other TEXT,
      proteomics_quantification TEXT,
      proteomics_labeling TEXT,
      proteomics_silac_details TEXT,
      proteomics_organisms TEXT,
      proteomics_organism_notes TEXT,
      proteomics_fasta_path TEXT,
      proteomics_experiment_type TEXT,
      proteomics_enrichments TEXT,
      proteomics_specific_details TEXT,
      metabolomics_analysis_type TEXT,
      metabolomics_analysis_type_other TEXT,
      metabolomics_analysis_family TEXT,
      metabolomics_sample_type TEXT,
      metabolomics_sample_type_other TEXT,
      metabolomics_cell_number TEXT,
      metabolomics_cell_number_unit TEXT,
      metabolomics_supernatant_volume TEXT,
      metabolomics_supernatant_volume_unit TEXT,
      metabolomics_tissue_weight TEXT,
      metabolomics_tissue_weight_unit TEXT,
      metabolomics_species TEXT,
      metabolomics_targeted_analyte_text TEXT,
      metabolomics_library_path TEXT,
      metabolomics_notes TEXT,
      upload_root TEXT,
      project_folder TEXT,
      upload_storage_status TEXT DEFAULT 'not_checked',
      invoice_recipient_address TEXT,
      invoice_institute_address TEXT DEFAULT '", gsub("'", "''", MS_INSTITUTE_ADDRESS), "',
      report_notes TEXT,
      additional_cost REAL,
      total_cost REAL,
      technician_user_id INTEGER,
      status TEXT DEFAULT 'Submitted',
      last_status_update_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users (id),
      FOREIGN KEY (budget_id) REFERENCES budget_holders (id),
      FOREIGN KEY (technician_user_id) REFERENCES users (id)
    )
  "))

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_samples (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      row_index INTEGER NOT NULL,
      tube_id TEXT,
      condition TEXT,
      replicate TEXT,
      is_control INTEGER DEFAULT 0,
      description TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_files (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      file_type TEXT NOT NULL,
      original_name TEXT NOT NULL,
      stored_name TEXT NOT NULL,
      path TEXT NOT NULL,
      size_bytes INTEGER,
      mime_type TEXT,
      checksum TEXT,
      uploaded_by TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_status_history (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      status TEXT NOT NULL,
      changed_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      changed_by TEXT,
      FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_project_status_history_project_id
    ON project_status_history (project_id, changed_at, id)
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS email_templates (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      template_name TEXT UNIQUE NOT NULL,
      subject TEXT NOT NULL,
      body_template TEXT NOT NULL,
      is_active INTEGER DEFAULT 1,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS email_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER,
      sent_to TEXT,
      subject TEXT,
      sent_at DATETIME,
      status TEXT,
      error_message TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (project_id) REFERENCES projects (id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_documents (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER NOT NULL,
      document_type TEXT NOT NULL,
      path TEXT NOT NULL,
      created_by TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (project_id) REFERENCES projects (id) ON DELETE CASCADE
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS backup_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      backup_timestamp TEXT NOT NULL,
      backup_size INTEGER NOT NULL,
      backed_up_by TEXT NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
}

ms_migrate_schema <- function(con) {
  if (ms_table_exists(con, "users")) {
    ms_add_column_if_missing(con, "users", "role TEXT NOT NULL DEFAULT 'user'")
    ms_add_column_if_missing(con, "users", "cost_center TEXT")
    dbExecute(con, "
      UPDATE users
      SET role = CASE
        WHEN lower(COALESCE(role, '')) IN ('admin', 'technician') THEN lower(role)
        WHEN COALESCE(is_admin, 0) = 1 THEN 'admin'
        ELSE 'user'
      END
    ")
    dbExecute(con, "
      UPDATE users
      SET is_admin = CASE WHEN role = 'admin' THEN 1 ELSE 0 END
    ")
  }

  if (!ms_table_exists(con, "projects")) return(invisible(TRUE))

  project_columns <- c(
    "project_code TEXT",
    "project_type TEXT DEFAULT 'proteomics'",
    "technical_replicates INTEGER NOT NULL DEFAULT 0 CHECK (technical_replicates >= 0 AND typeof(technical_replicates) = 'integer')",
    "submitter_name TEXT",
    "submitter_email TEXT",
    "submitter_phone TEXT",
    "submitter_group TEXT",
    "submission_date TEXT",
    "sample_material TEXT",
    "sample_buffer TEXT",
    "sample_amount TEXT",
    "sample_amount_unit TEXT",
    "sample_concentration TEXT",
    "sample_concentration_unit TEXT",
    "concentration_determination TEXT",
    "concentration_method TEXT",
    "sample_volume TEXT",
    "sample_volume_unit TEXT",
    "sample_notes TEXT",
    "sample_delivery_notes TEXT",
    "special_requirements TEXT",
    "column_type TEXT",
    "column_type_other TEXT",
    "ms_machine TEXT",
    "ms_machine_other TEXT",
    "data_acquisition TEXT",
    "intact_sample_type TEXT",
    "intact_sample_type_other TEXT",
    "intact_stoichiometry TEXT",
    "intact_sequence_name_structure TEXT",
    "intact_expected_mass_da TEXT",
    "intact_tags_modifications TEXT",
    "intact_analysis_goal TEXT",
    "intact_analysis_goal_other TEXT",
    "intact_measurement_conditions TEXT",
    "intact_expected_mass TEXT",
    "intact_modifications TEXT",
    "intact_fasta_path TEXT",
    "proteomics_project_type TEXT",
    "proteomics_project_type_other TEXT",
    "proteomics_sample_type TEXT",
    "proteomics_sample_type_other TEXT",
    "gel_image_confirmation INTEGER DEFAULT 0",
    "proteomics_acquisition_mode TEXT",
    "proteomics_quantification_strategy TEXT",
    "proteomics_silac_amino_acids TEXT",
    "proteomics_species TEXT",
    "proteomics_expression_host TEXT",
    "proteomics_digestion_enzyme TEXT",
    "proteomics_ptms TEXT",
    "proteomics_crosslinker TEXT",
    "proteomics_strategy TEXT",
    "proteomics_strategy_other TEXT",
    "proteomics_sample_processing TEXT",
    "proteomics_processing_other TEXT",
    "proteomics_quantification TEXT",
    "proteomics_labeling TEXT",
    "proteomics_silac_details TEXT",
    "proteomics_organisms TEXT",
    "proteomics_organism_notes TEXT",
    "proteomics_fasta_path TEXT",
    "proteomics_experiment_type TEXT",
    "proteomics_enrichments TEXT",
    "proteomics_specific_details TEXT",
    "metabolomics_analysis_type TEXT",
    "metabolomics_analysis_type_other TEXT",
    "metabolomics_analysis_family TEXT",
    "metabolomics_sample_type TEXT",
    "metabolomics_sample_type_other TEXT",
    "metabolomics_cell_number TEXT",
    "metabolomics_cell_number_unit TEXT",
    "metabolomics_supernatant_volume TEXT",
    "metabolomics_supernatant_volume_unit TEXT",
    "metabolomics_tissue_weight TEXT",
    "metabolomics_tissue_weight_unit TEXT",
    "metabolomics_species TEXT",
    "metabolomics_targeted_analyte_text TEXT",
    "metabolomics_library_path TEXT",
    "metabolomics_notes TEXT",
    "upload_root TEXT",
    "project_folder TEXT",
    "upload_storage_status TEXT DEFAULT 'not_checked'",
    "invoice_recipient_address TEXT",
    paste0("invoice_institute_address TEXT DEFAULT '", gsub("'", "''", MS_INSTITUTE_ADDRESS), "'"),
    "report_notes TEXT",
    "additional_cost REAL",
    "total_cost REAL",
    "technician_user_id INTEGER",
    "last_status_update_at DATETIME"
  )

  for (column_sql in project_columns) {
    ms_add_column_if_missing(con, "projects", column_sql)
  }

  dbExecute(con, "
    UPDATE projects
    SET last_status_update_at = COALESCE(last_status_update_at, created_at, updated_at, CURRENT_TIMESTAMP)
    WHERE last_status_update_at IS NULL OR last_status_update_at = ''
  ")

  dbExecute(con, "
    INSERT INTO project_status_history (project_id, status, changed_at, changed_by)
    SELECT p.id,
           COALESCE(NULLIF(trim(p.status), ''), 'Submitted'),
           COALESCE(p.last_status_update_at, p.created_at, p.updated_at, CURRENT_TIMESTAMP),
           'migration'
    FROM projects p
    WHERE NOT EXISTS (
      SELECT 1
      FROM project_status_history h
      WHERE h.project_id = p.id
    )
  ")
}

ms_seed_defaults <- function(con) {
  for (i in seq_len(nrow(ms_project_types))) {
    dbExecute(con, "
      INSERT OR IGNORE INTO project_types (slug, name, color, description, display_order)
      VALUES (?, ?, ?, ?, ?)
    ", params = list(
      ms_project_types$slug[i],
      ms_project_types$name[i],
      ms_project_types$color[i],
      ms_project_types$description[i],
      ms_project_types$display_order[i]
    ))
    dbExecute(con, "
      UPDATE project_types
      SET name = ?, color = ?, description = ?, display_order = ?
      WHERE slug = ?
    ", params = list(
      ms_project_types$name[i],
      ms_project_types$color[i],
      ms_project_types$description[i],
      ms_project_types$display_order[i],
      ms_project_types$slug[i]
    ))
  }

  for (i in seq_along(ms_status_options)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO controlled_options (option_group, value, display_order)
      VALUES ('status', ?, ?)
    ", params = list(ms_status_options[i], i))
  }

  for (organism in ms_reference_organisms) {
    dbExecute(con, "
      INSERT OR IGNORE INTO reference_organisms (name)
      VALUES (?)
    ", params = list(organism))
  }

  for (i in seq_len(nrow(ms_landing_text_defaults))) {
    dbExecute(con, "
      INSERT OR IGNORE INTO landing_text_blocks (block_key, title, body, display_order)
      VALUES (?, ?, ?, ?)
    ", params = list(
      ms_landing_text_defaults$block_key[i],
      ms_landing_text_defaults$title[i],
      ms_landing_text_defaults$body[i],
      ms_landing_text_defaults$display_order[i]
    ))
  }

  for (group_name in names(ms_controlled_options)) {
    values <- ms_controlled_options[[group_name]]
    for (i in seq_along(values)) {
      dbExecute(con, "
        INSERT OR IGNORE INTO controlled_options (option_group, value, display_order)
        VALUES (?, ?, ?)
      ", params = list(group_name, values[i], i))
    }
  }

  default_password <- ms_hash_password(Sys.getenv("MS_LOCAL_ADMIN_PASSWORD", "admin123"))
  dbExecute(con, "
    INSERT OR IGNORE INTO users (username, full_name, password, email, role, is_admin, research_group)
    VALUES ('admin', 'Local MS Admin', ?, ?, 'admin', 1, 'Mass Spectrometry Core Facility')
  ", params = list(default_password, MS_FACILITY_EMAIL))

  dbExecute(con, "
    INSERT OR IGNORE INTO users (username, full_name, password, email, role, is_admin, research_group)
    VALUES ('yeroslaviz', 'Yeroslaviz', ?, 'yeroslaviz@biochem.mpg.de', 'admin', 1, 'Cox')
  ", params = list(ms_hash_password("ldap-only")))

  ms_cleanup_budget_holders(con)
  for (i in seq_len(nrow(ms_facility_billing_groups))) {
    ms_upsert_facility_billing_holder(
      con,
      ms_facility_billing_groups$name[[i]],
      ms_facility_billing_groups$surname[[i]],
      ms_facility_billing_groups$cost_center[[i]],
      ms_facility_billing_groups$email[[i]]
    )
  }

  budget_holders_file <- "budget_holders.csv"
  if (file.exists(budget_holders_file)) {
    budget_holders_df <- read.csv(budget_holders_file, stringsAsFactors = FALSE)
    required_cols <- c("name", "surname", "email", "cost_center")
    if (all(required_cols %in% names(budget_holders_df))) {
      for (col in required_cols) {
        budget_holders_df[[col]] <- trimws(as.character(budget_holders_df[[col]]))
      }
      for (i in seq_len(nrow(budget_holders_df))) {
        if (!nzchar(budget_holders_df$name[i]) || !nzchar(budget_holders_df$surname[i])) next
        ms_insert_budget_holder_if_missing(
          con,
          budget_holders_df$name[i],
          budget_holders_df$surname[i],
          budget_holders_df$cost_center[i],
          budget_holders_df$email[i]
        )
      }
    }
  }

  ms_cleanup_budget_holders(con)

  project_creation_body <- paste(
    "Dear user,",
    "",
    "A new Mass Spectrometry Core Facility project has been submitted.",
    "",
    "{warning}",
    "",
    "Project summary:",
    "- Project code: {project_code}",
    "- Project name: {project_name}",
    "- Project type: {project_type}",
    "- Responsible user: {responsible_user}",
    "- Biological samples: {num_samples}",
    "- Technical replicates: {technical_replicates}",
    "- Submission date: {submission_date}",
    "",
    "{field_summary}",
    "",
    MS_SAMPLE_TABLE_STATEMENT,
    "",
    "Best regards,",
    "Mass Spectrometry Core Facility",
    sep = "\n"
  )

  dbExecute(con, "
    INSERT OR IGNORE INTO email_templates (template_name, subject, body_template)
    VALUES ('project_creation', 'New MS project submitted', ?)
  ", params = list(project_creation_body))

  data_release_body <- paste(
    "Dear {responsible_user},",
    "",
    "Results for Mass Spectrometry Core Facility project {project_code} ({project_name}) are now available.",
    "",
    "Data path:",
    "{data_path}",
    "",
    "Please contact the MS Core Facility if you have questions about the released data.",
    "",
    "Best regards,",
    "Mass Spectrometry Core Facility",
    sep = "\n"
  )

  dbExecute(con, "
    INSERT OR IGNORE INTO email_templates (template_name, subject, body_template)
    VALUES ('data_release', 'MS project data released', ?)
  ", params = list(data_release_body))

  cost_body <- paste(
    "Dear {budget_holder},",
    "",
    "The Mass Spectrometry Core Facility has reviewed project {project_code} ({project_name}).",
    "",
    "Current estimated project cost: {total_cost}",
    "",
    "Please contact the facility if the estimate does not match the planned work.",
    "",
    "Best regards,",
    "Mass Spectrometry Core Facility",
    sep = "\n"
  )

  dbExecute(con, "
    INSERT OR IGNORE INTO email_templates (template_name, subject, body_template)
    VALUES ('cost_estimation', 'MS project cost estimation', ?)
  ", params = list(cost_body))
}

ms_initialize_database <- function(reset = FALSE) {
  if (reset && file.exists(ms_db_path())) {
    file.remove(ms_db_path())
  }

  con <- ms_db_connect()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "PRAGMA foreign_keys = ON")
  ms_create_schema(con)
  ms_migrate_schema(con)
  ms_seed_defaults(con)

  invisible(ms_db_path())
}
