# setup_database_modified.R
library(DBI)
library(RSQLite)
library(digest)

##########################
#
# latest version - 14112026
#
##########################


setup_complete_database <- function() {

  # Delete old database if it exists
  if(file.exists("sequencing_projects.db")) {
    file.remove("sequencing_projects.db")
    message("Old database deleted")
  }

  # Connect to new SQLite database
  con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")

  # Users table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      full_name TEXT,
      password TEXT NOT NULL,
      email TEXT NOT NULL,
      phone TEXT,
      research_group TEXT,
      is_admin INTEGER DEFAULT 0,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Budget holders table with four columns
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

  # Sample service type table (replaces sample_types)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS service_types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      service_type TEXT UNIQUE NOT NULL,
      kit TEXT NOT NULL,
      costs_per_sample REAL NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Sequencing depth table (replaces sequencing_kits)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sequencing_depths (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      depth_description TEXT UNIQUE NOT NULL,
      cost_upto_150_cycles REAL,
      cost_upto_300_cycles REAL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Sequencing cycles table (replaces sequencing_types)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sequencing_cycles (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      cycles_description TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Sequencing platforms table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sequencing_platforms (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Types table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS types (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Reference genomes table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reference_genomes (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Landing text panels (post-login announcement/service blocks)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS announcement_panels (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      panel_key TEXT UNIQUE NOT NULL,
      title TEXT,
      subtitle TEXT,
      display_order INTEGER NOT NULL,
      is_active INTEGER NOT NULL DEFAULT 1,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_by TEXT
    )
  ")

  # Landing text items (one white box per row, markdown-lite content)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS announcement_items (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      panel_id INTEGER NOT NULL,
      display_order INTEGER NOT NULL,
      markdown_text TEXT NOT NULL,
      is_active INTEGER NOT NULL DEFAULT 1,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_by TEXT,
      FOREIGN KEY (panel_id) REFERENCES announcement_panels (id)
    )
  ")

  # Email templates table
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

  # email logs table
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

  # Projects table with all modifications
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS projects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      project_id INTEGER UNIQUE,
      project_name TEXT NOT NULL,
      user_id INTEGER NOT NULL,
      responsible_user TEXT NOT NULL,
      reference_genome TEXT NOT NULL,
      service_type_id INTEGER NOT NULL,
      budget_id INTEGER NOT NULL,
      description TEXT,
      num_samples INTEGER,
      sequencing_platform TEXT,
      sequencing_depth_id INTEGER NOT NULL,
      sequencing_cycles_id INTEGER NOT NULL,
      kickoff_meeting INTEGER,
      type_id INTEGER,
      additional_cost REAL,
      total_cost REAL,
      status TEXT DEFAULT 'Created' CHECK(status IN (
        'Created',
        'Samples received',
        'Library preparation',
        'QC done',
        'Data analysis',
        'Data released',
        'Legacy project'
      )),
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users (id),
      FOREIGN KEY (type_id) REFERENCES types (id),
      FOREIGN KEY (service_type_id) REFERENCES service_types (id),
      FOREIGN KEY (budget_id) REFERENCES budget_holders (id),
      FOREIGN KEY (sequencing_depth_id) REFERENCES sequencing_depths (id),
      FOREIGN KEY (sequencing_cycles_id) REFERENCES sequencing_cycles (id)
    )
  ")

  # Create trigger to auto-generate project_id - MODIFIED FOR PREFIX
  dbExecute(con, "
    CREATE TRIGGER IF NOT EXISTS auto_project_id
    AFTER INSERT ON projects
    FOR EACH ROW
    WHEN NEW.project_id IS NULL
    BEGIN
      UPDATE projects
      SET project_id = (SELECT COALESCE(MAX(project_id), 0) + 1 FROM projects)
      WHERE id = NEW.id;
    END;
  ")

  # Create backup_logs table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS backup_logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      backup_timestamp TEXT NOT NULL,
      backup_size INTEGER NOT NULL,
      backed_up_by TEXT NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Insert default admin user
  # here we can change the admin password if needed.
  default_password <- digest::digest("admin123")
  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('admin', ?, 'ngs@biochem.mpg.de', 1, 'Kim')
", params = list(default_password))

  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('admin2', ?, 'yeroslaviz@biochem.mpg.de', 1, 'Cox')
", params = list(digest::digest("yeroslaviz123")))

  # Ensure LDAP admin users exist after rebuild (add more by copying this block)
  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('yeroslaviz', ?, 'yeroslaviz@biochem.mpg.de', 1, 'Cox')
", params = list(digest::digest("ldap-only")))

  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('rkim', ?, 'rkim@biochem.mpg.de', 1, 'NGS Facility')
  ", params = list(digest::digest("ldap-only")))

  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('casper', ?, 'casper@biochem.mpg.de', 1, 'NGS Facility')
  ", params = list(digest::digest("ldap-only")))

  dbExecute(con, "
  INSERT OR IGNORE INTO users (username, password, email, is_admin, research_group)
  VALUES ('gautsch', ?, 'gautsch@biochem.mpg.de', 1, 'NGS Facility')
  ", params = list(digest::digest("ldap-only")))

  # Insert sample users with phone and research_group
  sample_users <- data.frame(
    username = c('user1', 'user2'),
    full_name = c('User One', 'User Two'),
    password = c(digest('user1123'), digest('user2123')),
    email = c('user1@institute.org', 'user2@institute.org'),
    phone = c("+49 89 12345678", "+49 89 87654321"),
    research_group = c("Cox", "Baier"),
    is_admin = c(0, 0)
  )

  for(i in 1:nrow(sample_users)) {
    dbExecute(con, "
    INSERT OR IGNORE INTO users (username, full_name, password, email, phone, research_group, is_admin)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    sample_users$username[i],
    sample_users$full_name[i],
    sample_users$password[i],
    sample_users$email[i],
    sample_users$phone[i],
    sample_users$research_group[i],
    sample_users$is_admin[i]
  ))
  }

  # Insert budget holders
  budget_holders_file <- "budget_holders.csv"

  if (!file.exists(budget_holders_file)) {
    stop("❌ Budget holders file not found: ", budget_holders_file,
         "\nPlease create a file named 'budget_holders.csv' in the project directory.")
  }

  # Read CSV
  budget_holders_df <- read.csv(budget_holders_file, stringsAsFactors = FALSE)

  # Validate required columns
  required_cols <- c("name", "surname", "email", "cost_center")
  missing_cols <- setdiff(required_cols, names(budget_holders_df))
  if (length(missing_cols) > 0) {
    stop("❌ Missing required columns in budget_holders.csv: ", paste(missing_cols, collapse = ", "))
  }

  # Clean whitespace
  budget_holders_df$name <- trimws(budget_holders_df$name)
  budget_holders_df$surname <- trimws(budget_holders_df$surname)
  budget_holders_df$email <- trimws(budget_holders_df$email)
  budget_holders_df$cost_center <- trimws(budget_holders_df$cost_center)

  # Insert into database
  for (i in 1:nrow(budget_holders_df)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO budget_holders (name, surname, cost_center, email)
      VALUES (?, ?, ?, ?)
    ", params = list(
      budget_holders_df$name[i],
      budget_holders_df$surname[i],
      budget_holders_df$cost_center[i],
      budget_holders_df$email[i]
    ))
  }

  message("✅ Budget holders loaded from ", budget_holders_file, " (", nrow(budget_holders_df), " entries)")

  # Insert sample service types
  service_types <- data.frame(
    service_type = c('single cell/low mRNAseq',
                     'single cell/low total RNAseq mammalian rRNA depletion',
                     'single cell/low total RNAseq others rRNA depletion',
                     'single cell/nuclei mRNAseq',
                     'bulk mRNAseq',
                     'ChIPseq',
                     'Final Library',
                     'something else'),
    kit = c('Smartseq2', 'Taraka kit', 'Taraka kit', '10x', 'NEB Ultra II',
            'Nugen Ovation kit', 'sequencing only', ''),
    costs_per_sample = c(45, 85, 150, 2250, 60, 45, 0, 50)
  )

  for(i in 1:nrow(service_types)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO service_types (service_type, kit, costs_per_sample)
      VALUES (?, ?, ?)
    ", params = list(
      service_types$service_type[i],
      service_types$kit[i],
      service_types$costs_per_sample[i]
    ))
  }

  # Insert sequencing depths
  sequencing_depths <- data.frame(
    depth_description = c('upto 200M', 'upto 400M', 'upto 800M', 'other'),
    cost_upto_150_cycles = c(600, 1000, 1900, NA),
    cost_upto_300_cycles = c(900, 1500, 3500, NA)
  )

  for(i in 1:nrow(sequencing_depths)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO sequencing_depths (depth_description, cost_upto_150_cycles, cost_upto_300_cycles)
      VALUES (?, ?, ?)
    ", params = list(
      sequencing_depths$depth_description[i],
      sequencing_depths$cost_upto_150_cycles[i],
      sequencing_depths$cost_upto_300_cycles[i]
    ))
  }

  # Insert sequencing cycles
  sequencing_cycles <- data.frame(
    cycles_description = c('upto 100/150 cycles (2x60 or 2x 75)',
                           'upto 200/300 cycles (2x 110 or 2x150)')
  )

  for(i in 1:nrow(sequencing_cycles)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO sequencing_cycles (cycles_description)
      VALUES (?)
    ", params = list(sequencing_cycles$cycles_description[i]))
  }

  # Insert default types
  default_types <- c("test run", "10x single cell ATACseq", "10x single cell RNAseq",
                     "10x single nucleus RNAseq", "ATACseq", "ChIPseq", "CRISPRscreen",
                     "Cut&Run/Tag", "DNAseq - single stranded", "HiCseq - Bulk",
                     "HiCseq - single nucleus", "MicroCseq", "Riboseq",
                     "RNAseq - mRNAs (poly-A enrichment) RNAseq - single cell/embryo",
                     "RNAseq - smallRNAs", "RNAseq - total + rRNAdepletion SeENseq",
                     "TIPseq", "tRNAseq", "WGS")
  default_types <- sort(default_types)
  for(type in default_types) {
    dbExecute(con, "
      INSERT OR IGNORE INTO types (name)
      VALUES (?)
    ", params = list(type))
  }

  # Insert default sequencing platforms
  default_platforms <- c("Illumina NovaSeq", "Illumina NextSeq", "Aviti", "Oxford Nanopore")
  for(platform in default_platforms) {
    dbExecute(con, "INSERT OR IGNORE INTO sequencing_platforms (name) VALUES (?)", params = list(platform))
  }

  # Insert default reference genomes
  default_genomes <- c(
    "Homo sapiens",
    "Mus musculus",
    "Caenorhabditis elegans",
    "Saccharomyces cerevisiae - yeast",
    "Drosophila melanogaster",
    "Drosophila pseudoobscura",
    "Danio rerio - Zebrafish",
    "Escherichia coli",
    "Gallus gallus",
    "Others or Mixed",
    "Custom"
  )
  for(genome in default_genomes) {
    dbExecute(con, "
      INSERT OR IGNORE INTO reference_genomes (name)
      VALUES (?)
    ", params = list(genome))
  }

  # Insert email templates
  email_templates <- data.frame(
    template_name = c('project_creation'),
    subject = c('New Sequencing Project Created'),
    body_template = c('Dear {surname} {name},\n\nA new sequencing project has been created under your cost center {cost_center}.\n\nProject Details:\n- Project Name: {project_name}\n- Responsible User: {responsible_user}\n- Number of Samples: {num_samples}\n- Sample Service Type: {service_type}\n- Total Project Costs will be provided soon after project assessment by the NGS core facility.\n\n{cost_warning}\n\nBest regards,\nSequencing Facility Team')
  )

  for(i in 1:nrow(email_templates)) {
    dbExecute(con, "
      INSERT OR IGNORE INTO email_templates (template_name, subject, body_template)
      VALUES (?, ?, ?)
    ", params = list(
      email_templates$template_name[i],
      email_templates$subject[i],
      email_templates$body_template[i]
    ))
  }

  # Insert default landing text panels and boxes
  dbExecute(con, "
    INSERT OR IGNORE INTO announcement_panels (panel_key, title, subtitle, display_order, is_active, updated_by)
    VALUES (?, ?, ?, ?, 1, ?)
  ", params = list("announcement", "Announcement", "**Updated February 2026**", 1, "setup_database.R"))

  dbExecute(con, "
    INSERT OR IGNORE INTO announcement_panels (panel_key, title, subtitle, display_order, is_active, updated_by)
    VALUES (?, ?, ?, ?, 1, ?)
  ", params = list("service_info", "", "", 2, "setup_database.R"))

  announcement_panel_id <- dbGetQuery(
    con,
    "SELECT id FROM announcement_panels WHERE panel_key = 'announcement'"
  )$id[1]

  service_panel_id <- dbGetQuery(
    con,
    "SELECT id FROM announcement_panels WHERE panel_key = 'service_info'"
  )$id[1]

  default_announcement_items <- c(
    "**QC submission**: Everyday **9:00 - 11:00**, Result will be available **13:00-15:00** in your datashare folder or ask a link.",
    "**8-strip tube**: If you submit more than **(>=) 4 samples for Qubit**, please use 8-strip PCR tube.",
    paste(
      "**NGS data in your personal pool folder:**",
      "(Windows) \\\\samba-pool-dnaseq\\pool-dnaseq\\",
      "(Mac) smb://samba-pool-dnaseq/pool-dnaseq/",
      "or, via datashare - ask a link.",
      sep = "\n"
    ),
    "**NGS Facility will not store your samples and data.** Please submit **aliquot** of your sample and **back-up** the data in your group storage space. We will discard the sample and data every two weeks."
  )

  default_service_items <- c(
    paste(
      "The NGS lab of the Core Facility is providing sequencing as a service. We highly recommend to contact us before the start of your experiment. In collaboration with the Bioinformatics Core Facility, we provide:",
      "",
      "- Assistance with experimental design of the studies (required number of samples and replicates)",
      "- Assistance with the use of NGS open source analysis tools",
      "- Data analysis on collaborative basis",
      sep = "\n"
    ),
    "If you have any further questions, please do not hesitate to contact us @ [**@NGS**](mailto:ngs@biochem.mpg.de) !",
    paste(
      "- To start, Click on **'Create New Project'**.",
      "- For better overview, we recommend keeping the project Name to this preferred format - **YYYYMMDD_AB_CD** - AB-Groupleader's initial, CD-Researcher's initial)",
      "- After creating the project an email will be sent to you as well as your group leader for approval. The NGS facility will also get a copy of this mail notifying them of a new project.",
      sep = "\n"
    )
  )

  existing_announcement_items <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM announcement_items WHERE panel_id = ?",
    params = list(announcement_panel_id)
  )$n[1]
  if (existing_announcement_items == 0) {
    for (i in seq_along(default_announcement_items)) {
      dbExecute(con, "
        INSERT INTO announcement_items (panel_id, display_order, markdown_text, is_active, updated_by)
        VALUES (?, ?, ?, 1, ?)
      ", params = list(announcement_panel_id, i, default_announcement_items[i], "setup_database.R"))
    }
  }

  existing_service_items <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM announcement_items WHERE panel_id = ?",
    params = list(service_panel_id)
  )$n[1]
  if (existing_service_items == 0) {
    for (i in seq_along(default_service_items)) {
      dbExecute(con, "
        INSERT INTO announcement_items (panel_id, display_order, markdown_text, is_active, updated_by)
        VALUES (?, ?, ?, 1, ?)
      ", params = list(service_panel_id, i, default_service_items[i], "setup_database.R"))
    }
  }

  # Insert some sample projects
  sample_projects <- data.frame(
    project_name = c('Test Project 1', 'Test Project 2'),
    user_id = c(2, 3),
    responsible_user = c('user1', 'user2'),
    reference_genome = c("Hsp.GRCh38", "Mmu.GrCm38"),
    service_type_id = c(1, 2),
    budget_id = c(1, 2),
    description = c('Test description 1', 'Test description 2'),
    num_samples = c(10, 5),
    sequencing_platform = c('Illumina NovaSeq', 'Illumina NextSeq'),
    sequencing_depth_id = c(1, 2),
    sequencing_cycles_id = c(1, 2),
    kickoff_meeting = c(1, 0),
    type_id = c(1, 2),
    total_cost = c(1050, 1925),
    status = c('Created', 'Samples received')
  )

  for(i in 1:nrow(sample_projects)) {
    dbExecute(con, "
      INSERT INTO projects
      (project_name, user_id, responsible_user, reference_genome, service_type_id,
       budget_id, description, num_samples, sequencing_platform, sequencing_depth_id,
       sequencing_cycles_id, kickoff_meeting, type_id, total_cost, status)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ", params = list(
      sample_projects$project_name[i],
      sample_projects$user_id[i],
      sample_projects$responsible_user[i],
      sample_projects$reference_genome[i],
      sample_projects$service_type_id[i],
      sample_projects$budget_id[i],
      sample_projects$description[i],
      sample_projects$num_samples[i],
      sample_projects$sequencing_platform[i],
      sample_projects$sequencing_depth_id[i],
      sample_projects$sequencing_cycles_id[i],
      sample_projects$kickoff_meeting[i],
      sample_projects$type_id[i],
      sample_projects$total_cost[i],
      sample_projects$status[i]
    ))
  }

  dbDisconnect(con)
  message("New database created successfully with all modifications!")
  message("Default login: admin/admin123")
  message("Sample users: user1/user1123, user2/user2123")
  message("Budget holders table with name, surname, cost_center, email")
  message("Sample service types table with costs per sample")
  message("Sequencing depth table with cost calculations")
  message("Email templates table for notifications")
  message("Announcement panels and markdown text boxes initialized")
  message("Index type and read lengths tables removed")
}

# Run the setup
setup_complete_database()
