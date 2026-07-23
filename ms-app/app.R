library(shiny)
library(DBI)
library(RSQLite)
library(digest)
library(tools)

source("db_schema.R")

options(shiny.maxRequestSize = as.numeric(Sys.getenv("MS_MAX_UPLOAD_MB", "75")) * 1024^2)

dt_available <- requireNamespace("DT", quietly = TRUE)
if (dt_available) {
  DTOutput <- DT::DTOutput
  renderDT <- DT::renderDT
  datatable <- DT::datatable
  formatStyle <- DT::formatStyle
  styleEqual <- DT::styleEqual
} else {
  DTOutput <- tableOutput
  renderDT <- renderTable
  datatable <- function(data, ...) data
  formatStyle <- function(table, ...) table
  styleEqual <- function(...) NULL
}

startup_db_error <- NULL
tryCatch(
  ms_initialize_database(reset = FALSE),
  error = function(e) startup_db_error <<- conditionMessage(e)
)

mail_available <- requireNamespace("mailR", quietly = TRUE)

to_bool <- function(x) {
  tolower(trimws(x %||% "")) %in% c("1", "true", "yes", "on")
}

auth_mode <- function() {
  tolower(trimws(Sys.getenv("AUTH_MODE", "local")))
}

is_ldap_mode <- function() {
  identical(auth_mode(), "ldap")
}

trust_proxy_auth_user_query <- function() {
  to_bool(Sys.getenv("TRUST_PROXY_AUTH_USER_QUERY", "0"))
}

scalar_text <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return(default)
  as.character(x[[1]])
}

trim_scalar <- function(x, default = "") {
  trimws(scalar_text(x, default))
}

non_empty <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[[1]]) && nzchar(trimws(as.character(x[[1]])))
}

first_non_empty <- function(..., default = "") {
  values <- trimws(as.character(c(...)))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) default else values[[1]]
}

is_email_like <- function(value) {
  grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", trim_scalar(value))
}

role_is_admin <- function(role) {
  identical(ms_normalize_user_role(role), "admin")
}

role_can_edit_projects <- function(role) {
  ms_normalize_user_role(role) %in% c("admin", "technician")
}

join_values <- function(x) {
  x <- x %||% character()
  x <- trimws(as.character(x))
  paste(x[nzchar(x)], collapse = "; ")
}

split_full_name <- function(full_name, fallback_username = "") {
  full_name <- trimws(full_name %||% "")
  if (!nzchar(full_name)) full_name <- fallback_username
  parts <- strsplit(full_name, "\\s+")[[1]]
  if (length(parts) <= 1) {
    list(first = full_name, last = "")
  } else {
    list(first = parts[1], last = paste(parts[-1], collapse = " "))
  }
}

sanitize_path_part <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", trimws(x %||% ""))
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "project" else substr(x, 1, 120)
}

safe_file_ext <- function(path) {
  tolower(tools::file_ext(path %||% ""))
}

sample_template_files <- c(
  intact_mass = "intact_template.txt",
  proteomics = "proteomics_template.txt",
  metabolomics = "metabolomics_template.txt"
)

sample_table_required_columns <- c("Tube_ID", "Condition", "Replicate", "Control?", "Concentration", "Unit", "Description", "Comments")

template_path_for_project_type <- function(project_type) {
  filename <- unname(sample_template_files[trim_scalar(project_type)])
  if (is.na(filename) || !nzchar(filename)) filename <- unname(sample_template_files["proteomics"])
  file.path(Sys.getenv("MS_TEMPLATE_DIR", "templates"), filename)
}

project_type_template_label <- function(project_type) {
  labels <- c(
    intact_mass = "Intact",
    proteomics = "Proteomics",
    metabolomics = "Metabolomics"
  )
  label <- unname(labels[trim_scalar(project_type)])
  if (is.na(label) || !nzchar(label)) "Template" else label
}

html_escape <- function(value) {
  value <- scalar_text(value)
  value <- gsub("&", "&amp;", value, fixed = TRUE)
  value <- gsub("<", "&lt;", value, fixed = TRUE)
  value <- gsub(">", "&gt;", value, fixed = TRUE)
  value <- gsub('"', "&quot;", value, fixed = TRUE)
  value
}

status_is_data_released <- function(status) {
  tolower(trim_scalar(status)) %in% c("data released", "data release")
}

parse_optional_nonnegative_number <- function(value, label) {
  text <- trim_scalar(value)
  if (!nzchar(text)) return(list(valid = TRUE, empty = TRUE, value = NA_real_))
  number <- suppressWarnings(as.numeric(text))
  if (length(number) == 0 || is.na(number) || number < 0) {
    return(list(
      valid = FALSE,
      empty = FALSE,
      value = NA_real_,
      error = paste0(label, " must be a non-negative number, for example 125 or 125.50.")
    ))
  }
  list(valid = TRUE, empty = FALSE, value = number)
}

format_cost_value <- function(value) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) == 0 || is.na(number)) "To be confirmed" else sprintf("EUR %.2f", number)
}

field_label <- function(text, help, example = NULL) {
  help_text <- help
  if (!is.null(example) && nzchar(example)) {
    help_text <- paste0(help, " Example: ", example)
  }
  tags$span(
    text,
    tags$span(
      class = "field-help",
      tabindex = "0",
      role = "button",
      `aria-label` = paste0("Help: ", help_text),
      `aria-controls` = "field-help-popover",
      `aria-expanded` = "false",
      `data-help` = help_text,
      "?"
    )
  )
}

warning_banner <- function(compact = FALSE) {
  div(
    class = if (compact) "submission-warning compact" else "submission-warning",
    strong("Warning: "),
    MS_SUBMISSION_WARNING
  )
}

guideline_panel <- function() {
  tagList(
    div(
      class = "guideline-panel",
      h3("Mass Spectrometry Core Facility"),
      p("The facility supports intact/native mass, proteomics, crosslinking-MS, and metabolomics/lipidomics workflows."),
      tags$ul(
        tags$li("Discuss the experiment, goal, and expectations with the facility before sample submission."),
        tags$li("For in-gel samples, provide the entire gel and a gel image indicating the bands to analyze."),
        tags$li("For recombinant, tagged, mutated, or non-standard proteins, upload a FASTA file."),
        tags$li("For proteomics samples requiring preparation, avoid Friday submissions unless arranged with the facility."),
        tags$li("For XL-MS, amine-free buffers are important; avoid Tris and ammonium salts.")
      ),
      p(tags$a(href = "mailto:ms-service@biochem.mpg.de", "ms-service@biochem.mpg.de"))
    )
  )
}

load_landing_text_blocks <- function(con, include_inactive = FALSE) {
  sql <- "
    SELECT id, block_key, title, body, display_order, is_active
    FROM landing_text_blocks
  "
  if (!isTRUE(include_inactive)) {
    sql <- paste(sql, "WHERE is_active = 1")
  }
  sql <- paste(sql, "ORDER BY display_order, title")
  dbGetQuery(con, sql)
}

render_landing_text_blocks <- function(blocks) {
  if (is.null(blocks) || nrow(blocks) == 0) return(NULL)
  div(
    class = "landing-text-grid",
    lapply(seq_len(nrow(blocks)), function(i) {
      body_lines <- strsplit(blocks$body[i] %||% "", "\n", fixed = TRUE)[[1]]
      body_lines <- body_lines[nzchar(trimws(body_lines))]
      div(
        class = "landing-text-block",
        h4(blocks$title[i]),
        if (length(body_lines) == 0) NULL else lapply(body_lines, function(line) {
          if (grepl("@", line) && grepl("^[^[:space:]]+@[^[:space:]]+$", trimws(line))) {
            p(tags$a(href = paste0("mailto:", trimws(line)), trimws(line)))
          } else {
            p(line)
          }
        })
      )
    })
  )
}

status_color <- function(status) {
  colors <- c(
    "Submitted" = "#DDCC77",
    "Under review" = "#88CCEE",
    "Awaiting samples" = "#E6D690",
    "Samples received" = "#9DD9F0",
    "Measurement in progress" = "#B7D89B",
    "Data analysis" = "#B4A7D6",
    "Data released" = "#8FD19E",
    "Completed" = "#77AADD",
    "On hold" = "#D8D8D8",
    "Cancelled" = "#CC6677",
    "Legacy project" = "#E9ECEF"
  )
  colors[[status]] %||% "#E9ECEF"
}

status_badge <- function(status) {
  status <- scalar_text(status, "Submitted")
  span(
    class = "status-badge",
    style = paste0("background-color:", status_color(status), ";"),
    status
  )
}

with_con <- function(expr) {
  con <- ms_db_connect()
  on.exit(dbDisconnect(con), add = TRUE)
  force(expr)
}

insert_record <- function(con, table, values) {
  values <- values[!vapply(values, is.null, logical(1))]
  cols <- names(values)
  placeholders <- paste(rep("?", length(cols)), collapse = ", ")
  sql <- paste0("INSERT INTO ", table, " (", paste(cols, collapse = ", "), ") VALUES (", placeholders, ")")
  dbExecute(con, sql, params = unname(values))
  dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id[1]
}

update_record <- function(con, table, values, where_sql, where_params = list()) {
  values <- values[!vapply(values, is.null, logical(1))]
  if (length(values) == 0) return(invisible(0))
  assignments <- paste(paste0(names(values), " = ?"), collapse = ", ")
  sql <- paste0("UPDATE ", table, " SET ", assignments, ", updated_at = CURRENT_TIMESTAMP WHERE ", where_sql)
  dbExecute(con, sql, params = c(unname(values), where_params))
}

load_choice_values <- function(con, option_group, fallback = character()) {
  rows <- dbGetQuery(
    con,
    "SELECT value FROM controlled_options WHERE option_group = ? AND is_active = 1 ORDER BY display_order, value",
    params = list(option_group)
  )
  values <- rows$value
  if (length(values) == 0 && length(fallback) > 0) values <- fallback
  values
}

load_reference_values <- function(con) {
  rows <- dbGetQuery(
    con,
    "SELECT name FROM reference_organisms WHERE is_active = 1 ORDER BY name"
  )
  rows$name
}

load_project_types <- function(con) {
  dbGetQuery(
    con,
    "SELECT slug, name, color, description FROM project_types WHERE is_active = 1 ORDER BY display_order"
  )
}

budget_holder_group_label <- function(holders) {
  if (is.null(holders) || nrow(holders) == 0) return(character())
  trimws(paste(holders$name %||% "", holders$surname %||% ""))
}

load_budget_holder_choices <- function(con) {
  holders <- dbGetQuery(
    con,
    "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY surname, name, cost_center"
  )
  labels <- paste(holders$name, holders$surname, "-", holders$cost_center)
  list(values = holders, choices = setNames(as.character(holders$id), labels))
}

load_budget_holders <- function(con) {
  dbGetQuery(
    con,
    "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY name, surname, cost_center"
  )
}

normalize_group_key <- function(value) {
  value <- trimws(as.character(value %||% ""))
  tolower(value)
}

extract_ldap_group_id <- function(ou_value) {
  ou_value <- trim_scalar(ou_value)
  if (!nzchar(ou_value)) return("")
  matches <- regmatches(ou_value, regexec("\\(([^\\)]+)\\)", ou_value))
  if (length(matches) > 0 && length(matches[[1]]) > 1) {
    ou_value <- trimws(matches[[1]][2])
  }
  ou_value <- sub("^[[:space:]]*(cn|ou)=([^,]+).*$", "\\2", ou_value, ignore.case = TRUE)
  trimws(ou_value)
}

extract_group_key_from_ou <- function(ou_value) {
  sub("^b[_-]", "", extract_ldap_group_id(ou_value), ignore.case = TRUE)
}

is_facility_ldap_group <- function(ou_value) {
  configured <- Sys.getenv("LDAP_FACILITY_GROUPS", "b_ms,b_ngs")
  facility_groups <- trimws(unlist(strsplit(configured, "[,;[:space:]]+")))
  facility_groups <- tolower(facility_groups[nzchar(facility_groups)])
  tolower(extract_ldap_group_id(ou_value)) %in% facility_groups
}

match_facility_billing_group <- function(holders, group_values) {
  if (is.null(holders) || nrow(holders) == 0) return(NULL)
  facility_values <- group_values[vapply(group_values, is_facility_ldap_group, logical(1))]
  group_ids <- unique(tolower(vapply(facility_values, extract_ldap_group_id, character(1))))
  mappings <- ms_facility_billing_groups[
    tolower(ms_facility_billing_groups$ldap_group) %in% group_ids,
    ,
    drop = FALSE
  ]
  if (nrow(mappings) != 1) return(NULL)

  group_label <- trimws(paste(mappings$name[[1]], mappings$surname[[1]]))
  rows <- holders[
    budget_holder_group_label(holders) == group_label &
      vapply(holders$cost_center, normalize_cost_center_key, character(1)) ==
        normalize_cost_center_key(mappings$cost_center[[1]]),
    ,
    drop = FALSE
  ]
  if (nrow(rows) != 1) return(NULL)
  list(group_label = group_label, cost_center = mappings$cost_center[[1]])
}

match_budget_holder_group <- function(holders, group_value) {
  if (is.null(holders) || nrow(holders) == 0) return("")
  full_names <- budget_holder_group_label(holders)
  name_norm <- normalize_group_key(holders$name)
  surname_norm <- normalize_group_key(holders$surname)
  full_norm <- normalize_group_key(full_names)

  group_values <- as.character(group_value %||% character())
  group_values <- group_values[!is.na(group_values) & nzchar(trimws(group_values))]
  for (value in group_values) {
    if (is_facility_ldap_group(value)) next
    key_norm <- normalize_group_key(extract_group_key_from_ou(value))
    if (!nzchar(key_norm)) next
    idx <- which(full_norm == key_norm)
    if (length(idx) == 0) idx <- which(name_norm == key_norm)
    if (length(idx) == 0) idx <- which(surname_norm == key_norm)
    if (length(idx) == 0) idx <- which(grepl(key_norm, full_norm, fixed = TRUE))
    if (length(idx) > 0) return(full_names[idx[1]])
  }
  facility <- match_facility_billing_group(holders, group_values)
  if (!is.null(facility)) return(facility$group_label)
  ""
}

map_group_from_ldap_ou <- function(ou_value, con) {
  holders <- tryCatch(load_budget_holders(con), error = function(e) data.frame())
  match_budget_holder_group(holders, ou_value)
}

budget_holder_group_choices <- function(holders) {
  labels <- unique(budget_holder_group_label(holders))
  labels <- labels[nzchar(labels)]
  setNames(labels, labels)
}

normalize_cost_center_key <- function(value) {
  toupper(gsub("[^A-Z0-9]", "", trim_scalar(value)))
}

match_budget_holder_id <- function(holders, group_value, cost_center = "") {
  if (is.null(holders) || nrow(holders) == 0) return("")
  group_label <- match_budget_holder_group(holders, group_value)
  rows <- budget_holders_for_group(holders, group_label)
  if (nrow(rows) == 0) return("")

  cost_key <- normalize_cost_center_key(cost_center)
  if (nzchar(cost_key)) {
    row_keys <- vapply(rows$cost_center, normalize_cost_center_key, character(1))
    idx <- which(row_keys == cost_key)
    if (length(idx) == 1) return(as.character(rows$id[[idx]]))
  }

  if (nrow(rows) == 1) as.character(rows$id[[1]]) else ""
}

budget_holders_for_group <- function(holders, group_label) {
  if (is.null(holders) || nrow(holders) == 0 || !nzchar(trim_scalar(group_label))) {
    return(holders[0, , drop = FALSE])
  }
  holders[budget_holder_group_label(holders) == trim_scalar(group_label), , drop = FALSE]
}

budget_holder_group_for_id <- function(con, budget_id) {
  budget_id <- suppressWarnings(as.integer(budget_id))
  if (is.na(budget_id)) return("")
  row <- dbGetQuery(
    con,
    "SELECT name, surname FROM budget_holders WHERE id = ? LIMIT 1",
    params = list(budget_id)
  )
  if (nrow(row) == 0) "" else budget_holder_group_label(row)
}

budget_holder_notice_ui <- function(holders, selected_group, selected_budget_id = NULL) {
  rows <- budget_holders_for_group(holders, selected_group)
  if (nrow(rows) == 0) return(NULL)

  notices <- list()
  cost_centers <- unique(trimws(as.character(rows$cost_center %||% "")))
  cost_centers <- cost_centers[nzchar(cost_centers)]
  if (length(cost_centers) > 1) {
    notices <- c(notices, list(
      div(class = "alert alert-info", "This PI / Group has multiple cost centers. Please verify the correct Billing Group.")
    ))
  }

  selected_id <- suppressWarnings(as.integer(selected_budget_id))
  if (length(selected_id) == 1 && !is.na(selected_id)) {
    selected <- rows[rows$id == selected_id, , drop = FALSE]
    if (nrow(selected) > 0) {
      cc <- trim_scalar(selected$cost_center[[1]])
      cc_norm <- toupper(gsub("[^A-Z0-9]", "", cc))
      if (!nzchar(cc) || identical(cc_norm, "NA")) {
        notices <- c(notices, list(
          div(class = "alert alert-warning", "No cost center is set for this Billing Group. Please notify the core facility.")
        ))
      }
    }
  }

  if (length(notices) == 0) NULL else tagList(notices)
}

load_staff_choices <- function(con, include_blank = TRUE) {
  staff <- dbGetQuery(con, "
    SELECT id, username, full_name, role
    FROM users
    WHERE role IN ('admin', 'technician') OR COALESCE(is_admin, 0) = 1
    ORDER BY role DESC, full_name, username
  ")
  if (nrow(staff) == 0) {
    labels <- character()
  } else {
    full_names <- trimws(as.character(staff$full_name %||% ""))
    full_names[is.na(full_names)] <- ""
    display_names <- ifelse(nzchar(full_names), full_names, staff$username)
    labels <- paste0(display_names, " (", staff$role, ")")
  }
  choices <- setNames(as.character(staff$id), labels)
  if (isTRUE(include_blank)) choices <- c("Unassigned" = "", choices)
  choices
}

parse_ldap_uris <- function(uri_string) {
  uri_string <- trim_scalar(uri_string)
  if (!nzchar(uri_string)) return(character())
  uris <- trimws(unlist(strsplit(uri_string, "[,[:space:]]+")))
  uris[nzchar(uris)]
}

ldap_escape_filter_value <- function(value) {
  value <- as.character(value %||% "")
  value <- gsub("\\\\", "\\\\5c", value)
  value <- gsub("\\*", "\\\\2a", value)
  value <- gsub("\\(", "\\\\28", value)
  value <- gsub("\\)", "\\\\29", value)
  value
}

normalize_ldap_lines <- function(lines) {
  if (is.null(lines) || length(lines) == 0) return(character())
  lines <- gsub("\r", "", lines, fixed = TRUE)
  out <- character()
  for (line in lines) {
    if (grepl("^[ \t]", line) && length(out) > 0) {
      out[length(out)] <- paste0(out[length(out)], sub("^[ \t]+", "", line))
    } else {
      out <- c(out, line)
    }
  }
  out
}

parse_ldapsearch_output <- function(lines, key, all = FALSE) {
  lines <- normalize_ldap_lines(lines)
  if (length(lines) == 0 || !nzchar(trim_scalar(key))) return(NULL)
  safe_key <- gsub("([\\^\\$\\.|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", key)
  pattern <- paste0("^", safe_key, "(;[^:]*)?::?[[:space:]]*(.*)$")
  matches <- regexec(pattern, lines, ignore.case = TRUE)
  match_list <- regmatches(lines, matches)
  values <- vapply(match_list, function(m) if (length(m) > 2) m[3] else NA_character_, character(1))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) NULL else if (isTRUE(all)) unique(values) else values[1]
}

ldapsearch_lookup <- function(uri, base_dn, filter, attrs, bind_dn = "", bind_pw = "") {
  ldapsearch_cmd <- Sys.getenv("LDAPSEARCH_PATH", "ldapsearch")
  args <- c("-LLL", "-x", "-H", uri, "-b", base_dn)

  pw_file <- NULL
  if (nzchar(bind_dn) && nzchar(bind_pw)) {
    pw_file <- tempfile("ldap_pw_")
    writeLines(bind_pw, pw_file)
    Sys.chmod(pw_file, "600")
    args <- c(args, "-D", bind_dn, "-y", pw_file)
  }
  on.exit({
    if (!is.null(pw_file) && file.exists(pw_file)) unlink(pw_file)
  }, add = TRUE)

  args <- c(args, filter, attrs)
  shell_args <- shQuote(args)
  output <- tryCatch(
    system2(ldapsearch_cmd, args = shell_args, stdout = TRUE, stderr = TRUE),
    warning = function(w) {
      out <- conditionMessage(w)
      attr(out, "status") <- 1L
      out
    },
    error = function(e) {
      out <- conditionMessage(e)
      attr(out, "status") <- 1L
      out
    }
  )

  if (to_bool(Sys.getenv("LDAP_DEBUG", "0"))) {
    cat(
      "LDAP DEBUG: ldapsearch",
      "uri=", uri,
      "status=", attr(output, "status") %||% 0,
      "\n",
      file = stderr()
    )
    flush.console()
  }

  status <- attr(output, "status")
  if (!is.null(status) && status != 0) return(character())
  output
}

ldap_lookup_user <- function(username) {
  attrs <- list(email = NULL, phone = NULL, ou = NULL, cost_center = NULL, full_name = NULL)
  username <- trim_scalar(username)
  if (!nzchar(username)) return(attrs)

  default_uri <- paste(
    "ldaps://ldapserv1.biochem.mpg.de",
    "ldaps://ldapserv2.biochem.mpg.de",
    "ldaps://ldapserv3.biochem.mpg.de",
    sep = ","
  )
  ldap_uris <- parse_ldap_uris(Sys.getenv("LDAP_URI", default_uri))
  ldap_base_dn <- Sys.getenv("LDAP_BASE_DN", "dc=biochem,dc=mpg,dc=de")
  attr_uid <- Sys.getenv("LDAP_ATTR_UID", "uid")
  attr_mail <- Sys.getenv("LDAP_ATTR_MAIL", "mail")
  attr_phone <- Sys.getenv("LDAP_ATTR_PHONE", "telephoneNumber")
  attr_ou <- Sys.getenv("LDAP_ATTR_OU", "ou")
  attr_cost_center <- Sys.getenv("LDAP_ATTR_COST_CENTER", "departmentNumber")
  attr_cn <- Sys.getenv("LDAP_ATTR_CN", "cn")
  attr_given <- Sys.getenv("LDAP_ATTR_GIVENNAME", "givenName")
  attr_sn <- Sys.getenv("LDAP_ATTR_SN", "sn")
  bind_dn <- Sys.getenv("LDAP_BIND_DN", "")
  bind_pw <- Sys.getenv("LDAP_BIND_PW", "")

  if (length(ldap_uris) == 0 || !nzchar(ldap_base_dn)) return(attrs)

  filter <- sprintf("(&(%s=%s)(objectClass=posixAccount))", attr_uid, ldap_escape_filter_value(username))
  lookup_attrs <- unique(c(attr_mail, attr_phone, attr_ou, attr_cost_center, attr_cn, attr_given, attr_sn))
  lookup_attrs <- lookup_attrs[nzchar(lookup_attrs)]

  for (uri in ldap_uris) {
    lines <- ldapsearch_lookup(uri, ldap_base_dn, filter, lookup_attrs, bind_dn, bind_pw)
    if (length(lines) == 0) next

    attrs$email <- parse_ldapsearch_output(lines, attr_mail)
    attrs$phone <- parse_ldapsearch_output(lines, attr_phone)
    attrs$ou <- parse_ldapsearch_output(lines, attr_ou, all = TRUE)
    attrs$cost_center <- parse_ldapsearch_output(lines, attr_cost_center)
    attrs$full_name <- parse_ldapsearch_output(lines, attr_cn)
    if (!nzchar(trim_scalar(attrs$full_name))) {
      given <- parse_ldapsearch_output(lines, attr_given)
      sn <- parse_ldapsearch_output(lines, attr_sn)
      attrs$full_name <- trimws(paste(given %||% "", sn %||% ""))
      if (!nzchar(attrs$full_name)) attrs$full_name <- NULL
    }

    if (any(vapply(attrs, function(x) nzchar(trim_scalar(x)), logical(1)))) {
      break
    }
  }

  if (to_bool(Sys.getenv("LDAP_DEBUG", "0"))) {
    cat(
      "LDAP DEBUG: attrs",
      "user=", username,
      "email=", attrs$email %||% "<missing>",
      "phone=", attrs$phone %||% "<missing>",
      "ou=", paste(attrs$ou %||% "<missing>", collapse = ","),
      "cost_center=", attrs$cost_center %||% "<missing>",
      "full_name=", attrs$full_name %||% "<missing>",
      "\n",
      file = stderr()
    )
    flush.console()
  }

  attrs
}

ensure_user <- function(con, username, full_name = NULL, email = NULL, phone = NULL, group = NULL, cost_center = NULL) {
  username <- trimws(username)
  row <- dbGetQuery(con, "SELECT * FROM users WHERE lower(username) = lower(?) LIMIT 1", params = list(username))
  if (nrow(row) == 0) {
    insert_record(con, "users", list(
      username = username,
      full_name = full_name %||% username,
      password = ms_hash_password("ldap-only"),
      email = email %||% paste0(username, "@biochem.mpg.de"),
      phone = phone %||% "",
      research_group = group %||% "",
      cost_center = cost_center %||% "",
      role = "user",
      is_admin = 0L
    ))
    row <- dbGetQuery(con, "SELECT * FROM users WHERE lower(username) = lower(?) LIMIT 1", params = list(username))
  }
  row
}

should_fill_profile_value <- function(current, replacement, placeholder = "") {
  current <- trim_scalar(current)
  replacement <- trim_scalar(replacement)
  if (!nzchar(replacement)) return(FALSE)
  !nzchar(current) || (nzchar(placeholder) && identical(tolower(current), tolower(placeholder)))
}

ensure_ldap_user <- function(con, username) {
  username <- trim_scalar(username)
  attrs <- ldap_lookup_user(username)
  attrs$email <- trim_scalar(attrs$email)
  attrs$phone <- trim_scalar(attrs$phone)
  ldap_groups <- as.character(attrs$ou %||% character())
  ldap_groups <- ldap_groups[!is.na(ldap_groups) & nzchar(trimws(ldap_groups))]
  attrs$cost_center <- trim_scalar(attrs$cost_center)
  attrs$full_name <- trim_scalar(attrs$full_name)

  mapped_group <- map_group_from_ldap_ou(ldap_groups, con)
  holders <- tryCatch(load_budget_holders(con), error = function(e) data.frame())
  facility_billing <- match_facility_billing_group(holders, ldap_groups)
  if (!is.null(facility_billing) && identical(mapped_group, facility_billing$group_label)) {
    attrs$cost_center <- facility_billing$cost_center
  }
  attrs$ou <- if (nzchar(mapped_group)) mapped_group else trim_scalar(ldap_groups)

  row <- ensure_user(
    con,
    username,
    full_name = if (nzchar(attrs$full_name)) attrs$full_name else NULL,
    email = if (nzchar(attrs$email)) attrs$email else NULL,
    phone = if (nzchar(attrs$phone)) attrs$phone else NULL,
    group = if (nzchar(attrs$ou)) attrs$ou else NULL,
    cost_center = if (nzchar(attrs$cost_center)) attrs$cost_center else NULL
  )

  updates <- list()
  current_group <- trim_scalar(row$research_group[[1]])
  if (nzchar(attrs$ou) && !identical(current_group, attrs$ou)) {
    updates$research_group <- attrs$ou
  }

  if (nzchar(attrs$cost_center) && !identical(trim_scalar(row$cost_center[[1]]), attrs$cost_center)) {
    updates$cost_center <- attrs$cost_center
  }

  if (should_fill_profile_value(row$email[[1]], attrs$email, paste0(username, "@biochem.mpg.de"))) {
    updates$email <- attrs$email
  }
  if (should_fill_profile_value(row$phone[[1]], attrs$phone)) {
    updates$phone <- attrs$phone
  }
  if (should_fill_profile_value(row$full_name[[1]], attrs$full_name, username)) {
    updates$full_name <- attrs$full_name
  }

  if (length(updates) > 0) {
    update_record(con, "users", updates, "id = ?", list(row$id[[1]]))
    row <- dbGetQuery(con, "SELECT * FROM users WHERE id = ? LIMIT 1", params = list(row$id[[1]]))
  }

  row
}

next_orbi_project_code <- function(con) {
  rows <- dbGetQuery(con, "
    SELECT project_code
    FROM projects
    WHERE project_code LIKE 'Orbi_%'
  ")
  codes <- rows$project_code %||% character()
  codes <- codes[grepl("^Orbi_[0-9]+$", codes)]
  nums <- suppressWarnings(as.integer(sub("^Orbi_", "", codes)))
  nums <- nums[!is.na(nums)]
  paste0("Orbi_", max(c(0L, nums)) + 1L)
}

project_summary_text <- function(project, samples = NULL) {
  lines <- c(
    paste("Project code:", scalar_text(project$project_code)),
    paste("Project name:", scalar_text(project$project_name)),
    paste("Project type:", scalar_text(project$project_type_name, scalar_text(project$project_type))),
    paste("Responsible user:", scalar_text(project$responsible_user)),
    paste("Submitter:", scalar_text(project$submitter_name)),
    paste("Submitter email:", scalar_text(project$submitter_email)),
    paste("Submission date:", scalar_text(project$submission_date)),
    paste("Number of samples:", scalar_text(project$num_samples)),
    paste("Buffer / solvent:", scalar_text(project$sample_buffer)),
    paste("Concentration determination:", paste(trim_scalar(project$concentration_determination), trim_scalar(project$concentration_method))),
    paste("Volume submitted:", paste(trim_scalar(project$sample_volume), trim_scalar(project$sample_volume_unit))),
    paste("Sample amount:", scalar_text(project$sample_amount)),
    paste("Biological question:", scalar_text(project$sample_notes)),
    paste("Special requirements:", scalar_text(project$special_requirements))
  )

  if (scalar_text(project$project_type) == "intact_mass") {
    lines <- c(lines,
      paste("Sample type:", scalar_text(project$intact_sample_type)),
      paste("Stoichiometry:", scalar_text(project$intact_stoichiometry)),
      paste("Sequence / name / structure:", scalar_text(project$intact_sequence_name_structure)),
      paste("Expected mass:", scalar_text(project$intact_expected_mass_da)),
      paste("Tags / modifications:", scalar_text(project$intact_tags_modifications))
    )
  }

  if (scalar_text(project$project_type) == "proteomics") {
    lines <- c(lines,
      paste("Proteomics project type:", scalar_text(project$proteomics_project_type)),
      paste("Proteomics sample type:", scalar_text(project$proteomics_sample_type)),
      paste("Acquisition mode:", scalar_text(project$proteomics_acquisition_mode)),
      paste("Quantification:", scalar_text(project$proteomics_quantification_strategy)),
      paste("SILAC amino acids:", scalar_text(project$proteomics_silac_amino_acids)),
      paste("Species:", scalar_text(project$proteomics_species)),
      paste("Expression host:", scalar_text(project$proteomics_expression_host)),
      paste("Digestion enzyme:", scalar_text(project$proteomics_digestion_enzyme)),
      paste("PTMs / variable modifications:", scalar_text(project$proteomics_ptms)),
      paste("Crosslinker:", scalar_text(project$proteomics_crosslinker))
    )
  }

  if (scalar_text(project$project_type) == "metabolomics") {
    lines <- c(lines,
      paste("Analysis family:", scalar_text(project$metabolomics_analysis_family)),
      paste("Approach:", scalar_text(project$metabolomics_analysis_type)),
      paste("Sample type:", scalar_text(project$metabolomics_sample_type)),
      paste("Cell number:", paste(trim_scalar(project$metabolomics_cell_number), trim_scalar(project$metabolomics_cell_number_unit))),
      paste("Supernatant volume:", paste(trim_scalar(project$metabolomics_supernatant_volume), trim_scalar(project$metabolomics_supernatant_volume_unit))),
      paste("Tissue weight:", paste(trim_scalar(project$metabolomics_tissue_weight), trim_scalar(project$metabolomics_tissue_weight_unit))),
      paste("Species:", scalar_text(project$metabolomics_species)),
      paste("Targeted analyte text:", scalar_text(project$metabolomics_targeted_analyte_text))
    )
  }

  if (!is.null(samples) && nrow(samples) > 0) {
    lines <- c(lines, "", "Sample overview:")
    for (i in seq_len(nrow(samples))) {
      lines <- c(lines, paste(
        samples$tube_id[i] %||% "",
        samples$condition[i] %||% "",
        samples$replicate[i] %||% "",
        ifelse(as.integer(samples$is_control[i] %||% 0) == 1, "control", "sample"),
        samples$description[i] %||% "",
        sep = " | "
      ))
    }
  }

  paste(lines, collapse = "\n")
}

draw_pdf_lines <- function(lines, title, file) {
  grDevices::pdf(file, width = 8.27, height = 11.69)
  on.exit(grDevices::dev.off(), add = TRUE)

  new_page <- function() {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::text(0.05, 0.96, title, adj = c(0, 1), cex = 1.25, font = 2)
    graphics::text(0.05, 0.925, format(Sys.time(), "%Y-%m-%d %H:%M"), adj = c(0, 1), cex = 0.75, col = "#555555")
    0.88
  }

  y <- new_page()
  for (line in lines) {
    wrapped <- if (nzchar(line)) strwrap(line, width = 92) else ""
    for (part in wrapped) {
      if (y < 0.06) y <- new_page()
      graphics::text(0.05, y, part, adj = c(0, 1), cex = 0.82)
      y <- y - 0.028
    }
    y <- y - 0.008
  }
}

write_project_report_pdf <- function(project, samples, files, file, document_type = "report") {
  title <- if (document_type == "invoice") "Mass Spectrometry Project Invoice" else "Mass Spectrometry Project Report"
  lines <- c(
    MS_SUBMISSION_WARNING,
    "",
    strsplit(project_summary_text(project, samples), "\n", fixed = TRUE)[[1]]
  )

  if (document_type == "invoice") {
    total_cost <- suppressWarnings(as.numeric(project$total_cost %||% NA_real_))
    additional_cost <- suppressWarnings(as.numeric(project$additional_cost %||% NA_real_))
    lines <- c(
      paste("Invoice recipient address:", scalar_text(project$invoice_recipient_address, "Not entered")),
      "",
      paste("Institute address:", gsub("\n", ", ", scalar_text(project$invoice_institute_address, MS_INSTITUTE_ADDRESS))),
      "",
      paste("Project cost:", ifelse(is.na(total_cost), "To be confirmed", sprintf("EUR %.2f", total_cost))),
      paste("Additional cost:", ifelse(is.na(additional_cost), "0.00", sprintf("EUR %.2f", additional_cost))),
      "",
      lines
    )
  }

  if (!is.null(files) && nrow(files) > 0) {
    lines <- c(lines, "", "Uploaded files:")
    for (i in seq_len(nrow(files))) {
      lines <- c(lines, paste(files$file_type[i], files$original_name[i], files$path[i], sep = " | "))
    }
  }

  draw_pdf_lines(lines, title, file)
}

write_sample_handoff_pdf <- function(project, samples, file) {
  lines <- c(
    paste("Project:", scalar_text(project$project_code), "-", scalar_text(project$project_name)),
    paste("Project type:", scalar_text(project$project_type_name, scalar_text(project$project_type))),
    paste("Responsible user:", scalar_text(project$responsible_user)),
    paste("Submission date:", scalar_text(project$submission_date)),
    "",
    "Tube / ID | Condition | Replicate | Control? | Description / Comments"
  )

  if (!is.null(samples) && nrow(samples) > 0) {
    for (i in seq_len(nrow(samples))) {
      lines <- c(lines, paste(
        samples$tube_id[i] %||% "",
        samples$condition[i] %||% "",
        samples$replicate[i] %||% "",
        ifelse(as.integer(samples$is_control[i] %||% 0) == 1, "Y", "N"),
        samples$description[i] %||% "",
        sep = " | "
      ))
    }
  }

  lines <- c(lines, "", MS_SAMPLE_TABLE_STATEMENT, "", MS_SUBMISSION_WARNING)
  draw_pdf_lines(lines, "MS Sample Handoff Sheet", file)
}

log_email <- function(con, project_id, recipients, subject, status, error_message = "") {
  dbExecute(con, "
    INSERT INTO email_logs (project_id, sent_to, subject, sent_at, status, error_message)
    VALUES (?, ?, ?, CURRENT_TIMESTAMP, ?, ?)
  ", params = list(
    project_id %||% NA,
    paste(recipients, collapse = ", "),
    subject,
    status,
    error_message
  ))
}

mail_smtp_config <- function() {
  host <- first_non_empty(
    Sys.getenv("SMTP_HOST", ""),
    Sys.getenv("MS_SMTP_HOST", ""),
    "msx.biochem.mpg.de"
  )
  port <- suppressWarnings(as.numeric(first_non_empty(
    Sys.getenv("SMTP_PORT", ""),
    Sys.getenv("MS_SMTP_PORT", ""),
    "25"
  )))
  if (is.na(port)) port <- 25
  username <- first_non_empty(Sys.getenv("SMTP_USER", ""), Sys.getenv("MS_SMTP_USER", ""))
  password <- first_non_empty(Sys.getenv("SMTP_PASSWORD", ""), Sys.getenv("MS_SMTP_PASSWORD", ""))

  smtp <- list(
    host.name = host,
    port = port,
    ssl = to_bool(first_non_empty(Sys.getenv("SMTP_SSL", ""), Sys.getenv("MS_SMTP_SSL", ""), "0")),
    tls = to_bool(first_non_empty(Sys.getenv("SMTP_TLS", ""), Sys.getenv("MS_SMTP_TLS", ""), "0"))
  )
  authenticate <- nzchar(username)
  if (authenticate) {
    smtp$user.name <- username
    smtp$passwd <- password
  }

  list(smtp = smtp, authenticate = authenticate)
}

normalize_recipients <- function(recipients) {
  recipients <- unique(trimws(as.character(recipients %||% character())))
  recipients <- recipients[!is.na(recipients) & nzchar(recipients)]
  recipients[vapply(recipients, is_email_like, logical(1))]
}

recipient_domain_allowed <- function(recipient) {
  allowed <- trimws(unlist(strsplit(Sys.getenv("MS_EMAIL_ALLOWED_DOMAINS", ""), ",")))
  allowed <- tolower(allowed[nzchar(allowed)])
  if (length(allowed) == 0) return(TRUE)
  domain <- sub("^.*@", "", tolower(trim_scalar(recipient)))
  domain %in% allowed
}

send_mail_safe <- function(con, project_id, recipients, subject, body, attachments = NULL, html = FALSE) {
  requested_recipients <- unique(trimws(as.character(recipients %||% character())))
  requested_recipients <- requested_recipients[!is.na(requested_recipients) & nzchar(requested_recipients)]
  invalid_recipients <- requested_recipients[!vapply(requested_recipients, is_email_like, logical(1))]
  recipients <- normalize_recipients(requested_recipients)
  blocked_recipients <- recipients[!vapply(recipients, recipient_domain_allowed, logical(1))]
  recipients <- recipients[vapply(recipients, recipient_domain_allowed, logical(1))]
  attachments <- as.character(attachments %||% character())
  attachments <- attachments[!is.na(attachments) & nzchar(attachments) & file.exists(attachments)]

  if (length(invalid_recipients) > 0) {
    log_email(con, project_id, invalid_recipients, subject, "skipped", "Invalid email address")
  }
  if (length(blocked_recipients) > 0) {
    log_email(con, project_id, blocked_recipients, subject, "skipped", "Recipient domain not allowed by MS_EMAIL_ALLOWED_DOMAINS")
  }

  if (length(recipients) == 0) {
    log_email(con, project_id, requested_recipients, subject, "skipped", "No valid recipients")
    return(list(success = FALSE, error = "No recipients"))
  }

  if (!isTRUE(mail_available) || Sys.getenv("MS_DISABLE_EMAIL", "0") == "1") {
    log_email(con, project_id, recipients, subject, "skipped", "mailR unavailable or email disabled")
    return(list(success = TRUE, skipped = TRUE))
  }

  smtp_config <- mail_smtp_config()

  results <- lapply(recipients, function(recipient) {
    tryCatch({
    mail_args <- list(
      from = MS_MAIL_FROM,
      to = recipient,
      subject = subject,
      body = body,
      smtp = smtp_config$smtp,
      authenticate = smtp_config$authenticate,
      send = TRUE,
      encoding = "utf-8",
      html = isTRUE(html)
    )
    if (length(attachments) > 0) mail_args$attach.files <- attachments
    do.call(mailR::send.mail, mail_args)
    log_email(con, project_id, recipient, subject, "sent", "")
    list(success = TRUE, recipient = recipient)
  }, error = function(e) {
    log_email(con, project_id, recipient, subject, "failed", conditionMessage(e))
    list(success = FALSE, recipient = recipient, error = conditionMessage(e))
  })
  })

  successes <- vapply(results, function(x) isTRUE(x$success), logical(1))
  if (any(successes)) {
    return(list(
      success = TRUE,
      partial = any(!successes) || length(invalid_recipients) > 0 || length(blocked_recipients) > 0,
      failed = results[!successes],
      skipped = c(invalid_recipients, blocked_recipients)
    ))
  }

  first_error <- results[[1]]$error %||% "Email sending failed for all recipients"
  list(success = FALSE, error = first_error, failed = results)
}

resolve_project_user_email <- function(con, project) {
  candidates <- c(
    scalar_text(project$submitter_email),
    if (is_email_like(project$responsible_user)) trim_scalar(project$responsible_user) else ""
  )

  responsible <- trim_scalar(project$responsible_user)
  if (nzchar(responsible)) {
    rows <- dbGetQuery(con, "
      SELECT email
      FROM users
      WHERE lower(username) = lower(?)
         OR lower(trim(full_name)) = lower(trim(?))
      LIMIT 1
    ", params = list(responsible, responsible))
    if (nrow(rows) > 0) candidates <- c(candidates, rows$email[[1]])
  }

  user_id <- suppressWarnings(as.integer(project$user_id[[1]] %||% NA_integer_))
  if (!is.na(user_id)) {
    rows <- dbGetQuery(con, "SELECT email FROM users WHERE id = ? LIMIT 1", params = list(user_id))
    if (nrow(rows) > 0) candidates <- c(candidates, rows$email[[1]])
  }

  normalize_recipients(candidates)
}

project_data_location <- function(project) {
  folder <- trim_scalar(project$project_folder)
  if (nzchar(folder)) return(folder)
  root <- trim_scalar(project$upload_root, MS_UPLOAD_ROOT)
  if (!nzchar(root)) root <- MS_UPLOAD_ROOT
  file.path(root, sanitize_path_part(scalar_text(project$project_code, "project")))
}

send_ms_submission_email <- function(con, project, samples, sample_table_path, handoff_pdf = NULL) {
  recipients <- unique(c(resolve_project_user_email(con, project), MS_FACILITY_EMAIL))
  subject <- paste(project$project_code, "MS project submitted", sep = " - ")
  body <- paste(
    "A new Mass Spectrometry Core Facility project was submitted.",
    "",
    "The tab-delimited sample overview table is attached.",
    "",
    MS_SUBMISSION_WARNING,
    "",
    project_summary_text(project, samples),
    "",
    MS_SAMPLE_TABLE_STATEMENT,
    sep = "\n"
  )
  send_mail_safe(con, project$id[[1]], recipients, subject, body, c(sample_table_path, handoff_pdf))
}

send_ms_cost_estimation_email <- function(con, project) {
  recipients <- unique(c(resolve_project_user_email(con, project), scalar_text(project$budget_email)))
  cost_text <- format_cost_value(project$total_cost)
  additional_text <- format_cost_value(project$additional_cost)
  show_additional <- nzchar(trim_scalar(project$additional_cost))
  subject <- paste(project$project_code, "MS project cost estimation", sep = " - ")

  body <- paste0(
    "<html><body style='font-family: Arial, sans-serif; color: #263238;'>",
    "<p>Dear user,</p>",
    "<p>The Mass Spectrometry Core Facility has reviewed your project.</p>",
    "<p><strong>Project:</strong> ", html_escape(project$project_code), " - ", html_escape(project$project_name), "<br>",
    "<strong>Budget holder:</strong> ", html_escape(paste(trim_scalar(project$budget_name), trim_scalar(project$budget_surname))), "<br>",
    "<strong>Cost center:</strong> ", html_escape(project$cost_center), "</p>",
    "<p style='color:#c00000; font-weight:700;'>Total Cost: ", html_escape(cost_text), "</p>",
    if (show_additional) paste0("<p>Additional Cost: ", html_escape(additional_text), "</p>") else "",
    "<p>Please contact the facility if the estimate does not match the planned work.</p>",
    "<p>Best regards,<br>Mass Spectrometry Core Facility</p>",
    "</body></html>"
  )

  send_mail_safe(con, project$id[[1]], recipients, subject, body, html = TRUE)
}

send_ms_data_released_email <- function(con, project) {
  recipients <- resolve_project_user_email(con, project)
  subject <- paste(project$project_code, "MS data released", sep = " - ")
  data_path <- project_data_location(project)
  body <- paste(
    paste0("Dear ", scalar_text(project$responsible_user, "user"), ","),
    "",
    paste0("Data for Mass Spectrometry Core Facility project ", scalar_text(project$project_code), " (", scalar_text(project$project_name), ") are now available."),
    "",
    "You can find the data in the project folder:",
    data_path,
    "",
    "Please contact the MS Core Facility if you have questions about the released data.",
    "",
    "Best regards,",
    "Mass Spectrometry Core Facility",
    sep = "\n"
  )

  send_mail_safe(con, project$id[[1]], recipients, subject, body)
}

validate_fasta_file <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, n = 500), error = function(e) character())
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0 || !any(startsWith(lines, ">"))) {
    return("FASTA files must contain at least one header line starting with '>'.")
  }
  seq_lines <- lines[!startsWith(lines, ">")]
  if (length(seq_lines) > 0 && any(!grepl("^[A-Za-z*.-]+$", gsub("\\s+", "", seq_lines)))) {
    return("FASTA sequence lines contain unexpected characters.")
  }
  NULL
}

validate_text_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = 4096)
  if (length(bytes) > 0 && any(bytes == as.raw(0))) {
    return("Text upload appears to contain binary data.")
  }
  NULL
}

validate_uploads <- function(upload, allowed_ext, max_mb, fasta = FALSE, text_only = FALSE) {
  if (is.null(upload) || nrow(upload) == 0) return(character())
  errors <- character()
  max_bytes <- max_mb * 1024^2
  for (i in seq_len(nrow(upload))) {
    original <- upload$name[i]
    ext <- safe_file_ext(original)
    if (!(ext %in% allowed_ext)) {
      errors <- c(errors, paste0(original, ": allowed extensions are .", paste(allowed_ext, collapse = ", .")))
      next
    }
    if (!is.na(upload$size[i]) && upload$size[i] > max_bytes) {
      errors <- c(errors, paste0(original, ": file is larger than ", max_mb, " MB."))
      next
    }
    if (isTRUE(fasta)) {
      fasta_error <- validate_fasta_file(upload$datapath[i])
      if (!is.null(fasta_error)) errors <- c(errors, paste(original, fasta_error))
    }
    if (isTRUE(text_only) && ext %in% c("txt", "csv", "tsv")) {
      text_error <- validate_text_file(upload$datapath[i])
      if (!is.null(text_error)) errors <- c(errors, paste(original, text_error))
    }
  }
  errors
}

sample_table_template_headers <- function(project_type) {
  path <- template_path_for_project_type(project_type)
  if (!file.exists(path)) return(sample_table_required_columns)

  headers <- tryCatch(
    names(utils::read.table(
      path,
      header = TRUE,
      sep = "\t",
      nrows = 0,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )),
    error = function(e) character()
  )
  if (length(headers) == 0) sample_table_required_columns else headers
}

detect_table_separator <- function(path) {
  first_line <- tryCatch(readLines(path, warn = FALSE, n = 1), error = function(e) "")
  if (length(first_line) == 0) first_line <- ""
  separators <- c("\t", ",", ";")
  counts <- vapply(
    separators,
    function(separator) lengths(regmatches(first_line, gregexpr(separator, first_line, fixed = TRUE))),
    integer(1)
  )
  separators[[which.max(counts)]]
}

read_sample_table_upload <- function(upload, project_type, num_samples) {
  if (is.null(upload) || nrow(upload) == 0) {
    return(list(errors = "Sample overview table upload is required.", table = NULL))
  }

  original <- upload$name[[1]]
  ext <- safe_file_ext(original)
  max_mb <- as.numeric(Sys.getenv("MS_MAX_TEXT_MB", "20"))
  max_bytes <- max_mb * 1024^2
  errors <- character()

  if (ext %in% c("xls", "xlsx")) {
    return(list(
      errors = paste0(original, ": Excel files are not accepted here. Please export the table as .txt, .csv, or .tsv."),
      table = NULL
    ))
  }
  if (!(ext %in% c("txt", "csv", "tsv"))) {
    return(list(errors = paste0(original, ": allowed extensions are .txt, .csv, or .tsv."), table = NULL))
  }
  if (!is.na(upload$size[[1]]) && upload$size[[1]] > max_bytes) {
    errors <- c(errors, paste0(original, ": file is larger than ", max_mb, " MB."))
  }

  text_error <- validate_text_file(upload$datapath[[1]])
  if (!is.null(text_error)) errors <- c(errors, paste(original, text_error))
  if (length(errors) > 0) return(list(errors = errors, table = NULL))

  sep <- detect_table_separator(upload$datapath[[1]])
  table <- tryCatch(
    utils::read.table(
      upload$datapath[[1]],
      header = TRUE,
      sep = sep,
      quote = "\"",
      comment.char = "",
      check.names = FALSE,
      stringsAsFactors = FALSE,
      na.strings = character(),
      strip.white = TRUE,
      fill = FALSE
    ),
    error = function(e) e
  )
  if (inherits(table, "error")) {
    return(list(errors = paste(original, "could not be parsed:", conditionMessage(table)), table = NULL))
  }

  expected <- sample_table_template_headers(project_type)
  if (!identical(names(table), expected)) {
    errors <- c(errors, paste0(
      "Sample overview table columns must be exactly: ",
      paste(expected, collapse = ", "),
      "."
    ))
  }

  expected_rows <- suppressWarnings(as.integer(num_samples))
  if (is.na(expected_rows) || expected_rows < 1) {
    errors <- c(errors, "Number of samples must be at least 1 before the sample table can be validated.")
  } else if (nrow(table) == 0 && expected_rows > 0) {
    errors <- c(errors, paste0("The uploaded sample overview table contains no sample rows. Add ", expected_rows, " sample row", ifelse(expected_rows == 1, "", "s"), " below the header, or adjust Number of samples."))
  } else if (nrow(table) != expected_rows) {
    errors <- c(errors, paste0("Sample overview table has ", nrow(table), " rows, but Number of samples is ", expected_rows, "."))
  }

  sample_table_column <- function(table, column_name) {
    if (column_name %in% names(table)) as.character(table[[column_name]]) else rep("", nrow(table))
  }

  if ("Tube_ID" %in% names(table)) {
    tube_ids <- trimws(sample_table_column(table, "Tube_ID"))
    if (any(!nzchar(tube_ids))) errors <- c(errors, "Every sample row needs a Tube_ID.")
    duplicates <- unique(tube_ids[duplicated(tube_ids) & nzchar(tube_ids)])
    if (length(duplicates) > 0) {
      errors <- c(errors, paste("Tube_ID values must be unique. Duplicates:", paste(duplicates, collapse = ", ")))
    }
    table[["Tube_ID"]] <- tube_ids
  }

  if ("Control?" %in% names(table)) {
    controls <- trimws(sample_table_column(table, "Control?"))
    invalid_controls <- unique(controls[!tolower(controls) %in% c("yes", "no")])
    invalid_controls <- invalid_controls[nzchar(invalid_controls)]
    if (length(invalid_controls) > 0) {
      errors <- c(errors, paste("Control? accepts only Yes or No. Invalid values:", paste(invalid_controls, collapse = ", ")))
    }
    if (any(!nzchar(controls))) errors <- c(errors, "Every sample row needs Control? set to Yes or No.")
    table[["Control?"]] <- ifelse(tolower(controls) == "yes", "Yes", ifelse(tolower(controls) == "no", "No", controls))
  }

  for (col in names(table)) {
    table[[col]] <- trimws(as.character(table[[col]]))
  }

  list(errors = errors[nzchar(errors)], table = if (length(errors) > 0) NULL else table)
}

sample_rows_from_table <- function(sample_table) {
  if (is.null(sample_table) || nrow(sample_table) == 0) {
    return(data.frame(
      row_index = integer(),
      tube_id = character(),
      condition = character(),
      replicate = character(),
      is_control = integer(),
      description = character(),
      stringsAsFactors = FALSE
    ))
  }

  description <- trimws(sample_table[["Description"]] %||% "")
  comments <- trimws(sample_table[["Comments"]] %||% "")
  combined_description <- ifelse(
    nzchar(description) & nzchar(comments),
    paste(description, comments, sep = " | "),
    ifelse(nzchar(description), description, comments)
  )

  data.frame(
    row_index = seq_len(nrow(sample_table)),
    tube_id = sample_table[["Tube_ID"]],
    condition = sample_table[["Condition"]] %||% "",
    replicate = sample_table[["Replicate"]] %||% "",
    is_control = as.integer(tolower(sample_table[["Control?"]]) == "yes"),
    description = combined_description,
    stringsAsFactors = FALSE
  )
}

write_normalized_sample_table <- function(sample_table, file) {
  utils::write.table(
    sample_table,
    file = file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    na = ""
  )
}

copy_file_maybe_gzip <- function(source, destination, compress = FALSE) {
  if (!compress) {
    file.copy(source, destination, overwrite = TRUE)
    return(destination)
  }

  in_con <- file(source, "rb")
  out_con <- gzfile(destination, "wb")
  on.exit(close(in_con), add = TRUE)
  on.exit(close(out_con), add = TRUE)
  repeat {
    chunk <- readBin(in_con, what = "raw", n = 1024 * 1024)
    if (length(chunk) == 0) break
    writeBin(chunk, out_con)
  }
  destination
}

prepare_project_folder <- function(project_code, project_name) {
  folder_name <- paste(sanitize_path_part(project_code), sanitize_path_part(project_name), sep = "_")
  root <- MS_UPLOAD_ROOT
  status <- "pool"

  if (!dir.exists(root)) {
    root <- MS_LOCAL_UPLOAD_FALLBACK
    status <- "pool_missing_fallback"
  }

  folder <- file.path(root, folder_name)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(folder)) {
    folder <- file.path(MS_LOCAL_UPLOAD_FALLBACK, folder_name)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    status <- "fallback"
  }

  list(root = root, folder = normalizePath(folder, mustWork = FALSE), status = status)
}

store_upload_set <- function(con, project_id, upload, file_type, folder, username, compress = TRUE) {
  if (is.null(upload) || nrow(upload) == 0) return(character())
  saved <- character()
  for (i in seq_len(nrow(upload))) {
    original <- upload$name[i]
    ext <- safe_file_ext(original)
    stem <- sanitize_path_part(file_path_sans_ext(original))
    stored_name <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", i, "_", stem, ".", ext)
    if (compress) stored_name <- paste0(stored_name, ".gz")
    destination <- file.path(folder, stored_name)
    copy_file_maybe_gzip(upload$datapath[i], destination, compress = compress)
    checksum <- digest::digest(file = destination, algo = "sha256")
    insert_record(con, "project_files", list(
      project_id = project_id,
      file_type = file_type,
      original_name = original,
      stored_name = stored_name,
      path = normalizePath(destination, mustWork = FALSE),
      size_bytes = file.info(destination)$size %||% upload$size[i],
      mime_type = upload$type[i] %||% "",
      checksum = checksum,
      uploaded_by = username
    ))
    saved <- c(saved, normalizePath(destination, mustWork = FALSE))
  }
  saved
}

store_sample_table_upload <- function(con, project_id, upload, sample_table, project_type, folder, username) {
  if (is.null(sample_table) || nrow(sample_table) == 0) return(character())

  original <- if (!is.null(upload) && nrow(upload) > 0) upload$name[[1]] else paste0(project_type, "_sample_table.txt")
  stored_name <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_sample_table_",
    sanitize_path_part(project_type),
    ".txt"
  )
  destination <- file.path(folder, stored_name)
  write_normalized_sample_table(sample_table, destination)
  checksum <- digest::digest(file = destination, algo = "sha256")

  insert_record(con, "project_files", list(
    project_id = project_id,
    file_type = "sample_table",
    original_name = original,
    stored_name = stored_name,
    path = normalizePath(destination, mustWork = FALSE),
    size_bytes = file.info(destination)$size,
    mime_type = "text/tab-separated-values",
    checksum = checksum,
    uploaded_by = username
  ))

  normalizePath(destination, mustWork = FALSE)
}

load_project_detail <- function(con, project_id) {
  dbGetQuery(con, "
    SELECT p.*, pt.name AS project_type_name, pt.color AS project_type_color,
           bh.name AS budget_name, bh.surname AS budget_surname,
           bh.cost_center, bh.email AS budget_email
    FROM projects p
    LEFT JOIN project_types pt ON p.project_type = pt.slug
    LEFT JOIN budget_holders bh ON p.budget_id = bh.id
    WHERE p.id = ?
    LIMIT 1
  ", params = list(project_id))
}

load_project_samples <- function(con, project_id) {
  dbGetQuery(con, "
    SELECT id, row_index, tube_id, condition, replicate, is_control, description
    FROM project_samples
    WHERE project_id = ?
    ORDER BY row_index
  ", params = list(project_id))
}

load_project_files <- function(con, project_id) {
  dbGetQuery(con, "
    SELECT id, file_type, original_name, stored_name, path, size_bytes, uploaded_by, created_at
    FROM project_files
    WHERE project_id = ?
    ORDER BY created_at DESC
  ", params = list(project_id))
}

ui <- fluidPage(
  tags$head(
    includeCSS("styles.css"),
    tags$script(HTML("
      function publishUrlSearch() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('client_url_search', window.location.search || '', {priority: 'event'});
        }
      }

      document.addEventListener('shiny:connected', publishUrlSearch);
      document.addEventListener('DOMContentLoaded', function() {
        window.setTimeout(publishUrlSearch, 0);
      });
      window.addEventListener('popstate', publishUrlSearch);

      (function() {
        var activeHelp = null;
        var helpPopover = null;

        function ensureHelpPopover() {
          if (helpPopover) return helpPopover;
          helpPopover = document.getElementById('field-help-popover');
          if (helpPopover) return helpPopover;
          helpPopover = document.createElement('div');
          helpPopover.id = 'field-help-popover';
          helpPopover.className = 'field-help-popover';
          helpPopover.setAttribute('role', 'tooltip');
          helpPopover.hidden = true;
          document.body.appendChild(helpPopover);
          return helpPopover;
        }

        function closeHelpPopover() {
          if (activeHelp) {
            activeHelp.classList.remove('is-open');
            activeHelp.setAttribute('aria-expanded', 'false');
          }
          if (helpPopover) helpPopover.hidden = true;
          activeHelp = null;
        }

        function positionHelpPopover(trigger) {
          var popover = ensureHelpPopover();
          var triggerRect = trigger.getBoundingClientRect();
          var popoverRect = popover.getBoundingClientRect();
          var gutter = 12;
          var left = triggerRect.left + (triggerRect.width - popoverRect.width) / 2;
          left = Math.max(gutter, Math.min(left, window.innerWidth - popoverRect.width - gutter));
          var top = triggerRect.top - popoverRect.height - 10;
          if (top < gutter) top = triggerRect.bottom + 10;
          popover.style.left = Math.round(left) + 'px';
          popover.style.top = Math.round(top) + 'px';
        }

        function openHelpPopover(trigger) {
          closeHelpPopover();
          var popover = ensureHelpPopover();
          popover.textContent = trigger.getAttribute('data-help') || '';
          popover.hidden = false;
          activeHelp = trigger;
          trigger.classList.add('is-open');
          trigger.setAttribute('aria-expanded', 'true');
          positionHelpPopover(trigger);
        }

        document.addEventListener('click', function(event) {
          var trigger = event.target && event.target.closest
            ? event.target.closest('.field-help')
            : null;
          if (!trigger) {
            closeHelpPopover();
            return;
          }
          event.preventDefault();
          event.stopPropagation();
          if (trigger === activeHelp) closeHelpPopover();
          else openHelpPopover(trigger);
        });

        document.addEventListener('keydown', function(event) {
          var trigger = event.target && event.target.closest
            ? event.target.closest('.field-help')
            : null;
          if (trigger && (event.key === 'Enter' || event.key === ' ')) {
            event.preventDefault();
            if (trigger === activeHelp) closeHelpPopover();
            else openHelpPopover(trigger);
          } else if (event.key === 'Escape' && activeHelp) {
            var previousHelp = activeHelp;
            closeHelpPopover();
            previousHelp.focus();
          }
        });

        window.addEventListener('resize', closeHelpPopover);
        document.addEventListener('scroll', closeHelpPopover, true);
      })();

      document.addEventListener('keyup', function(event) {
        var target = event.target;
        if (!target || (target.id !== 'login_username' && target.id !== 'login_password')) return;
        if (event.key === 'Enter' || event.keyCode === 13) {
          event.preventDefault();
          var username = document.getElementById('login_username');
          var password = document.getElementById('login_password');
          if (window.Shiny && username && password) {
            Shiny.setInputValue('login_username', username.value, {priority: 'event'});
            Shiny.setInputValue('login_password', password.value, {priority: 'event'});
          }
          window.setTimeout(function() {
            var loginButton = document.getElementById('login_btn');
            if (loginButton) loginButton.click();
          }, 25);
        }
      });

      (function() {
        var savedUploadScroll = null;
        var restoreUploadUntil = 0;

        function uploadAnchor() {
          return document.getElementById('sample_overview_section') || document.querySelector('.sample-upload-actions');
        }

        function scrollTargetsFor(target) {
          var targets = [];
          var add = function(element) {
            if (element && targets.indexOf(element) === -1) targets.push(element);
          };
          if (target && target.closest) {
            add(target.closest('.modal-body'));
            add(target.closest('.modal'));
          }
          add(document.scrollingElement);
          add(document.documentElement);
          add(document.body);
          return targets;
        }

        function currentScroll(target) {
          return {
            targets: scrollTargetsFor(target).map(function(element) {
              return {
                element: element,
                top: element.scrollTop || 0,
                left: element.scrollLeft || 0
              };
            })
          };
        }

        function isUploadTrigger(target) {
          if (!target || !target.closest) return false;
          if (target.matches && target.matches('input[type=\"file\"]')) return true;
          var uploadArea = target.closest('.sample-upload-actions');
          return !!(uploadArea && uploadArea.querySelector('input[type=\"file\"]'));
        }

        function rememberUploadScroll(target) {
          if (!isUploadTrigger(target)) return;
          savedUploadScroll = currentScroll(target);
          restoreUploadUntil = Date.now() + 3000;
        }

        function restoreUploadScroll() {
          if (!savedUploadScroll) return;
          var state = savedUploadScroll;
          var apply = function() {
            state.targets.forEach(function(item) {
              if (!item.element) return;
              item.element.scrollTop = item.top;
              item.element.scrollLeft = item.left;
            });
            var anchor = uploadAnchor();
            if (anchor && anchor.scrollIntoView) {
              anchor.scrollIntoView({block: 'center', inline: 'nearest'});
            }
          };
          apply();
          if (Date.now() < restoreUploadUntil) {
            window.setTimeout(function() {
              apply();
              if (Date.now() < restoreUploadUntil) restoreUploadScroll();
            }, 75);
          }
        }

        ['pointerdown', 'mousedown', 'click', 'focusin'].forEach(function(eventName) {
          document.addEventListener(eventName, function(event) {
            rememberUploadScroll(event.target);
          }, true);
        });

        document.addEventListener('change', function(event) {
          if (isUploadTrigger(event.target)) {
            restoreUploadUntil = Date.now() + 3000;
            restoreUploadScroll();
          }
        }, true);

        window.addEventListener('focus', function() {
          if (savedUploadScroll) {
            restoreUploadUntil = Date.now() + 1500;
            restoreUploadScroll();
          }
        });

        document.addEventListener('shiny:inputchanged', function(event) {
          if (event.name && event.name.indexOf('sample_table_upload') !== -1) {
            restoreUploadUntil = Date.now() + 3000;
            restoreUploadScroll();
          }
        });

        document.addEventListener('shiny:value', function(event) {
          if (event.name === 'sample_table_validation_preview') {
            restoreUploadUntil = Date.now() + 1500;
            restoreUploadScroll();
          }
        });

        ['pointerdown', 'mousedown', 'click'].forEach(function(eventName) {
          document.addEventListener(eventName, function(event) {
            if (event.target && event.target.closest && event.target.closest('.template-card-download')) {
              event.stopPropagation();
            }
          }, true);
        });
      })();
    "))
  ),
  uiOutput("app_ui"),
  tags$div(
    id = "field-help-popover",
    class = "field-help-popover",
    role = "tooltip",
    hidden = "hidden"
  )
)

server <- function(input, output, session) {
  user <- reactiveValues(
    logged_in = FALSE,
    user_id = NULL,
    username = NULL,
    full_name = NULL,
    email = NULL,
    phone = NULL,
    research_group = NULL,
    cost_center = NULL,
    role = "user",
    is_admin = FALSE
  )

  projects_data <- reactiveVal(data.frame())
  admin_refresh <- reactiveVal(0)
  editing_project_id <- reactiveVal(NULL)
  admin_edit_user_id <- reactiveVal(NULL)
  admin_edit_bh_id <- reactiveVal(NULL)
  admin_edit_organism_id <- reactiveVal(NULL)
  admin_edit_option_id <- reactiveVal(NULL)
  admin_edit_landing_id <- reactiveVal(NULL)
  client_url_search <- reactiveVal("")
  ldap_warned <- reactiveVal(FALSE)

  if (to_bool(Sys.getenv("LDAP_DEBUG", "0"))) {
    cat(
      "LDAP DEBUG: startup env",
      "AUTH_MODE=", Sys.getenv("AUTH_MODE", "<unset>"),
      "TRUST_PROXY_AUTH_USER_QUERY=", Sys.getenv("TRUST_PROXY_AUTH_USER_QUERY", "<unset>"),
      "\n",
      file = stderr()
    )
    flush.console()
  }

  observeEvent(input$client_url_search, {
    client_url_search(trim_scalar(input$client_url_search))
  }, ignoreInit = FALSE)

  get_req_header <- function(req, header_name) {
    if (is.null(req) || is.null(header_name) || !nzchar(header_name)) return("")

    key_upper <- toupper(gsub("-", "_", header_name))
    env_key <- paste0("HTTP_", key_upper)
    candidates <- c(req[[env_key]], req[[key_upper]])

    hdrs <- req$HEADERS
    if (is.null(hdrs) || !is.list(hdrs)) hdrs <- req$headers
    if (is.list(hdrs)) {
      candidates <- c(
        candidates,
        hdrs[[tolower(header_name)]],
        hdrs[[header_name]],
        hdrs[[key_upper]]
      )
    }

    for (value in candidates) {
      value <- trim_scalar(value)
      if (nzchar(value)) return(value)
    }
    ""
  }

  get_proxy_username <- function() {
    req <- session$request
    candidates <- c(
      get_req_header(req, "x-remote-user"),
      get_req_header(req, "remote-user"),
      scalar_text(req$REMOTE_USER, ""),
      get_req_header(req, "x-forwarded-user")
    )

    query_user <- ""
    query_candidates <- unique(c(
      client_url_search(),
      scalar_text(session$clientData$url_search, ""),
      scalar_text(req$QUERY_STRING, "")
    ))
    query_candidates <- query_candidates[nzchar(trimws(query_candidates))]
    if (trust_proxy_auth_user_query()) {
      for (query in query_candidates) {
        parsed <- parseQueryString(query)
        if (!is.null(parsed$auth_user) && nzchar(trim_scalar(parsed$auth_user))) {
          query_user <- trim_scalar(parsed$auth_user)
          break
        }
      }
    }
    if (nzchar(query_user)) {
      candidates <- c(candidates, query_user)
    }

    candidates <- trimws(candidates)
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) == 0) "" else candidates[1]
  }

  apply_user_row <- function(row) {
    role <- ms_normalize_user_role(row$role[[1]] %||% "", row$is_admin[[1]] %||% 0L)
    user$logged_in <- TRUE
    user$user_id <- row$id[[1]]
    user$username <- row$username[[1]]
    user$full_name <- row$full_name[[1]] %||% row$username[[1]]
    user$email <- row$email[[1]]
    user$phone <- row$phone[[1]] %||% ""
    user$research_group <- row$research_group[[1]] %||% ""
    user$cost_center <- row$cost_center[[1]] %||% ""
    user$role <- role
    user$is_admin <- role_is_admin(role)
    load_projects()
  }

  current_user_role <- function() {
    ms_normalize_user_role(user$role %||% "user", as.integer(isTRUE(user$is_admin)))
  }

  can_view_admin_panels <- function() {
    role_is_admin(current_user_role())
  }

  can_edit_all_projects <- function() {
    role_can_edit_projects(current_user_role())
  }

  can_delete_projects <- function() {
    role_is_admin(current_user_role())
  }

  proxy_login_attempted <- reactiveVal(FALSE)
  observe({
    if (isTRUE(user$logged_in) || isTRUE(proxy_login_attempted())) return()
    if (!is_ldap_mode()) return()
    proxy_username <- get_proxy_username()
    if (!nzchar(proxy_username)) {
      if (!isTRUE(ldap_warned()) && to_bool(Sys.getenv("LDAP_DEBUG", "0"))) {
        req <- session$request
        cat(
          "LDAP DEBUG: LDAP mode active but no authenticated user found",
          "X_REMOTE_USER=", get_req_header(req, "x-remote-user") %||% "<missing>",
          "REMOTE_USER=", scalar_text(req$REMOTE_USER, "<missing>"),
          "X_FORWARDED_USER=", get_req_header(req, "x-forwarded-user") %||% "<missing>",
          "CLIENT_URL_SEARCH=", client_url_search() %||% "<missing>",
          "SESSION_URL_SEARCH=", scalar_text(session$clientData$url_search, "<missing>"),
          "REQ_QUERY_STRING=", scalar_text(req$QUERY_STRING, "<missing>"),
          "\n",
          file = stderr()
        )
        flush.console()
      }
      ldap_warned(TRUE)
      return()
    }

    proxy_login_attempted(TRUE)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    row <- ensure_ldap_user(con, proxy_username)
    apply_user_row(row)
  })

  login_panel <- function() {
    ldap_mode <- is_ldap_mode()
    div(
      class = "login-page",
      div(
        class = "login-box",
        h2(if (ldap_mode) "LDAP Authentication" else "Mass Spectrometry Project Submission"),
        guideline_panel(),
        if (!is.null(startup_db_error)) {
          div(class = "error-box", strong("Database issue detected: "), startup_db_error)
        },
        if (ldap_mode) {
          div(
            class = "auth-status-box",
            strong("LDAP proxy mode is active."),
            p("Waiting for Apache to pass the authenticated user to Shiny."),
            if (trust_proxy_auth_user_query()) {
              p("For local proxy simulation, reopen this app URL with ?auth_user=<username>.")
            },
            div(
              class = "auth-env",
              tags$span("AUTH_MODE=", tags$code(auth_mode())),
              tags$span("TRUST_PROXY_AUTH_USER_QUERY=", tags$code(Sys.getenv("TRUST_PROXY_AUTH_USER_QUERY", "0")))
            )
          )
        } else {
          tagList(
            textInput("login_username", "Username", placeholder = "Local test user or MPIB username"),
            passwordInput("login_password", "Password", placeholder = "Password"),
            actionButton("login_btn", "Login", class = "btn btn-primary login-button")
          )
        }
      )
    )
  }

  main_panel <- function() {
    tabs <- list(
      tabPanel(
        "Projects",
        uiOutput("project_landing_text"),
        warning_banner(compact = TRUE),
        div(
          class = "action-bar",
          actionButton("new_project_btn", "Create New Project", class = "btn btn-primary"),
          actionButton("edit_project_btn", "Edit Selected", class = "btn btn-secondary"),
          downloadButton("download_report_btn", "Project Report"),
          downloadButton("download_invoice_btn", "Invoice"),
          if (can_delete_projects()) actionButton("delete_project_btn", "Delete Selected", class = "btn btn-danger")
        ),
        uiOutput("project_type_filter_ui"),
        DTOutput("projects_table")
      )
    )

    if (can_view_admin_panels()) {
      tabs <- c(tabs, list(tabPanel("Admin", uiOutput("admin_interface"))))
    }

    div(
      id = "main_app",
      div(
        class = "app-header",
        div(class = "header-left", img(src = "logo.png", class = "header-logo"), "Mass Spectrometry Core Facility"),
        div(class = "header-title", "MS Project Management"),
        div(
          class = "header-right",
          span(paste("Signed in as", user$username)),
          if (role_is_admin(current_user_role())) span(class = "admin-chip", "Admin"),
          if (identical(current_user_role(), "technician")) span(class = "admin-chip", "Technician"),
          actionButton("logout_btn", "End Session", class = "btn btn-light")
        )
      ),
      div(class = "main-content", do.call(tabsetPanel, c(list(id = "main_tabs"), tabs))),
      div(class = "app-footer", img(src = "bioinfo_core_concept_01_lockup.svg", class = "footer-logo"))
    )
  }

  output$app_ui <- renderUI({
    if (!isTRUE(user$logged_in)) login_panel() else main_panel()
  })

  output$project_landing_text <- renderUI({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    render_landing_text_blocks(load_landing_text_blocks(con))
  })

  observeEvent(input$login_btn, {
    if (is_ldap_mode()) {
      showNotification("Local password login is disabled in LDAP mode.", type = "warning")
      return()
    }
    req(input$login_username, input$login_password)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)

    row <- dbGetQuery(con, "SELECT * FROM users WHERE lower(username) = lower(?) LIMIT 1", params = list(trimws(input$login_username)))
    if (nrow(row) == 0 || row$password[[1]] != ms_hash_password(input$login_password)) {
      showNotification("Login failed.", type = "error")
      return()
    }
    apply_user_row(row)
  })

  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
    user$user_id <- NULL
    user$username <- NULL
    user$role <- "user"
    user$is_admin <- FALSE
    projects_data(data.frame())
  })

  load_projects <- function() {
    if (!isTRUE(user$logged_in)) return()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)

    if (can_edit_all_projects()) {
      projects <- dbGetQuery(con, "
        SELECT p.id, p.project_code, p.project_name, pt.name AS project_type,
               p.responsible_user, p.submitter_email,
               COALESCE(p.last_status_update_at, p.created_at) AS last_status_update_at,
               COALESCE(t.full_name, t.username, '') AS technician,
               trim(
                 COALESCE(bh.name, '') || ' ' ||
                 COALESCE(bh.surname, '') ||
                 CASE
                   WHEN bh.cost_center IS NOT NULL AND trim(bh.cost_center) != ''
                   THEN ' - ' || bh.cost_center
                   ELSE ''
                 END
               ) AS budget_holder,
               p.num_samples, p.status, p.total_cost, p.created_at
        FROM projects p
        LEFT JOIN project_types pt ON p.project_type = pt.slug
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN users t ON p.technician_user_id = t.id
        ORDER BY p.created_at DESC, p.id DESC
      ")
    } else {
      projects <- dbGetQuery(con, "
        SELECT p.id, p.project_code, p.project_name, pt.name AS project_type,
               p.responsible_user, p.submitter_email,
               COALESCE(p.last_status_update_at, p.created_at) AS last_status_update_at,
               COALESCE(t.full_name, t.username, '') AS technician,
               trim(
                 COALESCE(bh.name, '') || ' ' ||
                 COALESCE(bh.surname, '') ||
                 CASE
                   WHEN bh.cost_center IS NOT NULL AND trim(bh.cost_center) != ''
                   THEN ' - ' || bh.cost_center
                   ELSE ''
                 END
               ) AS budget_holder,
               p.num_samples, p.status, p.total_cost, p.created_at
        FROM projects p
        LEFT JOIN project_types pt ON p.project_type = pt.slug
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN users t ON p.technician_user_id = t.id
        WHERE p.user_id = ? OR lower(p.responsible_user) = lower(?)
        ORDER BY p.created_at DESC, p.id DESC
      ", params = list(user$user_id, user$username))
    }
    projects_data(projects)
  }

  output$project_type_filter_ui <- renderUI({
    dat <- projects_data()
    if (is.null(dat) || nrow(dat) == 0 || !("project_type" %in% names(dat))) return(NULL)
    types <- sort(unique(trimws(as.character(dat$project_type %||% ""))))
    types <- types[nzchar(types)]
    if (length(types) <= 1) return(NULL)
    div(
      class = "project-table-filter",
      selectInput("project_type_filter", "Project Type", choices = c("All project types" = "", setNames(types, types)), selected = "")
    )
  })

  filtered_projects_data <- reactive({
    dat <- projects_data()
    if (is.null(dat) || nrow(dat) == 0) return(dat)
    selected_type <- trim_scalar(input$project_type_filter)
    if (nzchar(selected_type) && "project_type" %in% names(dat)) {
      dat <- dat[trimws(as.character(dat$project_type)) == selected_type, , drop = FALSE]
    }
    dat
  })

  output$projects_table <- renderDT({
    dat <- filtered_projects_data()
    if (is.null(dat) || nrow(dat) == 0) {
      return(datatable(data.frame(Message = "No projects yet."), rownames = FALSE, options = list(dom = "t")))
    }
    display <- dat[, c(
      "project_code", "project_name", "responsible_user", "last_status_update_at",
      "technician", "status", "project_type", "budget_holder",
      "num_samples", "total_cost", "created_at"
    ), drop = FALSE]
    display$status <- vapply(display$status, function(x) as.character(status_badge(x)), character(1))
    names(display) <- c(
      "Project ID", "Project Name", "Responsible user", "Last Update",
      "Technician", "Status", "Project Type", "Budget holder",
      "Samples", "Total Cost", "Created"
    )
    table <- datatable(
      display,
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )

    if (dt_available) {
      table <- formatStyle(
        table,
        "Project Type",
        target = "row",
        backgroundColor = styleEqual(
          c("Intact / Native Mass", "Proteomics", "Metabolomics"),
          c("rgba(221,204,119,0.16)", "rgba(136,204,238,0.16)", "rgba(204,102,119,0.14)")
        )
      )
    }
    table
  })

  selected_project_row <- reactive({
    dat <- filtered_projects_data()
    selected <- input$projects_table_rows_selected
    if (is.null(selected) || length(selected) == 0 || nrow(dat) < selected[1]) return(NULL)
    dat[selected[1], , drop = FALSE]
  })

  measurement_selector <- function(types) {
    template_download_ids <- c(
      intact_mass = "download_intact_template",
      proteomics = "download_proteomics_template",
      metabolomics = "download_metabolomics_template"
    )
    div(
      class = "measurement-select",
      h4("Step 1 - Measurement Type Selection"),
      p("Choose one project type before the rest of the submission form is shown."),
      radioButtons(
        "project_type",
        label = NULL,
        selected = character(0),
        choiceNames = lapply(seq_len(nrow(types)), function(i) {
          download_id <- unname(template_download_ids[types$slug[i]])
          div(
            class = "measurement-choice",
            style = paste0("border-top-color:", types$color[i], ";"),
            strong(types$name[i]),
            span(types$description[i]),
            downloadButton(download_id, paste(types$name[i], "Template"), class = "btn btn-default btn-sm template-card-download")
          )
        }),
        choiceValues = types$slug,
        width = "100%"
      )
    )
  }

  template_download <- function(project_type) {
    downloadHandler(
      filename = function() {
        basename(template_path_for_project_type(project_type()))
      },
      content = function(file) {
        path <- template_path_for_project_type(project_type())
        if (file.exists(path)) {
          file.copy(path, file, overwrite = TRUE)
        } else {
          utils::write.table(
            as.data.frame(setNames(rep(list(character()), length(sample_table_required_columns)), sample_table_required_columns)),
            file = file,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE
          )
        }
      },
      contentType = "text/tab-separated-values"
    )
  }

  output$download_intact_template <- template_download(function() "intact_mass")
  output$download_proteomics_template <- template_download(function() "proteomics")
  output$download_metabolomics_template <- template_download(function() "metabolomics")
  output$download_selected_template <- template_download(function() trim_scalar(input$project_type, "proteomics"))

  observeEvent(input$new_project_btn, {
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    types <- load_project_types(con)

    showModal(modalDialog(
      title = "New MS Project Submission",
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_project_btn", "Submit Project", class = "btn btn-primary")
      ),
      warning_banner(),
      measurement_selector(types),
      uiOutput("new_project_form")
    ))
  })

  output$new_project_form <- renderUI({
    req(input$project_type)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    holders <- load_budget_holders(con)
    budget_groups <- c("Choose PI / Group" = "", budget_holder_group_choices(holders))
    selected_budget_group <- match_budget_holder_group(holders, user$research_group)
    refs <- load_reference_values(con)
    name_parts <- split_full_name(user$full_name, user$username)

    tagList(
      div(
        class = "form-section",
        h4("2. Contact and Billing"),
        fluidRow(
          column(6, textInput("submitter_first_name", field_label("First name *", "Auto-filled from the login session."), value = name_parts$first)),
          column(6, textInput("submitter_last_name", field_label("Last name *", "Auto-filled from the login session."), value = name_parts$last))
        ),
        fluidRow(
          column(6, textInput("submitter_email", field_label("E-mail *", "Used for confirmation and status notifications."), value = user$email)),
          column(6, textInput("responsible_user", field_label("Responsible user *", "Usually the logged-in user; admins may submit on behalf of someone else."), value = user$username))
        ),
        fluidRow(
          column(6, selectInput("budget_pi", field_label("PI / Group *", "First and last name of the PI."), choices = budget_groups, selected = selected_budget_group)),
          column(6, uiOutput("billing_group_ui"))
        ),
        uiOutput("budget_holder_notice")
      ),
      div(
        id = "sample_overview_section",
        class = "form-section",
        h4("3. Sample Overview Table"),
        div(class = "info-note", "Download the tab-delimited .txt template for the selected project type and fill it locally. Uploads may be .txt, .csv, or .tsv files using tab, comma, or semicolon separators."),
        div(
          class = "sample-upload-actions",
          fileInput("sample_table_upload", "Upload sample overview table (.txt / .csv / .tsv)", accept = c(".txt", ".csv", ".tsv")),
          downloadButton("download_selected_template", paste("Download", project_type_template_label(input$project_type), "Template"))
        ),
        uiOutput("sample_table_validation_preview"),
        div(class = "info-note", MS_SAMPLE_TABLE_STATEMENT)
      ),
      div(
        class = "form-section",
        h4("4. Sample Identity"),
        fluidRow(
          column(6, textInput("project_name", field_label("Sample name *", "Short unique identifier chosen by the user, max 80 characters.", "AB-001_proteinA"))),
          column(3, textInput("submission_date", field_label("Date of submission *", "Auto-set to today."), value = as.character(Sys.Date()))),
          column(3, numericInput("num_samples", field_label("Number of samples *", "Drives the row count in the sample overview table."), value = 1, min = 1, max = 500, step = 1))
        )
      ),
      div(
        class = "form-section",
        h4("5. Sample"),
        if (input$project_type == "metabolomics") {
          tagList(
            selectizeInput(
              "metabolomics_sample_type",
              field_label("Sample Type *", "Select all sample material types included in this submission."),
              choices = c("Pellet", "Supernatant", "Tissue"),
              multiple = TRUE
            ),
            uiOutput("metabolomics_sample_type_details"),
            selectizeInput(
              "metabolomics_species",
              field_label("Species *", "Human, mouse, rat, yeast, E. coli, other; multiple entries allowed."),
              choices = refs,
              multiple = TRUE
            ),
            fluidRow(
              column(6, radioButtons("concentration_determination", field_label("Concentration determination *", "Indicate whether concentration was determined."), choices = load_choice_values(con, "concentration_determination"), inline = TRUE)),
              column(6, selectInput("concentration_method", "Method", choices = c("", load_choice_values(con, "concentration_method"))))
            )
          )
        } else {
          tagList(
            textAreaInput("sample_buffer", field_label("Buffer / Solvent *", "List buffer type, salts, detergents, reducing agents, glycerol %, pH, and organic co-solvents. PBS, HEPES, and Tris can interfere with ESI."), rows = 3),
            fluidRow(
              column(6, radioButtons("concentration_determination", field_label("Concentration determination *", "Indicate whether concentration was determined."), choices = load_choice_values(con, "concentration_determination"), inline = TRUE)),
              column(6, selectInput("concentration_method", "Method", choices = c("", load_choice_values(con, "concentration_method"))))
            ),
            fluidRow(
              column(4, textInput("sample_volume", field_label("Volume submitted *", "Numeric volume submitted.", "50"))),
              column(2, selectInput("sample_volume_unit", "Unit", choices = load_choice_values(con, "volume_unit"), selected = "uL")),
              column(6, textInput("sample_amount", field_label("Sample amount *", "Describe the total submitted material.", "1e6 cells, 50 uL plasma, or 5 mg tissue")))
            ),
            if (input$project_type == "proteomics") {
              selectizeInput(
                "proteomics_species",
                field_label("Species *", "Default search database is UniProt reference proteome for selected species. Multiple species are allowed."),
                choices = refs,
                multiple = TRUE
              )
            }
          )
        }
      ),
      div(
        class = "form-section",
        h4("6. Biological Question"),
        textAreaInput("sample_notes", field_label("What do you want to know? *", "Describe the biological question, hypothesis, and expected outcome."), rows = 4),
        textAreaInput("special_requirements", field_label("Special requirements", "Optional constraints, timing, safety notes, or facility discussion notes."), rows = 3)
      ),
      type_specific_new_project_ui(input$project_type, con, refs),
      warning_banner(compact = TRUE)
    )
  })

  output$billing_group_ui <- renderUI({
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    holders <- load_budget_holders(con)
    selected_group <- trim_scalar(input$budget_pi)
    if (!nzchar(selected_group)) {
      selected_group <- match_budget_holder_group(holders, user$research_group)
    }

    rows <- budget_holders_for_group(holders, selected_group)
    if (nrow(rows) == 0) {
      return(selectInput(
        "budget_id",
        field_label("Billing Group *", "Cost center for the selected PI / Group."),
        choices = c("Choose PI / Group first" = "")
      ))
    }

    labels <- trimws(as.character(rows$cost_center %||% ""))
    labels[!nzchar(labels)] <- "N.A."
    choices <- setNames(as.character(rows$id), labels)
    automatic_id <- match_budget_holder_id(holders, selected_group, user$cost_center)
    selected <- trim_scalar(input$budget_id)
    if (!(selected %in% unname(choices))) {
      selected <- automatic_id
    }
    if (nrow(rows) > 1 && !nzchar(selected)) {
      choices <- c("Choose cost center" = "", choices)
    }

    selectInput(
      "budget_id",
      field_label("Billing Group *", "Cost center for the selected PI / Group."),
      choices = choices,
      selected = selected
    )
  })

  output$budget_holder_notice <- renderUI({
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    holders <- load_budget_holders(con)
    budget_holder_notice_ui(holders, input$budget_pi, input$budget_id)
  })

  output$metabolomics_sample_type_details <- renderUI({
    selected <- trimws(as.character(input$metabolomics_sample_type %||% character()))
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0) return(NULL)

    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    volume_units <- load_choice_values(con, "volume_unit", c("uL", "mL", "other"))

    div(
      class = "sample-type-details",
      lapply(selected, function(sample_type) {
        if (identical(sample_type, "Pellet")) {
          fluidRow(
            class = "sample-type-detail-row",
            column(6, textInput("metabolomics_cell_number", field_label("Cell Number *", "Number of cells in the pellet."))),
            column(6, selectInput("metabolomics_cell_number_unit", "Unit", choices = c("cells", "million cells", "other")))
          )
        } else if (identical(sample_type, "Supernatant")) {
          fluidRow(
            class = "sample-type-detail-row",
            column(6, textInput("metabolomics_supernatant_volume", field_label("Volumes *", "Submitted volume for supernatant samples."))),
            column(6, selectInput("metabolomics_supernatant_volume_unit", "Unit", choices = volume_units, selected = "uL"))
          )
        } else if (identical(sample_type, "Tissue")) {
          fluidRow(
            class = "sample-type-detail-row",
            column(6, textInput("metabolomics_tissue_weight", field_label("Weight *", "Submitted tissue weight."))),
            column(6, selectInput("metabolomics_tissue_weight_unit", "Unit", choices = c("ug", "mg", "g", "other"), selected = "mg"))
          )
        } else {
          NULL
        }
      })
    )
  })

  output$sample_table_validation_preview <- renderUI({
    if (is.null(input$sample_table_upload) || nrow(input$sample_table_upload) == 0) return(NULL)
    result <- read_sample_table_upload(input$sample_table_upload, input$project_type, input$num_samples)
    if (length(result$errors) > 0) {
      return(div(
        class = "error-box",
        tags$strong("Sample table needs attention"),
        tags$ul(lapply(result$errors, tags$li))
      ))
    }
    div(
      class = "info-note",
      paste0("Sample table parsed successfully: ", nrow(result$table), " rows.")
    )
  })

  type_specific_new_project_ui <- function(project_type, con, refs) {
    if (project_type == "intact_mass") {
      return(div(
      class = "form-section type-intact",
      h4("7. Intact / Native Mass"),
        selectInput("intact_sample_type", field_label("Sample type *", "Protein monomer, native complex, peptide, oligonucleotide, small molecule, or other."), choices = load_choice_values(con, "intact_sample_type")),
        conditionalPanel("input.intact_sample_type == 'Other'", textInput("intact_sample_type_other", "Other sample type")),
        conditionalPanel("input.intact_sample_type == 'Protein complex / Native complex'", textInput("intact_stoichiometry", field_label("Stoichiometry", "Shown for protein complexes.", "alpha-beta-gamma heterotrimer, 1:1:2"))),
        textAreaInput("intact_sequence_name_structure", field_label("Sequence / Name / Structure *", "Protein/peptide sequence or UniProt ID; small molecule IUPAC, CAS, SMILES, or InChI; oligonucleotide 5' to 3' sequence."), rows = 4),
        textInput("intact_expected_mass_da", field_label("Expected mass (Da) *", "Monoisotopic or average mass from sequence or formula.")),
        textAreaInput("intact_tags_modifications", field_label("Tags / Modifications", "Purification tags, fusion partners, known PTMs, disulfide bonds; write NA if not applicable.", "His6-SUMO, TEV site"), rows = 2),
        fileInput("intact_fasta", field_label("FASTA file upload", "Required for recombinant, tagged, mutated, or non-standard proteins. Allowed .fasta, .fa, .faa, .fna files."), accept = c(".fasta", ".fa", ".faa", ".fna")),
        tags$details(tags$summary("Show FASTA example"), pre(">protein_A His6 tagged construct\nMKWVTFISLLLLFSSAYSRGVFRRDAHKSEVAHRFKDLGE"))
      ))
    }

    if (project_type == "proteomics") {
      return(div(
      class = "form-section type-proteomics",
      h4("7. Proteomics"),
        selectInput("proteomics_project_type", field_label("Project type *", "Protein ID, protein coverage/PTMs, AP-MS, total proteome, PTMomics, XL-MS, or other."), choices = load_choice_values(con, "proteomics_project_type")),
        conditionalPanel("input.proteomics_project_type == 'Other'", textInput("proteomics_project_type_other", "Other project type")),
        selectInput("proteomics_sample_type", field_label("Sample type *", "Gel band/lane shows gel image upload and confirmation."), choices = load_choice_values(con, "proteomics_sample_type")),
        conditionalPanel(
          "input.proteomics_sample_type == 'Gel band / Gel lane'",
          fileInput("proteomics_gel_images", "Gel image upload (.jpg / .png / .tif)", multiple = TRUE, accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff")),
          checkboxInput("gel_image_confirmation", "I have labelled all bands with numbers and all marker bands with sizes.", value = FALSE)
        ),
        radioButtons("proteomics_acquisition_mode", field_label("Acquisition mode *", "Choose DDA, DIA, no preference, or discuss with facility."), choices = load_choice_values(con, "proteomics_acquisition_mode"), inline = TRUE),
        selectInput("proteomics_quantification_strategy", field_label("Quantification strategy *", "Label-free, SILAC, no quantification, or other."), choices = load_choice_values(con, "proteomics_quantification_strategy")),
        conditionalPanel(
          "input.proteomics_quantification_strategy == 'SILAC'",
          selectizeInput("proteomics_silac_amino_acids", "SILAC amino acids", choices = load_choice_values(con, "silac_amino_acids"), multiple = TRUE)
        ),
        textInput("proteomics_expression_host", field_label("Expression host", "If recombinant, specify expression system. Write NA if not applicable.", "E. coli BL21, Sf21, HEK293T")),
        fileInput("proteomics_fasta", field_label("FASTA file upload", "Always visible; critical for non-standard constructs. Standard proteins can leave it blank."), accept = c(".fasta", ".fa", ".faa", ".fna"), multiple = TRUE),
        div(class = "info-note", "FASTA upload is critical for correct database searching of non-standard constructs. Users with standard proteins leave it blank."),
        selectInput("proteomics_digestion_enzyme", field_label("Digestion enzyme", "Default is Trypsin, which cleaves C-terminally of K and R."), choices = load_choice_values(con, "digestion_enzyme"), selected = "Trypsin"),
        selectizeInput("proteomics_ptms", field_label("PTMs / Variable modifications", "Select expected variable modifications."), choices = load_choice_values(con, "ptm_variable_modifications"), multiple = TRUE),
        conditionalPanel(
          "input.proteomics_project_type == 'Crosslinking (XL-MS)'",
          selectInput("proteomics_crosslinker", field_label("Crosslinker", "Shown for XL-MS.", "PhoX, DSS/BS3, DSSO, EDC"), choices = c("", load_choice_values(con, "crosslinker")))
        )
      ))
    }

    div(
      class = "form-section type-metabolomics",
      h4("7. Metabolomics"),
      radioButtons(
        "metabolomics_analysis_family",
        field_label("Analysis Type *", "Choose the main metabolomics family."),
        choices = c("Metabolomics", "Lipidomics"),
        selected = "Metabolomics",
        inline = TRUE
      ),
      selectInput(
        "metabolomics_analysis_type",
        field_label("Approach *", "Targeted, untargeted, both, or other."),
        choices = c("Targeted", "Untargeted", "Both", "Other"),
        selected = "Targeted"
      ),
      conditionalPanel("input.metabolomics_analysis_type == 'Other'", textInput("metabolomics_analysis_type_other", "Other analysis information")),
      conditionalPanel(
        "input.metabolomics_analysis_type == 'Targeted' || input.metabolomics_analysis_type == 'Both'",
        textAreaInput("metabolomics_targeted_analyte_text", field_label("Targeted analyte list", "List compound names, CAS numbers, or HMDB IDs."), rows = 3),
        fileInput("metabolomics_library", "Targeted analyte file (.xlsx / .csv / .txt / .tsv)", accept = c(".xlsx", ".csv", ".txt", ".tsv"))
      ),
      textAreaInput("metabolomics_notes", "Metabolomics notes", rows = 3)
    )
  }

  validate_project_form <- function() {
    errors <- character()
    selected_type <- trim_scalar(input$project_type)
    required <- c(
      project_name = "Sample name",
      submitter_first_name = "First name",
      submitter_last_name = "Last name",
      submitter_email = "E-mail",
      responsible_user = "Responsible user",
      budget_id = "Billing Group",
      budget_pi = "PI / Group",
      concentration_determination = "Concentration determination",
      sample_notes = "Biological question"
    )
    if (!identical(selected_type, "metabolomics")) {
      required <- c(required,
        sample_buffer = "Buffer / Solvent",
        sample_volume = "Volume submitted",
        sample_amount = "Sample amount"
      )
    }
    for (id in names(required)) {
      if (!non_empty(input[[id]])) errors <- c(errors, paste(required[[id]], "is required."))
    }

    n <- suppressWarnings(as.integer(input$num_samples %||% NA_integer_))
    if (is.na(n) || n < 1) errors <- c(errors, "Number of samples must be at least 1.")

    sample_table_result <- read_sample_table_upload(input$sample_table_upload, selected_type, n)
    errors <- c(errors, sample_table_result$errors)

    if (!(selected_type %in% c("intact_mass", "proteomics", "metabolomics"))) {
      errors <- c(errors, "Choose a measurement type.")
    }

    if (nchar(trim_scalar(input$project_name)) > 80) {
      errors <- c(errors, "Sample name must be 80 characters or shorter.")
    }

    if (selected_type == "intact_mass") {
      intact_required <- c(
        intact_sample_type = "Sample type",
        intact_sequence_name_structure = "Sequence / Name / Structure",
        intact_expected_mass_da = "Expected mass"
      )
      for (id in names(intact_required)) {
        if (!non_empty(input[[id]])) errors <- c(errors, paste(intact_required[[id]], "is required."))
      }
      errors <- c(errors, validate_uploads(input$intact_fasta, c("fasta", "fa", "faa", "fna"), as.numeric(Sys.getenv("MS_MAX_FASTA_MB", "25")), fasta = TRUE))
    }

    if (selected_type == "proteomics") {
      proteomics_required <- c(
        proteomics_project_type = "Proteomics project type",
        proteomics_sample_type = "Proteomics sample type",
        proteomics_acquisition_mode = "Acquisition mode",
        proteomics_quantification_strategy = "Quantification strategy"
      )
      for (id in names(proteomics_required)) {
        if (!non_empty(input[[id]])) errors <- c(errors, paste(proteomics_required[[id]], "is required."))
      }
      if (is.null(input$proteomics_species) || length(input$proteomics_species) == 0) {
        errors <- c(errors, "Species is required.")
      }
      if (identical(input$proteomics_sample_type, "Gel band / Gel lane") &&
          !is.null(input$proteomics_gel_images) && nrow(input$proteomics_gel_images) > 0 &&
          !isTRUE(input$gel_image_confirmation)) {
        errors <- c(errors, "Gel image confirmation is required when gel images are uploaded.")
      }
      errors <- c(errors, validate_uploads(input$proteomics_fasta, c("fasta", "fa", "faa", "fna"), as.numeric(Sys.getenv("MS_MAX_FASTA_MB", "25")), fasta = TRUE))
      errors <- c(errors, validate_uploads(input$proteomics_gel_images, c("jpg", "jpeg", "png", "tif", "tiff"), as.numeric(Sys.getenv("MS_MAX_IMAGE_MB", "30"))))
    }

    if (selected_type == "metabolomics") {
      metabolomics_required <- c(
        metabolomics_analysis_family = "Analysis type",
        metabolomics_analysis_type = "Approach"
      )
      for (id in names(metabolomics_required)) {
        if (!non_empty(input[[id]])) errors <- c(errors, paste(metabolomics_required[[id]], "is required."))
      }
      selected_sample_types <- trimws(as.character(input$metabolomics_sample_type %||% character()))
      selected_sample_types <- selected_sample_types[nzchar(selected_sample_types)]
      if (length(selected_sample_types) == 0) {
        errors <- c(errors, "Sample Type is required.")
      }
      if ("Pellet" %in% selected_sample_types && !non_empty(input$metabolomics_cell_number)) {
        errors <- c(errors, "Cell Number is required for pellet samples.")
      }
      if ("Supernatant" %in% selected_sample_types && !non_empty(input$metabolomics_supernatant_volume)) {
        errors <- c(errors, "Volumes is required for supernatant samples.")
      }
      if ("Tissue" %in% selected_sample_types && !non_empty(input$metabolomics_tissue_weight)) {
        errors <- c(errors, "Weight is required for tissue samples.")
      }
      if (is.null(input$metabolomics_species) || length(input$metabolomics_species) == 0) {
        errors <- c(errors, "Species is required.")
      }
      if (identical(input$metabolomics_analysis_type, "Other") && !non_empty(input$metabolomics_analysis_type_other)) {
        errors <- c(errors, "Other analysis information is required when Approach is Other.")
      }
      if (input$metabolomics_analysis_type %in% c("Targeted", "Both")) {
        errors <- c(errors, validate_uploads(input$metabolomics_library, c("xlsx", "csv", "txt", "tsv"), as.numeric(Sys.getenv("MS_MAX_TEXT_MB", "20")), text_only = TRUE))
      }
    }

    errors[nzchar(errors)]
  }

  observeEvent(input$create_project_btn, {
    req(input$project_type)
    errors <- validate_project_form()
    if (length(errors) > 0) {
      showNotification(paste(errors, collapse = "\n"), type = "error", duration = 12)
      return()
    }

    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "PRAGMA foreign_keys = ON")

    submitter_name <- paste(trim_scalar(input$submitter_first_name), trim_scalar(input$submitter_last_name))
    sample_table_result <- read_sample_table_upload(input$sample_table_upload, input$project_type, input$num_samples)
    if (length(sample_table_result$errors) > 0) {
      showNotification(paste(sample_table_result$errors, collapse = "\n"), type = "error", duration = 12)
      return()
    }
    samples <- sample_rows_from_table(sample_table_result$table)
    selected_budget_id <- as.integer(input$budget_id)
    selected_budget_group <- budget_holder_group_for_id(con, selected_budget_id)
    if (!nzchar(selected_budget_group) || !identical(selected_budget_group, trim_scalar(input$budget_pi))) {
      showNotification("The selected PI / Group and cost center do not match. Please select them again.", type = "error", duration = 12)
      return()
    }

    project_values <- list(
      project_name = trim_scalar(input$project_name),
      project_type = input$project_type,
      user_id = user$user_id,
      responsible_user = trim_scalar(input$responsible_user),
      submitter_name = trimws(submitter_name),
      submitter_email = trim_scalar(input$submitter_email),
      submitter_phone = user$phone %||% "",
      submitter_group = selected_budget_group,
      budget_id = selected_budget_id,
      submission_date = trim_scalar(input$submission_date, as.character(Sys.Date())),
      num_samples = as.integer(input$num_samples),
      sample_buffer = trim_scalar(input$sample_buffer),
      concentration_determination = trim_scalar(input$concentration_determination),
      concentration_method = trim_scalar(input$concentration_method),
      sample_volume = trim_scalar(input$sample_volume),
      sample_volume_unit = trim_scalar(input$sample_volume_unit),
      sample_amount = trim_scalar(input$sample_amount),
      sample_notes = trim_scalar(input$sample_notes),
      special_requirements = trim_scalar(input$special_requirements),
      status = "Submitted",
      invoice_institute_address = MS_INSTITUTE_ADDRESS
    )

    if (input$project_type == "intact_mass") {
      project_values <- c(project_values, list(
        intact_sample_type = trim_scalar(input$intact_sample_type),
        intact_sample_type_other = trim_scalar(input$intact_sample_type_other),
        intact_stoichiometry = trim_scalar(input$intact_stoichiometry),
        intact_sequence_name_structure = trim_scalar(input$intact_sequence_name_structure),
        intact_expected_mass_da = trim_scalar(input$intact_expected_mass_da),
        intact_tags_modifications = trim_scalar(input$intact_tags_modifications)
      ))
    }

    if (input$project_type == "proteomics") {
      project_values <- c(project_values, list(
        proteomics_project_type = trim_scalar(input$proteomics_project_type),
        proteomics_project_type_other = trim_scalar(input$proteomics_project_type_other),
        proteomics_sample_type = trim_scalar(input$proteomics_sample_type),
        proteomics_sample_type_other = trim_scalar(input$proteomics_sample_type_other),
        gel_image_confirmation = as.integer(isTRUE(input$gel_image_confirmation)),
        proteomics_acquisition_mode = trim_scalar(input$proteomics_acquisition_mode),
        proteomics_quantification_strategy = trim_scalar(input$proteomics_quantification_strategy),
        proteomics_silac_amino_acids = join_values(input$proteomics_silac_amino_acids),
        proteomics_species = join_values(input$proteomics_species),
        proteomics_expression_host = trim_scalar(input$proteomics_expression_host),
        proteomics_digestion_enzyme = trim_scalar(input$proteomics_digestion_enzyme),
        proteomics_ptms = join_values(input$proteomics_ptms),
        proteomics_crosslinker = trim_scalar(input$proteomics_crosslinker)
      ))
    }

    if (input$project_type == "metabolomics") {
      project_values <- c(project_values, list(
        metabolomics_analysis_family = trim_scalar(input$metabolomics_analysis_family),
        metabolomics_analysis_type = trim_scalar(input$metabolomics_analysis_type),
        metabolomics_analysis_type_other = trim_scalar(input$metabolomics_analysis_type_other),
        metabolomics_sample_type = join_values(input$metabolomics_sample_type),
        metabolomics_cell_number = trim_scalar(input$metabolomics_cell_number),
        metabolomics_cell_number_unit = trim_scalar(input$metabolomics_cell_number_unit),
        metabolomics_supernatant_volume = trim_scalar(input$metabolomics_supernatant_volume),
        metabolomics_supernatant_volume_unit = trim_scalar(input$metabolomics_supernatant_volume_unit),
        metabolomics_tissue_weight = trim_scalar(input$metabolomics_tissue_weight),
        metabolomics_tissue_weight_unit = trim_scalar(input$metabolomics_tissue_weight_unit),
        metabolomics_species = join_values(input$metabolomics_species),
        metabolomics_targeted_analyte_text = trim_scalar(input$metabolomics_targeted_analyte_text),
        metabolomics_notes = trim_scalar(input$metabolomics_notes)
      ))
    }

    tryCatch({
      dbBegin(con)
      project_code <- next_orbi_project_code(con)
      project_values$project_code <- project_code
      project_values$last_status_update_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      project_id <- insert_record(con, "projects", project_values)
      storage <- prepare_project_folder(project_code, project_values$project_name)

      update_record(con, "projects", list(
        upload_root = storage$root,
        project_folder = storage$folder,
        upload_storage_status = storage$status
      ), "id = ?", list(project_id))

      for (i in seq_len(nrow(samples))) {
        insert_record(con, "project_samples", list(
          project_id = project_id,
          row_index = samples$row_index[i],
          tube_id = samples$tube_id[i],
          condition = samples$condition[i],
          replicate = samples$replicate[i],
          is_control = samples$is_control[i],
          description = samples$description[i]
        ))
      }

      stored_paths <- list()
      stored_paths$sample_table <- store_sample_table_upload(con, project_id, input$sample_table_upload, sample_table_result$table, input$project_type, storage$folder, user$username)
      if (input$project_type == "intact_mass") {
        stored_paths$intact_fasta_path <- store_upload_set(con, project_id, input$intact_fasta, "intact_fasta", storage$folder, user$username, compress = TRUE)
      }
      if (input$project_type == "proteomics") {
        stored_paths$proteomics_fasta_path <- store_upload_set(con, project_id, input$proteomics_fasta, "proteomics_fasta", storage$folder, user$username, compress = TRUE)
        stored_paths$gel_images <- store_upload_set(con, project_id, input$proteomics_gel_images, "gel_image", storage$folder, user$username, compress = FALSE)
      }
      if (input$project_type == "metabolomics") {
        stored_paths$metabolomics_library_path <- store_upload_set(con, project_id, input$metabolomics_library, "metabolomics_target_list", storage$folder, user$username, compress = TRUE)
      }

      update_values <- list()
      for (nm in names(stored_paths)) {
        if (length(stored_paths[[nm]]) > 0 && nm %in% c("intact_fasta_path", "proteomics_fasta_path", "metabolomics_library_path")) {
          update_values[[nm]] <- paste(stored_paths[[nm]], collapse = "; ")
        }
      }
      if (length(update_values) > 0) update_record(con, "projects", update_values, "id = ?", list(project_id))

      dbCommit(con)

      project <- load_project_detail(con, project_id)
      saved_samples <- load_project_samples(con, project_id)
      handoff_pdf <- tempfile(fileext = ".pdf")
      write_sample_handoff_pdf(project, saved_samples, handoff_pdf)

      send_ms_submission_email(con, project, saved_samples, unlist(stored_paths$sample_table, use.names = FALSE), handoff_pdf)

      if (!identical(storage$status, "pool")) {
        send_mail_safe(
          con,
          project_id,
          MS_POOL_FALLBACK_EMAIL,
          paste(project$project_code, "upload pool fallback used", sep = " - "),
          paste(
            "The configured upload pool was not available.",
            paste("Configured root:", MS_UPLOAD_ROOT),
            paste("Fallback folder:", storage$folder),
            sep = "\n"
          )
        )
      }

      removeModal()
      load_projects()
      showNotification("Project submitted successfully.", type = "message")
    }, error = function(e) {
      if (dbIsValid(con)) try(dbRollback(con), silent = TRUE)
      showNotification(paste("Project submission failed:", conditionMessage(e)), type = "error", duration = 12)
    })
  })

  observeEvent(input$edit_project_btn, {
    row <- selected_project_row()
    if (is.null(row)) {
      showNotification("Please select a project first.", type = "warning")
      return()
    }
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    project <- load_project_detail(con, row$id[[1]])
    if (nrow(project) == 0) return()
    staff_choices <- load_staff_choices(con)
    editing_project_id(project$id[[1]])

    showModal(modalDialog(
      title = paste("Edit", project$project_code, "-", project$project_name),
      size = "l",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Close"),
        if (can_edit_all_projects()) actionButton("send_cost_email_btn", "Send Cost Estimation", class = "btn btn-info"),
        actionButton("update_project_btn", "Save Changes", class = "btn btn-primary")
      ),
      tabsetPanel(
        tabPanel(
          "Project",
          warning_banner(compact = TRUE),
          fluidRow(
            column(6, textInput("edit_project_name", "Sample/project name", value = project$project_name)),
            column(3, numericInput("edit_num_samples", "Number of samples", value = project$num_samples, min = 1)),
            column(3, uiOutput("edit_status_badge"))
          ),
          textInput("edit_responsible_user", "Responsible user", value = project$responsible_user),
          textInput("edit_submitter_email", "Submitter email", value = project$submitter_email),
          if (can_edit_all_projects()) selectInput(
            "edit_technician_user_id",
            "Technician",
            choices = staff_choices,
            selected = scalar_text(project$technician_user_id)
          ),
          textAreaInput("edit_sample_buffer", "Buffer / Solvent", value = project$sample_buffer, rows = 3),
          fluidRow(
            column(4, textInput("edit_sample_concentration", "Concentration", value = project$sample_concentration)),
            column(2, textInput("edit_sample_concentration_unit", "Unit", value = project$sample_concentration_unit)),
            column(3, textInput("edit_sample_volume", "Volume", value = project$sample_volume)),
            column(3, textInput("edit_sample_amount", "Sample amount", value = project$sample_amount))
          ),
          textAreaInput("edit_sample_notes", "Biological question", value = project$sample_notes, rows = 4),
          textAreaInput("edit_special_requirements", "Special requirements", value = project$special_requirements, rows = 3),
          if (can_edit_all_projects()) tagList(
            selectInput("edit_status", "Project status", choices = ms_status_options, selected = project$status),
            fluidRow(
              column(6, textInput("edit_additional_cost", "Additional cost", value = scalar_text(project$additional_cost))),
              column(6, textInput("edit_total_cost", "Total cost", value = scalar_text(project$total_cost)))
            )
          )
        ),
        tabPanel(
          "Samples",
          div(class = "info-note", "Double-click cells to edit sample handoff rows."),
          DTOutput("edit_samples_table")
        ),
        tabPanel(
          "Files",
          DTOutput("edit_files_table"),
          div(
            class = "action-bar",
            downloadButton("download_project_file_btn", "Download Selected File"),
            if (can_edit_all_projects()) actionButton("delete_project_file_btn", "Delete Selected File", class = "btn btn-danger")
          )
        ),
        tabPanel(
          "Report / Invoice",
          textAreaInput("edit_report_notes", "Report notes", value = project$report_notes, rows = 5),
          textAreaInput("edit_invoice_recipient_address", "Invoice recipient address", value = project$invoice_recipient_address, rows = 5),
          textAreaInput("edit_invoice_institute_address", "Institute address", value = project$invoice_institute_address %||% MS_INSTITUTE_ADDRESS, rows = 5)
        )
      )
    ))
  })

  output$edit_status_badge <- renderUI({
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    project <- load_project_detail(con, project_id)
    if (nrow(project) == 0) return(NULL)
    div(class = "status-preview", "Current status", br(), status_badge(project$status))
  })

  output$edit_samples_table <- renderDT({
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    samples <- load_project_samples(con, project_id)
    display <- samples[, c("id", "tube_id", "condition", "replicate", "is_control", "description"), drop = FALSE]
    names(display) <- c("id", "Tube / ID", "Condition", "Replicate", "Control", "Description / Comments")
    datatable(display, rownames = FALSE, selection = "none", editable = TRUE, options = list(pageLength = 25, scrollX = TRUE))
  })

  observeEvent(input$edit_samples_table_cell_edit, {
    info <- input$edit_samples_table_cell_edit
    project_id <- editing_project_id()
    req(project_id, info)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    samples <- load_project_samples(con, project_id)
    if (nrow(samples) < info$row) return()
    sample_id <- samples$id[info$row]
    columns <- c("id", "tube_id", "condition", "replicate", "is_control", "description")
    column_name <- columns[info$col + 1]
    if (column_name == "id") return()
    value <- info$value
    if (column_name == "is_control") value <- as.integer(tolower(value) %in% c("1", "true", "yes", "y"))
    dbExecute(con, paste0("UPDATE project_samples SET ", column_name, " = ? WHERE id = ?"), params = list(value, sample_id))
  })

  output$edit_files_table <- renderDT({
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    files <- load_project_files(con, project_id)
    if (nrow(files) == 0) return(datatable(data.frame(Message = "No uploaded files."), rownames = FALSE, options = list(dom = "t")))
    datatable(files[, c("id", "file_type", "original_name", "size_bytes", "uploaded_by", "created_at"), drop = FALSE], rownames = FALSE, selection = "single", options = list(pageLength = 10))
  })

  output$download_project_file_btn <- downloadHandler(
    filename = function() {
      project_id <- editing_project_id()
      if (is.null(project_id)) return("project-file")
      con <- ms_db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      files <- load_project_files(con, project_id)
      selected <- input$edit_files_table_rows_selected
      if (is.null(selected) || length(selected) == 0 || nrow(files) < selected[1]) return("project-file")
      files$original_name[selected[1]]
    },
    content = function(file) {
      project_id <- editing_project_id()
      req(project_id)
      con <- ms_db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      files <- load_project_files(con, project_id)
      selected <- input$edit_files_table_rows_selected
      req(selected, nrow(files) >= selected[1])
      file.copy(files$path[selected[1]], file, overwrite = TRUE)
    }
  )

  observeEvent(input$delete_project_file_btn, {
    if (!can_edit_all_projects()) return()
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    files <- load_project_files(con, project_id)
    selected <- input$edit_files_table_rows_selected
    if (is.null(selected) || nrow(files) < selected[1]) {
      showNotification("Select a file first.", type = "warning")
      return()
    }
    file_row <- files[selected[1], ]
    if (file.exists(file_row$path)) unlink(file_row$path)
    dbExecute(con, "DELETE FROM project_files WHERE id = ?", params = list(file_row$id))
    showNotification("File deleted.", type = "message")
  })

  observeEvent(input$update_project_btn, {
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    project_before <- load_project_detail(con, project_id)
    if (nrow(project_before) == 0) return()
    values <- list(
      project_name = trim_scalar(input$edit_project_name),
      responsible_user = trim_scalar(input$edit_responsible_user),
      submitter_email = trim_scalar(input$edit_submitter_email),
      num_samples = as.integer(input$edit_num_samples),
      sample_buffer = trim_scalar(input$edit_sample_buffer),
      sample_concentration = trim_scalar(input$edit_sample_concentration),
      sample_concentration_unit = trim_scalar(input$edit_sample_concentration_unit),
      sample_volume = trim_scalar(input$edit_sample_volume),
      sample_amount = trim_scalar(input$edit_sample_amount),
      sample_notes = trim_scalar(input$edit_sample_notes),
      special_requirements = trim_scalar(input$edit_special_requirements),
      report_notes = trim_scalar(input$edit_report_notes),
      invoice_recipient_address = trim_scalar(input$edit_invoice_recipient_address),
      invoice_institute_address = trim_scalar(input$edit_invoice_institute_address, MS_INSTITUTE_ADDRESS)
    )
    if (can_edit_all_projects()) {
      new_status <- trim_scalar(input$edit_status, "Submitted")
      values$status <- new_status
      should_send_data_release <- !status_is_data_released(project_before$status) && status_is_data_released(new_status)
      if (!identical(new_status, scalar_text(project_before$status))) {
        values$last_status_update_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      }
      technician_id <- suppressWarnings(as.integer(input$edit_technician_user_id))
      values$technician_user_id <- if (is.na(technician_id)) NA_integer_ else technician_id
      values$additional_cost <- suppressWarnings(as.numeric(input$edit_additional_cost))
      values$total_cost <- suppressWarnings(as.numeric(input$edit_total_cost))
    } else {
      should_send_data_release <- FALSE
    }
    update_record(con, "projects", values, "id = ?", list(project_id))
    load_projects()
    showNotification("Project updated.", type = "message")

    if (isTRUE(should_send_data_release)) {
      project_after <- load_project_detail(con, project_id)
      res <- send_ms_data_released_email(con, project_after)
      if (isTRUE(res$success)) {
        showNotification("Data release email sent to the project user.", type = "message")
      } else {
        showNotification(paste("Project updated, but data release email failed:", res$error %||% "unknown error"), type = "warning", duration = 10)
      }
    }
  })

  observeEvent(input$send_cost_email_btn, {
    if (!can_edit_all_projects()) return()
    project_id <- editing_project_id()
    req(project_id)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    project <- load_project_detail(con, project_id)
    if (nrow(project) == 0) return()

    parsed_additional <- parse_optional_nonnegative_number(input$edit_additional_cost, "Additional cost")
    if (!parsed_additional$valid) {
      showNotification(parsed_additional$error, type = "error", duration = 10)
      return()
    }
    parsed_total <- parse_optional_nonnegative_number(input$edit_total_cost, "Total cost")
    if (!parsed_total$valid) {
      showNotification(parsed_total$error, type = "error", duration = 10)
      return()
    }

    update_record(con, "projects", list(
      additional_cost = if (parsed_additional$empty) NA_real_ else parsed_additional$value,
      total_cost = if (parsed_total$empty) NA_real_ else parsed_total$value
    ), "id = ?", list(project_id))

    project <- load_project_detail(con, project_id)
    res <- send_ms_cost_estimation_email(con, project)
    if (isTRUE(res$success)) {
      load_projects()
      showNotification("Cost estimation email sent to the project user and PI.", type = "message")
    } else {
      showNotification(res$error %||% "Cost estimation email failed.", type = "error", duration = 10)
    }
  })

  observeEvent(input$delete_project_btn, {
    if (!can_delete_projects()) return()
    row <- selected_project_row()
    if (is.null(row)) {
      showNotification("Please select a project first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete Project",
      paste("Delete", row$project_code, row$project_name, "?"),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete_project_btn", "Delete", class = "btn btn-danger"))
    ))
  })

  observeEvent(input$confirm_delete_project_btn, {
    if (!can_delete_projects()) return()
    row <- selected_project_row()
    req(row)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    files <- load_project_files(con, row$id[[1]])
    if (nrow(files) > 0) {
      existing <- files$path[file.exists(files$path)]
      if (length(existing) > 0) unlink(existing)
    }
    dbExecute(con, "DELETE FROM projects WHERE id = ?", params = list(row$id[[1]]))
    removeModal()
    load_projects()
    showNotification("Project deleted.", type = "message")
  })

  output$download_report_btn <- downloadHandler(
    filename = function() {
      row <- selected_project_row()
      paste0(if (is.null(row)) "ms_project" else row$project_code[[1]], "_report.pdf")
    },
    content = function(file) {
      row <- selected_project_row()
      req(row)
      con <- ms_db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      project <- load_project_detail(con, row$id[[1]])
      samples <- load_project_samples(con, row$id[[1]])
      files <- load_project_files(con, row$id[[1]])
      write_project_report_pdf(project, samples, files, file, "report")
    }
  )

  output$download_invoice_btn <- downloadHandler(
    filename = function() {
      row <- selected_project_row()
      paste0(if (is.null(row)) "ms_project" else row$project_code[[1]], "_invoice.pdf")
    },
    content = function(file) {
      row <- selected_project_row()
      req(row)
      con <- ms_db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      project <- load_project_detail(con, row$id[[1]])
      samples <- load_project_samples(con, row$id[[1]])
      files <- load_project_files(con, row$id[[1]])
      write_project_report_pdf(project, samples, files, file, "invoice")
    }
  )

  admin_panel_card <- function(title, button_id, button_label) {
    div(
      class = "admin-panel-card",
      h4(title),
      actionButton(button_id, button_label, class = "btn btn-primary")
    )
  }

  admin_users_management_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Add New User"),
      div(class = "admin-form-row",
        textInput("admin_new_username", "Username", placeholder = "Enter username"),
        textInput("admin_new_email", "Email", placeholder = "Enter email"),
        passwordInput("admin_new_password", "Password", placeholder = "Enter password"),
        selectInput("admin_new_role", "Role", choices = c("User" = "user", "Technician" = "technician", "Admin" = "admin"), selected = "user"),
        actionButton("admin_add_user_btn", "Add User", class = "btn btn-primary")
      ),
      tags$hr(),
      h3("Existing Users"),
      DTOutput("admin_users_table"),
      div(
        class = "admin-management-actions",
        actionButton("admin_edit_user_btn", "Edit Selected", class = "btn btn-warning"),
        actionButton("admin_delete_user_btn", "Delete Selected User", class = "btn btn-danger")
      )
    )
  }

  admin_budget_management_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Add New Budget Holder"),
      div(class = "admin-form-row",
        textInput("admin_bh_name", "Name", placeholder = "Enter name"),
        textInput("admin_bh_surname", "Surname", placeholder = "Enter surname"),
        textInput("admin_bh_cost_center", "Cost center", placeholder = "Enter cost center"),
        textInput("admin_bh_email", "Email", placeholder = "Enter email"),
        actionButton("admin_add_bh_btn", "Add Budget Holder", class = "btn btn-primary")
      ),
      tags$hr(),
      h3("Existing Budget Holders"),
      DTOutput("admin_budget_table"),
      div(
        class = "admin-management-actions",
        actionButton("admin_edit_bh_btn", "Edit Selected", class = "btn btn-warning"),
        actionButton("admin_delete_bh_btn", "Delete Selected Budget Holder", class = "btn btn-danger")
      )
    )
  }

  admin_organisms_management_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Add New Reference Organism"),
      div(class = "admin-form-row",
        textInput("admin_new_organism", "Reference organism", placeholder = "Enter organism"),
        actionButton("admin_add_organism_btn", "Add Organism", class = "btn btn-primary")
      ),
      tags$hr(),
      h3("Existing Reference Organisms"),
      DTOutput("admin_organisms_table"),
      div(
        class = "admin-management-actions",
        actionButton("admin_edit_organism_btn", "Edit Selected", class = "btn btn-warning"),
        actionButton("admin_delete_organism_btn", "Delete Selected Organism", class = "btn btn-danger")
      )
    )
  }

  admin_options_management_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Add New Dropdown Option"),
      div(class = "admin-form-row",
        selectInput("admin_option_group", "Option group", choices = sort(unique(c(names(ms_controlled_options), "status")))),
        textInput("admin_option_value", "Value", placeholder = "Enter option value"),
        actionButton("admin_add_option_btn", "Add Option", class = "btn btn-primary")
      ),
      tags$hr(),
      h3("Existing Dropdown Options"),
      DTOutput("admin_options_table"),
      div(
        class = "admin-management-actions",
        actionButton("admin_edit_option_btn", "Edit Selected", class = "btn btn-warning"),
        actionButton("admin_delete_option_btn", "Delete Selected Option", class = "btn btn-danger")
      )
    )
  }

  admin_email_log_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Email Log"),
      DTOutput("admin_email_log_table")
    )
  }

  admin_landing_text_management_ui <- function() {
    div(
      class = "admin-management-modal",
      h3("Add New Project Page Text Block"),
      div(class = "admin-form-row admin-form-row-wide",
        textInput("admin_landing_key", "Block key", placeholder = "unique_block_key"),
        textInput("admin_landing_title", "Title", placeholder = "Section title"),
        numericInput("admin_landing_order", "Display order", value = 10, min = 1, step = 1),
        checkboxInput("admin_landing_active", "Active", value = TRUE),
        textAreaInput("admin_landing_body", "Body", rows = 4, placeholder = "Text shown on the Project page"),
        actionButton("admin_add_landing_btn", "Add Text Block", class = "btn btn-primary")
      ),
      tags$hr(),
      h3("Existing Project Page Text Blocks"),
      DTOutput("admin_landing_table"),
      div(
        class = "admin-management-actions",
        actionButton("admin_edit_landing_btn", "Edit Selected", class = "btn btn-warning"),
        actionButton("admin_delete_landing_btn", "Delete Selected Text Block", class = "btn btn-danger")
      )
    )
  }

  output$admin_interface <- renderUI({
    req(can_view_admin_panels())
    div(
      class = "admin-dashboard",
      h2("Administrator Panel"),
      div(
        class = "admin-panel-grid",
        admin_panel_card("User Management", "open_admin_users_btn", "Manage Users"),
        admin_panel_card("Budget Holders", "open_admin_budget_btn", "Manage Budget Holders"),
        admin_panel_card("Reference Organisms", "open_admin_organisms_btn", "Manage Organisms"),
        admin_panel_card("Dropdown Options", "open_admin_options_btn", "Manage Dropdown Options"),
        admin_panel_card("Landing Text", "open_admin_landing_btn", "Manage Landing Text"),
        admin_panel_card("Email Log", "open_admin_email_log_btn", "View Email Log")
      ),
      div(
        class = "admin-database-panel",
        h4("Database Management"),
        p("Download a complete backup of the database for safekeeping."),
        downloadButton("admin_download_db_btn", "Download Database Backup", class = "btn btn-warning"),
        actionButton("admin_validate_db_btn", "Validate Database", class = "btn btn-info")
      )
    )
  })

  observeEvent(input$open_admin_users_btn, {
    showModal(modalDialog(
      title = "User Management",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_users_management_ui()
    ))
  })

  observeEvent(input$open_admin_budget_btn, {
    showModal(modalDialog(
      title = "Budget Holders Management",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_budget_management_ui()
    ))
  })

  observeEvent(input$open_admin_organisms_btn, {
    showModal(modalDialog(
      title = "Reference Organisms Management",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_organisms_management_ui()
    ))
  })

  observeEvent(input$open_admin_options_btn, {
    showModal(modalDialog(
      title = "Dropdown Options Management",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_options_management_ui()
    ))
  })

  observeEvent(input$open_admin_email_log_btn, {
    showModal(modalDialog(
      title = "Email Log",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_email_log_ui()
    ))
  })

  observeEvent(input$open_admin_landing_btn, {
    showModal(modalDialog(
      title = "Landing Text Management",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      admin_landing_text_management_ui()
    ))
  })

  output$admin_download_db_btn <- downloadHandler(
    filename = function() {
      paste0("ms_projects_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite")
    },
    content = function(file) {
      ok <- file.copy(ms_db_path(), file, overwrite = TRUE)
      if (!isTRUE(ok)) stop("Could not copy database file.")
      con <- ms_db_connect()
      on.exit(dbDisconnect(con), add = TRUE)
      dbExecute(con, "
        INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
        VALUES (?, ?, ?)
      ", params = list(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        file.info(file)$size %||% 0,
        user$username %||% "unknown"
      ))
    }
  )

  observeEvent(input$admin_validate_db_btn, {
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    integrity <- dbGetQuery(con, "PRAGMA integrity_check")[[1]][1]
    required_tables <- c(
      "users", "budget_holders", "project_types", "reference_organisms",
      "controlled_options", "landing_text_blocks", "projects", "project_samples", "project_files",
      "email_templates", "email_logs", "project_documents", "backup_logs"
    )
    missing_tables <- setdiff(required_tables, dbListTables(con))
    if (identical(integrity, "ok") && length(missing_tables) == 0) {
      showNotification("Database validation passed.", type = "message")
    } else {
      showNotification(
        paste(c(
          paste("Integrity:", integrity),
          if (length(missing_tables)) paste("Missing tables:", paste(missing_tables, collapse = ", "))
        ), collapse = "\n"),
        type = "error",
        duration = 12
      )
    }
  })

  output$admin_users_table <- renderDT({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    datatable(dbGetQuery(con, "SELECT id, username, full_name, email, research_group, role, created_at FROM users ORDER BY username"), rownames = FALSE, selection = "single")
  })

  output$admin_budget_table <- renderDT({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    datatable(dbGetQuery(con, "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY surname, name"), rownames = FALSE, selection = "single")
  })

  output$admin_organisms_table <- renderDT({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    datatable(dbGetQuery(con, "SELECT id, name, is_active FROM reference_organisms ORDER BY name"), rownames = FALSE, selection = "single")
  })

  output$admin_options_table <- renderDT({
    admin_refresh()
    req(input$admin_option_group)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    datatable(dbGetQuery(con, "SELECT id, option_group, value, display_order, is_active FROM controlled_options WHERE option_group = ? ORDER BY display_order, value", params = list(input$admin_option_group)), rownames = FALSE, selection = "single")
  })

  output$admin_email_log_table <- renderDT({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    logs <- dbGetQuery(con, "
      SELECT el.id,
             COALESCE(p.project_code, CAST(el.project_id AS TEXT), '') AS project_code,
             el.sent_to, el.subject, el.sent_at, el.status, el.error_message
      FROM email_logs el
      LEFT JOIN projects p ON el.project_id = p.id
      ORDER BY el.id DESC
      LIMIT 200
    ")
    names(logs) <- c("id", "Project ID", "Sent to", "Subject", "Sent at", "Status", "Error message")
    datatable(logs, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$admin_landing_table <- renderDT({
    admin_refresh()
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    datatable(load_landing_text_blocks(con, include_inactive = TRUE), rownames = FALSE, selection = "single", options = list(pageLength = 10, scrollX = TRUE))
  })

  observeEvent(input$admin_add_user_btn, {
    req(input$admin_new_username, input$admin_new_email, input$admin_new_password)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    insert_record(con, "users", list(
      username = trim_scalar(input$admin_new_username),
      full_name = trim_scalar(input$admin_new_username),
      password = ms_hash_password(input$admin_new_password),
      email = trim_scalar(input$admin_new_email),
      role = ms_normalize_user_role(input$admin_new_role),
      is_admin = as.integer(role_is_admin(input$admin_new_role))
    ))
    admin_refresh(admin_refresh() + 1)
  })

  observeEvent(input$admin_delete_user_btn, {
    selected <- input$admin_users_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    users <- dbGetQuery(con, "SELECT id, username FROM users ORDER BY username")
    if (nrow(users) >= selected[1] && users$username[selected[1]] != user$username) {
      dbExecute(con, "DELETE FROM users WHERE id = ?", params = list(users$id[selected[1]]))
      admin_refresh(admin_refresh() + 1)
    }
  })

  observeEvent(input$admin_edit_user_btn, {
    selected <- input$admin_users_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    users <- dbGetQuery(con, "SELECT id, username, full_name, email, phone, research_group, role, is_admin FROM users ORDER BY username")
    req(nrow(users) >= selected[1])
    row <- users[selected[1], , drop = FALSE]
    admin_edit_user_id(row$id[[1]])
    showModal(modalDialog(
      title = "Edit User",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_save_user_btn", "Save User", class = "btn btn-primary")
      ),
      div(class = "admin-form-row",
        textInput("admin_edit_username", "Username", value = row$username[[1]]),
        textInput("admin_edit_full_name", "Full name", value = row$full_name[[1]] %||% ""),
        textInput("admin_edit_email", "Email", value = row$email[[1]] %||% ""),
        textInput("admin_edit_phone", "Phone", value = row$phone[[1]] %||% ""),
        textInput("admin_edit_research_group", "Research group", value = row$research_group[[1]] %||% ""),
        passwordInput("admin_edit_password", "New password", placeholder = "Leave blank to keep current password"),
        selectInput(
          "admin_edit_role",
          "Role",
          choices = c("User" = "user", "Technician" = "technician", "Admin" = "admin"),
          selected = ms_normalize_user_role(row$role[[1]] %||% "", row$is_admin[[1]] %||% 0L)
        )
      )
    ))
  })

  observeEvent(input$admin_save_user_btn, {
    req(admin_edit_user_id(), input$admin_edit_username, input$admin_edit_email)
    new_role <- ms_normalize_user_role(input$admin_edit_role)
    if (identical(as.integer(admin_edit_user_id()), as.integer(user$user_id))) {
      new_role <- "admin"
    }
    values <- list(
      username = trim_scalar(input$admin_edit_username),
      full_name = trim_scalar(input$admin_edit_full_name),
      email = trim_scalar(input$admin_edit_email),
      phone = trim_scalar(input$admin_edit_phone),
      research_group = trim_scalar(input$admin_edit_research_group),
      role = new_role,
      is_admin = as.integer(role_is_admin(new_role))
    )
    if (non_empty(input$admin_edit_password)) {
      values$password <- ms_hash_password(trim_scalar(input$admin_edit_password))
    }
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    update_record(con, "users", values, "id = ?", list(admin_edit_user_id()))
    removeModal()
    admin_refresh(admin_refresh() + 1)
    showNotification("User updated.", type = "message")
  })

  observeEvent(input$admin_add_bh_btn, {
    req(input$admin_bh_name, input$admin_bh_surname, input$admin_bh_cost_center, input$admin_bh_email)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    insert_record(con, "budget_holders", list(
      name = trim_scalar(input$admin_bh_name),
      surname = trim_scalar(input$admin_bh_surname),
      cost_center = trim_scalar(input$admin_bh_cost_center),
      email = trim_scalar(input$admin_bh_email)
    ))
    admin_refresh(admin_refresh() + 1)
  })

  observeEvent(input$admin_delete_bh_btn, {
    selected <- input$admin_budget_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id FROM budget_holders ORDER BY surname, name")
    if (nrow(rows) >= selected[1]) {
      dbExecute(con, "DELETE FROM budget_holders WHERE id = ?", params = list(rows$id[selected[1]]))
      admin_refresh(admin_refresh() + 1)
    }
  })

  observeEvent(input$admin_edit_bh_btn, {
    selected <- input$admin_budget_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY surname, name")
    req(nrow(rows) >= selected[1])
    row <- rows[selected[1], , drop = FALSE]
    admin_edit_bh_id(row$id[[1]])
    showModal(modalDialog(
      title = "Edit Budget Holder",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_save_bh_btn", "Save Budget Holder", class = "btn btn-primary")
      ),
      div(class = "admin-form-row",
        textInput("admin_edit_bh_name", "Name", value = row$name[[1]]),
        textInput("admin_edit_bh_surname", "Surname", value = row$surname[[1]]),
        textInput("admin_edit_bh_cost_center", "Cost center", value = row$cost_center[[1]]),
        textInput("admin_edit_bh_email", "Email", value = row$email[[1]])
      )
    ))
  })

  observeEvent(input$admin_save_bh_btn, {
    req(admin_edit_bh_id(), input$admin_edit_bh_name, input$admin_edit_bh_surname, input$admin_edit_bh_cost_center, input$admin_edit_bh_email)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "
      UPDATE budget_holders
      SET name = ?, surname = ?, cost_center = ?, email = ?
      WHERE id = ?
    ", params = list(
      trim_scalar(input$admin_edit_bh_name),
      trim_scalar(input$admin_edit_bh_surname),
      trim_scalar(input$admin_edit_bh_cost_center),
      trim_scalar(input$admin_edit_bh_email),
      admin_edit_bh_id()
    ))
    removeModal()
    admin_refresh(admin_refresh() + 1)
    showNotification("Budget holder updated.", type = "message")
  })

  observeEvent(input$admin_add_organism_btn, {
    req(input$admin_new_organism)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "INSERT OR IGNORE INTO reference_organisms (name) VALUES (?)", params = list(trim_scalar(input$admin_new_organism)))
    admin_refresh(admin_refresh() + 1)
  })

  observeEvent(input$admin_delete_organism_btn, {
    selected <- input$admin_organisms_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id FROM reference_organisms ORDER BY name")
    if (nrow(rows) >= selected[1]) {
      dbExecute(con, "DELETE FROM reference_organisms WHERE id = ?", params = list(rows$id[selected[1]]))
      admin_refresh(admin_refresh() + 1)
    }
  })

  observeEvent(input$admin_edit_organism_btn, {
    selected <- input$admin_organisms_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id, name, is_active FROM reference_organisms ORDER BY name")
    req(nrow(rows) >= selected[1])
    row <- rows[selected[1], , drop = FALSE]
    admin_edit_organism_id(row$id[[1]])
    showModal(modalDialog(
      title = "Edit Reference Organism",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_save_organism_btn", "Save Organism", class = "btn btn-primary")
      ),
      div(class = "admin-form-row",
        textInput("admin_edit_organism_name", "Reference organism", value = row$name[[1]]),
        checkboxInput("admin_edit_organism_active", "Active", value = as.integer(row$is_active[[1]] %||% 1) == 1)
      )
    ))
  })

  observeEvent(input$admin_save_organism_btn, {
    req(admin_edit_organism_id(), input$admin_edit_organism_name)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "
      UPDATE reference_organisms
      SET name = ?, is_active = ?
      WHERE id = ?
    ", params = list(
      trim_scalar(input$admin_edit_organism_name),
      as.integer(isTRUE(input$admin_edit_organism_active)),
      admin_edit_organism_id()
    ))
    removeModal()
    admin_refresh(admin_refresh() + 1)
    showNotification("Reference organism updated.", type = "message")
  })

  observeEvent(input$admin_add_option_btn, {
    req(input$admin_option_group, input$admin_option_value)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    next_order <- dbGetQuery(con, "SELECT COALESCE(MAX(display_order), 0) + 1 AS n FROM controlled_options WHERE option_group = ?", params = list(input$admin_option_group))$n[1]
    dbExecute(con, "INSERT OR IGNORE INTO controlled_options (option_group, value, display_order) VALUES (?, ?, ?)", params = list(input$admin_option_group, trim_scalar(input$admin_option_value), next_order))
    admin_refresh(admin_refresh() + 1)
  })

  observeEvent(input$admin_delete_option_btn, {
    selected <- input$admin_options_table_rows_selected
    req(selected, input$admin_option_group)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id FROM controlled_options WHERE option_group = ? ORDER BY display_order, value", params = list(input$admin_option_group))
    if (nrow(rows) >= selected[1]) {
      dbExecute(con, "DELETE FROM controlled_options WHERE id = ?", params = list(rows$id[selected[1]]))
      admin_refresh(admin_refresh() + 1)
    }
  })

  observeEvent(input$admin_edit_option_btn, {
    selected <- input$admin_options_table_rows_selected
    req(selected, input$admin_option_group)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id, option_group, value, display_order, is_active FROM controlled_options WHERE option_group = ? ORDER BY display_order, value", params = list(input$admin_option_group))
    req(nrow(rows) >= selected[1])
    row <- rows[selected[1], , drop = FALSE]
    admin_edit_option_id(row$id[[1]])
    showModal(modalDialog(
      title = "Edit Dropdown Option",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_save_option_btn", "Save Option", class = "btn btn-primary")
      ),
      div(class = "admin-form-row",
        selectInput("admin_edit_option_group", "Option group", choices = sort(unique(c(names(ms_controlled_options), "status"))), selected = row$option_group[[1]]),
        textInput("admin_edit_option_value", "Value", value = row$value[[1]]),
        numericInput("admin_edit_option_order", "Display order", value = as.integer(row$display_order[[1]] %||% 1), min = 1, step = 1),
        checkboxInput("admin_edit_option_active", "Active", value = as.integer(row$is_active[[1]] %||% 1) == 1)
      )
    ))
  })

  observeEvent(input$admin_save_option_btn, {
    req(admin_edit_option_id(), input$admin_edit_option_group, input$admin_edit_option_value)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "
      UPDATE controlled_options
      SET option_group = ?, value = ?, display_order = ?, is_active = ?
      WHERE id = ?
    ", params = list(
      trim_scalar(input$admin_edit_option_group),
      trim_scalar(input$admin_edit_option_value),
      as.integer(input$admin_edit_option_order %||% 1),
      as.integer(isTRUE(input$admin_edit_option_active)),
      admin_edit_option_id()
    ))
    removeModal()
    admin_refresh(admin_refresh() + 1)
    showNotification("Dropdown option updated.", type = "message")
  })

  observeEvent(input$admin_add_landing_btn, {
    req(input$admin_landing_key, input$admin_landing_title, input$admin_landing_body)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    dbExecute(con, "
      INSERT OR IGNORE INTO landing_text_blocks (block_key, title, body, display_order, is_active)
      VALUES (?, ?, ?, ?, ?)
    ", params = list(
      trim_scalar(input$admin_landing_key),
      trim_scalar(input$admin_landing_title),
      trim_scalar(input$admin_landing_body),
      as.integer(input$admin_landing_order %||% 10),
      as.integer(isTRUE(input$admin_landing_active))
    ))
    admin_refresh(admin_refresh() + 1)
    showNotification("Project page text block added.", type = "message")
  })

  observeEvent(input$admin_delete_landing_btn, {
    selected <- input$admin_landing_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id FROM landing_text_blocks ORDER BY display_order, title")
    if (nrow(rows) >= selected[1]) {
      dbExecute(con, "DELETE FROM landing_text_blocks WHERE id = ?", params = list(rows$id[selected[1]]))
      admin_refresh(admin_refresh() + 1)
      showNotification("Project page text block deleted.", type = "message")
    }
  })

  observeEvent(input$admin_edit_landing_btn, {
    selected <- input$admin_landing_table_rows_selected
    req(selected)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    rows <- dbGetQuery(con, "SELECT id, block_key, title, body, display_order, is_active FROM landing_text_blocks ORDER BY display_order, title")
    req(nrow(rows) >= selected[1])
    row <- rows[selected[1], , drop = FALSE]
    admin_edit_landing_id(row$id[[1]])
    showModal(modalDialog(
      title = "Edit Project Page Text Block",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_save_landing_btn", "Save Text Block", class = "btn btn-primary")
      ),
      div(class = "admin-form-row admin-form-row-wide",
        textInput("admin_edit_landing_key", "Block key", value = row$block_key[[1]]),
        textInput("admin_edit_landing_title", "Title", value = row$title[[1]]),
        numericInput("admin_edit_landing_order", "Display order", value = as.integer(row$display_order[[1]] %||% 1), min = 1, step = 1),
        checkboxInput("admin_edit_landing_active", "Active", value = as.integer(row$is_active[[1]] %||% 1) == 1),
        textAreaInput("admin_edit_landing_body", "Body", value = row$body[[1]], rows = 5)
      )
    ))
  })

  observeEvent(input$admin_save_landing_btn, {
    req(admin_edit_landing_id(), input$admin_edit_landing_key, input$admin_edit_landing_title, input$admin_edit_landing_body)
    con <- ms_db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    update_record(con, "landing_text_blocks", list(
      block_key = trim_scalar(input$admin_edit_landing_key),
      title = trim_scalar(input$admin_edit_landing_title),
      body = trim_scalar(input$admin_edit_landing_body),
      display_order = as.integer(input$admin_edit_landing_order %||% 1),
      is_active = as.integer(isTRUE(input$admin_edit_landing_active))
    ), "id = ?", list(admin_edit_landing_id()))
    removeModal()
    admin_refresh(admin_refresh() + 1)
    showNotification("Project page text block updated.", type = "message")
  })
}

shinyApp(ui = ui, server = server)
