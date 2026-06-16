# app_modified.R
library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(digest)
library(DT)
library(shinyWidgets)
library(mailR)
library(ldapr)

# Shared announcement/service blocks (shown after login in both local + LDAP modes)
# Content lives in SQLite and is editable by admins via the UI.
default_announcement_seed <- function() {
  list(
    list(
      panel_key = "announcement",
      title = "Announcement",
      subtitle = "**Updated February 2026**",
      display_order = 1L,
      items = c(
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
    ),
    list(
      panel_key = "service_info",
      title = "",
      subtitle = "",
      display_order = 2L,
      items = c(
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
    )
  )
}

is_safe_markdown_href <- function(href) {
  href <- trimws(tolower(href))
  grepl("^(https?://|mailto:|smb://)", href)
}

escape_inline_markdown <- function(text) {
  escaped <- htmltools::htmlEscape(ifelse(is.null(text) || is.na(text), "", as.character(text)))
  gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", escaped, perl = TRUE)
}

render_inline_markdown <- function(text) {
  if (is.null(text) || is.na(text)) return("")

  remaining <- as.character(text)
  out <- character()
  pattern <- "\\[([^\\]]+)\\]\\(([^)]+)\\)"

  while (nchar(remaining) > 0) {
    match_pos <- regexec(pattern, remaining, perl = TRUE)
    match_vals <- regmatches(remaining, match_pos)[[1]]

    if (length(match_vals) == 0) {
      out <- c(out, escape_inline_markdown(remaining))
      break
    }

    start <- as.integer(match_pos[[1]][1])
    len <- attr(match_pos[[1]], "match.length")[1]

    if (start > 1) {
      out <- c(out, escape_inline_markdown(substr(remaining, 1, start - 1)))
    }

    label <- match_vals[2]
    href <- trimws(match_vals[3])

    if (is_safe_markdown_href(href)) {
      out <- c(
        out,
        paste0(
          '<a href="',
          htmltools::htmlEscape(href, attribute = TRUE),
          '">',
          escape_inline_markdown(label),
          "</a>"
        )
      )
    } else {
      out <- c(out, escape_inline_markdown(match_vals[1]))
    }

    next_start <- start + len
    remaining <- if (next_start <= nchar(remaining)) {
      substr(remaining, next_start, nchar(remaining))
    } else {
      ""
    }
  }

  paste(out, collapse = "")
}

markdown_lite_to_html <- function(markdown_text) {
  if (is.null(markdown_text) || is.na(markdown_text)) return("")

  markdown_text <- gsub("\r\n?", "\n", as.character(markdown_text))
  if (trimws(markdown_text) == "") return("")

  lines <- strsplit(markdown_text, "\n", fixed = TRUE)[[1]]
  html_parts <- character()
  paragraph_buffer <- character()
  list_buffer <- character()
  list_type <- NULL

  flush_paragraph <- function() {
    if (length(paragraph_buffer) == 0) return()
    paragraph_html <- paste(
      vapply(paragraph_buffer, render_inline_markdown, character(1)),
      collapse = "<br/>"
    )
    html_parts <<- c(html_parts, paste0("<p>", paragraph_html, "</p>"))
    paragraph_buffer <<- character()
  }

  flush_list <- function() {
    if (length(list_buffer) == 0) return()
    tag_name <- if (identical(list_type, "ol")) "ol" else "ul"
    item_html <- vapply(list_buffer, function(item_text) {
      paste0("<li>", render_inline_markdown(item_text), "</li>")
    }, character(1))
    html_parts <<- c(
      html_parts,
      paste0("<", tag_name, ">", paste(item_html, collapse = ""), "</", tag_name, ">")
    )
    list_buffer <<- character()
    list_type <<- NULL
  }

  for (line in lines) {
    trimmed <- trimws(line)

    if (trimmed == "") {
      flush_paragraph()
      flush_list()
      next
    }

    if (grepl("^[-*+]\\s+", trimmed)) {
      flush_paragraph()
      if (!is.null(list_type) && !identical(list_type, "ul")) {
        flush_list()
      }
      list_type <- "ul"
      list_buffer <- c(list_buffer, sub("^[-*+]\\s+", "", trimmed))
      next
    }

    if (grepl("^[0-9]+\\.\\s+", trimmed)) {
      flush_paragraph()
      if (!is.null(list_type) && !identical(list_type, "ol")) {
        flush_list()
      }
      list_type <- "ol"
      list_buffer <- c(list_buffer, sub("^[0-9]+\\.\\s+", "", trimmed))
      next
    }

    flush_list()
    paragraph_buffer <- c(paragraph_buffer, trimmed)
  }

  flush_paragraph()
  flush_list()

  paste(html_parts, collapse = "\n")
}

announcement_blocks <- function(announcement_data) {
  if (length(announcement_data) == 0) return(NULL)

  tagList(lapply(announcement_data, function(entry) {
    panel <- entry$panel
    panel_items <- entry$items

    panel_class <- if (!is.null(panel$panel_key) && panel$panel_key == "announcement") {
      "announcement-panel"
    } else {
      "service-panel"
    }

    content_nodes <- list()

    if (!is.null(panel$title) && !is.na(panel$title) && trimws(panel$title) != "") {
      content_nodes <- c(content_nodes, list(div(class = "panel-title", panel$title)))
    }

    if (!is.null(panel$subtitle) && !is.na(panel$subtitle) && trimws(panel$subtitle) != "") {
      content_nodes <- c(content_nodes, list(
        div(class = "panel-updated", HTML(markdown_lite_to_html(panel$subtitle)))
      ))
    }

    if (nrow(panel_items) == 0) {
      content_nodes <- c(content_nodes, list(div(class = "info-box", tags$em("No content configured."))))
    } else {
      for (i in seq_len(nrow(panel_items))) {
        content_nodes <- c(content_nodes, list(
          div(
            class = "info-box",
            HTML(markdown_lite_to_html(panel_items$markdown_text[i]))
          )
        ))
      }
    }

    do.call(div, c(list(class = panel_class), content_nodes))
  }))
}

# UI definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
#    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
  tags$style("
  @font-face {
    font-family: 'Inter';
    src: url('Inter/Inter-VariableFont_opsz,wght.ttf') format('truetype');
    font-weight: 100 900;
    font-style: normal;
    font-display: swap;
  }
  @font-face {
    font-family: 'Inter';
    src: url('Inter/Inter-Italic-VariableFont_opsz,wght.ttf') format('truetype');
    font-weight: 100 900;
    font-style: italic;
    font-display: swap;
  }
  /* Reset colors and ensure visibility */
  body {
    color: #333333 !important;
    background-color: #ffffff !important;
  }
  
  .app-header {
    background-color: #2c3e50 !important;
    color: white !important;
    padding: 10px 0;
  }
  
  .login-info {
    font-size: 0.9em;
    line-height: 1.5;
    color: #333333 !important;
  }
  .login-info ul {
    margin: 10px 0;
    padding-left: 20px;
  }
  .login-info li {
    margin-bottom: 5px;
    color: #333333 !important;
  }
  .login-info a {
    color: #3498db;
    text-decoration: none;
  }
  .login-info a:hover {
    text-decoration: underline;
  }
  .modal-body .form-group {
    margin-bottom: 15px;
  }
  .shiny-input-container {
    width: 100% !important;
  }
  .action-buttons {
    margin: 20px 0;
    display: flex;
    gap: 10px;
    flex-wrap: wrap;
  }
  .projects-table {
    margin-top: 20px;
  }
  .admin-management-table {
    margin: 15px 0;
  }
  .dataTables_wrapper {
    overflow-x: auto;
  }
  /* Projects table readability (scoped: do not affect admin modal tables) */
  #projects_table_wrapper {
    width: 100%;
  }
  #projects_table_wrapper .dataTables_scrollBody {
    overflow-x: auto !important;
  }
  #projects_table.dataTable {
    width: 100% !important;
  }
  #projects_table.dataTable th,
  #projects_table.dataTable td {
    white-space: normal !important;
    word-break: break-word;
    vertical-align: top;
    font-size: 12px;
    line-height: 1.35;
    padding: 6px 8px;
  }
  .status-application-received { background-color: #fff3cd !important; color: #333333 !important; }
  .status-under-review { background-color: #cce7ff !important; color: #333333 !important; }
  .status-approved { background-color: #d4edda !important; color: #333333 !important; }
  .status-rejected { background-color: #f8d7da !important; color: #333333 !important; }
  .status-sequencing-in-progress { background-color: #e2e3e5 !important; color: #333333 !important; }
  .status-data-delivered { background-color: #d1ecf1 !important; color: #333333 !important; }
  .status-project-completed { background-color: #d4edda !important; color: #333333 !important; }
  .home-page {
    text-align: center;
    padding: 40px 20px;
    color: #333333 !important;
  }
  .home-title {
    color: #2c3e50 !important;
    margin-bottom: 30px;
    font-size: 2.5em;
  }
  .home-subtitle {
    color: #34495e !important;
    font-size: 1.3em;
    margin-bottom: 40px;
    line-height: 1.6;
  }
  .home-features {
    display: flex;
    justify-content: center;
    gap: 30px;
    margin-top: 40px;
    flex-wrap: wrap;
  }
  .feature-card {
    background: white;
    padding: 30px;
    border-radius: 10px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    width: 300px;
    text-align: center;
    color: #333333 !important;
  }
  .feature-icon {
    font-size: 3em;
    color: #3498db;
    margin-bottom: 20px;
  }
  .feature-title {
    font-size: 1.5em;
    color: #2c3e50 !important;
    margin-bottom: 15px;
  }
  .feature-description {
    color: #7f8c8d !important;
    line-height: 1.5;
  }
  .announcement-panel,
  .service-panel {
    background-color: #cadced;
    border: 1px solid #d8c4f0;
    border-radius: 6px;
    padding: 16px 18px;
    margin: 20px 0;
    color: #2c3e50 !important;
    font-family: 'Inter', 'Helvetica Neue', Arial, sans-serif;
    font-weight: 200;
    font-style: italic;
  }
  .panel-title {
    font-size: 1.2em;
    font-weight: 700;
    color: #2c3e50 !important;
    margin-bottom: 8px;
  }
  .panel-updated {
    font-weight: 700;
    margin-bottom: 12px;
  }
  .info-box {
    border: 1.5pt solid #c9b7ea;
    border-radius: 4px;
    background-color: #ffffff;
    padding: 10px 12px;
    margin: 8px 0;
    line-height: 1.45;
  }
  .info-box p {
    margin: 0 0 8px 0;
  }
  .info-box ul {
    margin: 0 0 0 20px;
    padding-left: 18px;
  }
  .info-box a {
    color: #2c3e50;
    text-decoration: underline;
  }
  .header-container {
    display: flex;
    justify-content: space-between;
    align-items: center;
    width: 100%;
    padding: 0 20px;
  }
  .header-left {
    display: flex;
    align-items: center;
    gap: 15px;
  }
  .header-right {
    display: flex;
    align-items: center;
    gap: 15px;
  }
  .header-logo {
    height: 50px;
    width: auto;
  }
  #main_app {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }
  #main_content {
    flex: 1 0 auto;
  }
  .app-footer {
    flex-shrink: 0;
    padding: 10px 20px 18px 20px;
    text-align: left;
  }
  .footer-logo {
    height: 104px;
    width: auto;
    max-width: 720px;
  }
  .institute-name {
    color: white !important;
    font-size: 1.2em;
    font-weight: bold;
    margin: 0;
  }
  .app-title {
    color: white !important;
    font-size: 1.5em;
    font-weight: bold;
    margin: 0;
    text-align: center;
    flex-grow: 1;
  }
  .user-info-panel {
    display: flex;
    align-items: center;
    gap: 15px;
    color: white !important;
  }
  .user-info-panel .shiny-text-output {
    color: white !important;
  }
  .instructions-panel {
    background-color: #f8f9fa;
    border-left: 4px solid #3498db;
    padding: 20px;
    margin: 20px 0;
    border-radius: 4px;
    color: #333333 !important;
  }
  .instructions-title {
    color: #2c3e50 !important;
    font-size: 1.3em;
    margin-bottom: 15px;
    font-weight: bold;
  }
  .instructions-list {
    color: #34495e !important;
    line-height: 1.6;
    margin: 0;
    padding-left: 20px;
  }
  .instructions-list li {
    margin-bottom: 8px;
    color: #34495e !important;
  }
  .cost-calculation {
    background-color: #f8f9fa;
    border: 1px solid #dee2e6;
    border-radius: 5px;
    padding: 15px;
    margin-top: 20px;
    color: #333333 !important;
  }
  .cost-total {
    font-size: 1.2em;
    font-weight: bold;
    color: #2c3e50 !important;
  }
  .cost-warning {
    color: #e74c3c !important;
    font-style: italic;
  }

  .dev-mode-badge {
    display: inline-block;
    padding: 4px 8px;
    border-radius: 12px;
    font-size: 0.8em;
    font-weight: bold;
    color: white !important;
  }
  .dev-mode-ldap { background-color: #17a2b8 !important; }
  .dev-mode-local { background-color: #f39c12 !important; }
  
/*   Ensure all text is visible */
 /* * {                         */
 /*   color: #333333 !important;*/
 /* }                           */
  
  h1, h2, h3, h4, h5, h6 {
    color: #2c3e50 !important;
  }
  
  p, span, div {
    color: #333333 !important;
  }
  "),
    tags$script("
      function publishUrlSearch() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('client_url_search', window.location.search || '', {priority: 'event'});
        }
      }

      function adjustProjectsTable() {
        if (!window.jQuery || !$.fn || !$.fn.dataTable) return;
        var tableEl = $('#projects_table');
        if (tableEl.length === 0) return;

        if ($.fn.dataTable.isDataTable(tableEl[0])) {
          tableEl.DataTable().columns.adjust();
        }
      }

      $(document).ready(function() {
        $(document).on('keyup', '#login_username, #login_password', function(e) {
          if (e.keyCode === 13) {
            $('#login_btn').click();
          }
        });

        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"], a[data-bs-toggle=\"tab\"]', function() {
          setTimeout(adjustProjectsTable, 50);
        });

        var resizeTimer = null;
        $(window).on('resize', function() {
          clearTimeout(resizeTimer);
          resizeTimer = setTimeout(adjustProjectsTable, 150);
        });

        publishUrlSearch();
        $(window).on('popstate hashchange', publishUrlSearch);
        setTimeout(publishUrlSearch, 300);
        setTimeout(adjustProjectsTable, 250);
      });
    ")
  ),
  
  # Login screen
  div(
    id = "login_screen",
    class = "login-container",
    div(
      class = "login-box",
      h2("Sequencing Project Application", class = "login-title"),
      div(
        class = "login-info",
        style = "text-align: left; margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #3498db;",
        h4("NGS Sequencing Service", style = "color: #2c3e50; margin-top: 0;"),
        p("The NGS lab of the Core Facility is providing sequencing as a service. We highly recommend to contact us before the start of your experiment. In collaboration with the Bioinformatics Core Facility, we provide:"),
        tags$ul(
          tags$li("Assistance with experimental design of the studies (required number of samples and replicates)"),
          tags$li("Assistance with the use of NGS open source analysis tools"),
          tags$li("Data analysis on collaborative basis")
        ),
        p("If you have any further questions, please do not hesitate to ", 
          tags$a(href = "mailto:ngs.biochem.mpg.de", "contact us @ NGS"), "!"),
        
        h4("Registration", style = "color: #2c3e50; margin-top: 20px;"),
        p("If you want to use the NGS service for the first time, you have to ",
          tags$a(href = "https://ngs-vm.biochem.mpg.de/register.cgi", "register for an account", target = "_blank"),
          ". Please use your MPIB credentials for account registration."),
        
        h4("Login", style = "color: #2c3e50; margin-top: 20px;"),
        p("After registration is complete, you can directly login with your MPIB account to submit your new project(s).")
      ),
      textInput("login_username", "Username", placeholder = "Enter your username"),
      passwordInput("login_password", "Password", placeholder = "Enter your password"),
      actionButton("login_btn", "Login", class = "btn-login"),
      div(
        class = "login-links",
        actionLink("show_register_btn", "New user? Register here"),
        style = "text-align: center; margin-top: 1rem;"
      ),
      uiOutput("dev_tools_ui"),
      uiOutput("dev_login_ui"),
      div(id = "login_error", class = "error-message")
    )
  ),
  
  # Main application (hidden until login)
  hidden( 
    div(
      id = "main_app",
      
      # Header with logos and logout on right
      div(
        class = "app-header",
        div(
          class = "header-container",
          div(
            class = "header-left",
            img(src = "logo.png", class = "header-logo", alt = "Logo"),
            h3("MPIB Sequencing facility", class = "institute-name")
          ),
          div(
            class = "app-title",
            "Sequencing Project Application System"
          ),
          div(
            class = "header-right",
            img(src = "minerva.png", class = "header-logo", alt = "Minerva"),
            div(
              class = "user-info-panel",
              textOutput("welcome_user"),
              uiOutput("dev_mode_badge"),
              actionButton("logout_btn", "End Session", class = "btn-logout")
            )
          )
        )
      ),
      
      # Main content with tabs for admins
      uiOutput("main_content"),
      div(
        class = "app-footer",
        img(
          src = "bioinfo_core_concept_01_lockup.svg",
          class = "footer-logo",
          alt = "Bioinformatics Core Facility"
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  if (Sys.getenv("LDAP_DEBUG", "") == "1") {
    cat(
      "LDAP DEBUG: startup env",
      "AUTH_MODE=", Sys.getenv("AUTH_MODE", "<unset>"),
      "APP_ENV=", Sys.getenv("APP_ENV", "<unset>"),
      "ALLOW_UNTRUSTED_AUTH_FALLBACK=", Sys.getenv("ALLOW_UNTRUSTED_AUTH_FALLBACK", "<unset>"),
      "TRUST_PROXY_AUTH_USER_QUERY=", Sys.getenv("TRUST_PROXY_AUTH_USER_QUERY", "<unset>"),
      "\n",
      file = stderr()
    )
    flush.console()

    session$onFlushed(function() {
      tryCatch({
        req <- session$request
        keys <- names(req)
        keys <- keys[grepl("^HTTP_|REMOTE_USER$", keys)]
        cat("LDAP DEBUG: session headers snapshot\n", file = stderr())
        if (length(keys) == 0) {
          cat("  (no HTTP_* or REMOTE_USER headers found)\n", file = stderr())
        } else {
          for (k in keys) {
            val <- req[[k]]
            cat("  ", k, "=", ifelse(is.null(val) || val == "", "<empty>", val), "\n", file = stderr())
          }
        }

        x_remote <- req$HTTP_X_REMOTE_USER
        remote_user <- req$REMOTE_USER
        x_forwarded <- req$HTTP_X_FORWARDED_USER
        authz <- req$HTTP_AUTHORIZATION

        cat(
          "LDAP DEBUG: resolved auth candidates",
          "X_REMOTE_USER=", ifelse(is.null(x_remote) || x_remote == "", "<missing>", x_remote),
          "REMOTE_USER=", ifelse(is.null(remote_user) || remote_user == "", "<missing>", remote_user),
          "X_FORWARDED_USER=", ifelse(is.null(x_forwarded) || x_forwarded == "", "<missing>", x_forwarded),
          "AUTHORIZATION=", ifelse(is.null(authz) || authz == "", "<missing>", "<present>"),
          "CLIENT_URL_SEARCH=", client_url_search() %||% "<missing>",
          "SESSION_URL_SEARCH=", scalar_text(session$clientData$url_search) %||% "<missing>",
          "REQ_QUERY_STRING=", scalar_text(req$QUERY_STRING) %||% "<missing>",
          "\n",
          file = stderr()
        )
      }, error = function(e) {
        cat("LDAP DEBUG ERROR (onFlushed):", e$message, "\n", file = stderr())
      })
      flush.console()
    }, once = TRUE)
  }

  ##################################################################
  # ANNOUNCEMENT STORAGE HELPERS
  ##################################################################

  ensure_announcement_tables <- function(con) {
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

    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS announcement_items (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        panel_id INTEGER NOT NULL,
        display_order INTEGER NOT NULL,
        markdown_text TEXT NOT NULL,
        is_active INTEGER NOT NULL DEFAULT 1,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_by TEXT,
        FOREIGN KEY (panel_id) REFERENCES announcement_panels(id)
      )
    ")
  }

  seed_announcement_defaults <- function(con, updated_by = "system") {
    seed <- default_announcement_seed()
    if (length(seed) == 0) return(invisible(NULL))

    existing_panels <- dbGetQuery(
      con,
      "SELECT id, panel_key FROM announcement_panels"
    )

    for (panel in seed) {
      panel_key <- panel$panel_key
      panel_row <- existing_panels[existing_panels$panel_key == panel_key, , drop = FALSE]
      inserted_panel <- FALSE

      if (nrow(panel_row) == 0) {
        dbExecute(con, "
          INSERT INTO announcement_panels (panel_key, title, subtitle, display_order, is_active, updated_by)
          VALUES (?, ?, ?, ?, 1, ?)
        ", params = list(
          panel_key,
          panel$title,
          panel$subtitle,
          as.integer(panel$display_order),
          updated_by
        ))

        panel_id <- dbGetQuery(
          con,
          "SELECT id FROM announcement_panels WHERE panel_key = ?",
          params = list(panel_key)
        )$id[1]
        inserted_panel <- TRUE
      } else {
        panel_id <- panel_row$id[1]
      }

      if (isTRUE(inserted_panel) && length(panel$items) > 0) {
        for (idx in seq_along(panel$items)) {
          dbExecute(con, "
            INSERT INTO announcement_items (panel_id, display_order, markdown_text, is_active, updated_by)
            VALUES (?, ?, ?, 1, ?)
          ", params = list(
            panel_id,
            as.integer(idx),
            panel$items[[idx]],
            updated_by
          ))
        }
      }
    }
  }

  load_announcement_content <- function(con = NULL, active_only = TRUE) {
    close_con <- FALSE
    if (is.null(con)) {
      con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
      close_con <- TRUE
    }
    on.exit(if (close_con) dbDisconnect(con), add = TRUE)

    ensure_announcement_tables(con)
    seed_announcement_defaults(con)

    panel_query <- "
      SELECT id, panel_key, title, subtitle, display_order, is_active
      FROM announcement_panels
    "
    item_query <- "
      SELECT id, panel_id, display_order, markdown_text, is_active
      FROM announcement_items
    "

    if (isTRUE(active_only)) {
      panel_query <- paste(panel_query, "WHERE is_active = 1")
      item_query <- paste(item_query, "WHERE is_active = 1")
    }

    panel_query <- paste(panel_query, "ORDER BY display_order, id")
    item_query <- paste(item_query, "ORDER BY display_order, id")

    panels <- dbGetQuery(con, panel_query)
    items <- dbGetQuery(con, item_query)

    lapply(seq_len(nrow(panels)), function(i) {
      panel_row <- panels[i, , drop = FALSE]
      panel_items <- items[items$panel_id == panel_row$id[1], , drop = FALSE]
      list(panel = panel_row, items = panel_items)
    })
  }
  
  ##################################################################
  # DATABASE VALIDATION AND REPAIR FUNCTIONS
  ##################################################################
  
  # Database validation and repair function
  validate_and_repair_database <- function() {
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
      on.exit(dbDisconnect(con), add = TRUE)

      ensure_projects_additional_cost_column(con)
      ensure_users_full_name_column(con)
      ensure_announcement_tables(con)
      seed_announcement_defaults(con)
      
      # Check if all essential tables exist
      required_tables <- c("users", "projects", "budget_holders", "service_types", 
                           "sequencing_depths", "sequencing_cycles", "types",
                           "sequencing_platforms", "reference_genomes",
                           "announcement_panels", "announcement_items")
      
      existing_tables <- dbListTables(con)
      missing_tables <- setdiff(required_tables, existing_tables)
      
      if (length(missing_tables) > 0) {
        showNotification(paste("Missing tables detected:", paste(missing_tables, collapse = ", ")), 
                         type = "warning", duration = 10)
        # Don't auto-recreate, just warn the admin
        return(FALSE)
      }
      
      # Test basic query
      dbGetQuery(con, "SELECT 1 as test")

      migration_result <- tryCatch(
        normalize_legacy_responsible_users(con),
        error = function(e) {
          cat("RESPONSIBLE USER MIGRATION ERROR:", conditionMessage(e), "\n", file = stderr())
          list(updated_rows = 0L, normalized_values = 0L, unresolved_values = character(0))
        }
      )
      if (length(migration_result$unresolved_values) > 0) {
        showNotification(
          paste0(
            "Some legacy responsible-user values could not be auto-mapped (",
            length(migration_result$unresolved_values),
            "). Please review project assignments."
          ),
          type = "warning",
          duration = 12
        )
      }

      return(TRUE)
      
    }, error = function(e) {
      showNotification(paste("Database error:", e$message), type = "error", duration = 10)
      return(FALSE)
    })
  }
  
  # Check database on app start
  observe({
    if (!validate_and_repair_database()) {
      showModal(modalDialog(
        title = "Database Issue Detected",
        tagList(
          p("There seems to be an issue with the database."),
          p("Please contact the administrator."),
          p("Do NOT recreate the database as this will lose all existing data."),
          br(),
          actionButton("debug_db_btn", "Show Database Debug Info", class = "btn-info")
        ),
        footer = modalButton("Close"),
        size = "m"
      ))
    }
  })
  
  # Debug database info
  observeEvent(input$debug_db_btn, {
    tryCatch({
      con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
      tables <- dbListTables(con)
      table_counts <- sapply(tables, function(table) {
        count <- dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))$count
        paste0(table, ": ", count, " rows")
      })
      dbDisconnect(con)
      
      showModal(modalDialog(
        title = "Database Debug Information",
        tagList(
          h4("Tables and Row Counts:"),
          pre(paste(table_counts, collapse = "\n")),
          h4("Database File Info:"),
          pre(paste(capture.output(file.info("sequencing_projects.db")), collapse = "\n"))
        ),
        footer = modalButton("Close"),
        size = "l"
      ))
    }, error = function(e) {
      showNotification(paste("Debug failed:", e$message), type = "error")
    })
  })
  
  ##################################################################
  # END DATABASE VALIDATION AND REPAIR FUNCTIONS
  ##################################################################
  
  # Reactive values
  user <- reactiveValues(
    logged_in = FALSE,
    username = NULL,
    full_name = NULL,
    user_id = NULL,
    is_admin = FALSE
  )
  projects_data <- reactiveVal()
  edit_existing_additional_cost <- reactiveVal(NA_real_)
  announcement_refresh <- reactiveVal(0)
  announcement_items_current <- reactiveVal(data.frame())
  announcement_editing_item_id <- reactiveVal(NULL)
  max_announcement_chars <- 4000L
  
  # Reactive values for admin management
  admin_data <- reactiveValues(
    users = data.frame(),
    reference_genomes = c(),
    budget_holders = data.frame(),
    types = data.frame(),
    service_types = data.frame(),
    sequencing_depths = data.frame(),
    sequencing_cycles = data.frame(),
    sequencing_platforms = data.frame()
  )

  ##################################################################
  # AUTH / LDAP HELPERS
  ##################################################################

  initial_auth_mode <- tolower(Sys.getenv("AUTH_MODE", "local"))
  if (!initial_auth_mode %in% c("ldap", "local")) {
    initial_auth_mode <- "local"
  }
  auth_mode_val <- reactiveVal(initial_auth_mode)
  get_auth_mode <- function() auth_mode_val()

  to_bool <- function(x) {
    tolower(trimws(x)) %in% c("1", "true", "yes", "on")
  }

  app_env_is_dev <- tolower(Sys.getenv("APP_ENV", "")) == "dev"
  dev_mode <- app_env_is_dev || to_bool(Sys.getenv("SHOW_DEV_TOOLS", ""))
  # Security: untrusted URL/cookie fallback must be explicitly enabled.
  # Never auto-enable it just because APP_ENV=dev.
  allow_untrusted_auth_fallback <- to_bool(Sys.getenv("ALLOW_UNTRUSTED_AUTH_FALLBACK", ""))
  # Trusted only when Apache canonicalizes auth_user from REMOTE_USER.
  trust_proxy_auth_user_query <- to_bool(Sys.getenv("TRUST_PROXY_AUTH_USER_QUERY", ""))

  ldap_warned <- reactiveVal(FALSE)
  ldap_bind_warned <- reactiveVal(FALSE)
  ldap_login_blocked <- reactiveVal(FALSE)
  dev_auth_user <- reactiveVal(Sys.getenv("DEV_REMOTE_USER", ""))
  client_url_search <- reactiveVal("")

  pending_profile <- reactiveValues(
    active = FALSE,
    username = NULL,
    attrs = list(),
    is_new = FALSE
  )

  `%||%` <- function(x, y) {
    if (!is.null(x) && !is.na(x) && x != "") x else y
  }

  scalar_text <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    x <- as.character(x[[1]])
    x <- trimws(x)
    if (is.na(x) || x == "") return(NULL)
    x
  }

  observeEvent(input$client_url_search, {
    qs <- scalar_text(input$client_url_search)
    client_url_search(qs %||% "")
    if (Sys.getenv("LDAP_DEBUG", "") == "1") {
      cat("LDAP DEBUG: client url_search =", client_url_search(), "\n", file = stderr())
      flush.console()
    }
  }, ignoreInit = FALSE)

  get_req_header <- function(req, header_name) {
    if (is.null(req) || is.null(header_name) || header_name == "") return(NULL)

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

    for (v in candidates) {
      vv <- scalar_text(v)
      if (!is.null(vv)) return(vv)
    }
    NULL
  }

  get_proxy_authenticated_user <- function(session) {
    req <- session$request
    candidates <- c(
      get_req_header(req, "x-remote-user"),
      get_req_header(req, "remote-user"),
      scalar_text(req$REMOTE_USER),
      get_req_header(req, "x-forwarded-user")
    )
    candidates <- candidates[!is.na(candidates) & candidates != ""]
    if (length(candidates) == 0) return(NULL)
    candidates[1]
  }

  base64_decode_raw <- function(encoded) {
    encoded <- gsub("[[:space:]]", "", encoded)
    if (encoded == "" || nchar(encoded) %% 4 != 0) return(NULL)

    alphabet <- c(LETTERS, letters, as.character(0:9), "+", "/")
    map <- stats::setNames(0:63, alphabet)
    chars <- strsplit(encoded, "", fixed = TRUE)[[1]]
    values <- rep(NA_integer_, length(chars))

    for (i in seq_along(chars)) {
      ch <- chars[i]
      if (ch == "=") {
        values[i] <- 0L
      } else {
        v <- map[[ch]]
        if (is.null(v)) return(NULL)
        values[i] <- as.integer(v)
      }
    }

    out <- raw()
    for (i in seq(1, length(values), by = 4)) {
      v <- values[i:(i + 3)]
      b1 <- bitwShiftL(v[1], 2) + bitwShiftR(v[2], 4)
      b2 <- bitwShiftL(bitwAnd(v[2], 15), 4) + bitwShiftR(v[3], 2)
      b3 <- bitwShiftL(bitwAnd(v[3], 3), 6) + v[4]
      out <- c(out, as.raw(c(b1, b2, b3)))
    }

    pad <- sum(chars == "=")
    if (pad > 0 && length(out) >= pad) {
      out <- out[seq_len(length(out) - pad)]
    }
    out
  }

  parse_basic_auth_username <- function(auth_header) {
    auth_header <- scalar_text(auth_header)
    if (is.null(auth_header)) return(NULL)

    m <- regexec("^[Bb]asic[[:space:]]+(.+)$", auth_header)
    mm <- regmatches(auth_header, m)
    if (length(mm) == 0 || length(mm[[1]]) < 2) return(NULL)

    token <- trimws(mm[[1]][2])
    if (token == "") return(NULL)

    decoded <- tryCatch({
      raw <- base64_decode_raw(token)
      if (is.null(raw) || length(raw) == 0) return(NULL)
      rawToChar(raw)
    }, error = function(e) NULL)
    if (is.null(decoded) || decoded == "") return(NULL)

    user <- sub(":.*$", "", decoded)
    user <- trimws(user)
    if (is.na(user) || user == "") return(NULL)
    user
  }

  get_auth_username <- function(session) {
    if (get_auth_mode() != "ldap") return(NULL)

    req <- session$request
    basic_user <- parse_basic_auth_username(get_req_header(req, "authorization"))
    candidates <- c(get_proxy_authenticated_user(session), basic_user)
    candidates <- candidates[!is.na(candidates) & candidates != ""]
    header_user <- if (length(candidates) > 0) candidates[1] else NULL

    if (!is.null(header_user) && header_user != "") {
      if (ldap_login_blocked()) return(NULL)
      return(header_user)
    }

    parse_auth_user_from_qs <- function(qs) {
      qs <- scalar_text(qs)
      if (is.null(qs)) return(NULL)
      parsed <- shiny::parseQueryString(qs)
      if (is.null(parsed$auth_user) || parsed$auth_user == "") return(NULL)
      auth_vals <- parsed$auth_user
      if (length(auth_vals) > 1) auth_vals <- auth_vals[length(auth_vals)]
      scalar_text(auth_vals)
    }

    query_candidates <- unique(c(
      client_url_search(),
      session$clientData$url_search,
      req$QUERY_STRING
    ))

    if (trust_proxy_auth_user_query) {
      for (qs in query_candidates) {
        auth_user_qs <- parse_auth_user_from_qs(qs)
        if (!is.null(auth_user_qs) && auth_user_qs != "") return(auth_user_qs)
      }
    }

    # Optional emergency fallback for controlled debugging only.
    # This is intentionally disabled unless ALLOW_UNTRUSTED_AUTH_FALLBACK=true.
    if (allow_untrusted_auth_fallback) {
      for (qs in query_candidates) {
        auth_user_qs <- parse_auth_user_from_qs(qs)
        if (!is.null(auth_user_qs) && auth_user_qs != "") return(auth_user_qs)
      }

      cookie_header <- session$request$HTTP_COOKIE
      if (!is.null(cookie_header) && cookie_header != "") {
        cookies <- unlist(strsplit(cookie_header, ";"))
        cookies <- trimws(cookies)
        auth_kv <- cookies[grepl("^auth_user=", cookies)]
        if (length(auth_kv) > 0) {
          auth_val <- sub("^auth_user=", "", auth_kv[1])
          if (!is.null(auth_val) && auth_val != "") {
            return(utils::URLdecode(auth_val))
          }
        }
      }
    }

    if (dev_mode) {
      dev_user <- dev_auth_user()
      if (!is.null(dev_user) && dev_user != "") return(dev_user)
    }
    return(NULL)
  }

  parse_ldap_uris <- function(uri_string) {
    if (is.null(uri_string) || uri_string == "") return(list())
    uris <- trimws(unlist(strsplit(uri_string, ",")))
    uris <- uris[uris != ""]
    lapply(uris, function(uri) {
      scheme <- ifelse(grepl("^ldaps://", uri, ignore.case = TRUE), "ldaps", "ldap")
      host_port <- sub("^ldaps?://", "", uri, ignore.case = TRUE)
      host_port <- sub("/.*$", "", host_port)
      host <- host_port
      port <- NA_integer_
      if (grepl(":", host_port, fixed = TRUE)) {
        parts <- unlist(strsplit(host_port, ":", fixed = TRUE))
        host <- parts[1]
        port <- suppressWarnings(as.integer(parts[2]))
      }
      if (is.na(port)) {
        port <- if (scheme == "ldaps") 636L else 389L
      }
      list(host = host, port = port, use_ssl = (scheme == "ldaps"))
    })
  }

  extract_ldap_attr <- function(result, attr) {
    if (is.null(result) || is.null(attr) || attr == "") return(NULL)
    value <- NULL
    if (is.data.frame(result) && attr %in% names(result)) {
      value <- result[[attr]][1]
    } else if (is.list(result)) {
      if (!is.null(result[[attr]])) {
        value <- result[[attr]][1]
      } else if (length(result) > 0 && is.list(result[[1]]) && !is.null(result[[1]][[attr]])) {
        value <- result[[1]][[attr]][1]
      }
    }
    if (is.null(value) || length(value) == 0 || is.na(value)) return(NULL)
    as.character(value)
  }

  ldap_lookup_user <- function(username) {
    attrs <- list(email = NULL, phone = NULL, ou = NULL, full_name = NULL, bind_ok = NA)
    if (is.null(username) || username == "") return(attrs)

    default_uri <- paste(
      "ldaps://ldapserv1.biochem.mpg.de",
      "ldaps://ldapserv2.biochem.mpg.de",
      "ldaps://ldapserv3.biochem.mpg.de",
      sep = ","
    )
    ldap_uri <- Sys.getenv("LDAP_URI", default_uri)
    ldap_base_dn <- Sys.getenv("LDAP_BASE_DN", "dc=biochem,dc=mpg,dc=de")
    attr_uid <- Sys.getenv("LDAP_ATTR_UID", "uid")
    attr_mail <- Sys.getenv("LDAP_ATTR_MAIL", "mail")
    attr_phone <- Sys.getenv("LDAP_ATTR_PHONE", "telephoneNumber")
    attr_ou <- Sys.getenv("LDAP_ATTR_OU", "ou")
    attr_cn <- Sys.getenv("LDAP_ATTR_CN", "cn")
    attr_given <- Sys.getenv("LDAP_ATTR_GIVENNAME", "givenName")
    attr_sn <- Sys.getenv("LDAP_ATTR_SN", "sn")
    bind_dn <- Sys.getenv("LDAP_BIND_DN", "")
    bind_pw <- Sys.getenv("LDAP_BIND_PW", "")

    bind_base_dn <- ""
    if (bind_dn != "" && grepl(",", bind_dn, fixed = TRUE)) {
      bind_base_dn <- sub("^[^,]+,", "", bind_dn)
    }

    ldap_targets <- parse_ldap_uris(ldap_uri)
    if (length(ldap_targets) == 0) return(attrs)

    filter <- sprintf("(&(%s=%s)(objectClass=posixAccount))", attr_uid, username)

    ldap_try_bind <- function(conn, bind_dn, bind_pw) {
      if (bind_dn != "" && bind_pw != "") {
        rdn <- unlist(strsplit(bind_dn, ",", fixed = TRUE))[1]
        parts <- unlist(strsplit(rdn, "=", fixed = TRUE))
        if (length(parts) == 2) {
          attr <- tolower(trimws(parts[1]))
          user <- trimws(parts[2])
          if (attr %in% c("cn", "uid")) {
            ok <- tryCatch({
              conn$bind(user = user, pw = bind_pw, type = attr)
              TRUE
            }, error = function(e) FALSE)
            if (ok) return(TRUE)
          }
        }

        if (!grepl("=", bind_dn, fixed = TRUE)) {
          return(tryCatch({
            conn$bind(user = bind_dn, pw = bind_pw, type = "uid")
            TRUE
          }, error = function(e) FALSE))
        }

        return(FALSE)
      }

      tryCatch({
        conn$bind()
        TRUE
      }, error = function(e) {
        tryCatch({
          conn$bind(user = "", pw = "", type = "uid")
          TRUE
        }, error = function(e2) FALSE)
      })
    }

    ldap_search <- function(conn, base_dn, filter, attrs) {
      tryCatch({
        conn$search(base = base_dn, filter = filter, attrs = attrs)
      }, error = function(e1) {
        tryCatch({
          conn$search(base = base_dn, filter = filter, attributes = attrs)
        }, error = function(e2) {
          tryCatch({
            conn$search(base = base_dn, filter = filter)
          }, error = function(e3) {
            tryCatch({
              conn$search(filter = filter)
            }, error = function(e4) NULL)
          })
        })
      })
    }

    ldapsearch_lookup <- function(uri, base_dn, filter, attrs, bind_dn, bind_pw) {
      ldapsearch_cmd <- Sys.getenv("LDAPSEARCH_PATH", "ldapsearch")
      args <- c("-LLL", "-x", "-H", uri)

      pw_file <- NULL
      if (!is.null(bind_dn) && bind_dn != "" && !is.null(bind_pw) && bind_pw != "") {
        pw_file <- tempfile("ldap_pw_")
        writeLines(bind_pw, pw_file)
        Sys.chmod(pw_file, "600")
        args <- c(args, "-D", bind_dn, "-y", pw_file)
      }

      args <- c(args, "-b", base_dn, filter, attrs)

      output <- tryCatch({
        quoted_args <- vapply(args, shQuote, character(1))
        cmd <- paste(shQuote(ldapsearch_cmd), paste(quoted_args, collapse = " "))
        cmd_with_err <- paste(cmd, "2>&1")
        out <- system(cmd_with_err, intern = TRUE)
        attr(out, "cmd") <- cmd
        out
      }, error = function(e) NULL)

      if (!is.null(pw_file) && file.exists(pw_file)) {
        unlink(pw_file)
      }

      if (dev_mode && Sys.getenv("LDAP_DEBUG", "") == "1") {
        cmd_preview <- attr(output, "cmd")
        if (is.null(cmd_preview)) {
          cmd_preview <- paste(shQuote(ldapsearch_cmd), paste(vapply(args, shQuote, character(1)), collapse = " "))
        }
        cat("LDAPSEARCH CMD:\n", cmd_preview, "\n")
        cat("LDAPSEARCH STATUS:\n", attr(output, "status"), "\n")
        if (is.null(output) || length(output) == 0) {
          cat("LDAPSEARCH OUTPUT: (empty)\n")
        }
      }

      if (is.null(output)) return(list())
      status <- attr(output, "status")
      if (!is.null(status) && status != 0) return(list())
      output
    }

    normalize_ldap_lines <- function(lines) {
      if (is.null(lines) || length(lines) == 0) return(character(0))
      lines <- gsub("\r", "", lines, fixed = TRUE)
      out <- character(0)
      for (line in lines) {
        if (grepl("^[ \t]", line)) {
          if (length(out) > 0) {
            out[length(out)] <- paste0(out[length(out)], sub("^[ \t]+", "", line))
          }
        } else {
          out <- c(out, line)
        }
      }
      out
    }

    parse_ldapsearch_output <- function(lines, key) {
      if (is.null(lines) || length(lines) == 0) return(NULL)
      lines <- normalize_ldap_lines(lines)
      safe_key <- gsub("([\\^\\$\\.|\\?\\*\\+\\(\\)\\[\\]\\{\\}\\\\])", "\\\\\\1", key)
      pattern <- paste0("^", safe_key, "(;[^:]*)?::?[[:space:]]*(.*)$")
      matches <- regexec(pattern, lines, ignore.case = TRUE)
      match_list <- regmatches(lines, matches)
      values <- vapply(match_list, function(m) {
        if (length(m) > 2) m[3] else NA_character_
      }, character(1))
      values <- values[!is.na(values) & values != ""]
      if (length(values) > 0) values[1] else NULL
    }

    for (target in ldap_targets) {
      l <- tryCatch({
        if (isTRUE(target$use_ssl)) {
          try(assignInNamespace(
            x = "ldap_string",
            value = function(host, port) paste0("ldaps://", host, ":", port),
            ns = "ldapr"
          ), silent = TRUE)
        }
        base_for_bind <- if (bind_base_dn != "") bind_base_dn else ldap_base_dn
        tryCatch({
          ldapr::ldap$new(host = target$host, base_dn = base_for_bind, port = target$port)
        }, error = function(e) {
          ldapr::ldap$new(host = target$host, base_dn = base_for_bind)
        })
      }, error = function(e) NULL)

      if (is.null(l)) next

      bind_ok <- ldap_try_bind(l, bind_dn, bind_pw)
      attrs$bind_ok <- bind_ok

      result <- NULL
      if (isTRUE(bind_ok)) {
        result <- ldap_search(l, ldap_base_dn, filter, c(attr_mail, attr_phone, attr_ou, attr_cn, attr_given, attr_sn))
      }

      if (!is.null(result)) {
        attrs$email <- extract_ldap_attr(result, attr_mail)
        attrs$phone <- extract_ldap_attr(result, attr_phone)
        attrs$ou <- extract_ldap_attr(result, attr_ou)
        attrs$full_name <- extract_ldap_attr(result, attr_cn)
        if (is.null(attrs$full_name) || attrs$full_name == "") {
          given <- extract_ldap_attr(result, attr_given)
          sn <- extract_ldap_attr(result, attr_sn)
          if (!is.null(given) && given != "" && !is.null(sn) && sn != "") {
            attrs$full_name <- paste(given, sn)
          } else if (!is.null(given) && given != "") {
            attrs$full_name <- given
          } else if (!is.null(sn) && sn != "") {
            attrs$full_name <- sn
          }
        }
      }

      needs_lookup <- is.null(attrs$email) || is.null(attrs$phone) || is.null(attrs$ou) || is.null(attrs$full_name)
      if (needs_lookup) {
        ldapsearch_lines <- ldapsearch_lookup(
          uri = if (isTRUE(target$use_ssl)) paste0("ldaps://", target$host, ":", target$port) else paste0("ldap://", target$host, ":", target$port),
          base_dn = ldap_base_dn,
          filter = filter,
          attrs = c(attr_mail, attr_phone, attr_ou, attr_cn, attr_given, attr_sn),
          bind_dn = bind_dn,
          bind_pw = bind_pw
        )
        if (dev_mode && Sys.getenv("LDAP_DEBUG", "") == "1") {
          cat("LDAPSEARCH OUTPUT:\n", paste(ldapsearch_lines, collapse = "\n"), "\n")
        }
        if (is.null(attrs$email)) {
          attrs$email <- parse_ldapsearch_output(ldapsearch_lines, attr_mail)
        }
        if (is.null(attrs$phone)) {
          attrs$phone <- parse_ldapsearch_output(ldapsearch_lines, attr_phone)
        }
        if (is.null(attrs$ou)) {
          attrs$ou <- parse_ldapsearch_output(ldapsearch_lines, attr_ou)
        }
        if (is.null(attrs$full_name)) {
          attrs$full_name <- parse_ldapsearch_output(ldapsearch_lines, attr_cn)
          if (is.null(attrs$full_name) || attrs$full_name == "") {
            given <- parse_ldapsearch_output(ldapsearch_lines, attr_given)
            sn <- parse_ldapsearch_output(ldapsearch_lines, attr_sn)
            if (!is.null(given) && given != "" && !is.null(sn) && sn != "") {
              attrs$full_name <- paste(given, sn)
            } else if (!is.null(given) && given != "") {
              attrs$full_name <- given
            } else if (!is.null(sn) && sn != "") {
              attrs$full_name <- sn
            }
          }
        }
        if (dev_mode && Sys.getenv("LDAP_DEBUG", "") == "1") {
          cat("LDAP ATTRS PARSED: ", "email=", attrs$email %||% "NULL",
              " phone=", attrs$phone %||% "NULL",
              " ou=", attrs$ou %||% "NULL",
              " full_name=", attrs$full_name %||% "NULL", "\n")
        }
      }

      return(attrs)
    }

    attrs
  }

  normalize_group_key <- function(value) {
    if (is.null(value)) return("")
    tolower(trimws(value))
  }

  extract_group_key_from_ou <- function(ou_value) {
    if (is.null(ou_value) || ou_value == "") return("")
    # Prefer text inside parentheses, e.g. "(Cox)"
    m <- regexec("\\(([^\\)]+)\\)", ou_value)
    matches <- regmatches(ou_value, m)
    if (length(matches) > 0 && length(matches[[1]]) > 1) {
      return(trimws(matches[[1]][2]))
    }
    ou_value
  }

  map_group_from_ldap_ou <- function(ou_value, con) {
    if (is.null(ou_value) || ou_value == "") return(NULL)

    group_key <- extract_group_key_from_ou(ou_value)
    if (group_key == "") return(NULL)

    holders <- tryCatch({
      dbGetQuery(con, "SELECT id, name, surname, cost_center FROM budget_holders")
    }, error = function(e) NULL)

    if (is.null(holders) || nrow(holders) == 0) return(NULL)

    full_names <- paste(holders$name, holders$surname)

    key_norm <- normalize_group_key(group_key)
    name_norm <- normalize_group_key(holders$name)
    surname_norm <- normalize_group_key(holders$surname)
    full_norm <- normalize_group_key(full_names)

    idx <- which(full_norm == key_norm)
    if (length(idx) == 0) idx <- which(name_norm == key_norm)
    if (length(idx) == 0) idx <- which(surname_norm == key_norm)
    if (length(idx) == 0) idx <- which(grepl(key_norm, full_norm, fixed = TRUE))

    if (length(idx) == 0) return(NULL)
    full_names[idx[1]]
  }

  budget_holder_notice_ui <- function(selected_id) {
    if (is.null(selected_id) || selected_id == "" || is.null(admin_data$budget_holders)) return(NULL)
    if (selected_id == "other") return(NULL)

    holders <- admin_data$budget_holders
    if (nrow(holders) == 0) return(NULL)

    sel_id <- suppressWarnings(as.numeric(selected_id))
    sel_row <- holders[holders$id == sel_id, ]
    if (nrow(sel_row) == 0) return(NULL)

    group_key <- paste(sel_row$name[1], sel_row$surname[1])
    group_rows <- holders[paste(holders$name, holders$surname) == group_key, ]

    cc_values <- unique(trimws(group_rows$cost_center))
    cc_values <- cc_values[!is.na(cc_values) & cc_values != ""]
    has_multiple <- length(cc_values) > 1

    sel_cc <- trimws(sel_row$cost_center[1])
    sel_cc_norm <- toupper(gsub("[^A-Z0-9]", "", sel_cc))
    missing_cc <- is.na(sel_cc) || sel_cc == "" || sel_cc_norm == "NA"

    notices <- list()
    if (has_multiple) {
      notices <- c(notices, list(
        div(class = "alert alert-info",
            "Note: This group has multiple cost centers. Please verify the correct selection.")
      ))
    }
    if (missing_cc) {
      notices <- c(notices, list(
        div(class = "alert alert-warning",
            "No cost center is set for this group. Please notify the core facility.")
      ))
    }

    if (length(notices) == 0) return(NULL)
    tagList(notices)
  }

  budget_holder_other_fields_ui <- function(prefix = "") {
    name_id <- paste0(prefix, "other_bh_name")
    surname_id <- paste0(prefix, "other_bh_surname")
    cost_id <- paste0(prefix, "other_bh_cost_center")
    email_id <- paste0(prefix, "other_bh_email")

    tagList(
      h4("New Budget Holder"),
      textInput(name_id, "PI Name *", placeholder = "Enter PI first name"),
      textInput(surname_id, "PI Surname *", placeholder = "Enter PI surname"),
      textInput(cost_id, "Cost Center (optional)", placeholder = "Enter cost center if known"),
      textInput(email_id, "PI Email (optional)", placeholder = "Enter PI email if known"),
      tags$small("* Required fields")
    )
  }

  budget_holder_other_notice_ui <- function(cost_center_value) {
    cc <- trimws(cost_center_value %||% "")
    cc_norm <- toupper(gsub("[^A-Z0-9]", "", cc))
    if (cc == "" || cc_norm == "NA") {
      return(div(class = "alert alert-warning",
                 "No cost center is set for this group. Please notify the core facility."))
    }
    NULL
  }

  complete_login <- function(user_data) {
    user$logged_in <- TRUE
    user$username <- user_data$username
    full_name_val <- scalar_text(user_data$full_name)
    if (is.null(full_name_val) || full_name_val == "") {
      full_name_val <- scalar_text(user_data$username)
    }
    user$full_name <- full_name_val
    user$user_id <- user_data$id
    user$is_admin <- as.logical(user_data$is_admin)

    admin_data$budget_holders <- load_budget_holders()
    admin_data$service_types <- load_service_types()
    admin_data$sequencing_depths <- load_sequencing_depths()
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    admin_data$types <- load_types()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    admin_data$reference_genomes <- load_reference_genomes()

    shinyjs::hide("login_screen", anim = FALSE)
    shinyjs::show("main_app", anim = FALSE)

    if (user$is_admin) {
      load_admin_data()
    }
  }

  show_profile_modal <- function(username, attrs, is_new) {
    pending_profile$active <- TRUE
    pending_profile$username <- username
    pending_profile$attrs <- attrs
    pending_profile$is_new <- is_new

    showModal(modalDialog(
      title = "Complete Your Profile",
      size = "m",
      footer = tagList(
        actionButton("ldap_profile_cancel_btn", "Cancel", class = "btn-secondary"),
        actionButton("ldap_profile_save_btn", "Save", class = "btn-primary")
      ),
      textInput("ldap_profile_email", "Email *", value = attrs$email %||% ""),
      textInput("ldap_profile_phone", "Phone", value = attrs$phone %||% ""),
      textInput("ldap_profile_group", "Research Group *", value = attrs$ou %||% ""),
      tags$small("* Required fields")
    ))
  }

  maybe_prompt_profile_update <- function(user_row, attrs) {
    if (pending_profile$active) return()
    missing_email <- is.null(user_row$email) || is.na(user_row$email) || user_row$email == ""
    missing_group <- is.null(user_row$research_group) || is.na(user_row$research_group) || user_row$research_group == ""

    if (missing_email || missing_group) {
      show_profile_modal(user_row$username, list(
        email = user_row$email %||% attrs$email,
        phone = user_row$phone %||% attrs$phone,
        ou = user_row$research_group %||% attrs$ou
      ), is_new = FALSE)
    }
  }

  handle_ldap_login <- function(username) {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    user_data <- dbGetQuery(con,
                            "SELECT * FROM users WHERE username = ?",
                            params = list(username))
    attrs <- ldap_lookup_user(username)
    attrs$email <- scalar_text(attrs$email)
    attrs$phone <- scalar_text(attrs$phone)
    attrs$ou <- scalar_text(attrs$ou)
    attrs$full_name <- scalar_text(attrs$full_name)

    if (Sys.getenv("LDAP_DEBUG", "") == "1") {
      cat("LDAP LOGIN ATTRS:",
          "user=", username,
          "email=", attrs$email %||% "<missing>",
          "phone=", attrs$phone %||% "<missing>",
          "ou=", attrs$ou %||% "<missing>",
          "full_name=", attrs$full_name %||% "<missing>",
          "\n", file = stderr())
      flush.console()
    }

    mapped_group <- map_group_from_ldap_ou(attrs$ou, con)
    if (!is.null(mapped_group) && mapped_group != "") {
      attrs$ou <- mapped_group
    }

    if (dev_mode && isFALSE(attrs$bind_ok) && !ldap_bind_warned() &&
        is.null(attrs$email) && is.null(attrs$phone) && is.null(attrs$ou)) {
      showNotification(
        "LDAP bind failed. Attribute lookup is disabled until LDAP_BIND_DN/PW are set.",
        type = "warning",
        duration = 10
      )
      ldap_bind_warned(TRUE)
    }

    if (nrow(user_data) == 0) {
      if (is.null(attrs$email) || attrs$email == "") {
        show_profile_modal(username, attrs, is_new = TRUE)
        return()
      }

      placeholder_pw <- digest::digest(paste0("ldap-", username))
      if (users_has_full_name(con)) {
        dbExecute(con, "
          INSERT INTO users (username, full_name, password, email, phone, research_group, is_admin)
          VALUES (?, ?, ?, ?, ?, ?, 0)
        ", params = list(
          username,
          attrs$full_name %||% username,
          placeholder_pw,
          attrs$email,
          attrs$phone %||% "",
          attrs$ou %||% ""
        ))
      } else {
        dbExecute(con, "
          INSERT INTO users (username, password, email, phone, research_group, is_admin)
          VALUES (?, ?, ?, ?, ?, 0)
        ", params = list(
          username,
          placeholder_pw,
          attrs$email,
          attrs$phone %||% "",
          attrs$ou %||% ""
        ))
      }

      user_data <- dbGetQuery(con,
                              "SELECT * FROM users WHERE username = ?",
                              params = list(username))
    } else {
      # Normalize existing group value if it doesn't match budget holder names
      if (!is.null(user_data$research_group) && user_data$research_group[1] != "") {
        normalized_group <- map_group_from_ldap_ou(user_data$research_group[1], con)
        if (!is.null(normalized_group) && normalized_group != "" &&
            normalized_group != user_data$research_group[1]) {
          dbExecute(con, "
            UPDATE users SET research_group = ?
            WHERE username = ?
          ", params = list(normalized_group, username))
          user_data <- dbGetQuery(con,
                                  "SELECT * FROM users WHERE username = ?",
                                  params = list(username))
        }
      }

      updates <- list()
      if ((is.null(user_data$email) || is.na(user_data$email) || user_data$email == "") && !is.null(attrs$email) && attrs$email != "") {
        updates$email <- attrs$email
      }
      if ((is.null(user_data$phone) || is.na(user_data$phone) || user_data$phone == "") && !is.null(attrs$phone) && attrs$phone != "") {
        updates$phone <- attrs$phone
      }
      if ((is.null(user_data$research_group) || is.na(user_data$research_group) || user_data$research_group == "") && !is.null(attrs$ou) && attrs$ou != "") {
        updates$research_group <- attrs$ou
      }
      if (users_has_full_name(con)) {
        if ((is.null(user_data$full_name) || is.na(user_data$full_name) || user_data$full_name == "") && !is.null(attrs$full_name) && attrs$full_name != "") {
          updates$full_name <- attrs$full_name
        }
      }

      if (length(updates) > 0) {
        if (users_has_full_name(con)) {
          dbExecute(con, "
            UPDATE users SET email = ?, phone = ?, research_group = ?, full_name = ?
            WHERE username = ?
          ", params = list(
            updates$email %||% user_data$email,
            updates$phone %||% user_data$phone,
            updates$research_group %||% user_data$research_group,
            updates$full_name %||% user_data$full_name,
            username
          ))
        } else {
          dbExecute(con, "
            UPDATE users SET email = ?, phone = ?, research_group = ?
            WHERE username = ?
          ", params = list(
            updates$email %||% user_data$email,
            updates$phone %||% user_data$phone,
            updates$research_group %||% user_data$research_group,
            username
          ))
        }

        user_data <- dbGetQuery(con,
                                "SELECT * FROM users WHERE username = ?",
                                params = list(username))
      }
    }

    if (nrow(user_data) == 1) {
      complete_login(user_data)
      maybe_prompt_profile_update(user_data[1, ], attrs)
    }
  }

  ##################################################################
  # END AUTH / LDAP HELPERS
  ##################################################################

  # Define status options
  status_options <- c(
    "Created",
    "Samples received", 
    "Library preparation",
    "QC done",
    "Data analysis", 
    "Data released",
    "Legacy project"
  )

  status_colors <- c(
    "Created" = "#fff3cd",
    "Samples received" = "#cce7ff",
    "Library preparation" = "#e2e3e5",
    "QC done" = "#d1ecf1",
    "Data analysis" = "#f8d7da",
    "Data released" = "#d4edda",
    "Legacy project" = "#e9ecef"
  )

  make_status_choices <- function(current_status = NULL) {
    choices <- status_options
    if (!is.null(current_status) && nzchar(current_status) && !(current_status %in% choices)) {
      choices <- c(choices, setNames(current_status, paste0(current_status, " (legacy)")))
    }
    choices
  }

  projects_has_additional_cost <- function(con) {
    tryCatch({
      "additional_cost" %in% dbGetQuery(con, "PRAGMA table_info(projects)")$name
    }, error = function(e) FALSE)
  }

  ensure_projects_additional_cost_column <- function(con) {
    if (!projects_has_additional_cost(con)) {
      dbExecute(con, "ALTER TABLE projects ADD COLUMN additional_cost REAL")
    }
    invisible(NULL)
  }

  format_cost_amount <- function(value) {
    numeric_value <- suppressWarnings(as.numeric(value))
    if (length(numeric_value) == 0 || is.na(numeric_value)) return("")
    sprintf("%.2f", numeric_value)
  }

  parse_additional_cost <- function(value) {
    raw_value <- ""
    if (!is.null(value) && length(value) > 0) {
      raw_value <- as.character(value[[1]])
    }
    raw_value <- trimws(raw_value)

    if (raw_value == "") {
      return(list(valid = TRUE, empty = TRUE, value = NA_real_, error = NULL))
    }

    normalized <- gsub(",", ".", raw_value, fixed = TRUE)
    normalized <- gsub("\\s+", "", normalized)

    if (!grepl("^([0-9]+(\\.[0-9]+)?|\\.[0-9]+)$", normalized)) {
      return(list(
        valid = FALSE,
        empty = FALSE,
        value = NA_real_,
        error = "Additional costs must be a non-negative number (e.g., 125 or 125.50)."
      ))
    }

    numeric_value <- suppressWarnings(as.numeric(normalized))
    if (is.na(numeric_value) || numeric_value < 0) {
      return(list(
        valid = FALSE,
        empty = FALSE,
        value = NA_real_,
        error = "Additional costs must be a non-negative number (e.g., 125 or 125.50)."
      ))
    }

    list(valid = TRUE, empty = FALSE, value = round(numeric_value, 2), error = NULL)
  }
  
  users_has_full_name <- function(con) {
    tryCatch({
      "full_name" %in% dbGetQuery(con, "PRAGMA table_info(users)")$name
    }, error = function(e) FALSE)
  }

  ensure_users_full_name_column <- function(con) {
    if (!users_has_full_name(con)) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN full_name TEXT")
    }
    dbExecute(con, "
      UPDATE users
      SET full_name = username
      WHERE full_name IS NULL OR TRIM(full_name) = ''
    ")
    invisible(NULL)
  }

  normalize_key <- function(x) {
    if (is.null(x) || length(x) == 0) return(character(0))
    values <- as.character(x)
    values[is.na(values)] <- ""
    tolower(trimws(values))
  }

  to_scalar_text <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    values <- as.character(x)
    if (length(values) == 0) return(default)
    value <- values[[1]]
    if (is.na(value)) return(default)
    trimws(value)
  }

  format_responsible_label <- function(full_name, username) {
    username_clean <- to_scalar_text(username, "")
    if (!nzchar(username_clean)) return("")

    full_name_clean <- to_scalar_text(full_name, "")
    if (!nzchar(full_name_clean) || normalize_key(full_name_clean) == normalize_key(username_clean)) {
      return(username_clean)
    }

    paste0(full_name_clean, " (", username_clean, ")")
  }

  extract_username_from_label <- function(value) {
    label <- to_scalar_text(value, "")
    if (!nzchar(label)) return(NULL)

    match <- regexec("\\(([^()]+)\\)\\s*$", label, perl = TRUE)
    parsed <- regmatches(label, match)[[1]]
    if (length(parsed) < 2) return(NULL)

    username <- trimws(parsed[2])
    if (!nzchar(username)) return(NULL)
    username
  }

  get_responsible_users <- function(con) {
    ensure_users_full_name_column(con)
    users <- dbGetQuery(con, "SELECT username, full_name FROM users ORDER BY username")
    if (nrow(users) == 0) {
      return(data.frame(username = character(0), full_name = character(0), stringsAsFactors = FALSE))
    }

    users$username <- trimws(as.character(users$username))
    users$full_name <- trimws(as.character(users$full_name))
    users$full_name[is.na(users$full_name)] <- ""
    users <- users[!is.na(users$username) & users$username != "", , drop = FALSE]
    if (nrow(users) == 0) {
      return(data.frame(username = character(0), full_name = character(0), stringsAsFactors = FALSE))
    }

    users <- users[order(tolower(users$username), users$username), , drop = FALSE]
    users <- users[!duplicated(tolower(users$username)), , drop = FALSE]
    users
  }

  resolve_responsible_username <- function(value, users_df) {
    raw_value <- to_scalar_text(value, "")
    if (!nzchar(raw_value) || nrow(users_df) == 0 || !all(c("username", "full_name") %in% names(users_df))) {
      return(NA_character_)
    }

    username_norm <- normalize_key(users_df$username)
    target_norm <- normalize_key(raw_value)

    idx_username <- which(username_norm == target_norm)
    if (length(idx_username) > 0) return(users_df$username[idx_username[1]])

    parsed_username <- extract_username_from_label(raw_value)
    if (!is.null(parsed_username)) {
      idx_parsed <- which(username_norm == normalize_key(parsed_username))
      if (length(idx_parsed) > 0) return(users_df$username[idx_parsed[1]])
    }

    full_name_norm <- normalize_key(users_df$full_name)
    idx_full_name <- which(full_name_norm == target_norm & users_df$full_name != "")
    if (length(idx_full_name) == 1) return(users_df$username[idx_full_name[1]])

    NA_character_
  }

  resolve_responsible_for_storage <- function(value, con, fallback = "") {
    raw_value <- to_scalar_text(value, "")
    fallback_value <- to_scalar_text(fallback, "")
    if (!nzchar(raw_value)) return(fallback_value)

    users <- get_responsible_users(con)
    resolved <- resolve_responsible_username(raw_value, users)
    if (!is.na(resolved) && nzchar(resolved)) return(resolved)
    raw_value
  }

  is_responsible_for_user <- function(stored_value, username, full_name = NULL) {
    stored <- to_scalar_text(stored_value, "")
    if (!nzchar(stored)) return(FALSE)

    username_clean <- to_scalar_text(username, "")
    full_name_clean <- to_scalar_text(full_name, "")

    if (nzchar(username_clean) && normalize_key(stored) == normalize_key(username_clean)) return(TRUE)
    if (nzchar(full_name_clean) && normalize_key(stored) == normalize_key(full_name_clean)) return(TRUE)

    label_value <- format_responsible_label(full_name_clean, username_clean)
    if (nzchar(label_value) && normalize_key(stored) == normalize_key(label_value)) return(TRUE)

    parsed_username <- extract_username_from_label(stored)
    if (!is.null(parsed_username) && nzchar(username_clean) &&
        normalize_key(parsed_username) == normalize_key(username_clean)) {
      return(TRUE)
    }

    FALSE
  }

  # Database connection
  get_db_connection <- function() {
    con <- dbConnect(RSQLite::SQLite(), "sequencing_projects.db")
    tryCatch({
      ensure_projects_additional_cost_column(con)
      ensure_users_full_name_column(con)
    }, error = function(e) {
      cat("DB MIGRATION ERROR:", e$message, "\n", file = stderr())
      flush.console()
    })
    con
  }

  format_responsible_display <- function(value, con = NULL) {
    close_con <- FALSE
    if (is.null(con)) {
      con <- get_db_connection()
      close_con <- TRUE
    }
    on.exit(if (close_con) dbDisconnect(con), add = TRUE)

    raw_value <- to_scalar_text(value, "")
    if (!nzchar(raw_value)) return("")

    users <- get_responsible_users(con)
    resolved <- resolve_responsible_username(raw_value, users)
    if (!is.na(resolved) && nzchar(resolved)) {
      idx <- which(normalize_key(users$username) == normalize_key(resolved))
      if (length(idx) > 0) {
        row <- users[idx[1], , drop = FALSE]
        return(format_responsible_label(row$full_name[[1]], row$username[[1]]))
      }
    }

    raw_value
  }

  normalize_legacy_responsible_users <- function(con) {
    users <- get_responsible_users(con)
    result <- list(updated_rows = 0L, normalized_values = 0L, unresolved_values = character(0))
    if (nrow(users) == 0) return(result)

    values <- dbGetQuery(
      con,
      "
      SELECT DISTINCT TRIM(responsible_user) AS responsible_user
      FROM projects
      WHERE responsible_user IS NOT NULL AND TRIM(responsible_user) <> ''
      "
    )
    if (nrow(values) == 0) return(result)

    unresolved <- character(0)
    normalized_values <- 0L
    updated_rows <- 0L

    for (value_raw in values$responsible_user) {
      value <- trimws(as.character(value_raw %||% ""))
      if (!nzchar(value)) next

      resolved <- resolve_responsible_username(value, users)
      if (is.na(resolved) || !nzchar(resolved)) {
        unresolved <- c(unresolved, value)
        next
      }

      if (identical(value, resolved)) next

      affected <- dbExecute(
        con,
        "
        UPDATE projects
        SET responsible_user = ?, updated_at = CURRENT_TIMESTAMP
        WHERE responsible_user = ? OR TRIM(responsible_user) = ?
        ",
        params = list(resolved, value, value)
      )
      if (!is.null(affected) && affected > 0) {
        updated_rows <- updated_rows + as.integer(affected)
        normalized_values <- normalized_values + 1L
      }
    }

    unresolved <- sort(unique(unresolved))
    cat(
      "RESPONSIBLE USER MIGRATION: normalized_values=", normalized_values,
      " updated_rows=", updated_rows,
      " unresolved_values=", length(unresolved), "\n",
      sep = ""
    )
    if (length(unresolved) > 0) {
      cat(
        "RESPONSIBLE USER MIGRATION unresolved entries:",
        paste(unresolved, collapse = " | "),
        "\n"
      )
    }

    result$updated_rows <- updated_rows
    result$normalized_values <- normalized_values
    result$unresolved_values <- unresolved
    result
  }

  refresh_announcements <- function() {
    announcement_refresh(announcement_refresh() + 1)
  }

  get_announcement_panels <- function(con = NULL, active_only = FALSE) {
    close_con <- FALSE
    if (is.null(con)) {
      con <- get_db_connection()
      close_con <- TRUE
    }
    on.exit(if (close_con) dbDisconnect(con), add = TRUE)

    ensure_announcement_tables(con)
    seed_announcement_defaults(con)

    query <- "
      SELECT id, panel_key, title, subtitle, display_order, is_active
      FROM announcement_panels
    "
    if (isTRUE(active_only)) {
      query <- paste(query, "WHERE is_active = 1")
    }
    query <- paste(query, "ORDER BY display_order, id")
    dbGetQuery(con, query)
  }

  get_announcement_items_for_panel <- function(panel_key, con = NULL, active_only = FALSE) {
    close_con <- FALSE
    if (is.null(con)) {
      con <- get_db_connection()
      close_con <- TRUE
    }
    on.exit(if (close_con) dbDisconnect(con), add = TRUE)

    ensure_announcement_tables(con)
    seed_announcement_defaults(con)

    panel <- dbGetQuery(
      con,
      "SELECT id FROM announcement_panels WHERE panel_key = ?",
      params = list(panel_key)
    )
    if (nrow(panel) == 0) return(data.frame())

    query <- "
      SELECT id, panel_id, display_order, markdown_text, is_active
      FROM announcement_items
      WHERE panel_id = ?
    "
    if (isTRUE(active_only)) {
      query <- paste(query, "AND is_active = 1")
    }
    query <- paste(query, "ORDER BY display_order, id")
    dbGetQuery(con, query, params = list(panel$id[1]))
  }

  normalize_announcement_item_order <- function(con, panel_id) {
    rows <- dbGetQuery(
      con,
      "SELECT id FROM announcement_items WHERE panel_id = ? ORDER BY display_order, id",
      params = list(panel_id)
    )
    if (nrow(rows) == 0) return(invisible(NULL))

    for (idx in seq_len(nrow(rows))) {
      dbExecute(
        con,
        "UPDATE announcement_items SET display_order = ?, updated_at = CURRENT_TIMESTAMP WHERE id = ?",
        params = list(as.integer(idx), rows$id[idx])
      )
    }
    invisible(NULL)
  }

  validate_announcement_markdown <- function(markdown_text) {
    markdown_text <- as.character(markdown_text %||% "")
    if (trimws(markdown_text) == "") {
      return("Content cannot be empty.")
    }
    if (nchar(markdown_text) > max_announcement_chars) {
      return(paste0("Content is too long (max ", max_announcement_chars, " characters)."))
    }
    NULL
  }

  selected_announcement_item <- function() {
    selected_row <- input$announcement_items_table_admin_rows_selected
    items <- announcement_items_current()
    if (length(selected_row) == 0 || nrow(items) == 0) return(NULL)
    idx <- selected_row[1]
    if (idx < 1 || idx > nrow(items)) return(NULL)
    items[idx, , drop = FALSE]
  }

  tryCatch({
    con_init <- get_db_connection()
    on.exit(dbDisconnect(con_init), add = TRUE)
    ensure_announcement_tables(con_init)
    seed_announcement_defaults(con_init)
  }, error = function(e) {
    cat("ANNOUNCEMENT INIT ERROR:", e$message, "\n", file = stderr())
    flush.console()
  })
  
  get_responsible_user_choices <- function(selected_legacy = NULL) {
    con <- get_db_connection()
    on.exit(dbDisconnect(con), add = TRUE)

    users <- get_responsible_users(con)
    if (nrow(users) > 0) {
      users$display_label <- mapply(
        format_responsible_label,
        users$full_name,
        users$username,
        USE.NAMES = FALSE
      )
      users <- users[
        order(tolower(users$display_label), tolower(users$username), users$username),
        ,
        drop = FALSE
      ]
      users <- users[!duplicated(tolower(users$username)), , drop = FALSE]
      choices <- setNames(users$username, users$display_label)
    } else {
      choices <- character(0)
    }

    legacy_value <- to_scalar_text(selected_legacy, "")
    if (nzchar(legacy_value)) {
      resolved <- resolve_responsible_username(legacy_value, users)
      if (is.na(resolved) || !nzchar(resolved)) {
        if (!(legacy_value %in% unname(choices))) {
          choices <- c(choices, setNames(legacy_value, paste0("Legacy entry: ", legacy_value)))
        }
      }
    }

    choices
  }
  
  # Load budget holders from database
  load_budget_holders <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    holders <- dbGetQuery(con, "SELECT id, name, surname, cost_center, email FROM budget_holders ORDER BY name, surname")
    return(holders)
  }
  
  # Load sample service types from database
  load_service_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    service_types <- dbGetQuery(con, "SELECT id, service_type, kit, costs_per_sample FROM service_types ORDER BY service_type")
    return(service_types)
  }
  
  # Load sequencing depths from database
  load_sequencing_depths <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    depths <- dbGetQuery(con, "SELECT id, depth_description, cost_upto_150_cycles, cost_upto_300_cycles FROM sequencing_depths ORDER BY depth_description")
    return(depths)
  }
  
  # Load sequencing cycles from database
  load_sequencing_cycles <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    cycles <- dbGetQuery(con, "SELECT id, cycles_description FROM sequencing_cycles ORDER BY cycles_description")
    return(cycles)
  }
  
  # Load reference genomes from database
  load_reference_genomes <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    genomes <- dbGetQuery(con, "SELECT name FROM reference_genomes ORDER BY name")
    return(genomes$name)
  }
  
  # Load types from database
  load_types <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    types <- dbGetQuery(con, "SELECT id, name FROM types ORDER BY name")
    return(types)
  }
  
  # Load sequencing platforms from database
  load_sequencing_platforms <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    platforms <- dbGetQuery(con, "SELECT id, name FROM sequencing_platforms ORDER BY name")
    return(platforms)
  }
  
  # Load admin data
  load_admin_data <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Load users
    if (users_has_full_name(con)) {
      admin_data$users <- dbGetQuery(con, "SELECT id, username, full_name, email, phone, research_group, is_admin FROM users")
    } else {
      admin_data$users <- dbGetQuery(con, "SELECT id, username, email, phone, research_group, is_admin FROM users")
    }
    
    # Load all reference data from database
    admin_data$budget_holders <- load_budget_holders()
    admin_data$reference_genomes <- load_reference_genomes()
    admin_data$types <- load_types()
    admin_data$service_types <- load_service_types()
    admin_data$sequencing_depths <- load_sequencing_depths()
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
  }
  
  # Calculate base sequencing cost (without admin-entered additional costs)
  calculate_base_cost <- function(num_samples, service_type_id, sequencing_depth_id, sequencing_cycles_id) {
    if(is.null(num_samples) || is.null(service_type_id) || is.null(sequencing_depth_id) || is.null(sequencing_cycles_id)) {
      return(0)
    }
    
    service_type <- admin_data$service_types[admin_data$service_types$id == as.numeric(service_type_id), ]
    sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == as.numeric(sequencing_depth_id), ]
    
    if(nrow(service_type) == 0 || nrow(sequencing_depth) == 0) {
      return(0)
    }
    
    prep_cost <- service_type$costs_per_sample * as.numeric(num_samples)
    
    # Check if "other" is selected for sequencing depth
    if(sequencing_depth$depth_description == "other") {
      return(prep_cost)
    }
    
    # Determine which cost column to use based on sequencing cycles
    if(sequencing_cycles_id == "1") {
      seq_cost <- sequencing_depth$cost_upto_150_cycles
    } else {
      seq_cost <- sequencing_depth$cost_upto_300_cycles
    }
    
    prep_cost + ifelse(is.na(seq_cost), 0, seq_cost)
  }

  # Calculate total cost (base + optional additional costs)
  calculate_total_cost <- function(num_samples, service_type_id, sequencing_depth_id, sequencing_cycles_id, additional_cost = 0) {
    base_cost <- calculate_base_cost(num_samples, service_type_id, sequencing_depth_id, sequencing_cycles_id)
    add_cost <- suppressWarnings(as.numeric(additional_cost))
    if (length(add_cost) == 0 || is.na(add_cost) || add_cost < 0) {
      add_cost <- 0
    }
    base_cost + add_cost
  }
  
  # Send email notification
  send_project_creation_email <- function(project_data, budget_holder, user_email) {
    tryCatch({
      # Create ONE connection for the entire function
      con <- get_db_connection()
      on.exit(dbDisconnect(con))
      
      cat("DEBUG: Starting email function\n")
      cat("DEBUG: From: ngs@biochem.mpg.de\n")
      
      # Get email template (using the same connection)
      template <- dbGetQuery(con, "SELECT subject, body_template FROM email_templates WHERE template_name = 'project_creation' AND is_active = 1")
      
      if(nrow(template) == 0) {
        cat("DEBUG: Template not found\n")
        return(list(success = FALSE, error = "Email template not found"))
      }
      
      cat("DEBUG: Template found, preparing content\n")
      
      # Fixed notification address for the facility
      facility_email <- "ngs@biochem.mpg.de"

      # NOTE: Admin notifications are currently disabled to avoid emailing all admins
      # for every new project. If you want to re-enable, uncomment the lines below
      # and include `admin_emails` in the `to =` list.
      # admin_emails <- dbGetQuery(con, "SELECT email FROM users WHERE is_admin = 1")$email
      
      cat("DEBUG: To:", facility_email, "\n")
      
      # Prepare cost warning
      sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == project_data$sequencing_depth_id, ]
      cost_warning <- ""
      if(sequencing_depth$depth_description == "other") {
        cost_warning <- "NOTE: The costs shown are preliminary. Please contact us to discuss your specific sequencing needs as the 'other' option was selected for sequencing depth."
      }
      
      # Replace placeholders in template
      responsible_user_display <- format_responsible_display(project_data$responsible_user, con = con)
      if (!nzchar(responsible_user_display)) {
        responsible_user_display <- trimws(as.character(project_data$responsible_user %||% ""))
      }

      email_body <- template$body_template
      email_body <- gsub("\\{name\\}", budget_holder$name, email_body)
      email_body <- gsub("\\{surname\\}", budget_holder$surname, email_body)
      email_body <- gsub("\\{cost_center\\}", budget_holder$cost_center, email_body)
      email_body <- gsub("\\{project_name\\}", project_data$project_name, email_body)
      email_body <- gsub("\\{responsible_user\\}", responsible_user_display, email_body)
      email_body <- gsub("\\{num_samples\\}", project_data$num_samples, email_body)
      email_body <- gsub("\\{service_type\\}", admin_data$service_types$service_type[admin_data$service_types$id == project_data$service_type_id], email_body)
      email_body <- gsub(
        "(?m)^-\\s*Service\\s*Type:",
        "- Sample Service Type:",
        email_body,
        perl = TRUE
      )
      email_body <- gsub(
        "\\{total_cost\\}",
        "Total Project Costs will be provided soon after project assessment by the NGS core facility.",
        email_body
      )
      email_body <- gsub(
        "(?m)^-\\s*Total\\s*Estimated\\s*Cost:.*$",
        "- Total Project Costs will be provided soon after project assessment by the NGS core facility.",
        email_body,
        perl = TRUE
      )
      email_body <- gsub("\\{cost_warning\\}", cost_warning, email_body)

      # Append project description if provided
      project_description <- trimws(project_data$description %||% "")
      if (nzchar(project_description)) {
        email_body <- paste(
          email_body,
          "",
          "Project Description:",
          project_description,
          sep = "\n"
        )
      }
      
      cat("DEBUG: Attempting to send email...\n")
      
      # Build subject with project ID prefix (e.g., "P2 - Subject")
      subject_base <- template$subject
      subject <- subject_base
      if (!is.null(project_data$project_code) &&
          !is.na(project_data$project_code) &&
          nzchar(project_data$project_code)) {
        subject <- paste(project_data$project_code, subject_base, sep = " - ")
      }

      # Send email
      send.mail(
        from = "ngs@biochem.mpg.de",
        # NOTE: Admin notifications are currently disabled; to re-enable,
        # include `admin_emails` below and uncomment the query above.
        to = c(facility_email),
        encoding = "utf-8",
        subject = subject,
        body = email_body,
        smtp = list(
          host.name = "msx.biochem.mpg.de",
          port = 25,
          ssl = FALSE,
          tls = FALSE,
          authenticate = FALSE
        ),
        send = TRUE
      )
      
      cat("DEBUG: Email sent successfully\n")
      return(list(success = TRUE, message = "Email sent successfully"))
      
    }, error = function(e) {
      cat("DEBUG: Error occurred:", e$message, "\n")
      return(list(success = FALSE, error = e$message))
    })
  }

  # Send admin-triggered cost-estimation email notification
  send_project_cost_notification_email <- function(project_data, budget_holder, responsible_email) {
    tryCatch({
      con <- get_db_connection()
      on.exit(dbDisconnect(con), add = TRUE)

      facility_email <- "ngs@biochem.mpg.de"

      project_code <- trimws(project_data$project_code %||% "")
      if (!nzchar(project_code) && !is.null(project_data$project_id) && !is.na(project_data$project_id)) {
        project_code <- paste0("P", as.character(project_data$project_id))
      }
      if (!nzchar(project_code)) project_code <- "Project"

      safe_total_cost <- suppressWarnings(as.numeric(project_data$total_cost))
      if (length(safe_total_cost) == 0 || is.na(safe_total_cost)) safe_total_cost <- 0

      budget_name <- trimws(paste(budget_holder$surname[[1]] %||% "", budget_holder$name[[1]] %||% ""))
      if (!nzchar(budget_name)) budget_name <- "Budget holder"
      budget_center <- trimws(as.character(budget_holder$cost_center[[1]] %||% "N.A."))

      service_type <- trimws(as.character(project_data$service_type %||% ""))
      if (!nzchar(service_type)) service_type <- "N.A."
      sequencing_platform <- trimws(as.character(project_data$sequencing_platform %||% ""))
      if (!nzchar(sequencing_platform)) sequencing_platform <- "N.A."

      recipients <- unique(c(
        trimws(as.character(budget_holder$email[[1]] %||% "")),
        trimws(as.character(responsible_email %||% "")),
        facility_email
      ))
      recipients <- recipients[!is.na(recipients) & recipients != ""]
      if (length(recipients) == 0) {
        return(list(success = FALSE, error = "No valid recipients found for notification email."))
      }

      project_description <- trimws(as.character(project_data$description %||% ""))
      description_html <- ""
      if (nzchar(project_description)) {
        description_html <- paste0(
          "<p><strong>Project Description:</strong><br>",
          htmltools::htmlEscape(project_description),
          "</p>"
        )
      }

      cost_line <- paste0(
        "<strong><u>Total Estimated Cost: ",
        sprintf("%.2f", safe_total_cost),
        "€</u></strong>"
      )
      responsible_user_display <- format_responsible_display(project_data$responsible_user, con = con)
      if (!nzchar(responsible_user_display)) {
        responsible_user_display <- trimws(as.character(project_data$responsible_user %||% ""))
      }

      email_body <- paste0(
        "<html><body style='font-family: Arial, sans-serif;'>",
        "<p>Dear ", htmltools::htmlEscape(budget_name), ",</p>",
        "<p>A new sequencing project has been created under your cost center ",
        htmltools::htmlEscape(budget_center), ".</p>",
        "<p><strong>Project Details:</strong><br>",
        "- Project Name: ", htmltools::htmlEscape(as.character(project_data$project_name %||% "")), "<br>",
        "- Responsible User: ", htmltools::htmlEscape(responsible_user_display), "<br>",
        "- Number of Samples: ", htmltools::htmlEscape(as.character(project_data$num_samples %||% "")), "<br>",
        "<span style='color:#c00000;'>- Sample Service Type: ", htmltools::htmlEscape(service_type), "</span><br>",
        "<span style='color:#c00000;'>- Sequencing Platform: ", htmltools::htmlEscape(sequencing_platform), "</span><br>",
        "- ", cost_line,
        "</p>",
        description_html,
        "<p>Best regards,<br>Sequencing Facility Team</p>",
        "</body></html>"
      )

      send.mail(
        from = "ngs@biochem.mpg.de",
        to = recipients,
        encoding = "utf-8",
        subject = paste0(project_code, " - New Sequencing Project Created - cost estimation"),
        body = email_body,
        html = TRUE,
        smtp = list(
          host.name = "msx.biochem.mpg.de",
          port = 25,
          ssl = FALSE,
          tls = FALSE,
          authenticate = FALSE
        ),
        send = TRUE
      )

      list(success = TRUE, recipients = recipients)
    }, error = function(e) {
      cat("NOTIFICATION EMAIL ERROR:", e$message, "\n", file = stderr())
      list(success = FALSE, error = e$message)
    })
  }

  # Send notification when a project status changes to "Data released"
  send_data_released_email <- function(project_data) {
    tryCatch({
      con <- get_db_connection()
      on.exit(dbDisconnect(con))

      responsible_user <- trimws(project_data$responsible_user %||% "")
      users <- get_responsible_users(con)
      resolved_username <- resolve_responsible_username(responsible_user, users)
      user_row <- data.frame()

      if (!is.na(resolved_username) && nzchar(resolved_username)) {
        user_row <- dbGetQuery(
          con,
          "
          SELECT username, full_name, email
          FROM users
          WHERE lower(username) = lower(?)
          LIMIT 1
          ",
          params = list(resolved_username)
        )
      }

      if (nrow(user_row) == 0 && nzchar(responsible_user)) {
        full_name_matches <- dbGetQuery(
          con,
          "
          SELECT username, full_name, email
          FROM users
          WHERE lower(trim(full_name)) = lower(trim(?))
          ",
          params = list(responsible_user)
        )
        if (nrow(full_name_matches) == 1) {
          user_row <- full_name_matches[1, , drop = FALSE]
        }
      }

      if (nrow(user_row) == 0 && !is.null(project_data$user_id) && !is.na(project_data$user_id)) {
        user_row <- dbGetQuery(
          con,
          "SELECT username, full_name, email FROM users WHERE id = ? LIMIT 1",
          params = list(as.numeric(project_data$user_id))
        )
      }

      full_name <- format_responsible_display(responsible_user, con = con)
      if (!nzchar(full_name)) full_name <- responsible_user
      responsible_email <- ""
      if (nrow(user_row) > 0) {
        resolved_name <- trimws(user_row$full_name[[1]] %||% "")
        if (!nzchar(resolved_name)) resolved_name <- trimws(user_row$username[[1]] %||% "")
        if (nzchar(resolved_name)) full_name <- resolved_name
        responsible_email <- trimws(user_row$email[[1]] %||% "")
      }

      if (!nzchar(full_name)) full_name <- "User"

      project_code <- trimws(project_data$project_code %||% "")
      if (!nzchar(project_code) && !is.null(project_data$project_id) && !is.na(project_data$project_id)) {
        project_code <- paste0("P", as.character(project_data$project_id))
      }
      if (!nzchar(project_code)) {
        project_code <- as.character(project_data$project_id %||% "")
      }
      if (!nzchar(project_code)) project_code <- "project"

      recipients <- unique(c(responsible_email, "omicsdesk@biochem.mpg.de"))
      recipients <- recipients[!is.na(recipients) & recipients != ""]
      if (length(recipients) == 0) {
        return(list(success = FALSE, error = "No valid recipients found for status notification"))
      }

      subject <- paste0(project_code, " - Final sequencing results available")
      email_body <- paste0(
        "Dear ", full_name, ",\n\n",
        "Please note that your final sequencing results (project ", project_code, ") are now available.\n\n",
        "You can access the NGS data in your personal pool folder:\n\n",
        "(Windows) \\\\samba-pool-dnaseq\\pool-dnaseq\\\n",
        "(Mac) smb://samba-pool-dnaseq/pool-dnaseq/\n\n",
        "or, via datashare - ask us for a link.\n\n",
        "Best regards,\n",
        "The NGS sequencing facility"
      )

      send.mail(
        from = "ngs@biochem.mpg.de",
        to = recipients,
        encoding = "utf-8",
        subject = subject,
        body = email_body,
        smtp = list(
          host.name = "msx.biochem.mpg.de",
          port = 25,
          ssl = FALSE,
          tls = FALSE,
          authenticate = FALSE
        ),
        send = TRUE
      )

      return(list(success = TRUE, recipients = recipients))
    }, error = function(e) {
      cat("DATA RELEASE EMAIL ERROR:", e$message, "\n", file = stderr())
      return(list(success = FALSE, error = e$message))
    })
  }

  # User registration modal
  observeEvent(input$show_register_btn, {
    # Load budget holders for group dropdown
    con <- get_db_connection()
    budget_holders <- dbGetQuery(con, "SELECT DISTINCT name, surname FROM budget_holders ORDER BY name, surname")
    dbDisconnect(con)
    
    group_choices <- paste(budget_holders$name, budget_holders$surname)
    
    showModal(modalDialog(
      title = "User Registration",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("register_btn", "Register", class = "btn-primary")
      ),
      
      textInput("reg_username", "Username *", placeholder = "Choose a username"),
      textInput("reg_full_name", "Full Name", placeholder = "First Last"),
      textInput("reg_email", "Email *", placeholder = "your.email@institute.org"),
      passwordInput("reg_password", "Password *", placeholder = "Choose a password"),
      passwordInput("reg_confirm_password", "Confirm Password *", placeholder = "Confirm your password"),
      textInput("reg_phone", "Phone Number", placeholder = "+49 89 8578****"),
      selectInput("reg_group", "Research Group *", 
                  # This makes choosing a research group compulsory!
                  choices = c("Select your research group" = "", group_choices), # c("", group_choices), 
                  selected = ""),
      tags$small("* Required fields")
    ))
  })
  
  # Registration logic
  observeEvent(input$register_btn, {
    req(input$reg_username, input$reg_email, input$reg_password, input$reg_confirm_password)

    if(input$reg_password != input$reg_confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    if(nchar(input$reg_password) < 6) {
      showNotification("Password must be at least 6 characters", type = "error")
      return()
    }
    
    if(is.null(input$reg_group) || input$reg_group == "") {
      showNotification("Please select a research group", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Check if username exists
    existing_user <- dbGetQuery(con, 
                                "SELECT id FROM users WHERE username = ?", 
                                params = list(input$reg_username)
    )
    
    if(nrow(existing_user) > 0) {
      showNotification("Username already exists", type = "error")
      return()
    }
    
    # Insert new user
    full_name_input <- trimws(input$reg_full_name %||% "")
    if (full_name_input == "") {
      full_name_input <- input$reg_username
    }

    if (users_has_full_name(con)) {
      dbExecute(con, "
      INSERT INTO users (username, full_name, password, email, phone, research_group, is_admin)
      VALUES (?, ?, ?, ?, ?, ?, 0)
      ", params = list(
        input$reg_username,
        full_name_input,
        digest(input$reg_password),
        input$reg_email,
        input$reg_phone,
        input$reg_group
      ))
    } else {
      dbExecute(con, "
      INSERT INTO users (username, password, email, phone, research_group, is_admin)
      VALUES (?, ?, ?, ?, ?, 0)
      ", params = list(
        input$reg_username,
        digest(input$reg_password),
        input$reg_email,
        input$reg_phone,
        input$reg_group
      ))
    }

    removeModal()
    showNotification("Registration successful! You can now login.", type = "message")
  })

  ##################################################################
  # LDAP AUTO-LOGIN + PROFILE COMPLETION
  ##################################################################

  observe({
    if (get_auth_mode() == "ldap") {
      header_user <- get_proxy_authenticated_user(session)
      if (user$logged_in) {
        shinyjs::hide("login_screen", anim = FALSE)
      } else if (!is.null(header_user) && header_user != "" && !ldap_login_blocked()) {
        shinyjs::hide("login_screen", anim = FALSE)
      } else if (!is.null(dev_auth_user()) && dev_auth_user() != "") {
        shinyjs::hide("login_screen", anim = FALSE)
      } else {
        shinyjs::show("login_screen", anim = FALSE)
      }
    }
  })

  observe({
    if (get_auth_mode() != "ldap") return()
    if (ldap_login_blocked()) return()
    if (user$logged_in || pending_profile$active) return()

    username <- get_auth_username(session)
    if (is.null(username) || username == "") {
      if (!ldap_warned()) {
        showNotification(
          "LDAP mode enabled but no authenticated user found. For local dev, set DEV_REMOTE_USER.",
          type = "warning",
          duration = 10
        )
        ldap_warned(TRUE)
      }
      if (Sys.getenv("LDAP_DEBUG", "") == "1") {
        req <- session$request
        header_keys <- names(req)
        header_keys <- header_keys[grepl("^HTTP_|REMOTE_USER$", header_keys)]
        cat("LDAP DEBUG: headers when auth user missing\n", file = stderr())
        if (length(header_keys) == 0) {
          cat("  (no HTTP_* or REMOTE_USER headers found)\n", file = stderr())
        } else {
          for (k in header_keys) {
            val <- req[[k]]
            cat("  ", k, "=", ifelse(is.null(val) || val == "", "<empty>", val), "\n", file = stderr())
          }
        }
        cat(
          "LDAP DEBUG: resolved auth candidates when missing",
          "X_REMOTE_USER=", get_req_header(req, "x-remote-user") %||% "<missing>",
          "REMOTE_USER=", scalar_text(req$REMOTE_USER) %||% "<missing>",
          "X_FORWARDED_USER=", get_req_header(req, "x-forwarded-user") %||% "<missing>",
          "AUTHORIZATION=", ifelse(is.null(get_req_header(req, "authorization")), "<missing>", "<present>"),
          "CLIENT_URL_SEARCH=", client_url_search() %||% "<missing>",
          "SESSION_URL_SEARCH=", scalar_text(session$clientData$url_search) %||% "<missing>",
          "REQ_QUERY_STRING=", scalar_text(req$QUERY_STRING) %||% "<missing>",
          "\n",
          file = stderr()
        )
        flush.console()
      }
      return()
    }

    tryCatch({
      handle_ldap_login(username)
    }, error = function(e) {
      cat("LDAP LOGIN ERROR:", e$message, "\n", file = stderr())
      showNotification(
        "Authentication failed due to a server-side error. Please try again or contact admin.",
        type = "error",
        duration = 10
      )
    })
  })

  output$dev_tools_ui <- renderUI({
    if (!dev_mode) return(NULL)

    tagList(
      div(style = "margin-top: 1rem; padding: 0.75rem; border: 1px dashed #adb5bd; border-radius: 6px;",
          h4("Dev Tools"),
          radioButtons("dev_auth_mode", "Auth Mode",
                       choices = c("LDAP" = "ldap", "Local" = "local"),
                       selected = get_auth_mode(),
                       inline = TRUE)
      )
    )
  })

  observeEvent(input$dev_auth_mode, {
    req(input$dev_auth_mode)
    mode <- tolower(input$dev_auth_mode)
    if (!mode %in% c("ldap", "local")) mode <- "local"

    auth_mode_val(mode)
    Sys.setenv(AUTH_MODE = mode)

    user$logged_in <- FALSE
    user$username <- NULL
    user$full_name <- NULL
    user$user_id <- NULL
    user$is_admin <- FALSE

    if (mode == "local") {
      ldap_login_blocked(FALSE)
      dev_auth_user("")
      Sys.unsetenv("DEV_REMOTE_USER")
      show("login_screen")
      hide("main_app")
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
      hide("login_error")
    } else {
      show("login_screen")
      hide("main_app")
    }
  })

  output$dev_login_ui <- renderUI({
    if (!dev_mode) return(NULL)
    if (get_auth_mode() != "ldap") return(NULL)
    header_user <- get_proxy_authenticated_user(session)
    if (!is.null(header_user) && header_user != "") return(NULL)
    if (user$logged_in) return(NULL)

    tagList(
      div(style = "margin-top: 1rem; padding: 0.75rem; border: 1px solid #dee2e6; border-radius: 6px;",
          h4("Dev LDAP Simulation"),
          p("Local login is disabled in LDAP mode. Enter a username to simulate LDAP."),
          textInput("dev_login_username", "Dev Username", value = dev_auth_user()),
          actionButton("dev_login_btn", "Login as Dev User", class = "btn-primary")
      )
    )
  })

  output$dev_mode_badge <- renderUI({
    if (!dev_mode) return(NULL)
    mode_label <- toupper(get_auth_mode())
    badge_class <- if (get_auth_mode() == "ldap") "dev-mode-badge dev-mode-ldap" else "dev-mode-badge dev-mode-local"
    tags$span(paste("DEV ·", mode_label), class = badge_class)
  })

  output$budget_holder_notice <- renderUI({
    budget_holder_notice_ui(input$budget_id)
  })

  output$budget_holder_other_fields <- renderUI({
    if (input$budget_id != "other") return(NULL)
    tagList(
      budget_holder_other_fields_ui(),
      budget_holder_other_notice_ui(input$other_bh_cost_center)
    )
  })

  output$edit_budget_holder_notice <- renderUI({
    budget_holder_notice_ui(input$edit_budget_id)
  })

  output$edit_budget_holder_other_fields <- renderUI({
    if (input$edit_budget_id != "other") return(NULL)
    tagList(
      budget_holder_other_fields_ui(prefix = "edit_"),
      budget_holder_other_notice_ui(input$edit_other_bh_cost_center)
    )
  })

  observeEvent(input$dev_login_btn, {
    if (!dev_mode) return()
    req(input$dev_login_username)
    dev_auth_user(input$dev_login_username)
    Sys.setenv(DEV_REMOTE_USER = input$dev_login_username)
    handle_ldap_login(input$dev_login_username)
  })

  observeEvent(input$ldap_profile_save_btn, {
    req(pending_profile$active)

    if (is.null(input$ldap_profile_email) || input$ldap_profile_email == "") {
      showNotification("Email is required.", type = "error")
      return()
    }

    username <- pending_profile$username
    attrs <- pending_profile$attrs

    email <- input$ldap_profile_email
    phone <- input$ldap_profile_phone %||% attrs$phone %||% ""
    group <- input$ldap_profile_group %||% attrs$ou %||% ""

    if (is.null(group) || group == "") {
      showNotification("Research group is required.", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    if (isTRUE(pending_profile$is_new)) {
      placeholder_pw <- digest::digest(paste0("ldap-", username))
      dbExecute(con, "
        INSERT INTO users (username, password, email, phone, research_group, is_admin)
        VALUES (?, ?, ?, ?, ?, 0)
      ", params = list(username, placeholder_pw, email, phone, group))
    } else {
      dbExecute(con, "
        UPDATE users SET email = ?, phone = ?, research_group = ?
        WHERE username = ?
      ", params = list(email, phone, group, username))
    }

    pending_profile$active <- FALSE
    pending_profile$username <- NULL
    pending_profile$attrs <- list()
    pending_profile$is_new <- FALSE
    removeModal()

    user_data <- dbGetQuery(con,
                            "SELECT * FROM users WHERE username = ?",
                            params = list(username))
    if (nrow(user_data) == 1) {
      complete_login(user_data)
    }
  })

  observeEvent(input$ldap_profile_cancel_btn, {
    pending_profile$active <- FALSE
    pending_profile$username <- NULL
    pending_profile$attrs <- list()
    pending_profile$is_new <- FALSE
    removeModal()
    showNotification("Profile completion canceled. Reload the page to try again.", type = "warning")
  })

  observeEvent(input$ldap_relogin_btn, {
    ldap_login_blocked(FALSE)
    removeModal()
  })

  ##################################################################
  # END LDAP AUTO-LOGIN + PROFILE COMPLETION
  ##################################################################
  
  # Login functionality
  observeEvent(input$login_btn, {
    if (get_auth_mode() == "ldap") {
      showNotification("Local login is disabled in LDAP mode.", type = "warning")
      return()
    }

    req(input$login_username, input$login_password)
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    user_data <- dbGetQuery(con, 
                            "SELECT * FROM users WHERE username = ?", 
                            params = list(input$login_username)
    )
    
    if(nrow(user_data) == 1 && user_data$password == digest(input$login_password)) {
      complete_login(user_data)
    } else {
      show("login_error")
      output$login_error <- renderText("Invalid username or password")
    }
  })
  
  # Logout functionality
  observeEvent(input$logout_btn, {
    if (get_auth_mode() == "ldap") {
      showModal(modalDialog(
        title = "End Session (LDAP)",
        tagList(
          p("LDAP authentication is managed by the web server."),
          p("To fully log out or switch users, please close this tab/window or use a private window.")
        ),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }

    user$logged_in <- FALSE
    user$username <- NULL
    user$user_id <- NULL
    user$is_admin <- FALSE

    show("login_screen")
    hide("main_app")
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
    hide("login_error")
  })
  
  # Welcome message
  output$welcome_user <- renderText({
    if(user$logged_in) {
      paste("Welcome,", user$username, if(user$is_admin) "(Admin)" else "")
    }
  })

  output$announcement_blocks_ui <- renderUI({
    req(user$logged_in)
    announcement_refresh()

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    announcement_data <- load_announcement_content(con = con, active_only = TRUE)
    announcement_blocks(announcement_data)
  })
  
  # Main content with tabs for admins
  output$main_content <- renderUI({
    if(!user$logged_in) return()
    
    if(user$is_admin) {
      # Admin interface with tabs
      tabsetPanel(
        id = "admin_tabs",
        tabPanel(
          "Home",
          div(
            class = "home-page",
            h1("Welcome to the Sequencing Project Application System", class = "home-title"),
            div(
              class = "home-subtitle",
              p("This platform facilitates the submission and management of sequencing project applications."),
              p("As an administrator, you have access to both project application and management tools."),
              p("Use the tabs above to navigate between different functionalities.")
            ),
            div(
              class = "home-features",
              div(
                class = "feature-card",
                div(icon("plus-circle", class = "feature-icon")),
                h3("Apply for Projects", class = "feature-title"),
                p("Submit new sequencing project applications for your research needs.")
              ),
              div(
                class = "feature-card",
                div(icon("cogs", class = "feature-icon")),
                h3("Administer Projects", class = "feature-title"),
                p("Manage existing projects, update statuses, and oversee system settings.")
              )
            )
          )
        ),
        tabPanel(
          "Apply for Projects",
          uiOutput("announcement_blocks_ui"),
          uiOutput("user_interface")
        ),
        tabPanel(
          "Administer Projects",
          uiOutput("admin_interface")
        )
      )
    } else {
      tagList(
        uiOutput("announcement_blocks_ui"),
        uiOutput("user_interface")
      )
    }
  })

  # Load projects data
  load_projects <- function() {
    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    created_by_expr <- if (users_has_full_name(con)) {
      "COALESCE(NULLIF(u.full_name, ''), u.username)"
    } else {
      "u.username"
    }

    if(user$is_admin) {
      projects <- dbGetQuery(con, paste0("
        SELECT p.*, ", created_by_expr, " as created_by, t.name as type_name,
               bh.name as budget_holder_name, bh.surname as budget_holder_surname, bh.cost_center,
               st.service_type, sd.depth_description, sc.cycles_description
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN service_types st ON p.service_type_id = st.id
        LEFT JOIN sequencing_depths sd ON p.sequencing_depth_id = sd.id
        LEFT JOIN sequencing_cycles sc ON p.sequencing_cycles_id = sc.id
        ORDER BY p.project_id DESC
      "))
    } else {
      current_full_name <- user$full_name
      if (is.null(current_full_name) || current_full_name == "") {
        current_full_name <- user$username
      }
      current_label <- format_responsible_label(current_full_name, user$username)
      projects <- dbGetQuery(con, paste0("
        SELECT p.*, ", created_by_expr, " as created_by, t.name as type_name,
               bh.name as budget_holder_name, bh.surname as budget_holder_surname, bh.cost_center,
               st.service_type, sd.depth_description, sc.cycles_description
        FROM projects p 
        JOIN users u ON p.user_id = u.id
        LEFT JOIN types t ON p.type_id = t.id
        LEFT JOIN budget_holders bh ON p.budget_id = bh.id
        LEFT JOIN service_types st ON p.service_type_id = st.id
        LEFT JOIN sequencing_depths sd ON p.sequencing_depth_id = sd.id
        LEFT JOIN sequencing_cycles sc ON p.sequencing_cycles_id = sc.id
        WHERE lower(trim(p.responsible_user)) = lower(trim(?))
           OR lower(trim(p.responsible_user)) = lower(trim(?))
           OR lower(trim(p.responsible_user)) = lower(trim(?))
           OR lower(trim(p.responsible_user)) LIKE ('%(' || lower(trim(?)) || ')')
           OR p.user_id = ?
        ORDER BY p.project_id DESC
      "), params = list(user$username, current_full_name, current_label, user$username, user$user_id))
    }
    
    projects_data(projects)
  }
  
  # Regular user interface (project application)
  output$user_interface <- renderUI({
    if(!user$logged_in) return()
    
    tagList(
      div(
        class = "action-buttons",
        actionButton("new_project_btn", "Create New Project", 
                     class = "btn-primary", icon = icon("plus")),
        actionButton("edit_project_btn", "Edit Project", 
                     class = "btn-secondary", icon = icon("edit")),
        if (user$is_admin) {
          actionButton("delete_project_btn", "Delete Project", 
                       class = "btn-danger", icon = icon("trash"))
        }
      ),
      
      if(user$is_admin) {
        div(
          class = "action-buttons",
          actionButton("update_status_btn", "Update Project Status", 
                       class = "btn-info", icon = icon("sync"))
        )
      },
      
      div(
        class = "projects-table",
        DTOutput("projects_table")
      )
    )
  })
  
  # Admin interface (project administration)
  output$admin_interface <- renderUI({
    if(!user$logged_in || !user$is_admin) return()
    
    tagList(
      h3("Administrator Panel"),
      
      fluidRow(
        column(4,
               wellPanel(
                 h4("User Management"),
                 actionButton("manage_users_btn", "Manage Users", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Budget Holders"),
                 actionButton("manage_budget_holders_btn", "Manage Budget Holders", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Reference Genomes"),
                 actionButton("manage_genomes_btn", "Manage Genomes", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sample Types"),
                 actionButton("manage_types_btn", "Manage Sample Types", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sample Service Types"),
                 actionButton("manage_service_types_btn", "Manage Sample Service Types", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Depths"),
                 actionButton("manage_sequencing_depths_btn", "Manage Depths", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Cycles"),
                 actionButton("manage_sequencing_cycles_btn", "Manage Cycles", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Sequencing Platforms"),
                 actionButton("manage_sequencing_platforms_btn", "Manage Platforms", class = "btn-primary")
               )
        ),
        column(4,
               wellPanel(
                 h4("Landing Text"),
                 actionButton("manage_announcements_btn", "Manage Landing Text", class = "btn-primary")
               )
        )
      ),
      
      ##################################################################
      # DATABASE BACKUP/RESTORE FEATURES
      ##################################################################
      fluidRow(
        column(12,
               wellPanel(
                 h4("Database Management"),
                 p("Download a complete backup of the database for safekeeping."),
                 downloadButton("download_db_btn", "Download Database Backup", 
                                class = "btn-warning", style = "color: white;"),
                 fileInput("restore_db_btn", "Restore Backup", accept = ".sqlite"),
                 actionButton("validate_db_btn", "Validate Database", class = "btn-info")
               )
        )
      ),
      ##################################################################
      # END DATABASE BACKUP/RESTORE FEATURES
      ##################################################################
      
      div(
        class = "projects-table",
        DTOutput("admin_projects_table")
      )
    )
  })
  
  # New project modal with cost calculation
  observeEvent(input$new_project_btn, {
    showModal(modalDialog(
      title = "Create New Sequencing Project",
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_project_btn", "Create Project", class = "btn-primary")
      ),
      
      fluidRow(
        column(6,
               textInput("project_name", "Project Name *", placeholder = "Enter project name"),
               numericInput("num_samples", "Number of Samples *", value = 1, min = 1, max = 1000),
               selectInput("sequencing_platform", "Sequencing Platform *",
                           choices = admin_data$sequencing_platforms$name),
               selectInput("type_id", "Sample Type *",
                           choices = if(!is.null(admin_data$types) && nrow(admin_data$types) > 0) {
                             setNames(admin_data$types$id, admin_data$types$name)
                           } else {
                             c("No sample types available" = "")
                           }),
               selectInput("service_type_id", "Sample Service Type *",
                           choices = if(!is.null(admin_data$service_types) && nrow(admin_data$service_types) > 0) {
                             setNames(admin_data$service_types$id, 
                                      paste(admin_data$service_types$service_type, 
                                            "- €", admin_data$service_types$costs_per_sample, "/sample"))
                           } else {
                             c("No sample service types available" = "")
                           })
        ),
        column(6,
               selectInput("reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes),
               selectInput("sequencing_depth_id", "Data Amount (Total Reads) *",
                           choices = if(!is.null(admin_data$sequencing_depths) && nrow(admin_data$sequencing_depths) > 0) {
                             setNames(admin_data$sequencing_depths$id, 
                                      admin_data$sequencing_depths$depth_description)
                           } else {
                             c("No sequencing depths available" = "")
                           }),
               div(style = "font-size: 11px;", helpText("Estimated total sequencing output for this project (e.g., up to 200M / 400M / 800M reads).")),
               selectInput("sequencing_cycles_id", "Read Length (Cycles) *",
                           choices = if(!is.null(admin_data$sequencing_cycles) && nrow(admin_data$sequencing_cycles) > 0) {
                             setNames(admin_data$sequencing_cycles$id, 
                                      admin_data$sequencing_cycles$cycles_description)
                           } else {
                             c("No sequencing cycles available" = "")
                           }),
               div(style = "font-size: 11px;", helpText("Read setup, e.g., 2x75 (up to 150 cycles) or 2x150 (up to 300 cycles).")),
               selectInput("budget_id", "Budget Holder *",
                           choices = if(!is.null(admin_data$budget_holders) && nrow(admin_data$budget_holders) > 0) {
                             c(
                               setNames(as.character(admin_data$budget_holders$id),
                                        paste(admin_data$budget_holders$name,
                                              admin_data$budget_holders$surname,
                                              "-", admin_data$budget_holders$cost_center)),
                               "Other / not listed" = "other"
                             )
                           } else {
                             c("Other / not listed" = "other")
                           }),
               uiOutput("budget_holder_notice"),
               uiOutput("budget_holder_other_fields"),
               selectInput("responsible_user", "Responsible User *",
                           choices = get_responsible_user_choices(),
                           selected = user$username %||% ""),
               radioButtons("kickoff_meeting", "Kick-off Meeting Required? *",
                            choices = c("Yes" = 1, "No" = 0), 
                            selected = 0, inline = TRUE)
        )
      ),
      fluidRow(
        column(12,
               textAreaInput("project_description", "Project Description", 
                             placeholder = "Describe your project objectives, samples, and any special requirements...", 
                             rows = 4)
        )
      ),
      
      # Cost calculation display
      fluidRow(
        column(12,
               div(class = "cost-calculation",
                   h4("Cost Calculation"),
                   uiOutput("cost_calculation_display")
               )
        )
      ),
      tags$small("* Required fields")
    ))
    # Get current user's research group
    con <- get_db_connection()
    user_group <- dbGetQuery(con, 
                             "SELECT research_group FROM users WHERE id = ?", 
                             params = list(user$user_id)
    )$research_group
    dbDisconnect(con)
    
    # Safely find the budget holder ID that matches the user's group
    if(!is.null(user_group) && user_group != "" && !is.na(user_group)) {
      budget_holders <- admin_data$budget_holders
      
      # Check if budget_holders exists and has data
      if(!is.null(budget_holders) && nrow(budget_holders) > 0) {
        # Create full name for comparison
        budget_full_names <- paste(budget_holders$name, budget_holders$surname)
        
        # Find matching budget holder
        matching_index <- which(budget_full_names == user_group)
        if (length(matching_index) == 0) {
          # Fallback: partial match (e.g., "Cox" -> "Cox Juergen")
          matching_index <- which(grepl(user_group, budget_full_names, ignore.case = TRUE))
        }
        
        if(length(matching_index) > 0) {
          # Auto-select the matching budget holder
          updateSelectInput(session, "budget_id", selected = budget_holders$id[matching_index[1]])
          cat("Auto-selected budget holder:", user_group, "\n")
        } else {
          cat("No matching budget holder found for:", user_group, "\n")
        }
      } else {
        cat("Budget holders data is empty or NULL\n")
      }
    } else {
      cat("User group is null, empty, or NA:", user_group, "\n")
    }
  })
  
  # Dynamic cost calculation
  output$cost_calculation_display <- renderUI({
    base_cost <- calculate_base_cost(input$num_samples, input$service_type_id,
                                     input$sequencing_depth_id, input$sequencing_cycles_id)
    
    service_type <- admin_data$service_types[admin_data$service_types$id == as.numeric(input$service_type_id), ]
    sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == as.numeric(input$sequencing_depth_id), ]
    
    prep_cost <- if(nrow(service_type) > 0) service_type$costs_per_sample * as.numeric(input$num_samples) else 0
    sequencing_cost <- base_cost - prep_cost
    
    tagList(
      p(paste0("Preparation Cost: €", format_cost_amount(prep_cost))),
      p(paste0("Sequencing Cost: €", format_cost_amount(sequencing_cost))),
      p("Additional costs:"),
      p(class = "cost-total", paste0("Total Estimated Cost: €", format_cost_amount(base_cost))),
      if(!is.null(sequencing_depth) && nrow(sequencing_depth) > 0 && sequencing_depth$depth_description == "other") {
        p(class = "cost-warning", 
          "Note: 'Other' sequencing depth selected. These costs are preliminary. Please contact us to discuss your specific needs.")
      }
    )
  })
  
  get_or_create_budget_holder <- function(con, name, surname, cost_center, email) {
    name <- trimws(name)
    surname <- trimws(surname)
    cost_center <- trimws(cost_center)
    email <- trimws(email)

    existing <- dbGetQuery(con,
                           "SELECT id FROM budget_holders WHERE name = ? AND surname = ? AND cost_center = ?",
                           params = list(name, surname, cost_center))
    if (nrow(existing) > 0) {
      return(existing$id[1])
    }

    dbExecute(con, "
      INSERT INTO budget_holders (name, surname, cost_center, email)
      VALUES (?, ?, ?, ?)
    ", params = list(name, surname, cost_center, email))

    dbGetQuery(con,
               "SELECT id FROM budget_holders WHERE name = ? AND surname = ? AND cost_center = ?",
               params = list(name, surname, cost_center))$id[1]
  }

  # Create project with email notification
  observeEvent(input$create_project_btn, {
    # Validate required fields
    required_fields <- c("project_name", "num_samples", "sequencing_platform", 
                         "service_type_id", "reference_genome", "sequencing_depth_id", 
                         "sequencing_cycles_id", "budget_id", "responsible_user", "type_id")
    
    missing_fields <- sapply(required_fields, function(field) {
      value <- input[[field]]
      is.null(value) || value == "" || (is.numeric(value) && is.na(value))
    })
    
    if(any(missing_fields)) {
      showNotification("Please fill in all required fields", type = "error")
      return()
    }

    if (input$budget_id == "other") {
      if (is.null(input$other_bh_name) || input$other_bh_name == "" ||
          is.null(input$other_bh_surname) || input$other_bh_surname == "") {
        showNotification("Please enter PI name and surname for the new budget holder.", type = "error")
        return()
      }
    }
    
    # Calculate total cost
    total_cost <- calculate_total_cost(input$num_samples, input$service_type_id, 
                                       input$sequencing_depth_id, input$sequencing_cycles_id)
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Resolve budget holder
    budget_id_value <- input$budget_id
    if (budget_id_value == "other") {
      bh_cost_center <- trimws(input$other_bh_cost_center %||% "")
      if (bh_cost_center == "") bh_cost_center <- "N.A."
      bh_email <- trimws(input$other_bh_email %||% "")
      if (bh_email == "") bh_email <- "ngs@biochem.mpg.de"

      budget_id_value <- get_or_create_budget_holder(
        con,
        input$other_bh_name,
        input$other_bh_surname,
        bh_cost_center,
        bh_email
      )
    }

    responsible_user_value <- resolve_responsible_for_storage(
      input$responsible_user,
      con,
      fallback = user$username
    )

    # Insert project
    dbExecute(con, "
    INSERT INTO projects 
    (project_name, user_id, responsible_user, reference_genome, service_type_id, 
     budget_id, description, num_samples, sequencing_platform, sequencing_depth_id, 
     sequencing_cycles_id, kickoff_meeting, type_id, total_cost, status)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Created')
  ", params = list(
    input$project_name,
    user$user_id,
    responsible_user_value,
    input$reference_genome,
    as.numeric(input$service_type_id),
    as.numeric(budget_id_value),
    input$project_description,
    input$num_samples,
    input$sequencing_platform,
    as.numeric(input$sequencing_depth_id),
    as.numeric(input$sequencing_cycles_id),
    as.numeric(input$kickoff_meeting),
    as.numeric(input$type_id),
    total_cost
  ))
    
    # Resolve the auto-generated project_id for email subject (P<number>)
    new_row_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    project_id_value <- dbGetQuery(con, "SELECT project_id FROM projects WHERE id = ?",
                                   params = list(as.numeric(new_row_id)))$project_id
    project_code <- if (!is.null(project_id_value) && length(project_id_value) > 0 &&
                          !is.na(project_id_value)) {
      paste0("P", project_id_value)
    } else {
      ""
    }

    # Get the created project data for email
    project_data <- list(
      project_id = project_id_value,
      project_code = project_code,
      project_name = input$project_name,
      description = input$project_description,
      responsible_user = responsible_user_value,
      num_samples = input$num_samples,
      service_type_id = as.numeric(input$service_type_id),
      sequencing_depth_id = as.numeric(input$sequencing_depth_id),
      total_cost = total_cost
    )
    
    # Get budget holder and user email
    budget_holder <- dbGetQuery(con, "SELECT * FROM budget_holders WHERE id = ?", params = list(as.numeric(budget_id_value)))
    user_email <- dbGetQuery(con, "SELECT email FROM users WHERE id = ?", params = list(user$user_id))$email
    
    removeModal()
    load_projects()
    
    # Send email notification
    send_project_creation_email(project_data, budget_holder, user_email)
    
    showNotification("Project created successfully! NGS team notified.", type = "message")
  })
  
  # Projects table with updated columns - MODIFIED FOR PREFIXED PROJECT IDs
  output$projects_table <- renderDT({
    req(projects_data())
    
    cat("DEBUG: Projects data dimensions:", dim(projects_data()), "\n")
    cat("DEBUG: Projects data columns:", names(projects_data()), "\n")
    cat("DEBUG: Projects data row count:", nrow(projects_data()), "\n")
    
    display_data <- projects_data()

    # === ADD THIS CHECK ===
    if(nrow(display_data) == 0) {
      return(datatable(
        data.frame(Message = "No projects found. Click 'Create New Project' to get started."),
        options = list(dom = 't'),
        rownames = FALSE,
        colnames = ""
      ))
    }
    
    # Convert kickoff_meeting to descriptive text
    if("kickoff_meeting" %in% names(display_data)) {
      display_data$kickoff_meeting <- ifelse(
        display_data$kickoff_meeting == 1, 
        "Yes, I would like to get a support.", 
        "No, I am a self-sufficient user."
      )
    }
    
    # Create budget holder display
    if(all(c("budget_holder_name", "cost_center") %in% names(display_data))) {
      display_data$budget_display <- paste(display_data$budget_holder_name, "-", display_data$cost_center)
    }

    if ("responsible_user" %in% names(display_data)) {
      con_display <- get_db_connection()
      on.exit(dbDisconnect(con_display), add = TRUE)
      display_data$responsible_user <- vapply(
        display_data$responsible_user,
        function(x) format_responsible_display(x, con = con_display),
        FUN.VALUE = character(1)
      )
    }
    
    # Define the columns we want to show (project_id is numeric; we render the P-prefix at display-time)
    display_columns <- c("project_id", "project_name", "num_samples", "sequencing_platform", 
                         "reference_genome", "type_name", "service_type", 
                         "depth_description", "cycles_description", "budget_display",
                         "responsible_user", "kickoff_meeting", "total_cost", "status")
    
    # Add created_by for admins, created_at for all
    if(user$is_admin) {
      display_columns <- c(display_columns, "created_by", "created_at")
    } else {
      display_columns <- c(display_columns, "created_at")
    }
    
    # Select only columns that exist in the data
    available_columns <- display_columns[display_columns %in% names(display_data)]
    display_data <- display_data[, available_columns, drop = FALSE]
    
    # Define column names
    column_names <- c(
      "Project ID", "Project Name", "Samples", "Platform", "Reference", 
      "Sample Type", "Sample Service Type", "Sequencing Depth", "Sequencing Cycles",
      "Budget Holder", "Responsible User", "Kick-off Meeting", "Total Cost", "Status"
    )
    
    if(user$is_admin) {
      column_names <- c(column_names, "Created By", "Created")
    } else {
      column_names <- c(column_names, "Created")
    }

    column_width_defs <- list(
      list(
        targets = 0,
        width = "80px",
        render = JS("function(data, type, row, meta) { if (type === 'display') return 'P' + data; return data; }")
      ),   # Project ID (numeric sort, prefixed display)
      list(targets = 1, width = "180px"),  # Project Name
      list(targets = 2, width = "70px"),   # Samples
      list(targets = 3, width = "110px"),  # Platform
      list(targets = 4, width = "140px"),  # Reference
      list(targets = 5, width = "190px"),  # Sample Type
      list(targets = 6, width = "170px"),  # Sample Service Type
      list(targets = 7, width = "150px"),  # Sequencing Depth
      list(targets = 8, width = "130px"),  # Sequencing Cycles
      list(targets = 9, width = "180px"),  # Budget Holder
      list(targets = 10, width = "130px"), # Responsible User
      list(targets = 11, width = "240px"), # Kick-off Meeting
      list(targets = 12, width = "90px"),  # Total Cost
      list(targets = 13, width = "140px")  # Status
    )

    if(user$is_admin) {
      column_width_defs <- c(
        column_width_defs,
        list(
          list(targets = 14, width = "110px"), # Created By
          list(targets = 15, width = "150px")  # Created
        )
      )
    } else {
      column_width_defs <- c(
        column_width_defs,
        list(
          list(targets = 14, width = "150px") # Created
        )
      )
    }

    datatable(
      display_data,
      selection = 'single',
      options = list(
        pageLength = 10,
        autoWidth = FALSE,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        columnDefs = column_width_defs,
        order = list(list(0, "desc"))
      ),
      rownames = FALSE,
      colnames = column_names
    ) %>%
      formatStyle(
        'status',
        backgroundColor = styleEqual(
          names(status_colors),
          unname(status_colors)
        )
      ) %>%
      formatCurrency('total_cost', currency = "€", digits = 2)
  })
  
  # Load projects when user logs in
  observeEvent(user$logged_in, {
    if(user$logged_in) {
      load_projects()
    }
  })
  
  # === ADMIN MANAGEMENT MODALS ===
  
  # Budget Holders Management Modal
  observeEvent(input$manage_budget_holders_btn, {
    showModal(modalDialog(
      title = "Budget Holders Management",
      size = "l",
      footer = modalButton("Close"),
      
      h4("Add New Budget Holder"),
      fluidRow(
        column(3,
               textInput("new_bh_name", "Name", placeholder = "Enter name")
        ),
        column(3,
               textInput("new_bh_surname", "Surname", placeholder = "Enter surname")
        ),
        column(3,
               textInput("new_bh_cost_center", "Cost Center", placeholder = "Enter cost center")
        ),
        column(3,
               textInput("new_bh_email", "Email", placeholder = "Enter email")
        )
      ),
      fluidRow(
        column(12,
               actionButton("add_budget_holder_btn", "Add Budget Holder", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Existing Budget Holders"),
      DTOutput("budget_holders_table_admin"),
      div(
        actionButton("edit_budget_holder_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_budget_holder_btn", "Delete Selected Budget Holder", class = "btn-danger")
      )
    ))
  })
  
  # Sample Service Types Management Modal
  observeEvent(input$manage_service_types_btn, {
    showModal(modalDialog(
      title = "Sample Service Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sample Service Type"),
      fluidRow(
        column(4,
               textInput("new_service_type", "Sample Service Type", placeholder = "Enter sample service type")
        ),
        column(4,
               textInput("new_service_kit", "Kit", placeholder = "Enter kit name")
        ),
        column(4,
               numericInput("new_service_cost", "Cost per Sample", value = 0, min = 0)
        )
      ),
      fluidRow(
        column(12,
               actionButton("add_service_type_btn", "Add Sample Service Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sample Service Types"),
      DTOutput("service_types_table_admin"),
      div(  # ADD Adding the edit button to the table
        actionButton("edit_service_type_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_service_type_btn", "Delete Selected Sample Service Type", class = "btn-danger")
        )
      )
    )
  })
  
  # Sequencing Depths Management Modal
  observeEvent(input$manage_sequencing_depths_btn, {
    showModal(modalDialog(
      title = "Sequencing Depths Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Depth"),
      fluidRow(
        column(3,
               textInput("new_depth_description", "Depth Description", placeholder = "e.g., upto 200M")
        ),
        column(3,
               numericInput("new_depth_cost_150", "Cost up to 150 cycles", value = 0, min = 0)
        ),
        column(3,
               numericInput("new_depth_cost_300", "Cost up to 300 cycles", value = 0, min = 0)
        ),
        column(3,
               actionButton("add_sequencing_depth_btn", "Add Depth", class = "btn-primary", style = "margin-top: 25px;")
        )
      ),
      
      hr(),
      h4("Current Sequencing Depths"),
      DTOutput("sequencing_depths_table_admin"),
      div(  # ADD Adding the edit button to the table
        actionButton("edit_sequencing_depth_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_sequencing_depth_btn", "Delete Selected Depth", class = "btn-danger")
        )
      ) 
    )
  })
  
  # Sequencing Cycles Management Modal
  observeEvent(input$manage_sequencing_cycles_btn, {
    showModal(modalDialog(
      title = "Sequencing Cycles Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Cycle"),
      fluidRow(
        column(8,
               textInput("new_cycles_description", "Cycles Description", placeholder = "e.g., upto 100/150 cycles (2x60 or 2x 75)")
        ),
        column(4,
               actionButton("add_sequencing_cycles_btn", "Add Cycles", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Cycles"),
      DTOutput("sequencing_cycles_table_admin"),
      actionButton("delete_sequencing_cycles_btn", "Delete Selected Cycles", class = "btn-danger")
    ))
  })
  
  # Existing management modals (updated for new structure)
  observeEvent(input$manage_users_btn, {
    showModal(modalDialog(
      title = "User Management",
      size = "l",
      footer = modalButton("Close"),
      
      h4("Add New User"),
      fluidRow(
        column(6,
               textInput("new_user_username", "Username", placeholder = "Enter username")
        ),
        column(6,
               textInput("new_user_email", "Email", placeholder = "Enter email")
        )
      ),
      fluidRow(
        column(6,
               passwordInput("new_user_password", "Password", placeholder = "Enter password")
        ),
        column(6,
               passwordInput("new_user_confirm_password", "Confirm Password", placeholder = "Confirm password")
        )
      ),
      fluidRow(
        column(12,
               checkboxInput("new_user_admin", "Administrator", value = FALSE),
               actionButton("add_user_admin_btn", "Add User", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Existing Users"),
      DTOutput("users_table_admin"),
      actionButton("delete_user_admin_btn", "Delete Selected User", class = "btn-danger")
    ))
  })
  
  observeEvent(input$manage_genomes_btn, {
    showModal(modalDialog(
      title = "Reference Genome Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Reference Genome"),
      fluidRow(
        column(8,
               textInput("new_genome_name", "Genome Name", placeholder = "Enter genome name")
        ),
        column(4,
               actionButton("add_genome_btn", "Add Genome", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Reference Genomes"),
      DTOutput("genomes_table_admin"),
      div(
        actionButton("edit_genome_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_genome_btn", "Delete Selected Genome", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$manage_types_btn, {
    showModal(modalDialog(
      title = "Sample Types Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sample Type"),
      fluidRow(
        column(8,
               textInput("new_type_name", "Sample Type Name", placeholder = "Enter sample type name")
        ),
        column(4,
               actionButton("add_type_btn", "Add Sample Type", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sample Types"),
      DTOutput("types_table_admin"),
      div(
        actionButton("edit_type_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_type_btn", "Delete Selected Sample Type", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$manage_sequencing_platforms_btn, {
    showModal(modalDialog(
      title = "Sequencing Platforms Management",
      size = "m",
      footer = modalButton("Close"),
      
      h4("Add New Sequencing Platform"),
      fluidRow(
        column(8,
               textInput("new_sequencing_platform_name", "Platform Name", placeholder = "Enter sequencing platform name")
        ),
        column(4,
               actionButton("add_sequencing_platform_btn", "Add Platform", class = "btn-primary")
        )
      ),
      
      hr(),
      h4("Current Sequencing Platforms"),
      DTOutput("sequencing_platforms_table_admin"),
      div(
        actionButton("edit_sequencing_platform_btn", "Edit Selected", class = "btn-warning"),
        actionButton("delete_sequencing_platform_btn", "Delete Selected Platform", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$manage_announcements_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    panels <- get_announcement_panels(con = con, active_only = FALSE)

    if (nrow(panels) == 0) {
      showNotification("No announcement panels are configured.", type = "error")
      return()
    }

    panel_labels <- ifelse(
      is.na(panels$title) | trimws(panels$title) == "",
      panels$panel_key,
      panels$title
    )
    panel_choices <- setNames(panels$panel_key, panel_labels)

    showModal(modalDialog(
      title = "Landing Text Management",
      size = "l",
      footer = modalButton("Close"),
      selectInput(
        "announcement_panel_select",
        "Panel",
        choices = panel_choices,
        selected = panels$panel_key[1]
      ),
      tags$p(
        tags$strong("Markdown-lite syntax:"),
        " use **bold**, bullet lists (- item), numbered lists (1. item), and links like [@NGS](mailto:ngs@biochem.mpg.de)."
      ),
      DTOutput("announcement_items_table_admin"),
      div(
        class = "action-buttons",
        actionButton("announcement_add_btn", "Add Box", class = "btn-primary"),
        actionButton("announcement_edit_btn", "Edit Selected", class = "btn-warning"),
        actionButton("announcement_move_up_btn", "Move Up", class = "btn-info"),
        actionButton("announcement_move_down_btn", "Move Down", class = "btn-info"),
        actionButton("announcement_delete_btn", "Delete Selected", class = "btn-danger")
      )
    ))
  })
  
  # === ADMIN TABLES RENDERING ===

  output$announcement_items_table_admin <- renderDT({
    req(user$logged_in, user$is_admin, input$announcement_panel_select)
    announcement_refresh()

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    items <- get_announcement_items_for_panel(
      panel_key = input$announcement_panel_select,
      con = con,
      active_only = FALSE
    )
    announcement_items_current(items)

    if (nrow(items) == 0) {
      return(datatable(
        data.frame(Message = "No text boxes configured for this panel."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    preview <- gsub("[\r\n]+", " ", items$markdown_text)
    preview <- trimws(gsub("\\s+", " ", preview))
    preview <- ifelse(
      nchar(preview) > 140,
      paste0(substr(preview, 1, 137), "..."),
      preview
    )

    display <- data.frame(
      Order = items$display_order,
      Active = ifelse(items$is_active == 1, "Yes", "No"),
      Content = preview,
      stringsAsFactors = FALSE
    )

    datatable(
      display,
      selection = "single",
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  output$budget_holders_table_admin <- renderDT({
    datatable(
      admin_data$budget_holders[, c("name", "surname", "cost_center", "email")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Name", "Surname", "Cost Center", "Email")
    )
  })
  
  output$service_types_table_admin <- renderDT({
    datatable(
      admin_data$service_types[, c("service_type", "kit", "costs_per_sample")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Sample Service Type", "Kit", "Cost per Sample")
    ) %>% formatCurrency('costs_per_sample', currency = "€", digits = 2)
  })
  
  output$sequencing_depths_table_admin <- renderDT({
    datatable(
      admin_data$sequencing_depths[, c("depth_description", "cost_upto_150_cycles", "cost_upto_300_cycles")],
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Depth Description", "Cost up to 150 cycles", "Cost up to 300 cycles")
    ) %>% formatCurrency(c('cost_upto_150_cycles', 'cost_upto_300_cycles'), currency = "€", digits = 2)
  })
  
  output$sequencing_cycles_table_admin <- renderDT({
    cycles_df <- admin_data$sequencing_cycles
    if(nrow(cycles_df) > 0) {
      cycles_df <- cycles_df[, c("id", "cycles_description"), drop = FALSE]  # KEEP AS DATA FRAME
    }
    datatable(
      cycles_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Cycles Description")  # UPDATE COLUMN NAMES
    )
  })
  
  output$users_table_admin <- renderDT({
    # Check which columns actually exist
    available_cols <- names(admin_data$users)
    cat("Available user columns:", available_cols, "\n")
    
    # Select only columns that exist
    display_data <- admin_data$users[, available_cols, drop = FALSE]
    
    datatable(
      display_data,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  output$genomes_table_admin <- renderDT({
    genomes_df <- data.frame(name = admin_data$reference_genomes)
    datatable(
      genomes_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("Reference Genome Name")
    )
  })
  
  output$types_table_admin <- renderDT({
    types_df <- admin_data$types
    if(nrow(types_df) > 0) {
      types_df <- types_df[, c("id", "name"), drop = FALSE]
    }
    datatable(
      types_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Sample Type Name")
    )
  })
  
  output$sequencing_platforms_table_admin <- renderDT({
    platforms_df <- admin_data$sequencing_platforms
    if(nrow(platforms_df) > 0) {
      platforms_df <- platforms_df[, c("id", "name"), drop = FALSE]
    }
    datatable(
      platforms_df,
      selection = 'single',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("ID", "Platform Name")
    )
  })

  observeEvent(input$announcement_add_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    req(input$announcement_panel_select)
    announcement_editing_item_id(NULL)

    showModal(modalDialog(
      title = "Add Text Box",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_announcement_item_btn", "Save", class = "btn-primary")
      ),
      checkboxInput("item_active", "Active", value = TRUE),
      textAreaInput(
        "item_markdown_text",
        "Content (Markdown-lite)",
        value = "",
        rows = 10,
        placeholder = "Example:\n**QC submission**: Everyday **9:00 - 11:00**\n- Result will be available **13:00-15:00**\n- Ask us for a datashare link"
      ),
      tags$small(paste0("Maximum length: ", max_announcement_chars, " characters."))
    ))
  })

  observeEvent(input$save_announcement_item_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    req(input$announcement_panel_select)
    validation_error <- validate_announcement_markdown(input$item_markdown_text)
    if (!is.null(validation_error)) {
      showNotification(validation_error, type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    panel <- dbGetQuery(
      con,
      "SELECT id FROM announcement_panels WHERE panel_key = ?",
      params = list(input$announcement_panel_select)
    )
    if (nrow(panel) == 0) {
      showNotification("Selected panel does not exist.", type = "error")
      return()
    }

    next_order <- dbGetQuery(
      con,
      "SELECT COALESCE(MAX(display_order), 0) + 1 AS next_order FROM announcement_items WHERE panel_id = ?",
      params = list(panel$id[1])
    )$next_order[1]

    dbExecute(con, "
      INSERT INTO announcement_items (panel_id, display_order, markdown_text, is_active, updated_by)
      VALUES (?, ?, ?, ?, ?)
    ", params = list(
      panel$id[1],
      as.integer(next_order),
      as.character(input$item_markdown_text),
      as.integer(isTRUE(input$item_active)),
      user$username %||% "admin"
    ))

    removeModal()
    refresh_announcements()
    showNotification("Landing text box added.", type = "message")
  })

  observeEvent(input$announcement_edit_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    item <- selected_announcement_item()
    if (is.null(item)) {
      showNotification("Please select a text box to edit.", type = "warning")
      return()
    }

    announcement_editing_item_id(item$id[1])

    showModal(modalDialog(
      title = "Edit Text Box",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_announcement_item_btn", "Update", class = "btn-primary")
      ),
      checkboxInput("item_active", "Active", value = as.logical(item$is_active[1])),
      textAreaInput(
        "item_markdown_text",
        "Content (Markdown-lite)",
        value = item$markdown_text[1],
        rows = 10
      ),
      tags$small(paste0("Maximum length: ", max_announcement_chars, " characters."))
    ))
  })

  observeEvent(input$update_announcement_item_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    item_id <- announcement_editing_item_id()
    if (is.null(item_id)) {
      showNotification("No announcement item selected.", type = "error")
      return()
    }

    validation_error <- validate_announcement_markdown(input$item_markdown_text)
    if (!is.null(validation_error)) {
      showNotification(validation_error, type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    dbExecute(con, "
      UPDATE announcement_items
      SET markdown_text = ?, is_active = ?, updated_at = CURRENT_TIMESTAMP, updated_by = ?
      WHERE id = ?
    ", params = list(
      as.character(input$item_markdown_text),
      as.integer(isTRUE(input$item_active)),
      user$username %||% "admin",
      as.integer(item_id)
    ))

    announcement_editing_item_id(NULL)
    removeModal()
    refresh_announcements()
    showNotification("Landing text box updated.", type = "message")
  })

  observeEvent(input$announcement_delete_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    item <- selected_announcement_item()
    if (is.null(item)) {
      showNotification("Please select a text box to delete.", type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Delete selected text box (order", item$display_order[1], ")?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_announcement_item_btn", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_announcement_item_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    item <- selected_announcement_item()
    if (is.null(item)) {
      showNotification("Please select a text box to delete.", type = "warning")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    dbExecute(con, "DELETE FROM announcement_items WHERE id = ?", params = list(as.integer(item$id[1])))
    normalize_announcement_item_order(con, panel_id = as.integer(item$panel_id[1]))

    removeModal()
    refresh_announcements()
    showNotification("Landing text box deleted.", type = "message")
  })

  observeEvent(input$announcement_move_up_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    selected_row <- input$announcement_items_table_admin_rows_selected
    items <- announcement_items_current()
    if (length(selected_row) == 0 || nrow(items) == 0) {
      showNotification("Please select a text box to move.", type = "warning")
      return()
    }

    idx <- selected_row[1]
    if (idx <= 1) {
      showNotification("Selected text box is already at the top.", type = "message")
      return()
    }

    current_item <- items[idx, , drop = FALSE]
    previous_item <- items[idx - 1, , drop = FALSE]

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    dbExecute(con, "
      UPDATE announcement_items SET display_order = ?, updated_at = CURRENT_TIMESTAMP, updated_by = ?
      WHERE id = ?
    ", params = list(
      as.integer(previous_item$display_order[1]),
      user$username %||% "admin",
      as.integer(current_item$id[1])
    ))
    dbExecute(con, "
      UPDATE announcement_items SET display_order = ?, updated_at = CURRENT_TIMESTAMP, updated_by = ?
      WHERE id = ?
    ", params = list(
      as.integer(current_item$display_order[1]),
      user$username %||% "admin",
      as.integer(previous_item$id[1])
    ))

    refresh_announcements()
    showNotification("Text box moved up.", type = "message")
  })

  observeEvent(input$announcement_move_down_btn, {
    if (!isTRUE(user$is_admin)) {
      showNotification("Admin permissions required.", type = "error")
      return()
    }

    selected_row <- input$announcement_items_table_admin_rows_selected
    items <- announcement_items_current()
    if (length(selected_row) == 0 || nrow(items) == 0) {
      showNotification("Please select a text box to move.", type = "warning")
      return()
    }

    idx <- selected_row[1]
    if (idx >= nrow(items)) {
      showNotification("Selected text box is already at the bottom.", type = "message")
      return()
    }

    current_item <- items[idx, , drop = FALSE]
    next_item <- items[idx + 1, , drop = FALSE]

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    dbExecute(con, "
      UPDATE announcement_items SET display_order = ?, updated_at = CURRENT_TIMESTAMP, updated_by = ?
      WHERE id = ?
    ", params = list(
      as.integer(next_item$display_order[1]),
      user$username %||% "admin",
      as.integer(current_item$id[1])
    ))
    dbExecute(con, "
      UPDATE announcement_items SET display_order = ?, updated_at = CURRENT_TIMESTAMP, updated_by = ?
      WHERE id = ?
    ", params = list(
      as.integer(current_item$display_order[1]),
      user$username %||% "admin",
      as.integer(next_item$id[1])
    ))

    refresh_announcements()
    showNotification("Text box moved down.", type = "message")
  })
  
  # === ADD NEW ITEMS HANDLERS ===
  
  # Add new budget holder
  observeEvent(input$add_budget_holder_btn, {
    req(input$new_bh_name, input$new_bh_surname, input$new_bh_cost_center, input$new_bh_email)
    
    if(input$new_bh_name == "" || input$new_bh_surname == "" || input$new_bh_cost_center == "" || input$new_bh_email == "") {
      showNotification("Please fill in all budget holder fields", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    # Check if cost center already exists
    existing_bh <- dbGetQuery(con, 
                              "SELECT id FROM budget_holders WHERE cost_center = ?", 
                              params = list(input$new_bh_cost_center))
    
    if(nrow(existing_bh) > 0) {
      showNotification("Cost center already exists", type = "error")
      return()
    }
    
    # Insert new budget holder
    dbExecute(con, "
      INSERT INTO budget_holders (name, surname, cost_center, email)
      VALUES (?, ?, ?, ?)
    ", params = list(
      input$new_bh_name,
      input$new_bh_surname,
      input$new_bh_cost_center,
      input$new_bh_email
    ))
    
    # Clear form
    updateTextInput(session, "new_bh_name", value = "")
    updateTextInput(session, "new_bh_surname", value = "")
    updateTextInput(session, "new_bh_cost_center", value = "")
    updateTextInput(session, "new_bh_email", value = "")
    
    # Reload admin data
    load_admin_data()
    
    showNotification("Budget holder added successfully!", type = "message")
  })
  
  # Add new sample service type
  observeEvent(input$add_service_type_btn, {
    req(input$new_service_type, input$new_service_kit, input$new_service_cost)
    
    if(input$new_service_type == "") {
      showNotification("Please enter a sample service type", type = "error")
      return()
    }
    
    if(input$new_service_type %in% admin_data$service_types$service_type) {
      showNotification("This sample service type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO service_types (service_type, kit, costs_per_sample)
      VALUES (?, ?, ?)
    ", params = list(
      input$new_service_type,
      input$new_service_kit,
      input$new_service_cost
    ))
    
    admin_data$service_types <- load_service_types()
    updateTextInput(session, "new_service_type", value = "")
    updateTextInput(session, "new_service_kit", value = "")
    updateNumericInput(session, "new_service_cost", value = 0)
    showNotification("Sample service type added successfully!", type = "message")
  })
  
  # Edit Sample Service Type
  observeEvent(input$edit_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sample service type to edit", type = "warning")
      return()
    }
    
    service_to_edit <- admin_data$service_types[selected_row, ]
    
    showModal(modalDialog(
      title = "Edit Sample Service Type",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_service_type_btn", "Update Sample Service Type", class = "btn-primary")
      ),
      
      textInput("edit_service_type", "Sample Service Type", value = service_to_edit$service_type),
      textInput("edit_service_kit", "Kit", value = service_to_edit$kit),
      numericInput("edit_service_cost", "Cost per Sample", value = service_to_edit$costs_per_sample, min = 0)
    ))
  })

  # Edit Budget Holder
  observeEvent(input$edit_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a budget holder to edit", type = "warning")
      return()
    }

    bh_to_edit <- admin_data$budget_holders[selected_row, ]

    showModal(modalDialog(
      title = "Edit Budget Holder",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_budget_holder_btn", "Update Budget Holder", class = "btn-primary")
      ),
      textInput("edit_bh_name", "Name", value = bh_to_edit$name),
      textInput("edit_bh_surname", "Surname", value = bh_to_edit$surname),
      textInput("edit_bh_cost_center", "Cost Center", value = bh_to_edit$cost_center),
      textInput("edit_bh_email", "Email", value = bh_to_edit$email)
    ))
  })

  observeEvent(input$update_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    if (length(selected_row) == 0) return()

    bh_to_edit <- admin_data$budget_holders[selected_row, ]
    req(input$edit_bh_name, input$edit_bh_surname, input$edit_bh_cost_center, input$edit_bh_email)

    if (trimws(input$edit_bh_name) == "" ||
        trimws(input$edit_bh_surname) == "" ||
        trimws(input$edit_bh_cost_center) == "" ||
        trimws(input$edit_bh_email) == "") {
      showNotification("Please fill in all budget holder fields", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    existing_cc <- dbGetQuery(
      con,
      "SELECT id FROM budget_holders WHERE cost_center = ? AND id != ?",
      params = list(trimws(input$edit_bh_cost_center), bh_to_edit$id)
    )
    if (nrow(existing_cc) > 0) {
      showNotification("Cost center already exists", type = "error")
      return()
    }

    dbExecute(con, "
      UPDATE budget_holders
      SET name = ?, surname = ?, cost_center = ?, email = ?
      WHERE id = ?
    ", params = list(
      trimws(input$edit_bh_name),
      trimws(input$edit_bh_surname),
      trimws(input$edit_bh_cost_center),
      trimws(input$edit_bh_email),
      bh_to_edit$id
    ))

    removeModal()
    admin_data$budget_holders <- load_budget_holders()
    showNotification("Budget holder updated successfully!", type = "message")
  })

  # Edit Reference Genome
  observeEvent(input$edit_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a reference genome to edit", type = "warning")
      return()
    }

    old_name <- admin_data$reference_genomes[selected_row]

    showModal(modalDialog(
      title = "Edit Reference Genome",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_genome_btn", "Update Genome", class = "btn-primary")
      ),
      textInput("edit_genome_name", "Genome Name", value = old_name)
    ))
  })

  observeEvent(input$update_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    if (length(selected_row) == 0) return()

    old_name <- admin_data$reference_genomes[selected_row]
    req(input$edit_genome_name)
    new_name <- trimws(input$edit_genome_name)

    if (new_name == "") {
      showNotification("Please enter a genome name", type = "error")
      return()
    }
    if (new_name %in% admin_data$reference_genomes && new_name != old_name) {
      showNotification("This genome already exists", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    dbExecute(con, "UPDATE reference_genomes SET name = ? WHERE name = ?", params = list(new_name, old_name))

    removeModal()
    admin_data$reference_genomes <- load_reference_genomes()
    showNotification("Reference genome updated successfully!", type = "message")
  })

  # Edit Sample Type
  observeEvent(input$edit_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a sample type to edit", type = "warning")
      return()
    }

    type_to_edit <- admin_data$types[selected_row, ]

    showModal(modalDialog(
      title = "Edit Sample Type",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_type_btn", "Update Sample Type", class = "btn-primary")
      ),
      textInput("edit_type_name", "Sample Type Name", value = type_to_edit$name)
    ))
  })

  observeEvent(input$update_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    if (length(selected_row) == 0) return()

    type_to_edit <- admin_data$types[selected_row, ]
    req(input$edit_type_name)
    new_name <- trimws(input$edit_type_name)

    if (new_name == "") {
      showNotification("Please enter a sample type name", type = "error")
      return()
    }
    if (new_name %in% admin_data$types$name && new_name != type_to_edit$name) {
      showNotification("This type already exists", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    dbExecute(con, "UPDATE types SET name = ? WHERE id = ?", params = list(new_name, type_to_edit$id))

    removeModal()
    admin_data$types <- load_types()
    showNotification("Sample type updated successfully!", type = "message")
  })

  # Edit Sequencing Platform
  observeEvent(input$edit_sequencing_platform_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    if (length(selected_row) == 0) {
      showNotification("Please select a platform to edit", type = "warning")
      return()
    }

    plat_to_edit <- admin_data$sequencing_platforms[selected_row, ]

    showModal(modalDialog(
      title = "Edit Sequencing Platform",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_sequencing_platform_update_btn", "Update Platform", class = "btn-primary")
      ),
      textInput("edit_sequencing_platform_name2", "Platform Name", value = plat_to_edit$name)
    ))
  })

  observeEvent(input$update_sequencing_platform_update_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    if (length(selected_row) == 0) return()

    plat_to_edit <- admin_data$sequencing_platforms[selected_row, ]
    req(input$edit_sequencing_platform_name2)
    new_name <- trimws(input$edit_sequencing_platform_name2)

    if (new_name == "") {
      showNotification("Please enter a platform name", type = "error")
      return()
    }
    if (new_name %in% admin_data$sequencing_platforms$name && new_name != plat_to_edit$name) {
      showNotification("This platform already exists", type = "error")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    dbExecute(con, "UPDATE sequencing_platforms SET name = ? WHERE id = ?", params = list(new_name, plat_to_edit$id))

    removeModal()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    showNotification("Platform updated successfully!", type = "message")
  })
  
  # Update Sample Service Type
  observeEvent(input$update_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    service_to_edit <- admin_data$service_types[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
    UPDATE service_types 
    SET service_type = ?, kit = ?, costs_per_sample = ?
    WHERE id = ?
  ", params = list(
    input$edit_service_type,
    input$edit_service_kit,
    input$edit_service_cost,
    service_to_edit$id
  ))
    
    removeModal()
    admin_data$service_types <- load_service_types()
    showNotification("Sample service type updated successfully!", type = "message")
  })
  # Add new sequencing depth
  observeEvent(input$add_sequencing_depth_btn, {
    req(input$new_depth_description)
    
    if(input$new_depth_description == "") {
      showNotification("Please enter a depth description", type = "error")
      return()
    }
    
    if(input$new_depth_description %in% admin_data$sequencing_depths$depth_description) {
      showNotification("This sequencing depth already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO sequencing_depths (depth_description, cost_upto_150_cycles, cost_upto_300_cycles)
      VALUES (?, ?, ?)
    ", params = list(
      input$new_depth_description,
      ifelse(is.na(input$new_depth_cost_150), NULL, input$new_depth_cost_150),
      ifelse(is.na(input$new_depth_cost_300), NULL, input$new_depth_cost_300)
    ))
    
    admin_data$sequencing_depths <- load_sequencing_depths()
    updateTextInput(session, "new_depth_description", value = "")
    updateNumericInput(session, "new_depth_cost_150", value = 0)
    updateNumericInput(session, "new_depth_cost_300", value = 0)
    showNotification("Sequencing depth added successfully!", type = "message")
  })
  
  # Edit Sequencing Depth
  observeEvent(input$edit_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing depth to edit", type = "warning")
      return()
    }
    
    depth_to_edit <- admin_data$sequencing_depths[selected_row, ]
    
    showModal(modalDialog(
      title = "Edit Sequencing Depth",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("update_sequencing_depth_btn", "Update Depth", class = "btn-primary")
      ),
      
      textInput("edit_depth_description", "Depth Description", value = depth_to_edit$depth_description),
      numericInput("edit_depth_cost_150", "Cost up to 150 cycles", value = depth_to_edit$cost_upto_150_cycles, min = 0),
      numericInput("edit_depth_cost_300", "Cost up to 300 cycles", value = depth_to_edit$cost_upto_300_cycles, min = 0)
    ))
  })
  
  # Update Sequencing Depth
  observeEvent(input$update_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    depth_to_edit <- admin_data$sequencing_depths[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
    UPDATE sequencing_depths 
    SET depth_description = ?, cost_upto_150_cycles = ?, cost_upto_300_cycles = ?
    WHERE id = ?
  ", params = list(
    input$edit_depth_description,
    ifelse(is.na(input$edit_depth_cost_150), NULL, input$edit_depth_cost_150),
    ifelse(is.na(input$edit_depth_cost_300), NULL, input$edit_depth_cost_300),
    depth_to_edit$id
  ))
    
    removeModal()
    admin_data$sequencing_depths <- load_sequencing_depths()
    showNotification("Sequencing depth updated successfully!", type = "message")
  })
  
  # Add new sequencing cycles
  observeEvent(input$add_sequencing_cycles_btn, {
    req(input$new_cycles_description)
    
    if(input$new_cycles_description == "") {
      showNotification("Please enter a cycles description", type = "error")
      return()
    }
    
    if(input$new_cycles_description %in% admin_data$sequencing_cycles$cycles_description) {
      showNotification("This sequencing cycle already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO sequencing_cycles (cycles_description)
      VALUES (?)
    ", params = list(input$new_cycles_description))
    
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    updateTextInput(session, "new_cycles_description", value = "")
    showNotification("Sequencing cycles added successfully!", type = "message")
  })
  
  # Add new user (admin)
  observeEvent(input$add_user_admin_btn, {
    req(input$new_user_username, input$new_user_email, input$new_user_password)
    
    if(input$new_user_password != input$new_user_confirm_password) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    if(nchar(input$new_user_password) < 6) {
      showNotification("Password must be at least 6 characters", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    existing_user <- dbGetQuery(con, 
                                "SELECT id FROM users WHERE username = ?", 
                                params = list(input$new_user_username))
    
    if(nrow(existing_user) > 0) {
      showNotification("Username already exists", type = "error")
      return()
    }
    
    dbExecute(con, "
      INSERT INTO users (username, password, email, is_admin)
      VALUES (?, ?, ?, ?)
    ", params = list(
      input$new_user_username,
      digest(input$new_user_password),
      input$new_user_email,
      as.integer(input$new_user_admin)
    ))
    
    updateTextInput(session, "new_user_username", value = "")
    updateTextInput(session, "new_user_email", value = "")
    updateTextInput(session, "new_user_password", value = "")
    updateTextInput(session, "new_user_confirm_password", value = "")
    updateCheckboxInput(session, "new_user_admin", value = FALSE)
    
    load_admin_data()
    showNotification("User added successfully!", type = "message")
  })
  
  # Add new reference genome
  observeEvent(input$add_genome_btn, {
    req(input$new_genome_name)
    
    if(input$new_genome_name == "") {
      showNotification("Please enter a genome name", type = "error")
      return()
    }
    
    if(input$new_genome_name %in% admin_data$reference_genomes) {
      showNotification("This genome already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "
      INSERT INTO reference_genomes (name)
      VALUES (?)
    ", params = list(input$new_genome_name))
    
    admin_data$reference_genomes <- load_reference_genomes()
    updateTextInput(session, "new_genome_name", value = "")
    showNotification("Reference genome added successfully!", type = "message")
  })
  
  # Add new type
  observeEvent(input$add_type_btn, {
    req(input$new_type_name)
    
    if(input$new_type_name == "") {
      showNotification("Please enter a sample type name", type = "error")
      return()
    }
    
    if(input$new_type_name %in% admin_data$types$name) {
      showNotification("This type already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO types (name) VALUES (?)", params = list(input$new_type_name))
    admin_data$types <- load_types()
    updateTextInput(session, "new_type_name", value = "")
    showNotification("Sample type added successfully!", type = "message")
  })
  
  # Add new sequencing platform
  observeEvent(input$add_sequencing_platform_btn, {
    req(input$new_sequencing_platform_name)
    
    if(input$new_sequencing_platform_name == "") {
      showNotification("Please enter a sequencing platform name", type = "error")
      return()
    }
    
    if(input$new_sequencing_platform_name %in% admin_data$sequencing_platforms$name) {
      showNotification("This platform already exists", type = "error")
      return()
    }
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "INSERT INTO sequencing_platforms (name) VALUES (?)", params = list(input$new_sequencing_platform_name))
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    updateTextInput(session, "new_sequencing_platform_name", value = "")
    showNotification("Sequencing platform added successfully!", type = "message")
  })
  
  # === DELETE HANDLERS ===
  
  # Delete budget holder
  observeEvent(input$delete_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a budget holder to delete", type = "warning")
      return()
    }
    
    bh_to_delete <- admin_data$budget_holders[selected_row, ]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete budget holder:", bh_to_delete$name, bh_to_delete$surname, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_budget_holder_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_budget_holder_btn, {
    selected_row <- input$budget_holders_table_admin_rows_selected
    bh_to_delete <- admin_data$budget_holders[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM budget_holders WHERE id = ?", params = list(bh_to_delete$id))
    
    removeModal()
    load_admin_data()
    showNotification("Budget holder deleted successfully!", type = "message")
  })
  
  # Delete sample service type
  observeEvent(input$delete_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sample service type to delete", type = "warning")
      return()
    }
    
    service_to_delete <- admin_data$service_types[selected_row, "service_type"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sample service type:", service_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_service_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_service_type_btn, {
    selected_row <- input$service_types_table_admin_rows_selected
    service_to_delete <- admin_data$service_types[selected_row, "service_type"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM service_types WHERE service_type = ?", params = list(service_to_delete))
    
    removeModal()
    admin_data$service_types <- load_service_types()
    showNotification("Sample service type deleted successfully!", type = "message")
  })
  
  # Delete sequencing depth
  observeEvent(input$delete_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing depth to delete", type = "warning")
      return()
    }
    
    depth_to_delete <- admin_data$sequencing_depths[selected_row, "depth_description"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing depth:", depth_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_depth_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_depth_btn, {
    selected_row <- input$sequencing_depths_table_admin_rows_selected
    depth_to_delete <- admin_data$sequencing_depths[selected_row, "depth_description"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_depths WHERE depth_description = ?", params = list(depth_to_delete))
    
    removeModal()
    admin_data$sequencing_depths <- load_sequencing_depths()
    showNotification("Sequencing depth deleted successfully!", type = "message")
  })
  
  # Delete sequencing cycles
  observeEvent(input$delete_sequencing_cycles_btn, {
    selected_row <- input$sequencing_cycles_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing cycle to delete", type = "warning")
      return()
    }
    
    cycles_to_delete <- admin_data$sequencing_cycles[selected_row, "cycles_description"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing cycle:", cycles_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_cycles_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_cycles_btn, {
    selected_row <- input$sequencing_cycles_table_admin_rows_selected
    cycles_to_delete <- admin_data$sequencing_cycles[selected_row, "cycles_description"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_cycles WHERE cycles_description = ?", params = list(cycles_to_delete))
    
    removeModal()
    admin_data$sequencing_cycles <- load_sequencing_cycles()
    showNotification("Sequencing cycles deleted successfully!", type = "message")
  })
  
  # Delete user
  observeEvent(input$delete_user_admin_btn, {
    selected_row <- input$users_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a user to delete", type = "warning")
      return()
    }
    
    user_to_delete <- admin_data$users[selected_row, ]
    
    if(user_to_delete$username == user$username) {
      showNotification("You cannot delete your own account", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete user:", user_to_delete$username, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_user_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_user_btn, {
    selected_row <- input$users_table_admin_rows_selected
    user_to_delete <- admin_data$users[selected_row, ]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM users WHERE id = ?", params = list(user_to_delete$id))
    
    removeModal()
    load_admin_data()
    showNotification("User deleted successfully!", type = "message")
  })
  
  # Delete reference genome
  observeEvent(input$delete_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a reference genome to delete", type = "warning")
      return()
    }
    
    genome_to_delete <- admin_data$reference_genomes[selected_row]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete reference genome:", genome_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_genome_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_genome_btn, {
    selected_row <- input$genomes_table_admin_rows_selected
    genome_to_delete <- admin_data$reference_genomes[selected_row]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM reference_genomes WHERE name = ?", params = list(genome_to_delete))
    
    removeModal()
    admin_data$reference_genomes <- load_reference_genomes()
    showNotification("Reference genome deleted successfully!", type = "message")
  })
  
  # Delete type
  observeEvent(input$delete_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sample type to delete", type = "warning")
      return()
    }
    
    type_to_delete <- admin_data$types[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sample type:", type_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_type_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_type_btn, {
    selected_row <- input$types_table_admin_rows_selected
    type_to_delete <- admin_data$types[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM types WHERE name = ?", params = list(type_to_delete))
    
    removeModal()
    admin_data$types <- load_types()
    showNotification("Sample type deleted successfully!", type = "message")
  })
  
  # Delete sequencing platform
  observeEvent(input$delete_sequencing_platform_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a sequencing platform to delete", type = "warning")
      return()
    }
    
    platform_to_delete <- admin_data$sequencing_platforms[selected_row, "name"]
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete sequencing platform:", platform_to_delete, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_sequencing_platform_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_sequencing_platform_btn, {
    selected_row <- input$sequencing_platforms_table_admin_rows_selected
    platform_to_delete <- admin_data$sequencing_platforms[selected_row, "name"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM sequencing_platforms WHERE name = ?", params = list(platform_to_delete))
    
    removeModal()
    admin_data$sequencing_platforms <- load_sequencing_platforms()
    showNotification("Sequencing platform deleted successfully!", type = "message")
  })
  
  # === EDIT PROJECT FUNCTIONALITY ===
  
  # Edit project modal
  observeEvent(input$edit_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to edit", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    can_edit <- user$is_admin ||
      project$user_id == user$user_id ||
      is_responsible_for_user(project$responsible_user, user$username, user$full_name)
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }

    project_additional <- suppressWarnings(as.numeric(project$additional_cost[[1]]))
    if (length(project_additional) == 0 || is.na(project_additional)) {
      project_additional <- NA_real_
    }
    edit_existing_additional_cost(project_additional)

    current_responsible <- trimws(as.character(project$responsible_user[[1]] %||% ""))
    edit_responsible_choices <- get_responsible_user_choices(selected_legacy = current_responsible)
    edit_responsible_selected <- current_responsible

    if (nzchar(current_responsible)) {
      con_responsible <- get_db_connection()
      on.exit(dbDisconnect(con_responsible), add = TRUE)
      users_responsible <- get_responsible_users(con_responsible)
      resolved_current <- resolve_responsible_username(current_responsible, users_responsible)
      if (!is.na(resolved_current) && nzchar(resolved_current)) {
        edit_responsible_selected <- resolved_current
      }
    }
    
    showModal(modalDialog(
      title = "Edit Project",
      size = "l",
      footer = tagList(
        modalButton("Cancel"),
        if (user$is_admin) {
          actionButton(
            "send_notification_btn",
            "Notifications",
            style = "background-color: #a1d99b; border-color: #8bcf84; color: #1b4332;"
          )
        },
        actionButton("update_project_btn", "Update Project", class = "btn-primary")
      ),
      
      fluidRow(
        column(6,
               textInput("edit_project_name", "Project Name *", value = project$project_name),
               numericInput("edit_num_samples", "Number of Samples *", value = project$num_samples, min = 1),
               selectInput("edit_sequencing_platform", "Sequencing Platform *",
                           choices = admin_data$sequencing_platforms$name,
                           selected = project$sequencing_platform),
               selectInput("edit_type_id", "Sample Type *",
                           choices = setNames(admin_data$types$id, admin_data$types$name),
                           selected = project$type_id),
               selectInput("edit_service_type_id", "Sample Service Type *",
                           choices = setNames(admin_data$service_types$id, 
                                              paste(admin_data$service_types$service_type, 
                                                    "- €", admin_data$service_types$costs_per_sample, "/sample")),
                           selected = project$service_type_id)
        ),
        column(6,
               selectInput("edit_reference_genome", "Reference Genome *", 
                           choices = admin_data$reference_genomes,
                           selected = project$reference_genome),
               selectInput("edit_sequencing_depth_id", "Data Amount (Total Reads) *",
                           choices = setNames(admin_data$sequencing_depths$id, 
                                              admin_data$sequencing_depths$depth_description),
                           selected = project$sequencing_depth_id),
               div(style = "font-size: 11px;", helpText("Estimated total sequencing output for this project (e.g., up to 200M / 400M / 800M reads).")),
               selectInput("edit_sequencing_cycles_id", "Read Length (Cycles) *",
                           choices = setNames(admin_data$sequencing_cycles$id, 
                                              admin_data$sequencing_cycles$cycles_description),
                           selected = project$sequencing_cycles_id),
               div(style = "font-size: 11px;", helpText("Read setup, e.g., 2x75 (up to 150 cycles) or 2x150 (up to 300 cycles).")),
               selectInput("edit_budget_id", "Budget Holder *",
                           choices = c(
                             setNames(as.character(admin_data$budget_holders$id),
                                      paste(admin_data$budget_holders$name,
                                            admin_data$budget_holders$surname,
                                            "-", admin_data$budget_holders$cost_center)),
                             "Other / not listed" = "other"
                           ),
                           selected = as.character(project$budget_id)),
               uiOutput("edit_budget_holder_notice"),
               uiOutput("edit_budget_holder_other_fields"),
               selectInput("edit_responsible_user", "Responsible User *",
                           choices = edit_responsible_choices,
                           selected = edit_responsible_selected),
               radioButtons("edit_kickoff_meeting", "Kick-off Meeting Required? *",
                            choices = c("Yes" = 1, "No" = 0),
                            selected = {
                              kickoff_selected <- suppressWarnings(as.numeric(project$kickoff_meeting[[1]]))
                              if (length(kickoff_selected) == 0 || is.na(kickoff_selected) || !(kickoff_selected %in% c(0, 1))) {
                                "0"
                              } else {
                                as.character(kickoff_selected)
                              }
                            }, inline = TRUE)
        )
      ),
      fluidRow(
        column(12,
               textAreaInput("edit_project_description", "Project Description", 
                             value = project$description, rows = 4)
        )
      ),
      if (user$is_admin) {
        fluidRow(
          column(12,
                 textInput(
                   "edit_additional_cost",
                   "Additional costs (€)",
                   value = if (!is.na(project_additional)) sprintf("%.2f", project_additional) else "",
                   placeholder = "Leave empty if no additional costs"
                 )
          )
        )
      },
      # Cost calculation for edit
      fluidRow(
        column(12,
               div(class = "cost-calculation",
                   h4("Cost Calculation"),
                   uiOutput("edit_cost_calculation_display")
               )
        )
      ),
      if(user$is_admin) {
        fluidRow(
          column(12,
                selectInput("edit_project_status", "Project Status *",
                            choices = make_status_choices(project$status),
                            selected = project$status)
          )
        )
      }
    ))
  })
  
  # Edit cost calculation
  output$edit_cost_calculation_display <- renderUI({
    base_cost <- calculate_base_cost(input$edit_num_samples, input$edit_service_type_id,
                                     input$edit_sequencing_depth_id, input$edit_sequencing_cycles_id)
    
    service_type <- admin_data$service_types[admin_data$service_types$id == as.numeric(input$edit_service_type_id), ]
    sequencing_depth <- admin_data$sequencing_depths[admin_data$sequencing_depths$id == as.numeric(input$edit_sequencing_depth_id), ]
    
    prep_cost <- if(nrow(service_type) > 0) service_type$costs_per_sample * as.numeric(input$edit_num_samples) else 0
    sequencing_cost <- base_cost - prep_cost

    stored_additional <- edit_existing_additional_cost()
    additional_source <- if (isTRUE(user$is_admin)) {
      input$edit_additional_cost
    } else if (is.na(stored_additional)) {
      ""
    } else {
      as.character(stored_additional)
    }
    parsed_additional <- parse_additional_cost(additional_source)
    additional_cost <- if (parsed_additional$valid && !parsed_additional$empty) {
      parsed_additional$value
    } else if (!isTRUE(user$is_admin) && !is.na(edit_existing_additional_cost())) {
      edit_existing_additional_cost()
    } else {
      0
    }
    total_cost <- calculate_total_cost(
      input$edit_num_samples,
      input$edit_service_type_id,
      input$edit_sequencing_depth_id,
      input$edit_sequencing_cycles_id,
      additional_cost
    )

    additional_line <- if (parsed_additional$valid && !parsed_additional$empty) {
      paste0("Additional costs: €", format_cost_amount(parsed_additional$value))
    } else {
      "Additional costs:"
    }

    tagList(
      p(paste0("Preparation Cost: €", format_cost_amount(prep_cost))),
      p(paste0("Sequencing Cost: €", format_cost_amount(sequencing_cost))),
      p(additional_line),
      p(class = "cost-total", paste0("Total Estimated Cost: €", format_cost_amount(total_cost))),
      if (isTRUE(user$is_admin) && !parsed_additional$valid) {
        p(class = "cost-warning", parsed_additional$error)
      },
      if(!is.null(sequencing_depth) && nrow(sequencing_depth) > 0 && sequencing_depth$depth_description == "other") {
        p(class = "cost-warning", 
          "Note: 'Other' sequencing depth selected. These costs are preliminary. Please contact us to discuss your specific needs.")
      }
    )
  })
  
  # Update project
  observeEvent(input$update_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) {
      showNotification("Please select a project to edit", type = "warning")
      return()
    }

    projects <- projects_data()
    if (is.null(projects) || nrow(projects) < selected_row[1]) {
      showNotification("Selected project is no longer available. Please reload the table.", type = "warning")
      return()
    }

    project <- projects[selected_row[1], , drop = FALSE]
    project_id <- project$id[[1]]
    
    can_edit <- user$is_admin ||
      project$user_id == user$user_id ||
      is_responsible_for_user(project$responsible_user, user$username, user$full_name)
    
    if(!can_edit) {
      showNotification("You don't have permission to edit this project", type = "error")
      return()
    }
    
    if (input$edit_budget_id == "other") {
      if (is.null(input$edit_other_bh_name) || input$edit_other_bh_name == "" ||
          is.null(input$edit_other_bh_surname) || input$edit_other_bh_surname == "") {
        showNotification("Please enter PI name and surname for the new budget holder.", type = "error")
        return()
      }
    }

    # Normalize scalar values before DB bind (prevents length-0/length>1 parameter errors)
    scalar_text <- function(x, default = "") {
      if (is.null(x) || length(x) == 0) return(default)
      value <- as.character(x[[1]])
      if (is.na(value)) return(default)
      value
    }

    scalar_numeric <- function(x, default = NA_real_) {
      value <- suppressWarnings(as.numeric(scalar_text(x, "")))
      if (length(value) == 0 || is.na(value)) return(default)
      value[[1]]
    }

    old_status <- scalar_text(project$status, "")
    new_status_for_update <- scalar_text(input$edit_project_status, default = scalar_text(project$status, "Created"))
    should_send_data_released_email <- user$is_admin &&
      old_status != "Data released" &&
      new_status_for_update == "Data released"

    kickoff_default <- suppressWarnings(as.numeric(project$kickoff_meeting[[1]]))
    if (length(kickoff_default) == 0 || is.na(kickoff_default) || !(kickoff_default %in% c(0, 1))) {
      kickoff_default <- 0
    }
    kickoff_value <- scalar_numeric(input$edit_kickoff_meeting, kickoff_default)

    existing_additional_cost <- suppressWarnings(as.numeric(project$additional_cost[[1]]))
    if (length(existing_additional_cost) == 0 || is.na(existing_additional_cost)) {
      existing_additional_cost <- NA_real_
    }

    additional_cost_value <- existing_additional_cost
    if (user$is_admin) {
      parsed_additional <- parse_additional_cost(input$edit_additional_cost)
      if (!parsed_additional$valid) {
        showNotification(parsed_additional$error, type = "error")
        return()
      }
      additional_cost_value <- if (parsed_additional$empty) NA_real_ else parsed_additional$value
    }

    # Calculate total cost (base + additional costs if present)
    total_cost <- calculate_total_cost(
      input$edit_num_samples,
      input$edit_service_type_id,
      input$edit_sequencing_depth_id,
      input$edit_sequencing_cycles_id,
      additional_cost_value
    )
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    budget_id_value <- input$edit_budget_id
    if (budget_id_value == "other") {
      bh_cost_center <- trimws(input$edit_other_bh_cost_center %||% "")
      if (bh_cost_center == "") bh_cost_center <- "N.A."
      bh_email <- trimws(input$edit_other_bh_email %||% "")
      if (bh_email == "") bh_email <- "ngs@biochem.mpg.de"

      budget_id_value <- get_or_create_budget_holder(
        con,
        input$edit_other_bh_name,
        input$edit_other_bh_surname,
        bh_cost_center,
        bh_email
      )
    }

    responsible_user_value <- resolve_responsible_for_storage(
      input$edit_responsible_user,
      con,
      fallback = scalar_text(project$responsible_user, "")
    )
    
    if(user$is_admin) {
      dbExecute(con, "
        UPDATE projects 
        SET project_name = ?, reference_genome = ?, service_type_id = ?, 
            budget_id = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
            num_samples = ?, sequencing_platform = ?, sequencing_depth_id = ?,
            sequencing_cycles_id = ?, kickoff_meeting = ?, status = ?,
            type_id = ?, additional_cost = ?, total_cost = ?
        WHERE id = ?
      ", params = list(
        scalar_text(input$edit_project_name),
        scalar_text(input$edit_reference_genome),
        as.numeric(input$edit_service_type_id),
        as.numeric(budget_id_value),
        responsible_user_value,
        scalar_text(input$edit_project_description),
        input$edit_num_samples,
        scalar_text(input$edit_sequencing_platform),
        as.numeric(input$edit_sequencing_depth_id),
        as.numeric(input$edit_sequencing_cycles_id),
        kickoff_value,
        new_status_for_update,
        as.numeric(input$edit_type_id),
        additional_cost_value,
        total_cost,
        project_id
      ))
    } else {
      dbExecute(con, "
        UPDATE projects 
        SET project_name = ?, reference_genome = ?, service_type_id = ?, 
            budget_id = ?, responsible_user = ?, description = ?, updated_at = CURRENT_TIMESTAMP,
            num_samples = ?, sequencing_platform = ?, sequencing_depth_id = ?,
            sequencing_cycles_id = ?, kickoff_meeting = ?,
            type_id = ?, total_cost = ?
        WHERE id = ?
      ", params = list(
        scalar_text(input$edit_project_name),
        scalar_text(input$edit_reference_genome),
        as.numeric(input$edit_service_type_id),
        as.numeric(budget_id_value),
        responsible_user_value,
        scalar_text(input$edit_project_description),
        input$edit_num_samples,
        scalar_text(input$edit_sequencing_platform),
        as.numeric(input$edit_sequencing_depth_id),
        as.numeric(input$edit_sequencing_cycles_id),
        kickoff_value,
        as.numeric(input$edit_type_id),
        total_cost,
        project_id
      ))
    }
    
    removeModal()
    load_projects()
    edit_existing_additional_cost(additional_cost_value)
    showNotification("Project updated successfully!", type = "message")

    if (should_send_data_released_email) {
      project_code <- ""
      if (!is.null(project$project_id[[1]]) && !is.na(project$project_id[[1]])) {
        project_code <- paste0("P", as.character(project$project_id[[1]]))
      }

      email_result <- send_data_released_email(list(
        project_id = project$project_id[[1]],
        project_code = project_code,
        responsible_user = responsible_user_value,
        user_id = project$user_id[[1]]
      ))

      if (isTRUE(email_result$success)) {
        showNotification("Data released email sent to responsible user and omicsdesk.", type = "message")
      } else {
        showNotification(
          paste("Status updated, but data released email failed:", email_result$error %||% "unknown error"),
          type = "warning",
          duration = 10
        )
      }
    }
  })

  observeEvent(input$send_notification_btn, {
    if (!user$is_admin) {
      showNotification("Only administrators can send cost notification emails.", type = "error")
      return()
    }

    selected_row <- input$projects_table_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) {
      showNotification("Please select a project first.", type = "warning")
      return()
    }

    projects <- projects_data()
    if (is.null(projects) || nrow(projects) < selected_row[1]) {
      showNotification("Selected project is no longer available. Please reload the table.", type = "warning")
      return()
    }

    project <- projects[selected_row[1], , drop = FALSE]

    parsed_additional <- parse_additional_cost(input$edit_additional_cost)
    if (!parsed_additional$valid) {
      showNotification(parsed_additional$error, type = "error")
      return()
    }
    additional_cost_value <- if (parsed_additional$empty) NA_real_ else parsed_additional$value

    total_cost <- calculate_total_cost(
      input$edit_num_samples,
      input$edit_service_type_id,
      input$edit_sequencing_depth_id,
      input$edit_sequencing_cycles_id,
      additional_cost_value
    )

    con <- get_db_connection()
    on.exit(dbDisconnect(con))

    budget_holder <- data.frame()

    selected_budget_id <- suppressWarnings(as.numeric(input$edit_budget_id))
    if (!is.na(selected_budget_id)) {
      bh_row <- dbGetQuery(
        con,
        "SELECT name, surname, cost_center, email FROM budget_holders WHERE id = ? LIMIT 1",
        params = list(selected_budget_id)
      )
      if (nrow(bh_row) > 0) budget_holder <- bh_row
    }

    if (nrow(budget_holder) == 0 && !is.null(project$budget_id[[1]]) && !is.na(project$budget_id[[1]])) {
      bh_row <- dbGetQuery(
        con,
        "SELECT name, surname, cost_center, email FROM budget_holders WHERE id = ? LIMIT 1",
        params = list(as.numeric(project$budget_id[[1]]))
      )
      if (nrow(bh_row) > 0) budget_holder <- bh_row
    }

    if (nrow(budget_holder) == 0) {
      budget_holder <- data.frame(
        name = "NGS",
        surname = "Facility",
        cost_center = "N.A.",
        email = "ngs@biochem.mpg.de",
        stringsAsFactors = FALSE
      )
    }

    service_type_label <- admin_data$service_types$service_type[
      admin_data$service_types$id == as.numeric(input$edit_service_type_id)
    ]
    if (length(service_type_label) == 0) {
      service_type_label <- admin_data$service_types$service_type[
        admin_data$service_types$id == as.numeric(project$service_type_id[[1]])
      ]
    }
    if (length(service_type_label) == 0) service_type_label <- "N.A."

    project_id_value <- suppressWarnings(as.numeric(project$project_id[[1]]))
    project_code <- if (!is.na(project_id_value)) paste0("P", as.integer(project_id_value)) else "Project"
    responsible_user_value <- resolve_responsible_for_storage(
      input$edit_responsible_user %||% project$responsible_user[[1]],
      con,
      fallback = project$responsible_user[[1]]
    )
    users_responsible <- get_responsible_users(con)
    resolved_username <- resolve_responsible_username(responsible_user_value, users_responsible)
    responsible_email <- ""

    if (!is.na(resolved_username) && nzchar(resolved_username)) {
      responsible_email_row <- dbGetQuery(
        con,
        "SELECT email FROM users WHERE lower(username) = lower(?) LIMIT 1",
        params = list(resolved_username)
      )
      if (nrow(responsible_email_row) > 0) {
        responsible_email <- trimws(as.character(responsible_email_row$email[[1]] %||% ""))
      }
    }

    if (!nzchar(responsible_email) && nzchar(responsible_user_value)) {
      full_name_matches <- dbGetQuery(
        con,
        "
        SELECT email
        FROM users
        WHERE lower(trim(full_name)) = lower(trim(?))
        ",
        params = list(responsible_user_value)
      )
      if (nrow(full_name_matches) == 1) {
        responsible_email <- trimws(as.character(full_name_matches$email[[1]] %||% ""))
      }
    }

    # If responsible_user is stored as a direct email (legacy/manual entry), use it.
    if (!nzchar(responsible_email) &&
        grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", responsible_user_value)) {
      responsible_email <- responsible_user_value
    }

    notification_payload <- list(
      project_id = project_id_value,
      project_code = project_code,
      project_name = trimws(input$edit_project_name %||% project$project_name[[1]]),
      description = trimws(input$edit_project_description %||% project$description[[1]]),
      responsible_user = responsible_user_value,
      num_samples = input$edit_num_samples %||% project$num_samples[[1]],
      sequencing_platform = trimws(input$edit_sequencing_platform %||% project$sequencing_platform[[1]]),
      service_type = service_type_label[[1]],
      total_cost = total_cost
    )

    email_result <- send_project_cost_notification_email(notification_payload, budget_holder, responsible_email)

    if (isTRUE(email_result$success)) {
      showNotification("Cost estimation notification sent.", type = "message")
    } else {
      showNotification(
        paste("Could not send notification email:", email_result$error %||% "unknown error"),
        type = "error",
        duration = 10
      )
    }
  })
  
  # === DELETE PROJECT ===
  
  observeEvent(input$delete_project_btn, {
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to delete", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    can_delete <- user$is_admin
    
    if(!can_delete) {
      showNotification("Only administrators can delete projects", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Delete",
      paste("Are you sure you want to delete project:", project$project_name, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_btn", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete_btn, {
    selected_row <- input$projects_table_rows_selected
    project_id <- projects_data()[selected_row, "id"]
    
    con <- get_db_connection()
    on.exit(dbDisconnect(con))
    
    dbExecute(con, "DELETE FROM projects WHERE id = ?", params = list(project_id))
    
    removeModal()
    load_projects()
    showNotification("Project deleted successfully!", type = "message")
  })
  
  # === UPDATE STATUS ===
  
  observeEvent(input$update_status_btn, {
    if(!user$is_admin) {
      showNotification("Only administrators can update project status", type = "error")
      return()
    }
    
    selected_row <- input$projects_table_rows_selected
    if(length(selected_row) == 0) {
      showNotification("Please select a project to update status", type = "warning")
      return()
    }
    
    project <- projects_data()[selected_row, ]
    
    showModal(modalDialog(
      title = "Update Project Status",
      size = "m",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_status_update_btn", "Update Status", class = "btn-primary")
      ),
      
      h4(paste("Project:", project$project_name)),
      p(paste("Current Status:", project$status)),
      
      selectInput("new_project_status", "New Status:",
                  choices = make_status_choices(project$status),
                  selected = project$status),
      
      textAreaInput("status_notes", "Status Notes (Optional):", 
                    placeholder = "Add any notes about this status change...",
                    rows = 3)
    ))
  })
  
  observeEvent(input$confirm_status_update_btn, {
    selected_row <- input$projects_table_rows_selected
    if (is.null(selected_row) || length(selected_row) == 0) {
      showNotification("Please select a project before updating status", type = "warning")
      return()
    }

    projects <- projects_data()
    if (is.null(projects) || nrow(projects) < selected_row[1]) {
      showNotification("Selected project is no longer available. Please reload the table.", type = "warning")
      return()
    }

    project <- projects[selected_row[1], , drop = FALSE]
    project_id <- project$id[[1]]
    new_status <- input$new_project_status %||% ""
    old_status <- as.character(project$status[[1]] %||% "")

    if (is.null(new_status) || !nzchar(new_status)) {
      showNotification("Please choose a valid status", type = "warning")
      return()
    }

    con <- get_db_connection()
    on.exit(dbDisconnect(con), add = TRUE)

    tryCatch({
      dbExecute(con, "
        UPDATE projects
        SET status = ?, updated_at = CURRENT_TIMESTAMP
        WHERE id = ?
      ", params = list(
        new_status,
        project_id
      ))

      removeModal()
      load_projects()
      showNotification("Project status updated successfully!", type = "message")

      should_send_data_released_email <- old_status != "Data released" && new_status == "Data released"
      if (should_send_data_released_email) {
        project_code <- ""
        if (!is.null(project$project_id[[1]]) && !is.na(project$project_id[[1]])) {
          project_code <- paste0("P", as.character(project$project_id[[1]]))
        }

        email_result <- send_data_released_email(list(
          project_id = project$project_id[[1]],
          project_code = project_code,
          responsible_user = as.character(project$responsible_user[[1]] %||% ""),
          user_id = project$user_id[[1]]
        ))

        if (isTRUE(email_result$success)) {
          showNotification("Data released email sent to responsible user and omicsdesk.", type = "message")
        } else {
          showNotification(
            paste("Status updated, but data released email failed:", email_result$error %||% "unknown error"),
            type = "warning",
            duration = 10
          )
        }
      }
    }, error = function(e) {
      cat("STATUS UPDATE ERROR:", conditionMessage(e), "\n", file = stderr())
      showNotification(
        paste("Failed to update project status:", conditionMessage(e)),
        type = "error",
        duration = 10
      )
    })
  })
  
  ##################################################################
  # DATABASE BACKUP/RESTORE HANDLERS
  ##################################################################
  
  # Database validation button
  observeEvent(input$validate_db_btn, {
    if (validate_and_repair_database()) {
      showNotification("Database validation successful!", type = "message")
    }
  })
  
  # Enhanced backup handler
  output$download_db_btn <- downloadHandler(
    filename = function() {
      paste0("sequencing_db_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sqlite")
    },
    content = function(file) {
      showNotification("Creating database backup...", type = "message", duration = NULL)
      
      tryCatch({
        if (!file.exists("sequencing_projects.db")) {
          stop("Database file not found")
        }
        
        # Create backup
        file.copy("sequencing_projects.db", file)
        
        # Log the backup
        con <- get_db_connection()
        on.exit(dbDisconnect(con))
        
        file_info <- file.info("sequencing_projects.db")
        dbExecute(con, "
          INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
          VALUES (?, ?, ?)
        ", params = list(
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          file_info$size,
          user$username
        ))
        
        showNotification("Backup created successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Backup failed:", e$message), type = "error")
      })
    },
    contentType = "application/x-sqlite3"
  )
  
  # Restore handler
  observeEvent(input$restore_db_btn, {
    req(input$restore_db_btn)
    
    showModal(modalDialog(
      title = "Confirm Database Restore",
      tagList(
        p("WARNING: This will replace the current database with the backup file."),
        p("All current data will be lost!"),
        p("Backup file:", input$restore_db_btn$name),
        br(),
        p("Are you sure you want to continue?")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_restore_btn", "Restore Database", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_restore_btn, {
    tryCatch({
      # Backup current database first
      if(file.exists("sequencing_projects.db")) {
        backup_name <- paste0("sequencing_projects_pre_restore_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".db")
        file.copy("sequencing_projects.db", backup_name)
      }
      
      # Restore from uploaded file
      file.copy(input$restore_db_btn$datapath, "sequencing_projects.db", overwrite = TRUE)
      
      removeModal()
      showNotification("Database restored successfully! Previous database backed up.", type = "message")
      
      # Reload app
      session$reload()
      
    }, error = function(e) {
      showNotification(paste("Restore failed:", e$message), type = "error")
    })
  })
  
  ##################################################################
  # END DATABASE BACKUP/RESTORE HANDLERS
  ##################################################################
  
  ##################################################################
  # AUTOMATIC BI-WEEKLY BACKUPS (COMMENTED OUT)
  ##################################################################
  #
  # To activate automatic bi-weekly backups, remove the # comments below
  # and adjust the backup directory path if needed.
  #
  # observe({
  #   # Check if backup needed (every 14 days)
  #   invalidateLater(14 * 24 * 60 * 60 * 1000) # 14 days in milliseconds
  #   
  #   backup_dir <- "ngs_project_management_sql_backups"
  #   if (!dir.exists(backup_dir)) dir.create(backup_dir)
  #   
  #   # Create backup filename with bi-weekly indicator
  #   current_date <- Sys.Date()
  #   bi_week <- as.numeric(format(current_date, "%U")) %/% 2  # Bi-weekly indicator
  #   backup_file <- file.path(backup_dir, paste0("backup_", format(current_date, "%Y"), "_biweek_", bi_week, ".sqlite"))
  #   
  #   if (!file.exists(backup_file) && file.exists("sequencing_projects.db")) {
  #     tryCatch({
  #       file.copy("sequencing_projects.db", backup_file)
  #       cat("Automatic bi-weekly backup created:", backup_file, "\n")
  #       
  #       # Log the automatic backup
  #       con <- get_db_connection()
  #       file_info <- file.info("sequencing_projects.db")
  #       dbExecute(con, "
  #         INSERT INTO backup_logs (backup_timestamp, backup_size, backed_up_by)
  #         VALUES (?, ?, ?)
  #       ", params = list(
  #         format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  #         file_info$size,
  #         'auto_backup_system'
  #       ))
  #       dbDisconnect(con)
  #       
  #     }, error = function(e) {
  #       cat("Automatic backup failed:", e$message, "\n")
  #     })
  #   }
  # })
  #
  ##################################################################
  # END AUTOMATIC BI-WEEKLY BACKUPS
  ##################################################################
  
}

# Run the application
shinyApp(ui = ui, server = server)
