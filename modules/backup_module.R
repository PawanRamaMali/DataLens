# modules/backup_module.R
# Backup & Restore Module
#
# Three capabilities:
#   1. Backup to Schema  – copies selected tables into a new timestamped schema
#                          within the same database using CREATE TABLE AS SELECT.
#   2. Export to RDS     – downloads selected tables as a named list saved in
#                          an .rds file (via Shiny downloadHandler).
#   3. Restore           – copies tables from a backup schema back to a target
#                          schema. Supports two modes:
#                            "create"    – CREATE TABLE AS SELECT (fails if exists)
#                            "overwrite" – TRUNCATE + INSERT INTO (wrapped in a
#                                          transaction so the target is never
#                                          left in a partially-restored state).
#
# Safety:
#   - All schema/table names quoted with DBI::dbQuoteIdentifier().
#   - Restore runs inside a DB transaction so failures roll back completely.
#   - Write operations emit a prominent warning in the UI.

BACKUP_PREFIX <- "bkp_"

# ---- Internal helpers --------------------------------------------------------

.is_valid_con <- function(con_rv) {
  conn <- con_rv()
  !is.null(conn) && tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)
}

.default_backup_name <- function(schema) {
  paste0(BACKUP_PREFIX, schema, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

# Append a timestamped line to a log reactiveVal (safe to call anywhere)
.log <- function(log_rv, type = "info", msg) {
  ts   <- format(Sys.time(), "%H:%M:%S")
  pfx  <- switch(type, ok = "[OK]  ", error = "[ERR] ", warn = "[WARN]", "[INFO]")
  entry <- paste0(ts, " ", pfx, " ", msg)
  log_rv(c(isolate(log_rv()), entry))
}

# Render a log reactiveVal as coloured HTML lines
.render_log <- function(log_rv) {
  renderUI({
    entries <- log_rv()
    if (length(entries) == 0) {
      return(p(class = "text-muted fst-italic", "No operations yet."))
    }
    tagList(lapply(entries, function(e) {
      col <- if      (grepl("\\[ERR\\]",  e)) "#dc3545"
             else if (grepl("\\[OK\\]",   e)) "#198754"
             else if (grepl("\\[WARN\\]", e)) "#856404"
             else                             "#495057"
      tags$div(style = paste0("color:", col, "; margin-bottom:2px; white-space:pre;"), e)
    }))
  })
}

# ---- UI ----------------------------------------------------------------------

backupUI <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    # Header
    div(
      class = "row mb-3",
      div(
        class = "col-12",
        h3(class = "d-flex align-items-center gap-2 mb-1",
           icon("hard-drive"), " Backup & Restore"),
        p(class = "text-muted mb-0",
          "Copy table data to a new schema, export to an RDS file,",
          "or restore from a previous backup schema.")
      )
    ),

    # Write-operation warning
    div(
      class = "alert alert-warning d-flex align-items-start gap-2 mb-4",
      icon("triangle-exclamation"),
      div(
        strong("Write operations: "),
        "Backup to schema and Restore both modify the database.",
        " Restore in ", strong("Overwrite"), " mode truncates existing tables.",
        " Always confirm the target environment before proceeding."
      )
    ),

    navset_tab(

      # ==================================================================
      # TAB 1 – BACKUP
      # ==================================================================
      nav_panel(
        title = tagList(icon("cloud-arrow-up"), " Backup"),

        div(
          class = "row g-4 mt-1",

          # ---- Left: configuration ----
          div(
            class = "col-lg-4",
            div(
              class = "card shadow-sm",
              div(class = "card-header fw-semibold",
                  tagList(icon("gear"), " Backup Configuration")),
              div(
                class = "card-body",

                selectInput(ns("bkp_env"), "Source Environment",
                            choices = c("DEV", "UAT", "PROD"), width = "100%"),

                selectInput(ns("bkp_schema"), "Source Schema",
                            choices = c("\u2014 Loading \u2014" = ""),
                            width = "100%"),

                # Tables with Select All / None links
                div(
                  class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label(class = "form-label mb-0 fw-semibold small", "Tables"),
                  div(class = "d-flex gap-1 small",
                      actionLink(ns("bkp_all"),  "All"),
                      span(class = "text-muted", "|"),
                      actionLink(ns("bkp_none"), "None"))
                ),
                selectInput(ns("bkp_tables"), label = NULL,
                            choices  = c("\u2014 Select schema first \u2014" = ""),
                            multiple = TRUE, width = "100%",
                            selectize = FALSE, size = 7),

                hr(),

                # --- Backup to schema ---
                div(
                  class = "card bg-light border-0 mb-3",
                  div(
                    class = "card-body p-3",
                    h6(class = "d-flex align-items-center gap-1 mb-2",
                       icon("database"), " Schema Backup"),
                    textInput(ns("bkp_dest"), "Destination Schema Name",
                              value = "", placeholder = "bkp_schema_YYYYMMDD_HHMMSS",
                              width = "100%"),
                    actionButton(ns("btn_backup_schema"),
                                 label = tagList(icon("cloud-arrow-up"), " Backup to Schema"),
                                 class = "btn btn-primary w-100")
                  )
                ),

                # --- Export to RDS ---
                div(
                  class = "card bg-light border-0",
                  div(
                    class = "card-body p-3",
                    h6(class = "d-flex align-items-center gap-1 mb-2",
                       icon("file-arrow-down"), " RDS Export"),
                    p(class = "small text-muted mb-2",
                      "Downloads all selected tables as a named R list saved in ",
                      code(".rds"), " format."),
                    downloadButton(ns("btn_export_rds"),
                                   label = tagList(icon("download"), " Export to RDS"),
                                   class = "btn btn-info text-white w-100")
                  )
                )
              )
            )
          ),

          # ---- Right: log ----
          div(
            class = "col-lg-8",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-header fw-semibold d-flex align-items-center gap-2",
                icon("terminal"), " Backup Log",
                div(class = "ms-auto",
                    actionButton(ns("bkp_clear"), "Clear",
                                 class = "btn btn-sm btn-outline-secondary"))
              ),
              div(
                class = "card-body p-0",
                div(
                  id    = ns("bkp_log_box"),
                  class = "p-3",
                  style = "height:430px; overflow-y:auto; font-family:monospace; font-size:0.82rem;",
                  uiOutput(ns("bkp_log_ui"))
                )
              )
            )
          )
        )
      ),

      # ==================================================================
      # TAB 2 – RESTORE
      # ==================================================================
      nav_panel(
        title = tagList(icon("cloud-arrow-down"), " Restore"),

        div(
          class = "row g-4 mt-1",

          # ---- Left: configuration ----
          div(
            class = "col-lg-4",
            div(
              class = "card shadow-sm",
              div(class = "card-header fw-semibold",
                  tagList(icon("gear"), " Restore Configuration")),
              div(
                class = "card-body",

                selectInput(ns("rst_env"), "Environment",
                            choices = c("DEV", "UAT", "PROD"), width = "100%"),

                div(
                  class = "d-flex gap-2 align-items-end mb-3",
                  div(
                    class = "flex-grow-1",
                    selectInput(ns("rst_backup_schema"), "Backup Schema (source)",
                                choices = c("\u2014 Loading \u2014" = ""),
                                width = "100%")
                  ),
                  actionButton(ns("btn_refresh_rst"), tagList(icon("rotate")),
                               class = "btn btn-outline-secondary mb-0",
                               title = "Refresh schema list")
                ),

                selectInput(ns("rst_target_schema"), "Target Schema (destination)",
                            choices = c("\u2014 Loading \u2014" = ""),
                            width = "100%"),

                # Tables
                div(
                  class = "d-flex justify-content-between align-items-center mb-1",
                  tags$label(class = "form-label mb-0 fw-semibold small", "Tables to Restore"),
                  div(class = "d-flex gap-1 small",
                      actionLink(ns("rst_all"),  "All"),
                      span(class = "text-muted", "|"),
                      actionLink(ns("rst_none"), "None"))
                ),
                selectInput(ns("rst_tables"), label = NULL,
                            choices  = c("\u2014 Select backup schema \u2014" = ""),
                            multiple = TRUE, width = "100%",
                            selectize = FALSE, size = 6),

                hr(),

                # Restore mode
                div(
                  class = "card bg-light border-0 mb-3",
                  div(
                    class = "card-body p-2",
                    tags$label(class = "form-label fw-semibold small mb-2", "Restore Mode"),
                    radioButtons(
                      ns("rst_mode"), label = NULL,
                      choices = c(
                        "Create new tables (safe \u2014 fails if table exists)" = "create",
                        "Overwrite existing (TRUNCATE + INSERT, transactional)"  = "overwrite"
                      ),
                      selected = "create"
                    )
                  )
                ),

                div(
                  class = "d-grid",
                  actionButton(ns("btn_restore"),
                               label = tagList(icon("rotate-left"), " Restore"),
                               class = "btn btn-danger")
                )
              )
            )
          ),

          # ---- Right: log ----
          div(
            class = "col-lg-8",
            div(
              class = "card shadow-sm h-100",
              div(
                class = "card-header fw-semibold d-flex align-items-center gap-2",
                icon("terminal"), " Restore Log",
                div(class = "ms-auto",
                    actionButton(ns("rst_clear"), "Clear",
                                 class = "btn btn-sm btn-outline-secondary"))
              ),
              div(
                class = "card-body p-0",
                div(
                  id    = ns("rst_log_box"),
                  class = "p-3",
                  style = "height:430px; overflow-y:auto; font-family:monospace; font-size:0.82rem;",
                  uiOutput(ns("rst_log_ui"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# ---- Server ------------------------------------------------------------------

# connections: named list of reactiveVals — list(DEV = dev_con, UAT = uat_con, PROD = prod_con)
backupServer <- function(id, connections) {
  moduleServer(id, function(input, output, session) {

    # Helpers
    get_con   <- function(env) connections[[env]]
    is_valid  <- function(env) .is_valid_con(get_con(env))
    conn_of   <- function(env) get_con(env)()

    # All three connection reactives, used as bindEvent triggers
    all_cons <- function() lapply(connections, function(rv) rv())

    # ---- Logs ---------------------------------------------------------------

    bkp_log <- reactiveVal(character(0))
    rst_log <- reactiveVal(character(0))

    output$bkp_log_ui <- .render_log(bkp_log)
    output$rst_log_ui <- .render_log(rst_log)

    observeEvent(input$bkp_clear, bkp_log(character(0)))
    observeEvent(input$rst_clear, rst_log(character(0)))

    # Auto-scroll log box to bottom after each update
    observe({
      bkp_log()
      shinyjs::runjs(sprintf(
        'var el = document.getElementById("%s"); if(el) el.scrollTop = el.scrollHeight;',
        session$ns("bkp_log_box")
      ))
    })
    observe({
      rst_log()
      shinyjs::runjs(sprintf(
        'var el = document.getElementById("%s"); if(el) el.scrollTop = el.scrollHeight;',
        session$ns("rst_log_box")
      ))
    })

    # =========================================================================
    # BACKUP TAB
    # =========================================================================

    # Load schemas when environment changes or connection changes
    observe({
      env <- input$bkp_env
      if (!is_valid(env)) {
        updateSelectInput(session, "bkp_schema",
                          choices = c("\u2014 Not connected \u2014" = ""))
        return()
      }
      tryCatch({
        schemas <- DBI::dbGetQuery(conn_of(env),
          "SELECT schema_name FROM information_schema.schemata ORDER BY schema_name")
        updateSelectInput(session, "bkp_schema",
                          choices = c("\u2014 Select schema \u2014" = "",
                                      schemas$schema_name))
      }, error = function(e) NULL)
    }) |> bindEvent(input$bkp_env,
                    connections$DEV(), connections$UAT(), connections$PROD(),
                    ignoreNULL = FALSE)

    # Reactive: tables in the selected backup source schema
    bkp_table_list <- reactive({
      req(nzchar(input$bkp_schema))
      env <- input$bkp_env
      req(is_valid(env))
      tryCatch({
        DBI::dbGetQuery(conn_of(env),
          "SELECT table_name FROM information_schema.tables
            WHERE table_schema = $1 AND table_type = 'BASE TABLE'
            ORDER BY table_name",
          params = list(input$bkp_schema))$table_name
      }, error = function(e) character(0))
    })

    # Update table selector and default dest name when schema changes
    observe({
      tbls <- bkp_table_list()
      updateSelectInput(session, "bkp_tables", choices = tbls, selected = tbls)
      updateTextInput(session, "bkp_dest",
                      value = .default_backup_name(input$bkp_schema))
    })

    observeEvent(input$bkp_all,  updateSelectInput(session, "bkp_tables",
                                                   selected = bkp_table_list()))
    observeEvent(input$bkp_none, updateSelectInput(session, "bkp_tables",
                                                   selected = character(0)))

    # ---- Backup to schema ---------------------------------------------------

    observeEvent(input$btn_backup_schema, {
      env        <- input$bkp_env
      src_schema <- input$bkp_schema
      dest       <- trimws(input$bkp_dest)
      tables     <- input$bkp_tables

      if (!is_valid(env)) {
        .log(bkp_log, "error", paste(env, "is not connected.")); return()
      }
      if (!nzchar(src_schema)) {
        .log(bkp_log, "error", "No source schema selected."); return()
      }
      if (!nzchar(dest)) {
        .log(bkp_log, "error", "No destination schema name specified."); return()
      }
      if (length(tables) == 0) {
        .log(bkp_log, "error", "No tables selected."); return()
      }

      conn <- conn_of(env)
      .log(bkp_log, "info", paste0("Starting backup [", env, "]: ",
                                    src_schema, " \u2192 ", dest))
      .log(bkp_log, "info", paste("Tables:", paste(tables, collapse = ", ")))

      # Create destination schema
      q_dest <- DBI::dbQuoteIdentifier(conn, dest)
      schema_ok <- tryCatch({
        DBI::dbExecute(conn, sprintf("CREATE SCHEMA IF NOT EXISTS %s", q_dest))
        .log(bkp_log, "ok", paste("Schema created:", dest))
        TRUE
      }, error = function(e) {
        .log(bkp_log, "error", paste("Cannot create schema:", e$message))
        FALSE
      })
      if (!schema_ok) return()

      # Copy each table
      ok_n   <- 0L
      fail_n <- 0L

      for (tbl in tables) {
        tryCatch({
          q_src <- DBI::dbQuoteIdentifier(conn, src_schema)
          q_tbl <- DBI::dbQuoteIdentifier(conn, tbl)

          DBI::dbExecute(conn, sprintf(
            "CREATE TABLE %s.%s AS SELECT * FROM %s.%s",
            q_dest, q_tbl, q_src, q_tbl
          ))

          n <- DBI::dbGetQuery(conn,
            sprintf("SELECT COUNT(*) AS n FROM %s.%s", q_dest, q_tbl))$n

          .log(bkp_log, "ok",
               sprintf("  \u2713 %-35s  %s rows", tbl, format(n, big.mark = ",")))
          ok_n <- ok_n + 1L

        }, error = function(e) {
          .log(bkp_log, "error", sprintf("  \u2717 %-35s  %s", tbl, e$message))
          fail_n <<- fail_n + 1L
        })
      }

      .log(bkp_log,
           if (fail_n == 0L) "ok" else "warn",
           sprintf("Done: %d/%d tables backed up to [%s]",
                   ok_n, length(tables), dest))
    })

    # ---- Export to RDS -------------------------------------------------------

    output$btn_export_rds <- downloadHandler(
      filename = function() {
        schema <- input$bkp_schema
        if (!nzchar(schema)) schema <- "backup"
        paste0(schema, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        env    <- input$bkp_env
        schema <- input$bkp_schema
        tables <- input$bkp_tables

        if (!is_valid(env) || !nzchar(schema) || length(tables) == 0) {
          .log(bkp_log, "error", "RDS export: environment not connected or nothing selected.")
          saveRDS(list(), file)
          return()
        }

        conn <- conn_of(env)
        .log(bkp_log, "info",
             paste0("RDS export started [", env, "]: ", schema,
                    " (", length(tables), " tables)"))

        data_list <- vector("list", length(tables))
        names(data_list) <- tables

        for (tbl in tables) {
          tryCatch({
            q_sch <- DBI::dbQuoteIdentifier(conn, schema)
            q_tbl <- DBI::dbQuoteIdentifier(conn, tbl)
            df    <- DBI::dbGetQuery(conn,
                       sprintf("SELECT * FROM %s.%s", q_sch, q_tbl))
            data_list[[tbl]] <- df
            .log(bkp_log, "ok",
                 sprintf("  \u2713 %-35s  %s rows \xd7 %s cols",
                         tbl,
                         format(nrow(df), big.mark = ","),
                         ncol(df)))
          }, error = function(e) {
            data_list[[tbl]] <<- NULL
            .log(bkp_log, "error", sprintf("  \u2717 %-35s  %s", tbl, e$message))
          })
        }

        # Embed backup metadata
        attr(data_list, "backup_meta") <- list(
          env         = env,
          schema      = schema,
          tables      = tables,
          exported_at = Sys.time(),
          r_version   = R.version$version.string
        )

        saveRDS(data_list, file = file)
        non_null <- sum(!sapply(data_list, is.null))
        .log(bkp_log, "ok",
             sprintf("RDS file ready: %d/%d tables exported.", non_null, length(tables)))
      }
    )

    # =========================================================================
    # RESTORE TAB
    # =========================================================================

    # Helper: load backup + target schemas for the restore tab
    load_rst_schemas <- function() {
      env <- input$rst_env
      if (!is_valid(env)) {
        updateSelectInput(session, "rst_backup_schema",
                          choices = c("\u2014 Not connected \u2014" = ""))
        updateSelectInput(session, "rst_target_schema",
                          choices = c("\u2014 Not connected \u2014" = ""))
        return()
      }
      conn <- conn_of(env)

      tryCatch({
        # Backup schemas (prefixed with BACKUP_PREFIX, newest first)
        bkp_schemas <- DBI::dbGetQuery(conn,
          "SELECT schema_name FROM information_schema.schemata
            WHERE schema_name LIKE $1
            ORDER BY schema_name DESC",
          params = list(paste0(BACKUP_PREFIX, "%")))

        updateSelectInput(session, "rst_backup_schema",
          choices = if (nrow(bkp_schemas) > 0)
            c("\u2014 Select backup \u2014" = "", bkp_schemas$schema_name)
          else
            c("\u2014 No backup schemas found \u2014" = "")
        )

        # All non-backup, non-system schemas as restore targets
        tgt_schemas <- DBI::dbGetQuery(conn,
          "SELECT schema_name FROM information_schema.schemata
            WHERE schema_name NOT LIKE $1
              AND schema_name NOT IN ('information_schema',
                                      'pg_catalog', 'pg_toast')
            ORDER BY schema_name",
          params = list(paste0(BACKUP_PREFIX, "%")))

        updateSelectInput(session, "rst_target_schema",
                          choices = c("\u2014 Select target \u2014" = "",
                                      tgt_schemas$schema_name))
      }, error = function(e) NULL)
    }

    observe({ load_rst_schemas() }) |>
      bindEvent(input$rst_env, input$btn_refresh_rst,
                connections$DEV(), connections$UAT(), connections$PROD(),
                ignoreNULL = FALSE)

    # Reactive: tables inside the selected backup schema
    rst_table_list <- reactive({
      req(nzchar(input$rst_backup_schema))
      env <- input$rst_env
      req(is_valid(env))
      tryCatch({
        DBI::dbGetQuery(conn_of(env),
          "SELECT table_name FROM information_schema.tables
            WHERE table_schema = $1 AND table_type = 'BASE TABLE'
            ORDER BY table_name",
          params = list(input$rst_backup_schema))$table_name
      }, error = function(e) character(0))
    })

    observe({
      tbls <- rst_table_list()
      updateSelectInput(session, "rst_tables", choices = tbls, selected = tbls)
    })

    observeEvent(input$rst_all,  updateSelectInput(session, "rst_tables",
                                                   selected = rst_table_list()))
    observeEvent(input$rst_none, updateSelectInput(session, "rst_tables",
                                                   selected = character(0)))

    # ---- Execute restore ----------------------------------------------------

    observeEvent(input$btn_restore, {
      env           <- input$rst_env
      backup_schema <- input$rst_backup_schema
      target_schema <- input$rst_target_schema
      tables        <- input$rst_tables
      mode          <- input$rst_mode

      if (!is_valid(env)) {
        .log(rst_log, "error", paste(env, "is not connected.")); return()
      }
      if (!nzchar(backup_schema)) {
        .log(rst_log, "error", "No backup schema selected."); return()
      }
      if (!nzchar(target_schema)) {
        .log(rst_log, "error", "No target schema selected."); return()
      }
      if (length(tables) == 0) {
        .log(rst_log, "error", "No tables selected."); return()
      }
      if (backup_schema == target_schema) {
        .log(rst_log, "error", "Backup and target schema must differ."); return()
      }

      conn <- conn_of(env)
      .log(rst_log, "info",
           sprintf("Starting restore [%s]: %s \u2192 %s [mode: %s]",
                   env, backup_schema, target_schema, mode))

      ok_n   <- 0L
      fail_n <- 0L

      if (mode == "overwrite") {
        # Wrap all TRUNCATE + INSERT operations in a single transaction so a
        # failure rolls everything back, leaving the target in a clean state.
        tryCatch({
          DBI::dbWithTransaction(conn, {
            for (tbl in tables) {
              q_src  <- DBI::dbQuoteIdentifier(conn, backup_schema)
              q_dest <- DBI::dbQuoteIdentifier(conn, target_schema)
              q_tbl  <- DBI::dbQuoteIdentifier(conn, tbl)

              # Check whether the table already exists in the target
              tbl_exists <- DBI::dbGetQuery(conn,
                "SELECT EXISTS(
                   SELECT 1 FROM information_schema.tables
                    WHERE table_schema = $1 AND table_name = $2
                 ) AS tbl_exists",
                params = list(target_schema, tbl))$tbl_exists

              if (isTRUE(tbl_exists)) {
                DBI::dbExecute(conn,
                  sprintf("TRUNCATE %s.%s", q_dest, q_tbl))
                DBI::dbExecute(conn,
                  sprintf("INSERT INTO %s.%s SELECT * FROM %s.%s",
                          q_dest, q_tbl, q_src, q_tbl))
                n <- DBI::dbGetQuery(conn,
                  sprintf("SELECT COUNT(*) AS n FROM %s.%s", q_dest, q_tbl))$n
                .log(rst_log, "ok",
                     sprintf("  \u21ba %-35s  %s rows (overwritten)",
                             tbl, format(n, big.mark = ",")))
              } else {
                DBI::dbExecute(conn,
                  sprintf("CREATE TABLE %s.%s AS SELECT * FROM %s.%s",
                          q_dest, q_tbl, q_src, q_tbl))
                n <- DBI::dbGetQuery(conn,
                  sprintf("SELECT COUNT(*) AS n FROM %s.%s", q_dest, q_tbl))$n
                .log(rst_log, "ok",
                     sprintf("  \u2713 %-35s  %s rows (created)",
                             tbl, format(n, big.mark = ",")))
              }
              ok_n <<- ok_n + 1L
            }
          })
          .log(rst_log, "ok",
               sprintf("Transaction committed: %d/%d tables restored.",
                       ok_n, length(tables)))
        }, error = function(e) {
          .log(rst_log, "error",
               paste("Transaction rolled back \u2014 no changes applied:", e$message))
        })

      } else {
        # Create mode: no transaction needed, each table is independent
        for (tbl in tables) {
          tryCatch({
            q_src  <- DBI::dbQuoteIdentifier(conn, backup_schema)
            q_dest <- DBI::dbQuoteIdentifier(conn, target_schema)
            q_tbl  <- DBI::dbQuoteIdentifier(conn, tbl)

            DBI::dbExecute(conn,
              sprintf("CREATE TABLE %s.%s AS SELECT * FROM %s.%s",
                      q_dest, q_tbl, q_src, q_tbl))
            n <- DBI::dbGetQuery(conn,
              sprintf("SELECT COUNT(*) AS n FROM %s.%s", q_dest, q_tbl))$n
            .log(rst_log, "ok",
                 sprintf("  \u2713 %-35s  %s rows", tbl, format(n, big.mark = ",")))
            ok_n <- ok_n + 1L
          }, error = function(e) {
            .log(rst_log, "error",
                 sprintf("  \u2717 %-35s  %s", tbl, e$message))
            fail_n <<- fail_n + 1L
          })
        }
        .log(rst_log,
             if (fail_n == 0L) "ok" else "warn",
             sprintf("Done: %d/%d tables restored.", ok_n, length(tables)))
      }
    })
  })
}
