# modules/table_structure_module.R
# Table Structure Module
#
# Displays three sub-tabs for a selected table:
#   1. Columns   - column names, types, nullability, defaults
#   2. Constraints - primary keys, foreign keys, unique, check
#   3. Sequences  - all sequences in the selected schema

# ---- UI ------------------------------------------------------------------

tableStructureUI <- function(id) {
  ns <- NS(id)

  navset_tab(
    # --- Columns tab ---
    nav_panel(
      title = tagList(icon("list-ul"), " Columns"),
      div(
        class = "p-3",
        uiOutput(ns("no_sel_columns")),
        DT::DTOutput(ns("dt_columns"))
      )
    ),

    # --- Constraints tab ---
    nav_panel(
      title = tagList(icon("key"), " Constraints"),
      div(
        class = "p-3",
        uiOutput(ns("no_sel_constraints")),
        DT::DTOutput(ns("dt_constraints"))
      )
    ),

    # --- Sequences tab ---
    nav_panel(
      title = tagList(icon("arrow-up-1-9"), " Sequences"),
      div(
        class = "p-3",
        p(class = "text-muted small mb-2",
          "Sequences defined in the selected schema."),
        DT::DTOutput(ns("dt_sequences"))
      )
    )
  )
}

# ---- Server --------------------------------------------------------------

tableStructureServer <- function(id, con, schema, table) {
  moduleServer(id, function(input, output, session) {

    # Helper predicates
    is_connected <- function() {
      conn <- con()
      !is.null(conn) && tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)
    }

    # Null-coalescing operator
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    has_table <- reactive({
      is_connected() &&
        nzchar(schema() %||% "") &&
        nzchar(table()  %||% "")
    })

    # ------------------------------------------------------------------
    # Columns
    # ------------------------------------------------------------------
    col_data <- reactive({
      req(has_table())

      tryCatch({
        DBI::dbGetQuery(
          con(),
          "SELECT
             ordinal_position                          AS \"#\",
             column_name                               AS \"Column\",
             data_type                                 AS \"Type\",
             COALESCE(
               character_maximum_length::text,
               numeric_precision::text
             )                                         AS \"Length / Prec\",
             is_nullable                               AS \"Nullable\",
             column_default                            AS \"Default\"
           FROM information_schema.columns
           WHERE table_schema = $1
             AND table_name   = $2
           ORDER BY ordinal_position",
          params = list(schema(), table())
        )
      }, error = function(e) {
        showNotification(paste("Column query error:", e$message), type = "error")
        data.frame()
      })
    })

    output$no_sel_columns <- renderUI({
      if (!has_table()) {
        div(class = "alert alert-info mt-2",
            icon("circle-info"), "  Select a schema and table to view column structure.")
      }
    })

    output$dt_columns <- DT::renderDT({
      req(has_table())
      df <- col_data()
      req(nrow(df) > 0)

      DT::datatable(
        df,
        options  = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      ) |>
        DT::formatStyle(
          "Nullable",
          backgroundColor = DT::styleEqual(
            c("YES", "NO"),
            c("#fff3cd", "#d1ecf1")
          )
        )
    })

    # ------------------------------------------------------------------
    # Constraints
    # ------------------------------------------------------------------
    con_data <- reactive({
      req(has_table())

      tryCatch({
        DBI::dbGetQuery(
          con(),
          "SELECT
             tc.constraint_type  AS \"Type\",
             tc.constraint_name  AS \"Constraint\",
             kcu.column_name     AS \"Column\",
             ccu.table_schema    AS \"Ref Schema\",
             ccu.table_name      AS \"Ref Table\",
             ccu.column_name     AS \"Ref Column\"
           FROM information_schema.table_constraints AS tc
           LEFT JOIN information_schema.key_column_usage AS kcu
             ON kcu.constraint_name = tc.constraint_name
            AND kcu.table_schema    = tc.table_schema
            AND kcu.table_name      = tc.table_name
           LEFT JOIN information_schema.constraint_column_usage AS ccu
             ON ccu.constraint_name = tc.constraint_name
            AND ccu.table_schema    = tc.table_schema
           WHERE tc.table_schema = $1
             AND tc.table_name   = $2
           ORDER BY tc.constraint_type, tc.constraint_name, kcu.ordinal_position",
          params = list(schema(), table())
        )
      }, error = function(e) {
        data.frame()
      })
    })

    output$no_sel_constraints <- renderUI({
      if (!has_table()) {
        div(class = "alert alert-info mt-2",
            icon("circle-info"), "  Select a schema and table to view constraints.")
      }
    })

    output$dt_constraints <- DT::renderDT({
      req(has_table())
      df <- con_data()

      if (nrow(df) == 0) {
        df <- data.frame(Message = "No constraints found for this table.")
      }

      type_colors <- c(
        "PRIMARY KEY" = "#cfe2ff",
        "FOREIGN KEY" = "#d1ecf1",
        "UNIQUE"      = "#d4edda",
        "CHECK"       = "#fff3cd"
      )

      dt <- DT::datatable(
        df,
        options  = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )

      if ("Type" %in% names(df)) {
        dt <- DT::formatStyle(
          dt, "Type",
          backgroundColor = DT::styleEqual(
            names(type_colors), unname(type_colors)
          )
        )
      }

      dt
    })

    # ------------------------------------------------------------------
    # Sequences (schema-level, not table-specific)
    # ------------------------------------------------------------------
    seq_data <- reactive({
      if (!is_connected() || !nzchar(schema() %||% "")) return(data.frame())

      tryCatch({
        DBI::dbGetQuery(
          con(),
          "SELECT
             sequence_name    AS \"Sequence\",
             data_type        AS \"Type\",
             start_value      AS \"Start\",
             minimum_value    AS \"Min\",
             maximum_value    AS \"Max\",
             increment        AS \"Increment\",
             cycle_option     AS \"Cycles\"
           FROM information_schema.sequences
           WHERE sequence_schema = $1
           ORDER BY sequence_name",
          params = list(schema())
        )
      }, error = function(e) data.frame())
    })

    output$dt_sequences <- DT::renderDT({
      df <- seq_data()

      if (nrow(df) == 0) {
        df <- data.frame(Message = "No sequences found in this schema.")
      }

      DT::datatable(
        df,
        options  = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )
    })
  })
}
