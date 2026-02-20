# modules/data_preview_module.R
# Data Preview Module
#
# Displays a configurable-row-count preview of a table with:
#   - Column filters
#   - Global search
#   - Pagination
#   - CSV / Excel export
#
# Safety: row count is hard-capped at 1 000 rows.

MAX_PREVIEW_ROWS <- 1000L

# ---- UI ------------------------------------------------------------------

dataPreviewUI <- function(id) {
  ns <- NS(id)

  div(
    # Toolbar
    div(
      class = "d-flex flex-wrap align-items-center gap-3 mb-3 p-2 bg-light rounded",

      div(
        class = "d-flex align-items-center gap-2",
        tags$label(
          class = "form-label mb-0 small fw-semibold",
          `for` = ns("row_limit"),
          "Rows:"
        ),
        selectInput(
          ns("row_limit"),
          label    = NULL,
          choices  = c("50" = 50, "100" = 100, "250" = 250,
                       "500" = 500, "1000" = 1000),
          selected = 100,
          width    = "100px"
        )
      ),

      actionButton(
        ns("btn_load"),
        label = tagList(icon("cloud-download-alt"), " Load Data"),
        class = "btn btn-primary btn-sm"
      ),

      uiOutput(ns("load_status"))
    ),

    # Placeholder / error
    uiOutput(ns("placeholder")),

    # Data table
    div(
      style = "overflow-x: auto;",
      DT::DTOutput(ns("dt_data"))
    )
  )
}

# ---- Server --------------------------------------------------------------

dataPreviewServer <- function(id, con, schema, table) {
  moduleServer(id, function(input, output, session) {

    is_connected <- function() {
      conn <- con()
      !is.null(conn) && tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)
    }

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    has_table <- reactive({
      is_connected() && nzchar(schema() %||% "") && nzchar(table() %||% "")
    })

    load_status <- reactiveVal(NULL)  # NULL | list(ok, msg)

    # Reset status when table selection changes
    observe({
      schema(); table()
      load_status(NULL)
    })

    # --- Load data on button click ---
    preview_data <- eventReactive(input$btn_load, {
      req(has_table())

      limit <- min(as.integer(input$row_limit), MAX_PREVIEW_ROWS)
      conn  <- con()
      sch   <- schema()
      tbl   <- table()

      load_status(list(ok = TRUE, msg = "Loading\u2026"))

      tryCatch({
        q_sch  <- DBI::dbQuoteIdentifier(conn, sch)
        q_tbl  <- DBI::dbQuoteIdentifier(conn, tbl)
        query  <- sprintf("SELECT * FROM %s.%s LIMIT %d", q_sch, q_tbl, limit)
        df     <- DBI::dbGetQuery(conn, query)

        load_status(list(
          ok  = TRUE,
          msg = sprintf("Loaded %s rows \u00d7 %s columns",
                        format(nrow(df), big.mark = ","),
                        ncol(df))
        ))
        df
      }, error = function(e) {
        load_status(list(ok = FALSE, msg = paste("Error:", e$message)))
        showNotification(paste("Data load error:", e$message), type = "error")
        data.frame()
      })
    })

    # --- Placeholder when nothing is selected ---
    output$placeholder <- renderUI({
      if (!has_table()) {
        div(
          class = "alert alert-info",
          icon("circle-info"),
          "  Select a schema and table, then click \u2018Load Data\u2019."
        )
      }
    })

    # --- Status label ---
    output$load_status <- renderUI({
      st <- load_status()
      if (is.null(st)) return(NULL)
      cls <- if (isTRUE(st$ok)) "text-muted small" else "text-danger small fw-semibold"
      span(class = cls, st$msg)
    })

    # --- Data table ---
    output$dt_data <- DT::renderDT({
      df <- preview_data()
      req(is.data.frame(df) && nrow(df) > 0)

      DT::datatable(
        df,
        filter     = "top",
        extensions = "Buttons",
        options    = list(
          pageLength = 25,
          scrollX    = TRUE,
          dom        = "Bfrtip",
          buttons    = list(
            list(extend = "csv",   text = "Export CSV"),
            list(extend = "excel", text = "Export Excel"),
            list(extend = "copy",  text = "Copy")
          )
        ),
        rownames = FALSE,
        class    = "table table-sm table-hover table-striped"
      )
    })
  })
}
