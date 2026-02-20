# modules/schema_selector_module.R
# Schema & Table Selector Module
#
# Populates schema and table dropdowns from the database and displays the
# row count for the currently selected table.
#
# Returns: list(schema = reactive, table = reactive, row_count = reactive)

# ---- UI ------------------------------------------------------------------

schemaSelectorUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Schema selector card
    div(
      class = "card border-0 shadow-sm mb-3",
      div(class = "card-header py-2 fw-semibold",
          tagList(icon("layer-group"), " Schema")),
      div(
        class = "card-body p-2",
        selectInput(
          ns("schema"),
          label   = NULL,
          choices = c("\u2014 Select schema \u2014" = ""),
          width   = "100%"
        ),
        actionButton(
          ns("btn_refresh"),
          label = tagList(icon("rotate"), " Refresh"),
          class = "btn btn-sm btn-outline-secondary w-100"
        )
      )
    ),

    # Table selector card
    div(
      class = "card border-0 shadow-sm mb-3",
      div(class = "card-header py-2 fw-semibold",
          tagList(icon("table"), " Table")),
      div(
        class = "card-body p-2",
        selectInput(
          ns("table"),
          label   = NULL,
          choices = c("\u2014 Select table \u2014" = ""),
          width   = "100%"
        ),
        uiOutput(ns("row_count_ui"))
      )
    )
  )
}

# ---- Server --------------------------------------------------------------

schemaSelectorServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    # Helper: is the connection valid?
    is_connected <- function() {
      conn <- con()
      !is.null(conn) && tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)
    }

    # --- Load schemas when connection changes or user refreshes ---
    schemas <- eventReactive(
      list(con(), input$btn_refresh),
      {
        if (!is_connected()) return(character(0))

        tryCatch({
          res <- DBI::dbGetQuery(
            con(),
            "SELECT schema_name
               FROM information_schema.schemata
              ORDER BY schema_name"
          )
          res$schema_name
        }, error = function(e) {
          showNotification(paste("Error loading schemas:", e$message), type = "error")
          character(0)
        })
      },
      ignoreNULL = FALSE
    )

    observe({
      choices <- c("\u2014 Select schema \u2014" = "", schemas())
      updateSelectInput(session, "schema", choices = choices)
    })

    # --- Load tables when schema changes ---
    tables <- eventReactive(
      list(input$schema, con()),
      {
        if (!is_connected() || !nzchar(input$schema)) return(character(0))

        tryCatch({
          res <- DBI::dbGetQuery(
            con(),
            "SELECT table_name
               FROM information_schema.tables
              WHERE table_schema = $1
                AND table_type  = 'BASE TABLE'
              ORDER BY table_name",
            params = list(input$schema)
          )
          res$table_name
        }, error = function(e) {
          showNotification(paste("Error loading tables:", e$message), type = "error")
          character(0)
        })
      },
      ignoreNULL = FALSE
    )

    observe({
      choices <- c("\u2014 Select table \u2014" = "", tables())
      updateSelectInput(session, "table", choices = choices)
    })

    # --- Row count for selected table ---
    row_count <- reactive({
      if (!is_connected() || !nzchar(input$schema) || !nzchar(input$table)) {
        return(NULL)
      }

      tryCatch({
        conn   <- con()
        q_sch  <- DBI::dbQuoteIdentifier(conn, input$schema)
        q_tbl  <- DBI::dbQuoteIdentifier(conn, input$table)
        query  <- sprintf("SELECT COUNT(*) AS n FROM %s.%s", q_sch, q_tbl)
        result <- DBI::dbGetQuery(conn, query)
        as.integer(result$n[1])
      }, error = function(e) NA_integer_)
    })

    output$row_count_ui <- renderUI({
      count <- row_count()
      if (is.null(count)) return(NULL)

      div(
        class = "mt-2 d-flex align-items-center gap-1 small",
        icon("hashtag"),
        if (is.na(count)) {
          span(class = "text-danger", "Error counting rows")
        } else {
          span(class = "text-muted",
               strong(format(count, big.mark = ",")), " rows")
        }
      )
    })

    # --- Return reactive selections ---
    list(
      schema    = reactive(input$schema),
      table     = reactive(input$table),
      row_count = row_count
    )
  })
}
