# modules/table_compare_module.R
# Table Compare Module
#
# Side-by-side structural comparison of the same table in DEV and PROD.
# Highlights mismatches in data type, nullability, and presence.

# ---- UI ------------------------------------------------------------------

tableCompareUI <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-3",

    # Selection row
    div(
      class = "row g-3 mb-4",
      div(
        class = "col-lg-4",
        div(
          class = "card shadow-sm",
          div(class = "card-header fw-semibold",
              tagList(icon("arrows-left-right"), " Select Table to Compare")),
          div(
            class = "card-body",
            selectInput(
              ns("cmp_schema"),
              label   = "Schema (from DEV)",
              choices = c("\u2014 Loading \u2014" = ""),
              width   = "100%"
            ),
            selectInput(
              ns("cmp_table"),
              label   = "Table",
              choices = c("\u2014 Select schema first \u2014" = ""),
              width   = "100%"
            ),
            div(
              class = "d-flex gap-2",
              actionButton(
                ns("btn_compare"),
                label = tagList(icon("magnifying-glass"), " Compare"),
                class = "btn btn-primary"
              ),
              actionButton(
                ns("btn_refresh_schemas"),
                label = tagList(icon("rotate"), " Refresh"),
                class = "btn btn-outline-secondary"
              )
            )
          )
        )
      ),
      div(
        class = "col-lg-8",
        uiOutput(ns("summary_card"))
      )
    ),

    # Structures side by side
    div(
      class = "row g-3 mb-4",
      div(
        class = "col-md-6",
        div(
          class = "card shadow-sm",
          div(class = "card-header bg-info text-white fw-semibold",
              tagList(icon("code-branch"), " DEV Structure")),
          div(class = "card-body p-2", DT::DTOutput(ns("dt_dev")))
        )
      ),
      div(
        class = "col-md-6",
        div(
          class = "card shadow-sm",
          div(class = "card-header bg-warning text-dark fw-semibold",
              tagList(icon("server"), " PROD Structure")),
          div(class = "card-body p-2", DT::DTOutput(ns("dt_prod")))
        )
      )
    ),

    # Diff
    div(
      class = "row g-3",
      div(
        class = "col-12",
        div(
          class = "card shadow-sm",
          div(class = "card-header fw-semibold",
              tagList(icon("triangle-exclamation"), " Differences")),
          div(class = "card-body p-2", DT::DTOutput(ns("dt_diff")))
        )
      )
    )
  )
}

# ---- Server --------------------------------------------------------------

tableCompareServer <- function(id, dev_con, prod_con) {
  moduleServer(id, function(input, output, session) {

    # Helper
    is_valid <- function(conn_reactive) {
      conn <- conn_reactive()
      !is.null(conn) && tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)
    }

    get_structure <- function(conn, schema, tbl) {
      tryCatch({
        DBI::dbGetQuery(
          conn,
          "SELECT
             column_name    AS column_name,
             data_type      AS data_type,
             is_nullable    AS is_nullable,
             column_default AS column_default,
             ordinal_position
           FROM information_schema.columns
           WHERE table_schema = $1
             AND table_name   = $2
           ORDER BY ordinal_position",
          params = list(schema, tbl)
        )
      }, error = function(e) NULL)
    }

    # --- Load schemas from DEV when connection becomes available ---
    observe({
      req(is_valid(dev_con))
      tryCatch({
        schemas <- DBI::dbGetQuery(
          dev_con(),
          "SELECT schema_name FROM information_schema.schemata ORDER BY schema_name"
        )
        updateSelectInput(session, "cmp_schema",
                          choices = c("\u2014 Select schema \u2014" = "",
                                      schemas$schema_name))
      }, error = function(e) NULL)
    }) |> bindEvent(dev_con(), input$btn_refresh_schemas, ignoreNULL = FALSE)

    # --- Load tables when schema changes ---
    observeEvent(input$cmp_schema, {
      req(is_valid(dev_con), nzchar(input$cmp_schema))
      tryCatch({
        tbls <- DBI::dbGetQuery(
          dev_con(),
          "SELECT table_name
             FROM information_schema.tables
            WHERE table_schema = $1
              AND table_type   = 'BASE TABLE'
            ORDER BY table_name",
          params = list(input$cmp_schema)
        )
        updateSelectInput(session, "cmp_table",
                          choices = c("\u2014 Select table \u2014" = "",
                                      tbls$table_name))
      }, error = function(e) NULL)
    })

    # --- Comparison reactive ---
    comparison <- eventReactive(input$btn_compare, {
      req(nzchar(input$cmp_schema), nzchar(input$cmp_table))

      dev_struct  <- if (is_valid(dev_con))
        get_structure(dev_con(),  input$cmp_schema, input$cmp_table)
      else NULL

      prod_struct <- if (is_valid(prod_con))
        get_structure(prod_con(), input$cmp_schema, input$cmp_table)
      else NULL

      list(
        schema = input$cmp_schema,
        table  = input$cmp_table,
        dev    = dev_struct,
        prod   = prod_struct
      )
    })

    # --- Summary card ---
    output$summary_card <- renderUI({
      comp <- req(comparison())

      dev_ok  <- !is.null(comp$dev)  && nrow(comp$dev)  > 0
      prod_ok <- !is.null(comp$prod) && nrow(comp$prod) > 0

      both_ok <- dev_ok && prod_ok
      match   <- if (both_ok) {
        # quick check: same columns, same types
        same_cols  <- identical(sort(comp$dev$column_name), sort(comp$prod$column_name))
        same_types <- if (same_cols) {
          m_dev  <- comp$dev[order(comp$dev$column_name), ]
          m_prod <- comp$prod[order(comp$prod$column_name), ]
          identical(m_dev$data_type, m_prod$data_type)
        } else FALSE
        same_cols && same_types
      } else FALSE

      status_class <- if (match) "alert-success" else if (both_ok) "alert-warning" else "alert-danger"
      status_icon  <- if (match) "circle-check"  else if (both_ok) "triangle-exclamation" else "circle-xmark"
      status_msg   <- if (match) "Structures match exactly." else if (both_ok) "Differences detected." else "One or both environments returned no data."

      div(
        class = paste("card shadow-sm h-100"),
        div(class = "card-header fw-semibold", "Comparison Summary"),
        div(
          class = "card-body",
          h5(code(paste0(comp$schema, ".", comp$table))),
          div(
            class = paste("alert", status_class, "d-flex align-items-center gap-2 mb-3"),
            icon(status_icon),
            status_msg
          ),
          div(
            class = "row g-2",
            div(class = "col-6",
              div(
                class = paste("p-2 rounded border",
                              if (dev_ok) "border-success bg-success bg-opacity-10" else "border-danger bg-danger bg-opacity-10"),
                div(class = "d-flex align-items-center gap-2",
                    icon(if (dev_ok) "check" else "xmark"),
                    strong("DEV")),
                p(class = "mb-0 small",
                  if (dev_ok) paste(nrow(comp$dev), "columns") else "Table not found")
              )
            ),
            div(class = "col-6",
              div(
                class = paste("p-2 rounded border",
                              if (prod_ok) "border-success bg-success bg-opacity-10" else "border-danger bg-danger bg-opacity-10"),
                div(class = "d-flex align-items-center gap-2",
                    icon(if (prod_ok) "check" else "xmark"),
                    strong("PROD")),
                p(class = "mb-0 small",
                  if (prod_ok) paste(nrow(comp$prod), "columns") else "Table not found")
              )
            )
          )
        )
      )
    })

    # --- DEV structure table ---
    output$dt_dev <- DT::renderDT({
      comp <- req(comparison())
      df   <- comp$dev
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(Message = "Table not found in DEV.")
      } else {
        df <- df[, c("ordinal_position", "column_name", "data_type",
                     "is_nullable", "column_default")]
        names(df) <- c("#", "Column", "Type", "Nullable", "Default")
      }
      DT::datatable(df, options = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
                    rownames = FALSE, class = "table table-sm")
    })

    # --- PROD structure table ---
    output$dt_prod <- DT::renderDT({
      comp <- req(comparison())
      df   <- comp$prod
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(Message = "Table not found in PROD.")
      } else {
        df <- df[, c("ordinal_position", "column_name", "data_type",
                     "is_nullable", "column_default")]
        names(df) <- c("#", "Column", "Type", "Nullable", "Default")
      }
      DT::datatable(df, options = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
                    rownames = FALSE, class = "table table-sm")
    })

    # --- Diff table ---
    output$dt_diff <- DT::renderDT({
      comp <- req(comparison())

      dev  <- comp$dev
      prod <- comp$prod

      if (is.null(dev) || is.null(prod)) {
        return(DT::datatable(
          data.frame(Message = "Cannot diff: one or both connections unavailable."),
          rownames = FALSE
        ))
      }

      all_cols <- union(dev$column_name, prod$column_name)

      rows <- lapply(all_cols, function(col) {
        d <- dev[dev$column_name  == col, ]
        p <- prod[prod$column_name == col, ]

        if (nrow(d) == 0) {
          return(data.frame(
            Column  = col,
            Issue   = "Missing in DEV",
            DEV     = "\u2014",
            PROD    = p$data_type,
            stringsAsFactors = FALSE
          ))
        }
        if (nrow(p) == 0) {
          return(data.frame(
            Column  = col,
            Issue   = "Missing in PROD",
            DEV     = d$data_type,
            PROD    = "\u2014",
            stringsAsFactors = FALSE
          ))
        }

        issues <- character(0)
        if (!identical(d$data_type,   p$data_type))   issues <- c(issues, "type mismatch")
        if (!identical(d$is_nullable, p$is_nullable)) issues <- c(issues, "nullable mismatch")

        dev_val  <- paste0(d$data_type,   " | nullable=", d$is_nullable)
        prod_val <- paste0(p$data_type,   " | nullable=", p$is_nullable)

        if (length(issues) > 0) {
          data.frame(
            Column = col,
            Issue  = paste(issues, collapse = "; "),
            DEV    = dev_val,
            PROD   = prod_val,
            stringsAsFactors = FALSE
          )
        } else {
          NULL
        }
      })

      diff_df <- do.call(rbind, Filter(Negate(is.null), rows))

      if (is.null(diff_df) || nrow(diff_df) == 0) {
        diff_df <- data.frame(
          Message = "\u2705 No structural differences found \u2014 DEV and PROD match."
        )
      }

      dt <- DT::datatable(
        diff_df,
        options  = list(pageLength = 50, dom = "frtip", scrollX = TRUE),
        rownames = FALSE,
        class    = "table table-sm"
      )

      # Highlight rows with differences using JS row callback
      if ("Issue" %in% names(diff_df)) {
        dt <- DT::formatStyle(
          dt, "Issue",
          backgroundColor = DT::styleEqual(
            c("Missing in DEV", "Missing in PROD",
              "type mismatch", "nullable mismatch",
              "type mismatch; nullable mismatch"),
            c("#f8d7da", "#f8d7da", "#fff3cd", "#fff3cd", "#f8d7da")
          )
        )
      }

      dt
    })
  })
}
