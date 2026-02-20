# modules/db_connection_module.R
# Database Connection Module
#
# Manages a PostgreSQL connection for a given environment (DEV or PROD).
# Reads credentials exclusively from environment variables - never hardcoded.
#
# Returns: list(con = reactiveVal, status = reactiveVal)
#   con    - holds the active DBI connection, or NULL
#   status - one of "disconnected", "connecting", "connected", "error"

# ---- UI ------------------------------------------------------------------

dbConnectionUI <- function(id) {
  ns <- NS(id)

  div(
    class = "card border-0 shadow-sm mb-3",
    div(
      class = "card-body p-3",
      # Status row
      div(
        class = "d-flex align-items-center gap-2 mb-2",
        # Colour dot updated via JS
        tags$span(
          id = ns("status_dot"),
          style = paste0(
            "display:inline-block; width:10px; height:10px;",
            "border-radius:50%; background:#6c757d; flex-shrink:0;"
          )
        ),
        uiOutput(ns("status_badge")),
        div(class = "ms-auto d-flex gap-2",
          actionButton(
            ns("btn_connect"),
            label  = tagList(icon("plug"), " Connect"),
            class  = "btn btn-sm btn-success"
          ),
          actionButton(
            ns("btn_disconnect"),
            label  = tagList(icon("power-off"), " Disconnect"),
            class  = "btn btn-sm btn-outline-danger"
          )
        )
      ),
      # Connection detail line
      uiOutput(ns("conn_detail"))
    )
  )
}

# ---- Server --------------------------------------------------------------

dbConnectionServer <- function(id, env_prefix) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive state ---
    con        <- reactiveVal(NULL)
    con_status <- reactiveVal("disconnected")  # disconnected | connecting | connected | error
    con_detail <- reactiveVal("")

    # --- Helpers ---

    get_creds <- function() {
      list(
        dbname   = Sys.getenv(paste0(env_prefix, "_DB"),   unset = ""),
        host     = Sys.getenv(paste0(env_prefix, "_HOST"), unset = ""),
        user     = Sys.getenv(paste0(env_prefix, "_USER"), unset = ""),
        password = Sys.getenv(paste0(env_prefix, "_PASS"), unset = ""),
        port     = suppressWarnings(
          as.integer(Sys.getenv(paste0(env_prefix, "_PORT"), unset = "5432"))
        )
      )
    }

    safe_disconnect <- function() {
      old <- con()
      if (!is.null(old)) {
        tryCatch(
          if (DBI::dbIsValid(old)) DBI::dbDisconnect(old),
          error = function(e) NULL
        )
      }
    }

    do_connect <- function() {
      creds <- get_creds()

      # Check for missing mandatory credentials
      mandatory <- c("dbname", "host", "user", "password")
      missing   <- mandatory[sapply(creds[mandatory], function(x) !nzchar(x))]

      if (length(missing) > 0) {
        var_names <- paste(
          paste0(env_prefix, "_", c(DB = "DB", host = "HOST", user = "USER", password = "PASS")[missing]),
          collapse = ", "
        )
        con_status("error")
        con_detail(paste("Missing env vars:", var_names))
        return(invisible(FALSE))
      }

      con_status("connecting")
      con_detail(paste0(creds$user, "@", creds$host, "/", creds$dbname))

      result <- tryCatch({
        safe_disconnect()

        new_con <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname   = creds$dbname,
          host     = creds$host,
          user     = creds$user,
          password = creds$password,
          port     = creds$port
        )

        # Set the default search path
        DBI::dbExecute(new_con, "SET search_path TO fast2pool, public;")

        con(new_con)
        con_status("connected")
        con_detail(paste0(creds$user, "@", creds$host, "/", creds$dbname))
        TRUE
      }, error = function(e) {
        con(NULL)
        con_status("error")
        con_detail(conditionMessage(e))
        FALSE
      })

      result
    }

    # --- Auto-connect on first flush if env vars are present ---
    session$onFlushed(function() {
      creds <- get_creds()
      mandatory <- c("dbname", "host", "user", "password")
      if (all(sapply(creds[mandatory], nzchar))) {
        do_connect()
      }
    }, once = TRUE)

    # --- Manual connect ---
    observeEvent(input$btn_connect, {
      success <- do_connect()
      if (success) {
        showNotification(
          paste(env_prefix, "database connected."),
          type     = "message",
          duration = 3
        )
      } else {
        showNotification(
          paste(env_prefix, "connection failed:", con_detail()),
          type     = "error",
          duration = 8
        )
      }
    })

    # --- Manual disconnect ---
    observeEvent(input$btn_disconnect, {
      safe_disconnect()
      con(NULL)
      con_status("disconnected")
      con_detail("")
      showNotification(
        paste(env_prefix, "disconnected."),
        type     = "warning",
        duration = 3
      )
    })

    # --- UI outputs ---

    output$status_badge <- renderUI({
      switch(con_status(),
        connected    = span(class = "badge bg-success",          "Connected"),
        disconnected = span(class = "badge bg-secondary",         "Disconnected"),
        connecting   = span(class = "badge bg-warning text-dark", "Connecting\u2026"),
        error        = span(class = "badge bg-danger",            "Error"),
        span(class = "badge bg-secondary", "Unknown")
      )
    })

    output$conn_detail <- renderUI({
      detail <- con_detail()
      status <- con_status()
      if (!nzchar(detail)) return(NULL)
      cls <- if (status == "error") "text-danger small" else "text-muted small"
      p(class = cls,
        style = "margin:0; word-break:break-all;",
        detail)
    })

    # Update the colour dot via JavaScript
    observe({
      color <- switch(con_status(),
        connected    = "#198754",
        disconnected = "#6c757d",
        connecting   = "#ffc107",
        error        = "#dc3545",
        "#6c757d"
      )
      shinyjs::runjs(sprintf(
        'var el = document.getElementById("%s"); if(el) el.style.backgroundColor = "%s";',
        ns("status_dot"), color
      ))
    })

    # --- Return reactive handles ---
    list(con = con, status = con_status)
  })
}
