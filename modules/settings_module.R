# modules/settings_module.R
# Settings Module
#
# Provides a UI form for entering database credentials.
# "Save & Connect" applies values via Sys.setenv() and immediately reconnects.
# "Test Connection" opens a temporary connection to validate credentials
# without saving them or affecting the live connection.

# ---- Internal helpers ----------------------------------------------------

# Build one environment's credential form fields.
# `ns`     - the module's namespace function
# `prefix` - "dev" or "prod" (lowercase input IDs)
# `env`    - "DEV" or "PROD" (used for env-var labels)
.cred_fields <- function(ns, prefix, env) {
  tagList(
    div(
      class = "row g-2",
      div(
        class = "col-md-8",
        textInput(
          ns(paste0(prefix, "_db")),
          label       = paste0(env, "_DB"),
          value       = Sys.getenv(paste0(env, "_DB"),   unset = ""),
          placeholder = "database name",
          width       = "100%"
        )
      ),
      div(
        class = "col-md-4",
        textInput(
          ns(paste0(prefix, "_port")),
          label       = paste0(env, "_PORT"),
          value       = Sys.getenv(paste0(env, "_PORT"), unset = "5432"),
          placeholder = "5432",
          width       = "100%"
        )
      )
    ),
    textInput(
      ns(paste0(prefix, "_host")),
      label       = paste0(env, "_HOST"),
      value       = Sys.getenv(paste0(env, "_HOST"), unset = ""),
      placeholder = "hostname or IP address",
      width       = "100%"
    ),
    textInput(
      ns(paste0(prefix, "_user")),
      label       = paste0(env, "_USER"),
      value       = Sys.getenv(paste0(env, "_USER"), unset = ""),
      placeholder = "username",
      width       = "100%"
    ),
    passwordInput(
      ns(paste0(prefix, "_pass")),
      label       = paste0(env, "_PASS"),
      value       = Sys.getenv(paste0(env, "_PASS"), unset = ""),
      placeholder = "password",
      width       = "100%"
    )
  )
}

# ---- UI ------------------------------------------------------------------

settingsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid py-4",

    div(
      class = "row mb-4",
      div(
        class = "col-12",
        h3(
          class = "d-flex align-items-center gap-2 mb-1",
          icon("gear"),
          "Settings"
        ),
        p(class = "text-muted mb-0",
          "Enter database credentials below. Values are applied to the current",
          "R session via ", code("Sys.setenv()"), ". Restart the app or re-enter",
          "credentials if you need to change them after restart.")
      )
    ),

    # Info banner
    div(
      class = "alert alert-info d-flex align-items-start gap-2 mb-4",
      icon("circle-info"),
      div(
        "Credentials are stored in memory only for this R session.",
        "They are ", strong("never"), " written to disk unless your",
        "shell profile already exports them."
      )
    ),

    # DEV + PROD cards
    div(
      class = "row g-4",

      # ---- DEV ----
      div(
        class = "col-xl-6",
        div(
          class = "card shadow-sm h-100",
          div(
            class = "card-header bg-info text-white d-flex align-items-center gap-2",
            icon("code-branch"),
            strong("DEV Database")
          ),
          div(
            class = "card-body",
            .cred_fields(ns, "dev", "DEV"),
            div(
              class = "d-flex gap-2 mt-3 flex-wrap",
              actionButton(
                ns("save_dev"),
                label = tagList(icon("floppy-disk"), " Save & Connect"),
                class = "btn btn-success"
              ),
              actionButton(
                ns("test_dev"),
                label = tagList(icon("plug"), " Test Connection"),
                class = "btn btn-outline-secondary"
              )
            ),
            div(class = "mt-3", uiOutput(ns("dev_status")))
          )
        )
      ),

      # ---- PROD ----
      div(
        class = "col-xl-6",
        div(
          class = "card shadow-sm h-100",
          div(
            class = "card-header bg-warning text-dark d-flex align-items-center gap-2",
            icon("server"),
            strong("PROD Database")
          ),
          div(
            class = "card-body",
            .cred_fields(ns, "prod", "PROD"),
            div(
              class = "d-flex gap-2 mt-3 flex-wrap",
              actionButton(
                ns("save_prod"),
                label = tagList(icon("floppy-disk"), " Save & Connect"),
                class = "btn btn-success"
              ),
              actionButton(
                ns("test_prod"),
                label = tagList(icon("plug"), " Test Connection"),
                class = "btn btn-outline-secondary"
              )
            ),
            div(class = "mt-3", uiOutput(ns("prod_status")))
          )
        )
      )
    ),

    # Current env var reference table
    hr(class = "my-4"),
    div(
      class = "card shadow-sm",
      div(class = "card-header fw-semibold",
          tagList(icon("circle-info"), " Current Environment Variable Values")),
      div(
        class = "card-body p-0",
        div(
          class = "table-responsive",
          tags$table(
            class = "table table-sm table-hover mb-0",
            tags$thead(
              tags$tr(
                tags$th("Variable"),
                tags$th("Value")
              )
            ),
            tags$tbody(
              lapply(
                c("DEV_DB", "DEV_HOST", "DEV_USER", "DEV_PORT",
                  "PROD_DB", "PROD_HOST", "PROD_USER", "PROD_PORT"),
                function(var) {
                  val <- Sys.getenv(var, unset = "")
                  tags$tr(
                    tags$td(code(var)),
                    tags$td(
                      if (nzchar(val)) span(class = "text-success", val)
                      else             span(class = "text-muted fst-italic", "not set")
                    )
                  )
                }
              )
            )
          )
        ),
        div(
          class = "p-2 border-top small text-muted",
          icon("lock"), " Password variables (DEV_PASS, PROD_PASS) are not displayed."
        )
      )
    )
  )
}

# ---- Server --------------------------------------------------------------

settingsServer <- function(id, dev_connect_fn, prod_connect_fn) {
  moduleServer(id, function(input, output, session) {

    # ---- Shared helpers --------------------------------------------------

    # Validate that required fields are filled in; return error message or NULL
    validate_fields <- function(prefix) {
      db   <- input[[paste0(prefix, "_db")]]
      host <- input[[paste0(prefix, "_host")]]
      user <- input[[paste0(prefix, "_user")]]
      pass <- input[[paste0(prefix, "_pass")]]

      missing <- c(
        if (!nzchar(db))   paste0(toupper(prefix), "_DB"),
        if (!nzchar(host)) paste0(toupper(prefix), "_HOST"),
        if (!nzchar(user)) paste0(toupper(prefix), "_USER"),
        if (!nzchar(pass)) paste0(toupper(prefix), "_PASS")
      )

      if (length(missing) > 0) {
        paste("Required fields are empty:", paste(missing, collapse = ", "))
      } else {
        NULL
      }
    }

    # Set env vars for one environment
    apply_env_vars <- function(prefix, env) {
      db   <- input[[paste0(prefix, "_db")]]
      host <- input[[paste0(prefix, "_host")]]
      user <- input[[paste0(prefix, "_user")]]
      pass <- input[[paste0(prefix, "_pass")]]
      port <- input[[paste0(prefix, "_port")]]
      port <- if (nzchar(port)) port else "5432"

      args <- setNames(
        list(db, host, user, pass, port),
        paste0(env, c("_DB", "_HOST", "_USER", "_PASS", "_PORT"))
      )
      do.call(Sys.setenv, args)
    }

    # Test a connection without saving to env vars
    test_connection <- function(prefix) {
      db   <- input[[paste0(prefix, "_db")]]
      host <- input[[paste0(prefix, "_host")]]
      user <- input[[paste0(prefix, "_user")]]
      pass <- input[[paste0(prefix, "_pass")]]
      port <- suppressWarnings(
        as.integer(input[[paste0(prefix, "_port")]])
      )
      if (is.na(port)) port <- 5432L

      tryCatch({
        tmp <- DBI::dbConnect(
          RPostgres::Postgres(),
          dbname   = db,
          host     = host,
          user     = user,
          password = pass,
          port     = port
        )
        DBI::dbDisconnect(tmp)
        list(ok = TRUE, msg = "Test connection successful.")
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      })
    }

    # Render a status alert
    status_alert <- function(ok, msg) {
      if (isTRUE(ok)) {
        div(
          class = "alert alert-success d-flex align-items-center gap-2 mb-0",
          icon("circle-check"), msg
        )
      } else {
        div(
          class = "alert alert-danger d-flex align-items-start gap-2 mb-0",
          icon("circle-xmark"),
          div(strong("Error: "), msg)
        )
      }
    }

    # ---- DEV: Save & Connect ---------------------------------------------

    observeEvent(input$save_dev, {
      err <- validate_fields("dev")

      if (!is.null(err)) {
        output$dev_status <- renderUI(status_alert(FALSE, err))
        return()
      }

      apply_env_vars("dev", "DEV")

      # Reconnect using the connection module's do_connect function
      success <- tryCatch(dev_connect_fn(), error = function(e) FALSE)

      output$dev_status <- renderUI(
        status_alert(success,
          if (success) "DEV environment variables saved and connected."
          else         "Variables saved but connection failed \u2014 check credentials.")
      )
    })

    # ---- DEV: Test -------------------------------------------------------

    observeEvent(input$test_dev, {
      err <- validate_fields("dev")
      if (!is.null(err)) {
        output$dev_status <- renderUI(status_alert(FALSE, err))
        return()
      }

      result <- test_connection("dev")
      output$dev_status <- renderUI(status_alert(result$ok, result$msg))
    })

    # ---- PROD: Save & Connect --------------------------------------------

    observeEvent(input$save_prod, {
      err <- validate_fields("prod")

      if (!is.null(err)) {
        output$prod_status <- renderUI(status_alert(FALSE, err))
        return()
      }

      apply_env_vars("prod", "PROD")

      success <- tryCatch(prod_connect_fn(), error = function(e) FALSE)

      output$prod_status <- renderUI(
        status_alert(success,
          if (success) "PROD environment variables saved and connected."
          else         "Variables saved but connection failed \u2014 check credentials.")
      )
    })

    # ---- PROD: Test ------------------------------------------------------

    observeEvent(input$test_prod, {
      err <- validate_fields("prod")
      if (!is.null(err)) {
        output$prod_status <- renderUI(status_alert(FALSE, err))
        return()
      }

      result <- test_connection("prod")
      output$prod_status <- renderUI(status_alert(result$ok, result$msg))
    })
  })
}
