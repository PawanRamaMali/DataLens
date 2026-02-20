# app.R
# DataLens
#
# A professional internal tool for auditing PostgreSQL schemas across
# DEV and PROD environments.  Credentials are read exclusively from
# environment variables - nothing is hardcoded.
#
# Required environment variables:
#   DEV_DB, DEV_HOST, DEV_USER, DEV_PASS, DEV_PORT (optional, default 5432)
#   PROD_DB, PROD_HOST, PROD_USER, PROD_PASS, PROD_PORT (optional, default 5432)
#
# Usage:
#   Rscript -e "shiny::runApp('.')"
#   or open in RStudio and click Run App.

# ============================================================
# 1. Dependencies
# ============================================================

library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(DT)
library(shinyjs)

# ============================================================
# 2. Modules
# ============================================================

source("modules/db_connection_module.R")
source("modules/schema_selector_module.R")
source("modules/table_structure_module.R")
source("modules/data_preview_module.R")
source("modules/table_compare_module.R")
source("modules/settings_module.R")

# ============================================================
# 3. Theme
# ============================================================

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2c3e50",
  secondary  = "#7f8c8d",
  success    = "#27ae60",
  info       = "#2980b9",
  warning    = "#f39c12",
  danger     = "#e74c3c",
  base_font  = font_google("Inter"),
  code_font  = font_google("JetBrains Mono")
)

# ============================================================
# 4. Helper UI snippets
# ============================================================

# Reusable explorer layout (sidebar + main content)
explorer_layout <- function(env_label, env_class, conn_id, selector_id,
                             structure_id, data_id, header_id) {
  layout_sidebar(
    sidebar = sidebar(
      width = 290,
      open  = TRUE,
      bg    = "#f8f9fa",

      div(
        class = paste("fw-bold mb-3 d-flex align-items-center gap-2 px-1",
                      env_class),
        icon(if (env_label == "DEV") "code-branch" else "server"),
        paste(env_label, "Environment")
      ),

      dbConnectionUI(conn_id),
      schemaSelectorUI(selector_id)
    ),

    # Main panel
    div(
      class = "p-3",
      uiOutput(header_id),
      navset_tab(
        nav_panel(
          title = tagList(icon("table-columns"), " Structure"),
          tableStructureUI(structure_id)
        ),
        nav_panel(
          title = tagList(icon("table"), " Data Preview"),
          dataPreviewUI(data_id)
        )
      )
    )
  )
}

# ============================================================
# 5. UI
# ============================================================

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$img(
      src    = "https://www.postgresql.org/media/img/about/press/elephant.png",
      height = "28px",
      alt    = "PostgreSQL"
    ),
    "DataLens"
  ),
  theme    = app_theme,
  bg       = "#2c3e50",
  inverse  = TRUE,
  collapsible = TRUE,

  useShinyjs(),

  # ----------------------------------------------------------------
  # Tab 1 – Overview
  # ----------------------------------------------------------------
  nav_panel(
    title = tagList(icon("house"), " Overview"),
    value = "overview",

    div(
      class = "container-fluid py-4",

      div(
        class = "row mb-4",
        div(
          class = "col-12",
          h2(
            class = "d-flex align-items-center gap-2 mb-1",
            icon("database"),
            "DataLens"
          ),
          p(class = "lead text-muted mb-0",
            "Inspect and compare PostgreSQL schemas, tables, and data across DEV and PROD.")
        )
      ),

      hr(),

      # Connection status cards
      div(
        class = "row g-4 mb-4",

        # DEV card
        div(
          class = "col-md-6",
          div(
            class = "card shadow-sm h-100",
            div(
              class = "card-header bg-info text-white d-flex align-items-center gap-2",
              icon("code-branch"), strong("DEV Environment")
            ),
            div(
              class = "card-body",
              uiOutput("overview_dev_status"),
              hr(class = "my-2"),
              tags$dl(
                class = "row small text-muted mb-0",
                tags$dt(class = "col-4", "Host"),
                tags$dd(class = "col-8", code(Sys.getenv("DEV_HOST", "not set"))),
                tags$dt(class = "col-4", "Database"),
                tags$dd(class = "col-8", code(Sys.getenv("DEV_DB", "not set"))),
                tags$dt(class = "col-4", "User"),
                tags$dd(class = "col-8", code(Sys.getenv("DEV_USER", "not set")))
              )
            )
          )
        ),

        # PROD card
        div(
          class = "col-md-6",
          div(
            class = "card shadow-sm h-100",
            div(
              class = "card-header bg-warning text-dark d-flex align-items-center gap-2",
              icon("server"), strong("PROD Environment")
            ),
            div(
              class = "card-body",
              uiOutput("overview_prod_status"),
              hr(class = "my-2"),
              tags$dl(
                class = "row small text-muted mb-0",
                tags$dt(class = "col-4", "Host"),
                tags$dd(class = "col-8", code(Sys.getenv("PROD_HOST", "not set"))),
                tags$dt(class = "col-4", "Database"),
                tags$dd(class = "col-8", code(Sys.getenv("PROD_DB", "not set"))),
                tags$dt(class = "col-4", "User"),
                tags$dd(class = "col-8", code(Sys.getenv("PROD_USER", "not set")))
              )
            )
          )
        )
      ),

      # Feature info
      div(
        class = "row g-4",
        div(
          class = "col-md-4",
          div(
            class = "card shadow-sm h-100",
            div(class = "card-header fw-semibold",
                tagList(icon("circle-info"), " Setup")),
            div(
              class = "card-body small",
              p("Set these environment variables before starting the app:"),
              tags$table(
                class = "table table-sm table-bordered",
                tags$thead(tags$tr(tags$th("Variable"), tags$th("Purpose"))),
                tags$tbody(
                  tags$tr(tags$td(code("DEV_DB")),   tags$td("DEV database name")),
                  tags$tr(tags$td(code("DEV_HOST")), tags$td("DEV host")),
                  tags$tr(tags$td(code("DEV_USER")), tags$td("DEV username")),
                  tags$tr(tags$td(code("DEV_PASS")), tags$td("DEV password")),
                  tags$tr(tags$td(code("PROD_DB")),   tags$td("PROD database name")),
                  tags$tr(tags$td(code("PROD_HOST")), tags$td("PROD host")),
                  tags$tr(tags$td(code("PROD_USER")), tags$td("PROD username")),
                  tags$tr(tags$td(code("PROD_PASS")), tags$td("PROD password"))
                )
              )
            )
          )
        ),

        div(
          class = "col-md-4",
          div(
            class = "card shadow-sm h-100",
            div(class = "card-header fw-semibold",
                tagList(icon("wand-magic-sparkles"), " Features")),
            div(
              class = "card-body",
              tags$ul(
                class = "list-unstyled small",
                tags$li(tagList(icon("check", class = "text-success"), " Browse schemas & tables")),
                tags$li(tagList(icon("check", class = "text-success"), " Column types, nullability, defaults")),
                tags$li(tagList(icon("check", class = "text-success"), " Primary / foreign key constraints")),
                tags$li(tagList(icon("check", class = "text-success"), " Sequence inspection")),
                tags$li(tagList(icon("check", class = "text-success"), " Paginated data preview")),
                tags$li(tagList(icon("check", class = "text-success"), " Column-level search & filter")),
                tags$li(tagList(icon("check", class = "text-success"), " CSV / Excel export")),
                tags$li(tagList(icon("check", class = "text-success"), " DEV vs PROD structure diff"))
              )
            )
          )
        ),

        div(
          class = "col-md-4",
          div(
            class = "card shadow-sm h-100",
            div(class = "card-header fw-semibold",
                tagList(icon("shield-halved"), " Security")),
            div(
              class = "card-body",
              tags$ul(
                class = "list-unstyled small",
                tags$li(tagList(icon("lock", class = "text-success"), " No hardcoded credentials")),
                tags$li(tagList(icon("lock", class = "text-success"), " Env-var-only connection")),
                tags$li(tagList(icon("lock", class = "text-success"), " Parameterised queries")),
                tags$li(tagList(icon("lock", class = "text-success"), " dbQuoteIdentifier() for names")),
                tags$li(tagList(icon("lock", class = "text-success"), " Max 1 000 preview rows")),
                tags$li(tagList(icon("lock", class = "text-success"), " Read-only operations only")),
                tags$li(tagList(icon("lock", class = "text-success"), " Connections closed on exit"))
              )
            )
          )
        )
      )
    )
  ),

  # ----------------------------------------------------------------
  # Tab 2 – DEV Explorer
  # ----------------------------------------------------------------
  nav_panel(
    title = tagList(icon("code-branch"), " DEV"),
    value = "dev_explorer",
    explorer_layout(
      env_label    = "DEV",
      env_class    = "text-info",
      conn_id      = "dev_conn",
      selector_id  = "dev_selector",
      structure_id = "dev_structure",
      data_id      = "dev_data",
      header_id    = "dev_header"
    )
  ),

  # ----------------------------------------------------------------
  # Tab 3 – PROD Explorer
  # ----------------------------------------------------------------
  nav_panel(
    title = tagList(icon("server"), " PROD"),
    value = "prod_explorer",
    explorer_layout(
      env_label    = "PROD",
      env_class    = "text-warning",
      conn_id      = "prod_conn",
      selector_id  = "prod_selector",
      structure_id = "prod_structure",
      data_id      = "prod_data",
      header_id    = "prod_header"
    )
  ),

  # ----------------------------------------------------------------
  # Tab 4 – Compare
  # ----------------------------------------------------------------
  nav_panel(
    title = tagList(icon("arrows-left-right"), " Compare"),
    value = "compare",
    tableCompareUI("compare")
  ),

  # ----------------------------------------------------------------
  # Tab 5 – Settings
  # ----------------------------------------------------------------
  nav_panel(
    title = tagList(icon("gear"), " Settings"),
    value = "settings",
    settingsUI("settings")
  ),

  # Navbar right-side spacer / version tag
  nav_spacer(),
  nav_item(
    tags$span(
      class = "navbar-text small opacity-75",
      "v1.0.0 | DataLens"
    )
  )
)

# ============================================================
# 6. Server
# ============================================================

server <- function(input, output, session) {

  # ------------------------------------------------------------------
  # Connections
  # ------------------------------------------------------------------

  dev_conn_res  <- dbConnectionServer("dev_conn",  env_prefix = "DEV")
  prod_conn_res <- dbConnectionServer("prod_conn", env_prefix = "PROD")

  dev_con  <- dev_conn_res$con
  prod_con <- prod_conn_res$con

  # ------------------------------------------------------------------
  # Overview status badges (read-only display of connection state)
  # ------------------------------------------------------------------

  make_status_ui <- function(status_rv) {
    renderUI({
      st <- status_rv()
      badge <- switch(st,
        connected    = span(class = "badge bg-success fs-6",   tagList(icon("circle-check"),  " Connected")),
        disconnected = span(class = "badge bg-secondary fs-6", tagList(icon("circle-xmark"),  " Disconnected")),
        connecting   = span(class = "badge bg-warning fs-6 text-dark",
                            tagList(icon("spinner"),      " Connecting\u2026")),
        error        = span(class = "badge bg-danger fs-6",    tagList(icon("triangle-exclamation"), " Error")),
        span(class = "badge bg-secondary", "Unknown")
      )
      div(class = "mb-1", badge)
    })
  }

  output$overview_dev_status  <- make_status_ui(dev_conn_res$status)
  output$overview_prod_status <- make_status_ui(prod_conn_res$status)

  # ------------------------------------------------------------------
  # DEV Explorer modules
  # ------------------------------------------------------------------

  dev_sel <- schemaSelectorServer("dev_selector", con = dev_con)

  tableStructureServer(
    "dev_structure",
    con    = dev_con,
    schema = dev_sel$schema,
    table  = dev_sel$table
  )

  dataPreviewServer(
    "dev_data",
    con    = dev_con,
    schema = dev_sel$schema,
    table  = dev_sel$table
  )

  output$dev_header <- renderUI({
    sch <- dev_sel$schema()
    tbl <- dev_sel$table()
    if (!nzchar(sch %||% "") || !nzchar(tbl %||% "")) return(NULL)
    div(
      class = "mb-3 pb-2 border-bottom",
      h5(
        class = "d-flex align-items-center gap-2 mb-0",
        span(class = "badge bg-info", "DEV"),
        code(paste0(sch, ".", tbl)),
        uiOutput("dev_row_count_badge", inline = TRUE)
      )
    )
  })

  output$dev_row_count_badge <- renderUI({
    cnt <- dev_sel$row_count()
    if (is.null(cnt)) return(NULL)
    if (is.na(cnt))   return(span(class = "badge bg-danger ms-1", "count error"))
    span(class = "badge bg-light text-dark border ms-1",
         format(cnt, big.mark = ","), " rows")
  })

  # ------------------------------------------------------------------
  # PROD Explorer modules
  # ------------------------------------------------------------------

  prod_sel <- schemaSelectorServer("prod_selector", con = prod_con)

  tableStructureServer(
    "prod_structure",
    con    = prod_con,
    schema = prod_sel$schema,
    table  = prod_sel$table
  )

  dataPreviewServer(
    "prod_data",
    con    = prod_con,
    schema = prod_sel$schema,
    table  = prod_sel$table
  )

  output$prod_header <- renderUI({
    sch <- prod_sel$schema()
    tbl <- prod_sel$table()
    if (!nzchar(sch %||% "") || !nzchar(tbl %||% "")) return(NULL)
    div(
      class = "mb-3 pb-2 border-bottom",
      h5(
        class = "d-flex align-items-center gap-2 mb-0",
        span(class = "badge bg-warning text-dark", "PROD"),
        code(paste0(sch, ".", tbl)),
        uiOutput("prod_row_count_badge", inline = TRUE)
      )
    )
  })

  output$prod_row_count_badge <- renderUI({
    cnt <- prod_sel$row_count()
    if (is.null(cnt)) return(NULL)
    if (is.na(cnt))   return(span(class = "badge bg-danger ms-1", "count error"))
    span(class = "badge bg-light text-dark border ms-1",
         format(cnt, big.mark = ","), " rows")
  })

  # ------------------------------------------------------------------
  # Compare module
  # ------------------------------------------------------------------

  tableCompareServer("compare", dev_con = dev_con, prod_con = prod_con)

  # ------------------------------------------------------------------
  # Settings module
  # ------------------------------------------------------------------

  settingsServer(
    "settings",
    dev_connect_fn  = dev_conn_res$connect,
    prod_connect_fn = prod_conn_res$connect
  )

  # ------------------------------------------------------------------
  # Cleanup: disconnect on session end
  # ------------------------------------------------------------------

  session$onSessionEnded(function() {
    for (con_rv in list(dev_con, prod_con)) {
      conn <- tryCatch(isolate(con_rv()), error = function(e) NULL)
      if (!is.null(conn)) {
        tryCatch(
          if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn),
          error = function(e) NULL
        )
      }
    }
  })
}

# Null-coalescing operator (available globally for use in server scope)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================
# 7. Launch
# ============================================================

shinyApp(ui = ui, server = server)
