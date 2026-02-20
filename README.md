# DataLens

A professional R Shiny application for auditing PostgreSQL databases across **DEV** and **PROD** environments. Built with `bslib` (Bootstrap 5), `DBI`, `RPostgres`, and `DT`.

---

## Features

| Feature | Description |
|---|---|
| Schema browser | Lists all schemas from `information_schema.schemata` |
| Table browser | Lists all base tables for a selected schema |
| Structure viewer | Columns, types, nullability, defaults (ordered by position) |
| Constraint viewer | Primary keys, foreign keys, unique, check constraints |
| Sequence viewer | All sequences in the selected schema |
| Data preview | Paginated preview up to 1 000 rows with column filters |
| Row count | Live COUNT(*) for selected table |
| CSV / Excel export | Export preview data via DT Buttons |
| DEV vs PROD diff | Side-by-side structure comparison with mismatch highlighting |
| Safe queries | Parameterised queries + `dbQuoteIdentifier()` for all identifiers |

---

## Quick Start

### 1. Install R packages

```r
install.packages(c(
  "shiny",
  "bslib",
  "DBI",
  "RPostgres",
  "DT",
  "shinyjs"
))
```

### 2. Set environment variables

**Linux / macOS (bash/zsh):**

```bash
export DEV_DB=fast2pool_dev
export DEV_HOST=localhost
export DEV_USER=postgres
export DEV_PASS=your_dev_password
export DEV_PORT=5432          # optional, default 5432

export PROD_DB=fast2pool
export PROD_HOST=prod-db.example.com
export PROD_USER=app_readonly
export PROD_PASS=your_prod_password
export PROD_PORT=5432
```

**Windows (PowerShell):**

```powershell
$env:DEV_DB   = "fast2pool_dev"
$env:DEV_HOST = "localhost"
$env:DEV_USER = "postgres"
$env:DEV_PASS = "your_dev_password"

$env:PROD_DB   = "fast2pool"
$env:PROD_HOST = "prod-db.example.com"
$env:PROD_USER = "app_readonly"
$env:PROD_PASS = "your_prod_password"
```

**Using a `.Renviron` file** (recommended for development):

Create `.Renviron` in the project root — add it to `.gitignore`!

```
DEV_DB=fast2pool_dev
DEV_HOST=localhost
DEV_USER=postgres
DEV_PASS=secret

PROD_DB=fast2pool
PROD_HOST=prod-db.example.com
PROD_USER=app_readonly
PROD_PASS=secret
```

Then restart R so `readRenviron(".Renviron")` picks up the values.

### 3. Run the app

```r
shiny::runApp(".")
```

or from the terminal:

```bash
Rscript -e "shiny::runApp('.')"
```

---

## Project Structure

```
.
├── app.R                          # Main application entry point
├── modules/
│   ├── db_connection_module.R     # Connection management (per environment)
│   ├── schema_selector_module.R   # Schema & table dropdowns + row count
│   ├── table_structure_module.R   # Columns / Constraints / Sequences tabs
│   ├── data_preview_module.R      # Paginated data preview with export
│   └── table_compare_module.R     # DEV vs PROD structure diff
└── README.md
```

---

## Architecture

The app uses Shiny's **module pattern** throughout:

```
app.R
 ├── dbConnectionServer("dev_conn")   → dev_con (reactiveVal<connection>)
 ├── dbConnectionServer("prod_conn")  → prod_con
 │
 ├── schemaSelectorServer("dev_selector", con = dev_con)
 │    └── returns list(schema, table, row_count)
 ├── tableStructureServer("dev_structure", con, schema, table)
 ├── dataPreviewServer("dev_data", con, schema, table)
 │
 ├── schemaSelectorServer("prod_selector", con = prod_con)
 ├── tableStructureServer("prod_structure", ...)
 ├── dataPreviewServer("prod_data", ...)
 │
 └── tableCompareServer("compare", dev_con, prod_con)
```

Connections live in `reactiveVal` objects and are closed automatically via `session$onSessionEnded`.

---

## Security Notes

- Passwords are **never** logged, displayed, or stored in code.
- All schema/table names are quoted with `DBI::dbQuoteIdentifier()`.
- Data queries use parameterised `$1` placeholders to prevent SQL injection.
- Preview rows are hard-capped at **1 000** regardless of UI selection.
- The app performs **read-only** queries only (`SELECT`, `information_schema`).

---

## PostgreSQL Role Recommendation

For PROD, create a dedicated read-only role:

```sql
CREATE ROLE db_explorer_ro LOGIN PASSWORD 'strong_password';
GRANT CONNECT ON DATABASE fast2pool TO db_explorer_ro;
GRANT USAGE ON SCHEMA fast2pool, public TO db_explorer_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA fast2pool TO db_explorer_ro;
ALTER DEFAULT PRIVILEGES IN SCHEMA fast2pool
  GRANT SELECT ON TABLES TO db_explorer_ro;
```

---

## Troubleshooting

| Problem | Solution |
|---|---|
| "Missing env vars" on startup | Verify all four `*_DB/HOST/USER/PASS` variables are exported |
| Tables not appearing | Only `BASE TABLE` types are listed — views are excluded by design |
| `search_path` error | Confirm the `fast2pool` schema exists, or adjust the `SET search_path` line in `db_connection_module.R` |
| DT Buttons not exporting | Ensure the DataTables Buttons extension JS is available (bundled with the `DT` package) |
| Google Fonts not loading | Remove `font_google(...)` from `app_theme` in `app.R` when running offline |
