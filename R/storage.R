# Growth Forecaster: plan version storage
# A version is an immutable snapshot of everything needed to reproduce a
# forecast: the actuals, assumptions, monthly plan, method, horizon, GST basis
# and scenario levers. Snapshots are stored as JSON so they stay readable
# outside R. Backend: SQLite file by default, Postgres when GF_DB=postgres.

VERSION_STATUSES <- c("Draft", "In review", "Approved", "Archived")
SNAPSHOT_SCHEMA <- 1L

store_backend <- function() if (identical(Sys.getenv("GF_DB"), "postgres")) "postgres" else "sqlite"

sqlite_path <- function() Sys.getenv("GF_SQLITE_PATH", file.path("data", "forecaster.sqlite"))

store_description <- function() {
  if (store_backend() == "postgres") {
    sprintf("Postgres database %s on %s", Sys.getenv("PGDATABASE", "?"), Sys.getenv("PGHOST", "localhost"))
  } else {
    sprintf("SQLite file %s", sqlite_path())
  }
}

# shinyapps.io resets the app's file system on every restart or redeploy.
store_is_ephemeral <- function() store_backend() == "sqlite" && identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")

store_connect <- function(path = sqlite_path()) {
  con <- if (store_backend() == "postgres") {
    # Connection details come from the standard libpq variables:
    # PGHOST, PGPORT, PGDATABASE, PGUSER, PGPASSWORD.
    DBI::dbConnect(RPostgres::Postgres())
  } else {
    if (path != ":memory:") dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    DBI::dbConnect(RSQLite::SQLite(), path)
  }
  store_init(con)
  con
}

store_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS plan_versions (
      id TEXT PRIMARY KEY,
      name TEXT NOT NULL,
      notes TEXT,
      status TEXT NOT NULL,
      created_at TEXT NOT NULL,
      created_by TEXT,
      status_updated_at TEXT,
      status_updated_by TEXT,
      source TEXT,
      method TEXT,
      horizon TEXT,
      gst_mode TEXT,
      last_actual TEXT,
      n_series INTEGER,
      net_sales DOUBLE PRECISION,
      gm DOUBLE PRECISION,
      gm_pct DOUBLE PRECISION,
      payload TEXT NOT NULL
    )")
  invisible(con)
}

# Shared connection with one reconnect-and-retry, so a restarted database or
# dropped socket doesn't take the app down. Usage: store(function(con) ...).
new_store <- function(connect = store_connect) {
  con <- NULL
  function(f) {
    run <- function() {
      if (is.null(con) || !DBI::dbIsValid(con)) con <<- connect()
      f(con)
    }
    tryCatch(run(), error = function(e) {
      if (!is.null(con)) try(DBI::dbDisconnect(con), silent = TRUE)
      con <<- NULL
      run()
    })
  }
}

utc_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

format_local_time <- function(utc, tz = Sys.getenv("GF_TZ", "Asia/Kolkata")) {
  t <- as.POSIXct(utc, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  format(t, "%d %b %Y %H:%M", tz = tz)
}

new_version_id <- function() {
  # Independent of the R random seed (the sample-data generator sets one).
  paste0(format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"), "-", Sys.getpid())
}

encode_snapshot <- function(snapshot) {
  as.character(jsonlite::toJSON(c(list(schema = SNAPSHOT_SCHEMA), snapshot),
                                dataframe = "rows", Date = "ISO8601", digits = NA,
                                na = "null", null = "null", auto_unbox = TRUE))
}

decode_snapshot <- function(json) {
  s <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  as_dates <- function(df, cols) {
    if (!is.data.frame(df)) return(NULL)
    for (col in intersect(cols, names(df))) df[[col]] <- as.Date(df[[col]])
    df
  }
  s$clean <- as_dates(s$clean, "Date")
  s$plan <- as_dates(s$plan, "Date")
  s$fy_summary <- as_dates(s$fy_summary, "Period")
  s$assumptions <- as.data.frame(s$assumptions)[ASSUMPTION_COLS]
  s
}

version_save <- function(con, snapshot, name, notes = "", status = "Draft", user = "anonymous", kpis = list()) {
  stopifnot(nzchar(trimws(name)), status %in% VERSION_STATUSES)
  num <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)
  id <- new_version_id()
  row <- data.frame(
    id = id, name = trimws(name), notes = notes, status = status,
    created_at = utc_now(), created_by = user,
    status_updated_at = NA_character_, status_updated_by = NA_character_,
    source = snapshot$source %||% NA_character_,
    method = snapshot$settings$method %||% NA_character_,
    horizon = snapshot$settings$horizon %||% NA_character_,
    gst_mode = snapshot$settings$gst_mode %||% NA_character_,
    last_actual = if (is.null(snapshot$clean)) NA_character_ else format(max(snapshot$clean$Date)),
    n_series = if (is.null(snapshot$assumptions)) NA_integer_ else nrow(snapshot$assumptions),
    net_sales = num(kpis$net_sales), gm = num(kpis$gm), gm_pct = num(kpis$gm_pct),
    payload = encode_snapshot(snapshot),
    stringsAsFactors = FALSE
  )
  DBI::dbAppendTable(con, "plan_versions", row)
  id
}

version_list <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT id, name, notes, status, created_at, created_by, status_updated_at, status_updated_by,
           source, method, horizon, gst_mode, last_actual, n_series, net_sales, gm, gm_pct
    FROM plan_versions
    ORDER BY created_at DESC, id DESC")
}

version_get <- function(con, id) {
  sql <- DBI::sqlInterpolate(con, "SELECT id, name, status, payload FROM plan_versions WHERE id = ?id", id = id)
  row <- DBI::dbGetQuery(con, sql)
  if (!nrow(row)) return(NULL)
  c(list(id = row$id, name = row$name, status = row$status), decode_snapshot(row$payload))
}

version_set_status <- function(con, id, status, user = "anonymous") {
  stopifnot(status %in% VERSION_STATUSES)
  sql <- DBI::sqlInterpolate(con, "
    UPDATE plan_versions SET status = ?status, status_updated_at = ?at, status_updated_by = ?user
    WHERE id = ?id", status = status, at = utc_now(), user = user, id = id)
  DBI::dbExecute(con, sql) == 1
}

version_delete <- function(con, id) {
  sql <- DBI::sqlInterpolate(con, "DELETE FROM plan_versions WHERE id = ?id", id = id)
  DBI::dbExecute(con, sql) == 1
}
