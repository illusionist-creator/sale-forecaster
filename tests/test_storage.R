# Version storage tests (SQLite in memory). Run from the project root:
#   Rscript tests/test_storage.R

source(file.path("R", "engine.R"))
source(file.path("R", "storage.R"))

failures <- 0
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "  PASS " else "  FAIL ", desc, "\n", sep = "")
  if (!ok) failures <<- failures + 1
}

# SQLite in memory by default; with GF_DB=postgres (CI) the same tests run
# against a real Postgres server, starting from an empty table.
on_postgres <- store_backend() == "postgres"
cat("Backend:", store_description(), "\n")
con <- store_connect(":memory:")
if (on_postgres) DBI::dbExecute(con, "DELETE FROM plan_versions")
clean <- validate_actuals(make_sample_data(as.Date("2026-03-01")))$data
monthly <- consolidate_monthly(clean)
assumptions <- default_assumptions(monthly)
assumptions$Price_Hike_Annual <- 0.05
plan <- plan_template(monthly, assumptions, as.Date("2026-06-01"))
plan$Supply_Qty[1] <- 4321
snapshot <- list(
  source = "test.xlsx", clean = clean, issues = validate_actuals(clean)$issues,
  assumptions = assumptions, plan = plan, plan_name = "plan.xlsx",
  settings = list(method = "ets", horizon = "2027-03-01", gst_mode = "inclusive", levers = SCENARIO_DEFAULTS),
  fy_summary = data.frame(Period = as.Date("2026-04-01"), Net_Sales = 123.456789, GM = 45.6, Basis = "F", Scenario = "Base")
)

cat("Save & list\n")
id1 <- version_save(con, snapshot, "FY27 Budget v1", "first cut", "Draft", "analyst",
                    kpis = list(net_sales = 1e8, gm = 4.5e7, gm_pct = 0.45))
Sys.sleep(0.01)
id2 <- version_save(con, modifyList(snapshot, list(plan = NULL, plan_name = NULL)), "FY27 Budget v2")
v <- version_list(con)
check("Two versions listed", nrow(v) == 2)
check("Newest first", v$id[1] == id2)
check("IDs are unique", id1 != id2)
check("KPIs stored", v$net_sales[v$id == id1] == 1e8 && v$gm_pct[v$id == id1] == 0.45)
check("Metadata stored", v$method[v$id == id1] == "ets" && v$last_actual[v$id == id1] == "2026-03-01" &&
        v$n_series[v$id == id1] == 9 && v$created_by[v$id == id1] == "analyst")
check("Blank name rejected", inherits(try(version_save(con, snapshot, "   "), silent = TRUE), "try-error"))

cat("Round trip\n")
s <- version_get(con, id1)
check("Actuals restored exactly", isTRUE(all.equal(s$clean, clean, check.attributes = FALSE)))
check("Dates restored as Date", inherits(s$clean$Date, "Date") && inherits(s$plan$Date, "Date"))
check("Assumptions restored with full precision", isTRUE(all.equal(s$assumptions, assumptions, check.attributes = FALSE)))
check("Monthly plan restored, including NA and override volumes",
      s$plan$Supply_Qty[1] == 4321 && is.na(s$plan$Supply_Qty[2]))
check("Settings restored", s$settings$method == "ets" && s$settings$horizon == "2027-03-01")
check("Scenario levers restored", isTRUE(all.equal(s$settings$levers, SCENARIO_DEFAULTS)))
check("FY summary restored", s$fy_summary$Net_Sales == 123.456789 && inherits(s$fy_summary$Period, "Date"))
check("Missing plan restored as NULL", is.null(version_get(con, id2)$plan))
check("Restored data forecasts identically", {
  a <- build_forecast(consolidate_monthly(s$clean), s$assumptions, "yoy", as.Date("2027-03-01"), s$plan)
  b <- build_forecast(monthly, assumptions, "yoy", as.Date("2027-03-01"), plan)
  isTRUE(all.equal(a$Supply_Qty, b$Supply_Qty)) && isTRUE(all.equal(a$MRP, b$MRP))
})
check("Unknown id returns NULL", is.null(version_get(con, "nope")))

cat("Status, delete & safety\n")
check("Status updated", version_set_status(con, id1, "Approved", "cfo"))
v <- version_list(con)
check("Status audit fields set", v$status[v$id == id1] == "Approved" && v$status_updated_by[v$id == id1] == "cfo")
check("Invalid status rejected", inherits(try(version_set_status(con, id1, "Bogus"), silent = TRUE), "try-error"))
check("Quotes in ids are not injectable", !version_delete(con, "x' OR '1'='1") && nrow(version_list(con)) == 2)
check("Delete removes one version", version_delete(con, id2) && nrow(version_list(con)) == 1)

cat("Reconnect\n")
calls <- 0
shared <- new_store(function() {
  calls <<- calls + 1
  store_connect(":memory:")
})
shared(function(con) DBI::dbDisconnect(con))
check("Dropped connection is replaced on next use", is.data.frame(shared(version_list)) && calls == 2)

if (on_postgres) {
  c2 <- store_connect()
  check("Versions visible from a second connection", nrow(version_list(c2)) == 1)
  DBI::dbDisconnect(c2)
} else {
  file_db <- tempfile(fileext = ".sqlite")
  c1 <- store_connect(file_db)
  invisible(version_save(c1, snapshot, "persisted"))
  DBI::dbDisconnect(c1)
  c2 <- store_connect(file_db)
  check("Versions persist in the SQLite file across connections", nrow(version_list(c2)) == 1)
  DBI::dbDisconnect(c2)
}

DBI::dbDisconnect(con)
cat(sprintf("\n%d failure(s)\n", failures))
if (failures) quit(status = 1)
