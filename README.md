# Growth Forecaster

R Shiny P&L planning suite for consumer brands selling through multiple partner channels.
Upload monthly sales history by partner and product category to get a fiscal-year
(April to March) P&L, volume forecasts with prediction ranges, Base/Upside/Downside
scenarios, driver sensitivity, model back-testing and an Excel board pack.

## Run

```r
shiny::runApp("sales_forecaster.R")
```

Click **Explore with sample data** to try it without a file. Use **Download data template**
on the Data tab for the upload format.

## Layout

| Path | Purpose |
|---|---|
| `sales_forecaster.R` | UI and server wiring (deployed to shinyapps.io as the primary document) |
| `R/engine.R` | All finance and forecasting logic, as pure functions with no Shiny dependency |
| `R/storage.R` | Saved plan versions (SQLite locally, Postgres in production) |
| `tests/` | `Rscript tests/test_engine.R` and `Rscript tests/test_storage.R` from the project root |
| `Dockerfile`, `docker-compose.yml`, `deploy/` | Self-hosted production setup |
| `.github/workflows/ci.yml` | Builds the image and runs all tests (including against Postgres) on every push |

## Saved versions

The **Versions** tab saves the current plan (data, assumptions, monthly plan, method, horizon,
GST basis, scenario levers) as an immutable snapshot with a name, notes and a status
(Draft, In review, Approved, Archived). Versions can be reloaded exactly or compared by fiscal
year. Approved versions are locked against deletion, and each version records who saved it.

Storage defaults to a SQLite file at `data/forecaster.sqlite`. Set `GF_DB=postgres` plus the
standard `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER` and `PGPASSWORD` variables to use Postgres.

## Deployment

- **Production:** Docker on your own server, with HTTPS, a login prompt, Postgres and nightly backups.
  Step-by-step guide: [deploy/DEPLOY.md](deploy/DEPLOY.md).
- **shinyapps.io:** still works for demos, but its file system resets on every restart, so saved
  versions are temporary there (the app shows a warning). Redeploying needs `RSQLite` and `RPostgres`
  installed locally.

## Packages

shiny, bslib, bsicons, DT, plotly, rhandsontable, readxl, writexl, openxlsx, dplyr, tidyr,
lubridate, forecast, DBI, RSQLite, RPostgres, jsonlite.

## Key modelling choices

- **GST** is backed out of GST-inclusive MRP (`Gross sales x rate / (1 + rate)`). The original
  behaviour (`Gross sales x rate`) is still available under *Accounting basis* in the sidebar.
- **Quarters are fiscal** (Q1 = Apr to Jun).
- **The forecast starts the month after the latest actual**, and the horizon is chosen as a fiscal-year end.
- **Ratios are recomputed** from summed amounts (GM %, realisation per unit), never summed or averaged.

The in-app **Methodology** tab documents every formula.
