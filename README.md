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
| `tests/test_engine.R` | Engine tests: `Rscript tests/test_engine.R` from the project root |

## Packages

shiny, bslib, bsicons, DT, plotly, rhandsontable, readxl, writexl, openxlsx, dplyr, tidyr,
lubridate, forecast.

## Key modelling choices

- **GST** is backed out of GST-inclusive MRP (`Gross sales x rate / (1 + rate)`). The original
  behaviour (`Gross sales x rate`) is still available under *Accounting basis* in the sidebar.
- **Quarters are fiscal** (Q1 = Apr to Jun).
- **The forecast starts the month after the latest actual**, and the horizon is chosen as a fiscal-year end.
- **Ratios are recomputed** from summed amounts (GM %, realisation per unit), never summed or averaged.

The in-app **Methodology** tab documents every formula.
