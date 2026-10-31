# Growth Forecaster
# P&L planning suite: historical analysis, volume forecasting, scenarios,
# sensitivity, model back-testing and an Excel board pack.
# All calculations live in R/engine.R; this file is UI and wiring only.

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(plotly)
library(rhandsontable)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(lubridate)
library(forecast)

source(file.path("R", "engine.R"), local = TRUE)
source(file.path("R", "storage.R"), local = TRUE)

# One database connection shared by all sessions; connects lazily on first use.
store <- new_store()

# ---- Presentation constants ----------------------------------------------------

# Validated categorical order (CVD-safe adjacent pairs); colour follows the entity.
SERIES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
INK <- list(primary = "#1B2733", secondary = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", up = "#006300", down = "#d03b3b")
NAVY <- "#0F2A4A"

UNITS <- c("1e5", "1e7", "1")
names(UNITS) <- paste(INR, c("Lakh", "Crore", "(absolute)"))
UNIT_SHORT <- c("1e5" = "L", "1e7" = "Cr", "1" = "")

SCENARIO_COLORS <- c(Base = SERIES[1], Upside = SERIES[3], Downside = SERIES[2])

EXPLORER_METRICS <- c("Net sales" = "Net_Sales", "Gross margin" = "GM", "Gross margin %" = "GM_Pct",
                      "Volume (units)" = "Supply_Qty", "MRP billing" = "MRP_Billing",
                      "Net realisation / unit" = "NSR_per_unit", "Gross margin / unit" = "GM_per_unit")

PCT_ASSUMPTIONS <- c("Growth_YoY", "Growth_MoM", "Price_Hike_Annual", "Commission_Pct", "Return_Pct",
                     "GST_Pct", "Cost_Inflation_Annual", "Yield_Loss_Pct")
EDITABLE_ASSUMPTIONS <- setdiff(ASSUMPTION_COLS, c("Partner_Category", "Category", "Last_Qty"))

METHOD_NOTES <- c(
  yoy = "Each month equals the same month last year × (1 + Growth_YoY). Seasonality such as festive peaks is preserved. Series with under 12 months of history fall back to MoM growth.",
  ets = "An exponential smoothing model (error, trend, seasonality) is selected per series by AICc. Produces 80% and 95% prediction ranges. Seasonal terms are used once a series has 24+ months.",
  arima = "An ARIMA model is selected per series by AICc (forecast::auto.arima). Produces 80% and 95% prediction ranges. Seasonal terms are used once a series has 24+ months.",
  growth = "The latest month's volume compounded by Growth_MoM every month. Ignores seasonality. When a monthly plan is loaded, its Supply_Qty_Growth_Pct drives each month.",
  manual = "A flat monthly volume equal to Manual_Qty (defaults to the latest 3-month average)."
)

DISCLAIMER <- paste("Forecasts are estimates derived from historical data and user-defined assumptions.",
                    "They are not guarantees of future performance and should be reviewed before use in decisions.")

app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = INK$primary,
  primary = NAVY, secondary = "#5B6B7B", info = SERIES[1],
  success = "#0ca30c", warning = "#fab219", danger = "#d03b3b",
  base_font = font_collection("Inter", "Segoe UI", "system-ui", "-apple-system", "Roboto", "Arial", "sans-serif"),
  "border-radius" = "0.5rem",
  "card-border-color" = "#E3E7ED"
)

app_css <- "
  body { background: #F6F8FB; }
  .navbar { box-shadow: 0 1px 0 rgba(0,0,0,.08); }
  .navbar-brand { font-weight: 700; letter-spacing: .2px; }
  .card { box-shadow: 0 1px 2px rgba(16,24,40,.04); }
  .card-header { font-weight: 600; background: #FFFFFF; }
  .card-header .text-muted { font-weight: 400; }
  .bslib-value-box .value-box-title { font-size: .85rem; color: #52514e; text-transform: uppercase; letter-spacing: .4px; }
  .bslib-value-box .value-box-value { font-weight: 700; color: #0F2A4A; }
  .bslib-value-box .value-box-showcase { color: #2a78d6; }
  .bslib-value-box .value-box-showcase .bi { width: 1.75rem !important; height: 1.75rem !important; }
  .kpi-up { color: #006300; font-weight: 600; }
  .kpi-down { color: #d03b3b; font-weight: 600; }
  .kpi-delta .text-muted { font-weight: 400; margin-left: .25rem; }
  table.dataTable td, table.dataTable th { font-variant-numeric: tabular-nums; }
  .statement-table td:first-child { white-space: nowrap; }
  .sidebar-status { font-size: .85rem; background: #F1F4F8; border-radius: .5rem; padding: .6rem .75rem; }
  .sidebar-status .label { color: #52514e; }
  .method-note { font-size: .9rem; color: #52514e; }
  .hero { padding: 2.5rem 2rem; }
  .hero h2 { color: #0F2A4A; font-weight: 700; }
  .app-footer { font-size: .78rem; color: #898781; padding: .75rem 1rem 1.25rem; text-align: center; }
  .badge-sev { min-width: 4.5rem; display: inline-block; }
"

# ---- UI helpers ------------------------------------------------------------------

chart_card <- function(title, output_id, height = "380px", subtitle = NULL, ...) {
  card(
    full_screen = TRUE,
    card_header(title, if (!is.null(subtitle)) tags$small(class = "text-muted ms-2", subtitle)),
    card_body(..., plotlyOutput(output_id, height = height))
  )
}

js_number <- function(digits = 0, prefix = "") {
  DT::JS(sprintf(
    "function(d, t) { if (t !== 'display' || d === null || d === '') return d;
       var n = Number(d); if (isNaN(n)) return d;
       var s = Math.abs(n).toLocaleString('en-IN', {minimumFractionDigits: %d, maximumFractionDigits: %d});
       return n < 0 ? '(' + '%s' + s + ')' : '%s' + s; }", digits, digits, prefix, prefix))
}

js_pct <- DT::JS("function(d, t) { if (t !== 'display' || d === null || d === '') return d;
                    return (Number(d) * 100).toFixed(1) + '%'; }")

dt_table <- function(df, inr = character(), qty = character(), num = character(), pct = character(),
                     page = 15, dom = "frtip", ...) {
  idx <- function(cols) which(names(df) %in% cols) - 1
  defs <- Filter(function(d) length(d$targets), list(
    list(targets = idx(inr), render = js_number(0, INR), className = "dt-right"),
    list(targets = idx(qty), render = js_number(0), className = "dt-right"),
    list(targets = idx(num), render = js_number(2), className = "dt-right"),
    list(targets = idx(pct), render = js_pct, className = "dt-right")
  ))
  DT::datatable(df, rownames = FALSE, class = "compact hover stripe",
                options = list(pageLength = page, scrollX = TRUE, dom = dom, columnDefs = defs,
                               lengthMenu = c(15, 30, 60, 120)), ...)
}

style_plot <- function(p, y_title = "", y_format = NULL, x_type = NULL) {
  p %>%
    layout(
      font = list(family = "Inter, Segoe UI, system-ui, sans-serif", size = 12, color = INK$secondary),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(title = "", showgrid = FALSE, linecolor = INK$axis, tickcolor = INK$axis,
                   zeroline = FALSE, type = x_type),
      yaxis = list(title = y_title, gridcolor = INK$grid, zeroline = FALSE, tickformat = y_format,
                   linecolor = INK$axis),
      legend = list(orientation = "h", x = 0, y = -0.12, font = list(color = INK$secondary)),
      hovermode = "x unified",
      hoverlabel = list(bgcolor = "#FFFFFF", bordercolor = INK$axis, font = list(color = INK$primary)),
      margin = list(l = 10, r = 10, t = 20, b = 10)
    ) %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}

# Fixed colour per entity; slots past eight fold to grey rather than inventing hues.
entity_colors <- function(levels) {
  levels <- sort(unique(levels))
  stats::setNames(c(SERIES, rep(INK$muted, max(0, length(levels) - length(SERIES))))[seq_along(levels)], levels)
}

severity_badge <- function(sev) {
  cls <- c(Error = "bg-danger", Warning = "bg-warning text-dark", Info = "bg-secondary", Pass = "bg-success")
  sprintf('<span class="badge badge-sev %s">%s</span>', cls[sev], sev)
}

issues_table <- function(issues) {
  if (is.null(issues) || !nrow(issues)) return(NULL)
  issues$Severity <- severity_badge(issues$Severity)
  DT::datatable(issues, rownames = FALSE, escape = FALSE, class = "compact",
                options = list(dom = "t", ordering = FALSE, paging = FALSE))
}

lever_inputs <- function(prefix, levers) {
  tagList(
    numericInput(paste0(prefix, "_volume"), "Volume change (%)", 100 * levers$volume, step = 1),
    numericInput(paste0(prefix, "_price"), "Price (MRP) change (%)", 100 * levers$price, step = 0.5),
    numericInput(paste0(prefix, "_cost"), "Unit cost change (%)", 100 * levers$cost, step = 0.5),
    numericInput(paste0(prefix, "_commission"), "Commission (pp)", 100 * levers$commission_pp, step = 0.25),
    numericInput(paste0(prefix, "_returns"), "Returns (pp)", 100 * levers$returns_pp, step = 0.25)
  )
}

# ---- UI ----------------------------------------------------------------------------

ui <- page_navbar(
  id = "main_nav",
  title = tags$span(bs_icon("graph-up-arrow", class = "me-2"), "Growth Forecaster"),
  window_title = "Growth Forecaster | P&L Planning",
  theme = app_theme,
  fillable = FALSE,
  navbar_options = navbar_options(bg = NAVY, theme = "dark"),
  header = tags$head(tags$style(HTML(app_css))),

  sidebar = sidebar(
    width = 300,
    uiOutput("data_status"),
    selectInput("horizon", "Forecast horizon", choices = NULL),
    selectInput("method", "Volume forecasting method", choices = METHODS, selected = "yoy"),
    radioButtons("scenario", "Scenario", choices = names(SCENARIO_DEFAULTS), selected = "Base", inline = TRUE),
    selectizeInput("f_partner", "Partner category", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All partners", plugins = list("remove_button"))),
    selectizeInput("f_category", "Product category", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "All categories", plugins = list("remove_button"))),
    selectInput("unit", "Display amounts in", choices = UNITS, selected = "1e5"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Accounting basis", icon = bs_icon("journal-text"),
        selectInput("gst_mode", "GST treatment", choices = GST_MODES, selected = "inclusive"),
        tags$small(class = "text-muted",
                   "Indian MRP is GST-inclusive, so GST is backed out as Gross sales × rate / (1 + rate). ",
                   "The legacy option applies the rate directly to gross sales.")
      )
    ),
    downloadButton("board_pack", "Board pack (Excel)", class = "btn-primary w-100", icon = icon("file-excel"))
  ),

  nav_panel(
    "Overview", value = "overview", icon = bs_icon("speedometer2"),
    uiOutput("kpis"),
    layout_columns(
      col_widths = c(8, 4),
      chart_card("Net sales & gross margin", "trajectory_chart", subtitle = "actuals solid, forecast dashed"),
      chart_card("Net sales mix by partner", "mix_chart", subtitle = "by fiscal year")
    ),
    layout_columns(
      col_widths = c(7, 5),
      chart_card("Revenue-to-margin bridge", "bridge_chart", subtitle = "forecast period"),
      chart_card("Gross margin % by partner", "gm_partner_chart", subtitle = "by fiscal year")
    )
  ),

  nav_panel(
    "Data", value = "data", icon = bs_icon("database"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Load historical data"),
        card_body(
          fileInput("file_upload", NULL, accept = c(".xlsx", ".xls", ".csv"),
                    buttonLabel = "Upload…", placeholder = "Excel or CSV"),
          div(class = "d-grid gap-2",
              actionButton("load_sample", "Load sample dataset", icon = icon("flask"), class = "btn-outline-primary"),
              downloadButton("download_template", "Download data template", class = "btn-outline-secondary")),
          hr(),
          tags$small(class = "text-muted",
                     tags$b("Required columns: "), paste(ACTUAL_COLS, collapse = ", "), ". ",
                     "Any granularity (daily, weekly, monthly) is accepted and consolidated to months. ",
                     "Percentages may be entered as 0.18 or 18.")
        )
      ),
      card(card_header("Data quality report"), card_body(DTOutput("issues_table", fill = FALSE)))
    ),
    navset_card_underline(
      title = "Historical explorer",
      nav_panel(
        "Trend",
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("ex_metric", "Metric", EXPLORER_METRICS, selected = "Net_Sales"),
          selectInput("ex_by", "Break down by", c("Total" = "Total", "Partner" = "Partner_Category", "Category" = "Category")),
          selectInput("ex_grain", "Period", c("Month" = "month", "Fiscal quarter" = "quarter", "Fiscal year" = "year"))
        ),
        plotlyOutput("explorer_chart", height = "420px")
      ),
      nav_panel("Monthly actuals", DTOutput("actuals_table", fill = FALSE))
    )
  ),

  nav_panel(
    "Assumptions", value = "assumptions", icon = bs_icon("sliders"),
    card(
      card_header("Forecasting method"),
      card_body(uiOutput("method_note"))
    ),
    card(
      card_header("Assumptions by partner × category",
                  tags$small(class = "text-muted ms-2", "edit cells directly; the forecast updates automatically")),
      card_body(
        layout_columns(
          col_widths = c(4, 3, 5),
          selectInput("bulk_col", "Set a column for all rows in the current filter", EDITABLE_ASSUMPTIONS,
                      selected = "Price_Hike_Annual"),
          numericInput("bulk_val", "Value (percent columns: 5 = 5%)", value = 5),
          div(class = "pt-4",
              actionButton("bulk_apply", "Apply", icon = icon("check"), class = "btn-primary"),
              actionButton("reset_assumptions", "Reset to history", icon = icon("rotate-left"),
                           class = "btn-outline-secondary ms-2"))
        ),
        rHandsontableOutput("assumptions_table"),
        tags$small(class = "text-muted",
                   tags$b("Growth_YoY"), " drives Seasonal YoY; ", tags$b("Growth_MoM"), " drives Compound MoM growth; ",
                   tags$b("Manual_Qty"), " drives Manual run-rate. ", tags$b("Price_Hike_Annual"), " and ",
                   tags$b("Cost_Inflation_Annual"), " step MRP and unit costs up at the start of each new fiscal year (April). ",
                   "Defaults come from the latest actual month and trailing YoY growth.")
      )
    ),
    card(
      card_header("Monthly plan (optional)",
                  tags$small(class = "text-muted ms-2", "month-level overrides for prices, costs, growth or volume")),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          downloadButton("download_plan_template", "Download plan template", class = "btn-outline-secondary w-100"),
          fileInput("plan_upload", NULL, accept = c(".xlsx", ".xls", ".csv"), buttonLabel = "Upload plan…"),
          actionButton("clear_plan", "Remove plan", icon = icon("xmark"), class = "btn-outline-danger w-100")
        ),
        uiOutput("plan_status"),
        DTOutput("plan_issues", fill = FALSE)
      )
    )
  ),

  nav_panel(
    "Scenarios", value = "scenarios", icon = bs_icon("signpost-split"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Scenario levers", tags$small(class = "text-muted ms-2", "applied on top of Base")),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(h6("Upside", class = "fw-semibold"), lever_inputs("up", SCENARIO_DEFAULTS$Upside)),
            div(h6("Downside", class = "fw-semibold"), lever_inputs("down", SCENARIO_DEFAULTS$Downside))
          ),
          actionButton("reset_levers", "Reset levers", icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm")
        )
      ),
      card(card_header("Scenario comparison", tags$small(class = "text-muted ms-2", "forecast period")),
           card_body(DTOutput("scenario_table", fill = FALSE)))
    ),
    layout_columns(
      col_widths = c(7, 5),
      chart_card("Monthly net sales by scenario", "scenario_chart"),
      chart_card("Gross margin by fiscal year", "scenario_fy_chart", subtitle = "actuals + scenario forecast")
    )
  ),

  nav_panel(
    "P&L Statement", value = "pnl", icon = bs_icon("table"),
    navset_card_underline(
      nav_panel(
        "Statement",
        div(class = "d-flex flex-wrap gap-3 align-items-end mb-2",
            radioButtons("st_grain", NULL, c("Fiscal year" = "year", "Fiscal quarter" = "quarter", "Month" = "month"),
                         inline = TRUE),
            downloadButton("download_statement", "Download statement", class = "btn-outline-secondary btn-sm")),
        uiOutput("statement_caption"),
        DTOutput("statement_table", fill = FALSE)
      ),
      nav_panel(
        "Forecast detail",
        div(class = "mb-2", downloadButton("download_forecast", "Download forecast detail", class = "btn-outline-secondary btn-sm")),
        DTOutput("forecast_table", fill = FALSE)
      )
    )
  ),

  nav_panel(
    "Analysis", value = "analysis", icon = bs_icon("bullseye"),
    navset_card_underline(
      nav_panel(
        "Sensitivity",
        layout_columns(
          col_widths = c(4, 4, 4),
          sliderInput("sens_shock", "Shock each driver by ±", min = 5, max = 30, value = 10, step = 5, post = "%"),
          radioButtons("sens_metric", "Impact on", c("Gross margin" = "GM", "Net sales" = "Net_Sales"), inline = TRUE),
          uiOutput("sens_headline")
        ),
        plotlyOutput("tornado_chart", height = "420px"),
        DTOutput("sens_table", fill = FALSE)
      ),
      nav_panel(
        "Model accuracy",
        p(class = "method-note",
          "Each statistical method is refitted on history excluding the latest 6 months, then scored on how well it ",
          "predicted total volume over those months. WAPE (weighted absolute % error) is the headline measure; ",
          "bias shows systematic over- (+) or under- (−) forecasting."),
        uiOutput("backtest_headline"),
        layout_columns(
          col_widths = c(5, 7),
          DTOutput("backtest_table", fill = FALSE),
          plotlyOutput("backtest_chart", height = "360px")
        )
      )
    )
  ),

  nav_panel(
    "Versions", value = "versions", icon = bs_icon("clock-history"),
    uiOutput("storage_note"),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Save current plan"),
        card_body(
          textInput("version_name", "Version name", placeholder = "e.g. FY27 Budget v1"),
          textAreaInput("version_notes", "Notes", rows = 3, placeholder = "What changed, key assumptions, who asked for it"),
          selectInput("version_status", "Status", VERSION_STATUSES, selected = "Draft"),
          actionButton("save_version", "Save version", icon = icon("floppy-disk"), class = "btn-primary w-100"),
          tags$small(class = "text-muted d-block mt-2",
                     "Saves the data, assumptions, monthly plan, method, horizon, GST basis and scenario levers ",
                     "exactly as they are now. Saved versions never change; save again to record a new one.")
        )
      ),
      card(
        card_header("Saved versions", tags$small(class = "text-muted ms-2", "select one to load, two or more to compare")),
        card_body(
          div(class = "d-flex flex-wrap gap-2 mb-2 align-items-end",
              actionButton("load_version", "Load", icon = icon("folder-open"), class = "btn-primary btn-sm"),
              div(class = "d-flex gap-1 align-items-end",
                  selectInput("new_status", NULL, VERSION_STATUSES, width = "140px"),
                  actionButton("apply_status", "Set status", class = "btn-outline-secondary btn-sm mb-3")),
              actionButton("delete_version", "Delete", icon = icon("trash"), class = "btn-outline-danger btn-sm")),
          DTOutput("versions_table", fill = FALSE)
        )
      )
    ),
    uiOutput("version_compare")
  ),

  nav_panel(
    "Methodology", value = "method", icon = bs_icon("book"),
    card(card_body(
      h4("How the numbers are built"),
      h6("1. Revenue-to-margin waterfall (per partner × category × month)"),
      tags$table(class = "table table-sm w-auto",
        tags$tbody(
          tags$tr(tags$td("MRP billing"), tags$td("Supply_Qty × MRP")),
          tags$tr(tags$td("Gross billing"), tags$td("MRP billing − Commission (MRP billing × Commission_Pct)")),
          tags$tr(tags$td("Gross sales"), tags$td("Gross billing − Returns (Gross billing × Return_Pct)")),
          tags$tr(tags$td("GST"), tags$td("Gross sales × GST_Pct / (1 + GST_Pct), since MRP is tax-inclusive (legacy option: Gross sales × GST_Pct)")),
          tags$tr(tags$td("Net sales"), tags$td("Gross sales − GST")),
          tags$tr(tags$td("Cost of goods"), tags$td("(RM + PM + MFG per unit) × Supply_Qty")),
          tags$tr(tags$td("Yield loss"), tags$td("Gross billing × Yield_Loss_Pct")),
          tags$tr(tags$td(tags$b("Gross margin")), tags$td(tags$b("Net sales − Cost of goods − Yield loss")))
        )),
      p("Ratios (GM %, average MRP, realisation per unit) are always recomputed from summed amounts, never averaged."),
      h6("2. Fiscal calendar"),
      p("April to March. Quarters are fiscal (Q1 = Apr–Jun). Column labels mark Actual (A), Forecast (F) or a mix (A+F)."),
      h6("3. Volume forecast"),
      tags$ul(lapply(names(METHODS), function(n) tags$li(tags$b(n), ": ", METHOD_NOTES[[METHODS[[n]]]]))),
      p("The forecast always starts the month after the latest actual, so no months are skipped. Prediction ranges ",
        "for rupee amounts scale the volume range by each month's per-unit economics; portfolio ranges add series ranges, ",
        "which assumes series move together and is therefore conservative."),
      h6("4. Prices, costs and plans"),
      p("Base drivers come from each series' latest actual month. Annual price hikes and cost inflation step up at the start ",
        "of each fiscal year. A monthly plan file, if loaded, overrides drivers for the months and series it covers, and its ",
        "optional Supply_Qty column fixes volume outright."),
      h6("5. Scenarios and sensitivity"),
      p("Upside and Downside apply volume, price and unit-cost multipliers plus commission and returns shifts (percentage points) ",
        "to the Base driver set. Sensitivity flexes one driver at a time by the chosen percentage and holds everything else constant."),
      h6("6. Data handling"),
      p("Uploads are validated (columns, dates, numeric types, percentage scale, gaps, duplicates). Multiple rows in a month are ",
        "consolidated with drivers back-solved so that amounts reconcile exactly."),
      tags$hr(),
      tags$small(class = "text-muted", DISCLAIMER)
    ))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Fiscal year: April–March")),

  footer = div(class = "app-footer", DISCLAIMER)
)

# ---- Server ------------------------------------------------------------------------

server <- function(input, output, session) {
  state <- reactiveValues(
    clean = NULL, source = NULL, issues = NULL,
    plan = NULL, plan_name = NULL, plan_issues = NULL,
    assumptions = NULL, assumptions_rev = 0,
    version = NULL
  )
  versions_rev <- reactiveVal(0)
  current_user <- session$user %||% session$request$HTTP_X_FORWARDED_USER %||% "anonymous"
  model_cache <- new.env()

  # ---- Loading -------------------------------------------------------------------

  read_upload <- function(path, name) {
    if (tolower(tools::file_ext(name)) == "csv") {
      utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      readxl::read_excel(path)
    }
  }

  # Install a validated dataset and reset everything derived from it.
  # Returns the monthly consolidation so callers can build assumptions.
  apply_dataset <- function(clean, issues, source_name, gst_mode = input$gst_mode, horizon = NULL) {
    rm(list = ls(model_cache), envir = model_cache)
    state$clean <- clean
    state$issues <- issues
    state$source <- source_name
    state$plan <- NULL
    state$plan_name <- NULL
    state$plan_issues <- NULL
    state$version <- NULL

    m <- consolidate_monthly(clean, gst_mode)
    for (id in c("f_partner", "f_category", "horizon")) freezeReactiveValue(input, id)
    updateSelectizeInput(session, "f_partner", choices = sort(unique(m$Partner_Category)), selected = character(0))
    updateSelectizeInput(session, "f_category", choices = sort(unique(m$Category)), selected = character(0))
    hc <- horizon_choices(max(m$Date) %m+% months(1))
    updateSelectInput(session, "horizon", choices = hc,
                      selected = if (!is.null(horizon) && horizon %in% hc) horizon else hc[2])
    m
  }

  load_actuals <- function(raw, source_name) {
    res <- validate_actuals(raw)
    state$issues <- res$issues
    if (is.null(res$data)) {
      showNotification("The file failed validation. See the data quality report.", type = "error", duration = 8)
      return(invisible(FALSE))
    }
    m <- apply_dataset(res$data, res$issues, source_name)
    state$assumptions <- default_assumptions(m)
    state$assumptions_rev <- state$assumptions_rev + 1
    showNotification(sprintf("Loaded %s: %d series, actuals to %s.", source_name,
                             nrow(state$assumptions), format(max(m$Date), "%b-%Y")), type = "message")
    invisible(TRUE)
  }

  observeEvent(input$file_upload, {
    f <- input$file_upload
    tryCatch(load_actuals(read_upload(f$datapath, f$name), f$name),
             error = function(e) showNotification(paste("Could not read file:", conditionMessage(e)), type = "error"))
  })

  load_sample <- function() {
    load_actuals(make_sample_data(), "Sample dataset (synthetic)")
    nav_select("main_nav", "overview")
  }
  observeEvent(input$load_sample, load_sample())
  observeEvent(input$load_sample_hero, load_sample())

  output$download_template <- downloadHandler(
    filename = "growth_forecaster_template.xlsx",
    content = function(file) writexl::write_xlsx(list(Data = actuals_template(), Guide = template_guide()), file)
  )

  # ---- Core reactive pipeline --------------------------------------------------------

  has_data <- reactive(!is.null(state$clean))
  need_data <- function() {
    validate(need(has_data(), "Load your data, or the sample dataset, on the Data tab to begin."))
  }

  monthly <- reactive({
    req(state$clean)
    consolidate_monthly(state$clean, input$gst_mode)
  })
  actuals <- reactive(compute_pnl(monthly(), input$gst_mode) %>% mutate(Type = "Actual"))
  last_actual <- reactive(max(monthly()$Date))

  assumptions_d <- debounce(reactive(state$assumptions), 600)

  base_fc <- reactive({
    req(monthly(), assumptions_d(), input$horizon, input$method)
    withProgress(message = "Building forecast", value = 0.4, {
      build_forecast(monthly(), assumptions_d(), input$method, as.Date(input$horizon), state$plan, model_cache)
    })
  })

  read_levers <- function(prefix, defaults) {
    val <- function(name, default) {
      v <- input[[paste0(prefix, "_", name)]]
      if (is.null(v) || is.na(v)) default else v / 100
    }
    list(volume = val("volume", defaults$volume), price = val("price", defaults$price),
         cost = val("cost", defaults$cost), commission_pp = val("commission", defaults$commission_pp),
         returns_pp = val("returns", defaults$returns_pp))
  }
  levers <- reactive(list(
    Base = SCENARIO_DEFAULTS$Base,
    Upside = read_levers("up", SCENARIO_DEFAULTS$Upside),
    Downside = read_levers("down", SCENARIO_DEFAULTS$Downside)
  ))

  scenario_frames <- reactive({
    fc <- base_fc()
    lapply(levers(), function(l) price_forecast(apply_scenario(fc, l), input$gst_mode))
  })

  in_filter <- function(df) {
    keep <- rep(TRUE, nrow(df))
    if (length(input$f_partner)) keep <- keep & df$Partner_Category %in% input$f_partner
    if (length(input$f_category)) keep <- keep & df$Category %in% input$f_category
    keep
  }
  filter_rows <- function(df) df[in_filter(df), , drop = FALSE]

  act_view <- reactive(filter_rows(actuals()))
  scenario_views <- reactive(lapply(scenario_frames(), filter_rows))
  fc_view <- reactive(scenario_views()[[input$scenario]])
  combined_view <- reactive(bind_rows(act_view(), fc_view()))

  unit_div <- reactive(as.numeric(input$unit))
  unit_label <- reactive(trimws(paste(INR, UNIT_SHORT[[input$unit]])))
  unit_digits <- reactive(if (input$unit == "1") 0 else 2)

  partner_colors <- reactive(entity_colors(monthly()$Partner_Category))
  category_colors <- reactive(entity_colors(monthly()$Category))

  forecast_window <- reactive({
    fc <- base_fc()
    if (!nrow(fc)) return(NULL)
    range(fc$Date)
  })
  window_label <- reactive({
    w <- forecast_window()
    if (is.null(w)) return("")
    sprintf("%s to %s (%d months)", format(w[1], "%b-%y"), format(w[2], "%b-%y"), n_months_between(w[1], w[2]) + 1L)
  })

  # ---- Sidebar status --------------------------------------------------------------

  output$data_status <- renderUI({
    if (!has_data()) {
      return(div(class = "sidebar-status", bs_icon("info-circle"), " No data loaded. Use the ", tags$b("Data"),
                 " tab or the sample dataset."))
    }
    div(class = "sidebar-status",
        div(tags$span(class = "label", "Source: "), tags$b(state$source)),
        div(tags$span(class = "label", "Latest actual: "), tags$b(format(last_actual(), "%b-%Y"))),
        div(tags$span(class = "label", "Series: "), tags$b(nrow(state$assumptions))),
        div(tags$span(class = "label", "Forecast: "), tags$b(window_label())),
        if (!is.null(state$plan)) div(bs_icon("calendar-check"), " Monthly plan active"),
        if (!is.null(state$version)) {
          div(bs_icon("clock-history"), " Based on ", tags$b(state$version$name),
              tags$span(class = "text-muted", sprintf(" (%s)", state$version$status)))
        })
  })

  # ---- Overview ----------------------------------------------------------------------

  delta_tag <- function(change, kind = "pct") {
    if (is.null(change) || is.na(change)) {
      return(tags$small(class = "text-muted", "No full prior-year comparison"))
    }
    up <- change >= 0
    txt <- if (kind == "pct") fmt_pct(change, 1, signed = TRUE) else sprintf("%+.0f bps", change * 1e4)
    tags$small(class = "kpi-delta",
               tags$span(class = if (up) "kpi-up" else "kpi-down",
                         bs_icon(if (up) "arrow-up-right" else "arrow-down-right"), txt),
               tags$span(class = "text-muted", "vs same months last year"))
  }

  output$kpis <- renderUI({
    if (!has_data()) {
      return(card(class = "hero", card_body(
        h2("Turn sales history into a board-ready P&L forecast"),
        p(class = "lead text-muted",
          "Upload monthly sales by partner and category to get a fiscal-year P&L, statistical volume forecasts ",
          "with prediction ranges, Base/Upside/Downside scenarios, driver sensitivity and an Excel board pack."),
        div(class = "d-flex gap-2",
            actionButton("load_sample_hero", "Explore with sample data", icon = icon("flask"), class = "btn-primary btn-lg"),
            actionButton("go_data", "Upload my data", icon = icon("upload"), class = "btn-outline-primary btn-lg"))
      )))
    }
    fc <- fc_view()
    validate(need(nrow(fc) > 0, "No forecast rows for the current filters and horizon."))
    cur <- summarise_pnl(fc)
    fc_months <- unique(fc$Date)
    prior_rows <- combined_view() %>% filter(Date %in% (fc_months %m-% years(1)))
    pri <- if (n_distinct(prior_rows$Date) == length(fc_months)) summarise_pnl(prior_rows) else NULL
    growth <- function(col) if (is.null(pri)) NA else safe_div(cur[[col]], pri[[col]]) - 1
    subtitle <- tags$small(class = "text-muted d-block", window_label(), " · ", input$scenario)

    layout_column_wrap(
      width = "240px", class = "mb-3",
      value_box("Forecast net sales", fmt_compact(cur$Net_Sales), delta_tag(growth("Net_Sales")), subtitle,
                showcase = bs_icon("currency-rupee"), showcase_layout = "top right"),
      value_box("Gross margin", fmt_compact(cur$GM), delta_tag(growth("GM")), subtitle,
                showcase = bs_icon("cash-stack"), showcase_layout = "top right"),
      value_box("Gross margin %", fmt_pct(cur$GM_Pct),
                delta_tag(if (is.null(pri)) NA else cur$GM_Pct - pri$GM_Pct, "bps"), subtitle,
                showcase = bs_icon("percent"), showcase_layout = "top right"),
      value_box("Volume", paste(fmt_compact(cur$Supply_Qty, currency = FALSE), "units"),
                delta_tag(growth("Supply_Qty")), subtitle, showcase = bs_icon("box-seam"), showcase_layout = "top right")
    )
  })
  observeEvent(input$go_data, nav_select("main_nav", "data"))

  output$trajectory_chart <- renderPlotly({
    need_data()
    u <- unit_div()
    a <- summarise_pnl(act_view(), Date)
    f <- summarise_pnl(fc_view(), Date)
    validate(need(nrow(a) > 0, "No data for the current filters."))
    join <- bind_rows(tail(a, 1), f)  # connect forecast line to the last actual point
    hover <- function(label) paste0(label, ": ", INR, "%{y:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")

    p <- plot_ly()
    if (nrow(f) && !all(is.na(f$Net_Sales_Lo95))) {
      p <- p %>%
        add_ribbons(data = f, x = ~Date, ymin = ~Net_Sales_Lo95 / u, ymax = ~Net_Sales_Hi95 / u, name = "95% range",
                    fillcolor = "rgba(42,120,214,0.10)", line = list(width = 0), hoverinfo = "skip") %>%
        add_ribbons(data = f, x = ~Date, ymin = ~Net_Sales_Lo80 / u, ymax = ~Net_Sales_Hi80 / u, name = "80% range",
                    fillcolor = "rgba(42,120,214,0.20)", line = list(width = 0), hoverinfo = "skip")
    }
    p <- p %>%
      add_lines(data = a, x = ~Date, y = ~Net_Sales / u, name = "Net sales", line = list(color = SERIES[1], width = 2),
                hovertemplate = hover("Net sales")) %>%
      add_lines(data = a, x = ~Date, y = ~GM / u, name = "Gross margin", line = list(color = SERIES[2], width = 2),
                hovertemplate = hover("Gross margin"))
    if (nrow(f)) {
      p <- p %>%
        add_lines(data = join, x = ~Date, y = ~Net_Sales / u, name = "Net sales (forecast)",
                  line = list(color = SERIES[1], width = 2, dash = "dash"), hovertemplate = hover("Net sales F")) %>%
        add_lines(data = join, x = ~Date, y = ~GM / u, name = "Gross margin (forecast)",
                  line = list(color = SERIES[2], width = 2, dash = "dash"), hovertemplate = hover("Gross margin F"))
    }
    start <- last_actual() %m+% days(15)
    p %>%
      style_plot(y_title = unit_label()) %>%
      layout(xaxis = list(hoverformat = "%b %Y"), yaxis = list(rangemode = "tozero"),
             shapes = list(list(type = "line", x0 = start, x1 = start, y0 = 0, y1 = 1, yref = "paper",
                                line = list(color = INK$axis, dash = "dot", width = 1))),
             annotations = list(list(x = start, y = 1, yref = "paper", text = "Forecast →", showarrow = FALSE,
                                     xanchor = "left", font = list(color = INK$muted, size = 11))))
  })

  fy_partner <- reactive({
    combined_view() %>%
      mutate(Period = period_start(Date, "year")) %>%
      group_by(Period) %>%
      mutate(Basis = paste(sort(unique(substr(Type, 1, 1))), collapse = "+")) %>%
      ungroup() %>%
      summarise_pnl(Period, Basis, Partner_Category) %>%
      mutate(Label = paste0(fy_label(Period), "<br>", Basis))
  })

  output$mix_chart <- renderPlotly({
    need_data()
    d <- fy_partner()
    validate(need(nrow(d) > 0, "No data for the current filters."))
    u <- unit_div()
    plot_ly(d, x = ~Label, y = ~Net_Sales / u, color = ~Partner_Category, colors = partner_colors(), type = "bar",
            marker = list(line = list(color = "#FFFFFF", width = 1)),
            hovertemplate = paste0("%{fullData.name}: ", INR, "%{y:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")) %>%
      style_plot(y_title = unit_label(), x_type = "category") %>%
      layout(barmode = "stack", xaxis = list(categoryorder = "array", categoryarray = unique(d$Label)))
  })

  output$gm_partner_chart <- renderPlotly({
    need_data()
    d <- fy_partner()
    validate(need(nrow(d) > 0, "No data for the current filters."))
    plot_ly(d, x = ~Label, y = ~GM_Pct, color = ~Partner_Category, colors = partner_colors(), type = "bar",
            hovertemplate = "%{fullData.name}: %{y:.1%}<extra></extra>") %>%
      style_plot(y_format = ".0%", x_type = "category") %>%
      layout(barmode = "group", xaxis = list(categoryorder = "array", categoryarray = unique(d$Label)))
  })

  output$bridge_chart <- renderPlotly({
    need_data()
    fc <- fc_view()
    validate(need(nrow(fc) > 0, "No forecast for the current filters."))
    s <- summarise_pnl(fc)
    u <- unit_div()
    steps <- c("MRP billing" = s$MRP_Billing, "Commission" = -s$Commission, "Returns" = -s$Return, "GST" = -s$GST,
               "Net sales" = 0, "Raw material" = -s$RM, "Packing" = -s$PM, "Manufacturing" = -s$MFG,
               "Yield loss" = -s$Yield_Loss, "Gross margin" = 0)
    measure <- c("absolute", "relative", "relative", "relative", "total", "relative", "relative", "relative",
                 "relative", "total")
    shown <- c(s$MRP_Billing, steps[2:4], s$Net_Sales, steps[6:9], s$GM)
    labels <- formatC(abs(shown) / u, format = "f", digits = unit_digits(), big.mark = ",")
    plot_ly(type = "waterfall", x = names(steps), y = unname(steps) / u, measure = measure,
            text = labels, textposition = "outside", textfont = list(color = INK$secondary, size = 11),
            cliponaxis = FALSE,
            hovertemplate = paste0("%{x}: ", INR, "%{text} ", UNIT_SHORT[[input$unit]], "<extra></extra>"),
            connector = list(line = list(color = INK$axis, width = 1)),
            increasing = list(marker = list(color = SERIES[1])),
            decreasing = list(marker = list(color = "#B9C2CC")),
            totals = list(marker = list(color = NAVY))) %>%
      style_plot(y_title = unit_label(), x_type = "category") %>%
      layout(showlegend = FALSE, hovermode = "closest",
             xaxis = list(categoryorder = "array", categoryarray = names(steps)))
  })

  # ---- Data tab ----------------------------------------------------------------------

  output$issues_table <- renderDT({
    validate(need(!is.null(state$issues), "Upload a file to see validation results."))
    issues_table(state$issues)
  })

  output$explorer_chart <- renderPlotly({
    need_data()
    df <- act_view()
    validate(need(nrow(df) > 0, "No data for the current filters."))
    grain <- input$ex_grain
    metric <- input$ex_metric
    by <- input$ex_by
    df$Period <- period_start(df$Date, grain)
    groups <- if (by == "Total") "Period" else c("Period", by)
    agg <- summarise_pnl(df, pick(all_of(groups))) %>%
      left_join(df %>% group_by(Period) %>% summarise(Months = n_distinct(Date), .groups = "drop"), by = "Period") %>%
      mutate(Label = paste0(period_label(Period, grain), ifelse(grain != "month" & Months < ifelse(grain == "year", 12, 3),
                                                                 paste0(" (", Months, " mo, partial)"), "")))
    is_amt <- metric %in% AMOUNT_COLS
    agg$y <- if (is_amt) agg[[metric]] / unit_div() else agg[[metric]]
    y_title <- if (is_amt) unit_label() else if (metric == "Supply_Qty") "Units" else if (metric == "GM_Pct") NULL else paste(INR, "per unit")
    y_fmt <- if (metric == "GM_Pct") ".1%" else NULL
    name <- names(EXPLORER_METRICS)[EXPLORER_METRICS == metric]

    p <- if (by == "Total") {
      plot_ly(agg, x = ~Period, y = ~y, type = "scatter", mode = "lines+markers", name = name, text = ~Label,
              line = list(color = SERIES[1], width = 2), marker = list(size = 8, color = SERIES[1]),
              hovertemplate = "%{text}: %{y:,.2f}<extra></extra>")
    } else {
      cols <- if (by == "Partner_Category") partner_colors() else category_colors()
      plot_ly(agg, x = ~Period, y = ~y, color = agg[[by]], colors = cols, type = "scatter", mode = "lines+markers",
              text = ~Label, line = list(width = 2), marker = list(size = 8),
              hovertemplate = "%{fullData.name} %{text}: %{y:,.2f}<extra></extra>")
    }
    p %>% style_plot(y_title = y_title, y_format = y_fmt) %>% layout(hovermode = "closest")
  })

  output$actuals_table <- renderDT({
    need_data()
    df <- act_view() %>%
      select(Date, Partner_Category, Category, Supply_Qty, MRP, MRP_Billing, Gross_Billing, Net_Sales, COGS,
             Yield_Loss, GM, GM_Pct, Commission_Pct, Return_Pct, GST_Pct) %>%
      arrange(desc(Date), Partner_Category, Category) %>%
      mutate(Date = format(Date, "%b-%Y"))
    dt_table(df, inr = c("MRP_Billing", "Gross_Billing", "Net_Sales", "COGS", "Yield_Loss", "GM"),
             qty = "Supply_Qty", num = "MRP", pct = c("GM_Pct", "Commission_Pct", "Return_Pct", "GST_Pct"))
  })

  # ---- Assumptions tab -----------------------------------------------------------------

  output$method_note <- renderUI({
    tagList(
      p(class = "method-note mb-1", tags$b(method_label(input$method)), ": ", METHOD_NOTES[[input$method]]),
      if (has_data() && input$method %in% c("ets", "arima")) {
        used <- sort(table(base_fc()$Method_Used[!duplicated(paste(base_fc()$Partner_Category, base_fc()$Category))]),
                     decreasing = TRUE)
        p(class = "method-note mb-0", "Models selected: ",
          paste(sprintf("%s (%d)", names(used), used), collapse = ", "))
      }
    )
  })

  output$assumptions_table <- renderRHandsontable({
    state$assumptions_rev
    a <- isolate(state$assumptions)
    validate(need(!is.null(a), "Load data to generate default assumptions."))
    rhandsontable(a, rowHeaders = NULL, stretchH = "all", height = min(520, 60 + 25 * nrow(a))) %>%
      hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
      hot_col(c("Partner_Category", "Category", "Last_Qty"), readOnly = TRUE) %>%
      hot_col(c("Last_Qty", "Manual_Qty"), format = "0,0") %>%
      hot_col(PCT_ASSUMPTIONS, format = "0.00%") %>%
      hot_col(c("MRP", "RM_per_unit", "PM_per_unit", "MFG_per_unit"), format = "0,0.00")
  })

  observeEvent(input$assumptions_table, {
    df <- hot_to_r(input$assumptions_table)
    if (!is.null(df) && !is.null(state$assumptions) && nrow(df) == nrow(state$assumptions)) {
      state$assumptions <- as.data.frame(df)
    }
  })

  observeEvent(input$bulk_apply, {
    a <- state$assumptions
    req(a, !is.na(input$bulk_val))
    val <- if (input$bulk_col %in% PCT_ASSUMPTIONS) input$bulk_val / 100 else input$bulk_val
    rows <- in_filter(a)
    a[[input$bulk_col]][rows] <- val
    state$assumptions <- a
    state$assumptions_rev <- state$assumptions_rev + 1
    showNotification(sprintf("%s set for %d row(s).", input$bulk_col, sum(rows)), type = "message")
  })

  observeEvent(input$reset_assumptions, {
    req(has_data())
    state$assumptions <- default_assumptions(monthly())
    state$assumptions_rev <- state$assumptions_rev + 1
  })

  output$download_plan_template <- downloadHandler(
    filename = function() sprintf("monthly_plan_template_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      validate(need(has_data(), "Load data first."))
      writexl::write_xlsx(plan_template(monthly(), state$assumptions, as.Date(input$horizon)), file)
    }
  )

  observeEvent(input$plan_upload, {
    req(has_data())
    f <- input$plan_upload
    res <- tryCatch(validate_plan(read_upload(f$datapath, f$name), state$assumptions),
                    error = function(e) list(data = NULL, issues = data.frame(Severity = "Error", Check = "File",
                                                                              Detail = conditionMessage(e))))
    state$plan_issues <- res$issues
    if (is.null(res$data)) {
      showNotification("The monthly plan failed validation.", type = "error")
      return()
    }
    state$plan <- res$data
    state$plan_name <- f$name
    showNotification("Monthly plan applied.", type = "message")
  })

  observeEvent(input$clear_plan, {
    state$plan <- NULL
    state$plan_name <- NULL
    state$plan_issues <- NULL
  })

  output$plan_status <- renderUI({
    if (is.null(state$plan)) {
      return(p(class = "text-muted small mb-0", "No plan loaded. The assumptions table drives every forecast month."))
    }
    p(class = "small mb-1", bs_icon("calendar-check"), " ", tags$b(state$plan_name), sprintf(
      ": %d rows active. Drivers in the plan override the assumptions table for those months.", nrow(state$plan)))
  })
  output$plan_issues <- renderDT(issues_table(state$plan_issues))

  # ---- Scenarios tab ----------------------------------------------------------------------

  set_lever_inputs <- function(all_levers) {
    for (prefix in c("up", "down")) {
      d <- all_levers[[if (prefix == "up") "Upside" else "Downside"]]
      updateNumericInput(session, paste0(prefix, "_volume"), value = 100 * d$volume)
      updateNumericInput(session, paste0(prefix, "_price"), value = 100 * d$price)
      updateNumericInput(session, paste0(prefix, "_cost"), value = 100 * d$cost)
      updateNumericInput(session, paste0(prefix, "_commission"), value = 100 * d$commission_pp)
      updateNumericInput(session, paste0(prefix, "_returns"), value = 100 * d$returns_pp)
    }
  }
  observeEvent(input$reset_levers, set_lever_inputs(SCENARIO_DEFAULTS))

  output$scenario_table <- renderDT({
    need_data()
    s <- scenario_summary(scenario_views())
    u <- unit_div()
    d <- unit_digits()
    rows <- data.frame(
      Metric = c("Volume (units)", paste0("Net sales (", unit_label(), ")"), paste0("Gross margin (", unit_label(), ")"),
                 "Gross margin %", "Net sales vs Base", paste0("Gross margin vs Base (", unit_label(), ")")),
      stringsAsFactors = FALSE
    )
    for (i in seq_len(nrow(s))) {
      rows[[s$Scenario[i]]] <- c(fmt_indian(s$Volume[i]), fmt_indian(s$Net_Sales[i] / u, d), fmt_indian(s$GM[i] / u, d),
                                 fmt_pct(s$GM_Pct[i]), fmt_pct(s$Net_Sales_vs_Base[i], 1, signed = TRUE),
                                 fmt_indian(s$GM_vs_Base[i] / u, d))
    }
    DT::datatable(rows, rownames = FALSE, class = "compact",
                  options = list(dom = "t", ordering = FALSE,
                                 columnDefs = list(list(targets = 1:3, className = "dt-right")))) %>%
      formatStyle(input$scenario, fontWeight = "700", backgroundColor = "#F1F4F8")
  })

  output$scenario_chart <- renderPlotly({
    need_data()
    u <- unit_div()
    a <- summarise_pnl(act_view(), Date) %>% filter(Date > last_actual() %m-% months(24))
    p <- plot_ly() %>%
      add_lines(data = a, x = ~Date, y = ~Net_Sales / u, name = "Actual", line = list(color = INK$secondary, width = 2))
    for (s in names(scenario_views())) {
      f <- bind_rows(tail(a, 1), summarise_pnl(scenario_views()[[s]], Date))
      p <- p %>% add_lines(data = f, x = ~Date, y = ~Net_Sales / u, name = s,
                           line = list(color = SCENARIO_COLORS[[s]], width = 2, dash = if (s == "Base") "solid" else "dash"))
    }
    p %>% style_plot(y_title = unit_label()) %>% layout(xaxis = list(hoverformat = "%b %Y"))
  })

  output$scenario_fy_chart <- renderPlotly({
    need_data()
    u <- unit_div()
    d <- bind_rows(lapply(names(scenario_views()), function(s) {
      bind_rows(act_view(), scenario_views()[[s]]) %>%
        mutate(Period = period_start(Date, "year")) %>%
        filter(Period >= period_start(last_actual() %m+% months(1), "year")) %>%
        summarise_pnl(Period) %>%
        mutate(Scenario = s)
    })) %>% mutate(Label = fy_label(Period), Scenario = factor(Scenario, levels = names(SCENARIO_COLORS)))
    validate(need(nrow(d) > 0, "No forecast for the current filters."))
    plot_ly(d, x = ~Label, y = ~GM / u, color = ~Scenario, colors = SCENARIO_COLORS, type = "bar",
            hovertemplate = paste0("%{fullData.name}: ", INR, "%{y:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")) %>%
      style_plot(y_title = unit_label(), x_type = "category") %>%
      layout(barmode = "group")
  })

  # ---- P&L statement tab ----------------------------------------------------------------

  statement <- reactive({
    need_data()
    build_statement(combined_view(), input$st_grain)
  })

  output$statement_caption <- renderUI({
    req(statement())
    p(class = "text-muted small",
      sprintf("Amounts in %s except volume and per-unit lines (%s). Scenario: %s. A = actual, F = forecast, A+F = mixed period.",
              unit_label(), INR, input$scenario))
  })

  output$statement_table <- renderDT({
    st <- statement()
    validate(need(!is.null(st), "No data for the current filters."))
    u <- unit_div()
    digits <- unit_digits()
    vals <- st$table[-1]
    disp <- data.frame(Line = ifelse(st$lines$style %in% c("total", "subtotal") | st$lines$kind == "qty",
                                     st$lines$line, paste0("   ", st$lines$line)),
                       stringsAsFactors = FALSE)
    for (j in seq_along(vals)) {
      col <- vals[[j]]
      disp[[names(vals)[j]]] <- vapply(seq_along(col), function(i) {
        switch(st$lines$kind[i],
               amt = fmt_indian(col[i] / u, digits),
               qty = fmt_indian(col[i]),
               pct = fmt_pct(col[i]),
               unit = fmt_indian(col[i], 2))
      }, character(1))
    }
    disp$style <- st$lines$style
    n <- ncol(disp)
    dt <- DT::datatable(disp, rownames = FALSE, class = "compact statement-table", extensions = "FixedColumns",
                        options = list(dom = "t", ordering = FALSE, paging = FALSE, scrollX = TRUE,
                                       fixedColumns = list(leftColumns = 1),
                                       columnDefs = list(list(targets = n - 1, visible = FALSE),
                                                         list(targets = seq_len(n - 2), className = "dt-right"))))
    fc_cols <- grep("F$", names(disp), value = TRUE)
    if (length(fc_cols)) dt <- formatStyle(dt, fc_cols, backgroundColor = "#F5F8FC")
    dt %>%
      formatStyle(names(disp)[-n], valueColumns = "style",
                  fontWeight = styleEqual(c("total", "subtotal"), c("700", "600")),
                  borderTop = styleEqual("total", paste("1px solid", NAVY)),
                  color = styleEqual("ratio", INK$secondary))
  })

  export_cover <- function() {
    filters <- c(if (length(input$f_partner)) paste("Partners:", paste(input$f_partner, collapse = ", ")),
                 if (length(input$f_category)) paste("Categories:", paste(input$f_category, collapse = ", ")))
    c("Prepared on" = format(Sys.time(), "%d %b %Y %H:%M"),
      "Plan version" = if (is.null(state$version)) "Unsaved working copy" else {
        if (identical(state$version$status, "Approved")) paste0(state$version$name, " (Approved)")
        else paste0(state$version$name, " (", state$version$status, "; edits after loading are not saved)")
      },
      "Data source" = state$source,
      "Latest actual month" = format(last_actual(), "%b-%Y"),
      "Forecast period" = window_label(),
      "Volume method" = method_label(input$method),
      "Scenario (P&L sheets)" = input$scenario,
      "GST treatment" = names(GST_MODES)[GST_MODES == input$gst_mode],
      "Monthly plan" = state$plan_name %||% "None",
      "Filters" = if (length(filters)) paste(filters, collapse = " | ") else "None (all series)",
      "Amounts" = "Indian Rupees, unscaled",
      "Disclaimer" = DISCLAIMER)
  }

  output$download_statement <- downloadHandler(
    filename = function() sprintf("PnL_statement_%s_%s.xlsx", input$st_grain, format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      write_board_pack(file, export_cover(), statements = stats::setNames(list(statement()), "P&L Statement"))
    }
  )

  forecast_detail <- reactive({
    fc_view() %>%
      select(Date, Partner_Category, Category, Method_Used, Supply_Qty, Qty_Lo95, Qty_Hi95, MRP,
             Commission_Pct, Return_Pct, RM_per_unit, PM_per_unit, MFG_per_unit, Net_Sales, GM, GM_Pct) %>%
      arrange(Date, Partner_Category, Category)
  })

  output$forecast_table <- renderDT({
    need_data()
    df <- forecast_detail()
    validate(need(nrow(df) > 0, "No forecast for the current filters."))
    df$Date <- format(df$Date, "%b-%Y")
    dt_table(df, inr = c("Net_Sales", "GM"), qty = c("Supply_Qty", "Qty_Lo95", "Qty_Hi95"),
             num = c("MRP", "RM_per_unit", "PM_per_unit", "MFG_per_unit"),
             pct = c("Commission_Pct", "Return_Pct", "GM_Pct"))
  })

  output$download_forecast <- downloadHandler(
    filename = function() sprintf("forecast_detail_%s_%s.xlsx", tolower(input$scenario), format(Sys.Date(), "%Y%m%d")),
    content = function(file) writexl::write_xlsx(forecast_detail(), file)
  )

  # ---- Analysis tab ------------------------------------------------------------------------

  sensitivity <- reactive({
    need_data()
    sensitivity_table(fc_view(), input$gst_mode, input$sens_shock / 100, input$sens_metric)
  })

  output$sens_headline <- renderUI({
    s <- sensitivity()
    req(s)
    metric <- if (input$sens_metric == "GM") "gross margin" else "net sales"
    p(class = "method-note pt-4",
      sprintf("Base forecast %s: %s. The biggest lever is ", metric, fmt_compact(s$Base[1])),
      tags$b(s$Driver[1]), sprintf(": ±%d%% moves it by %s.", input$sens_shock, fmt_compact(abs(s$Impact_High[1]))))
  })

  output$tornado_chart <- renderPlotly({
    s <- sensitivity()
    validate(need(!is.null(s), "No forecast for the current filters."))
    u <- unit_div()
    s$Driver <- factor(s$Driver, levels = rev(s$Driver))
    shock <- input$sens_shock
    plot_ly(s, y = ~Driver) %>%
      add_bars(x = ~Impact_Low / u, orientation = "h", name = sprintf("Driver −%d%%", shock),
               marker = list(color = SERIES[2]),
               hovertemplate = paste0("%{y} −", shock, "%: ", INR, "%{x:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")) %>%
      add_bars(x = ~Impact_High / u, orientation = "h", name = sprintf("Driver +%d%%", shock),
               marker = list(color = SERIES[1]),
               hovertemplate = paste0("%{y} +", shock, "%: ", INR, "%{x:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")) %>%
      style_plot() %>%
      layout(barmode = "overlay", hovermode = "closest",
             xaxis = list(title = paste("Change vs base,", unit_label()), zeroline = TRUE, zerolinecolor = INK$axis,
                          showgrid = TRUE, gridcolor = INK$grid),
             yaxis = list(title = "", gridcolor = "rgba(0,0,0,0)"))
  })

  output$sens_table <- renderDT({
    s <- sensitivity()
    req(s)
    k <- input$sens_shock
    cols <- c(sprintf("At −%d%%", k), "Base", sprintf("At +%d%%", k),
              sprintf("Change at −%d%%", k), sprintf("Change at +%d%%", k))
    df <- s %>% select(Driver, Low, Base, High, Impact_Low, Impact_High)
    names(df) <- c("Driver", cols)
    dt_table(df, dom = "t", inr = cols)
  })

  backtest <- reactive({
    need_data()
    withProgress(message = "Back-testing forecasting methods", value = 0.3, backtest_methods(monthly()))
  })

  output$backtest_headline <- renderUI({
    bt <- backtest()
    if (is.null(bt)) {
      return(div(class = "alert alert-secondary", "Back-testing needs at least 18 months of history."))
    }
    best <- bt$metrics[1, ]
    div(class = "alert alert-light border d-flex align-items-center justify-content-between flex-wrap gap-2",
        div(bs_icon("trophy"), " Recommended: ", tags$b(best$Method_Label),
            sprintf(" with %s weighted error on the %d months after %s.",
                    fmt_pct(best$WAPE), bt$holdout, format(bt$cutoff, "%b-%Y"))),
        if (input$method != best$Method) {
          actionButton("use_recommended", "Use this method", class = "btn-primary btn-sm")
        } else tags$span(class = "badge bg-success", "In use"))
  })

  observeEvent(input$use_recommended, {
    updateSelectInput(session, "method", selected = backtest()$metrics$Method[1])
  })

  output$backtest_table <- renderDT({
    bt <- backtest()
    req(bt)
    dt_table(bt$metrics %>% select(Rank, Method = Method_Label, WAPE, MAPE, Bias), dom = "t",
             pct = c("WAPE", "MAPE", "Bias"))
  })

  output$backtest_chart <- renderPlotly({
    bt <- backtest()
    req(bt)
    actual <- bt$series %>% distinct(Date, Actual)
    p <- plot_ly() %>%
      add_lines(data = actual, x = ~Date, y = ~Actual, name = "Actual", line = list(color = INK$primary, width = 3))
    for (m in unique(bt$series$Method)) {
      p <- p %>% add_lines(data = bt$series %>% filter(Method == m), x = ~Date, y = ~Forecast,
                           name = method_label(m), line = list(color = SERIES[match(m, METHODS)], width = 2, dash = "dash"))
    }
    p %>% style_plot(y_title = "Units") %>% layout(xaxis = list(hoverformat = "%b %Y"))
  })

  # ---- Versions ------------------------------------------------------------------------------

  output$storage_note <- renderUI({
    if (store_is_ephemeral()) {
      div(class = "alert alert-warning d-flex gap-2 align-items-start", bs_icon("exclamation-triangle"),
          div(tags$b("Saved versions are temporary on shinyapps.io. "),
              "Its file system is reset whenever the app restarts or is redeployed. ",
              "Use the Docker deployment (deploy/DEPLOY.md) for permanent storage."))
    } else {
      p(class = "text-muted small", bs_icon("database"), " Versions are stored in ", store_description(), ".")
    }
  })

  # Whole-portfolio FY totals per scenario (sidebar filters ignored: a version is the full plan).
  fy_summary <- function() {
    bind_rows(lapply(names(scenario_frames()), function(s) {
      bind_rows(actuals(), scenario_frames()[[s]]) %>%
        mutate(Period = period_start(Date, "year")) %>%
        group_by(Period) %>%
        summarise(Net_Sales = sum(Net_Sales), GM = sum(GM),
                  Basis = paste(sort(unique(substr(Type, 1, 1))), collapse = "+"), .groups = "drop") %>%
        mutate(Scenario = s)
    }))
  }

  store_call <- function(f, action) {
    tryCatch(store(f), error = function(e) {
      showNotification(sprintf("Could not %s: %s", action, conditionMessage(e)), type = "error", duration = 10)
      NULL
    })
  }

  observeEvent(input$save_version, {
    if (!has_data()) {
      showNotification("Load data before saving a version.", type = "warning")
      return()
    }
    name <- trimws(input$version_name)
    if (!nzchar(name)) {
      showNotification("Give the version a name.", type = "warning")
      return()
    }
    base <- summarise_pnl(scenario_frames()$Base)
    snapshot <- list(
      source = state$source, clean = state$clean, issues = state$issues,
      assumptions = state$assumptions, plan = state$plan, plan_name = state$plan_name,
      settings = list(method = input$method, horizon = input$horizon, gst_mode = input$gst_mode, levers = levers()),
      fy_summary = fy_summary()
    )
    id <- store_call(function(con) {
      version_save(con, snapshot, name, input$version_notes, input$version_status, current_user,
                   kpis = list(net_sales = base$Net_Sales, gm = base$GM, gm_pct = base$GM_Pct))
    }, "save the version")
    if (is.null(id)) return()
    state$version <- list(id = id, name = name, status = input$version_status)
    updateTextInput(session, "version_name", value = "")
    updateTextAreaInput(session, "version_notes", value = "")
    versions_rev(versions_rev() + 1)
    showNotification(sprintf("Saved version '%s'.", name), type = "message")
  })

  versions <- reactive({
    versions_rev()
    invalidateLater(60000)  # pick up versions saved by other users
    store_call(version_list, "list versions")
  })
  selected_ids <- reactive({
    v <- versions()
    if (is.null(v)) character(0) else v$id[input$versions_table_rows_selected]
  })

  version_badge <- function(status) {
    cls <- c(Draft = "bg-secondary", "In review" = "bg-info", Approved = "bg-success", Archived = "bg-light text-dark border")
    sprintf('<span class="badge %s">%s</span>', cls[status], status)
  }

  output$versions_table <- renderDT({
    v <- versions()
    validate(need(!is.null(v) && nrow(v) > 0, "No saved versions yet. Save the current plan to start a history."))
    disp <- data.frame(
      Name = v$name,
      Status = version_badge(v$status),
      Saved = format_local_time(v$created_at),
      By = v$created_by,
      Method = method_label(v$method),
      Horizon = ifelse(is.na(v$horizon), "–", paste("to", fy_label(as.Date(v$horizon)))),
      "Net sales (Base)" = fmt_compact(v$net_sales),
      "GM (Base)" = fmt_compact(v$gm),
      "GM %" = fmt_pct(v$gm_pct),
      Notes = v$notes,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    DT::datatable(disp, rownames = FALSE, selection = "multiple", class = "compact hover",
                  escape = setdiff(names(disp), "Status"),
                  options = list(dom = "tip", pageLength = 10, ordering = FALSE, scrollX = TRUE))
  })

  # Snapshots never change, so decoded payloads can be cached for the session.
  version_cache <- new.env()
  get_version <- function(id) {
    if (!exists(id, envir = version_cache, inherits = FALSE)) {
      s <- store_call(function(con) version_get(con, id), "load the version")
      if (is.null(s)) return(NULL)
      assign(id, s, envir = version_cache)
    }
    get(id, envir = version_cache)
  }

  observeEvent(input$load_version, {
    ids <- selected_ids()
    if (length(ids) != 1) {
      showNotification("Select exactly one version to load.", type = "warning")
      return()
    }
    s <- get_version(ids)
    req(s)
    for (id in c("method", "gst_mode")) freezeReactiveValue(input, id)
    updateSelectInput(session, "method", selected = s$settings$method)
    updateSelectInput(session, "gst_mode", selected = s$settings$gst_mode)
    apply_dataset(s$clean, s$issues, s$source, gst_mode = s$settings$gst_mode, horizon = s$settings$horizon)
    state$assumptions <- s$assumptions
    state$assumptions_rev <- state$assumptions_rev + 1
    state$plan <- s$plan
    state$plan_name <- s$plan_name
    set_lever_inputs(s$settings$levers)
    state$version <- list(id = s$id, name = s$name, status = versions()$status[versions()$id == s$id])
    nav_select("main_nav", "overview")
    showNotification(sprintf("Loaded version '%s'.", s$name), type = "message")
  })

  observeEvent(input$apply_status, {
    ids <- selected_ids()
    if (!length(ids)) {
      showNotification("Select at least one version.", type = "warning")
      return()
    }
    done <- store_call(function(con) {
      for (id in ids) version_set_status(con, id, input$new_status, current_user)
      TRUE
    }, "update the status")
    if (is.null(done)) return()
    if (!is.null(state$version) && state$version$id %in% ids) {
      v <- state$version
      v$status <- input$new_status
      state$version <- v
    }
    versions_rev(versions_rev() + 1)
  })

  pending_delete <- reactiveVal(character(0))
  observeEvent(input$delete_version, {
    v <- versions()
    sel <- v[v$id %in% selected_ids(), , drop = FALSE]
    if (!nrow(sel)) {
      showNotification("Select at least one version.", type = "warning")
      return()
    }
    if (any(sel$status == "Approved")) {
      showNotification("Approved versions are locked. Change their status before deleting.", type = "warning")
      return()
    }
    pending_delete(sel$id)
    showModal(modalDialog(
      title = "Delete saved versions?",
      p(sprintf("This permanently deletes %d version(s):", nrow(sel))),
      tags$ul(lapply(sel$name, tags$li)),
      footer = tagList(modalButton("Cancel"), actionButton("confirm_delete", "Delete", class = "btn-danger"))
    ))
  })
  observeEvent(input$confirm_delete, {
    removeModal()
    ids <- pending_delete()
    done <- store_call(function(con) {
      for (id in ids) version_delete(con, id)
      TRUE
    }, "delete")
    if (is.null(done)) return()
    if (!is.null(state$version) && state$version$id %in% ids) state$version <- NULL
    pending_delete(character(0))
    versions_rev(versions_rev() + 1)
    showNotification(sprintf("Deleted %d version(s).", length(ids)), type = "message")
  })

  compare_data <- reactive({
    ids <- selected_ids()
    req(length(ids) >= 2)
    v <- versions()
    labels <- v$name[match(ids, v$id)]
    dup <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    labels[dup] <- paste0(labels[dup], " (", format_local_time(v$created_at[match(ids, v$id)][dup]), ")")
    bind_rows(lapply(seq_along(ids), function(i) {
      s <- get_version(ids[i])
      if (is.null(s)) return(NULL)
      s$fy_summary %>% filter(Scenario == "Base") %>% mutate(Version = labels[i])
    })) %>%
      mutate(Version = factor(Version, levels = labels), FY = fy_label(Period))
  })

  output$version_compare <- renderUI({
    if (length(selected_ids()) < 2) return(NULL)
    card(
      card_header("Version comparison", tags$small(class = "text-muted ms-2", "whole portfolio, Base scenario, by fiscal year")),
      card_body(
        radioButtons("compare_metric", NULL, c("Net sales" = "Net_Sales", "Gross margin" = "GM"), inline = TRUE),
        layout_columns(col_widths = c(7, 5),
                       plotlyOutput("compare_chart", height = "340px"),
                       DTOutput("compare_table", fill = FALSE))
      )
    )
  })

  output$compare_chart <- renderPlotly({
    d <- compare_data()
    metric <- input$compare_metric %||% "Net_Sales"
    u <- unit_div()
    cols <- stats::setNames(c(SERIES, rep(INK$muted, 99))[seq_along(levels(d$Version))], levels(d$Version))
    plot_ly(d, x = ~FY, y = d[[metric]] / u, color = ~Version, colors = cols, type = "bar",
            hovertemplate = paste0("%{fullData.name}: ", INR, "%{y:,.2f} ", UNIT_SHORT[[input$unit]], "<extra></extra>")) %>%
      style_plot(y_title = unit_label(), x_type = "category") %>%
      layout(barmode = "group")
  })

  output$compare_table <- renderDT({
    d <- compare_data()
    metric <- input$compare_metric %||% "Net_Sales"
    wide <- d %>%
      select(FY, Version, value = all_of(metric)) %>%
      mutate(value = fmt_indian(value / unit_div(), unit_digits())) %>%
      tidyr::pivot_wider(names_from = Version, values_from = value)
    DT::datatable(wide, rownames = FALSE, class = "compact nowrap",
                  options = list(dom = "t", ordering = FALSE, paging = FALSE,
                                 columnDefs = list(list(targets = seq_len(ncol(wide) - 1), className = "dt-right"))))
  })

  # ---- Board pack ----------------------------------------------------------------------------

  output$board_pack <- downloadHandler(
    filename = function() sprintf("Growth_Forecaster_Board_Pack_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      validate(need(has_data(), "Load data first."))
      comb <- combined_view()
      lever_df <- bind_rows(lapply(names(levers()), function(s) data.frame(Scenario = s, levers()[[s]])))
      names(lever_df) <- c("Scenario", "Volume_Change_Pct", "Price_Change_Pct", "Cost_Change_Pct",
                           "Commission_Shift_Pct", "Returns_Shift_Pct")
      withProgress(message = "Writing board pack", value = 0.5, write_board_pack(
        file, export_cover(),
        statements = list(
          "P&L FY" = build_statement(comb, "year"),
          "P&L Quarterly" = build_statement(comb, "quarter"),
          "P&L Monthly" = build_statement(comb, "month")
        ),
        sheets = list(
          "Scenario comparison" = scenario_summary(scenario_views()),
          "Scenario levers" = lever_df,
          "Assumptions" = state$assumptions,
          "Forecast detail" = forecast_detail(),
          "Sensitivity" = sensitivity_table(fc_view(), input$gst_mode, 0.10) %>%
            select(Driver, Low, Base, High, Impact_Low, Impact_High),
          "Actuals (monthly)" = act_view() %>% select(-Type),
          "Data quality" = state$issues
        )
      ))
    }
  )
}

shinyApp(ui = ui, server = server)
