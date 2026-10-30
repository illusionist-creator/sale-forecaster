# Growth Forecaster: P&L and forecasting engine
# Pure functions with no Shiny dependency, so every number the app shows can be
# reproduced and unit-tested headless (see tests/test_engine.R).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(forecast)
})

# ---- Constants ---------------------------------------------------------------

INR <- "₹"

ACTUAL_COLS <- c("Date", "Partner_Category", "Category", "Supply_Qty", "MRP",
                 "Commission_Pct", "Return_Pct", "GST_Pct", "RM_per_unit",
                 "PM_per_unit", "MFG_per_unit", "Yield_Loss_Pct")
NUMERIC_COLS <- setdiff(ACTUAL_COLS, c("Date", "Partner_Category", "Category"))
PCT_COLS <- c("Commission_Pct", "Return_Pct", "GST_Pct", "Yield_Loss_Pct")
DRIVER_COLS <- c("MRP", "Commission_Pct", "Return_Pct", "GST_Pct", "RM_per_unit",
                 "PM_per_unit", "MFG_per_unit", "Yield_Loss_Pct")
PLAN_COLS <- c("Date", "Partner_Category", "Category", "Supply_Qty_Growth_Pct", DRIVER_COLS)
AMOUNT_COLS <- c("MRP_Billing", "Commission", "Gross_Billing", "Return", "Gross_Sales",
                 "GST", "Net_Sales", "RM", "PM", "MFG", "COGS", "Yield_Loss", "GM")
RANGE_COLS <- c("Qty_Lo80", "Qty_Hi80", "Qty_Lo95", "Qty_Hi95")
BANDS <- c("Lo80", "Hi80", "Lo95", "Hi95")

ASSUMPTION_COLS <- c("Partner_Category", "Category", "Last_Qty", "Growth_YoY", "Growth_MoM",
                     "Manual_Qty", "MRP", "Price_Hike_Annual", "Commission_Pct", "Return_Pct",
                     "GST_Pct", "RM_per_unit", "PM_per_unit", "MFG_per_unit",
                     "Cost_Inflation_Annual", "Yield_Loss_Pct")

METHODS <- c(
  "Seasonal YoY growth" = "yoy",
  "Exponential smoothing (ETS)" = "ets",
  "Auto ARIMA" = "arima",
  "Compound MoM growth" = "growth",
  "Manual run-rate" = "manual"
)

GST_MODES <- c(
  "MRP includes GST (back out GST)" = "inclusive",
  "GST charged on gross sales (legacy)" = "exclusive"
)

SCENARIO_DEFAULTS <- list(
  Base     = list(volume = 0,     price = 0,     cost = 0,     commission_pp = 0,     returns_pp = 0),
  Upside   = list(volume = 0.10,  price = 0.02,  cost = -0.02, commission_pp = -0.01, returns_pp = -0.005),
  Downside = list(volume = -0.15, price = -0.02, cost = 0.05,  commission_pp = 0.01,  returns_pp = 0.01)
)

SENSITIVITY_DRIVERS <- c(
  "Volume" = "Supply_Qty",
  "MRP / price" = "MRP",
  "Commission %" = "Commission_Pct",
  "Returns %" = "Return_Pct",
  "Raw material / unit" = "RM_per_unit",
  "Packing material / unit" = "PM_per_unit",
  "Manufacturing / unit" = "MFG_per_unit",
  "Yield loss %" = "Yield_Loss_Pct"
)

STATEMENT_LINES <- data.frame(
  line = c("Volume (units)", "MRP billing", "Less: Commission", "Gross billing",
           "Less: Returns", "Gross sales", "Less: GST", "Net sales",
           "Raw material", "Packing material", "Manufacturing", "Yield loss",
           "Total cost of goods", "Gross margin", "Gross margin %",
           "Net sales growth (YoY)", "Net realisation / unit", "Gross margin / unit"),
  key = c("Supply_Qty", "MRP_Billing", "Commission", "Gross_Billing", "Return",
          "Gross_Sales", "GST", "Net_Sales", "RM", "PM", "MFG", "Yield_Loss",
          "Total_Cost", "GM", "GM_Pct", "NS_Growth", "NSR_per_unit", "GM_per_unit"),
  kind = c("qty", rep("amt", 13), "pct", "pct", "unit", "unit"),
  sign = c(1, 1, -1, 1, -1, 1, -1, 1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 1),
  style = c("", "subtotal", "", "subtotal", "", "subtotal", "", "total",
            "", "", "", "", "subtotal", "total", "ratio", "ratio", "ratio", "ratio"),
  stringsAsFactors = FALSE
)

# ---- Small helpers ------------------------------------------------------------

safe_div <- function(a, b) {
  if (!length(a) || !length(b)) return(numeric(0))
  n <- max(length(a), length(b))
  a <- rep_len(as.numeric(a), n)
  b <- rep_len(as.numeric(b), n)
  out <- a / b
  out[!(is.finite(b) & abs(b) > 1e-12)] <- NA_real_
  out
}

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

n_months_between <- function(from, to) {
  (year(to) - year(from)) * 12L + (month(to) - month(from))
}

method_label <- function(code) {
  lbl <- names(METHODS)[match(code, METHODS)]
  ifelse(is.na(lbl), code, lbl)
}

# ---- Fiscal calendar (April to March) -------------------------------------------

fy_start_year <- function(d) {
  d <- as.Date(d)
  as.integer(ifelse(month(d) >= 4, year(d), year(d) - 1L))
}

fy_label <- function(d) {
  y <- fy_start_year(d)
  sprintf("FY%02d-%02d", y %% 100L, (y + 1L) %% 100L)
}

fy_quarter <- function(d) as.integer(((month(as.Date(d)) - 4L) %% 12L) %/% 3L + 1L)

period_start <- function(d, grain = c("month", "quarter", "year")) {
  grain <- match.arg(grain)
  d <- floor_date(as.Date(d), "month")
  switch(grain,
         month = d,
         quarter = d %m-% months(((month(d) - 4L) %% 12L) %% 3L),
         year = as.Date(sprintf("%d-04-01", fy_start_year(d))))
}

period_label <- function(start, grain = c("month", "quarter", "year")) {
  grain <- match.arg(grain)
  switch(grain,
         month = format(start, "%b-%y"),
         quarter = paste0(fy_label(start), " Q", fy_quarter(start)),
         year = fy_label(start))
}

# Fiscal-year ends offered as forecast horizons, starting with the FY that
# contains `first_month`. Values are the March month-start dates.
horizon_choices <- function(first_month, years = 10) {
  y0 <- fy_start_year(first_month)
  ends <- as.Date(sprintf("%d-03-01", y0 + seq_len(years)))
  stats::setNames(format(ends), paste0("Through ", fy_label(ends), " (Mar-", format(ends, "%Y"), ")"))
}

# ---- Formatting (Indian conventions) ------------------------------------------------

# 12345678 -> "1,23,45,678"; negatives shown in parentheses, financial style.
fmt_indian <- function(x, digits = 0, parens = TRUE) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return("–")
    neg <- v < 0 && round(abs(v), digits) > 0
    s <- formatC(abs(v), format = "f", digits = digits, big.mark = "")
    parts <- strsplit(s, ".", fixed = TRUE)[[1]]
    int <- parts[1]
    if (nchar(int) > 3) {
      head_digits <- substr(int, 1, nchar(int) - 3)
      head_digits <- gsub("\\B(?=(\\d{2})+(?!\\d))", ",", head_digits, perl = TRUE)
      int <- paste0(head_digits, ",", substr(int, nchar(int) - 2, nchar(int)))
    }
    s <- if (length(parts) > 1) paste0(int, ".", parts[2]) else int
    if (!neg) s else if (parens) paste0("(", s, ")") else paste0("-", s)
  }, character(1), USE.NAMES = FALSE)
}

# Headline style: 12.34 Cr / 5.67 L / 45,678
fmt_compact <- function(x, currency = TRUE) {
  vapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return("–")
    sym <- if (currency) INR else ""
    sgn <- if (v < 0) "-" else ""
    a <- abs(v)
    if (a >= 1e7) paste0(sgn, sym, formatC(a / 1e7, format = "f", digits = 2), " Cr")
    else if (a >= 1e5) paste0(sgn, sym, formatC(a / 1e5, format = "f", digits = 2), " L")
    else paste0(sgn, sym, fmt_indian(a))
  }, character(1), USE.NAMES = FALSE)
}

fmt_pct <- function(x, digits = 1, signed = FALSE) {
  fmt <- paste0(if (signed) "%+." else "%.", digits, "f%%")
  ifelse(is.na(x) | !is.finite(x), "–", sprintf(fmt, 100 * x))
}

# ---- Input parsing & validation --------------------------------------------------

parse_any_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))  # Excel serial
  x <- trimws(as.character(x))
  num <- suppressWarnings(as.numeric(x))
  out <- as.Date(parse_date_time(x, orders = c("Ymd", "dmY", "Ym", "bY", "my", "mdY"), quiet = TRUE))
  serial <- is.na(out) & !is.na(num) & num > 20000 & num < 80000
  out[serial] <- as.Date(num[serial], origin = "1899-12-30")
  out
}

# Map loosely written headers ("supply qty", "GST %") onto the expected names.
match_columns <- function(df, expected) {
  key <- function(x) gsub("[^a-z0-9]", "", gsub("%", "pct", tolower(x), fixed = TRUE))
  idx <- match(key(expected), key(names(df)))
  hit <- !is.na(idx)
  names(df)[idx[hit]] <- expected[hit]
  df
}

new_issue_log <- function() {
  log <- list()
  list(
    add = function(severity, check, detail) {
      log[[length(log) + 1]] <<- data.frame(Severity = severity, Check = check,
                                            Detail = detail, stringsAsFactors = FALSE)
    },
    get = function() {
      if (!length(log)) return(data.frame(Severity = character(), Check = character(), Detail = character()))
      out <- bind_rows(log)
      out[order(match(out$Severity, c("Error", "Warning", "Info", "Pass"))), , drop = FALSE]
    }
  )
}

coerce_numeric <- function(v) {
  if (is.numeric(v)) return(v)
  suppressWarnings(as.numeric(gsub("[,%₹[:space:]]", "", as.character(v))))
}

# Rates entered as 18 instead of 0.18 are converted, with a note.
normalise_pct <- function(v, col, log, signed = FALSE) {
  mags <- abs(v[!is.na(v)])
  if (length(mags) && any(mags > 1.5) && max(mags) <= 100) {
    log$add("Info", col, "Values looked like whole-number percentages (e.g. 18) and were converted to fractions (0.18).")
    v <- v / 100
  }
  lo <- if (signed) -1 else 0
  out <- sum(v < lo | v > 1, na.rm = TRUE)
  if (out) log$add("Warning", col, sprintf("%d value(s) outside the expected %s to 100%% range.", out, if (signed) "-100%" else "0%"))
  v
}

validate_actuals <- function(raw) {
  log <- new_issue_log()
  fail <- function() list(data = NULL, issues = log$get())

  df <- as.data.frame(raw, stringsAsFactors = FALSE)
  df <- match_columns(df, ACTUAL_COLS)
  missing <- setdiff(ACTUAL_COLS, names(df))
  if (length(missing)) {
    log$add("Error", "Required columns", paste("Missing:", paste(missing, collapse = ", ")))
    return(fail())
  }
  if (!nrow(df)) {
    log$add("Error", "Rows", "The file contains no data rows.")
    return(fail())
  }
  extra <- setdiff(names(df), ACTUAL_COLS)
  if (length(extra)) log$add("Info", "Extra columns", paste("Ignored:", paste(extra, collapse = ", ")))
  df <- df[ACTUAL_COLS]

  df$Date <- parse_any_date(df$Date)
  bad_dates <- sum(is.na(df$Date))
  if (bad_dates == nrow(df)) {
    log$add("Error", "Dates", "No dates could be read. Use a date column such as 2025-04-01.")
    return(fail())
  }
  if (bad_dates) {
    log$add("Warning", "Dates", sprintf("%d row(s) with unreadable dates were excluded.", bad_dates))
    df <- df[!is.na(df$Date), , drop = FALSE]
  }
  df$Date <- floor_date(df$Date, "month")

  for (col in c("Partner_Category", "Category")) {
    v <- trimws(as.character(df[[col]]))
    blank <- is.na(v) | v == ""
    if (any(blank)) log$add("Warning", col, sprintf("%d blank value(s) labelled 'Unassigned'.", sum(blank)))
    v[blank] <- "Unassigned"
    df[[col]] <- v
  }

  for (col in NUMERIC_COLS) {
    v <- coerce_numeric(df[[col]])
    if (anyNA(v)) {
      log$add("Warning", col, sprintf("%d missing or non-numeric value(s) treated as 0.", sum(is.na(v))))
      v[is.na(v)] <- 0
    }
    df[[col]] <- v
  }
  for (col in PCT_COLS) df[[col]] <- normalise_pct(df[[col]], col, log)
  for (col in c("Supply_Qty", "MRP", "RM_per_unit", "PM_per_unit", "MFG_per_unit")) {
    n_neg <- sum(df[[col]] < 0)
    if (n_neg) log$add("Warning", col, sprintf("%d negative value(s).", n_neg))
  }

  dup <- df %>% count(Date, Partner_Category, Category) %>% filter(n > 1)
  if (nrow(dup)) {
    log$add("Info", "Granularity", sprintf(
      "%d partner-category-month(s) had several rows; they are consolidated to monthly totals.", nrow(dup)))
  }

  series <- df %>%
    distinct(Partner_Category, Category, Date) %>%
    group_by(Partner_Category, Category) %>%
    summarise(first = min(Date), last = max(Date), got = n(), .groups = "drop") %>%
    mutate(expected = n_months_between(first, last) + 1L)
  gaps <- series %>% filter(got < expected)
  if (nrow(gaps)) {
    log$add("Warning", "Continuity", sprintf(
      "%d series have missing months (e.g. %s); gaps are interpolated for statistical models.",
      nrow(gaps), paste(head(paste(gaps$Partner_Category, gaps$Category, sep = " / "), 3), collapse = "; ")))
  }
  latest <- max(df$Date)
  dormant <- series %>% filter(last < latest %m-% months(3))
  if (nrow(dormant)) {
    log$add("Info", "Dormant series", sprintf(
      "%d series have no sales in the last 3 months but are still forecast; set their Manual_Qty to 0 to exclude them.",
      nrow(dormant)))
  }

  n_months <- n_distinct(df$Date)
  if (n_months < 12) {
    log$add("Warning", "History length", sprintf(
      "Only %d month(s) of history. Seasonal YoY needs 12+ months; ETS/ARIMA seasonality needs 24+.", n_months))
  } else if (n_months < 24) {
    log$add("Info", "History length", sprintf(
      "%d months of history: ETS/ARIMA will run without seasonality (needs 24+).", n_months))
  }

  issues <- log$get()
  if (!any(issues$Severity %in% c("Error", "Warning"))) log$add("Pass", "Quality checks", "No errors or warnings.")
  log$add("Info", "Coverage", sprintf(
    "%s rows | %d partner categories | %d product categories | %s to %s (%d months).",
    fmt_indian(nrow(df)), n_distinct(df$Partner_Category), n_distinct(df$Category),
    format(min(df$Date), "%b-%Y"), format(latest, "%b-%Y"), n_months))

  list(data = df, issues = log$get())
}

validate_plan <- function(raw, known_combos = NULL) {
  log <- new_issue_log()
  df <- match_columns(as.data.frame(raw, stringsAsFactors = FALSE), c(PLAN_COLS, "Supply_Qty"))
  missing <- setdiff(PLAN_COLS, names(df))
  if (length(missing)) {
    log$add("Error", "Required columns", paste("Missing:", paste(missing, collapse = ", ")))
    return(list(data = NULL, issues = log$get()))
  }
  keep <- intersect(c(PLAN_COLS, "Supply_Qty"), names(df))
  df <- df[keep]
  df$Date <- floor_date(parse_any_date(df$Date), "month")
  if (anyNA(df$Date)) {
    log$add("Warning", "Dates", sprintf("%d row(s) with unreadable dates were excluded.", sum(is.na(df$Date))))
    df <- df[!is.na(df$Date), , drop = FALSE]
  }
  df$Partner_Category <- trimws(as.character(df$Partner_Category))
  df$Category <- trimws(as.character(df$Category))
  for (col in setdiff(keep, c("Date", "Partner_Category", "Category"))) df[[col]] <- coerce_numeric(df[[col]])
  for (col in PCT_COLS) df[[col]] <- normalise_pct(df[[col]], col, log)
  df$Supply_Qty_Growth_Pct <- normalise_pct(df$Supply_Qty_Growth_Pct, "Supply_Qty_Growth_Pct", log, signed = TRUE)

  dup <- duplicated(df[c("Date", "Partner_Category", "Category")], fromLast = TRUE)
  if (any(dup)) {
    log$add("Warning", "Duplicates", sprintf("%d duplicate partner-category-month row(s); the last one is used.", sum(dup)))
    df <- df[!dup, , drop = FALSE]
  }
  if (!is.null(known_combos)) {
    key <- paste(df$Partner_Category, df$Category)
    unknown <- unique(key[!key %in% paste(known_combos$Partner_Category, known_combos$Category)])
    if (length(unknown)) {
      log$add("Warning", "Unknown series", sprintf(
        "%d partner-category combination(s) have no history and are ignored (e.g. %s).",
        length(unknown), paste(head(unknown, 3), collapse = "; ")))
    }
  }
  if (!nrow(df)) {
    log$add("Error", "Rows", "No usable plan rows.")
    return(list(data = NULL, issues = log$get()))
  }
  log$add("Info", "Coverage", sprintf("%d plan rows from %s to %s.", nrow(df),
                                      format(min(df$Date), "%b-%Y"), format(max(df$Date), "%b-%Y")))
  list(data = df, issues = log$get())
}

# ---- P&L arithmetic --------------------------------------------------------------

# Revenue waterfall from MRP to gross margin. Every amount is linear in volume,
# which is what lets volume prediction intervals carry through to rupees.
compute_pnl <- function(df, gst_mode = "inclusive") {
  inclusive <- identical(gst_mode, "inclusive")
  df %>%
    mutate(
      MRP_Billing = Supply_Qty * MRP,
      Commission = MRP_Billing * Commission_Pct,
      Gross_Billing = MRP_Billing - Commission,
      Return = Gross_Billing * Return_Pct,
      Gross_Sales = Gross_Billing - Return,
      GST = if (inclusive) Gross_Sales * GST_Pct / (1 + GST_Pct) else Gross_Sales * GST_Pct,
      Net_Sales = Gross_Sales - GST,
      RM = RM_per_unit * Supply_Qty,
      PM = PM_per_unit * Supply_Qty,
      MFG = MFG_per_unit * Supply_Qty,
      COGS = RM + PM + MFG,
      Yield_Loss = Yield_Loss_Pct * Gross_Billing,
      GM = Net_Sales - COGS - Yield_Loss,
      GM_Pct = safe_div(GM, Net_Sales)
    )
}

add_ratios <- function(df) {
  df %>% mutate(
    GM_Pct = safe_div(GM, Net_Sales),
    Avg_MRP = safe_div(MRP_Billing, Supply_Qty),
    NSR_per_unit = safe_div(Net_Sales, Supply_Qty),
    GM_per_unit = safe_div(GM, Supply_Qty)
  )
}

# Sum amounts, then recompute ratios: averaging GM% or summing MRP is wrong.
summarise_pnl <- function(df, ...) {
  band_cols <- intersect(c(RANGE_COLS, paste0("Net_Sales_", BANDS), paste0("GM_", BANDS)), names(df))
  df %>%
    group_by(...) %>%
    summarise(across(c(Supply_Qty, all_of(AMOUNT_COLS)), \(x) sum(x, na.rm = TRUE)),
              across(all_of(band_cols), sum_or_na),
              .groups = "drop") %>%
    add_ratios()
}

# Collapse any granularity (daily, weekly, duplicate rows) to one row per
# partner-category-month, back-solving drivers so amounts are preserved exactly.
consolidate_monthly <- function(df, gst_mode = "inclusive") {
  df %>%
    mutate(Date = floor_date(Date, "month")) %>%
    compute_pnl(gst_mode) %>%
    group_by(Date, Partner_Category, Category) %>%
    summarise(across(c(Supply_Qty, all_of(AMOUNT_COLS)), \(x) sum(x, na.rm = TRUE)),
              across(all_of(DRIVER_COLS), \(x) mean(x, na.rm = TRUE), .names = "{.col}_avg"),
              .groups = "drop") %>%
    mutate(
      MRP = coalesce(safe_div(MRP_Billing, Supply_Qty), MRP_avg),
      Commission_Pct = coalesce(safe_div(Commission, MRP_Billing), Commission_Pct_avg),
      Return_Pct = coalesce(safe_div(Return, Gross_Billing), Return_Pct_avg),
      GST_Pct = coalesce(if (gst_mode == "inclusive") safe_div(GST, Net_Sales) else safe_div(GST, Gross_Sales),
                         GST_Pct_avg),
      RM_per_unit = coalesce(safe_div(RM, Supply_Qty), RM_per_unit_avg),
      PM_per_unit = coalesce(safe_div(PM, Supply_Qty), PM_per_unit_avg),
      MFG_per_unit = coalesce(safe_div(MFG, Supply_Qty), MFG_per_unit_avg),
      Yield_Loss_Pct = coalesce(safe_div(Yield_Loss, Gross_Billing), Yield_Loss_Pct_avg)
    ) %>%
    select(all_of(ACTUAL_COLS)) %>%
    arrange(Partner_Category, Category, Date)
}

# ---- Volume forecasting ----------------------------------------------------------

# Month-regular series; internal gaps interpolated.
regularise_series <- function(dates, qty) {
  if (!length(dates)) return(numeric(0))
  o <- order(dates)
  dates <- dates[o]
  qty <- qty[o]
  grid <- seq(min(dates), max(dates), by = "month")
  y <- qty[match(grid, dates)]
  if (anyNA(y)) {
    y <- if (sum(!is.na(y)) >= 2) as.numeric(forecast::na.interp(y)) else replace(y, is.na(y), 0)
  }
  y
}

# Growth of the latest (up to) 12 months over the same months a year earlier.
trailing_yoy <- function(dates, qty) {
  y <- regularise_series(dates, qty)
  n <- length(y)
  m <- min(12L, n - 12L)
  if (m < 1) return(NA_real_)
  recent <- sum(y[(n - m + 1):n])
  prior <- sum(y[(n - m + 1 - 12):(n - 12)])
  safe_div(recent, prior) - 1
}

forecast_volume <- function(dates, qty, h, method, growth_mom = 0, growth_yoy = 0, manual_qty = NA_real_) {
  na_band <- rep(NA_real_, max(h, 0))
  result <- function(q, lo80 = na_band, hi80 = na_band, lo95 = na_band, hi95 = na_band, used = "") {
    data.frame(step = seq_len(h), Supply_Qty = pmax(0, q),
               Qty_Lo80 = pmax(0, lo80), Qty_Hi80 = pmax(0, hi80),
               Qty_Lo95 = pmax(0, lo95), Qty_Hi95 = pmax(0, hi95),
               Method_Used = rep(used, h), stringsAsFactors = FALSE)
  }
  if (h <= 0) return(result(numeric(0)))

  y <- regularise_series(dates, qty)
  n <- length(y)
  last <- if (n) y[n] else 0
  growth_path <- function(g) last * (1 + g)^seq_len(h)

  if (method == "manual") {
    return(result(rep(if (is.na(manual_qty)) last else manual_qty, h), used = "Manual run-rate"))
  }
  if (method == "growth") {
    return(result(growth_path(growth_mom), used = paste("MoM growth", fmt_pct(growth_mom, 2, signed = TRUE))))
  }
  if (method == "yoy") {
    if (n < 12) return(result(growth_path(growth_mom), used = "MoM growth (YoY needs 12 months)"))
    path <- c(y, numeric(h))
    for (k in seq_len(h)) path[n + k] <- path[n + k - 12] * (1 + growth_yoy)
    return(result(path[n + seq_len(h)], used = paste("Seasonal YoY", fmt_pct(growth_yoy, 1, signed = TRUE))))
  }

  # Statistical models: seasonal only with two full cycles of history.
  fc <- if (n >= 6 && stats::var(y) > 0) {
    tryCatch(suppressWarnings({
      yt <- stats::ts(y, frequency = if (n >= 24) 12 else 1)
      model <- if (method == "ets") forecast::ets(yt) else forecast::auto.arima(yt)
      forecast::forecast(model, h = h, level = c(80, 95))
    }), error = function(e) NULL)
  }
  if (is.null(fc)) {
    flat <- rep(last, h)
    return(result(flat, flat, flat, flat, flat, used = "Naive (insufficient history)"))
  }
  result(as.numeric(fc$mean), fc$lower[, 1], fc$upper[, 1], fc$lower[, 2], fc$upper[, 2], used = fc$method)
}

# ---- Assumptions & forecast build --------------------------------------------------

default_assumptions <- function(monthly) {
  out <- monthly %>%
    arrange(Partner_Category, Category, Date) %>%
    group_by(Partner_Category, Category) %>%
    summarise(
      Last_Qty = last(Supply_Qty),
      Growth_YoY = trailing_yoy(Date, Supply_Qty),
      Manual_Qty = round(mean(tail(Supply_Qty, 3))),
      across(all_of(DRIVER_COLS), last),
      .groups = "drop"
    ) %>%
    mutate(
      Growth_YoY = round(clamp(coalesce(Growth_YoY, 0), -0.5, 1), 4),
      Growth_MoM = round((1 + Growth_YoY)^(1 / 12) - 1, 4),
      Price_Hike_Annual = 0,
      Cost_Inflation_Annual = 0
    )
  as.data.frame(out[ASSUMPTION_COLS])
}

empty_forecast <- function() {
  data.frame(Date = as.Date(character()), Partner_Category = character(), Category = character(),
             Supply_Qty = numeric(), stats::setNames(rep(list(numeric()), length(DRIVER_COLS)), DRIVER_COLS),
             stats::setNames(rep(list(numeric()), length(RANGE_COLS)), RANGE_COLS),
             Method_Used = character(), Type = character(), stringsAsFactors = FALSE)
}

# Driver-level forecast (volume + prices + costs) per partner-category-month,
# starting the month after the latest actual and running to `end_date`.
# `plan` (optional) overrides drivers month by month; `cache` (an environment)
# memoises statistical model fits across recalculations.
build_forecast <- function(monthly, assumptions, method, end_date, plan = NULL, cache = NULL) {
  end_date <- floor_date(as.Date(end_date), "month")
  start <- max(monthly$Date) %m+% months(1)
  if (end_date < start || !nrow(assumptions)) return(empty_forecast())

  num_cols <- setdiff(ASSUMPTION_COLS, c("Partner_Category", "Category", "Manual_Qty"))
  for (col in num_cols) assumptions[[col]] <- coalesce(as.numeric(assumptions[[col]]), 0)

  rows <- lapply(seq_len(nrow(assumptions)), function(i) {
    a <- assumptions[i, ]
    hist <- monthly[monthly$Partner_Category == a$Partner_Category & monthly$Category == a$Category, ]
    if (!nrow(hist)) return(NULL)
    last_date <- max(hist$Date)
    h <- n_months_between(last_date, end_date)
    dates <- seq(last_date %m+% months(1), by = "month", length.out = h)

    p <- if (!is.null(plan)) plan[plan$Partner_Category == a$Partner_Category & plan$Category == a$Category, ]
    has_plan <- !is.null(p) && nrow(p) > 0
    plan_row <- if (has_plan) match(dates, p$Date) else rep(NA_integer_, h)
    qty_override <- if (has_plan && "Supply_Qty" %in% names(p)) p$Supply_Qty[plan_row] else rep(NA_real_, h)

    stat <- method %in% c("ets", "arima")
    key <- paste(method, a$Partner_Category, a$Category, h, nrow(hist), sum(hist$Supply_Qty), last_date, sep = "|")
    vol <- if (stat && !is.null(cache) && exists(key, envir = cache, inherits = FALSE)) {
      get(key, envir = cache)
    } else {
      v <- forecast_volume(hist$Date, hist$Supply_Qty, h, method, a$Growth_MoM, a$Growth_YoY, a$Manual_Qty)
      if (stat && !is.null(cache)) assign(key, v, envir = cache)
      v
    }

    if (method == "growth" && has_plan) {
      # Plan growth compounds month on month; explicit plan volumes reset the base.
      prev <- hist$Supply_Qty[which.max(hist$Date)]
      for (k in seq_len(h)) {
        g <- if (!is.na(plan_row[k]) && !is.na(p$Supply_Qty_Growth_Pct[plan_row[k]])) {
          p$Supply_Qty_Growth_Pct[plan_row[k]]
        } else a$Growth_MoM
        prev <- if (!is.na(qty_override[k])) qty_override[k] else prev * (1 + g)
        vol$Supply_Qty[k] <- max(0, prev)
      }
      vol$Method_Used <- "MoM growth (monthly plan)"
    } else {
      hit <- !is.na(qty_override)
      vol$Supply_Qty[hit] <- qty_override[hit]
      for (col in RANGE_COLS) vol[[col]][hit & !is.na(vol[[col]])] <- qty_override[hit & !is.na(vol[[col]])]
      vol$Method_Used[hit] <- "Plan volume"
    }

    # Price hikes and cost inflation step up at the start of each new fiscal year.
    steps <- fy_start_year(dates) - fy_start_year(last_date)
    price_idx <- (1 + a$Price_Hike_Annual)^steps
    cost_idx <- (1 + a$Cost_Inflation_Annual)^steps
    drv <- data.frame(
      Date = dates, Partner_Category = a$Partner_Category, Category = a$Category,
      MRP = a$MRP * price_idx,
      Commission_Pct = a$Commission_Pct, Return_Pct = a$Return_Pct, GST_Pct = a$GST_Pct,
      RM_per_unit = a$RM_per_unit * cost_idx, PM_per_unit = a$PM_per_unit * cost_idx,
      MFG_per_unit = a$MFG_per_unit * cost_idx, Yield_Loss_Pct = a$Yield_Loss_Pct,
      stringsAsFactors = FALSE
    )
    if (has_plan) {
      for (col in DRIVER_COLS) {
        v <- p[[col]][plan_row]
        drv[[col]][!is.na(v)] <- v[!is.na(v)]
      }
    }
    out <- cbind(drv, vol[c("Supply_Qty", RANGE_COLS, "Method_Used")])
    out[out$Date >= start, , drop = FALSE]
  })

  out <- bind_rows(rows)
  if (!nrow(out)) return(empty_forecast())
  out %>%
    mutate(Supply_Qty = round(Supply_Qty), Type = "Forecast") %>%
    relocate(Supply_Qty, .after = Category) %>%
    arrange(Partner_Category, Category, Date)
}

# ---- Scenarios -----------------------------------------------------------------

apply_scenario <- function(fc, levers) {
  if (!nrow(fc)) return(fc)
  fc %>% mutate(
    across(all_of(c("Supply_Qty", RANGE_COLS)), \(x) x * (1 + levers$volume)),
    MRP = MRP * (1 + levers$price),
    across(c(RM_per_unit, PM_per_unit, MFG_per_unit), \(x) x * (1 + levers$cost)),
    Commission_Pct = clamp(Commission_Pct + levers$commission_pp, 0, 1),
    Return_Pct = clamp(Return_Pct + levers$returns_pp, 0, 1)
  )
}

# Full P&L for forecast rows, with volume prediction bands carried into rupees.
price_forecast <- function(fc, gst_mode = "inclusive") {
  out <- compute_pnl(fc, gst_mode)
  if (!nrow(out)) return(out)
  per_unit <- compute_pnl(mutate(fc, Supply_Qty = 1), gst_mode)
  for (b in BANDS) {
    q <- out[[paste0("Qty_", b)]]
    out[[paste0("Net_Sales_", b)]] <- q * per_unit$Net_Sales
    out[[paste0("GM_", b)]] <- q * per_unit$GM
  }
  out
}

scenario_summary <- function(frames) {
  bind_rows(lapply(names(frames), function(s) {
    f <- frames[[s]]
    data.frame(Scenario = s, Volume = sum(f$Supply_Qty), Net_Sales = sum(f$Net_Sales),
               GM = sum(f$GM), stringsAsFactors = FALSE)
  })) %>%
    mutate(GM_Pct = safe_div(GM, Net_Sales),
           Net_Sales_vs_Base = safe_div(Net_Sales, Net_Sales[Scenario == "Base"]) - 1,
           GM_vs_Base = GM - GM[Scenario == "Base"])
}

# ---- Analysis ------------------------------------------------------------------

sensitivity_table <- function(fc, gst_mode = "inclusive", shock = 0.10, metric = "GM") {
  if (!nrow(fc)) return(NULL)
  total <- function(x) sum(compute_pnl(x, gst_mode)[[metric]])
  base <- total(fc)
  bind_rows(lapply(names(SENSITIVITY_DRIVERS), function(lbl) {
    col <- SENSITIVITY_DRIVERS[[lbl]]
    shift <- function(m) {
      x <- fc
      x[[col]] <- x[[col]] * m
      total(x)
    }
    data.frame(Driver = lbl, Low = shift(1 - shock), High = shift(1 + shock), stringsAsFactors = FALSE)
  })) %>%
    mutate(Base = base, Impact_Low = Low - base, Impact_High = High - base, Swing = abs(High - Low)) %>%
    arrange(desc(Swing))
}

# Hold out the latest months, refit each method on the rest with the same
# default assumptions the app would use, and score total-volume accuracy.
backtest_methods <- function(monthly, holdout = 6, methods = c("yoy", "ets", "arima", "growth")) {
  months_all <- sort(unique(monthly$Date))
  if (length(months_all) < holdout + 12) return(NULL)
  cutoff <- months_all[length(months_all) - holdout]
  train <- monthly[monthly$Date <= cutoff, ]
  actual <- monthly %>%
    filter(Date > cutoff) %>%
    group_by(Date) %>%
    summarise(Actual = sum(Supply_Qty), .groups = "drop")
  assumptions <- default_assumptions(train)

  series <- bind_rows(lapply(methods, function(m) {
    fc <- build_forecast(train, assumptions, m, max(months_all))
    tot <- fc %>% group_by(Date) %>% summarise(Forecast = sum(Supply_Qty), .groups = "drop")
    actual %>% left_join(tot, by = "Date") %>% mutate(Forecast = coalesce(Forecast, 0), Method = m)
  }))
  metrics <- series %>%
    group_by(Method) %>%
    summarise(
      MAPE = mean(abs(Forecast - Actual)[Actual > 0] / Actual[Actual > 0]),
      WAPE = sum(abs(Forecast - Actual)) / sum(Actual),
      Bias = sum(Forecast - Actual) / sum(Actual),
      .groups = "drop"
    ) %>%
    arrange(WAPE) %>%
    mutate(Rank = row_number(), Method_Label = method_label(Method))
  list(metrics = metrics, series = series, cutoff = cutoff, holdout = holdout)
}

# Financial-statement layout: line items as rows, fiscal periods as columns.
build_statement <- function(df, grain = "year") {
  if (is.null(df) || !nrow(df)) return(NULL)
  agg <- df %>%
    mutate(Period = period_start(Date, grain)) %>%
    group_by(Period) %>%
    summarise(across(c(Supply_Qty, all_of(AMOUNT_COLS)), \(x) sum(x, na.rm = TRUE)),
              Basis = paste(sort(unique(substr(Type, 1, 1))), collapse = "+"),
              .groups = "drop") %>%
    add_ratios() %>%
    mutate(Total_Cost = COGS + Yield_Loss) %>%
    arrange(Period)
  prior <- match(agg$Period %m-% years(1), agg$Period)
  agg$NS_Growth <- safe_div(agg$Net_Sales, agg$Net_Sales[prior]) - 1

  values <- do.call(rbind, lapply(seq_len(nrow(STATEMENT_LINES)), function(i) {
    agg[[STATEMENT_LINES$key[i]]] * STATEMENT_LINES$sign[i]
  }))
  labels <- paste(period_label(agg$Period, grain), agg$Basis)
  table <- data.frame(Line = STATEMENT_LINES$line, values, check.names = FALSE, stringsAsFactors = FALSE)
  names(table) <- c("Line", labels)
  list(table = table, lines = STATEMENT_LINES, periods = agg$Period, basis = agg$Basis, grain = grain)
}

# ---- Templates & sample data -------------------------------------------------------

actuals_template <- function() {
  data.frame(
    Date = as.Date(c("2025-04-01", "2025-04-01", "2025-05-01", "2025-05-01")),
    Partner_Category = c("Partner1", "Partner1", "Partner1", "Partner1"),
    Category = c("CategoryA", "CategoryB", "CategoryA", "CategoryB"),
    Supply_Qty = c(1000, 800, 1050, 820),
    MRP = c(500, 750, 500, 750),
    Commission_Pct = c(0.15, 0.12, 0.15, 0.12),
    Return_Pct = c(0.05, 0.03, 0.05, 0.03),
    GST_Pct = c(0.18, 0.18, 0.18, 0.18),
    RM_per_unit = c(200, 300, 200, 300),
    PM_per_unit = c(50, 75, 50, 75),
    MFG_per_unit = c(25, 40, 25, 40),
    Yield_Loss_Pct = c(0.02, 0.01, 0.02, 0.01)
  )
}

template_guide <- function() {
  data.frame(
    Column = ACTUAL_COLS,
    Description = c(
      "Month of activity (any day in the month; e.g. 2025-04-01)",
      "Channel or partner group (e.g. E-commerce, Modern Trade)",
      "Product category",
      "Units supplied in the month",
      "Maximum retail price per unit (GST-inclusive, as printed)",
      "Partner commission as a fraction of MRP billing (0.15 = 15%)",
      "Returns as a fraction of gross billing",
      "GST rate (0.18 = 18%)",
      "Raw material cost per unit",
      "Packing material cost per unit",
      "Manufacturing / conversion cost per unit",
      "Yield loss as a fraction of gross billing"
    )
  )
}

# Monthly plan pre-filled from the current assumptions, one row per series-month.
plan_template <- function(monthly, assumptions, end_date) {
  fc <- build_forecast(monthly, assumptions, "growth", end_date)
  if (!nrow(fc)) return(NULL)
  g <- assumptions[c("Partner_Category", "Category", "Growth_MoM")]
  fc %>%
    left_join(g, by = c("Partner_Category", "Category")) %>%
    transmute(Date, Partner_Category, Category, Supply_Qty_Growth_Pct = Growth_MoM,
              across(all_of(DRIVER_COLS)), Supply_Qty = NA_real_) %>%
    arrange(Date, Partner_Category, Category) %>%
    as.data.frame()
}

# Synthetic FMCG-style history: festive-season peaks, April price hikes, channel
# economics. Deterministic for a given seed.
make_sample_data <- function(end_month = floor_date(Sys.Date(), "month") %m-% months(1), months = 36, seed = 42) {
  set.seed(seed)
  end_month <- floor_date(as.Date(end_month), "month")
  dates <- seq(end_month %m-% months(months - 1), by = "month", length.out = months)
  # Indexed Jan..Dec: festive peak in Oct-Nov, soft Feb.
  season <- c(0.93, 0.88, 0.96, 0.92, 0.95, 1.00, 0.97, 1.02, 1.08, 1.30, 1.25, 1.00)[month(dates)]

  partners <- data.frame(Partner_Category = c("E-commerce", "Modern Trade", "General Trade"),
                         scale = c(1.0, 0.7, 1.4), trend = c(0.022, 0.010, 0.004),
                         Commission_Pct = c(0.22, 0.15, 0.10), Return_Pct = c(0.06, 0.03, 0.015))
  categories <- data.frame(Category = c("Skin Care", "Hair Care", "Oral Care"),
                           base = c(4200, 6800, 9500), MRP = c(399, 249, 99),
                           RM = c(92, 61, 24), PM = c(31, 18, 9), MFG = c(22, 14, 6),
                           Yield = c(0.015, 0.012, 0.008))

  grid <- tidyr::crossing(Date = dates, Partner_Category = partners$Partner_Category, Category = categories$Category) %>%
    left_join(partners, by = "Partner_Category") %>%
    left_join(categories, by = "Category") %>%
    mutate(
      t = n_months_between(min(Date), Date),
      fy_steps = fy_start_year(Date) - fy_start_year(min(Date)),
      Supply_Qty = round(base * scale * (1 + trend)^t * season[match(Date, dates)] * exp(rnorm(n(), 0, 0.06))),
      MRP = round(MRP * 1.05^fy_steps),
      GST_Pct = 0.18,
      RM_per_unit = round(RM * 1.04^fy_steps * exp(rnorm(n(), 0, 0.01)), 2),
      PM_per_unit = round(PM * 1.03^fy_steps, 2),
      MFG_per_unit = round(MFG * 1.03^fy_steps, 2),
      Yield_Loss_Pct = Yield
    )
  as.data.frame(grid[ACTUAL_COLS])
}

# ---- Excel board pack ------------------------------------------------------------

col_number_format <- function(name) {
  if (grepl("Date|Period|Cutoff", name)) return("mmm-yy")
  if (grepl("Pct|Growth|Hike|Inflation|MAPE|WAPE|Bias|vs_Base$", name) && !grepl("GM_vs_Base", name)) return("0.0%")
  if (grepl("Qty|Volume|Units|Actual$|Forecast$", name)) return("#,##0")
  if (grepl("per_unit|MRP$|NSR|Avg_", name)) return("#,##0.00")
  if (grepl(paste(c(AMOUNT_COLS, "Net_Sales", "GM", "Low", "High", "Base", "Impact", "Swing"), collapse = "|"), name)) {
    return("#,##0;(#,##0);\"-\"")
  }
  NULL
}

write_board_pack <- function(path, cover, statements = list(), sheets = list()) {
  wb <- openxlsx::createWorkbook(creator = "Growth Forecaster")
  hdr <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#0F2A4A", textDecoration = "bold",
                               halign = "center", valign = "center", wrapText = TRUE)
  title <- openxlsx::createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#0F2A4A")
  bold <- openxlsx::createStyle(textDecoration = "bold")
  total_row <- openxlsx::createStyle(textDecoration = "bold", border = "top", borderColour = "#0F2A4A")
  formats <- list(
    amt = openxlsx::createStyle(numFmt = "#,##0;(#,##0);\"-\""),
    qty = openxlsx::createStyle(numFmt = "#,##0"),
    unit = openxlsx::createStyle(numFmt = "#,##0.00"),
    pct = openxlsx::createStyle(numFmt = "0.0%")
  )

  openxlsx::addWorksheet(wb, "Summary", gridLines = FALSE)
  openxlsx::writeData(wb, "Summary", "Growth Forecaster | P&L Forecast Board Pack", startRow = 1)
  openxlsx::addStyle(wb, "Summary", title, rows = 1, cols = 1)
  openxlsx::writeData(wb, "Summary", data.frame(Item = names(cover), Value = unname(cover)),
                      startRow = 3, headerStyle = hdr)
  openxlsx::addStyle(wb, "Summary", bold, rows = 3 + seq_along(cover), cols = 1, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Summary", cols = 1:2, widths = c(28, 70))

  for (nm in names(statements)) {
    st <- statements[[nm]]
    if (is.null(st)) next
    openxlsx::addWorksheet(wb, nm, gridLines = FALSE)
    openxlsx::writeData(wb, nm, st$table, headerStyle = hdr)
    value_cols <- 2:ncol(st$table)
    for (i in seq_len(nrow(st$lines))) {
      openxlsx::addStyle(wb, nm, formats[[st$lines$kind[i]]], rows = i + 1, cols = value_cols, gridExpand = TRUE)
      if (st$lines$style[i] %in% c("total", "subtotal")) {
        openxlsx::addStyle(wb, nm, if (st$lines$style[i] == "total") total_row else bold,
                           rows = i + 1, cols = c(1, value_cols), gridExpand = TRUE, stack = TRUE)
      }
    }
    openxlsx::setColWidths(wb, nm, cols = 1, widths = 28)
    openxlsx::setColWidths(wb, nm, cols = value_cols, widths = 15)
    openxlsx::freezePane(wb, nm, firstActiveRow = 2, firstActiveCol = 2)
  }

  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    if (is.null(df) || !nrow(df)) next
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, df, headerStyle = hdr, withFilter = TRUE)
    for (j in seq_along(df)) {
      fmt <- col_number_format(names(df)[j])
      if (!is.null(fmt) && (is.numeric(df[[j]]) || inherits(df[[j]], "Date"))) {
        openxlsx::addStyle(wb, nm, openxlsx::createStyle(numFmt = fmt), rows = 1 + seq_len(nrow(df)), cols = j)
      }
    }
    openxlsx::setColWidths(wb, nm, cols = seq_along(df), widths = "auto")
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
