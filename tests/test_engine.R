# Engine tests. Run from the project root:  Rscript tests/test_engine.R
# Base R only (no testthat dependency).

source(file.path("R", "engine.R"))

failures <- 0
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  cat(if (ok) "  PASS " else "  FAIL ", desc, "\n", sep = "")
  if (!ok) failures <<- failures + 1
}
near <- function(a, b, tol = 1e-6) isTRUE(all(abs(a - b) <= tol * pmax(1, abs(b))))

one_row <- data.frame(Date = as.Date("2025-04-01"), Partner_Category = "P", Category = "C",
                      Supply_Qty = 100, MRP = 118, Commission_Pct = 0.10, Return_Pct = 0.05, GST_Pct = 0.18,
                      RM_per_unit = 20, PM_per_unit = 5, MFG_per_unit = 3, Yield_Loss_Pct = 0.01)

cat("P&L arithmetic\n")
inc <- compute_pnl(one_row, "inclusive")
exc <- compute_pnl(one_row, "exclusive")
check("MRP billing = qty x MRP", near(inc$MRP_Billing, 11800))
check("Gross billing after 10% commission", near(inc$Gross_Billing, 10620))
check("Gross sales after 5% returns", near(inc$Gross_Sales, 10089))
check("Inclusive GST backs out 18/118", near(inc$GST, 10089 * 0.18 / 1.18))
check("Legacy GST applies rate directly", near(exc$GST, 10089 * 0.18))
check("GM = NS - COGS - yield loss", near(inc$GM, inc$Net_Sales - 2800 - 106.2))
check("GM% is NA (not Inf) at zero sales", is.na(compute_pnl(transform(one_row, Supply_Qty = 0))$GM_Pct))
check("safe_div recycles a scalar denominator", identical(safe_div(c(2, 4), 2), c(1, 2)))
check("safe_div returns NA on zero", is.na(safe_div(1, 0)))

cat("Fiscal calendar\n")
check("Mar-26 is FY25-26", fy_label(as.Date("2026-03-15")) == "FY25-26")
check("Apr-26 is FY26-27", fy_label(as.Date("2026-04-01")) == "FY26-27")
check("Fiscal quarters: Apr=Q1, Jul=Q2, Jan=Q4",
      identical(fy_quarter(as.Date(c("2025-04-01", "2025-07-01", "2026-01-01"))), c(1L, 2L, 4L)))
check("Quarter start for Feb is Jan", period_start(as.Date("2026-02-10"), "quarter") == as.Date("2026-01-01"))
check("Year start for Feb-26 is Apr-25", period_start(as.Date("2026-02-10"), "year") == as.Date("2025-04-01"))

cat("Formatting\n")
check("Indian grouping", fmt_indian(12345678) == "1,23,45,678")
check("Negative in parentheses", fmt_indian(-1234567) == "(12,34,567)")
check("Small numbers ungrouped", fmt_indian(999) == "999")
check("Compact crore", fmt_compact(123456789) == paste0(INR, "12.35 Cr"))
check("Compact lakh", fmt_compact(250000) == paste0(INR, "2.50 L"))

cat("Validation\n")
v_missing <- validate_actuals(one_row[, -5])
check("Missing column is an error", is.null(v_missing$data) && any(v_missing$issues$Severity == "Error"))
whole_pct <- transform(one_row, GST_Pct = 18, Commission_Pct = 10)
v_pct <- validate_actuals(whole_pct)
check("Whole-number percentages converted", near(v_pct$data$GST_Pct, 0.18) && near(v_pct$data$Commission_Pct, 0.10))
loose <- one_row
names(loose) <- c("date", "partner category", "CATEGORY", "Supply Qty", "mrp", "Commission %", "return_pct",
                  "GST pct", "RM per unit", "pm_per_unit", "MFG_per_unit", "yield loss pct")
check("Loose headers are matched", !is.null(validate_actuals(loose)$data))
text_dates <- transform(one_row, Date = "15-04-2025")
check("Text dates parsed day-first and floored to month",
      validate_actuals(text_dates)$data$Date == as.Date("2025-04-01"))
extra <- cbind(one_row, Notes = "x")
check("Extra columns dropped", identical(names(validate_actuals(extra)$data), ACTUAL_COLS))

cat("Monthly consolidation\n")
two_rows <- rbind(one_row, transform(one_row, Date = as.Date("2025-04-20"), Supply_Qty = 50, MRP = 130))
cons <- consolidate_monthly(two_rows)
check("Two rows in a month collapse to one", nrow(cons) == 1)
check("Consolidation preserves net sales",
      near(sum(compute_pnl(cons)$Net_Sales), sum(compute_pnl(two_rows)$Net_Sales)))
check("Consolidation preserves GM", near(sum(compute_pnl(cons)$GM), sum(compute_pnl(two_rows)$GM)))

cat("Forecasting\n")
sample <- consolidate_monthly(validate_actuals(make_sample_data(as.Date("2026-03-01")))$data)
assm <- default_assumptions(sample)
check("One assumption row per series", nrow(assm) == 9)

g <- assm
g$Growth_MoM <- 0.02
g$Price_Hike_Annual <- 0.10
fc_g <- build_forecast(sample, g, "growth", as.Date("2027-03-01"))
s1 <- fc_g[fc_g$Partner_Category == "E-commerce" & fc_g$Category == "Skin Care", ]
last_q <- tail(sample$Supply_Qty[sample$Partner_Category == "E-commerce" & sample$Category == "Skin Care"], 1)
check("Forecast starts the month after last actual", min(fc_g$Date) == as.Date("2026-04-01"))
check("Forecast ends at horizon", max(fc_g$Date) == as.Date("2027-03-01"))
check("Growth compounds monthly", near(s1$Supply_Qty[3], round(last_q * 1.02^3)))
check("Price hike applied from April of new FY",
      near(s1$MRP[1], assm$MRP[assm$Partner_Category == "E-commerce" & assm$Category == "Skin Care"] * 1.10))

y <- assm
y$Growth_YoY <- 0.10
fc_y <- build_forecast(sample, y, "yoy", as.Date("2027-03-01"))
oct_hist <- sum(sample$Supply_Qty[sample$Date == as.Date("2025-10-01")])
oct_fc <- sum(fc_y$Supply_Qty[fc_y$Date == as.Date("2026-10-01")])
check("Seasonal YoY = same month last year x 1.10", abs(oct_fc / oct_hist - 1.10) < 0.001)

fc_e <- build_forecast(sample, assm, "ets", as.Date("2027-03-01"))
check("ETS forecasts every series-month", nrow(fc_e) == 9 * 12)
check("ETS volumes non-negative and finite", all(is.finite(fc_e$Supply_Qty)) && all(fc_e$Supply_Qty >= 0))
check("ETS 95% band contains the 80% band", all(fc_e$Qty_Lo95 <= fc_e$Qty_Lo80 + 1e-6 & fc_e$Qty_Hi95 >= fc_e$Qty_Hi80 - 1e-6))

cache <- new.env()
invisible(build_forecast(sample, assm, "ets", as.Date("2027-03-01"), cache = cache))
check("Model cache populated", length(ls(cache)) == 9)

short <- sample[sample$Date >= as.Date("2025-12-01"), ]
fc_short <- build_forecast(short, default_assumptions(short), "ets", as.Date("2026-06-01"))
check("Short history falls back without error", nrow(fc_short) == 9 * 3)

plan <- data.frame(Date = as.Date("2026-04-01"), Partner_Category = "E-commerce", Category = "Skin Care",
                   Supply_Qty_Growth_Pct = 0, MRP = 999, Commission_Pct = 0.2, Return_Pct = 0.05, GST_Pct = 0.18,
                   RM_per_unit = 1, PM_per_unit = 1, MFG_per_unit = 1, Yield_Loss_Pct = 0, Supply_Qty = 5000)
fc_p <- build_forecast(sample, assm, "yoy", as.Date("2026-06-01"), plan = plan)
row_p <- fc_p[fc_p$Date == as.Date("2026-04-01") & fc_p$Partner_Category == "E-commerce" & fc_p$Category == "Skin Care", ]
check("Plan overrides MRP", row_p$MRP == 999)
check("Plan volume overrides model", row_p$Supply_Qty == 5000)

pt <- plan_template(sample, assm, as.Date("2026-06-01"))
check("Plan template keeps real Date class", inherits(pt$Date, "Date"))
check("Plan template round-trips through validation", !is.null(validate_plan(pt, assm)$data))

cat("Scenarios & analysis\n")
base_pnl <- price_forecast(fc_e)
up_pnl <- price_forecast(apply_scenario(fc_e, list(volume = 0.10, price = 0, cost = 0, commission_pp = 0, returns_pp = 0)))
check("+10% volume lifts net sales 10%", near(sum(up_pnl$Net_Sales) / sum(base_pnl$Net_Sales), 1.10, 1e-3))
check("Volume bands scale with volume lever", near(sum(up_pnl$Qty_Hi95) / sum(base_pnl$Qty_Hi95), 1.10))
check("Net sales band brackets point forecast",
      sum(base_pnl$Net_Sales_Lo95) <= sum(base_pnl$Net_Sales) && sum(base_pnl$Net_Sales) <= sum(base_pnl$Net_Sales_Hi95))

summ <- scenario_summary(list(Base = base_pnl, Upside = up_pnl))
check("Scenario summary: Upside net sales +10% vs Base", near(summ$Net_Sales_vs_Base, c(0, 0.10), 1e-3))

sens <- sensitivity_table(fc_e)
check("Sensitivity base equals forecast GM", near(sens$Base[1], sum(base_pnl$GM)))
check("Price is the largest GM lever in sample", sens$Driver[1] == "MRP / price")

combined <- bind_rows(compute_pnl(sample) %>% mutate(Type = "Actual"), base_pnl)
st <- build_statement(combined, "year")
ns_row <- which(st$lines$key == "Net_Sales")
check("Statement FY net sales reconcile to detail", near(sum(unlist(st$table[ns_row, -1])), sum(combined$Net_Sales)))
check("Statement labels mark actual vs forecast", any(grepl(" A$", names(st$table))) && any(grepl(" F$", names(st$table))))
check("Quarterly statement has fiscal quarter labels", any(grepl("Q1", names(build_statement(combined, "quarter")$table))))

bt <- backtest_methods(sample)
check("Back-test ranks four methods", !is.null(bt) && nrow(bt$metrics) == 4 && bt$metrics$Rank[1] == 1)

cat("Export\n")
path <- tempfile(fileext = ".xlsx")
write_board_pack(path, c("Prepared" = "test"), statements = list("P&L FY" = st),
                 sheets = list("Forecast detail" = fc_e, "Assumptions" = assm))
check("Board pack written with expected sheets",
      identical(readxl::excel_sheets(path), c("Summary", "P&L FY", "Forecast detail", "Assumptions")))

cat(sprintf("\n%d failure(s)\n", failures))
if (failures) quit(status = 1)
