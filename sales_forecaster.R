# Growth Forecaster App
# R Shiny application for forecasting growth and P&L metrics

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(plotly)
library(shinyjs)
library(lubridate)
library(tidyr)
library(forecast)
library(bslib)
library(rhandsontable)

# Define UI
ui <- bslib::page_fluid(
  tags$head(tags$link(rel = "shortcut icon", href = "arrow-up-right-dots")),
  tags$title("Growth Forecaster"),
  theme = bslib::bs_theme(
    bootswatch = "simplex",
    primary = "#404040",
    secondary = "#5555b4"
  ),
  useShinyjs(),
  # App title
  div(
    class = "container-fluid",
    style = "padding: 15px; background-color: #404040; color: #ecf0f1; margin-bottom: 20px;",
    div(
      class = "row", 
      div(
        class = "col-md-8",
        h1("Growth Forecaster", style = "margin-top: 0;"),
        p("Forecast P&L profiles based on historical data and customizable inputs")
      ),
      div(
        class = "col-md-4 text-right",
        style = "display: flex; align-items: center; justify-content: flex-end; height: 100%;",
        p("Fiscal Year: April to March", style = "margin: 0;")
      )
    )
  ),
  
  # Main layout
  navs_tab_card(
    id = "main_tabs",
    nav_panel(
      title = "Upload Data",
      icon = icon("upload"),
      div(
        class = "row",
        div(
          class = "col-md-6",
          card(
            card_header("Upload Historical Data"),
            card_body(
              fileInput("file_upload", "Upload Excel file:", accept = c(".xlsx", ".xls")),
              downloadButton("download_template", "Download Template"),
              br(), br(),
              p("Required columns:", style = "font-weight: bold;"),
              p("Date, Partner_Category, Category, Supply_Qty, MRP, Commission_Pct, Return_Pct, GST_Pct, RM_per_unit, PM_per_unit, MFG_per_unit, Yield_Loss_Pct"),
              br(),
              actionButton("process_upload", "Process Upload", class = "btn-primary")
            )
          )
        ),
        div(
          class = "col-md-6",
          card(
            card_header("Upload Status"),
            card_body(
              verbatimTextOutput("upload_status"),
              uiOutput("preview_instructions"),
              div(style = "max-height: 400px; overflow-y: auto;",
                  DT::dataTableOutput("preview_data"))
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Historical Data",
      icon = icon("chart-line"),
      div(
        class = "row",
        div(
          class = "col-md-12",
          bslib::navset_card_tab(
            id = "historical_tabs",
            nav_panel(
              title = "Table View",
              card_body(
                div(
                  class = "row",
                  div(
                    class = "col-md-12",
                    selectInput("historical_table_filter_partner", "Filter by Partner Category:",
                                choices = c("All"), selected = "All", multiple = TRUE),
                    selectInput("historical_table_filter_category", "Filter by Category:",
                                choices = c("All"), selected = "All", multiple = TRUE)
                  )
                ),
                hr(),
                div(
                  style = "overflow-x: auto;",
                  DT::dataTableOutput("historical_data_table")
                )
              )
            ),
            nav_panel(
              title = "Charts",
              card_body(
                div(
                  class = "row",
                  div(
                    class = "col-md-3",
                    selectInput("chart_metric", "Select Metrics:",
                                choices = c("Supply_Qty", "MRP", "Gross_Billing", "Net_Sales", "GM", "GM_Pct"),
                                selected = "GM", multiple = TRUE),
                    selectInput("chart_partner", "Filter by Partner Category:",
                                choices = c("All"), selected = "All"),
                    selectInput("chart_category", "Filter by Category:",
                                choices = c("All"), selected = "All"),
                    checkboxInput("chart_aggregate", "Aggregate Data", value = FALSE)
                  ),
                  div(
                    class = "col-md-9",
                    plotlyOutput("historical_chart", height = "500px")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Inputs",
      icon = icon("sliders-h"),
      div(
        class = "row",
        div(
          class = "col-md-12",
          card(
            card_header("Forecast Settings"),
            card_body(
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  dateRangeInput("forecast_period", "Forecast Period:", 
                                 start = paste0(year(Sys.Date()), "-04-01"),
                                 end = paste0(year(Sys.Date()) + 1, "-03-31"),
                                 format = "yyyy-mm-dd"),
                  p("Maximum forecast period is 10 years.", class = "text-muted")
                ),
                div(
                  class = "col-md-6",
                  selectInput("forecast_method", "Forecasting Method:",
                              choices = c("Simple Growth Rate" = "simple",
                                          "Exponential Smoothing" = "ets",
                                          "Manual Inputs Only" = "manual"))
                )
              ),
              hr(),
              div(
                class = "row",
                div(
                  class = "col-md-12",
                  h4("Forecast Variables"),
                  p("Enter values for the forecast period. You can adjust them by Partner Category and Product Category."),
                  p("Supply_Qty_Growth_Pct represents the month-over-month growth rate for Supply_Qty."),
                  br(),
                  downloadButton("download_monthly_template", "Download Monthly Input Template"),
                  br(), br(),
                  fileInput("monthly_input_upload", "Upload Monthly Inputs:", accept = c(".xlsx", ".xls")),
                  actionButton("process_monthly_inputs", "Process Monthly Inputs", class = "btn-primary"),
                  hr(),
                  uiOutput("forecast_inputs_ui")
                )
              ),
              hr(),
              div(
                class = "row",
                div(
                  class = "col-md-12 text-center",
                  actionButton("generate_forecast", "Generate Forecast", class = "btn-lg btn-primary")
                )
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "P&L",
      icon = icon("table"),
      div(
        class = "row",
        div(
          class = "col-md-12",
          card(
            card_header("Profit & Loss Analysis"),
            card_body(
              div(
                class = "row",
                div(
                  class = "col-md-3",
                  selectInput("pl_partner_category", "Partner Category:",
                              choices = c("All"), selected = "All", multiple = TRUE),
                  selectInput("pl_view_type", "View Type:",
                              choices = c("Historical Only" = "historical",
                                          "Historical + Forecast" = "both",
                                          "Forecast Only" = "forecast"),
                              selected = "historical"),
                  selectInput("pl_time_period", "Time Period:",
                              choices = c("Monthly" = "monthly",
                                          "Quarterly" = "quarterly",
                                          "Yearly" = "yearly"),
                              selected = "monthly"),
                  downloadButton("download_pl", "Download P&L")
                ),
                div(
                  class = "col-md-9",
                  div(
                    style = "overflow-x: auto;",
                    DT::dataTableOutput("pl_table")
                  ),
                  br(),
                  plotlyOutput("pl_chart", height = "400px")
                )
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      title = "Forecast",
      icon = icon("binoculars"),
      div(
        class = "row",
        div(
          class = "col-md-12",
          bslib::navset_card_tab(
            id = "forecast_tabs",
            nav_panel(
              title = "Table View",
              card_body(
                div(
                  class = "row",
                  div(
                    class = "col-md-12",
                    selectInput("forecast_table_filter_partner", "Filter by Partner Category:",
                                choices = c("All"), selected = "All", multiple = TRUE),
                    selectInput("forecast_table_filter_category", "Filter by Category:",
                                choices = c("All"), selected = "All", multiple = TRUE),
                    downloadButton("download_forecast", "Download Forecast Data")
                  )
                ),
                hr(),
                div(
                  style = "overflow-x: auto;",
                  DT::dataTableOutput("forecast_data_table")
                )
              )
            ),
            nav_panel(
              title = "Charts",
              card_body(
                div(
                  class = "row",
                  div(
                    class = "col-md-3",
                    selectInput("forecast_chart_metric", "Select Metrics:",
                                choices = c("Supply_Qty", "MRP", "Gross_Billing", "Net_Sales", "GM", "GM_Pct"),
                                selected = "GM", multiple = TRUE),
                    selectInput("forecast_chart_partner", "Filter by Partner Category:",
                                choices = c("All"), selected = "All"),
                    selectInput("forecast_chart_category", "Filter by Category:",
                                choices = c("All"), selected = "All"),
                    checkboxInput("forecast_chart_aggregate", "Aggregate Data", value = FALSE),
                    checkboxInput("forecast_chart_include_historical", "Include Historical Data", value = TRUE)
                  ),
                  div(
                    class = "col-md-9",
                    plotlyOutput("forecast_chart", height = "500px")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # Footer
  div(
    class = "container-fluid",
    style = "margin-top: 20px; padding: 10px; background-color: #ecf0f1; color: #2c3e50; border-top: 1px solid #bdc3c7;",
    div(
      class = "row",
      div(
        class = "col-md-12 text-center",
        p("Growth Forecaster App - R Shiny Application")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store app data
  values <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    forecast_data = NULL,
    has_uploaded = FALSE,
    has_forecast = FALSE,
    partner_categories = NULL,
    product_categories = NULL,
    forecast_inputs = NULL,
    monthly_inputs = NULL,
    last_historical_data = NULL
  )
  
  # Create template for download
  output$download_template <- downloadHandler(
    filename = function() {
      "growth_forecaster_template.xlsx"
    },
    content = function(file) {
      template_data <- data.frame(
        Date = as.Date(c("2023-04-01", "2023-04-01")),
        Partner_Category = c("Partner1", "Partner1"),
        Category = c("CategoryA", "CategoryB"),
        Supply_Qty = c(1000, 800),
        MRP = c(500, 750),
        Commission_Pct = c(0.15, 0.12),
        Return_Pct = c(0.05, 0.03),
        GST_Pct = c(0.18, 0.18),
        RM_per_unit = c(200, 300),
        PM_per_unit = c(50, 75),
        MFG_per_unit = c(25, 40),
        Yield_Loss_Pct = c(0.02, 0.01)
      )
      writexl::write_xlsx(template_data, file)
    }
  )
  
  # Process file upload
  observeEvent(input$process_upload, {
    req(input$file_upload)
    
    # Read the Excel file
    tryCatch({
      # Read the data
      raw_data <- readxl::read_excel(input$file_upload$datapath)
      
      # Check if all required columns exist
      required_cols <- c("Date", "Partner_Category", "Category", "Supply_Qty", "MRP", 
                         "Commission_Pct", "Return_Pct", "GST_Pct", "RM_per_unit", 
                         "PM_per_unit", "MFG_per_unit", "Yield_Loss_Pct")
      
      missing_cols <- required_cols[!(required_cols %in% colnames(raw_data))]
      
      if (length(missing_cols) > 0) {
        output$upload_status <- renderText({
          paste("Error: Missing required columns:", paste(missing_cols, collapse = ", "))
        })
        return()
      }
      
      # Ensure Date is a Date type
      raw_data$Date <- as.Date(raw_data$Date)
      
      # Process the data and calculate derived metrics
      processed_data <- raw_data %>%
        mutate(
          MRP_Billing = Supply_Qty * MRP,
          Commission = MRP_Billing * Commission_Pct,
          Gross_Billing = MRP_Billing - Commission,
          Return = Gross_Billing * Return_Pct,
          Gross_Sales = Gross_Billing - Return,
          GST = Gross_Sales * GST_Pct,
          Net_Sales = Gross_Sales - GST,
          RM = RM_per_unit * Supply_Qty,
          PM = PM_per_unit * Supply_Qty,
          MFG = MFG_per_unit * Supply_Qty,
          Yield_Loss = Yield_Loss_Pct * Gross_Billing,
          GM = Net_Sales - RM - PM - MFG - Yield_Loss,
          GM_Pct = GM / Net_Sales
        )
      
      # Store data in reactive values
      values$raw_data <- raw_data
      values$processed_data <- processed_data
      values$has_uploaded <- TRUE
      
      # Extract unique partner categories and product categories
      values$partner_categories <- unique(processed_data$Partner_Category)
      values$product_categories <- unique(processed_data$Category)
      
      # Store the last historical data points for each partner-category combination
      values$last_historical_data <- processed_data %>%
        group_by(Partner_Category, Category) %>%
        arrange(desc(Date)) %>%
        slice(1) %>%
        ungroup()
      
      # Update filter choices
      updateSelectInput(session, "historical_table_filter_partner", 
                        choices = c("All", values$partner_categories),
                        selected = "All")
      
      updateSelectInput(session, "historical_table_filter_category", 
                        choices = c("All", values$product_categories),
                        selected = "All")
      
      updateSelectInput(session, "chart_partner", 
                        choices = c("All", values$partner_categories),
                        selected = "All")
      
      updateSelectInput(session, "chart_category", 
                        choices = c("All", values$product_categories),
                        selected = "All")
      
      updateSelectInput(session, "pl_partner_category", 
                        choices = c("All", values$partner_categories),
                        selected = "All")
      
      # Show success message
      output$upload_status <- renderText({
        "Data successfully uploaded and processed!"
      })
      
      # Initialize forecast inputs
      initializeForecastInputs()
      
    }, error = function(e) {
      output$upload_status <- renderText({
        paste("Error processing file:", e$message)
      })
    })
  })
  
  # Create monthly input template for download
  output$download_monthly_template <- downloadHandler(
    filename = function() {
      "input_template.xlsx"
    },
    content = function(file) {
      req(values$has_uploaded)
      
      # Get forecast period start and end dates
      forecast_start <- input$forecast_period[1]
      forecast_end <- input$forecast_period[2]
      
      # Generate monthly dates for the forecast period
      forecast_dates <- seq(from = forecast_start, to = forecast_end, by = "month")
      forecast_dates <- as.Date(paste0(year(forecast_dates), "-", month(forecast_dates), "-01"))
      
      # Get unique partner and category combinations
      partner_category_combinations <- expand.grid(
        Partner_Category = values$partner_categories,
        Category = values$product_categories
      )
      
      # Create a template for each month and each partner-category combination
      template_rows <- list()
      
      for (i in 1:nrow(partner_category_combinations)) {
        partner <- partner_category_combinations$Partner_Category[i]
        category <- partner_category_combinations$Category[i]
        
        # Get the latest data for this combination
        latest_data <- values$last_historical_data %>%
          filter(Partner_Category == partner, Category == category)
        
        if (nrow(latest_data) > 0) {
          for (date in forecast_dates) {
            template_rows[[length(template_rows) + 1]] <- data.frame(
              Date = date,
              Partner_Category = partner,
              Category = category,
              Supply_Qty_Growth_Pct = 0.00,  # Default to 0% growth
              MRP = latest_data$MRP,
              Commission_Pct = latest_data$Commission_Pct,
              Return_Pct = latest_data$Return_Pct,
              GST_Pct = latest_data$GST_Pct,
              RM_per_unit = latest_data$RM_per_unit,
              PM_per_unit = latest_data$PM_per_unit,
              MFG_per_unit = latest_data$MFG_per_unit,
              Yield_Loss_Pct = latest_data$Yield_Loss_Pct
            )
          }
        }
      }
      
      # Combine all rows into a single data frame
      template_data <- do.call(rbind, template_rows)
      
      # Sort by Date, Partner_Category, and Category
      template_data <- template_data %>%
        arrange(Date, Partner_Category, Category)
      
      # Write to Excel
      writexl::write_xlsx(template_data, file)
    }
  )
  
  # Process monthly input upload
  observeEvent(input$process_monthly_inputs, {
    req(input$monthly_input_upload, values$has_uploaded)
    
    # Read the Excel file
    tryCatch({
      monthly_inputs <- readxl::read_excel(input$monthly_input_upload$datapath)
      
      # Check if all required columns exist
      required_cols <- c("Date", "Partner_Category", "Category", "Supply_Qty_Growth_Pct", "MRP", 
                         "Commission_Pct", "Return_Pct", "GST_Pct", "RM_per_unit", 
                         "PM_per_unit", "MFG_per_unit", "Yield_Loss_Pct")
      
      missing_cols <- required_cols[!(required_cols %in% colnames(monthly_inputs))]
      
      if (length(missing_cols) > 0) {
        showNotification(paste("Error: Missing required columns:", paste(missing_cols, collapse = ", ")), 
                         type = "error")
        return()
      }
      
      # Ensure Date is a Date type
      monthly_inputs$Date <- as.Date(monthly_inputs$Date)
      
      # Store the monthly inputs
      values$monthly_inputs <- monthly_inputs
      
      # Show success message
      showNotification("Monthly inputs successfully uploaded!", type = "message")
      
      # Generate forecast automatically
      generateForecastFromMonthlyInputs()
      
    }, error = function(e) {
      showNotification(paste("Error processing monthly inputs:", e$message), type = "error")
    })
  })
  
  # Preview of uploaded data
  output$preview_data <- DT::renderDataTable({
    req(values$raw_data)
    DT::datatable(head(values$raw_data, 10), 
                  options = list(scrollX = TRUE, 
                                 dom = 't',
                                 ordering = FALSE))
  })
  
  output$preview_instructions <- renderUI({
    req(values$raw_data)
    tagList(
      h4("Data Preview (first 10 rows)"),
      p("Go to 'Historical Data' tab to see full data and analysis.")
    )
  })
  
  # Historical data table
  output$historical_data_table <- DT::renderDataTable({
    req(values$processed_data)
    
    filtered_data <- values$processed_data
    
    # Apply partner category filter
    if (!("All" %in% input$historical_table_filter_partner)) {
      filtered_data <- filtered_data %>%
        filter(Partner_Category %in% input$historical_table_filter_partner)
    }
    
    # Apply category filter
    if (!("All" %in% input$historical_table_filter_category)) {
      filtered_data <- filtered_data %>%
        filter(Category %in% input$historical_table_filter_category)
    }
    
    DT::datatable(filtered_data, 
                  options = list(scrollX = TRUE,
                                 pageLength = 15,
                                 lengthMenu = c(15, 30, 50)),
                  rownames = FALSE) %>%
      formatCurrency(c("MRP", "MRP_Billing", "Commission", "Gross_Billing", 
                       "Return", "Gross_Sales", "GST", "Net_Sales",
                       "RM", "PM", "MFG", "Yield_Loss", "GM"), 
                     currency = "₹", digits = 0) %>%
      formatPercentage(c("Commission_Pct", "Return_Pct", "GST_Pct", 
                         "Yield_Loss_Pct", "GM_Pct"), 
                       digits = 2)
  })
  
  # Historical data chart
  output$historical_chart <- renderPlotly({
    req(values$processed_data, input$chart_metric)
    
    filtered_data <- values$processed_data
    
    # Apply partner category filter
    if (input$chart_partner != "All") {
      filtered_data <- filtered_data %>%
        filter(Partner_Category == input$chart_partner)
    }
    
    # Apply category filter
    if (input$chart_category != "All") {
      filtered_data <- filtered_data %>%
        filter(Category == input$chart_category)
    }
    
    # Aggregate data if requested
    if (input$chart_aggregate) {
      plot_data <- filtered_data %>%
        group_by(Date) %>%
        summarize(across(all_of(input$chart_metric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    } else {
      plot_data <- filtered_data %>%
        group_by(Date, Partner_Category, Category) %>%
        summarize(across(all_of(input$chart_metric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    }
    
    # Create plot
    p <- plot_ly(plot_data, x = ~Date)
    
    for (metric in input$chart_metric) {
      if (input$chart_aggregate) {
        p <- p %>% add_trace(y = as.formula(paste0("~", metric)), 
                             name = metric,
                             type = 'scatter', 
                             mode = 'lines+markers')
      } else {
        for (partner in unique(plot_data$Partner_Category)) {
          for (category in unique(plot_data$Category[plot_data$Partner_Category == partner])) {
            subset_data <- plot_data %>% 
              filter(Partner_Category == partner, Category == category)
            
            name <- paste(partner, category, metric)
            p <- p %>% add_trace(data = subset_data,
                                 y = as.formula(paste0("~", metric)), 
                                 name = name,
                                 type = 'scatter', 
                                 mode = 'lines+markers')
          }
        }
      }
    }
    
    # Format plot
    p <- p %>% layout(
      title = "Historical Data Trend",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      hovermode = "closest"
    )
    
    p
  })
  
  # Initialize forecast inputs
  initializeForecastInputs <- function() {
    req(values$processed_data)
    
    # Create a data frame for forecast inputs
    forecast_inputs <- expand.grid(
      Partner_Category = values$partner_categories,
      Category = values$product_categories
    )
    
    # Get the most recent values for each combination
    for (i in 1:nrow(forecast_inputs)) {
      partner <- forecast_inputs$Partner_Category[i]
      category <- forecast_inputs$Category[i]
      
      recent_data <- values$processed_data %>%
        filter(Partner_Category == partner, Category == category) %>%
        arrange(desc(Date)) %>%
        slice(1)
      
      if (nrow(recent_data) > 0) {
        forecast_inputs$Supply_Qty[i] <- recent_data$Supply_Qty
        forecast_inputs$Supply_Qty_Growth_Pct[i] <- 0.00  # Default to 0% growth
        forecast_inputs$MRP[i] <- recent_data$MRP
        forecast_inputs$Commission_Pct[i] <- recent_data$Commission_Pct
        forecast_inputs$Return_Pct[i] <- recent_data$Return_Pct
        forecast_inputs$GST_Pct[i] <- recent_data$GST_Pct
        forecast_inputs$RM_per_unit[i] <- recent_data$RM_per_unit
        forecast_inputs$PM_per_unit[i] <- recent_data$PM_per_unit
        forecast_inputs$MFG_per_unit[i] <- recent_data$MFG_per_unit
        forecast_inputs$Yield_Loss_Pct[i] <- recent_data$Yield_Loss_Pct
      }
    }
    
    values$forecast_inputs <- forecast_inputs
  }
  
  # Dynamic UI for forecast inputs
  output$forecast_inputs_ui <- renderUI({
    req(values$forecast_inputs)
    
    div(
      class = "row",
      div(
        class = "col-md-12",
        rHandsontableOutput("forecast_input_table")
      )
    )
  })
  
  # Render the forecast input table
  output$forecast_input_table <- renderRHandsontable({
    req(values$forecast_inputs)
    
    rhandsontable(values$forecast_inputs, stretchH = "all", height = 300) %>%
      hot_col("Partner_Category", readOnly = TRUE) %>%
      hot_col("Category", readOnly = TRUE) %>%
      hot_col("Supply_Qty", format = "0") %>%
      hot_col("Supply_Qty_Growth_Pct", format = "0.00%") %>%  # Column for growth percentage
      hot_col("MRP", format = "0.00") %>%
      hot_col("Commission_Pct", format = "0.00%") %>%
      hot_col("Return_Pct", format = "0.00%") %>%
      hot_col("GST_Pct", format = "0.00%") %>%
      hot_col("RM_per_unit", format = "0.00") %>%
      hot_col("PM_per_unit", format = "0.00") %>%
      hot_col("MFG_per_unit", format = "0.00") %>%
      hot_col("Yield_Loss_Pct", format = "0.00%")
  })
  
  # Update forecast inputs when edited
  observeEvent(input$forecast_input_table, {
    values$forecast_inputs <- hot_to_r(input$forecast_input_table)
  })
  
  # Generate forecast from monthly inputs
  generateForecastFromMonthlyInputs <- function() {
    req(values$monthly_inputs, values$last_historical_data)
    
    # Extract unique dates from monthly inputs
    forecast_dates <- unique(values$monthly_inputs$Date)
    forecast_dates <- sort(forecast_dates)
    
    # Create forecast data frame
    forecast_data <- data.frame()
    
    # For each partner/category combination
    unique_combinations <- values$monthly_inputs %>%
      select(Partner_Category, Category) %>%
      distinct()
    
    for (i in 1:nrow(unique_combinations)) {
      partner <- unique_combinations$Partner_Category[i]
      category <- unique_combinations$Category[i]
      
      # Get the last historical data point for this combination
      last_historical <- values$last_historical_data %>%
        filter(Partner_Category == partner, Category == category)
      
      if (nrow(last_historical) == 0) {
        next  # Skip if no historical data
      }
      
      # Get all monthly inputs for this combination
      monthly_data <- values$monthly_inputs %>%
        filter(Partner_Category == partner, Category == category) %>%
        arrange(Date)
      
      # Start with the last historical supply quantity
      last_supply_qty <- last_historical$Supply_Qty[1]
      
      # For each forecast date
      for (idx in seq_along(forecast_dates)) {
        date <- forecast_dates[idx]
        
        # Find the input data for this date
        date_data <- monthly_data %>%
          filter(Date == date)
        
        if (nrow(date_data) == 0) {
          next  # Skip if no data for this date
        }
        
        # Calculate new supply quantity based on growth percentage
        if (idx == 1) {
          # First forecast month - apply growth to last historical data
          new_supply_qty <- last_supply_qty * (1 + date_data$Supply_Qty_Growth_Pct)
        } else {
          # Subsequent months - apply growth to previous forecast month
          prev_forecast_supply_qty <- forecast_data %>%
            filter(Partner_Category == partner, Category == category, Date == forecast_dates[idx-1]) %>%
            pull(Supply_Qty)
          
          if (length(prev_forecast_supply_qty) > 0) {
            new_supply_qty <- prev_forecast_supply_qty * (1 + date_data$Supply_Qty_Growth_Pct)
          } else {
            new_supply_qty <- last_supply_qty * (1 + date_data$Supply_Qty_Growth_Pct)
          }
        }
        
        # Create forecast row
        forecast_row <- data.frame(
          Date = date,
          Partner_Category = partner,
          Category = category,
          Supply_Qty = round(new_supply_qty),
          MRP = date_data$MRP,
          Commission_Pct = date_data$Commission_Pct,
          Return_Pct = date_data$Return_Pct,
          GST_Pct = date_data$GST_Pct,
          RM_per_unit = date_data$RM_per_unit,
          PM_per_unit = date_data$PM_per_unit,
          MFG_per_unit = date_data$MFG_per_unit,
          Yield_Loss_Pct = date_data$Yield_Loss_Pct
        )
        
        # Calculate derived metrics
        forecast_row <- forecast_row %>%
          mutate(
            MRP_Billing = Supply_Qty * MRP,
            Commission = MRP_Billing * Commission_Pct,
            Gross_Billing = MRP_Billing - Commission,
            Return = Gross_Billing * Return_Pct,
            Gross_Sales = Gross_Billing - Return,
            GST = Gross_Sales * GST_Pct,
            Net_Sales = Gross_Sales - GST,
            RM = RM_per_unit * Supply_Qty,
            PM = PM_per_unit * Supply_Qty,
            MFG = MFG_per_unit * Supply_Qty,
            Yield_Loss = Yield_Loss_Pct * Gross_Billing,
            GM = Net_Sales - RM - PM - MFG - Yield_Loss,
            GM_Pct = GM / Net_Sales
          )
        
        # Add to forecast data
        forecast_data <- rbind(forecast_data, forecast_row)
        
        # Update last supply quantity for next iteration
        last_supply_qty <- new_supply_qty
      }
    }
    
    # Store forecast data
    values$forecast_data <- forecast_data
    values$has_forecast <- TRUE
    
    # Update forecast table filters
    updateSelectInput(session, "forecast_table_filter_partner", 
                      choices = c("All", unique(forecast_data$Partner_Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_table_filter_category", 
                      choices = c("All", unique(forecast_data$Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_chart_partner", 
                      choices = c("All", unique(forecast_data$Partner_Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_chart_category", 
                      choices = c("All", unique(forecast_data$Category)),
                      selected = "All")
    
    # Show success message
    showNotification("Forecast generated successfully!", type = "message")
  }
  
  # Generate forecast using selected method
  observeEvent(input$generate_forecast, {
    req(values$processed_data, values$forecast_inputs)
    
    # Get forecast period start and end dates
    forecast_start <- input$forecast_period[1]
    forecast_end <- input$forecast_period[2]
    
    # Check if monthly inputs exist and use them if available
    if (!is.null(values$monthly_inputs)) {
      generateForecastFromMonthlyInputs()
      return()
    }
    
    # Generate monthly dates for the forecast period
    forecast_dates <- seq(from = forecast_start, to = forecast_end, by = "month")
    forecast_dates <- as.Date(paste0(year(forecast_dates), "-", month(forecast_dates), "-01"))
    
    # Create forecast data frame
    forecast_data <- data.frame()
    
    # For each partner/category combination
    for (i in 1:nrow(values$forecast_inputs)) {
      partner <- values$forecast_inputs$Partner_Category[i]
      category <- values$forecast_inputs$Category[i]
      
      # Get inputs for this combination
      inputs <- values$forecast_inputs[i, ]
      
      # Get historical data for this combination
      historical_data <- values$processed_data %>%
        filter(Partner_Category == partner, Category == category) %>%
        arrange(Date)
      
      if (nrow(historical_data) == 0) {
        next  # Skip if no historical data
      }
      
      # Get the last historical value
      last_historical <- historical_data %>% slice_tail(n = 1)
      last_date <- last_historical$Date
      last_supply_qty <- last_historical$Supply_Qty
      
      # For each forecast date
      for (date_idx in seq_along(forecast_dates)) {
        date <- forecast_dates[date_idx]
        
        # Calculate supply quantity based on forecasting method
        if (input$forecast_method == "simple") {
          # Simple growth rate
          if (date_idx == 1) {
            supply_qty <- last_supply_qty * (1 + inputs$Supply_Qty_Growth_Pct)
          } else {
            prev_forecast <- forecast_data %>%
              filter(Partner_Category == partner, Category == category, Date == forecast_dates[date_idx-1])
            
            if (nrow(prev_forecast) > 0) {
              supply_qty <- prev_forecast$Supply_Qty * (1 + inputs$Supply_Qty_Growth_Pct)
            } else {
              supply_qty <- last_supply_qty * (1 + inputs$Supply_Qty_Growth_Pct)
            }
          }
        } else if (input$forecast_method == "ets") {
          # Exponential smoothing
          ts_data <- ts(historical_data$Supply_Qty, frequency = 12)
          forecast_periods <- as.numeric(date - last_date) / 30  # Approximate months
          ets_model <- forecast::ets(ts_data)
          ets_forecast <- forecast::forecast(ets_model, h = ceiling(forecast_periods))
          forecast_value <- as.numeric(ets_forecast$mean[ceiling(forecast_periods)])
          supply_qty <- max(0, forecast_value)
        } else {
          # Manual inputs only
          supply_qty <- inputs$Supply_Qty
        }
        
        # Create forecast row
        forecast_row <- data.frame(
          Date = date,
          Partner_Category = partner,
          Category = category,
          Supply_Qty = round(supply_qty),
          MRP = inputs$MRP,
          Commission_Pct = inputs$Commission_Pct,
          Return_Pct = inputs$Return_Pct,
          GST_Pct = inputs$GST_Pct,
          RM_per_unit = inputs$RM_per_unit,
          PM_per_unit = inputs$PM_per_unit,
          MFG_per_unit = inputs$MFG_per_unit,
          Yield_Loss_Pct = inputs$Yield_Loss_Pct
        )
        
        # Calculate derived metrics
        forecast_row <- forecast_row %>%
          mutate(
            MRP_Billing = Supply_Qty * MRP,
            Commission = MRP_Billing * Commission_Pct,
            Gross_Billing = MRP_Billing - Commission,
            Return = Gross_Billing * Return_Pct,
            Gross_Sales = Gross_Billing - Return,
            GST = Gross_Sales * GST_Pct,
            Net_Sales = Gross_Sales - GST,
            RM = RM_per_unit * Supply_Qty,
            PM = PM_per_unit * Supply_Qty,
            MFG = MFG_per_unit * Supply_Qty,
            Yield_Loss = Yield_Loss_Pct * Gross_Billing,
            GM = Net_Sales - RM - PM - MFG - Yield_Loss,
            GM_Pct = GM / Net_Sales
          )
        
        # Add to forecast data
        forecast_data <- rbind(forecast_data, forecast_row)
      }
    }
    
    # Store forecast data
    values$forecast_data <- forecast_data
    values$has_forecast <- TRUE
    
    # Update forecast table filters
    updateSelectInput(session, "forecast_table_filter_partner", 
                      choices = c("All", unique(forecast_data$Partner_Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_table_filter_category", 
                      choices = c("All", unique(forecast_data$Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_chart_partner", 
                      choices = c("All", unique(forecast_data$Partner_Category)),
                      selected = "All")
    
    updateSelectInput(session, "forecast_chart_category", 
                      choices = c("All", unique(forecast_data$Category)),
                      selected = "All")
    
    # Show success message
    showNotification("Forecast generated successfully!", type = "message")
  })
  
  # Forecast data table
  output$forecast_data_table <- DT::renderDataTable({
    req(values$forecast_data)
    
    filtered_data <- values$forecast_data
    
    # Apply partner category filter
    if (!("All" %in% input$forecast_table_filter_partner)) {
      filtered_data <- filtered_data %>%
        filter(Partner_Category %in% input$forecast_table_filter_partner)
    }
    
    # Apply category filter
    if (!("All" %in% input$forecast_table_filter_category)) {
      filtered_data <- filtered_data %>%
        filter(Category %in% input$forecast_table_filter_category)
    }
    
    DT::datatable(filtered_data, 
                  options = list(scrollX = TRUE,
                                 pageLength = 15,
                                 lengthMenu = c(15, 30, 50)),
                  rownames = FALSE) %>%
      formatCurrency(c("MRP", "MRP_Billing", "Commission", "Gross_Billing", 
                       "Return", "Gross_Sales", "GST", "Net_Sales",
                       "RM", "PM", "MFG", "Yield_Loss", "GM"), 
                     currency = "₹", digits = 0) %>%
      formatPercentage(c("Commission_Pct", "Return_Pct", "GST_Pct", 
                         "Yield_Loss_Pct", "GM_Pct"), 
                       digits = 2)
  })
  
  # Forecast chart
  output$forecast_chart <- renderPlotly({
    req(values$forecast_data, input$forecast_chart_metric)
    
    # Start with forecast data
    chart_data <- values$forecast_data
    
    # Include historical data if requested
    if (input$forecast_chart_include_historical && !is.null(values$processed_data)) {
      chart_data <- rbind(values$processed_data, chart_data)
    }
    
    # Apply partner category filter
    if (input$forecast_chart_partner != "All") {
      chart_data <- chart_data %>%
        filter(Partner_Category == input$forecast_chart_partner)
    }
    
    # Apply category filter
    if (input$forecast_chart_category != "All") {
      chart_data <- chart_data %>%
        filter(Category == input$forecast_chart_category)
    }
    
    # Aggregate data if requested
    if (input$forecast_chart_aggregate) {
      plot_data <- chart_data %>%
        group_by(Date) %>%
        summarize(across(all_of(input$forecast_chart_metric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      
      # Add a column to identify forecast vs historical
      if (input$forecast_chart_include_historical && !is.null(values$processed_data)) {
        last_historical_date <- max(values$processed_data$Date)
        plot_data$Type <- ifelse(plot_data$Date <= last_historical_date, "Historical", "Forecast")
      } else {
        plot_data$Type <- "Forecast"
      }
    } else {
      plot_data <- chart_data %>%
        group_by(Date, Partner_Category, Category) %>%
        summarize(across(all_of(input$forecast_chart_metric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
      
      # Add a column to identify forecast vs historical
      if (input$forecast_chart_include_historical && !is.null(values$processed_data)) {
        last_historical_date <- max(values$processed_data$Date)
        plot_data$Type <- ifelse(plot_data$Date <= last_historical_date, "Historical", "Forecast")
      } else {
        plot_data$Type <- "Forecast"
      }
    }
    
    # Create plot
    p <- plot_ly(plot_data, x = ~Date)
    
    for (metric in input$forecast_chart_metric) {
      if (input$forecast_chart_aggregate) {
        # Split by historical/forecast
        for (data_type in unique(plot_data$Type)) {
          subset_data <- plot_data %>% filter(Type == data_type)
          line_style <- if(data_type == "Historical") "solid" else "dash"
          
          p <- p %>% add_trace(data = subset_data,
                               y = as.formula(paste0("~", metric)), 
                               name = paste(metric, "-", data_type),
                               type = 'scatter', 
                               mode = 'lines+markers',
                               line = list(dash = line_style))
        }
      } else {
        for (partner in unique(plot_data$Partner_Category)) {
          for (category in unique(plot_data$Category[plot_data$Partner_Category == partner])) {
            # Split by historical/forecast
            for (data_type in unique(plot_data$Type)) {
              subset_data <- plot_data %>% 
                filter(Partner_Category == partner, Category == category, Type == data_type)
              
              if (nrow(subset_data) > 0) {
                name <- paste(partner, category, metric, "-", data_type)
                line_style <- if(data_type == "Historical") "solid" else "dash"
                
                p <- p %>% add_trace(data = subset_data,
                                     y = as.formula(paste0("~", metric)), 
                                     name = name,
                                     type = 'scatter', 
                                     mode = 'lines+markers',
                                     line = list(dash = line_style))
              }
            }
          }
        }
      }
    }
    
    # Format plot
    p <- p %>% layout(
      title = "Forecast Data Trend",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Value"),
      hovermode = "closest"
    )
    
    p
  })
  
  # Download forecast data
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste("forecast_data_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      req(values$forecast_data)
      writexl::write_xlsx(values$forecast_data, file)
    }
  )
  
  # P&L table
  output$pl_table <- DT::renderDataTable({
    req(values$processed_data)
    
    # Determine which data to use
    if (input$pl_view_type == "historical") {
      pl_data <- values$processed_data
    } else if (input$pl_view_type == "forecast" && !is.null(values$forecast_data)) {
      pl_data <- values$forecast_data
    } else if (input$pl_view_type == "both" && !is.null(values$forecast_data)) {
      pl_data <- rbind(values$processed_data, values$forecast_data)
    } else {
      pl_data <- values$processed_data
    }
    
    # Filter by partner category if selected
    if (!("All" %in% input$pl_partner_category)) {
      pl_data <- pl_data %>%
        filter(Partner_Category %in% input$pl_partner_category)
    }
    
    # Group by time period
    if (input$pl_time_period == "monthly") {
      pl_data <- pl_data %>%
        mutate(Period = format(Date, "%Y-%m")) %>%
        group_by(Period)
    } else if (input$pl_time_period == "quarterly") {
      pl_data <- pl_data %>%
        mutate(
          Quarter = quarter(Date),
          Year = year(Date),
          Period = paste0(Year, "-Q", Quarter)
        ) %>%
        group_by(Period)
    } else if (input$pl_time_period == "yearly") {
      pl_data <- pl_data %>%
        mutate(
          # For fiscal year April-March
          FY = ifelse(month(Date) >= 4, 
                      paste0("FY", year(Date), "-", year(Date) + 1),
                      paste0("FY", year(Date) - 1, "-", year(Date))),
          Period = FY
        ) %>%
        group_by(Period)
    }
    
    # Aggregate P&L data
    pl_summary <- pl_data %>%
      summarize(
        Supply_Qty = sum(Supply_Qty, na.rm = TRUE),
        Gross_Billing = sum(Gross_Billing, na.rm = TRUE),
        Net_Sales = sum(Net_Sales, na.rm = TRUE),
        RM = sum(RM, na.rm = TRUE),
        PM = sum(PM, na.rm = TRUE),
        MFG = sum(MFG, na.rm = TRUE),
        Yield_Loss = sum(Yield_Loss, na.rm = TRUE),
        GM = sum(GM, na.rm = TRUE),
        GM_Pct = GM / Net_Sales,
        .groups = "drop"
      ) %>%
      arrange(Period)
    
    DT::datatable(pl_summary, 
                  options = list(scrollX = TRUE,
                                 pageLength = 15),
                  rownames = FALSE) %>%
      formatCurrency(c("Gross_Billing", "Net_Sales", "RM", "PM", "MFG", "Yield_Loss", "GM"), 
                     currency = "₹", digits = 0) %>%
      formatPercentage("GM_Pct", digits = 2)
  })
  
  # P&L chart
  output$pl_chart <- renderPlotly({
    req(values$processed_data)
    
    # Determine which data to use
    if (input$pl_view_type == "historical") {
      pl_data <- values$processed_data
    } else if (input$pl_view_type == "forecast" && !is.null(values$forecast_data)) {
      pl_data <- values$forecast_data
    } else if (input$pl_view_type == "both" && !is.null(values$forecast_data)) {
      pl_data <- rbind(values$processed_data, values$forecast_data)
    } else {
      pl_data <- values$processed_data
    }
    
    # Filter by partner category if selected
    if (!("All" %in% input$pl_partner_category)) {
      pl_data <- pl_data %>%
        filter(Partner_Category %in% input$pl_partner_category)
    }
    
    # Group by time period
    if (input$pl_time_period == "monthly") {
      pl_data <- pl_data %>%
        mutate(Period = format(Date, "%Y-%m")) %>%
        group_by(Period)
    } else if (input$pl_time_period == "quarterly") {
      pl_data <- pl_data %>%
        mutate(
          Quarter = quarter(Date),
          Year = year(Date),
          Period = paste0(Year, "-Q", Quarter)
        ) %>%
        group_by(Period)
    } else if (input$pl_time_period == "yearly") {
      pl_data <- pl_data %>%
        mutate(
          # For fiscal year April-March
          FY = ifelse(month(Date) >= 4, 
                      paste0("FY", year(Date), "-", year(Date) + 1),
                      paste0("FY", year(Date) - 1, "-", year(Date))),
          Period = FY
        ) %>%
        group_by(Period)
    }
    
    # Aggregate P&L data
    pl_summary <- pl_data %>%
      summarize(
        Net_Sales = sum(Net_Sales, na.rm = TRUE),
        RM = sum(RM, na.rm = TRUE),
        PM = sum(PM, na.rm = TRUE),
        MFG = sum(MFG, na.rm = TRUE),
        Yield_Loss = sum(Yield_Loss, na.rm = TRUE),
        GM = sum(GM, na.rm = TRUE),
        GM_Pct = GM / Net_Sales,
        .groups = "drop"
      ) %>%
      arrange(Period)
    
    # Create plot
    p <- plot_ly() %>%
      add_trace(data = pl_summary, x = ~Period, y = ~Net_Sales, 
                type = 'bar', name = 'Net Sales',
                marker = list(color = '#2ECC71')) %>%
      add_trace(data = pl_summary, x = ~Period, y = ~GM, 
                type = 'bar', name = 'Gross Margin',
                marker = list(color = '#3498DB'))
    
    # Add GM% as a line on secondary y-axis
    p <- p %>%
      add_trace(data = pl_summary, x = ~Period, y = ~GM_Pct, 
                type = 'scatter', mode = 'lines+markers', name = 'GM%', 
                yaxis = 'y2',
                line = list(color = '#E74C3C', width = 3),
                marker = list(color = '#E74C3C', size = 8))
    
    # Format plot
    p <- p %>% layout(
      title = "P&L Summary",
      xaxis = list(title = "Period", categoryorder = "array", categoryarray = pl_summary$Period),
      yaxis = list(title = "Amount (₹)", side = 'left'),
      yaxis2 = list(title = "GM%", side = 'right', overlaying = "y", 
                    tickformat = ".0%", rangemode = "tozero"),
      barmode = 'group',
      hovermode = "x unified"
    )
    
    p
  })
  
  # Download P&L data
  output$download_pl <- downloadHandler(
    filename = function() {
      paste("pl_summary_", format(Sys.Date(), "%Y%m%d"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Determine which data to use
      if (input$pl_view_type == "historical") {
        pl_data <- values$processed_data
      } else if (input$pl_view_type == "forecast" && !is.null(values$forecast_data)) {
        pl_data <- values$forecast_data
      } else if (input$pl_view_type == "both" && !is.null(values$forecast_data)) {
        pl_data <- rbind(values$processed_data, values$forecast_data)
      } else {
        pl_data <- values$processed_data
      }
      
      # Filter by partner category if selected
      if (!("All" %in% input$pl_partner_category)) {
        pl_data <- pl_data %>%
          filter(Partner_Category %in% input$pl_partner_category)
      }
      
      # Group by time period
      if (input$pl_time_period == "monthly") {
        pl_data <- pl_data %>%
          mutate(Period = format(Date, "%Y-%m")) %>%
          group_by(Period)
      } else if (input$pl_time_period == "quarterly") {
        pl_data <- pl_data %>%
          mutate(
            Quarter = quarter(Date),
            Year = year(Date),
            Period = paste0(Year, "-Q", Quarter)
          ) %>%
          group_by(Period)
      } else if (input$pl_time_period == "yearly") {
        pl_data <- pl_data %>%
          mutate(
            # For fiscal year April-March
            FY = ifelse(month(Date) >= 4, 
                        paste0("FY", year(Date), "-", year(Date) + 1),
                        paste0("FY", year(Date) - 1, "-", year(Date))),
            Period = FY
          ) %>%
          group_by(Period)
      }
      
      # Aggregate P&L data
      pl_summary <- pl_data %>%
        summarize(
          Supply_Qty = sum(Supply_Qty, na.rm = TRUE),
          Gross_Billing = sum(Gross_Billing, na.rm = TRUE),
          Net_Sales = sum(Net_Sales, na.rm = TRUE),
          RM = sum(RM, na.rm = TRUE),
          PM = sum(PM, na.rm = TRUE),
          MFG = sum(MFG, na.rm = TRUE),
          Yield_Loss = sum(Yield_Loss, na.rm = TRUE),
          GM = sum(GM, na.rm = TRUE),
          GM_Pct = GM / Net_Sales,
          .groups = "drop"
        ) %>%
        arrange(Period)
      
      writexl::write_xlsx(pl_summary, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)