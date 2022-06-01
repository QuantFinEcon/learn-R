library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(lubridate)

# setwd("C:\\Users\\yeoshuiming\\Downloads\\Assignments")


# Load data ---------------------------------------------------------
perf_data = read.csv("./data/fund_performance_tidy.csv")
perf_data$Date <- as.POSIXct(perf_data$Date, format = "%d/%m/%Y")
perf_data = perf_data[order(perf_data$Date), ]

choiceMeasure = unique(perf_data$Measure)
choiceLookback = unique(perf_data$LookbackPeriod)
years = unique(perf_data$Date)



# UI  ---------------------------------------------------------
ui <-
  fluidPage(# App title -------------------------------------------------------
            titlePanel(h2(
              "Risk Analysis Dashboard",
              align = "center",
              style = list(fontWeight = "bold")
            )),
            
            # Sidebar layout with a input and output definitions --------------
            sidebarLayout(
              # Inputs --------------------------------------------------------
              sidebarPanel(
                sliderInput(
                  inputId = "dateRange",
                  label = "Month End Dates",
                  min = min(perf_data$Date),
                  max = max(perf_data$Date),
                  step = 1,
                  value = c(max(perf_data$Date) - days(30 * 15), max(perf_data$Date)),
                  timeFormat = "%Y-%m-%d"
                ),
                
                selectInput(
                  inputId = "plotType",
                  label = "Plot Type:",
                  choices = c(
                    "Area" = "area",
                    "Bar" = "column",
                    "Line" = "line"
                  ),
                  selected = "column"
                ),
                
                selectInput(
                  inputId = "measureSelect",
                  label = "Risk measure:",
                  choices = choiceMeasure,
                  selected = "TotalReturn"
                ),
                
                selectInput(
                  inputId = "lookbackPeriod",
                  label = "Lookback Period:",
                  choices = choiceLookback,
                  selected = "MTD"
                ),
                
                selectInput(
                  inputId = "theme",
                  label = "Theme",
                  choices = c(
                    "No theme",
                    "Chalk" = "chalk",
                    "Dark Unica" = "darkunica",
                    "Economist" = "economist",
                    "FiveThirtyEight" = "fivethirtyeight",
                    "Gridlight" = "gridlight",
                    "Handdrawn" = "handdrawn",
                    "Sandsignika" = "sandsignika"
                  )
                )
              ),
              
              # Output --------------------------------------------------------
              mainPanel(highchartOutput("hcontainer", height = "500px"))
              
            ))

# SERVER ------------------------------------------------------------
server = function(input, output) {
  # Data reactive to filter -----------------------------------------
  datasetInput <- reactive({
    perf_data %>%
      filter(Measure == input$measureSelect) %>%
      filter(LookbackPeriod == input$lookbackPeriod) %>%
      filter(between(Date, input$dateRange[1], input$dateRange[2]))
  })
  
  # Highchart -------------------------------------------------------
  output$hcontainer <- renderHighchart({
    hc <- highchart() %>%
      hc_add_series(
        data = (datasetInput() %>% filter(Portfolio == "Benchmark"))$value,
        type = input$plotType,
        name = "Benchmark",
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        data = (datasetInput() %>% filter(Portfolio == "Portfolio"))$value,
        type = input$plotType,
        name = "Portfolio",
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        data = (datasetInput() %>% filter(Portfolio == "ActiveReturn"))$value,
        type = input$plotType,
        name = "Active Return",
        showInLegend = TRUE
      )  %>%
      hc_yAxis(title = list(text = "Percentage (%)"),
               allowDecimals = FALSE) %>%
      hc_xAxis(
        categories = format((
          datasetInput() %>% filter(Portfolio == "Portfolio")
        )$Date, "%b-%y"),
        title = list(text = "<b>Date</b>"),
        type = "datetime",
        labels = list(rotation = 90),
        tickmarkPlacement = "on"
      ) %>%
      hc_title(
        text = paste(
          "Fund Performance:",
          input$measureSelect,
          ", ",
          input$lookbackPeriod
        ),
        style = list(fontWeight = "bold")
      ) %>%
      hc_subtitle(text = paste(
        "Date Range:",
        format(input$dateRange[1], "%d-%m-%Y"),
        "-",
        format(input$dateRange[2], "%d-%m-%Y")
      )) %>%
      hc_tooltip(valueDecimals = 4,
                 pointFormat = "{point.y}%") %>%
      hc_credits(
        enabled = TRUE,
        text = "Sources: XIX Investment Management (TH)",
        style = list(fontSize = "10px")
      )
    
    # Determine theme and apply to highchart ------------------------
    if (input$theme != "No theme") {
      theme <- switch(
        input$theme,
        chalk = hc_theme_chalk(),
        darkunica = hc_theme_darkunica(),
        fivethirtyeight = hc_theme_538(),
        gridlight = hc_theme_gridlight(),
        handdrawn = hc_theme_handdrawn(),
        economist = hc_theme_economist(),
        sandsignika = hc_theme_sandsignika()
      )
      hc <- hc %>% hc_add_theme(theme)
    }
    
    # Print highchart -----------------------------------------------
    hc
  }) #renderHighchart
  
  
} #server


# Run app -----------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)


# Resources ------------------------------------------------------
# shiny::runGitHub("learn-R", "QuantFinEcon", subdir = "shiny")
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "118-highcharter-births")
# https://byollin.github.io/RInteractiveCharts/#1
