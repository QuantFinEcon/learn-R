library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(lubridate)
library(pivottabler)
library(shinyWidgets)

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
  fluidPage(
    # App title -------------------------------------------------------
    titlePanel(h2(
      "Risk Analysis Dashboard",
      align = "center",
      style = list(fontWeight = "bold")
    )),
    
    # Output --------------------------------------------------------
    mainPanel(
      "Copyrights(c)2022 XIX",
      fluidRow(highchartOutput(
        "hcontainer", height = "550px", width = "100%"
      )),
      hr(),
      fluidRow(align="center",pivottablerOutput(
        "pvt", height = "550px", width = "100%"
      ))
    ),
    hr(),
    
    # Bottom row layout with a input and output definitions --------------
    sidebarPanel(
      h4("Chart Parameters:"),
      sliderInput(
        inputId = "dateRange",
        label = "Month End Dates",
        min = min(perf_data$Date),
        max = max(perf_data$Date),
        step = 1,
        value = c(max(perf_data$Date) - days(30 * 15), max(perf_data$Date)),
        timeFormat = "%Y-%m-%d",
        width = "100%"
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
        ),
        selected = "fivethirtyeight"
      ),
      br(),
      h4("Table Parameters:"),
      selectInput(
        inputId = "TableDate",
        label = "Pivot Table Date:",
        choices = years,
        selected = max(perf_data$Date)
      ),
      multiInput(
        inputId = "listMeasures",
        label = "List of Risk measures:",
        choices = choiceMeasure,
        selected = c("TotalReturn", "DownsideRisk", "Alpha")
      )
    )
  )

# SERVER ------------------------------------------------------------
server = function(input, output) {
  # Data reactive to filter -----------------------------------------
  datasetInput <- reactive({
    perf_data %>%
      filter(Measure == input$measureSelect) %>%
      filter(LookbackPeriod == input$lookbackPeriod) %>%
      filter(between(Date, input$dateRange[1], input$dateRange[2]))
  })
  pivotTblData <- reactive({
    perf_data %>%
      filter(Date == input$TableDate)
  })
  # Pivottabler -------------------------------------------------------
  tblData <- reactive({
    perf_data %>%
      filter(Date == input$TableDate) %>%
      filter(Measure %in% input$listMeasures)
  })
  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(tblData())
    pt$addColumnDataGroups(
      "LookbackPeriod",
      addTotal = FALSE,
      dataSortOrder = "custom",
      customSortOrder = c("MTD", "1M", "3M", "1Y", "3Y")
    )
    pt$addColumnDataGroups(
      "Measure",
      addTotal = FALSE,
      dataSortOrder = "custom",
      customSortOrder = choiceMeasure
    )
    pt$addRowDataGroups(
      "Portfolio",
      addTotal = FALSE,
      dataSortOrder = "custom",
      customSortOrder = c("Portfolio", "Benchmark", "ActiveReturn")
    )
    pt$defineCalculation(
      calculationName = "value",
      summariseExpression = "mean(value, na.rm=TRUE)",
      format = "%.3f"
    )
    pt$evaluatePivot()
    pivottabler(pt)
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
        labels = list(rotation = 315),
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
# shiny::runGitHub("learn-R", "QuantFinEcon", subdir = "shiny-fundperformance")
# shiny::runGitHub("shiny-examples", "rstudio", subdir = "118-highcharter-births")
# https://byollin.github.io/RInteractiveCharts/#1
# http://www2.stat.duke.edu/courses/Summer20/sta323.001-1/lecture_slides/lec-17.html#1
# https://cran.r-project.org/web/packages/highcharter/highcharter.pdf
# http://www.pivottabler.org.uk/articles/v14-shiny.html
# http://pivottabler.org.uk/articles/v03-calculations.html
