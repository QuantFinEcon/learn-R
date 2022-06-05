# Fund Attribution Dashboard

library(shiny)
library(highcharter)
library(dplyr)
library(tidyr)
library(lubridate)
library(pivottabler)
library(shinyWidgets)
library(randomForest)
# setwd("C:\\Users\\yeoshuiming\\Downloads\\Assignments\\shiny-fundattribution")
source("./scripts/roll_attribution.R")
source("./scripts/perfMetrics.R")


# Load data ---------------------------------------------------------
bbg_data = read.csv("./data/bbg_data.csv")
bbg_data$Date = ceiling_date(as.Date(bbg_data$Date, format = "%Y-%m-%d"), unit =
                               "months")
rownames(bbg_data) = bbg_data$Date
bbg_data = bbg_data[-1]

hfperf = read.csv("./data/hedge_fund_raw.csv", sep = ";")
hfperf$date = ceiling_date(as.Date(hfperf$date, format = "%d/%m/%Y"), unit =
                             "months")
rownames(hfperf) = hfperf$date
hfperf = hfperf[-1]
r = rownames(hfperf)
hfperf = as.data.frame(apply(hfperf, 2, function(y)
  as.numeric(gsub("%", "", y)) / 100))
rownames(hfperf) = r

prices = merge(hfperf, bbg_data, by = "row.names", all = TRUE)
rownames(prices) = prices$Row.names
prices = prices[-1]


choiceFund = unique(colnames(hfperf))
choicePredictors = unique(colnames(bbg_data))
choiceDate = unique(rownames(prices))


# UI  ---------------------------------------------------------
ui <-
  fluidPage(
    # App title -------------------------------------------------------
    titlePanel(
      h2(
        "Hedge Fund Performance Attribution",
        align = "center",
        style = list(fontWeight = "bold")
      )
    ),
    
    # Output --------------------------------------------------------

    mainPanel(
      "Copyrights(c)2022 XIX",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Sensitivity",
          fluidRow(
            highchartOutput(
              "regressionFitActualPlot",
              height = "400px",
              width = "100%"
            )
          ),
          hr(),
          fluidRow(
            highchartOutput("regressionBetaPlot",
                            height = "400px",
                            width = "100%")
          )
        ),
        tabPanel("Performance Attribution",
                 fluidRow(
                   highchartOutput(
                     "regressionAttributionPlot",
                     height = "400px",
                     width = "100%"
                   )
                 ),
                 hr(),
                 fluidRow(
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     highchartOutput("varImpRFPlot",height = "400px",width = "100%"),
                     tableOutput('riskMetricTbl')
                   )
                 )
               )
      )
    ),
    
    # Bottom row layout with a input and output definitions --------------
    sidebarPanel(
      h4("Regression Parameters:"),
      selectInput(
        inputId = "var_outcome",
        label = "Outcome Variable:",
        choices = choiceFund,
        selected = "Global.Macro"
      ),
      
      multiInput(
        inputId = "var_predictors",
        label = "Predictor variables:",
        choices = c(choicePredictors, choiceFund),
        selected = c("SPX_Index", "VIX_Index")
      ),
      
      numericInput(
        inputId = "reg_lookback",
        label = "Rolling Lookback Window",
        value = 12,
        min = 1,
        max = 12,
        step = 1
      ),
      
      selectInput(
        inputId = "metrics_benchmark",
        label = "Benchmark for Risk Metrics:",
        choices = choicePredictors,
        selected = "SPX_Index"
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
      )
    )
  )

# SERVER ------------------------------------------------------------
server = function(input, output, session) {
  # Update Input from server based on other inputs ----------------------
  observe({
    selectedFund = input$var_outcome
    selectedPredictors = switch(
      selectedFund,
      "CTA.Global" = c(
        "IV_RV_spread",
        "VVIX_Index",
        "SKEW_Index",
        "SUM_OSC_Index",
        "TRADPAUS_Index"
      ),
      "Global.Macro" = c(
        "SPX_Index",
        "VIX_Index",
        "USGG10YR_Index",
        "CRY_Index",
        "USYC2Y10_Index"
      ),
      "Relative.Value" = c(
        "SPX_Index",
        "VIX_Index",
        "USGG10YR_Index",
        "CRY_Index",
        "USYC2Y10_Index"
      ),
      "Fixed.Income.Arbitrage" = c(
        "USSP10_BGN_Curncy",
        #swap spread
        "USYC2Y10_Index",
        #tenor spread
        "BICLB10Y_Index",
        #credit spread
        ".TED_G_Index",
        #interbank spread
        "USGG10YR_Index" #10Y yield starting point
      ),
      "Long.Short.Equity" = c(
        "MXI_US_Equity",
        "JXI_US_Equity",
        "RXI_US_Equity",
        "KXI_US_Equity",
        "IXC_US_Equity",
        "IXG_US_Equity",
        "IXJ_US_Equity",
        "EXI_US_Equity",
        "IXN_US_Equity",
        "IXP_US_Equity"
      ),
      "Equity.Market.Neutral" = c("SPX_US_Equity"),
      "Emerging.Markets" = c("SPX_US_Equity", "USGG10YR_Index", "CRY_Index"),
      "Funds.Of.Funds" = c(
        "Convertible.Arbitrage",
        "CTA.Global",
        "Emerging.Markets",
        "Fixed.Income.Arbitrage",
        "Long/Short.Equity",
        "Merger.Arbitrage",
        "Short.Selling"
      )
    )
    
    updateSelectInput(session,
                      inputId = "var_predictors",
                      selected = selectedPredictors)
  })
  
  # Data reactive to filter -----------------------------------------
  regForecast <- reactive({
    lm_predict = rollRegPredict(
      prices,
      width = input$reg_lookback,
      outcome = input$var_outcome,
      predictor = input$var_predictors
    )
    return(lm_predict)
  })
  
  regStatsBeta <- reactive({
    lm_beta = rollRegStats(
      prices,
      width = input$reg_lookback,
      outcome = input$var_outcome,
      predictor = input$var_predictors,
      stats = "beta"
    )
    return(lm_beta)
  })
  
  regR2Attribution <- reactive({
    # rolling R2 as attribution of explained variance
    lm_R2 = rollRegR2(
      prices,
      width = input$reg_lookback,
      outcome = input$var_outcome,
      predictor = input$var_predictors
    )
    # rolling incremental R2 as proportion of total R2 -> attribution
    lm_IncrR2 = rollIncR2(
      prices,
      width = input$reg_lookback,
      outcome = input$var_outcome,
      predictor = input$var_predictors
    )
    IncrR2_normalised = t(apply(lm_IncrR2, 1, function(x)
      x / sum(x)))
    R2_normalised = IncrR2_normalised * rep(t(lm_R2$R2), ncol(IncrR2_normalised))
    return(R2_normalised)
  })

  varImpRF <- reactive({
    d = query(prices, c(input$var_outcome,
                        input$var_predictors))
    RF = randomForest(
      x = d[, input$var_predictors],
      y = d[, input$var_outcome],
      data = d,
      importance = TRUE)
    return(as.numeric(importance(RF)[, "%IncMSE"]))
  })
  
  riskDT <- reactive({

    fund = prices[, input$var_outcome]
    benchmark = prices[, input$metrics_benchmark]
    rf = prices[, 'yield_10Y']
    trimb = query(prices,c(input$var_outcome,input$metrics_benchmark,'yield_10Y'))
    trimb_fund = trimb[, input$var_outcome]
    trimb_benchmark = trimb[, input$metrics_benchmark]
    trimy = query(prices,c(input$var_outcome, 'yield_10Y'))
    trimy_fund = trimy[, input$var_outcome]

    out = data.frame(
      Measure = c("Single-Factor Model Alpha",
                  "Beta to Single-Factor benchmark",
                  "Beta to benchmark downside",
                  "Beta to benchmark upside",
                  "Sharpe Ratio (trailing 12M)^",
                  "Sortino Ratio (trailing 12M)^",
                  "Treynor Ratio (trailing 12M)^",
                  "Vol(UpsideRisk)/Vol(DownsideRisk)",
                  "Information Ratio to benchmark"
                  ),
      Value = c(formatRptRange(formatRpt100perc(pmBeta(fund, benchmark)$alpha)),
                formatRptRange(formatRptDouble(pmBeta(fund, benchmark)$beta)),
                formatRptDouble(pmBetaBullBear(fund, benchmark, bear = TRUE)),
                formatRptDouble(pmBetaBullBear(fund, benchmark, bear = FALSE)),
                formatRptDouble(pmSharpeRatio(trimy_fund, rf, lookback = 12, downside = FALSE)),
                formatRptDouble(pmSharpeRatio(trimy_fund, rf, lookback = 12, downside = TRUE)),
                formatRpt100perc(pmTreynorRatio(trimb_fund, rf, trimb_benchmark, lookback = 12)),
                formatRptDouble(pmUpsideDownsideRisk(fund)),
                formatRptDouble(pmInformationRatio(fund, benchmark))
                )
               )
    return(out)
  })
  
  # Highchart -------------------------------------------------------
  output$regressionFitActualPlot <- renderHighchart({
    hc <- highchart() %>%
      hc_add_series(
        data = cumsum(as.numeric(unlist(
          regForecast()$actual
        ))) * 100,
        type = "area",
        name = "Actual Cumulative Returns (%)",
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        data = cumsum(as.numeric(unlist(
          regForecast()$fit
        ))) * 100,
        type = "area",
        name = "Fit Cumulative Returns (%)",
        showInLegend = TRUE
      ) %>%
      hc_yAxis(title = list(text = "Actual vs Fit"),
               allowDecimals = FALSE) %>%
      hc_xAxis(
        categories = format(as.Date(
          rownames(regForecast()$actual),
          format = "%Y-%m-%d"
        ), "%b-%y"),
        title = list(text = "<b>Date</b>"),
        type = "datetime",
        labels = list(rotation = 315),
        tickmarkPlacement = "on"
      ) %>%
      hc_title(
        text = paste(
          "Actual vs Model Cumulative Performance:",
          paste(
            input$var_outcome,
            "~",
            paste(input$var_predictors, collapse = " + ")
          ),
          ", Rolling window =",
          input$reg_lookback
        ),
        style = list(fontWeight = "bold")
      ) %>%
      hc_subtitle(text = paste("Date Range:",
                               min(rownames(
                                 regForecast()$actual
                               )),
                               "-",
                               max(rownames(
                                 regForecast()$actual
                               )))) %>%
      hc_tooltip(valueDecimals = 2,
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
    
    hc
  })
  
  output$regressionBetaPlot <- renderHighchart({
    hc <- highchart()
    df = regStatsBeta()
    for (p in colnames(df)[-1]) {
      hc = hc %>%  hc_add_series(
        data = df[, p],
        type = "line",
        name = p,
        showInLegend = TRUE
      )
    }
    
    hc = hc %>%
      hc_yAxis(title = list(text = "Sensitivity to 1% Delta"),
               allowDecimals = FALSE) %>%
      hc_xAxis(
        categories = format(as.Date(rownames(df),
                                    format = "%Y-%m-%d"), "%b-%y"),
        title = list(text = "<b>Date</b>"),
        type = "datetime",
        labels = list(rotation = 315),
        tickmarkPlacement = "on"
      ) %>%
      hc_tooltip(valueDecimals = 2,
                 pointFormat = "B={point.y}") %>%
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
    
    hc
  })
  
  output$regressionAttributionPlot <- renderHighchart({
    
    r2attr = regR2Attribution()*100

    hc <- highchart()
    for(p in colnames(r2attr)) {
      hc = hc %>% hc_add_series(
        data = as.numeric(r2attr[,p]),
        name = p,
        showInLegend = TRUE,
        stack = "Attribution")
    }
    
    hc = hc %>% 
      hc_chart(type = "area") %>%
      hc_plotOptions(area = list(stacking = "normal"))
    
    
    hc = hc %>%
      hc_yAxis(title = list(text = "R-Squared Contribution (%)"),
               allowDecimals = FALSE, max=100) %>%
      hc_xAxis(
        categories = format(as.Date(rownames(r2attr), format = "%Y-%m-%d"), "%b-%y"),
        title = list(text = "<b>Date</b>"),
        type = "datetime",
        labels = list(rotation = 315),
        tickmarkPlacement = "on"
      ) %>%
      hc_title(
        text = paste(
          "Returns Attribution:",
          paste(
            input$var_outcome,
            "~",
            paste(input$var_predictors, collapse = " + ")
          ),
          ", Rolling window =",
          input$reg_lookback
        ),
        style = list(fontWeight = "bold")
      ) %>%
      hc_subtitle(text = paste("Date Range:",
                               min(rownames(r2attr)),
                               "-",
                               max(rownames(r2attr))
                               )
                  ) %>%
      hc_tooltip(valueDecimals = 2,
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
    
    hc
  })
  
  output$varImpRFPlot <- renderHighchart({
    hc <- highchart() %>%
      hc_add_series(
        data = varImpRF(),
        type = "bar",
        name = "Variable Importance (%IncMSE)",
        showInLegend = FALSE
      ) %>%
      hc_xAxis(
        categories = input$var_predictors,
        labels = list(rotation = 0),
        tickmarkPlacement = "on"
      ) %>%
      hc_yAxis(title = list(text = "%IncMSE"),
               allowDecimals = FALSE) %>%
      hc_title(
        text = "Variable Importance (%IncMSE)",
        style = list(fontWeight = "bold")
      ) %>%
      hc_tooltip(valueDecimals = 4,
                 pointFormat = "{point.y}%")

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
    
    hc
  })
  
  output$riskMetricTbl <- renderTable(riskDT(),
                caption='^: Where data is most recently available.')
  
}


# Run app -----------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)


# APPENDIX -------------------------------------------------------------

# https://www.stat.cmu.edu/summer/cmsacamp/Week_04_Tuesday/RF_Var_Imp.Rmd
# If we ignore the `type` argument of the `importance()` function:
# - Regression: if you do not specify `importance=TRUE`, only `IncNodePurity` is output. Otherwise `%IncMSE` is output in column 1, and `IncNodePurity` is output in column 2.
# - Classification: if you do not specify `importance=TRUE`, only `MeanDecreaseGini` is output. Otherwise $p+2$ columns are output; `MeanDecreaseAccuracy` is output in column $p+1$ and `MeanDecreaseGini` is output in column $p+2$.
# %IncMSE: Percentage Increase in MSE (Regression)
# ===
#
#   Algorithm:
#
#   1. Grow forest. Using OOB data, compute the MSE. Call this MSE.min.
# 2. For each predictor variable in turn, randomly permute the data values, then repeat Step 1. Call this MSE.ii.
# 3. %IncMSE for the ii^th variable is 100*(MSE.ii-MSE.min)/(MSE.min).
#
# - Magnitude does not depend on sample size.
#
# - This is analogous to `MeanDecreaseAccuracy`.
#
# IncNodePurity: Increase in Node Purity
# ===
#
#   - How much does a split reduce the RSS? The output value represents the sum over all splits for that variable, averaged over all trees. That value will be larger or smaller depending on whether the dataset has a larger or smaller sample size.
#
# - This is analogous to `MeanDecreaseGini`.
#
# MeanDecreaseGini
# ===
#
#   - How much does a split reduce the Gini coefficient? The output value represents the (weighted?) sum over all splits for that variable, averaged over all trees. That value will be larger or smaller depending on whether the dataset has a larger or smaller sample size.
#
# - This is analogous to `IncNodePurity`.
#
# MeanDecreaseAccuracy
# ===
#
#   - How many more OOB observations are misclassified if we randomly permute the data in the named data frame column, as opposed to not permuting the data? (This is analogous to `%IncMSE`.)
#
# - The magnitude will depend on sample size.
