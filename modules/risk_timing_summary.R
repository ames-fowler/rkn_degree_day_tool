############################################################
# Risk Timing Summary Module
# File: modules/risk_timing_summary.R
############################################################

library(shiny)
library(dplyr)

riskTimingSummaryUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Risk Timing"),
    tableOutput(ns("timing_table"))
  )
}

riskTimingSummaryServer <- function(id, dd_data, thresholds) {
  moduleServer(id, function(input, output, session) {
    output$timing_table <- renderTable({
      req(dd_data())
      req(thresholds())

      summarize_threshold_timing(
        df_dd = dd_data(),
        thresholds = thresholds()
      ) %>%
        transmute(
          Event = label,
          `Threshold DD` = threshold,
          `Crossing date` = ifelse(is.na(crossing_date), "", as.character(crossing_date)),
          `Days to event` = status,
          `Current DD` = current_dd
        )
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  })
}
