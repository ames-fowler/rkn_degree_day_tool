############################################################
# Temperature Plot Module
# File: modules/temperature_plot.R
#
# Purpose:
#   Plot daily mean soil temperature from planting date onward,
#   showing observed values as a solid line and forecast values
#   as a dashed line, plus a horizontal base-temperature line
#   and a vertical planting-date marker.
#
# Expected input data columns in dd_data():
#   - date
#   - mean_temp_c
#   - source
#
# Expected source values:
#   - "Observed"
#   - "Forecast"
############################################################

library(shiny)
library(plotly)
library(dplyr)

############################################################
# UI
############################################################

temperaturePlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("temp_plot"), height = "260px")
  )
}

############################################################
# SERVER
############################################################

temperaturePlotServer <- function(id, dd_data, base_temp) {
  moduleServer(id, function(input, output, session) {
    
    output$temp_plot <- renderPlotly({
      
      req(dd_data())
      req(base_temp())
      
      df <- dd_data()
      
      shiny::validate(
        shiny::need(nrow(df) > 0, "No temperature data available yet."),
        shiny::need(
          all(c("date", "mean_temp_c", "source") %in% names(df)),
          "Temperature plot is missing required columns."
        )
      )
      
      df <- df %>% arrange(date)
      
      obs <- df %>%
        filter(source == "Observed") %>%
        arrange(date)
      
      fc <- df %>%
        filter(source == "Forecast") %>%
        arrange(date)
      
      planting_date <- min(df$date, na.rm = TRUE)
      
      x_start <- planting_date - 10
      x_end   <- planting_date + 110
      
      y_min <- min(c(df$mean_temp_c, base_temp()), na.rm = TRUE)
      y_max <- max(c(df$mean_temp_c, base_temp()), na.rm = TRUE)
      
      # Add a little padding so the planting label has space
      y_pad_bottom <- max(1, 0.05 * (y_max - y_min))
      y_pad_top    <- max(1, 0.10 * (y_max - y_min))
      
      y_lower <- y_min - y_pad_bottom
      y_upper <- y_max + y_pad_top
      
      p <- plot_ly()
      
      ######################################################
      # Observed temperature line
      ######################################################
      if (nrow(obs) > 0) {
        p <- p %>%
          add_lines(
            data = obs,
            x = ~date,
            y = ~mean_temp_c,
            name = "Observed mean soil temp",
            line = list(width = 2),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Temp: %{y:.2f} °C",
              "<extra></extra>"
            )
          )
      }
      
      ######################################################
      # Forecast temperature line
      ######################################################
      if (nrow(fc) > 0) {
        p <- p %>%
          add_lines(
            data = fc,
            x = ~date,
            y = ~mean_temp_c,
            name = "Forecast mean soil temp",
            line = list(width = 2, dash = "dash"),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Temp: %{y:.2f} °C",
              "<extra></extra>"
            )
          )
      }
      
      ######################################################
      # Base temperature reference line
      ######################################################
      p <- p %>%
        add_segments(
          x = x_start,
          xend = x_end,
          y = base_temp(),
          yend = base_temp(),
          inherit = FALSE,
          name = "Base temp",
          line = list(dash = "dot"),
          hovertemplate = paste(
            "Base temperature: ", round(base_temp(), 2), " °C",
            "<extra></extra>"
          )
        )
      
      ######################################################
      # Planting date label
      ######################################################
      p <- p %>%
        add_annotations(
          x = planting_date,
          y = y_upper,
          text = paste0("Planting<br>", format(planting_date, "%Y-%m-%d")),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top"
        )
      
      ######################################################
      # Final layout
      ######################################################
      p %>%
        layout(
          title = "Daily Mean Soil Temperature",
          xaxis = list(
            title = "Date",
            range = c(x_start, x_end)
          ),
          yaxis = list(
            title = "Temperature (°C)",
            range = c(y_lower, y_upper)
          ),
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(t = 60, r = 20, b = 50, l = 60),
          shapes = list(
            list(
              type = "line",
              x0 = planting_date,
              x1 = planting_date,
              y0 = y_lower,
              y1 = y_upper,
              line = list(dash = "dot", width = 2, color = "black")
            )
          )
        )
      
    })
  })
}