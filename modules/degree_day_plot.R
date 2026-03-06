############################################################
# Degree Day Plot Module
# File: modules/degree_day_plot.R
#
# Purpose:
#   Plot cumulative degree days from planting date onward,
#   showing observed values as a solid line and forecast
#   values as a dashed line, with horizontal risk thresholds,
#   shaded risk zones, a planting-date marker, and threshold-
#   crossing annotations on either the observed or predicted
#   line.
#
# Expected input data columns in dd_data():
#   - date
#   - cum_dd
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

degreeDayPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("dd_plot"), height = "420px")
  )
}

############################################################
# SERVER
############################################################

degreeDayPlotServer <- function(id, dd_data, risk1, risk2, risk3) {
  moduleServer(id, function(input, output, session) {
    
    output$dd_plot <- renderPlotly({
      
      req(dd_data())
      req(risk1(), risk2(), risk3())
      
      df <- dd_data()
      
      shiny::validate(
        shiny::need(nrow(df) > 0, "No degree-day data available yet."),
        shiny::need(
          all(c("date", "cum_dd", "source") %in% names(df)),
          "Degree-day plot is missing required columns."
        ),
        shiny::need(
          risk1() < risk2() && risk2() < risk3(),
          "Risk thresholds must increase from zone 1 to zone 3."
        )
      )
      
      df <- df %>% arrange(date)
      
      planting_date <- min(df$date, na.rm = TRUE)
      
      x_start <- planting_date - 10
      x_end   <- planting_date + 110
      
      df <- df %>%
        filter(date >= x_start & date <= x_end)
      
      obs <- df %>%
        filter(source == "Observed") %>%
        arrange(date)
      
      fc <- df %>%
        filter(source == "Forecast") %>%
        arrange(date)
      

      
      x_min <- x_start
      x_max <- x_end
      
      y_max <- max(c(df$cum_dd, risk3()), na.rm = TRUE)
      y_top <- y_max * 1.05
      
      risk_labels <- c(
        # "Early Risk",
        # "High Risk",
        # "Severe Risk"
        "J2 emergence", "infection window", "reproduction threshold"
      )
      
      ######################################################
      # Threshold crossings on either observed or forecast
      ######################################################
      get_risk_crossing <- function(threshold, risk_label) {
        hit <- df %>%
          arrange(date) %>%
          filter(cum_dd >= threshold) %>%
          slice(1)
        
        if (nrow(hit) == 0) {
          return(NULL)
        }
        
        status_short <- if (hit$source[[1]] == "Observed") "obs" else "pred"
        
        list(
          x = hit$date[[1]],
          y = hit$cum_dd[[1]],
          source = hit$source[[1]],
          label = paste0(
            risk_label, " (", status_short, "): ",
            format(hit$date[[1]], "%Y-%m-%d")
          
          )
        )
      }
      
      risk1_hit <- get_risk_crossing(risk1(), risk_labels[1])
      risk2_hit <- get_risk_crossing(risk2(), risk_labels[2])
      risk3_hit <- get_risk_crossing(risk3(), risk_labels[3])
      
      p <- plot_ly()
      
      ######################################################
      # Observed cumulative DD
      ######################################################
      if (nrow(obs) > 0) {
        p <- p %>%
          add_lines(
            data = obs,
            x = ~date,
            y = ~cum_dd,
            name = "Observed cumulative DD",
            line = list(width = 3),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cum DD: %{y:.1f}",
              "<extra></extra>"
            )
          )
      }
      
      ######################################################
      # Forecast cumulative DD
      ######################################################
      if (nrow(fc) > 0) {
        p <- p %>%
          add_lines(
            data = fc,
            x = ~date,
            y = ~cum_dd,
            name = "Forecast cumulative DD",
            line = list(width = 3, dash = "dash"),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cum DD: %{y:.1f}",
              "<extra></extra>"
            )
          )
      }
      
      ######################################################
      # Risk threshold lines
      ######################################################
      p <- p %>%
        add_segments(
          x = x_min,
          xend = x_max,
          y = risk1(),
          yend = risk1(),
          inherit = FALSE,
          name = risk_labels[1],
          line = list(dash = "dot"),
          hovertemplate = paste(
            risk_labels[1],": ", round(risk1(), 1), " DD",
            "<extra></extra>"
          )
        ) %>%
        add_segments(
          x = x_min,
          xend = x_max,
          y = risk2(),
          yend = risk2(),
          inherit = FALSE,
          name = risk_labels[2],
          line = list(dash = "dot"),
          hovertemplate = paste(
            risk_labels[2],": ", round(risk2(), 1), " DD",
            "<extra></extra>"
          )
        ) %>%
        add_segments(
          x = x_min,
          xend = x_max,
          y = risk3(),
          yend = risk3(),
          inherit = FALSE,
          name = risk_labels[3],
          line = list(dash = "dot"),
          hovertemplate = paste(
            risk_labels[3],": ", round(risk3(), 1), " DD",
            "<extra></extra>"
          )
        )
      
      ######################################################
      # Planting date label
      ######################################################
      p <- p %>%
        add_annotations(
          x = planting_date,
          y = y_top * 0.75,
          text = paste0("Planting<br>", format(planting_date, "%Y-%m-%d")),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top"
        )
      
      ######################################################
      # Threshold crossing lines + labels
      ######################################################
      if (!is.null(risk1_hit)) {
        p <- p %>%
          add_segments(
            x = risk1_hit$x,
            xend = risk1_hit$x,
            y = 0,
            yend = y_top * 0.60,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(width = 2, color = "black")
          ) %>%
          add_annotations(
            x = risk1_hit$x,
            y = y_top * 0.60,
            text = risk1_hit$label,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            xshift = -6
          )
      }
      
      if (!is.null(risk2_hit)) {
        p <- p %>%
          add_segments(
            x = risk2_hit$x,
            xend = risk2_hit$x,
            y = 0,
            yend = y_top*.7,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(width = 2, color = "black")
          ) %>%
          add_annotations(
            x = risk2_hit$x,
            y = y_top * 0.7,
            text = risk2_hit$label,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            xshift = -6
          )
      }
      
      if (!is.null(risk3_hit)) {
        p <- p %>%
          add_segments(
            x = risk3_hit$x,
            xend = risk3_hit$x,
            y = 0,
            yend = y_top*0.8,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(width = 2, color = "black")
          ) %>%
          add_annotations(
            x = risk3_hit$x,
            y = y_top * 0.8,
            text = risk3_hit$label,
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            xshift = -6
          )
      }
      
      ######################################################
      # Final layout
      ######################################################
      p %>%
        layout(
          title = "Cumulative Degree Days from Planting",
          xaxis = list(
            title = "Date",
            range = c(x_start, x_end)
          ),
          yaxis = list(
            title = "Cumulative Degree Days",
            range = c(0, y_top)
          ),
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(t = 90, r = 20, b = 50, l = 60),
          shapes = list(
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk1(), y1 = risk2(),
              fillcolor = "rgba(255, 215, 0, 0.15)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk2(), y1 = risk3(),
              fillcolor = "rgba(255, 140, 0, 0.12)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk3(), y1 = y_top,
              fillcolor = "rgba(220, 20, 60, 0.10)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "line",
              x0 = planting_date,
              x1 = planting_date,
              y0 = 0,
              y1 = y_top,
              line = list(dash = "dot", width = 2, color = "black"),
              layer = "above"
            )
          )
        )
    })
  })
}