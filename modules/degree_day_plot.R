############################################################
# Degree Day Plot Module
# File: modules/degree_day_plot.R
#
# Purpose:
#   Plot cumulative degree days from planting date onward,
#   showing observed values as a solid line and forecast
#   values, provisional gap-filled values, and forecast values,
#   with horizontal risk thresholds,
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
#   - "Provisional"
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

degreeDayPlotServer <- function(id,
                                dd_data,
                                risk1,
                                risk2,
                                risk3,
                                plot_title = reactive("Cumulative Degree Days from Planting")) {
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
      
      if (!("provider" %in% names(df))) {
        df$provider <- "Weather API"
      }

      df <- df %>%
        mutate(source_name = format_weather_source(provider, source)) %>%
        arrange(date)
      
      planting_date <- min(df$date, na.rm = TRUE)
      
      x_start <- planting_date - 10
      x_end   <- planting_date + 110
      
      df <- df %>%
        filter(date >= x_start & date <= x_end)
      
      obs <- df %>%
        filter(source == "Observed") %>%
        arrange(date)

      provisional <- df %>%
        filter(source == "Provisional") %>%
        arrange(date)
      
      fc <- df %>%
        filter(source == "Forecast") %>%
        arrange(date)
      

      
      x_min <- x_start
      x_max <- x_end
      
      y_max <- max(c(df$cum_dd, risk3()), na.rm = TRUE)
      y_top <- y_max * 1.05
      
      risk_labels <- c(
        "J2 emergence", "infection window", "reproduction threshold"
      )

      source_colors <- c(
        Observed = "#2563eb",
        Provisional = "#0f766e",
        Forecast = "#64748b"
      )

      threshold_colors <- c("#d97706", "#ea580c", "#dc2626")

      spread_label_positions <- function(values, lower, upper, min_gap) {
        if (length(values) <= 1) {
          return(values)
        }

        ord <- order(values)
        adjusted <- values[ord]

        for (i in seq_along(adjusted)[-1]) {
          if ((adjusted[[i]] - adjusted[[i - 1]]) < min_gap) {
            adjusted[[i]] <- adjusted[[i - 1]] + min_gap
          }
        }

        overflow <- max(adjusted, na.rm = TRUE) - upper
        if (overflow > 0) {
          adjusted <- adjusted - overflow
        }

        for (i in rev(seq_along(adjusted)[-length(adjusted)])) {
          if ((adjusted[[i + 1]] - adjusted[[i]]) < min_gap) {
            adjusted[[i]] <- adjusted[[i + 1]] - min_gap
          }
        }

        adjusted <- pmin(pmax(adjusted, lower), upper)
        out <- values
        out[ord] <- adjusted
        out
      }

      latest_current <- df %>%
        filter(date <= Sys.Date()) %>%
        filter(!is.na(cum_dd)) %>%
        arrange(date) %>%
        slice_tail(n = 1)
      
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
        
        status_short <- switch(
          hit$source[[1]],
          Observed = "obs",
          Provisional = "prov",
          Forecast = "pred",
          "pred"
        )
        
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
            name = obs$source_name[[1]],
            line = list(width = 4, color = source_colors[["Observed"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cumulative DD: %{y:.1f}<br>",
              "Source: ", obs$source_name[[1]],
              "<extra></extra>"
            )
          )
      }

      ######################################################
      # Provisional cumulative DD
      ######################################################
      if (nrow(provisional) > 0) {
        p <- p %>%
          add_lines(
            data = provisional,
            x = ~date,
            y = ~cum_dd,
            name = provisional$source_name[[1]],
            line = list(width = 4, dash = "dot", color = source_colors[["Provisional"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cumulative DD: %{y:.1f}<br>",
              "Source: ", provisional$source_name[[1]],
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
            name = fc$source_name[[1]],
            line = list(width = 4, dash = "dash", color = source_colors[["Forecast"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cumulative DD: %{y:.1f}<br>",
              "Source: ", fc$source_name[[1]],
              "<extra></extra>"
            )
          )
      }

      if (nrow(latest_current) > 0) {
        current_source_name <- latest_current$source_name[[1]]
        p <- p %>%
          add_markers(
            data = latest_current,
            x = ~date,
            y = ~cum_dd,
            name = "Current value",
            showlegend = FALSE,
            marker = list(
              size = 11,
              color = "white",
              line = list(color = "#111827", width = 3)
            ),
            hovertemplate = paste(
              "Current DD: %{y:.1f}<br>",
              "Date: %{x}<br>",
              "Source: ", current_source_name,
              "<extra></extra>"
            )
          ) %>%
          add_annotations(
            x = 0.98,
            y = 0.07,
            xref = "paper",
            yref = "paper",
            text = paste0(
              "Current: ",
              round(latest_current$cum_dd[[1]], 1),
              " DD<br>",
              format(latest_current$date[[1]], "%Y-%m-%d"),
              "<br>",
              current_source_name
            ),
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "bottom",
            align = "right",
            bgcolor = "rgba(255,255,255,0.92)",
            bordercolor = "#111827",
            borderwidth = 1
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
          showlegend = FALSE,
          line = list(dash = "dot", color = threshold_colors[[1]], width = 2),
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
          showlegend = FALSE,
          line = list(dash = "dot", color = threshold_colors[[2]], width = 2),
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
          showlegend = FALSE,
          line = list(dash = "dot", color = threshold_colors[[3]], width = 2),
          hovertemplate = paste(
            risk_labels[3],": ", round(risk3(), 1), " DD",
            "<extra></extra>"
          )
        )

      threshold_values <- c(risk1(), risk2(), risk3())
      threshold_label_y <- spread_label_positions(
        threshold_values,
        lower = y_top * 0.08,
        upper = y_top * 0.96,
        min_gap = y_top * 0.075
      )
      for (i in seq_along(threshold_values)) {
        p <- p %>%
          add_annotations(
            x = x_max,
            y = threshold_label_y[[i]],
            text = paste0(risk_labels[[i]], "<br>", round(threshold_values[[i]], 0), " DD"),
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "middle",
            xshift = 8,
            font = list(color = threshold_colors[[i]], size = 11),
            bgcolor = "rgba(255,255,255,0.85)"
          )
      }
      
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

      if (x_min <= Sys.Date() && Sys.Date() <= x_max) {
        p <- p %>%
          add_segments(
            x = Sys.Date(),
            xend = Sys.Date(),
            y = 0,
            yend = y_top,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(dash = "dash", width = 1.5, color = "#111827")
          ) %>%
          add_annotations(
            x = Sys.Date(),
            y = y_top,
            text = "Today",
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 5
          )
      }
      
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
            line = list(width = 2, color = threshold_colors[[1]])
          ) %>%
          add_annotations(
            x = risk1_hit$x,
            y = y_top * 0.60,
            text = risk1_hit$label,
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 6
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
            line = list(width = 2, color = threshold_colors[[2]])
          ) %>%
          add_annotations(
            x = risk2_hit$x,
            y = y_top * 0.7,
            text = risk2_hit$label,
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 6
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
            line = list(width = 2, color = threshold_colors[[3]])
          ) %>%
          add_annotations(
            x = risk3_hit$x,
            y = y_top * 0.8,
            text = risk3_hit$label,
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 6
          )
      }
      
      ######################################################
      # Final layout
      ######################################################
      p %>%
        layout(
          title = plot_title(),
          xaxis = list(
            title = "Date",
            range = c(x_start, x_end),
            showgrid = TRUE,
            gridcolor = "#e5e7eb"
          ),
          yaxis = list(
            title = "Cumulative Degree Days",
            range = c(0, y_top),
            showgrid = TRUE,
            gridcolor = "#e5e7eb",
            zeroline = FALSE
          ),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.16),
          margin = list(t = 105, r = 155, b = 50, l = 65),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          shapes = list(
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = 0, y1 = risk1(),
              fillcolor = "rgba(34, 197, 94, 0.08)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk1(), y1 = risk2(),
              fillcolor = "rgba(245, 158, 11, 0.10)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk2(), y1 = risk3(),
              fillcolor = "rgba(249, 115, 22, 0.10)",
              line = list(width = 0),
              layer = "below"
            ),
            list(
              type = "rect",
              x0 = x_min, x1 = x_max,
              y0 = risk3(), y1 = y_top,
              fillcolor = "rgba(220, 38, 38, 0.10)",
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
