############################################################
# Temperature Plot Module
# File: modules/temperature_plot.R
#
# Purpose:
#   Plot daily mean temperature from planting date onward,
#   showing observed, provisional, and forecast values plus
#   the base-temperature line used for degree-day accumulation.
############################################################

library(shiny)
library(plotly)
library(dplyr)

temperaturePlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    plotlyOutput(ns("temp_plot"), height = "260px")
  )
}

temperaturePlotServer <- function(id,
                                  dd_data,
                                  base_temp,
                                  plot_title = reactive("Daily Mean Temperature")) {
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

      if (!("provider" %in% names(df))) {
        df$provider <- "Weather API"
      }

      df <- df %>%
        mutate(source_name = format_weather_source(provider, source)) %>%
        arrange(date)

      planting_date <- min(df$date, na.rm = TRUE)
      x_start <- planting_date - 10
      x_end <- planting_date + 110

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

      source_colors <- c(
        Observed = "#2563eb",
        Provisional = "#0f766e",
        Forecast = "#64748b"
      )

      latest_current <- df %>%
        filter(date <= Sys.Date()) %>%
        filter(!is.na(mean_temp_c)) %>%
        arrange(date) %>%
        slice_tail(n = 1)

      y_min <- min(c(df$mean_temp_c, base_temp()), na.rm = TRUE)
      y_max <- max(c(df$mean_temp_c, base_temp()), na.rm = TRUE)
      y_pad_bottom <- max(1, 0.05 * (y_max - y_min))
      y_pad_top <- max(1, 0.10 * (y_max - y_min))
      y_lower <- y_min - y_pad_bottom
      y_upper <- y_max + y_pad_top

      p <- plot_ly()

      if (nrow(obs) > 0) {
        p <- p %>%
          add_lines(
            data = obs,
            x = ~date,
            y = ~mean_temp_c,
            name = obs$source_name[[1]],
            line = list(width = 3, color = source_colors[["Observed"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Mean temp: %{y:.2f} deg C<br>",
              "Source: ", obs$source_name[[1]],
              "<extra></extra>"
            )
          )
      }

      if (nrow(provisional) > 0) {
        p <- p %>%
          add_lines(
            data = provisional,
            x = ~date,
            y = ~mean_temp_c,
            name = provisional$source_name[[1]],
            line = list(width = 3, dash = "dot", color = source_colors[["Provisional"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Mean temp: %{y:.2f} deg C<br>",
              "Source: ", provisional$source_name[[1]],
              "<extra></extra>"
            )
          )
      }

      if (nrow(fc) > 0) {
        p <- p %>%
          add_lines(
            data = fc,
            x = ~date,
            y = ~mean_temp_c,
            name = fc$source_name[[1]],
            line = list(width = 3, dash = "dash", color = source_colors[["Forecast"]]),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Mean temp: %{y:.2f} deg C<br>",
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
            y = ~mean_temp_c,
            name = "Current temperature",
            showlegend = FALSE,
            marker = list(
              size = 9,
              color = "white",
              line = list(color = "#111827", width = 2)
            ),
            hovertemplate = paste(
              "Current mean temp: %{y:.2f} deg C<br>",
              "Date: %{x}<br>",
              "Source: ", current_source_name,
              "<extra></extra>"
            )
          )
      }

      p <- p %>%
        add_segments(
          x = x_start,
          xend = x_end,
          y = base_temp(),
          yend = base_temp(),
          inherit = FALSE,
          name = "Base temp",
          showlegend = FALSE,
          line = list(dash = "dot", color = "#9333ea", width = 2),
          hovertemplate = paste(
            "Base temperature: ", round(base_temp(), 2), " deg C",
            "<extra></extra>"
          )
        ) %>%
        add_annotations(
          x = x_end,
          y = base_temp(),
          text = paste0("Base temp<br>", round(base_temp(), 1), " deg C"),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "middle",
          xshift = 8,
          font = list(color = "#9333ea", size = 11),
          bgcolor = "rgba(255,255,255,0.85)"
        ) %>%
        add_annotations(
          x = planting_date,
          y = y_upper,
          text = paste0("Planting<br>", format(planting_date, "%Y-%m-%d")),
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top"
        )

      if (x_start <= Sys.Date() && Sys.Date() <= x_end) {
        p <- p %>%
          add_segments(
            x = Sys.Date(),
            xend = Sys.Date(),
            y = y_lower,
            yend = y_upper,
            inherit = FALSE,
            showlegend = FALSE,
            line = list(dash = "dash", width = 1.5, color = "#111827")
          ) %>%
          add_annotations(
            x = Sys.Date(),
            y = y_upper,
            text = "Today",
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 5
          )
      }

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
            title = "Temperature (deg C)",
            range = c(y_lower, y_upper),
            showgrid = TRUE,
            gridcolor = "#e5e7eb",
            zeroline = FALSE
          ),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.16),
          margin = list(t = 80, r = 130, b = 50, l = 65),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
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
