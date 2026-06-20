############################################################
# Generic Degree-Day Threshold Plot
# File: modules/threshold_plot.R
############################################################

library(shiny)
library(plotly)
library(dplyr)

thresholdPlotUI <- function(id, height = "380px") {
  ns <- NS(id)
  plotlyOutput(ns("threshold_plot"), height = height)
}

thresholdPlotServer <- function(id, dd_data, thresholds, plot_title) {
  moduleServer(id, function(input, output, session) {
    output$threshold_plot <- renderPlotly({
      req(dd_data())
      req(thresholds())

      df <- dd_data()
      if (!("provider" %in% names(df))) {
        df$provider <- "Weather API"
      }

      df <- df %>%
        mutate(source_name = format_weather_source(provider, source)) %>%
        arrange(date)
      th <- thresholds()

      shiny::validate(
        shiny::need(nrow(df) > 0, "No degree-day data available yet."),
        shiny::need(all(c("date", "cum_dd", "source") %in% names(df)), "Degree-day data is missing required columns."),
        shiny::need(nrow(th) > 0, "No thresholds are configured.")
      )

      x_min <- min(df$date, na.rm = TRUE)
      x_max <- max(df$date, na.rm = TRUE)
      y_top <- max(c(df$cum_dd, th$threshold), na.rm = TRUE) * 1.08
      y_top <- max(y_top, 10)

      source_colors <- c(
        Observed = "#2563eb",
        Provisional = "#0f766e",
        Forecast = "#64748b"
      )
      threshold_colors <- c("#d97706", "#ea580c", "#dc2626", "#7c3aed")

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

      p <- plot_ly()

      for (src in unique(df$source)) {
        src_df <- df %>% filter(source == src)
        source_color <- if (src %in% names(source_colors)) {
          source_colors[[src]]
        } else {
          "#2563eb"
        }

        p <- p %>%
          add_lines(
            data = src_df,
            x = ~date,
            y = ~cum_dd,
            name = src_df$source_name[[1]],
            line = list(
              width = 3,
              dash = if (src == "Forecast") {
                "dash"
              } else if (src == "Provisional") {
                "dot"
              } else {
                "solid"
              },
              color = source_color
            ),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cumulative DD: %{y:.1f}<br>",
              "Source: ", src_df$source_name[[1]],
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

      threshold_label_y <- spread_label_positions(
        th$threshold,
        lower = y_top * 0.08,
        upper = y_top * 0.96,
        min_gap = y_top * 0.075
      )

      for (i in seq_len(nrow(th))) {
        threshold <- th$threshold[[i]]
        label <- th$label[[i]]
        threshold_color <- threshold_colors[[((i - 1) %% length(threshold_colors)) + 1]]

        p <- p %>%
          add_segments(
            x = x_min,
            xend = x_max,
            y = threshold,
            yend = threshold,
            inherit = FALSE,
            name = label,
            showlegend = FALSE,
            line = list(dash = "dot", color = threshold_color, width = 2),
            hovertemplate = paste0(label, ": ", round(threshold, 1), " DD<extra></extra>")
          ) %>%
          add_annotations(
            x = x_max,
            y = threshold_label_y[[i]],
            text = paste0(label, "<br>", round(threshold, 0), " DD"),
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "middle",
            xshift = 8,
            font = list(color = threshold_color, size = 11),
            bgcolor = "rgba(255,255,255,0.85)"
          )

        hit <- df %>% filter(cum_dd >= threshold) %>% slice_head(n = 1)
        if (nrow(hit) > 0) {
          p <- p %>%
            add_segments(
              x = hit$date[[1]],
              xend = hit$date[[1]],
              y = 0,
              yend = threshold,
              inherit = FALSE,
              showlegend = FALSE,
              line = list(width = 2, color = threshold_color)
            ) %>%
            add_annotations(
              x = hit$date[[1]],
              y = threshold,
              text = paste0(label, "<br>", format(hit$date[[1]], "%Y-%m-%d")),
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "bottom",
              xshift = 6
            )
        }
      }

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

      p %>%
        layout(
          title = plot_title(),
          xaxis = list(
            title = "Date",
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
          margin = list(t = 100, r = 155, b = 50, l = 65),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff"
        )
    })
  })
}
