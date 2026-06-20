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

      df <- dd_data() %>% arrange(date)
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

      p <- plot_ly()

      for (src in unique(df$source)) {
        src_df <- df %>% filter(source == src)
        p <- p %>%
          add_lines(
            data = src_df,
            x = ~date,
            y = ~cum_dd,
            name = paste(src, "cumulative DD"),
            line = list(width = 3, dash = if (src == "Forecast") "dash" else "solid"),
            hovertemplate = paste(
              "Date: %{x}<br>",
              "Cum DD: %{y:.1f}",
              "<extra></extra>"
            )
          )
      }

      for (i in seq_len(nrow(th))) {
        threshold <- th$threshold[[i]]
        label <- th$label[[i]]

        p <- p %>%
          add_segments(
            x = x_min,
            xend = x_max,
            y = threshold,
            yend = threshold,
            inherit = FALSE,
            name = label,
            line = list(dash = "dot"),
            hovertemplate = paste0(label, ": ", round(threshold, 1), " DD<extra></extra>")
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
              line = list(width = 2, color = "black")
            ) %>%
            add_annotations(
              x = hit$date[[1]],
              y = threshold,
              text = paste0(label, "<br>", format(hit$date[[1]], "%Y-%m-%d")),
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "bottom",
              xshift = -6
            )
        }
      }

      p %>%
        layout(
          title = plot_title(),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Cumulative Degree Days", range = c(0, y_top)),
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(t = 80, r = 20, b = 50, l = 60)
        )
    })
  })
}
