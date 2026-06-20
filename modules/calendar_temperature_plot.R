############################################################
# Calendar Temperature Context Plot Module
# File: modules/calendar_temperature_plot.R
############################################################

library(shiny)
library(plotly)
library(dplyr)

calendarTemperaturePlotUI <- function(id, height = "420px") {
  ns <- NS(id)

  tagList(
    plotlyOutput(ns("calendar_temp_plot"), height = height)
  )
}

calendarTemperaturePlotServer <- function(id,
                                          current_daily,
                                          normals_daily,
                                          planting_date,
                                          plot_title,
                                          variable_label,
                                          active = reactive(TRUE)) {
  moduleServer(id, function(input, output, session) {
    output$calendar_temp_plot <- renderPlotly({
      req(active())
      req(current_daily())
      req(normals_daily())
      req(planting_date())

      current <- current_daily()
      normals <- normals_daily()
      plant_date <- as.Date(planting_date())

      shiny::validate(
        shiny::need(nrow(current) > 0, "No current-year temperature data available yet."),
        shiny::need(nrow(normals) > 0, "No long-term temperature reference available yet."),
        shiny::need(
          all(c("date", "mean_temp_c") %in% names(current)),
          "Current-year temperature data is missing required columns."
        ),
        shiny::need(
          all(c("date", "long_term_mean_c", "lo_1sd", "hi_1sd", "lo_2sd", "hi_2sd") %in% names(normals)),
          "Long-term temperature reference is missing required columns."
        )
      )

      plot_year <- as.integer(format(max(current$date, na.rm = TRUE), "%Y"))
      x_start <- as.Date(paste0(plot_year, "-01-01"))
      x_end <- as.Date(paste0(plot_year, "-12-31"))

      current <- current %>%
        filter(date >= x_start, date <= x_end) %>%
        arrange(date)

      normals <- normals %>%
        filter(date >= x_start, date <= x_end) %>%
        arrange(date)

      y_min <- min(c(current$mean_temp_c, normals$lo_2sd), na.rm = TRUE)
      y_max <- max(c(current$mean_temp_c, normals$hi_2sd), na.rm = TRUE)
      y_pad <- max(1, 0.06 * (y_max - y_min))

      p <- plot_ly() %>%
        add_ribbons(
          data = normals,
          x = ~date,
          ymin = ~lo_2sd,
          ymax = ~hi_2sd,
          name = "Long-term +/- 2 SD",
          fillcolor = "rgba(220, 38, 38, 0.08)",
          line = list(color = "rgba(220, 38, 38, 0)"),
          hoverinfo = "skip"
        ) %>%
        add_ribbons(
          data = normals,
          x = ~date,
          ymin = ~lo_1sd,
          ymax = ~hi_1sd,
          name = "Long-term +/- 1 SD",
          fillcolor = "rgba(220, 38, 38, 0.14)",
          line = list(color = "rgba(220, 38, 38, 0)"),
          hoverinfo = "skip"
        ) %>%
        add_lines(
          data = normals,
          x = ~date,
          y = ~long_term_mean_c,
          name = "Long-term mean",
          line = list(color = "rgba(220, 38, 38, 0.55)", width = 2),
          hovertemplate = paste(
            "Date: %{x}<br>",
            "Long-term mean: %{y:.1f} deg C",
            "<extra></extra>"
          )
        ) %>%
        add_lines(
          data = current,
          x = ~date,
          y = ~mean_temp_c,
          name = "Current year",
          line = list(color = "#111827", width = 3),
          hovertemplate = paste(
            "Date: %{x}<br>",
            "Current year: %{y:.1f} deg C",
            "<extra></extra>"
          )
        )

      shapes <- list()
      if (!is.na(plant_date) && plant_date >= x_start && plant_date <= x_end) {
        shapes <- list(
          list(
            type = "line",
            x0 = plant_date,
            x1 = plant_date,
            y0 = y_min - y_pad,
            y1 = y_max + y_pad,
            line = list(dash = "dash", width = 2, color = "#111827")
          )
        )

        p <- p %>%
          add_annotations(
            x = plant_date,
            y = y_max + y_pad,
            text = paste0("Planting<br>", format(plant_date, "%Y-%m-%d")),
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "top",
            xshift = 5
          )
      }

      p %>%
        layout(
          title = paste(plot_title(), "-", variable_label()),
          xaxis = list(
            title = "Calendar date",
            range = c(x_start, x_end),
            showgrid = TRUE,
            gridcolor = "#e5e7eb"
          ),
          yaxis = list(
            title = "Daily mean temperature (deg C)",
            range = c(y_min - y_pad, y_max + y_pad),
            showgrid = TRUE,
            gridcolor = "#e5e7eb",
            zeroline = FALSE
          ),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.14),
          margin = list(t = 95, r = 35, b = 50, l = 65),
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          shapes = shapes
        )
    })
  })
}
