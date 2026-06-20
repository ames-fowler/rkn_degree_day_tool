############################################################
# Root-Knot Nematode Page
# File: modules/root_knot_page.R
############################################################

rootKnotPageUI <- function() {
  tagList(
    div(
      style = "max-width: 1180px; margin: auto; line-height: 1.5;",

      h2(class = "page-title", "Root-Knot Nematode"),
      p(class = "muted-note", "Soil-temperature degree-day risk for Meloidogyne spp."),
      div(
        class = "model-chip-row",
        span(class = "model-chip", "Soil temperature"),
        span(class = "model-chip", "CoAgMET + gridded weather"),
        span(class = "model-chip", "Observed / provisional / forecast")
      ),

      fluidRow(
        column(
          width = 5,
          div(
            class = "tool-section",
            h3("Location"),
            conditionalPanel(
              condition = "input.location_source == 'map'",
              mapModuleUI("map")
            ),
            conditionalPanel(
              condition = "input.location_source == 'coagmet'",
              coagmetStationUI(
                "rkn_station",
                title = "CoAgMET Station"
              )
            )
          )
        ),
        column(
          width = 7,
          div(
            class = "tool-section",
            h3("Risk"),
            riskTimingSummaryUI("rkn_timing"),
            br(),
            tabsetPanel(
              id = "rkn_plot_view",
              tabPanel(
                "Risk plot",
                value = "risk",
                degreeDayPlotUI("dd_plot")
              ),
              tabPanel(
                "Calendar temperature",
                value = "calendar_temperature",
                calendarTemperaturePlotUI("calendar_temp")
              )
            )
          )
        )
      ),

      tags$details(
        tags$summary("Temperature data"),
        p(
          class = "muted-note",
          "CoAgMET mode uses station observations, Open-Meteo provisional ",
          "gap-fill through today when station data lag, and Open-Meteo ",
          "forecast after today. Map mode uses gridded soil temperature for ",
          "the selected point or polygon centroid."
        ),
        temperaturePlotUI("temp_plot")
      ),

      tags$details(
        tags$summary("Model notes"),
        p(
          "The model accumulates degree days from the selected start date using ",
          "the base temperature and threshold values in Advanced model parameters."
        )
      )
    )
  )
}
