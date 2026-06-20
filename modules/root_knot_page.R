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
            degreeDayPlotUI("dd_plot")
          )
        )
      ),

      tags$details(
        tags$summary("Temperature data"),
        p(
          class = "muted-note",
          "CoAgMET mode uses station observations plus gridded projection data ",
          "at the station coordinates. Map mode uses gridded soil temperature ",
          "for the selected point or polygon centroid."
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
