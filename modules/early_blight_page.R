############################################################
# Early Blight Page
# File: modules/early_blight_page.R
############################################################

library(shiny)

earlyBlightPageUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      style = "max-width: 1180px; margin: auto; line-height: 1.5;",

      h2(class = "page-title", "Early Blight"),
      p(class = "muted-note", "Air-temperature degree-day timing for initial fungicide decisions."),

      fluidRow(
        column(
          width = 5,
          div(
            class = "tool-section",
            h3("Location"),
            conditionalPanel(
              condition = "input.location_source == 'coagmet'",
              coagmetStationUI(
                ns("station"),
                title = "CoAgMET Station",
                selected = "ctr01"
              )
            ),
            conditionalPanel(
              condition = "input.location_source == 'map'",
              mapModuleUI(ns("early_map"))
            )
          )
        ),
        column(
          width = 7,
          div(
            class = "tool-section",
            h3("Risk"),
            riskTimingSummaryUI(ns("timing")),
            br(),
            thresholdPlotUI(ns("dd_plot"))
          )
        )
      ),

      tags$details(
        tags$summary("Data sources"),
        p(
          "CoAgMET mode uses station observations plus gridded projection data ",
          "at the station coordinates. Map mode uses gridded Open-Meteo air ",
          "temperature for the selected point or polygon centroid."
        )
      ),

      tags$details(
        tags$summary("Paper model and thresholds"),
        p(
          "Franc, Harrison, and Lahman (1988) used degree days accumulated ",
          "from planting above 7.2 deg C for Alternaria solani timing."
        ),
        tags$table(
          class = "table table-condensed",
          tags$thead(
            tags$tr(
              tags$th("Area"),
              tags$th("DDC threshold"),
              tags$th("Interpretation")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("San Luis Valley"),
              tags$td("361"),
              tags$td("First-lesion timing threshold.")
            ),
            tags$tr(
              tags$td("Northeastern plains"),
              tags$td("626"),
              tags$td("Higher empirical threshold for local timing.")
            )
          )
        ),
        tags$ul(
          tags$li("Daily DDC = max(0, ((daily max + daily min) / 2) - 7.2)."),
          tags$li("Spore detection often lagged first lesions by about 5 to 10 days."),
          tags$li("Thresholds may need local adjustment outside tested Colorado production areas.")
        )
      ),

      tags$details(
        tags$summary("Reference"),
        p(
          "Franc, G. D., Harrison, M. D., and Lahman, L. K. 1988. ",
          em("A Simple Day-Degree Model for Initiating Chemical Control of Potato Early Blight in Colorado. "),
          "Plant Disease 72:851-854. DOI: ",
          tags$a(
            href = "https://doi.org/10.1094/PD-72-0851",
            target = "_blank",
            "10.1094/PD-72-0851"
          ),
          "."
        )
      )
    )
  )
}

earlyBlightPageServer <- function(id,
                                  location_source,
                                  start_date,
                                  base_temp,
                                  threshold_ddc,
                                  production_area,
                                  refresh_trigger,
                                  map_state = NULL) {
  moduleServer(id, function(input, output, session) {

    station <- coagmetStationServer("station", initial_station = "ctr01")
    map_location <- mapModuleServer(
      "early_map",
      parent_session = session,
      target_tab = "early_blight",
      shared_state = map_state
    )

    thresholds <- reactive({
      build_threshold_table(
        labels = "Initial spray timing threshold",
        thresholds = threshold_ddc()
      )
    })

    dd_data <- eventReactive(refresh_trigger(), {
      req(start_date())
      req(base_temp())

      if (identical(location_source(), "coagmet")) {
        req(station())
        temps <- fetch_coagmet_with_projection(
          station_row = station(),
          planting_date = start_date(),
          measure = "air_minmax_mean",
          forecast_days = 14
        )
      } else {
        req(map_location())
        temps <- fetch_openmeteo_air_timeseries(
          lat = map_location()$lat,
          lon = map_location()$lon,
          planting_date = start_date(),
          forecast_days = 14
        )
      }

      build_degree_days(
        df_hourly = temps,
        planting_date = start_date(),
        base_temp = base_temp()
      )
    }, ignoreNULL = FALSE)

    riskTimingSummaryServer(
      "timing",
      dd_data = dd_data,
      thresholds = thresholds
    )

    thresholdPlotServer(
      "dd_plot",
      dd_data = dd_data,
      thresholds = thresholds,
      plot_title = reactive("Early Blight Cumulative Degree Days from Planting")
    )
  })
}
