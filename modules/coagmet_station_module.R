############################################################
# CoAgMET Station Module
# File: modules/coagmet_station_module.R
############################################################

library(shiny)
library(leaflet)
library(dplyr)

coagmetStationUI <- function(id, title = "CoAgMET Station", selected = "ftc01") {
  ns <- NS(id)

  tagList(
    h4(title),
    selectInput(
      ns("station_id"),
      "Station",
      choices = coagmet_station_choices(active_only = TRUE),
      selected = selected
    ),
    leafletOutput(ns("station_map"), height = "320px"),
    tags$p(
      class = "muted-note",
      "Choose from the dropdown or click a station marker."
    ),
    tags$details(
      tags$summary("Selected station details"),
      tags$pre(
        class = "compact-pre",
        textOutput(ns("station_info"), container = span)
      )
    )
  )
}

coagmetStationServer <- function(id, initial_station = "ftc01") {
  moduleServer(id, function(input, output, session) {
    stations <- read_coagmet_stations(active_only = TRUE)

    selected_station_id <- reactiveVal(initial_station)

    observeEvent(input$station_id, {
      selected_station_id(input$station_id)
    }, ignoreInit = FALSE)

    observeEvent(input$station_map_marker_click, {
      click <- input$station_map_marker_click
      if (!is.null(click$id) && click$id %in% stations$site_id) {
        selected_station_id(click$id)
        updateSelectInput(session, "station_id", selected = click$id)
      }
    })

    selected_station <- reactive({
      station_id <- selected_station_id()
      station <- stations %>% filter(site_id == station_id) %>% slice_head(n = 1)

      if (nrow(station) == 0) {
        station <- stations %>% slice_head(n = 1)
      }

      station
    })

    output$station_map <- renderLeaflet({
      station <- selected_station()

      leaflet(stations) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~site_id,
          popup = ~paste0(name, " (", site_id, ")"),
          radius = 5,
          stroke = TRUE,
          fillOpacity = 0.8
        ) %>%
        setView(lng = station$longitude[[1]], lat = station$latitude[[1]], zoom = 7)
    })

    observe({
      station <- selected_station()

      leafletProxy("station_map", session = session) %>%
        clearGroup("selected_station") %>%
        addCircleMarkers(
          lng = station$longitude[[1]],
          lat = station$latitude[[1]],
          group = "selected_station",
          radius = 9,
          color = "#1f78b4",
          fillColor = "#1f78b4",
          fillOpacity = 1,
          popup = paste0("Selected: ", station$name[[1]], " (", station$site_id[[1]], ")")
        )
    })

    output$station_info <- renderText({
      station <- selected_station()

      paste0(
        "Station: ", station$name[[1]], " (", station$site_id[[1]], ")\n",
        "Lat: ", round(station$latitude[[1]], 5), "\n",
        "Lon: ", round(station$longitude[[1]], 5), "\n",
        "Elevation: ", round(station$elevation_m[[1]], 0), " m\n",
        "Record start: ", station$start_date[[1]]
      )
    })

    selected_station
  })
}
