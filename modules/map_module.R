############################################################
# Map Module
# File: modules/map_module.R
#
# Purpose:
#   Provide an interactive map for selecting a location by:
#     - searching address text
#     - pasting lat,lon
#     - clicking the map
#     - drawing a marker, polygon, or rectangle
#
# Returns:
#   A reactive list with:
#     - lat
#     - lon
#     - label
#     - geom_type
############################################################

library(shiny)
library(leaflet)
library(leaflet.extras)

############################################################
# UI
############################################################

mapModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      style = "max-width: 900px; margin: auto; line-height: 1.6;",
      
      h3("How to Use This Tool"),
      
      tags$ol(
        tags$li("Select a location on the map using search, click, or polygon drawing."),
        tags$li("Choose the planting date for the field."),
        tags$li("Adjust the base temperature or risk thresholds if needed."),
        tags$li("View predicted infection timing and risk progression on the Risk Plots tab.")
      ),
      
      hr()
    ),
    
    div(
      style = "display: flex; gap: 10px; align-items: flex-end;",
      
      div(
        style = "flex: 1;",
        textInput(
          inputId = ns("location_query"),
          label = NULL,
          value = "Fort Collins, CO",
          placeholder = "Search address or paste lat,lon"
        )
      ),
      
      div(
        style = "width: 140px; margin-bottom: 8px;",
        actionButton(
          inputId = ns("search_btn"),
          label = "Find",
          width = "100%"
        )
      )
    ),
    
    tags$p(
      style = "color:#555; font-size:0.9em;",
      "You can also click the map to drop a pin or draw a polygon or rectangle. ",
      "Polygons and rectangles use the centroid."
    ),
    
    leafletOutput(ns("map"), height = "350px"),
    
    uiOutput(ns("selection_action_ui")),
    
    tags$div(
      style = "margin-top: 10px;",
      strong("Selected location")
    ),
    
    tags$pre(
      style = paste(
        "font-size: 12px;",
        "padding: 8px;",
        "margin-top: 6px;",
        "background: #f8f9fa;",
        "border: 1px solid #ddd;",
        "border-radius: 4px;"
      ),
      textOutput(ns("selected_coords"), container = span)
    )
  )
}

############################################################
# SERVER
############################################################

mapModuleServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    ########################################################
    # Reactive values to store current selected location
    ########################################################
    rv <- reactiveValues(
      lat = 40.5853,
      lon = -105.0844,
      label = "Fort Collins, CO",
      geom_type = "Point",
      has_user_selection = FALSE
    )
    
    ########################################################
    # Initial map render
    ########################################################
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = rv$lon, lat = rv$lat, zoom = 9) %>%
        addDrawToolbar(
          targetGroup = "drawn",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = drawMarkerOptions(repeatMode = FALSE),
          polygonOptions = drawPolygonOptions(showArea = TRUE),
          rectangleOptions = drawRectangleOptions(),
          editOptions = editToolbarOptions()
        ) %>%
        addLayersControl(
          overlayGroups = c("drawn", "selection"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMarkers(
          lng = rv$lon,
          lat = rv$lat,
          group = "selection",
          popup = rv$label
        )
    })
    
    ########################################################
    # Search button:
    # - first try parsing lat,lon
    # - otherwise geocode text query
    ########################################################
    observeEvent(input$search_btn, {
      query <- trimws(input$location_query)
      
      shiny::validate(
        shiny::need(nzchar(query), "Enter an address or lat,lon.")
      )
      
      latlon <- parse_latlon(query)
      loc <- NULL
      
      if (!is.null(latlon)) {
        loc <- list(
          lat = latlon$lat,
          lon = latlon$lon,
          label = query
        )
      } else {
        loc <- tryCatch(
          geocode_nominatim(query),
          error = function(e) NULL
        )
      }
      
      shiny::validate(
        shiny::need(!is.null(loc), "Location not found. Try a clearer address or lat,lon.")
      )
      
      rv$lat <- loc$lat
      rv$lon <- loc$lon
      rv$label <- if (!is.null(loc$label)) loc$label else query
      rv$geom_type <- "Point"
      rv$has_user_selection <- TRUE
      
      leafletProxy("map", session = session) %>%
        clearGroup("selection") %>%
        clearGroup("drawn") %>%
        setView(lng = rv$lon, lat = rv$lat, zoom = 11) %>%
        addMarkers(
          lng = rv$lon,
          lat = rv$lat,
          group = "selection",
          popup = rv$label
        )
    })
    
    ########################################################
    # Map click:
    # use clicked point as selected location
    ########################################################
    observeEvent(input$map_click, {
      rv$lat <- input$map_click$lat
      rv$lon <- input$map_click$lng
      rv$label <- "Map click"
      rv$geom_type <- "Point"
      rv$has_user_selection <- TRUE
      
      leafletProxy("map", session = session) %>%
        clearGroup("selection") %>%
        addMarkers(
          lng = rv$lon,
          lat = rv$lat,
          group = "selection",
          popup = "Selected point"
        )
    })
    
    ########################################################
    # Draw new feature:
    # - marker -> use exact point
    # - polygon/rectangle -> use centroid
    ########################################################
    observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      
      extracted <- tryCatch(
        extract_drawn_centroid(feat),
        error = function(e) NULL
      )
      
      if (is.null(extracted)) {
        return()
      }
      
      rv$lat <- extracted$lat
      rv$lon <- extracted$lon
      rv$geom_type <- extracted$type
      rv$label <- paste(extracted$type, "selection")
      rv$has_user_selection <- TRUE
      
      leafletProxy("map", session = session) %>%
        clearGroup("selection") %>%
        addCircleMarkers(
          lng = rv$lon,
          lat = rv$lat,
          radius = 7,
          stroke = TRUE,
          fillOpacity = 0.9,
          group = "selection",
          popup = paste0("Using ", extracted$type, " centroid")
        )
    })
    
    ########################################################
    # Go-to-plots button:
    # appears only after user has chosen a location
    ########################################################
    output$selection_action_ui <- renderUI({
      if (!isTRUE(rv$has_user_selection)) {
        return(NULL)
      }
      
      div(
        style = "margin-top: 10px; margin-bottom: 4px; text-align: right;",
        actionButton(
          inputId = session$ns("confirm_selection"),
          label = "Go to Risk Plots",
          class = "btn-primary"
        )
      )
    })
    
    ########################################################
    # Switch to risk plots tab
    ########################################################
    observeEvent(input$confirm_selection, {
      updateTabsetPanel(
        session = parent_session,
        inputId = "main_tabs",
        selected = "Risk Plots"
      )
    })
    
    ########################################################
    # Show selected coordinates
    ########################################################
    output$selected_coords <- renderText({
      paste0(
        "Type: ", rv$geom_type, "\n",
        "Lat: ", round(rv$lat, 6), "\n",
        "Lon: ", round(rv$lon, 6), "\n",
        "Label: ", rv$label
      )
    })
    
    ########################################################
    # Return selected location as a reactive
    ########################################################
    selected_location <- reactive({
      list(
        lat = rv$lat,
        lon = rv$lon,
        label = rv$label,
        geom_type = rv$geom_type
      )
    })
    
    selected_location
  })
}