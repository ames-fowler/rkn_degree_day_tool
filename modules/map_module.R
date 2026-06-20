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
      class = "muted-note",
      "Search, click, or draw a polygon. Polygon and rectangle selections use the centroid."
    ),
    
    leafletOutput(ns("map"), height = "320px"),
    
    uiOutput(ns("selection_action_ui")),
    
    tags$details(
      tags$summary("Selected location details"),
      tags$pre(
        class = "compact-pre",
        textOutput(ns("selected_coords"), container = span)
      )
    )
  )
}

############################################################
# SERVER
############################################################

mapModuleServer <- function(id,
                            parent_session,
                            target_tab = "rkn",
                            shared_state = NULL) {
  moduleServer(id, function(input, output, session) {

    default_location <- list(
      lat = 40.5853,
      lon = -105.0844,
      label = "Fort Collins, CO",
      geom_type = "Point",
      has_user_selection = FALSE
    )

    ########################################################
    # Reactive values to store current selected location
    ########################################################
    rv <- reactiveValues(
      lat = default_location$lat,
      lon = default_location$lon,
      label = default_location$label,
      geom_type = default_location$geom_type,
      has_user_selection = default_location$has_user_selection,
      draw_feature = NULL,
      map_lat = default_location$lat,
      map_lon = default_location$lon,
      map_zoom = 9
    )

    set_location <- function(lat,
                             lon,
                             label,
                             geom_type,
                             has_user_selection = TRUE,
                             draw_feature = NULL) {
      rv$lat <- lat
      rv$lon <- lon
      rv$label <- label
      rv$geom_type <- geom_type
      rv$has_user_selection <- has_user_selection
      rv$draw_feature <- draw_feature

      if (!is.null(shared_state)) {
        shared_state$lat <- lat
        shared_state$lon <- lon
        shared_state$label <- label
        shared_state$geom_type <- geom_type
        shared_state$has_user_selection <- has_user_selection
        shared_state$draw_feature <- draw_feature
      }
    }

    selected_location_icon <- leaflet::makeAwesomeIcon(
      icon = "map-marker",
      library = "fa",
      markerColor = "darkgreen",
      iconColor = "white"
    )

    draw_persisted_boundary <- function() {
      proxy <- leafletProxy("map", session = session) %>%
        clearGroup("drawn")

      if (!is.null(rv$draw_feature)) {
        proxy <- proxy %>%
          addGeoJSON(
            rv$draw_feature,
            group = "drawn",
            options = pathOptions(
              color = "#1e4d2b",
              weight = 2,
              fillOpacity = 0.10
            )
          )
      }

      invisible(proxy)
    }

    draw_selected_location <- function(zoom = NULL, clear_drawn = FALSE) {
      proxy <- leafletProxy("map", session = session) %>%
        clearGroup("selection")

      if (isTRUE(clear_drawn)) {
        proxy <- proxy %>% clearGroup("drawn")
      }

      if (!is.null(zoom)) {
        proxy <- proxy %>% setView(lng = rv$lon, lat = rv$lat, zoom = zoom)
      }

      proxy %>%
        addAwesomeMarkers(
          lng = rv$lon,
          lat = rv$lat,
          group = "selection",
          icon = selected_location_icon,
          popup = rv$label
        )
    }
    
    ########################################################
    # Initial map render
    ########################################################
    output$map <- renderLeaflet({
      aes_sites <- read_csu_aes_sites()

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = rv$map_lon, lat = rv$map_lat, zoom = rv$map_zoom) %>%
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
          overlayGroups = c("drawn", "selection", "CSU AES sites"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addCircleMarkers(
          data = aes_sites,
          lng = ~longitude,
          lat = ~latitude,
          group = "CSU AES sites",
          radius = 6,
          color = "#1e4d2b",
          fillColor = "#f1b82d",
          fillOpacity = 0.9,
          stroke = TRUE,
          popup = ~paste0(site_name, "<br>", round(latitude, 5), ", ", round(longitude, 5))
        ) %>%
        addAwesomeMarkers(
          lng = rv$lon,
          lat = rv$lat,
          group = "selection",
          icon = selected_location_icon,
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
      
      set_location(
        lat = loc$lat,
        lon = loc$lon,
        label = if (!is.null(loc$label)) loc$label else query,
        geom_type = "Point",
        draw_feature = NULL
      )

      draw_selected_location(zoom = 11, clear_drawn = TRUE)
    })
    
    ########################################################
    # Map click:
    # use clicked point as selected location
    ########################################################
    observeEvent(input$map_click, {
      set_location(
        lat = input$map_click$lat,
        lon = input$map_click$lng,
        label = "Map click",
        geom_type = "Point",
        draw_feature = NULL
      )

      draw_selected_location()
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
      
      set_location(
        lat = extracted$lat,
        lon = extracted$lon,
        label = paste(extracted$type, "selection"),
        geom_type = extracted$type,
        draw_feature = feat
      )
      
      leafletProxy("map", session = session) %>%
        clearGroup("selection") %>%
        clearGroup("drawn") %>%
        addGeoJSON(
          feat,
          group = "drawn",
          options = pathOptions(
            color = "#1e4d2b",
            weight = 2,
            fillOpacity = 0.10
          )
        ) %>%
        addAwesomeMarkers(
          lng = rv$lon,
          lat = rv$lat,
          icon = selected_location_icon,
          group = "selection",
          popup = paste0("Using ", extracted$type, " centroid")
        )
    })

    observeEvent(input$map_center, {
      rv$map_lat <- input$map_center$lat
      rv$map_lon <- input$map_center$lng

      if (!is.null(shared_state)) {
        shared_state$map_lat <- rv$map_lat
        shared_state$map_lon <- rv$map_lon
      }
    }, ignoreInit = TRUE)

    observeEvent(input$map_zoom, {
      rv$map_zoom <- input$map_zoom

      if (!is.null(shared_state)) {
        shared_state$map_zoom <- rv$map_zoom
      }
    }, ignoreInit = TRUE)

    if (!is.null(shared_state)) {
      observe({
        req(shared_state$lat, shared_state$lon)

        changed <- !isTRUE(all.equal(rv$lat, shared_state$lat)) ||
          !isTRUE(all.equal(rv$lon, shared_state$lon)) ||
          !identical(rv$label, shared_state$label) ||
          !identical(rv$geom_type, shared_state$geom_type) ||
          !identical(rv$draw_feature, shared_state$draw_feature)

        if (!isTRUE(changed)) {
          return()
        }

        rv$lat <- shared_state$lat
        rv$lon <- shared_state$lon
        rv$label <- shared_state$label
        rv$geom_type <- shared_state$geom_type
        rv$has_user_selection <- isTRUE(shared_state$has_user_selection)
        rv$draw_feature <- shared_state$draw_feature

        updateTextInput(session, "location_query", value = rv$label)
        draw_persisted_boundary()
        draw_selected_location()
      })

      observe({
        req(shared_state$map_lat, shared_state$map_lon, shared_state$map_zoom)

        map_changed <- !isTRUE(all.equal(rv$map_lat, shared_state$map_lat)) ||
          !isTRUE(all.equal(rv$map_lon, shared_state$map_lon)) ||
          !identical(rv$map_zoom, shared_state$map_zoom)

        if (!isTRUE(map_changed)) {
          return()
        }

        rv$map_lat <- shared_state$map_lat
        rv$map_lon <- shared_state$map_lon
        rv$map_zoom <- shared_state$map_zoom

        leafletProxy("map", session = session) %>%
          setView(lng = rv$map_lon, lat = rv$map_lat, zoom = rv$map_zoom)
      })
    }
    
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
          label = "Update Risk Plots",
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
        selected = target_tab
      )
      session$sendCustomMessage("clickElement", "refresh_model")
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
