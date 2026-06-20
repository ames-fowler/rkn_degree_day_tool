############################################################
# Potato Predictive Tools
#
# Main application file
#
# Responsibilities:
#   - Load libraries
#   - Load helper functions
#   - Load modules
#   - Define UI layout
#   - Wire module outputs together
############################################################



############################################################
# 1. LOAD LIBRARIES
############################################################

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(lubridate)
library(here)



############################################################
# 2. LOAD HELPER FUNCTIONS
#
# These contain NON-Shiny logic
# (API calls, math, geospatial helpers)
############################################################

list.files(here("R"), full.names = TRUE) |> lapply(source)



############################################################
# 3. LOAD MODULES
#
# These contain reusable Shiny components
############################################################

list.files(here("modules"), full.names = TRUE) |> lapply(source)

risk_labels <- c(
  "J2 emergence", "infection window", "reproduction threshold"
)



############################################################
# 4. USER INTERFACE
############################################################

ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('clickElement', function(id) {
        var el = document.getElementById(id);
        if (el) {
          el.click();
        }
      });
    ")),
    tags$style(HTML("
      body {
        background: #f4f6f1;
        color: #1f2a1f;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      .container-fluid {
        max-width: 1480px;
      }
      .app-header {
        background: #ffffff;
        border-bottom: 4px solid #1e4d2b;
        box-shadow: 0 1px 8px rgba(31, 42, 31, 0.08);
        margin: 0 -15px 18px -15px;
        padding: 14px 24px 16px 24px;
      }
      .app-logo {
        max-height: 68px;
        max-width: 100%;
        object-fit: contain;
      }
      .app-eyebrow {
        color: #6b762f;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.08em;
        margin-bottom: 2px;
        text-transform: uppercase;
      }
      .app-header h1 {
        color: #1e4d2b;
        font-size: 30px;
        font-weight: 700;
        line-height: 1.15;
        margin: 0;
      }
      .app-header p {
        color: #4f5d4b;
        font-size: 15px;
        margin: 4px 0 0 0;
      }
      .app-badge {
        background: #f1b82d;
        border-radius: 999px;
        color: #1f2a1f;
        display: inline-block;
        font-size: 12px;
        font-weight: 700;
        margin-top: 8px;
        padding: 4px 10px;
      }
      .sidebar-panel {
        background: #ffffff;
        border: 1px solid #d8dfd2;
        border-radius: 8px;
        box-shadow: 0 1px 8px rgba(31, 42, 31, 0.06);
        padding-top: 12px;
      }
      .workflow-step {
        border-bottom: 1px solid #edf1e8;
        margin-bottom: 12px;
        padding-bottom: 10px;
      }
      .workflow-step:last-of-type {
        border-bottom: 0;
      }
      .workflow-step h4 {
        color: #1e4d2b;
        font-size: 13px;
        font-weight: 700;
        letter-spacing: 0.02em;
        margin: 0 0 6px 0;
        text-transform: uppercase;
      }
      .workflow-step .help-block {
        color: #657260;
        font-size: 12px;
        margin-bottom: 0;
      }
      .btn-primary {
        background: #1e4d2b;
        border-color: #1e4d2b;
        font-weight: 700;
      }
      .btn-primary:hover,
      .btn-primary:focus {
        background: #16391f;
        border-color: #16391f;
      }
      .nav-tabs {
        border-bottom-color: #d8dfd2;
        margin-bottom: 14px;
      }
      .nav-tabs > li > a {
        color: #38513a;
        font-weight: 600;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        border-top: 3px solid #1e4d2b;
        color: #1e4d2b;
        font-weight: 700;
      }
      .tool-section {
        background: #ffffff;
        border: 1px solid #d8dfd2;
        border-radius: 8px;
        box-shadow: 0 1px 8px rgba(31, 42, 31, 0.06);
        padding: 14px;
        margin-bottom: 14px;
      }
      .tool-section h3,
      .tool-section h4 {
        color: #1e4d2b;
        margin-top: 0;
      }
      .page-title {
        color: #1e4d2b;
        font-weight: 700;
        margin-top: 0;
      }
      details {
        background: #ffffff;
        border: 1px solid #d8dfd2;
        border-radius: 8px;
        padding: 10px 12px;
        margin: 10px 0;
      }
      summary {
        cursor: pointer;
        font-weight: 600;
      }
      .muted-note {
        color: #5f6b5c;
        font-size: 0.92em;
      }
      .model-chip-row {
        margin: 8px 0 14px 0;
      }
      .model-chip {
        background: #edf3e8;
        border: 1px solid #d3dec9;
        border-radius: 999px;
        color: #29472d;
        display: inline-block;
        font-size: 12px;
        font-weight: 700;
        margin: 0 6px 6px 0;
        padding: 5px 10px;
      }
      table.table {
        background: #ffffff;
      }
      .form-control {
        border-color: #ccd6c5;
        border-radius: 6px;
      }
      .compact-pre {
        font-size: 12px;
        padding: 8px;
        margin-top: 8px;
        background: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 4px;
        white-space: pre-wrap;
      }
      .app-footer {
        color: #667360;
        font-size: 12px;
        margin: 24px 0 12px 0;
        text-align: center;
      }
      .leaflet-container {
        cursor: crosshair;
      }
    "))
  ),

  fluidRow(
    class = "app-header",
    column(
      width = 1,
      img(
        src = "logo.png",
        class = "app-logo"
      )
    ),

    column(
      width = 11,
      div(class = "app-eyebrow", "Research decision support"),
      h1("Potato Predictive Tools"),
      p("Weather-driven pest and disease risk models for potato production."),
      span(class = "app-badge", "Research preview")
    )
  ),

  sidebarLayout(

    ########################################################
    # SIDEBAR CONTROLS
    ########################################################

    sidebarPanel(
      class = "sidebar-panel",
      modelWorkflowUI()
    ),



    ########################################################
    # MAIN PANEL
    ########################################################

    mainPanel(
      tabsetPanel(
        id = "main_tabs",

        tabPanel(
          "Root-Knot Nematode",
          value = "rkn",
          br(),
          rootKnotPageUI()
        ),

        tabPanel(
          "Early Blight",
          value = "early_blight",
          br(),
          earlyBlightPageUI("early_blight")
        ),

        tabPanel(
          "About",
          value = "about",
          br(),
          aboutPageUI()
        )
      ),
      div(
        class = "app-footer",
        "For research and educational use. Model outputs should be interpreted with scouting and local expertise."
      )
    )
  )
)



############################################################
# 5. SERVER LOGIC
############################################################

server <- function(input, output, session) {

  ##########################################################
  # 5.1 MAP MODULE
  #
  # Returns selected location
  ##########################################################

  shared_map_state <- reactiveValues(
    lat = 40.5853,
    lon = -105.0844,
    label = "Fort Collins, CO",
    geom_type = "Point",
    has_user_selection = FALSE,
    draw_feature = NULL,
    map_lat = 40.5853,
    map_lon = -105.0844,
    map_zoom = 9
  )

  location <- mapModuleServer(
    "map",
    parent_session = session,
    target_tab = "rkn",
    shared_state = shared_map_state
  )
  rkn_station <- coagmetStationServer("rkn_station", initial_station = "ftc01")

  observeEvent(input$model_choice, {
    updateTabsetPanel(
      session = session,
      inputId = "main_tabs",
      selected = input$model_choice
    )
  }, ignoreInit = TRUE)

  observeEvent(input$main_tabs, {
    if (input$main_tabs %in% c("rkn", "early_blight")) {
      updateSelectInput(session, "model_choice", selected = input$main_tabs)
    }
  }, ignoreInit = TRUE)



  ##########################################################
  # 5.2 WEATHER DATA
  #
  # Pull soil temperature history + forecast for root-knot page
  ##########################################################

  weather_data <- eventReactive(input$refresh_model, {
    req(input$start_date)

    if (identical(input$location_source, "coagmet")) {
      req(rkn_station())
      req(input$rkn_coagmet_measure)

      fetch_coagmet_with_projection(
        station_row = rkn_station(),
        planting_date = input$start_date,
        measure = input$rkn_coagmet_measure,
        forecast_days = 14
      )
    } else {
      req(location())
      req(input$rkn_soil_depth)

      fetch_openmeteo_timeseries(
        lat = location()$lat,
        lon = location()$lon,
        planting_date = input$start_date,
        depth_var = input$rkn_soil_depth,
        forecast_days = 14
      )
    }
  }, ignoreNULL = FALSE)



  ##########################################################
  # 5.3 DEGREE DAY CALCULATION
  ##########################################################

  dd_data <- reactive({
    req(weather_data())
    req(input$start_date)
    req(input$rkn_base_temp)

    build_degree_days(
      df_hourly = weather_data(),
      planting_date = input$start_date,
      base_temp = input$rkn_base_temp
    )
  })



  ##########################################################
  # 5.4 DISEASE PAGE MODULES
  ##########################################################

  rkn_location_label <- reactive({
    if (identical(input$location_source, "coagmet")) {
      req(rkn_station())
      format_station_location_label(rkn_station())
    } else {
      req(location())
      format_map_location_label(location())
    }
  })

  degreeDayPlotServer(
    "dd_plot",
    dd_data = dd_data,
    risk1 = reactive(input$risk1),
    risk2 = reactive(input$risk2),
    risk3 = reactive(input$risk3),
    plot_title = reactive(paste("Root-Knot Cumulative Degree Days -", rkn_location_label()))
  )

  temperaturePlotServer(
    "temp_plot",
    dd_data = dd_data,
    base_temp = reactive(input$rkn_base_temp),
    plot_title = reactive(paste("Daily Mean Soil Temperature -", rkn_location_label()))
  )

  rkn_temperature_context <- reactive({
    if (identical(input$location_source, "coagmet")) {
      req(rkn_station())
      req(input$rkn_coagmet_measure)

      list(
        lat = as.numeric(rkn_station()$latitude[[1]]),
        lon = as.numeric(rkn_station()$longitude[[1]]),
        kind = "soil",
        measure = input$rkn_coagmet_measure
      )
    } else {
      req(location())
      req(input$rkn_soil_depth)

      list(
        lat = location()$lat,
        lon = location()$lon,
        kind = "soil",
        measure = input$rkn_soil_depth
      )
    }
  })

  rkn_calendar_current_daily <- eventReactive(input$refresh_model, {
    req(rkn_temperature_context())
    ctx <- rkn_temperature_context()
    year_start <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))

    temps <- if (identical(input$location_source, "coagmet")) {
      req(rkn_station())
      fetch_coagmet_with_projection(
        station_row = rkn_station(),
        planting_date = year_start,
        measure = ctx$measure,
        forecast_days = 1
      )
    } else {
      fetch_openmeteo_timeseries(
        lat = ctx$lat,
        lon = ctx$lon,
        planting_date = year_start,
        depth_var = ctx$measure,
        forecast_days = 1
      )
    }

    build_daily_temperature(temps, start_date = year_start) %>%
      filter(date <= Sys.Date())
  }, ignoreNULL = FALSE)

  rkn_calendar_normals <- eventReactive(input$refresh_model, {
    req(rkn_temperature_context())
    ctx <- rkn_temperature_context()

    build_temperature_normals(
      lat = ctx$lat,
      lon = ctx$lon,
      daily_var = archive_daily_variable_for_temperature(ctx$kind, ctx$measure)
    )
  }, ignoreNULL = FALSE)

  calendarTemperaturePlotServer(
    "calendar_temp",
    current_daily = rkn_calendar_current_daily,
    normals_daily = rkn_calendar_normals,
    planting_date = reactive(input$start_date),
    plot_title = reactive(paste("Calendar Temperature Context -", rkn_location_label())),
    variable_label = reactive({
      ctx <- rkn_temperature_context()
      temperature_context_label(ctx$kind, ctx$measure)
    }),
    active = reactive(identical(input$rkn_plot_view, "calendar_temperature"))
  )

  riskTimingSummaryServer(
    "rkn_timing",
    dd_data = dd_data,
    thresholds = reactive(
      build_threshold_table(
        labels = risk_labels,
        thresholds = c(input$risk1, input$risk2, input$risk3)
      )
    )
  )

  early_threshold <- reactive({
    switch(
      input$early_production_area,
      san_luis_valley = 361,
      northeastern_plains = 626,
      custom = input$early_custom_threshold,
      361
    )
  })

  earlyBlightPageServer(
    "early_blight",
    location_source = reactive(input$location_source),
    start_date = reactive(input$start_date),
    base_temp = reactive(input$early_base_temp),
    threshold_ddc = early_threshold,
    production_area = reactive(input$early_production_area),
    refresh_trigger = reactive(input$refresh_model),
    map_state = shared_map_state
  )
}



############################################################
# 6. RUN APP
############################################################

shinyApp(ui, server)
