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
    tags$style(HTML("
      body { background: #f7f8f5; }
      .app-header {
        border-bottom: 1px solid #d9dfd0;
        margin-bottom: 16px;
        padding: 10px 0 12px 0;
      }
      .app-header h2 { margin: 8px 0 2px 0; }
      .app-header p { color: #556052; margin: 0; }
      .sidebar-panel {
        background: #ffffff;
        border: 1px solid #dce2d5;
        border-radius: 8px;
      }
      .tool-section {
        background: #ffffff;
        border: 1px solid #dce2d5;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 12px;
      }
      .tool-section h3,
      .tool-section h4 { margin-top: 0; }
      .page-title { margin-top: 0; }
      details {
        background: #ffffff;
        border: 1px solid #dce2d5;
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
      .compact-pre {
        font-size: 12px;
        padding: 8px;
        margin-top: 8px;
        background: #f8f9fa;
        border: 1px solid #ddd;
        border-radius: 4px;
        white-space: pre-wrap;
      }
    "))
  ),

  fluidRow(
    class = "app-header",
    column(
      width = 2,
      img(
        src = "logo.png",
        height = "70px"
      )
    ),

    column(
      width = 10,
      h2("Potato Predictive Tools"),
      p("Weather-driven decision support for potato pest and disease risk")
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
    has_user_selection = FALSE
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

  degreeDayPlotServer(
    "dd_plot",
    dd_data = dd_data,
    risk1 = reactive(input$risk1),
    risk2 = reactive(input$risk2),
    risk3 = reactive(input$risk3)
  )

  temperaturePlotServer(
    "temp_plot",
    dd_data = dd_data,
    base_temp = reactive(input$rkn_base_temp)
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
