############################################################
# Root-Knot Nematode Degree Day Tool
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
  # "Early Risk",
  # "High Risk",
  # "Severe Risk"
  "J2 emergence", "infection window", "reproduction threshold"
)

############################################################
# 4. USER INTERFACE
############################################################

ui <- fluidPage(
  
  fluidRow(
    column(
      width = 2,
      img(
        src = "logo.png",
        height = "70px"
      )
    ),
    
    column(
      width = 10,
      h2("Potato Root-Knot Nematode Risk Tool"),
      p("Temperature-driven prediction of root-knot nematode infection risk")
    )
  ),
  
  sidebarLayout(
    
    ########################################################
    # SIDEBAR CONTROLS
    ########################################################
    
    sidebarPanel(
      
      dateInput(
        "planting_date",
        "Planting date",
        value = Sys.Date() - 30
      ),
      
      numericInput(
        "base_temp",
        "Base temperature (°C)",
        value = 5,
        min = -5,
        max = 20,
        step = 0.5
      ),
      
      selectInput(
        "soil_depth",
        "Soil temperature depth",
        choices = c(
          "0 cm"  = "soil_temperature_0cm",
          "6 cm"  = "soil_temperature_6cm",
          "18 cm" = "soil_temperature_18cm",
          "54 cm" = "soil_temperature_54cm"
        ),
        selected = "soil_temperature_6cm"
      ),
      
      hr(),
      
      numericInput(
        "risk1",
        risk_labels[1],
        value = 400,
        min = 0,
        step = 25
      ),
      
      numericInput(
        "risk2",
        risk_labels[2],
        value = 600,
        min = 0,
        step = 25
      ),
      
      numericInput(
        "risk3",
        risk_labels[3],
        value = 800,
        min = 0,
        step = 25
      ),
      
      hr(),
      
      helpText("Search, click, or draw on the map to select a location.")
    ),
    
    
    
    ########################################################
    # MAIN PANEL
    ########################################################
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Map",
          br(),
          mapModuleUI("map")
        ),
        
        tabPanel(
          "Risk Plots",
          br(),
          temperaturePlotUI("temp_plot"),
          br(),
          degreeDayPlotUI("dd_plot")
        ),
        
        tabPanel(
          "About",
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
  
  location <- mapModuleServer("map")
  
  
  
  ##########################################################
  # 5.2 INPUT VALIDATION
  ##########################################################
  
  # observe({
  #   shiny::validate(
  #     shiny::need(input$risk1 < input$risk2, "Risk zone 1 must be less than risk zone 2."),
  #     shiny::need(input$risk2 < input$risk3, "Risk zone 2 must be less than risk zone 3.")
  #   )
  # })
  # 
  
  
  ##########################################################
  # 5.3 WEATHER DATA
  #
  # Pull soil temperature history + forecast
  ##########################################################
  
  weather_data <- reactive({
    req(location())
    req(input$planting_date)
    req(input$soil_depth)
    
    fetch_openmeteo_timeseries(
      lat = location()$lat,
      lon = location()$lon,
      planting_date = input$planting_date,
      depth_var = input$soil_depth,
      forecast_days = 14
    )
  })
  
  
  
  ##########################################################
  # 5.4 DEGREE DAY CALCULATION
  ##########################################################
  
  dd_data <- reactive({
    req(weather_data())
    req(input$planting_date)
    req(input$base_temp)
    
    build_degree_days(
      df_hourly = weather_data(),
      planting_date = input$planting_date,
      base_temp = input$base_temp
    )
  })
  
  
  
  ##########################################################
  # 5.5 PLOT MODULES
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
    base_temp = reactive(input$base_temp)
  )
}



############################################################
# 6. RUN APP
############################################################

shinyApp(ui, server)