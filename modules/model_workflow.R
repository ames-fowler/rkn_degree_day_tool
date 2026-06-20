############################################################
# Shared Model Workflow
# File: modules/model_workflow.R
############################################################

modelWorkflowUI <- function() {
  tagList(
    h4("1. Choose Crop"),
    selectInput(
      "crop",
      label = NULL,
      choices = c("Potato" = "potato"),
      selected = "potato"
    ),

    h4("2. Choose Disease or Pest"),
    selectInput(
      "model_choice",
      label = NULL,
      choices = c(
        "Root-knot nematode" = "rkn",
        "Early blight" = "early_blight"
      ),
      selected = "rkn"
    ),

    h4("3. Choose Location Source"),
    radioButtons(
      "location_source",
      label = NULL,
      choices = c(
        "CoAgMET station" = "coagmet",
        "Map point or polygon" = "map"
      ),
      selected = "coagmet"
    ),

    h4("4. Choose Location"),
    conditionalPanel(
      condition = "input.location_source == 'coagmet'",
      helpText("Select a station on the model page.")
    ),
    conditionalPanel(
      condition = "input.location_source == 'map'",
      helpText("Search, click, or draw on the model page.")
    ),

    h4("5. Enter Start Date"),
    dateInput(
      "start_date",
      label = NULL,
      value = Sys.Date() - 30
    ),

    h4("6. View Risk"),
    actionButton(
      "refresh_model",
      "Refresh Risk",
      class = "btn-primary",
      width = "100%"
    ),
    helpText("Refresh after changing location, date, or advanced parameters."),

    hr(),

    tags$details(
      tags$summary("Advanced model parameters"),

      conditionalPanel(
        condition = "input.model_choice == 'rkn'",
        numericInput(
          "rkn_base_temp",
          "Root-knot base temperature (deg C)",
          value = 5,
          min = -5,
          max = 20,
          step = 0.5
        ),
        conditionalPanel(
          condition = "input.location_source == 'map'",
          selectInput(
            "rkn_soil_depth",
            "Gridded soil temperature depth",
            choices = c(
              "0 cm"  = "soil_temperature_0cm",
              "6 cm"  = "soil_temperature_6cm",
              "18 cm" = "soil_temperature_18cm",
              "54 cm" = "soil_temperature_54cm"
            ),
            selected = "soil_temperature_6cm"
          )
        ),
        conditionalPanel(
          condition = "input.location_source == 'coagmet'",
          selectInput(
            "rkn_coagmet_measure",
            "CoAgMET soil temperature depth",
            choices = c(
              "5 cm" = "soil_5cm",
              "15 cm" = "soil_15cm"
            ),
            selected = "soil_5cm"
          )
        ),
        numericInput("risk1", "J2 emergence", value = 400, min = 0, step = 25),
        numericInput("risk2", "infection window", value = 600, min = 0, step = 25),
        numericInput("risk3", "reproduction threshold", value = 800, min = 0, step = 25)
      ),

      conditionalPanel(
        condition = "input.model_choice == 'early_blight'",
        selectInput(
          "early_production_area",
          "Production area / threshold",
          choices = c(
            "San Luis Valley: 361 DDC" = "san_luis_valley",
            "Northeastern plains: 626 DDC" = "northeastern_plains",
            "Custom threshold" = "custom"
          ),
          selected = "san_luis_valley"
        ),
        conditionalPanel(
          condition = "input.early_production_area == 'custom'",
          numericInput(
            "early_custom_threshold",
            "Custom DDC threshold",
            value = 361,
            min = 0,
            step = 25
          )
        ),
        numericInput(
          "early_base_temp",
          "Early blight base temperature (deg C)",
          value = 7.2,
          min = 0,
          max = 20,
          step = 0.1
        )
      )
    )
  )
}
