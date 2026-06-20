############################################################
# About Page
# File: modules/about_page.R
############################################################

aboutPageUI <- function(id = NULL) {

  tagList(

    div(
      style = "max-width: 980px; margin: auto; line-height: 1.5;",

      h2("About Potato Predictive Tools"),

      p(
        "This app is a growing collection of weather-driven predictive tools ",
        "for potato pests and diseases. Each page keeps the workflow consistent ",
        "while preserving disease-specific thresholds and data sources."
      ),

      hr(),

      h3("Current Disease Pages"),

      tags$table(
        class = "table table-condensed",
        tags$thead(
          tags$tr(
            tags$th("Page"),
            tags$th("Current model"),
            tags$th("Primary data")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Root-Knot Nematode"),
            tags$td("Soil-temperature degree-day risk timing for Meloidogyne spp."),
            tags$td("CoAgMET station observations with gridded projection, or gridded map-point soil temperature")
          ),
          tags$tr(
            tags$td("Early Blight"),
            tags$td("Franc et al. 1988 air-temperature day-degree thresholds for potato early blight"),
            tags$td("CoAgMET station observations with gridded projection, or gridded map-point air temperature")
          )
        )
      ),

      tags$details(
        tags$summary("Design pattern"),
        tags$ul(
          tags$li("Each pest or disease gets a dedicated page."),
          tags$li("The sidebar workflow stays consistent across models."),
          tags$li("Advanced thresholds remain available but are lower-friction for normal users."),
          tags$li("Future models can be added as new modules.")
        )
      ),

      tags$details(
        tags$summary("Limitations"),
        tags$ul(
          tags$li("Degree-day models approximate development and risk; they do not replace scouting or local expertise."),
          tags$li("Forecast uncertainty increases beyond approximately 10 to 14 days."),
          tags$li("Disease-specific thresholds should be recalibrated before use outside the regions where they were developed.")
        )
      ),

      hr(),

      p(
        em("This tool suite is intended for research and educational purposes.")
      )
    )
  )
}
