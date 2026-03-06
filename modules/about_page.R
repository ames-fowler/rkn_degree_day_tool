############################################################
# About Page
# File: modules/about_page.R
############################################################

aboutPageUI <- function(id = NULL) {
  
  tagList(
    
    div(
      style = "max-width: 900px; margin: auto; line-height: 1.6;",
      
      h2("About This Tool"),
      
      p(
        "This tool estimates seasonal risk for potato root-knot nematodes ",
        "(Meloidogyne spp.) using accumulated soil temperature degree days ",
        "derived from historical and forecast soil temperature data."
      ),
      
      hr(),
      
      h3("Biological Background"),
      
      p(
        "Root-knot nematodes infect potato roots during the second-stage juvenile ",
        "(J2) life stage. Eggs develop in soil and hatch when temperatures become ",
        "suitable for development."
      ),
      
      p(
        "Because egg development and juvenile emergence are strongly controlled by ",
        "soil temperature, accumulated soil temperature (degree days) can be used ",
        "to estimate key stages in nematode population development."
      ),
      
      tags$ul(
        tags$li("Initial J2 emergence, marking the beginning of infection risk"),
        tags$li("Peak infection windows when root invasion is most likely"),
        tags$li("Population reproduction thresholds that can lead to rapid population growth")
      ),
      
      hr(),
      
      h3("Data Sources"),
      
      tags$ul(
        tags$li(
          strong("Soil temperature data: "),
          "Open-Meteo weather API historical and forecast soil temperature products."
        ),
        tags$li(
          strong("Forecast horizon: "),
          "14-day soil temperature forecast."
        ),
        tags$li(
          strong("Biological parameters: "),
          "Published research on Meloidogyne development and potato infection dynamics."
        )
      ),
      
      hr(),
      
      h3("Model Limitations"),
      
      tags$ul(
        tags$li("Degree-day models approximate nematode development but do not account for soil moisture."),
        tags$li("Local soil conditions, irrigation practices, and cultivar resistance can alter infection timing."),
        tags$li("Forecast uncertainty increases beyond approximately 10 to 14 days.")
      ),
      
      hr(),
      
      p(
        em("This tool is intended for research and educational purposes.")
      )
    )
  )
}