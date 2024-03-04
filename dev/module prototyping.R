library(shiny)
library(dplyr)

ui <- fluidPage(
  mod_page_AreaProfile2_ui("page")
  #mod_pt_VarLevelSelect_ui("select"),
  #mod_pt_AreaMap2_ui("map")

)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$profile <- "E06000001"
  Sys.setenv(MAPBOX_TOKEN = 11122223333444)

  mod_page_AreaProfile2_server("page", r=r)
  #mod_pt_VarLevelSelect_server("select", r=r)
  #mod_pt_AreaMap2_server("map", r=r)

}

shinyApp(ui, server)
