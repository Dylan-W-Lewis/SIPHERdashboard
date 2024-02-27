library(shiny)
library(dplyr)

ui <- fluidPage(
  mod_pt_AreaMap_ui("areamap")

)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$profile <- "E06000001"

  mod_pt_AreaMap_server("areamap", r=r)

}

shinyApp(ui, server)
