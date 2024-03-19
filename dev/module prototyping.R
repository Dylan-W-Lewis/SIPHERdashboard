library(shiny)
library(dplyr)

ui <- fluidPage(
  mod_page_AreaProfile2_ui("page")
  #mod_pt_VarLevelSelect_ui("select"),
  #radioButtons("choose", "Choose a category", choices=c("total household net income - no deductions (mean)", "employment status")),
  #mod_pt_AreaMap3_ui("pt_AreaMap3_1")
  #mod_pt_profile_health_ui("pt_profile_poverty_1")
)

server <- function(input, output, session) {
  r <- reactiveValues()
  r$profile <- "E06000001"
  Sys.setenv(MAPBOX_TOKEN = 11122223333444)

  mod_page_AreaProfile2_server("page", r=r)
  # mod_pt_AreaMap3_server("pt_AreaMap3_1", r=r,
  #                        varbl = reactive(input$choose),#"total household net income - no deductions (mean)",#
  #                        categ = reactive(get_cats(input$choose)[1]))
  # #mod_pt_VarLevelSelect_server("select", r=r)
  #mod_pt_profile_health_server("pt_profile_poverty_1", r=r)


}

shinyApp(ui, server)
