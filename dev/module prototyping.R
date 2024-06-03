library(shiny)

ui <- fluidPage(

  mod_pt_BYOG_parcoords_ui("pt_BYOG_StackedBar_1")


)

server <- function(input, output, session) {
  # r <- reactiveValues()
  # r$profile <- "E06000001"
  # Sys.setenv(MAPBOX_TOKEN = 11122223333444)
  #
  # text <- reactive(get_cats(input$choose)[1])
  #
  # output$txt <- renderPrint(text())
  #
  # #mod_page_AreaProfile2_server("page", r=r)
  # mod_pt_AreaMap3_server("pt_AreaMap3_1", r=r,
  #                         varbl = reactive(input$choose),#"total household net income - no deductions (mean)",#
  #                         categ = reactive(get_cats(input$choose)[1]))
  # # #mod_pt_VarLevelSelect_server("select", r=r)
  # #mod_pt_profile_health_server("pt_profile_poverty_1", r=r)

  mod_pt_BYOG_parcoords_server("pt_BYOG_StackedBar_1")


}

shinyApp(ui, server)
