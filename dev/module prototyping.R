library(shiny)
library(dplyr)


ui <- fluidPage(

  #mod_page_AreaProfile2_ui("page")
  #mod_pt_VarLevelSelect_ui("select"),
  radioButtons("choose", "Choose a category",
                choices=c(#"sf12pcs_dv",
                          #"sf12mcs_dv",
                          #"scghq1_dv",
                          "scsf2a",
                          "sclonely")),
    #mod_pt_profile_health_ui("pt_profile_poverty_1")
  # verbatimTextOutput("txt")

  #tags$head(tags$style(HTML( ".selectize-input {border: 1px solid #CCCCCC;}"))),
  div(style = "margin-top:-24px"),
  selectizeInput("selectize_inp",
                 label = "",
                 choices=c("Select areas by clicking the map or type here" = "", setNames(ladSF$lad,ladSF$lad_name)),
                 multiple=TRUE,
                 width="100%")
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


}

shinyApp(ui, server)
