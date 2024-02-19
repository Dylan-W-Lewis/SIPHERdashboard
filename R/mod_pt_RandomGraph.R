#' pt_RandomGraph UI Function
#'
#' @description A practice module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_RandomGraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("type"), "Graph type",
                choices = c("bar","boxplot","col","violin")),
    plotOutput(ns("plot"))
  )
}

#' pt_RandomGraph Server Functions
#'
#' @noRd
#'
#' @importFrom shinipsum random_ggplot
mod_pt_RandomGraph_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #graph <- reactive(shinipsum::random_ggplot(type = input$type))
    output$plot <- renderPlot({
      shinipsum::random_ggplot(type = input$type)
      })
  })
}

## To be copied in the UI
# mod_pt_RandomGraph_ui("pt_RandomGraph_1")

## To be copied in the server
# mod_pt_RandomGraph_server("pt_RandomGraph_1")
