#' pt_ComparisonGraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues to get selected area ids
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_ComparisonGraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    #selectInput(ns("var"), label = "", choices = unique(laDat$obs)),
    plotOutput(ns("graph"), height="400px", fill = F)

  )
}

#' pt_ComparisonGraph Server Functions
#'
#' @noRd
mod_pt_ComparisonGraph_server <- function(id, r, var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dat <- reactive({
      ladDat %>%
        filter(sex=="both",
             age_dv=="all_ages",
             lad %in% r$selected_area,
             obs==var) %>%
        group_by(lad)
    })

    output$graph <- renderPlot({
      if(!is.null(r$selected_area)) {
        dat() %>%
          ggplot2::ggplot(aes(x=lad, y=value, fill=cat)) +
          geom_col() +
          coord_flip() +
          theme(legend.position = "top")
      }

    })
  })
}

## To be copied in the UI
# mod_pt_ComparisonGraph_ui("pt_ComparisonGraph_1")

## To be copied in the server
# mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1")
