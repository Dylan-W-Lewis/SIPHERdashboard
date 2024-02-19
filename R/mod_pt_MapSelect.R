#' pt_MapSelect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_MapSelect_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("map"))
  )
}

#' pt_MapSelect Server Functions
#'
#' @noRd
mod_pt_MapSelect_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$map <- renderPlotly({
      ladSF %>%
        highlight_key(~LAD23NM, group="Local Authority District") %>%
        plot_ly(
          split = ~LAD23NM,
          color = I("gray90"),
          showlegend = FALSE) %>%
        highlight(
          color="rgba(0,117,176,1)",
          selectize = TRUE,
          opacityDim = 0.6
        )
    })

    observeEvent( event_data("plotly_click") , {
      r$selected_area <- event_data("plotly_click")$key[[1]]
      #ensure global reactiveValues 'r' is initialised in app_server
    })
  })
}

## To be copied in the UI
# mod_pt_MapSelect_ui("pt_MapSelect_1")

## To be copied in the server
# mod_pt_MapSelect_server("pt_MapSelect_1")
