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
    plotly::plotlyOutput(ns("map"))
  )
}

#' pt_MapSelect Server Functions
#'
#' @noRd
mod_pt_MapSelect_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    ladShared <- crosstalk::SharedData$new(ladSF, ~LAD23NM, group="Local Authority District")

    output$map <- plotly::renderPlotly({
      ladShared %>%
        plotly::plot_ly(
         split = ~LAD23NM,
         color = I("gray90"),
         showlegend = FALSE) %>%
       plotly::highlight(
         color="rgba(0,117,176,1)",
         selectize = TRUE,
         persistent = TRUE,
         opacityDim = 0.6
          )
     })

    observeEvent(event_data("plotly_click"), {
      message(paste("click!", event_data("plotly_click")$key[[1]]))
      #selection <- event_data("plotly_click")$key[[1]]
      #selection_all <- c(r$selected_area, selection)
      #r$selected_area <- unique(selection_all)
    })

    observeEvent(ladShared$selection(),{
      r$selected_area <- ladSF$LAD23NM[ladShared$selection()]
    })


    # r$selected_area <- NULL
    # observeEvent( plotly::event_data("plotly_click") , {
    #   selection <- plotly::event_data("plotly_click")$key[[1]]
    #   #selection <- plotly::event_data("plotly_click")$customdata
    #   selection_all <- c(r$selected_area(), selection)
    #   r$selected_area <- unique(selection)
    # })
    # observeEvent( plotly::event_data("plotly_doubleclick") , {
    #   r$selected_area <- NULL
    #   #ensure global reactiveValues 'r' is initialised in app_server
    # })
  })
}

## To be copied in the UI
# mod_pt_MapSelect_ui("pt_MapSelect_1")

## To be copied in the server
# mod_pt_MapSelect_server("pt_MapSelect_1")
