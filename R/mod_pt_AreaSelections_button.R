#' pt_AreaSelections_button UI Function
#'
#' @description button to link to area profile, should be called by AreaSelections module
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param area area name or code supplied by AreaSelections module
#' @param r global reactives list
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_AreaSelections_button_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionLink(ns("link"), "View area profile")
  )
}

#' pt_AreaSelections_button Server Functions
#'
#' @noRd
mod_pt_AreaSelections_button_server <- function(id, area, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$link,{
      message(paste("clicked", area))
      r$profile <- area
      r$active_page <- "area_profile"
    })
  })
}

## To be copied in the UI
# mod_pt_AreaSelections_button_ui("pt_AreaSelections_button_1")

## To be copied in the server
# mod_pt_AreaSelections_button_server("pt_AreaSelections_button_1")
