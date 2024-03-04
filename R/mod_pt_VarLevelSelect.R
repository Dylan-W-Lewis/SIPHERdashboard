#' pt_VarLevelSelect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_VarLevelSelect_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(selectInput(ns("var"), label="", choices=codebook$obs), width=6),
             column(selectInput(ns("level"), label="", choices = NULL), width=6))

  )
}

#' pt_VarLevelSelect Server Functions
#'
#' @noRd
mod_pt_VarLevelSelect_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$var,{
      updateSelectInput(inputId = "level", choices = dplyr::pull(codebook$cat[codebook$obs==input$var][[1]]))
    })

    observe({
      r$var_selection <- list(var = input$var, level = input$level)})


  })
}

## To be copied in the UI
# mod_pt_VarLevelSelect_ui("pt_VarLevelSelect_1")

## To be copied in the server
# mod_pt_VarLevelSelect_server("pt_VarLevelSelect_1")
