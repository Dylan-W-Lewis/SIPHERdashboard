#' pt_VarLevelSelect UI Function
#'
#' @description select input that automatically updates level choices for selected variable.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param vars which variables to include in first selectInput
#' @param inline stack selectInputs or arrange side-by-side
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_VarLevelSelect_ui <- function(id, vars, inline=T){
  ns <- NS(id)
  tagList(
    fluidRow(column(selectInput(ns("var"), label="Variable", setNames(vars, translate_codes(vars)), width = "98%"),
                    width = ifelse(inline, 6, 12)),
             column(selectInput(ns("level"), label="Category", choices = NULL, width = "98%"),
                    width = ifelse(inline, 6, 12))
             )
    )
}

#' pt_VarLevelSelect Server Functions
#'
#' @noRd
mod_pt_VarLevelSelect_server <- function(id){
  moduleServer( id, function(input, output, session){

    cats <- reactive({
      cats <- get_cats(input$var)
      if(reference$categorical[reference$obs==input$var]) {
        cats <- setNames(cats, translate_codes(cats))
      }
      else {
        cats <- setNames(cats, "--")
      }
      return(cats)
      })

    ns <- session$ns
    observeEvent(input$var,{
      updateSelectInput(inputId = "level", choices = cats())
    })

    return(
      eventReactive(input$level, list(var = input$var, level = input$level))
      )

  })
}

## To be copied in the UI
# mod_pt_VarLevelSelect_ui("pt_VarLevelSelect_1")

## To be copied in the server
# mod_pt_VarLevelSelect_server("pt_VarLevelSelect_1")
