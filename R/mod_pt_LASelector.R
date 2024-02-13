#' pt_LASelector UI Function
#'
#' @description select LA by country.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_LASelector_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("country"), "Country",
                 choiceValues = c("S","E","W"),
                 choiceNames = c("Scotland", "England", "Wales")),
    selectInput(ns("LA"), "Local Authority",
                choices = NULL),
    textOutput(ns("LAchoice"))
  )
}

#' pt_LASelector Server Functions
#'
#' @noRd
mod_pt_LASelector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #input control
    countryLAs <- reactive({
      lookupList$lad[lookupList$ctr==input$country][[1]]
    })
    LAChoiceList <-reactive({
      choicelist <- setNames(as.list(countryLAs()$ladcd), countryLAs()$ladnm)
      sortedlist <- choicelist[sort(names(choicelist))]
      return(sortedlist)
    })
    observeEvent(countryLAs(),{
      freezeReactiveValue(input, "LA")
      updateSelectInput(inputId = "LA",
                        choices = LAChoiceList())
    })

    #output
    selection <- reactive({
      countryLAs() %>% filter(ladcd == input$LA)
    })

    output$LAchoice <- renderText({
      paste0("You have chosen ", selection()$ladnm, " (", selection()$ladcd, ")")
      })
  })
}

## To be copied in the UI
# mod_pt_LASelector_ui("pt_LASelector_1")

## To be copied in the server
# mod_pt_LASelector_server("pt_LASelector_1")
