#' pt_AreaSelections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom purrr map
mod_pt_AreaSelections_ui <- function(id){
  ns <- NS(id)
  tagList(
    #h1("Selected areas"),
    #  fluidRow(
    #    column(8,
    #           offset = 1,
    #           strong(textOutput(ns("selection1")))),
    #    column(3,
    #           actionButton(ns("button1"), "View area profile"))
    #    )
    uiOutput(ns("selections"))

  )
}

#' pt_AreaSelections Server Functions
#'
#' @noRd
mod_pt_AreaSelections_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$selections <- renderUI(
        purrr::map(r$selected_area, function(.x){
          fluidRow(
            column(8,
                   unique(lookup_wd_lad$lad_name[lookup_wd_lad$lad==.x])),
            column(4,
                   #actionButton(ns(.x), label = .x)
                   mod_pt_AreaSelections_button_ui(ns(paste0("button", .x)))
                   )
          )
      }))

    observeEvent(r$selected_area, {
      if(!is.null(r$selected_area)){
        purrr::map(r$selected_area, ~mod_pt_AreaSelections_button_server(paste0("button", .x), .x, r))
      }
    })
  })
}

## To be copied in the UI
# mod_pt_AreaSelections_ui("pt_AreaSelections_1")

## To be copied in the server
# mod_pt_AreaSelections_server("pt_AreaSelections_1")
