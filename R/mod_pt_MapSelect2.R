#' pt_MapSelect2 UI Function
#'
#' @description Select areas by map or selectize input.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues object to store area ids
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_MapSelect2_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("selectize_inp"),
                   label = "",
                   choices=c("Select areas by clicking the map or type here" = "", setNames(ladSF$lad,ladSF$lad_name)),
                   multiple=TRUE,
                   width="100%"),
    div(style = "margin-top:-10vh"),
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("map"), height = "85vh"),
      color = "#005CBA")
  )
}

#' pt_MapSelect2 Server Functions
#'
#' @noRd
mod_pt_MapSelect2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #render map
    output$map <- plotly::renderPlotly({
      ladSF %>%
        plotly::plot_ly(
          key = ~lad,
          split = ~lad,
          color = I("gray"),
          source = "map",
          showlegend = FALSE,
          text = ~lad_name,
          hoveron = "fills",
          hoverinfo="text"
        ) %>%
        plotly::add_sf()
    })

    observeEvent(plotly::event_data("plotly_click", source = "map"), {
      #identify area code from map click
      cd <- plotly::event_data("plotly_click", source = "map")$key[[1]]
      #add to input
      updateSelectizeInput("selectize_inp", session = session, selected = unique(c(input$selectize_inp, cd)))

    })

    observeEvent(input$selectize_inp, {
      #pick colour based on selection
      col = ifelse(ladSF$lad %in% input$selectize_inp, "#005CBA", "gray80")

      #restyle plot to assign new colours
      plotly:: plotlyProxy("map", session) %>%
        plotly::plotlyProxyInvoke(
          "restyle",
          list(
            fillcolor = col
          )
        )

      #update global reactiveValues
      r$selected_area <- input$selectize_inp
    })

  })
}

## To be copied in the UI
# mod_pt_MapSelect2_ui("pt_MapSelect2_1")

## To be copied in the server
# mod_pt_MapSelect2_server("pt_MapSelect2_1")
