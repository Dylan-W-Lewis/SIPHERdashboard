#' pt_MapSelect2 UI Function
#'
#' @description Select areas by map or selectize input.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactives to store area ids
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_MapSelect2_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("selectize_inp"),
                   label = "",
                   choices=c("Choose an area" = "", set_names(ladSF$LAD23CD,ladSF$LAD23NM)),
                   multiple=TRUE,
                   width="100%"),
    plotly::plotlyOutput(ns("map"))
  )
}

#' pt_MapSelect2 Server Functions
#'
#' @noRd
mod_pt_MapSelect2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #render map
    output$map <- renderPlotly({
      ladSF %>%
        plot_ly(
          key = ~LAD23CD,
          split = ~LAD23CD,
          color = I("gray"),
          source = "map",
          showlegend = FALSE,
          text = ~LAD23NM,
          hoverinfo="text"
        ) %>%
        add_sf()
    })

    observeEvent(event_data("plotly_click", source = "map"), {
      #identify area code from map click
      cd <- event_data("plotly_click", source = "map")$key[[1]]
      #add to input
      updateSelectizeInput("selectize_inp", session = session, selected = unique(c(input$selectize_inp, cd)))

    })

    observeEvent(input$selectize_inp, {
      #pick colour based on selection
      col = ifelse(ladSF$LAD23CD %in% input$selectize_inp, "#005CBA", "gray80")

      #restyle plot to assign new colours
      plotlyProxy("map", session) %>%
        plotlyProxyInvoke(
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
