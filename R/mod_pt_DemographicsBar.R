#' pt_DemographicsBar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_DemographicsBar_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("bar"))

  )
}

#' pt_DemographicsBar Server Functions
#'
#' @noRd
mod_pt_DemographicsBar_server <- function(id, dat, varbl, categ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plotDat <- reactive({
      toPlot <- dat() %>%
        dplyr::filter(obs==varbl(),
               cat==categ(),
               age!="all_ages",
               sex!="both")

      return(toPlot)
    })


    output$bar <- plotly::renderPlotly(
      plotly::ggplotly(
        plotDat() %>%
          ggplot2::ggplot(ggplot2::aes(x=age, y=value, fill=as.factor(sex), text=labelled)) +
          ggplot2::geom_col(position = "dodge") +
          scale_fill_sipher(palette_name = "full", type = "discrete") +
          ggplot2::scale_x_discrete(labels = function(x) translate_codes(x)) +
          ggplot2::theme_bw()+
          ggplot2::labs(fill = "Gender", #stringr::str_to_sentence(var),
                        y= NULL,
                        x= "Age"),
        tooltip = c("text")
      )
    )

  })
}

## To be copied in the UI
# mod_pt_DemographicsBar_ui("pt_DemographicsBar_1")

## To be copied in the server
# mod_pt_DemographicsBar_server("pt_DemographicsBar_1")
