#' pt_ParCoord UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_pt_ParCoord_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("par_coords"))

  )
}

#' pt_ParCoord Server Functions
#'
#' @noRd
mod_pt_ParCoord_server <- function(id, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plotDat <- reactive({
      toPlot <- dat() %>%
        dplyr::mutate(
          #scaled = scale(value),
          alpha = dplyr::if_else(geo=="Wards", 0.7, 1))

      # means <- codebook$mean[match(toPlot$obs, codebook$obs)]
      #
      # toPlot$label[means] <- paste0(round(toPlot$value[means], 2), " (avg.)")
      # toPlot$label[!means] <- paste0(round(toPlot$value[!means], 2), "%")
      #
       return(toPlot)
    })

    output$par_coords <- plotly::renderPlotly(
      plotly::ggplotly(
        ggplot2::ggplot(plotDat(),
                        ggplot2::aes(obs, scaled, text = labelled, group=area, color=geo, alpha=alpha, linetype = geo)) +
          ggplot2::geom_point() +
          ggplot2::geom_line() +
          ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                         legend.position = "top") +
          ggplot2::labs(color = NULL,
                        alpha = NULL,
                        linetype= NULL,
                        x = NULL),
        tooltip = c("text")
      )  %>%
        plotly::layout(legend = list(orientation = 'h', x = 0.45, y = 1.1))
    )

  })
}

## To be copied in the UI
# mod_pt_ParCoord_ui("pt_ParCoord_1")

## To be copied in the server
# mod_pt_ParCoord_server("pt_ParCoord_1")
