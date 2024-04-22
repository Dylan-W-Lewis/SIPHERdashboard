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
mod_pt_ParCoord_server <- function(id, dat, varNames){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    plotDat <- reactive({
      toPlot <- dat() %>%
        dplyr::left_join(., sf::st_drop_geometry(wardSF[, c("ward", "ward_name")]), by=c("area" = "ward")) %>%
        dplyr::group_by(obs) %>%
        dplyr::mutate(
          labelled_new = dplyr::if_else(is.na(ward_name),
                                        stringr::str_c(geo, labelled, sep= ": "),
                                        stringr::str_c(ward_name, labelled, sep= ": ")),
          #var_label =
            # dplyr::if_else(reference$categorical[reference$obs==obs],
            #                          stringr::str_c(codebook$name[codebook$code==obs],
            #                                         codebook$name[codebook$code==cat],
            #                                         sep= ": "),
            #                          codebook$name[codebook$code==obs]),
          alpha = dplyr::if_else(geo=="Wards", 0.7, 1)
        )


      # means <- codebook$mean[match(toPlot$obs, codebook$obs)]
      #
      # toPlot$label[means] <- paste0(round(toPlot$value[means], 2), " (avg.)")
      # toPlot$label[!means] <- paste0(round(toPlot$value[!means], 2), "%")
      #
       return(toPlot)
    })

    output$par_coords <- plotly::renderPlotly(
      plotly::ggplotly(
        ggplot2::ggplot(data = plotDat(),
                        ggplot2::aes(y= obs, x=scaled, text = labelled_new, group=area, color=geo, alpha=alpha, shape=geo)) +
          ggplot2::geom_point() +
          #ggplot2::geom_line(#data = . %>% filter(geo=="GB")
          #                   ) +
          ggplot2::scale_shape_manual(values = c(3, 16, 16)) +
          ggplot2::scale_y_discrete(labels = function(y) stringr::str_wrap(make_var_labels(y), width = 35)) +
          #ggplot2::annotate("text", x = max(plotDat()$scaled)-1, y = 0.5, label = "Higher than average", color="grey") +
          #ggplot2::annotate("text", x = min(plotDat()$scaled)+1, y = 0.5, label = "Lower than average", color="grey") +
          ggplot2::theme_minimal() +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = -200), hjust = 0.5, color="grey"),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(vjust = 0.5, hjust = 0),
                         legend.position = "top") +
          ggplot2::labs(color = NULL,
                        alpha = NULL,
                        linetype= NULL,
                        y = NULL,
                        x = "Lower than average                    Higher than average",
                        shape = NULL),
        tooltip = c("text")
      )  %>%
        plotly::layout(legend = list(orientation = 'h', x = 0.5, y = 1.05))
    )

  })
}

## To be copied in the UI
# mod_pt_ParCoord_ui("pt_ParCoord_1")

## To be copied in the server
# mod_pt_ParCoord_server("pt_ParCoord_1")
