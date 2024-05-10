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
      toPlot <- dat() |>
        dplyr::left_join(sf::st_drop_geometry(wardSF[, c("ward", "ward_name")]), by=c("area" = "ward")) |>
        dplyr::group_by(obs) |>
        dplyr::mutate(
          labelled_new = dplyr::if_else(is.na(ward_name),
                                        stringr::str_c(geo, labelled, sep= ": "),
                                        stringr::str_c(ward_name, labelled, sep= ": ")),
          obs_label = make_var_labels(obs, cat),
          geo_type = factor(dplyr::case_when(!is.na(ward_name) ~ "ward",
                                      stringr::str_detect(geo, "GB|England|Wales|Scotland") ~ "country",
                                      .default = "LAD"),
                            levels = c("country", "ward", "LAD")),
          alpha = dplyr::if_else(geo_type == "ward", 0.8, 1)
        )



       return(toPlot)
    })

    output$par_coords <- plotly::renderPlotly(
      plotly::ggplotly(
        ggplot2::ggplot(data = plotDat(),
                        ggplot2::aes(y= obs_label, x=scaled, text = labelled_new, group=area, color=geo, alpha=alpha, shape=geo_type)) +
          ggplot2::geom_point() +
          ggplot2::geom_vline(xintercept = 0, colour = "grey", linetype="dotted") +
          scale_colour_sipher(type=c("discrete")) +
          ggplot2::scale_shape_manual(values = c(3, 19, 15)) +
          ggplot2::scale_y_discrete(labels = function(y) stringr::str_wrap(y, width = 25)) +
          ggplot2::annotate("text", x = 1.5, y = 0.5, label = "Higher than average", color="grey") +
          ggplot2::annotate("text", x = -1.5, y = 0.5, label = "Lower than average", color="grey") +
          ggplot2::scale_x_continuous(limits = c(-3, 3)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         #axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = -200), hjust = 0.5, color="grey"),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(vjust = 0.5, hjust = 0),
                         legend.position = "top") +
          ggplot2::labs(color = NULL,
                        alpha = NULL,
                        linetype= NULL,
                        y = NULL,
                        x = NULL,#"Lower than average                    Higher than average",
                        shape = NULL) +
          ggplot2::guides(shape = "none"),
        tooltip = c("text")
      )  |>
      plotly::layout(legend = list(orientation = 'h', x = 0.5, y = 1.05)) |>
      plotly::config(displayModeBar = FALSE)
    )

  })
}

## To be copied in the UI
# mod_pt_ParCoord_ui("pt_ParCoord_1")

## To be copied in the server
# mod_pt_ParCoord_server("pt_ParCoord_1")
