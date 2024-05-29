#' pt_ComparisonGraph UI Function
#'
#' @description compare variables across local authorities.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues to get selected area ids
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr filter group_by
#' @importFrom ggplot2 ggplot aes geom_col coord_flip theme labs scale_x_discrete
#' @importFrom stringr str_to_sentence
mod_pt_ComparisonGraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    #selectInput(ns("var"), label = "", choices = unique(laDat$obs)),
    plotly::plotlyOutput(ns("graph"), height="auto", fill = F)

  )
}

#' pt_ComparisonGraph Server Functions
#'
#' @noRd
mod_pt_ComparisonGraph_server <- function(id, r, var, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    plotDat <- reactive({
      dat() |>
        dplyr::filter(
             obs==var) |>
        dplyr::mutate(cat = factor(translate_codes(cat), levels= translate_codes(get_cats(var))),
                      new_labs = stringr::str_c(cat, labelled, sep = ": ")) |>
        dplyr::group_by(area)
    })

    # function to reverse legend order
    reverse_legend_labels <- function(plotly_plot) {
      n_labels <- length(plotly_plot$x$data)
      plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
      plotly_plot
    }

    # create plotly
    graph <- reactive({
      plotly::ggplotly(
        plotDat() |>
          ggplot2::ggplot(ggplot2::aes(x=stringr::str_wrap(lad_name, width=15),
                                       y=value,
                                       fill=cat,
                                       text = new_labs)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          scale_fill_sipher(palette_name = "full", type = "discrete") +
          ggplot2::theme_bw() +
          #ggplot2::theme(legend.position = "top") +
          ggplot2::labs(fill = NULL,
                        y= NULL,
                        x= NULL) +
          ggplot2::ggtitle(translate_codes(var)),
         tooltip = c("text")) |>
        reverse_legend_labels()
    })

    output$graph <- plotly::renderPlotly({
      if(!is.null(r$selected_area)) {
        graph() |>
          plotly::layout(legend=list(y=-0.1,
                                     x=0.5,
                                     xanchor='center',
                                     yanchor='top',
                                     orientation='h',
                                     itemclick="toggleothers")#,
                         # margin=list(t=20,
                         #             autoexpand=F)
                         ) |>
        plotly::config(displayModeBar = FALSE)
      }

    })
  })
}

## To be copied in the UI
# mod_pt_ComparisonGraph_ui("pt_ComparisonGraph_1")

## To be copied in the server
# mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1")
