#' pt_ComparisonGraph UI Function
#'
#' @description compare variables across local authorities.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr filter group_by
#' @importFrom ggplot2 ggplot aes geom_col coord_flip theme labs scale_x_discrete
#' @importFrom stringr str_to_sentence
mod_pt_ComparisonGraph_ui <- function(id, fill=NULL){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("graph"), height="auto", fill = isTRUE(fill))

  )
}

#' pt_ComparisonGraph Server Functions
#'
#' @noRd
mod_pt_ComparisonGraph_server <- function(id, var, dat, subtitle = NULL, ggoutput = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    var_reactive <- if (is.reactive(var)) var else reactive({ var })
    subtitle_reactive <- if (is.reactive(subtitle)) subtitle else reactive({ subtitle })

    plotDat <- reactive({

      req(nrow(dat())>0)

      variable <- var_reactive()

      dat() |>
        dplyr::filter(
             obs==variable) |>
        dplyr::mutate(cat = factor(translate_codes(cat), levels= translate_codes(get_cats(variable))),
                      new_labs = stringr::str_c(cat, labelled, sep = ": ")) |>
        dplyr::group_by(area)
    })

    # function to reverse legend order
    reverse_legend_labels <- function(plotly_plot) {
      n_labels <- length(plotly_plot$x$data)
      plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
      plotly_plot
    }

    # create ggplot
    graph <- reactive({
      plotDat() |>
        ggplot2::ggplot(ggplot2::aes(x=stringr::str_wrap(lad_name, width=15),
                                     y=value,
                                     fill=cat,
                                     text = new_labs)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        scale_fill_sipher(palette_name = "full", type = "discrete") +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(fill = NULL,
                      y= NULL,
                      x= NULL)
    })

    if(!isTRUE(ggoutput)){

      output$graph <- plotly::renderPlotly({
        variable <- var_reactive()
        sub <- subtitle_reactive()
        plotly::ggplotly(
          graph(),
          tooltip = c("text")) |>
          reverse_legend_labels() |>
          plotly::layout(legend=list(y=-0.1,
                                     x=0.5,
                                     xanchor='center',
                                     yanchor='top',
                                     orientation='h',
                                     itemclick="toggleothers",
                                     tracegroupgap= 0),
                         title = list(text = paste0(translate_codes(variable),
                                                    '<br>',
                                                    '<sup>',
                                                    sub,
                                                    '</sup>')),
                         margin=list(t=40)
          ) |>
          plotly::config(displayModeBar = FALSE)
        })
    }

    if(isTRUE(ggoutput)){
      return(isolate(graph() +
               ggplot2::ggtitle(translate_codes(var_reactive()), subtitle = subtitle_reactive())))
    }

  })
}

## To be copied in the UI
# mod_pt_ComparisonGraph_ui("pt_ComparisonGraph_1")

## To be copied in the server
# mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1")
