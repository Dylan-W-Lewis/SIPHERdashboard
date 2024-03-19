#' pt_ComparisonGraph UI Function
#'
#' @description A shiny Module.
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
    plotOutput(ns("graph"), height="300px", fill = F)

  )
}

#' pt_ComparisonGraph Server Functions
#'
#' @noRd
mod_pt_ComparisonGraph_server <- function(id, r, var, dat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    plotDat <- reactive({
      dat() %>%
        dplyr::filter(
             obs==var) %>%
        dplyr::mutate(cat = factor(cat, levels= get_cats(var))) %>%
        dplyr::group_by(area) #%>%
        # dplyr::arrange(match(cat, get_cats(var)),
        #                .by_group = T)
    })

    output$graph <- renderPlot({
      if(!is.null(r$selected_area)) {
        plotDat() %>%
          ggplot2::ggplot(ggplot2::aes(x=lad_name, y=value, fill=cat)) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          # ggplot2::scale_x_discrete(name = NULL#,
          #                           #labels = ladSF$lad_name[match(dat()$area, ladSF$lad)])
          #                           )+
          scale_fill_sipher(palette_name = "full", type = "discrete") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "top") +
          ggplot2::labs(fill = NULL, #stringr::str_to_sentence(var),
                        y= NULL,
                        x= NULL)
      }

    })
  })
}

## To be copied in the UI
# mod_pt_ComparisonGraph_ui("pt_ComparisonGraph_1")

## To be copied in the server
# mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1")
