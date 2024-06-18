#' pt_DemographicsBar UI Function
#'
#' @description generates bar charts showing age and sex breakdown of variables.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param dat data filtered to area of interest
#' @param varbl,categ reactives returning variable and level names
#' @param output_type render plotly to ui (default) or return ggplot object
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
mod_pt_DemographicsBar_server <- function(id, dat, varbl, categ, output_type = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plotDat <- reactive({
      toPlot <- dat() |>
        dplyr::filter(obs%in%varbl(),
               cat%in%categ(),
               age!="all_ages",
               sex!="both")

      return(toPlot)
    })

    facet <- reactive({

      dat <- plotDat()
      mult_areas <- length(unique(dat$area)) > 1
      mult_vars <- length(unique(varbl())) > 1
      mult_cats <- length(categ()) > 1


      out <- dplyr::case_when(
        mult_areas && !mult_cats ~ "wrap_area",
        !mult_areas && mult_vars ~ "wrap_vars",
        !mult_areas && !mult_vars && mult_cats ~ "wrap_cats",
        mult_areas && mult_vars ~ "grid_vars",
        mult_areas && !mult_vars && mult_cats ~ "grid_cats",
        .default = "none"
      )

    })

    plot <- reactive({

        plotDat() |>
        #dplyr::group_by(area) |>
        ggplot2::ggplot(ggplot2::aes(x=age, y=value, fill=as.factor(stringr::str_to_title(sex)), text=labelled)) +
          ggplot2::geom_col(position = ggplot2::position_dodge()) +
          scale_fill_sipher(palette_name = "full", type = "discrete") +
          ggplot2::scale_x_discrete(labels = function(x) translate_codes(x)) +
          ggplot2::theme_bw() +
          ggplot2::labs(fill = "Sex", #stringr::str_to_sentence(var),
                        y= NULL,
                        x= "Age") +
          {if(facet() == "wrap_area")ggplot2::facet_wrap(~geo)} +
          {if(facet() == "wrap_cats")ggplot2::facet_wrap(~translate_codes(cat))} +
          {if(facet() == "wrap_vars")ggplot2::facet_wrap(~varLabels,
                                                         labeller = ggplot2::label_wrap_gen(),
                                                         scales = "free")} +
          {if(facet() == "grid_cats")ggplot2::facet_grid(translate_codes(cat)~geo)} +
          {if(facet() == "grid_vars")ggplot2::facet_grid(varLabels~geo,
                                                         labeller = ggplot2::label_wrap_gen(),
                                                         scales = "free")} +
        {if(facet()!="none")ggplot2::theme(legend.position = "top",
                                         strip.background = ggplot2::element_blank())}

        # {if(facet() == "grid_vars") ggplot2::theme(
        #                                            strip.background = ggplot2::element_blank()
        #                                            # plot.margin = ggplot2::margin(r=25),
        #                                            # strip.text.y = ggplot2::element_text(angle = 0,
        #                                           #                                      #margin = ggplot2::margin(25, 25, 25, 25)
        #                                           # )
        #                                            )}

    })


    fix_legend <- function(plot) {
      if(facet()!="none"){
        plotly::layout(plot,
                       legend=list(y=1.1,
                       x=0.5,
                       xanchor='center',
                       yanchor='bottom',
                       orientation='h'))}
      else plot}

    fix_facet_labs <- function(plot) {
      if(facet()=="grid_vars"){
        plotly::layout(plot,
                       margin = list(r=50))}
      else plot}

    if(is.null(output_type)){
      output$bar <- plotly::renderPlotly(
      plotly::ggplotly(
        plot(),
        tooltip = c("text")
      ) |>
        plotly::config(displayModeBar = FALSE) |>
        fix_legend() |>
        fix_facet_labs()
      )
    }

    if(!is.null(output_type)){
      return(plot())
    }
  })
}

## To be copied in the UI
# mod_pt_DemographicsBar_ui("pt_DemographicsBar_1")

## To be copied in the server
# mod_pt_DemographicsBar_server("pt_DemographicsBar_1")
