#' page_AreaProfile2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_AreaProfile2_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
                         bslib::layout_columns(
                           col_widths = c(-2,8,-2),
                           bslib::layout_columns(
                             col_widths = 12,
                             fill = F,
                             em("Area profile"),
                             h2(textOutput(ns("area_title"))),
                             bslib::navset_bar(position = "fixed-bottom",
                                               bslib::nav_spacer(),
                                               bslib::nav_panel("Poverty",
                                                 mod_pt_VarLevelSelect_ui(ns("pt_VarLevelSelect_1")),
                                                 p(textOutput(ns("intro_text"))),
                                                 bslib::card(
                                                   bslib::card_body(plotly::plotlyOutput(ns("plotly")),
                                                                    min_height = 150)),
                                                 bslib::value_box("Compared to other ares", "2nd"),
                                                 p(textOutput(ns("more_text"))),
                                                 bslib::card(
                                                   bslib::card_body(mod_pt_AreaMap2_ui(ns("pt_AreaMap_1")),
                                                                    min_height = 150)),
                                                 bslib::card(mod_pt_RandomGraph_ui(ns("RandomGraph"))),

                                                 ),
                                               bslib::nav_panel("Loneliness"),
                                               bslib::nav_spacer()
                             )
                           )

                    )

    )
  )
}

#' page_AreaProfile2 Server Functions
#'
#' @noRd
mod_page_AreaProfile2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$area_title <- renderText(unique(lookup_wd_lad$lad_name[lookup_wd_lad$lad==r$profile]))

    output$intro_text <- renderText(shinipsum::random_text(nwords = 50))

    output$more_text <- renderText(shinipsum::random_text(nwords = 30))

    output$plotly <- plotly::renderPlotly(shinipsum::random_ggplotly())

    mod_pt_AreaMap2_server("pt_AreaMap_1", r=r)
    mod_pt_VarLevelSelect_server("pt_VarLevelSelect_1", r=r)

    mod_pt_RandomGraph_server("RandomGraph")


  })
}

## To be copied in the UI
# mod_page_AreaProfile2_ui("page_AreaProfile2_1")

## To be copied in the server
# mod_page_AreaProfile2_server("page_AreaProfile2_1")
