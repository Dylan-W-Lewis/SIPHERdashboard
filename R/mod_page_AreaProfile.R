#' page_AreaProfile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_AreaProfile_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_columns(
        col_widths =  c(6,6),
        bslib::layout_columns(
          col_widths = 12,
          row_heights = "auto",
          h4("Area profile"),
          h2(textOutput(ns("area_title"))),
          bslib::card(
            mod_pt_AreaMap_ui(ns("pt_AreaMap_1"))
          )
          ),
        bslib::layout_columns(
          col_widths = 12,
          bslib::card(mod_pt_RandomGraph_ui(ns("RandomGraph"))),
          bslib::card(mod_pt_RandomGraph_ui(ns("RandomGraph_2")))
          )
      )
    )

  )
}

#' page_AreaProfile Server Functions
#'
#' @noRd
mod_page_AreaProfile_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$area_title <- renderText(unique(lookup_wd_lad$LAD23NM[lookup_wd_lad$LAD23CD==r$profile]))

    mod_pt_RandomGraph_server("RandomGraph")
    mod_pt_RandomGraph_server("RandomGraph_2")
    mod_pt_AreaMap_server("pt_AreaMap_1", r=r)

  })
}

## To be copied in the UI
# mod_page_AreaProfile_ui("page_AreaProfile_1")

## To be copied in the server
# mod_page_AreaProfile_server("page_AreaProfile_1")
