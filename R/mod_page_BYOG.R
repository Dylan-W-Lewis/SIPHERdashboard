#' page_BYOG UI Function
#'
#' @description Landing page for Graph Builder tool, giving an overview of each type of graph.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_BYOG_ui <- function(id){
  ns <- NS(id)
  bslib::page_fillable(
    bslib::as_fill_carrier(bslib::navset_hidden(
      id="pages",
      bslib::nav_panel_hidden("home",
                       bslib::layout_columns(
                         col_widths = bslib::breakpoints(
                           lg = c(-1,10,-1),
                           xl = c(-2, 8, -2),
                           xxl = c(-3, 6, -3)),
                         h2(strong("Graph Builder"), style="color:#005398"),
                         p("This tool allows the design and download of tailored
                           versions of the graphs from other sections of the dashboard,
                           without the need to write any code. Three types of graphs
                           are available catering to a wide range of applications:"),
                         bslib::layout_columns(
                           col_widths = c(4,4,4),
                           bslib::card(
                             div(style = "text-align: center",
                                 h2(icon("chart-gantt"), style="color:#005398"),
                                 actionLink( ns("to_dots"), "Parallel Dots"),
                                 p("Compare local authorities and electoral wards to the national average across multiple variables.")
                             )
                           ),
                           bslib::card(
                             div(style = "text-align: center",
                                 h2(icon("bars-progress"), style="color:#005398"),
                                 actionLink( ns("to_bars"), "Stacked Bars"),
                                 p("Examine local authorities across all categories of a single variable, with options to filter by age and sex.")
                             )),
                           bslib::card(
                             div(style = "text-align: center",
                                 h2(icon("chart-simple"), style="color:#005398"),
                                 actionLink( ns("to_cols"), "Demographic Columns"),
                                 p("Explore the age and sex distribution of variables in one or more local authority area.")
                             ))
                         )
                       )),
      bslib::nav_panel_hidden("bar",
                              actionLink(ns("back1"), "Back", icon = icon("square-caret-left")),
                              mod_pt_BYOG_StackedBar_ui(ns("stacked_bar"))
                              )  |> tagAppendAttributes( class = 'fill-tab' ),
      bslib::nav_panel_hidden("dot",
                              actionLink(ns("back2"), "Back", icon = icon("square-caret-left")),
                              mod_pt_BYOG_parcoords_ui(ns("par_coord"))
                              )  |> tagAppendAttributes( class = 'fill-tab' ),
      bslib::nav_panel_hidden("col",
                              actionLink(ns("back3"), "Back", icon = icon("square-caret-left")),
                              mod_pt_BYOG_dembar_ui(ns("dem_bar"))
                              )  |> tagAppendAttributes( class = 'fill-tab' )
    )
  ))
}

#' page_BYOG Server Functions
#'
#' @noRd
mod_page_BYOG_server <- function(id, parentSession){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_pt_BYOG_StackedBar_server("pt_BYOG_StackedBar_1")
    mod_pt_BYOG_parcoords_server("par_coord")
    mod_pt_BYOG_dembar_server("dem_bar")


    observeEvent(input$to_bars, {
      bslib::nav_select(id = "pages", selected = "bar", session = parentSession)
    })

    observeEvent(input$to_dots, {
      bslib::nav_select(id = "pages", selected = "dot", session = parentSession)
    })

    observeEvent(input$to_cols, {
      bslib::nav_select(id = "pages", selected = "col", session = parentSession)
    })

    observeEvent({
      input$back1
      input$back2
      input$back3
      },
      bslib::nav_select(id = "pages", selected = "home", session = parentSession))

  })
}

## To be copied in the UI
# mod_page_BYOG_ui("page_BYOG_1")

## To be copied in the server
# mod_page_BYOG_server("page_BYOG_1")
