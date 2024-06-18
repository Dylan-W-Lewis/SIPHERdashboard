#' page_LandingPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_LandingPage_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::page_fillable(

      bslib::layout_columns(
        fillable = FALSE,
        col_widths = bslib::breakpoints(
          lg = c(8,4),
          xl = c(-1, 7, 3, -1),
          xxl = c(-2, 6, 2, -2)
        ),

                bslib::card(h2(strong(style="color:#005398", "SIPHER Synthetic Population Dashboard")),

                            p("This dashboard allows users to easily explore an
                              aggregated version of the SIPHER Synthetic Population
                              for Individuals in Great Britain 2019-2021 - without
                              any coding or data preparation. The ‘click and
                              explore’ format enables users to compare areas of
                              interest, create bespoke detailed area profiles,
                              develop customised data visualisations, and download
                              the aggregate data used."),

                             p("Users can navigate the tabs along the top of the
                               dashboard to explore its various capabilities. Please
                               note that this dashboard does not contain any
                               individual-level data. No conclusions can and
                               should be made about 'real' individuals or Understandig
                               Society survey respondents. All results obtained from
                               this dashboard should be understood and treated
                               as 'model estimates'. For further information about
                               the dataset and the suggested citation visit the",
                               actionLink(ns("gotoAbout"), strong("About", style="color:#005398"), style = "text-decoration: none;"), "tab."),

                             h5(strong(style="color:#005398", "What can the dashboard do?")),

                             fluidRow(
                               column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("map-location-dot"), style="color:#005398"),
                                     p("Compare local authorities across Great Britain using the ",
                                       actionLink(ns("gotoME"), strong("Map Explore", style="color:#005398"), style = "text-decoration: none;"),
                                       "tool.")
                                 )
                               ),
                               column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("chart-bar"), style="color:#005398"),
                                     p("View a detailed ",
                                       actionLink(ns("gotoAP"), strong("Area Profile", style="color:#005398"), style = "text-decoration: none;"),
                                       "of any local authority, and explore data for its electoral wards.")
                                 )
                               ),
                               column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("table"), style="color:#005398"),
                                     p("Create custom outputs with the ",
                                       actionLink(ns("gotoGB"), strong("Graph Builder", style="color:#005398"), style = "text-decoration: none;"),
                                       "or tables with the ",
                                       actionLink(ns("gotoDD"), strong("Data Download", style="color:#005398"), style = "text-decoration: none;"),
                                       "tool.")
                                 )
                               )

                               )


                ),

        bslib::layout_columns(
          col_widths = 12,

                  bslib::card(

                    bslib::card_body(

                      p(strong("About the dataset")),

                      h4(style="color:#005398", #"How to use the dashboard's features and functionlity"),

                         actionLink(ns("gotobackground"), "What is the SIPHER Synthetic Population?", #"Go to 'Features and Functionality' tab",
                                    style = "text-decoration: none;"
                                    # style="color: #fff; background-color: #005398;
                                    #                          border-color: #005398")
                         )
                      )
                    )
                  ),


                  bslib::card(

                    bslib::card_body(

                      p(strong("Interpretation of results")),

                      h4(style="color:#005398", #"Guidance for interpreting the results"),

                      actionLink(ns("gotointerpretation"), "How do I interpret results obtained from the dataset?",# "Go to 'Interpreting the Results' tab",
                                 style = "text-decoration: none;"
                                 # style="color: #fff; background-color: #005398;
                                   #                          border-color: #005398")
                                 )
                      )
                    )
                  ),

                  bslib::card(

                    bslib::card_body(

                      p(strong("Citation guideance")),

                      h4(style="color:#005398", #"How do I cite the dashboard and the data"),

                         actionLink(ns("gotocitedashboard"), "How do I cite the dashboard and the underlying data?", #"Go to 'How to Cite' tab",
                                    style = "text-decoration: none;"
                                    # style="color: #fff; #background-color: #005398;
                                    #                          border-color: #005398"
                         )
                      )
                    )
                  ),
          ) # about column bracket

      ) # layout bracket

    ) # page bracket

  )
}

#' page_LandingPage Server Functions
#'
#' @noRd
mod_page_LandingPage_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #LINKS
    observeEvent(input$gotointerpretation,
                 ignoreInit = T, {
                   r$active_page <- "info"
                   r$info_tab <- "SyntheticPopNotes"
                 })

    observeEvent(input$gotocitedashboard,
                 ignoreInit = T, {
                   r$active_page <- "info"
                   r$info_tab <- "citation"
                 })
    observeEvent(input$gotobackground,
                 ignoreInit = T, {
                   r$active_page <- "info"
                   r$info_tab <- "background"
                 })

    observeEvent(input$gotoAbout,
                 ignoreInit = T, {
                   r$active_page <- "info"
                 })
    observeEvent(input$gotoME,
                 ignoreInit = T, {
                   r$active_page <- "map_explore"
                 })
    observeEvent(input$gotoAP,
                 ignoreInit = T, {
                   r$active_page <- "area_profile"
                 })
    observeEvent(input$gotoGB,
                 ignoreInit = T, {
                   r$active_page <- "graph_builder"
                 })
    observeEvent(input$gotoDD,
                 ignoreInit = T, {
                   r$active_page <- "data_download"
                 })

  })
}

## To be copied in the UI
# mod_page_LandingPage_ui("page_LandingPage_1")

## To be copied in the server
# mod_page_LandingPage_server("page_LandingPage_1")
