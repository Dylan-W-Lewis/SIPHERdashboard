#' page_LandingPage UI Function
#'
#' @description Home page with links to other tabs.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues object
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

                h5(strong(style="color:#005398", "Purpose of the Dashboard")),

                p("This dashboard allows easy exploration of an aggregated version
                of the SIPHER Synthetic Population without any coding or
                data preparation."),

                p("With our ‘click and explore’ format users can compare areas
                of interest, create bespoke detailed area profiles, develop
                customised data visualisations, and download the aggregated
                data used."),

                h5(strong(style="color:#005398", "Get Started")),

                p("Use the tabs along the top banner of the dashboard
                to explore its various capabilities."),

                p("This dashboard does not contain any individual-level data.
                No conclusions can or  should be made about 'real' individuals.
                All results obtained should be understood and treated as 'model
                outputs'. Data and visualisations can be freely downloaded from
                this dashboard and included in outputs provided that proper
                acknowledgements are given."),


                p("For a detailed overview of the dashboard including guidance,
                interpretation of ", actionLink(ns("gotoAbout"), "About"), "tab."),


                h5(strong(style="color:#005398", "Dashboard Features")),

                fluidRow(
                   column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("map-location-dot"), style="color:#005398"),
                                     p("Compare local authorities across Great Britain using the ",

                                       actionLink(ns("gotoME"), "Map Explore"),
                                       "tool.")
                                 )
                               ),
                               column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("chart-bar"), style="color:#005398"),
                                     p("View a detailed ",
                                       actionLink(ns("gotoAP"), "Area Profile"),
                                       "of any local authority, and explore data for its electoral wards.")

                                 )
                               ),
                               column(
                                 width = 4,
                                 div(style = "text-align: center",
                                     h2(icon("table"), style="color:#005398"),
                                     p("Create custom outputs with the ",
                                       actionLink(ns("gotoGB"), "Graph Builder"),
                                       "or tables with the ",
                                       actionLink(ns("gotoDD"), "Data Download"),
                                       "tool.")
                                 )
                               )

                               ),


                h5(strong(style="color:#005398", "Contact Us")),

                p("For support with the interpretation of results, to provide
                feedback and/or to discuss project ideas and applications
                please direct enquiries marked 'SIPHER Synthetic Population -
                Dashboard' to sipher@glasgow.ac.uk.")


                ),

        bslib::layout_columns(
          col_widths = 12,

                  bslib::card(

                    bslib::card_body(
                      gap = "0.5rem",

                      p(strong("About the dataset")),

                      h5(style="color:#005398", #"How to use the dashboard's features and functionality"),

                         actionLink(ns("gotobackground"), "What is the SIPHER Synthetic Population?", #"Go to 'Features and Functionality' tab",

                         )
                      )
                    )
                  ),


                  bslib::card(

                    bslib::card_body(
                      gap = "0.5rem",

                      p(strong("Interpretation of results")),

                      h5(style="color:#005398", #"Guidance for interpreting the results"),



                      actionLink(ns("gotointerpretation"), "How do I interpret results obtained from the dataset?",# "Go to 'Interpreting the Results' tab",

                                 )
                      )
                    )
                  ),

                  bslib::card(

                    bslib::card_body(
                      gap = "0.5rem",

                      p(strong("Citation guidance")),


                      h5(style="color:#005398", #"How to cite the dashboard and the data"),

                         actionLink(ns("gotocitedashboard"), "How do I cite the dashboard and the underlying data?", #"Go to 'How to Cite' tab",
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
