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

    fluidRow(

      mainPanel(width = 1), # White space

      mainPanel(width = 7,

                h2(strong(style="color:#005398", "SIPHER Synthetic Population Dashboard")),

                p("Welcome to SIPHER’s Synthetic Population Dashboard. The SIPHER team have developed this dashboard to allow users to quickly explore an aggregated version of the Synthetic Population for Individuals in Great Britain 2019-2021 without the need to write code or prepare any data. The dashboard provides a ‘click and explore’ experience for a select set of domains enabling users to compare areas of interest, create bespoke detailed area profiles, develop custom data visualisations, and download the aggregate data used."),

                p("To get started use the tabs along the top of the dashboard to explore the different capabilities of the dashboard. More information can be found in the ‘About’ tab"),

                h5(strong(style="color:#005398", "What can the dashboard do?")),

                br(),

                fluidRow(
                  column(
                    width = 4,
                    div(style = "text-align: center",
                        h2(icon("map-location-dot"), style="color:#005398"),
                        p("Compare local authorities across Great Britain using the ", strong("Map Explore", style="color:#005398"), "tool")
                        )
                  ),
                  column(
                    width = 4,
                    div(style = "text-align: center",
                        h2(icon("chart-bar"), style="color:#005398"),
                        p("View a detailed ", strong("Area Profile", style="color:#005398"), "of any local authority, and explore data for lower-level areas")
                        )
                    ),
                    column(
                      width = 4,
                      div(style = "text-align: center",
                          h2(icon("table"), style="color:#005398"),
                          p("Create custom graphs with the ", strong("Graph Builder", style="color:#005398"), "or tables with the ", strong("Data Download", style="color:#005398"), "tool")
                      )
                    )
                ),

                # div(
                #   style = "text-align: center",
                #   HTML('<iframe width="514" height="289" src="https://www.youtube.com/embed/CkiORY7GSLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                # ),

                # br(),
                #
                # h4(strong(style="color:#005398","Background")),
                #
                # p("The conditions in which we are born, grow, live, work, and age are key drivers of health and health
                #                       inequalities. To allow for these complex real-world relationships and interdependencies to be explored,
                #                       the SIPHER has developed an innovative systems science approach. This approach offers a powerful toolbox
                #                       which enables researchers and policymakers to explore of diverse policies that shape our health and wellbeing."),
                #
                # p("One key product of this toolbox is the SIPHER Synthetic Population for Individuals in Great Britain 2019-2021.
                #                       As a “digital twin” of the adult population in GB, the SIPHER Synthetic Population supports a wide range of applications
                #                       which relay on high-quality individual-level data at a granular spatial resolution.
                #                       The SIPHER Synthetic Population is available for full independent use [LINK EXTERNAL via the UK Data Service] "),
                #
                # p("This dashboard allows users to explore the SIPHER Synthetic Population without any need for writing code yourself.
                #                       Key features of the dashboard include [LINK INTERNAL: to explore areas through a spatial visualisation], and the [LINK INTERNAL:
                #                       comparison of areas across different domains].
                #                       In addition, this dashboard allows users to [LINK INTERNAL: build their own visualisations for areas and indicators of interest].  "),
                #
                # p("Further information about the dashboard can be found in the [LINK INTERNAL: About Section]. Please see [LINK INTERNAL: Citation and Acknowledgements]
                #                       for information on the suggested citation."),
                #
                # br(),
                #
                # h4(strong(style="color:#005398","Data Acknowledgement")),
                #
                # p("This dashboard was build using the following datasets:"),
                #
                # tags$ul(
                #
                #   tags$li("University of Essex, institute for social and economic research. (2022). Understanding society: Waves 1-12, 2009-2021 and harmonised BHPS: Waves 1-18, 1991-2009. [Data collection]. 17th edition. UK Data Service. SN: 6614, http://doi.org/10.5255/UKDA-SN-6614-18."),
                #
                #   tags$li("[SIPHER Synthetic Population: exact citation tbd]")
                #
                # ),
                #
                # br(),
                #
                # strong("This dashboard was last updated on 10 May 2024."),
                #
                # br(),
                #
                # br(),

                # div(style = "margin: auto; text-align: center",
                #     span(img(src='www/sipher_logo.png', height = "90px", align = "centre"),
                #          img(src='www/UKPRP.png', height = "90px", align = "centre"))
                #     ),

                # br(),
                #
                # br(),
                #
                # br()

      ),

      mainPanel(width = 3,

                bslib::card(

                  bslib::card_body(

                    p(strong("About the dataset")),

                    h4(style="color:#005398", #"How to use the dashboard's features and functionlity"),

                       actionLink(ns("gotobackground"), "What is the Synthetic Population?", #"Go to 'Features and Functionality' tab",
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

                    actionLink(ns("gotointerpretation"), "How to interpret synthetic data",# "Go to 'Interpreting the Results' tab",
                               style = "text-decoration: none;"
                               # style="color: #fff; background-color: #005398;
                                 #                          border-color: #005398")
                               )
                    )
                  )
                ),

                bslib::card(

                  bslib::card_body(

                    p(strong("Citation information")),

                    h4(style="color:#005398", #"How to cite the dashboard and the data"),

                       actionLink(ns("gotocitedashboard"), "How to cite the dashboard and data", #"Go to 'How to Cite' tab",
                                  style = "text-decoration: none;"
                                  # style="color: #fff; #background-color: #005398;
                                  #                          border-color: #005398"
                       )
                    )
                  )
                ),


                # bslib::card(
                #
                #   bslib::card_body(
                #
                #     p(strong("Features and functionality")),
                #
                #     h4(style="color:#005398", #"How to use the dashboard's features and functionlity"),
                #
                #     actionLink(ns("gotofeatures"), "How to use the dashboard", #"Go to 'Features and Functionality' tab",
                #                style = "text-decoration: none;"
                #                # style="color: #fff; background-color: #005398;
                #                  #                          border-color: #005398")
                #                  )
                #     )
                #   )
                # ),

                # bslib::card(
                #
                #   bslib::card_body(
                #
                #     p(strong("Reproducing our Approach")),
                #
                #     h4(style="color:#005398", "Access our reproducibility pack"),
                #
                #     actionButton(ns("gotoreproducibility"), "Go to 'Reproducibility Pack' tab",
                #                  style="color: #fff; background-color: #005398;
                #                                           border-color: #005398")
                #   )
                # ),


      ), #sidebar bracket

      mainPanel(width = 1) # White space

    )

  )
}

#' page_LandingPage Server Functions
#'
#' @noRd
mod_page_LandingPage_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

  })
}

## To be copied in the UI
# mod_page_LandingPage_ui("page_LandingPage_1")

## To be copied in the server
# mod_page_LandingPage_server("page_LandingPage_1")
