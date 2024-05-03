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

                h1(strong(style="color:#005398", "Welcome to the Synthetic Population Data Explorer")),

                br(),

                div(
                  style = "text-align: center",
                  HTML('<iframe width="514" height="289" src="https://www.youtube.com/embed/CkiORY7GSLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                ),

                br(),

                h4(strong(style="color:#005398","Background")),

                p("The conditions in which we are born, grow, live, work, and age are key drivers of health and health
                                      inequalities. To allow for these complex real-world relationships and interdependencies to be explored,
                                      the SIPHER has developed an innovative systems science approach. This approach offers a powerful toolbox
                                      which enables researchers and policymakers to explore of diverse policies that shape our health and wellbeing."),

                p("One key product of this toolbox is the SIPHER Synthetic Population for Individuals in Great Britain 2019-2021.
                                      As a “digital twin” of the adult population in GB, the SIPHER Synthetic Population supports a wide range of applications
                                      which relay on high-quality individual-level data at a granular spatial resolution.
                                      The SIPHER Synthetic Population is available for full independent use [LINK EXTERNAL via the UK Data Service] "),

                p("This dashboard allows users to explore the SIPHER Synthetic Population without any need for writing code yourself.
                                      Key features of the dashboard include [LINK INTERNAL: to explore areas through a spatial visualisation], and the [LINK INTERNAL:
                                      comparison of areas across different domains].
                                      In addition, this dashboard allows users to [LINK INTERNAL: build their own visualisations for areas and indicators of interest].  "),

                p("Further information about the dashboard can be found in the [LINK INTERNAL: About Section]. Please see [LINK INTERNAL: Citation and Acknowledgements]
                                      for information on the suggested citation."),

                br(),

                h4(strong(style="color:#005398","Data Acknowledgement")),

                p("This dashboard was build using the following datasets:"),

                tags$ul(

                  tags$li("University of Essex, institute for social and economic research. (2022). Understanding society: Waves 1-12, 2009-2021 and harmonised BHPS: Waves 1-18, 1991-2009. [Data collection]. 17th edition. UK Data Service. SN: 6614, http://doi.org/10.5255/UKDA-SN-6614-18."),

                  tags$li("[SIPHER Synthetic Population: exact citation tbd]")

                ),

                br(),

                strong("This dashboard was last updated on 30 April 2024."),

                br(),

                br(),

                div(style = "margin: auto; text-align: center",
                    span(img(src='www/sipher_logo.png', height = "90px", align = "centre"),
                         img(src='www/UKPRP.png', height = "90px", align = "centre"))
                    ),

                br(),

                br(),

                br()

      ),

      mainPanel(width = 3,

                bslib::card(

                  bslib::card_body(

                    p(strong("Interpreting the Results")),

                    h4(style="color:#005398", "Guidance for interpreting the results"),

                    actionButton(ns("gotointerpretation"), "Go to 'Interpreting the Results' tab",
                                 style="color: #fff; background-color: #005398;
                                                          border-color: #005398")
                  )
                ),


                bslib::card(

                  bslib::card_body(

                    p(strong("Dashboard Features and Functionality")),

                    h4(style="color:#005398", "How to use the dashboard's features and functionlity"),

                    actionButton(ns("gotofeatures"), "Go to 'Features and Functionality' tab",
                                 style="color: #fff; background-color: #005398;
                                                          border-color: #005398")
                  )
                ),

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

                bslib::card(

                  bslib::card_body(

                    p(strong("Citing the Dashboard")),

                    h4(style="color:#005398", "How to cite the dashboard and the data"),

                    actionButton(ns("gotocitedashboard"), "Go to 'How to Cite' tab",
                                 style="color: #fff; background-color: #005398;
                                                          border-color: #005398")
                  )
                ),

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

    # observeEvent(list(input$gotocitedashboard,
    #                   input$gotoreproducibility,
    #                   input$gotofeatures,
    #                   input$gotointerpretation),
    #              ignoreInit = T, {
    #   r$active_page <- "info"
    # })

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
    observeEvent(input$gotofeatures,
                 ignoreInit = T, {
                   r$active_page <- "info"
                   r$info_tab <- "features"
                 })

  })
}

## To be copied in the UI
# mod_page_LandingPage_ui("page_LandingPage_1")

## To be copied in the server
# mod_page_LandingPage_server("page_LandingPage_1")
