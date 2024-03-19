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
                             gap=0,
                             em("Area profile"),
                             h2(textOutput(ns("area_title"), inline = T),
                                     bslib::popover(
                                       icon("caret-down"),
                                       placement = "bottom",
                                       title = "Change selected area",
                                       "..."
                                     ),
                                ),
                             #actionButton(ns("switch_area"), label = NULL, icon = icon("repeat"), width="16px"),
                             bslib::navset_bar(position = "fixed-bottom",
                                               padding = c("0px","0px","70px"),
                                               bslib::nav_spacer(),
                                               bslib::nav_panel("Health & wellbeing",
                                                                mod_pt_profile_health_ui(ns("health"))),
                                               bslib::nav_panel("Income & Employment",
                                                                mod_pt_profile_poverty_ui(ns("income"))
                                                                ),
                                               bslib::nav_panel("Housing"),
                                               bslib::nav_panel("Deprivation"),
                                               bslib::nav_panel("Lifestyle, Diet & Nutrition"),
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

    observeEvent(r$profile, ignoreNULL = F, {
      if(is.null(r$profile)){
        r$profile <- "S12000049"
      }
    })

    output$area_title <- renderText(unique(ladSF$lad_name[ladSF$lad==r$profile]))

    output$intro_text <- renderText(shinipsum::random_text(nwords = 50))

    output$more_text <- renderText(shinipsum::random_text(nwords = 30))

    output$plotly <- plotly::renderPlotly(shinipsum::random_ggplotly())


    mod_pt_profile_poverty_server("income", r=r)
    mod_pt_profile_health_server("health", r=r)


  })
}

## To be copied in the UI
# mod_page_AreaProfile2_ui("page_AreaProfile2_1")

## To be copied in the server
# mod_page_AreaProfile2_server("page_AreaProfile2_1")
