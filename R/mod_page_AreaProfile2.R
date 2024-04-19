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
                                                                mod_pt_profile_ui(ns("health"),
                                                                                  vars=c("sf12pcs_dv",
                                                                                         "sf12mcs_dv",
                                                                                         "scghq1_dv",
                                                                                         "scsf2a",
                                                                                         "sclonely"),
                                                                                  varNames=c("physical health (SF-12)",
                                                                                             "mental health (SF-12)",
                                                                                             "psychological distress (GHQ)",
                                                                                             "scsf2a",
                                                                                             "sclonely"),
                                                                                  topic="health and wellbeing")
                                                                ),
                                               bslib::nav_panel("Income & employment",
                                                                mod_pt_profile_ui(ns("income"),
                                                                                   vars=c(#"total household net income - no deductions (mean)",
                                                                                          "basrate",
                                                                                          "sclfsat2",
                                                                                          "benbase4"),
                                                                                   varNames=c(#"average household income",
                                                                                              "average hourly wage",
                                                                                              "sclfsat2",
                                                                                              "benbase4"),
                                                                                   topic="income and employment")
                                                                ),
                                               bslib::nav_panel("Housing & Households",
                                                                mod_pt_profile_ui(ns("housing"),
                                                                                  vars=c("houscost1_dv",
                                                                                         "hheat",
                                                                                         "heatch",
                                                                                         "aidhh"),
                                                                                  varNames=c("average monthly housing cost",
                                                                                             "able to keep accomodation warm enough",
                                                                                             "heatch",
                                                                                             "aidhh"),
                                                                                  topic="housing and households")
                                                                ),
                                               bslib::nav_panel("Lifestyle, Diet & Nutrition",
                                                                mod_pt_profile_ui(ns("lifestyle"),
                                                                                  vars=c("auditc3",
                                                                                         "ecigs1",
                                                                                         "wkvege"),
                                                                                  varNames=c("alcohol consumption",
                                                                                             "ecigs1",
                                                                                             "frequency of eating vegetables"),
                                                                                  topic="lifestyle and diet")
                                                                ),
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


    mod_pt_profile_server("health", r=r,
                                 vars=c("sf12pcs_dv",
                                        "sf12mcs_dv",
                                        "scghq1_dv",
                                        "scsf2a",
                                        "sclonely"),
                                 varNames=c("physical health (SF-12)",
                                            "mental health (SF-12)",
                                            "psychological distress (GHQ)",
                                            "scsf2a",
                                            "sclonely"),
                                 topic="health and wellbeing")
    mod_pt_profile_server("income", r=r,
                          vars=c(#"total household net income - no deductions (mean)",
                                 "basrate",
                                 "sclfsat2",
                                 "benbase4"),
                          varNames=c(#"average houshold income",
                                     "average hourly wage",
                                     "sclfsat2",
                                     "benbase4"),
                          topic="income and employment")
    mod_pt_profile_server("lifestyle", r=r,
                      vars=c("auditc3",
                             "ecigs1",
                             "wkvege"),
                      varNames=c("alcohol consumption",
                                 "ecigs1",
                                 "frequency of eating vegetables"),
                      topic="lifestyle and diet")
    mod_pt_profile_server("housing", r=r,
                      vars=c("houscost1_dv",
                             "hheat",
                             "heatch",
                             "aidhh"),
                      varNames=c("average monthly housing cost",
                                 "able to keep accomodation warm enough",
                                 "heatch",
                                 "aidhh"),
                      topic="housing and households")


  })
}

## To be copied in the UI
# mod_page_AreaProfile2_ui("page_AreaProfile2_1")

## To be copied in the server
# mod_page_AreaProfile2_server("page_AreaProfile2_1")
