#' pt_profile_poverty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_profile_poverty_ui <- function(id){
  ns <- NS(id)
  tagList(
    p(textOutput(ns("intro_text"))),
    bslib::card(
      bslib::card_title("Income by area"),
      bslib::card_body(radioButtons(ns("income_map_choices"),
                                    "",
                                    choiceValues=c("total household net income - no deductions (mean)",
                                              "basic pay hourly rate (mean)"),
                                    choiceNames = c("Average houshold income",
                                                    "Average hourly wage"),
                                    inline = T
                                    )),
      bslib::card_body(mod_pt_AreaMap3_ui(ns("pt_AreaMap3_1")),
                       min_height = 150)),
    bslib::value_box("Compared to other ares", "2nd"),
    p(textOutput(ns("more_text"))),

  )
}

#' pt_profile_poverty Server Functions
#'
#' @noRd
mod_pt_profile_poverty_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$intro_text <- renderText(shinipsum::random_text(nwords = 50))

    mod_pt_AreaMap3_server("pt_AreaMap3_1", r=r,
                           varbl = reactive(input$income_map_choices),
                           reactive(get_cats(input$income_map_choices)[1])
                           )

  })
}

## To be copied in the UI
# mod_pt_profile_poverty_ui("pt_profile_poverty_1")

## To be copied in the server
# mod_pt_profile_poverty_server("pt_profile_poverty_1")
