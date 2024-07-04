#' page_AreaProfile2 UI Function
#'
#' @description Area profile page, mostly ui to host various instances of mod_pt_Profile.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues object
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_AreaProfile2_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
                         bslib::layout_columns(
                           gap = 0,
                           col_widths = bslib::breakpoints(
                             lg = c(-1,10,-1),
                             xl = c(-2, 8, -2),
                             xxl = c(-3, 6, -3)),
                           bslib::layout_columns(
                             col_widths = 12,
                             fill = F,
                             gap=0,
                             em("Area profile"),
                             h2(strong(style="color:#005398", textOutput(ns("area_title"), inline = T)),
                               bslib::popover(
                                 icon("caret-down"),
                                 placement = "bottom",
                                 title = "Selected area",
                                 selectInput(ns("change_area"),
                                             label = NULL,
                                             choices = setNames(c("",ladSF$lad), c("Change selected area...",ladSF$lad_name)))
                               )
                                ),
                             #actionButton(ns("switch_area"), label = NULL, icon = icon("repeat"), width="16px"),
                             bslib::navset_bar(
                               inverse = T,
                               bg = "#005398",
                               title= strong("Choose a topic:"),
                               position = "fixed-bottom",
                               padding = c("0px","0px","70px"),
                               bslib::nav_spacer(),
                               bslib::nav_panel(strong("Health & wellbeing"),
                                                mod_pt_profile_ui(ns("health"),
                                                                  vars=domain_vars("health"))
                                                ),
                               bslib::nav_panel(strong("Income & employment"),
                                                mod_pt_profile_ui(ns("income"),
                                                                   vars=domain_vars("income"))
                                                ),
                               bslib::nav_panel(strong("Housing & households"),
                                                mod_pt_profile_ui(ns("housing"),
                                                                  vars=domain_vars("housing"))
                                                ),
                               bslib::nav_panel(strong("Lifestyle, diet & nutrition"),
                                                mod_pt_profile_ui(ns("lifestyle"),
                                                                  vars=domain_vars("lifestyle"))
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

    observeEvent(input$change_area, {
      if(input$change_area != ""){
        r$profile <- input$change_area
      }
    })

    output$area_title <- renderText(unique(ladSF$lad_name[ladSF$lad==r$profile]))

    mod_pt_profile_server("health", r=r,
                          vars = domain_vars("health"),
                          # vars=c("sf12pcs_dv",
                          #        "sf12mcs_dv",
                          #        "scghq1_dv",
                          #        "scsf2a",
                          #        "sclonely",
                          #        "sclfsato"),
                                 topic="health and wellbeing")
    mod_pt_profile_server("income", r=r,
                          vars = domain_vars("income"),
                          # vars=c("sclfsat2",
                          #        "fimnlabnet_dv",
                          #        "benbase4"),
                          topic="income and employment")
    mod_pt_profile_server("lifestyle", r=r,
                          vars = domain_vars("lifestyle"),
                          # vars=c("auditc3",
                          #        "ecigs1",
                          #        "wkvege"),
                      topic="lifestyle and diet")
    mod_pt_profile_server("housing", r=r,
                          vars = domain_vars("housing"),
                          # vars=c("houscost1_dv",
                          #        "hheat",
                          #        "heatch",
                          #        "aidhh",
                          #        "ccare"),
                      topic="housing and households")


  })
}

## To be copied in the UI
# mod_page_AreaProfile2_ui("page_AreaProfile2_1")

## To be copied in the server
# mod_page_AreaProfile2_server("page_AreaProfile2_1")
