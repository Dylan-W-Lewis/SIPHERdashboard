#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      id = "topBar",
      "SIPHER Synthetic Population",
      # tabPanel("Map Explore",
      #          value = "map_explore",
      #          fluidRow(column(6,
      #                          fluidRow(
      #                            mod_pt_AreaSelections_ui("pt_AreaSelections_1")),
      #                          fluidRow(
      #                            mod_pt_RandomGraph_ui("pt_RandomGraph_1")),
      #                          ),
      #                   column(6,
      #                          mod_pt_MapSelect_ui("pt_MapSelect_1")))
      # ),
      tabPanel("Map Explore",
               value = "map_explore",
               bslib::page_fillable(bslib::layout_columns(
                 col_widths =  c(6,6),
                 bslib::layout_columns(
                   row_heights = "auto",
                   col_widths = 12,
                   #row_heights = c(1, 3),
                   bslib::card(
                     bslib::card_header("Selected areas"),
                     mod_pt_AreaSelections_ui("pt_AreaSelections_1")),
                   bslib::card(
                     mod_pt_RandomGraph_ui("pt_RandomGraph_1"))
                 ),
                 mod_pt_MapSelect2_ui("pt_MapSelect_1")
                 #bslib::card(mod_pt_MapSelect2_ui("pt_MapSelect_1")),
                 #            height = "600px", fill=F)
               ))
      ),
      tabPanel("Area profile",
               value = "area_profile",
               h3("Area profile"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SIPHERdashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
