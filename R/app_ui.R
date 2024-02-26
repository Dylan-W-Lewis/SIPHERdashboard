#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      #page setup
      id = "topBar",
      title="SIPHER Synthetic Population",
      inverse=TRUE,
      bg= "#005398",
      #theme=bslib::bs_theme(version = 5, preset = "bootstrap", "navbar-bg" = "#005398"),

      #navbar content
      bslib::nav_panel("Map Explore",
               value = "map_explore",
               bslib::page_fillable(
                 bslib::layout_columns(
                   col_widths =  c(6,6),
                   bslib::layout_columns(
                     row_heights = c("auto",1),
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
                   )
                 )
      ),
      bslib::nav_panel("Area profile",
               value = "area_profile",
               h3("Area profile"),
               h2(textOutput("area_title")),
               bslib::page_fillable(
                 bslib::layout_columns(
                 col_widths =  c(6,6),
                 bslib::card(mod_pt_RandomGraph_ui("pt_RandomGraph_2")),
                 bslib::card(mod_pt_RandomGraph_ui("pt_RandomGraph_3")),
                 bslib::card(mod_pt_RandomGraph_ui("pt_RandomGraph_4")),
                 bslib::card(mod_pt_RandomGraph_ui("pt_RandomGraph_5")),
               ))
      )
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
