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
      title= "SIPHER Synthetic Population",
        # div(tags$img(
        # src = "www/sipher_logo.png",
        # style = "width: 135.7px; height: 55px"
        # ),
        # style= "padding: 0px 0px; line-height: 55px; vertical-align: middle;",
        # "Synthetic Population Dashboard"),
      inverse=TRUE,
      bg= "#005398",
      #theme=bslib::bs_theme(version = 5, preset = "bootstrap", "navbar-bg" = "#005398"),

      #navbar content
      bslib::nav_panel("Home",
                       value = "home",
                       div(style = "margin: auto;",
                           tags$img(
                             src = "www/sipher_logo.png",
                             height = 143,
                             width = 353
                             )
                           ),
                       p("Home page coming soon")),

      bslib::nav_panel("Map Explore",
               value = "map_explore",
               mod_page_MapExplore_ui("page_MapExplore")),

      bslib::nav_panel("Area profile",
               value = "area_profile",
               mod_page_AreaProfile2_ui("page_AreaProfile")
               ),

      bslib::nav_panel("Graph builder",
                       value = "graph_builder"),

      bslib::nav_panel("Data download",
                       value = "data_download"),

      bslib::nav_spacer(),

      bslib::nav_panel("", value = "options", icon = shiny::icon("gear"))
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
      app_title = "SIPHER Synthetic Population dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
