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
      window_title= "SIPHER Synthetic Population",
      title=
        span(
          tags$a(
            tags$img(
              src = "www/sipher_logo_expanded.png",
              style = "width: 135.7px; height: 55px"),
            href = "https://www.gla.ac.uk/research/az/sipher/",
            target = "_blank"
          )),
      inverse=T,
      bg= "#005398",
      theme=bslib::bs_theme(version = 5,
                            #bootswatch = "default",
                            primary = "#005398",
                            secondary = "#005C83",
                            #"navbar-bg" = "#005398",
                            base_font = "Arial",
                            heading_font = "Arial",
                            "navbar-brand-padding-y" = "-1px",
                            "bslib-spacer" = "1rem",
                            "link-decoration" = "none"
                            ) |>
        bslib::bs_add_rules(list(".navbar-static-top {padding: 0px;}",
                                 ".navbar>.container-fluid {padding-left: 0px;}",
                                 ".bslib-card {overflow: visible !important;}",
                                 ".card{overflow: visible !important;}",
                                 ".card-body{overflow: visible !important;}",
                                 ".centred-card {@extend .justify-content-center }",
                                 ".graph-controls .control-label {margin-bottom: .2rem;}",
                                 ".graph-controls .form-group {margin-bottom: .2rem;}",
                                 ".graph-controls.bslib-gap-spacing {gap: .5rem;}",
                                 ".navbar+.container-fluid>.tab-content>.tab-pane.active.html-fill-container:has(>.dont-pad-pls) { padding: 0; }",
                                 ".selectize-input {border: 1px solid #dee2e6 !important;}",
                                 #".tab-content { @extend .html-fill-container !important}",
                                 ".tab-content { display: flex; flex-direction: column; flex-grow: 1; flex-shrink: 1}",
                                 ".fill-tab { display: flex; flex-direction: column; flex-grow: 1; flex-shrink: 1}",
                                 "a.action-button {font-weight: bold !important}"

                                 )),

      #navbar content
      bslib::nav_panel("Home",
                       value = "home",
                       mod_page_LandingPage_ui("page_LandingPage"),
                       ),

      bslib::nav_panel("Map Explore",
               value = "map_explore",
               mod_page_MapExplore_ui("page_MapExplore")),

      bslib::nav_panel("Area Profile",
               value = "area_profile",
               mod_page_AreaProfile2_ui("page_AreaProfile")
               ),

      bslib::nav_panel("Graph Builder",
                      value = "graph_builder",
                      mod_page_BYOG_ui("page_BYOG_1")
                      ),


      bslib::nav_panel("Data Download",
                       value = "data_download",
                       div(style = "margin: auto;",
                            h3(span(icon("hammer"), "Under construction")),
                            p("The data download tab will allow users to filter and download tables of aggregated data from the synthetic population")
                           )
                       ),

      bslib::nav_panel("About", value = "info",
                       mod_page_Info_ui("page_Info")),

      bslib::nav_spacer(),

      bslib::nav_item(tags$a(
        shiny::icon("github"),
        href = "https://github.com/Dylan-W-Lewis/SIPHERdashboard",
        target = "_blank"
      ))
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
