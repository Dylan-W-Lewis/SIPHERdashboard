#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # define global reactiveValues
  r <- reactiveValues()

  # set arbitrary mapbox token - this is not used but needs to be in env
  Sys.setenv(MAPBOX_TOKEN = 11122223333444)

  # navigation logic
  observeEvent(input$topBar, {
    r$active_page <- input$topBar
  })

  observeEvent(r$active_page, {
     if(!identical(r$active_page, input$topBar)){
       #message(paste("change page to", r$active_page, "from", input$topBar))
       bslib::nav_select(id = "topBar",
                       selected = r$active_page)
     }
  })

  #server home page
  mod_page_LandingPage_server("page_LandingPage", r=r)

  #server page 1
  mod_page_MapExplore_server("page_MapExplore", r=r, parent.session = session)

  #server page 2
  mod_page_AreaProfile2_server("page_AreaProfile", r=r)

  #server page 3
  #mod_page_GraphBuilder_server("page_GraphBuilder_1")
  mod_pt_BYOG_parcoords_server("pt_BYOG_parcoords_1")

  #server info page
  mod_page_Info_server("page_Info", r=r)

}
