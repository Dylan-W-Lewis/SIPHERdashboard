#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  #bslib::bs_themer()

  r <- reactiveValues()

  observeEvent(input$topBar, {
    r$active_page <- input$topBar
  })

  observeEvent(r$active_page, {
     if(!identical(r$active_page, input$topBar)){
       message(paste("change page to", r$active_page, "from", input$topBar))
       bslib::nav_select(id = "topBar",
                       selected = r$active_page)
     }
  })

  output$area_title <- renderText(r$profile)

  #server page 1
  mod_page_MapExplore_server("page_MapExplore", r=r, parent.session = session)

  #server page 2
  mod_pt_RandomGraph_server("pt_RandomGraph_2")
  mod_pt_RandomGraph_server("pt_RandomGraph_3")
  mod_pt_RandomGraph_server("pt_RandomGraph_4")
  mod_pt_RandomGraph_server("pt_RandomGraph_5")
}
