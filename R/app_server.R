#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_pt_RandomGraph_server("pt_RandomGraph_1")
  mod_pt_MapSelect_server("pt_MapSelect_1", r = r)
}
