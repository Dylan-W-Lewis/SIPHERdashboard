#' pt_DownloadGraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_DownloadGraph_ui <- function(id){
  ns <- NS(id)
  tagList(
    span(style = "text-align: center;",
         numericInput(ns("plot_width"), "Plot Width (in)", 7, min = 1, max = 50),
         numericInput(ns("plot_height"), "Plot Height (in)", 5, min = 1, max = 50)
    ),
    div(style = "text-align: center;",
        downloadButton(ns("download"), "Download", class = "btn-success btn-s"),
        )

  )
}

#' pt_DownloadGraph Server Functions
#'
#' @noRd
mod_pt_DownloadGraph_server <- function(id, plot){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    if(!is.reactive(plot)){
      warning("Plot should be provided as a reactive")
    }

    caption <- "This graph was produced using synthetic data from the SIPHER Synthetic Population for Individuals in Great Britain 2019-2021."

    outPlot <- reactive(plot() +
                          ggplot2::labs(caption = stringr::str_wrap(caption, input$"plot_width" * 18)) +
                          ggplot2::theme(plot.caption.position = "plot",
                                         plot.caption = ggplot2::element_text(hjust = 1, colour = "grey30"))
                        )

    output$download <- downloadHandler(
      filename = function() {
        paste0("SIPHER_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        outPlot(),
                        width = input$plot_width,
                        height = input$plot_height,
                        bg = "white",
                        units = "in"

        )
      }
    )

  })
}

## To be copied in the UI
# mod_pt_DownloadGraph_ui("pt_DownloadGraph_1")

## To be copied in the server
# mod_pt_DownloadGraph_server("pt_DownloadGraph_1")
