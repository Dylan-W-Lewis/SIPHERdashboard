library(shiny)
library(plotly)
library(crosstalk)

ui <- fluidPage(
  plotlyOutput("plot"),
  verbatimTextOutput("data")
)

server1 <- function(input, output, session) {
  sfShared <- crosstalk::SharedData$new(ladSF, ~LAD23CD, group="Local Authority District")

  output$plot <- renderPlotly({
    sfShared %>%
     # highlight_key(~LAD23NM, group="Local Authority District") %>%
    #SharedData$net(ladSF,
    #               key=~LAD23NM,
    #               group="Local Authority District") %>%
      plot_ly(
            #key=~LAD23CD,
            split = ~LAD23NM,
            color = I("gray90"),
            showlegend = FALSE) %>% #, text = ~LAD23NM, hoveron="fills", hoverinfo="text")
       highlight(
        color="rgba(0,117,176,1)",
        selectize = TRUE,
        persistent = TRUE,
        opacityDim = 0.8#,
        #selected = attrs_selected(opacity = 0.3)
      )
  })
  output$data <- renderPrint({
    sfShared$data(withSelection = TRUE)[,c(1,2,10)]
   # event_data("plotly_click")$key[[1]]
  #  req(input$plot_click)
  })

  observeEvent(event_data("plotly_click"), {
    message(paste0("click!", event_data("plotly_click")$key[[1]]))
  })
}

server2 <- function(input, output, session) {

  output$plot <- renderPlotly({
    ladSF %>%
      plot_ly(
        key=~LAD23CD,
        split = ~LAD23NM,
        fillcolor = I("gray90"),
        showlegend = FALSE,
        text = ~LAD23NM,
        hoveron="fills",
        hoverinfo="text")
  })
  output$data <- renderPrint({
    event_data("plotly_click")$key[[1]]
  })

  observeEvent(event_data("plotly_click"), {
    message(paste0("click!", event_data("plotly_click")$key[[1]]))
  })
}

shinyApp(ui, server1)

