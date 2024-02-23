library(shiny)
library(plotly)
library(crosstalk)

ui <- fluidPage(
  selectizeInput("inp",
                 label = "",
                 choices=c("Choose an area" = "", set_names(ladSF$LAD23CD,ladSF$LAD23NM)),
                           multiple=TRUE),
  plotlyOutput("plot"),
  verbatimTextOutput("data")
)

# server1 <- function(input, output, session) {
#   sfShared <- crosstalk::SharedData$new(ladSF, ~LAD23CD, group="Local Authority District")
#
#   output$plot <- renderPlotly({
#     sfShared %>%
#      # highlight_key(~LAD23NM, group="Local Authority District") %>%
#     #SharedData$net(ladSF,
#     #               key=~LAD23NM,
#     #               group="Local Authority District") %>%
#       plot_ly(
#             #key=~LAD23CD,
#             split = ~LAD23NM,
#             color = I("gray90"),
#             showlegend = FALSE) %>% #, text = ~LAD23NM, hoveron="fills", hoverinfo="text")
#        highlight(
#         color="rgba(0,117,176,1)",
#         #selectize = TRUE,
#         persistent = TRUE,
#         opacityDim = 0.8#,
#         #selected = attrs_selected(opacity = 0.3)
#       )
#   })
#   output$data <- renderPrint({
#     ladSF$LAD23NM[r$selected_area]
#     #sfShared$data(withSelection = TRUE)[,c(1,2,10)]
#    # event_data("plotly_click")$key[[1]]
#   #  req(input$plot_click)
#   })
#
#   r <- reactiveValues(selected_area = rep(FALSE, nrow(ladSF)))
#
#   observeEvent(sfShared$selection(), {
#     selection <- sfShared$selection()
#     selection_all <- r$selected_area | selection
#     r$selected_area <- selection_all
#     message("update selection")
#   })
#
#   observeEvent(input$inp, {
#     message("selectize selection")
#     selection <- ladSF$LAD23NM %in% input$inp
#     sfShared$selection(selection, ownerId= "plot")
#   })
#
# #
# #   observeEvent(event_data("plotly_click"), {
# #     message(paste0("click!", event_data("plotly_click")$key[[1]]))
# #   })
# }
#
# server2 <- function(input, output, session) {
#   r <- reactiveValues()
#   #dat <- reactive({
#   #  dat <- ladSF %>% mutate(selected = 0)
#   #  return(dat)
#   #})
#
#   output$plot <- renderPlotly({
#     #isolate(dat()) %>%
#       plot_ly() %>%
#       add_sf(
#         data=lad_sf, #dat(),
#         mode="markers+lines",
#         key=~LAD23CD,
#         split = ~LAD23NM,
#         #color = ~selected,#I("gray90"),
#         showlegend = FALSE,
#         text = ~LAD23NM,
#         hoveron="fills",
#         hoverinfo="text")
#   })
#   output$data <- renderPrint({
#     #event_data("plotly_click")$key[[1]]
#     #r$selected_area
#     dat$selected
#   })
#
#   observeEvent(event_data("plotly_click"), {
#     message(paste0("click!", event_data("plotly_click")$key[[1]]))
#     selection <- event_data("plotly_click")$key[[1]]
#     selection_all <- c(r$selected_area, selection)
#     r$selected_area <- unique(selection_all)
#   })
#
#   # observe(dat, {
#   #   plotlyProxy("plot", session) %>%
#   #     plotlyProxyInvoke(
#   #       "restyle",
#   #       list(
#   #         #lon = list(dat()$lon),
#   #         #lat = list(dat()$lat),
#   #         marker.color = list(dat()$selected)
#   #       ),
#   #       list(0)
#   #     )
#   # })
# }
#
# server3 <- function(input, output, session) {
#
#   # keep track of which selections have been clicked on
#   selections <- reactiveVal()
#
#   # On click, the key field of the event data contains the selection name
#   # Add that name to the set of all "selected" selections
#   eventDat <- reactive(event_data("plotly_click"))
#
#   observeEvent(event_data("plotly_click"), {
#     selection <- event_data("plotly_click")$key
#     selections_old_new <- c(selections(), selection)
#     selections(unique(selections_old_new))
#   })
#
#   # clear the set of selections when a double-click occurs
#   observeEvent(event_data("plotly_doubleclick"), {
#     selections(NULL)
#   })
#
#   output$plot <- renderPlotly({
#
#     # if the selection is selected, paint it red
#     dat <- ladSF
#     ladSF$cols <- ifelse(ladSF$LAD23CD %in% selections(), "red", "black")
#
#     ladSF %>%
#       plot_ly(
#         key = ~LAD23CD,
#         split = ~LAD23CD,
#         color = ~cols
#       ) %>%
#       add_sf()
#   })
#
#   output$data <- renderPrint({
#     filter(ladSF, LAD23CD %in% selections())
#   })
#
# }

# server4 <- function(input, output, session) {
#
#   eventDat <- reactive(event_data("plotly_click", source = "plot"))
#
#   #r <- reactiveValues(selections = data.frame(nm= NULL, cd= NULL, trace=NULL))
#   selections <- reactiveVal(data.frame(nm= NULL, cd= NULL, trace=NULL))
#
#   observeEvent(eventDat(), {
#     if(!(eventDat()$key %in% selections()[["cd"]])){
#       cd <- eventDat()$key[[1]]
#       nm <- ladSF$LAD23NM[ladSF$LAD23CD==cd]
#       trace <- eventDat()$curveNumber
#       df <- data.frame(cd, nm, trace)
#       selections(bind_rows(selections(), df))
#       #r$selections <- add_row(r$selections, cd=cd, nm=nm, trace=trace)
#     }
#   })
#
#   observeEvent(input$inp, {
#     if(!identical(input$inp,""))# & (!(input$inp %in% selections()[["nm"]])))
#       {
#       nm <- input$inp
#       cd <- ladSF$LAD23CD[ladSF$LAD23NM %in% nm]
#       trace <- map_int(cd, ~which(ladSF$LAD23CD == .x) - 1)  #-1 because javascript index starts from zero
#       df <- data.frame(cd, nm, trace)
#       selections(unique(bind_rows(selections(), df)))
#       #r$selections <- rbind(r$selections(), df)
#       #r$selections <- add_row(r$selections, cd=cd, nm=nm, trace=trace)
#     }
#   })
#
#   # clear the set of selections when a double-click occurs
#   observeEvent(event_data("plotly_doubleclick", source = "plot"), {
#     selections(data.frame(nm= NULL, cd= NULL, trace=NULL))
#     #r$selections <- data.frame(nm= NULL, cd= NULL, trace=NULL)
#
#     plotlyProxy("plot", session) %>%
#       plotlyProxyInvoke(
#         "restyle",
#         list(
#           #lon = list(dat()$lon),
#           #lat = list(dat()$lat),
#           fillcolor = "gray"
#         ))
#   })
#
#   output$plot <- renderPlotly({
#
#
#     dat <- ladSF
#
#     ladSF %>%
#       plot_ly(
#         key = ~LAD23CD,
#         split = ~LAD23CD,
#         color = I("gray"),
#         source = "plot"
#       ) %>%
#       add_sf()
#   })
#
#   output$data <- renderPrint({
#     selections()
#   })
#
# #colour map by selections
# observeEvent(selections(), {
#   if(nrow(selections())>0){
#     plotlyProxy("plot", session) %>%
#       plotlyProxyInvoke(
#         "restyle",
#         list(
#           #lon = list(dat()$lon),
#           #lat = list(dat()$lat),
#           fillcolor = "red"
#         ),
#         as.list((as.integer(selections()[["trace"]])))
#       )
#     updateSelectizeInput("inp", session = session, selected = selections()[["nm"]])
#   }
#   })
# }

server5 <- function(input, output, session) {

  observeEvent(event_data("plotly_click", source = "plot"), {
    #identify area code from map click
    cd <- event_data("plotly_click", source = "plot")$key[[1]]
    #add to input
    updateSelectizeInput("inp", session = session, selected = unique(c(input$inp, cd)))

  })

  observeEvent(input$inp, {
    #pick colour based on selection
    col = ifelse(ladSF$LAD23CD %in% input$inp, "red", "gray80")
    #restyle plot to assign new colours
    plotlyProxy("plot", session) %>%
      plotlyProxyInvoke(
        "restyle",
        list(
          fillcolor = col
        )
      )
  })

  output$plot <- renderPlotly({
    ladSF %>%
      plot_ly(
        key = ~LAD23CD,
        split = ~LAD23CD,
        color = I("gray"),
        source = "plot"
      ) %>%
      add_sf()
  })

}

shinyApp(ui, server5)

