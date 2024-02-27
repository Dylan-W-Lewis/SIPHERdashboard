#' pt_AreaMap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues object with area profile id
#' @param var variable for cloropleth map
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_AreaMap_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("map")),
    fluidRow(column(selectInput(ns("var"), label="", choices=codebook$obs), width=6),
             column(selectInput(ns("level"), label="", choices = NULL), width=6)),
  )
}

#' pt_AreaMap Server Functions
#'
#' @noRd
mod_pt_AreaMap_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$var,{
      updateSelectInput(inputId = "level", choices = pull(codebook$cat[codebook$obs==input$var][[1]]))
    })

    filteredDat <- reactive({
      wardDat %>% filter(sex=="both", age_dv=="all_ages", ward %in% lookup_wd_lad$WD23CD[lookup_wd_lad$LAD23CD==r$profile])
    })

    mapDat <- reactive({
      temp <- filteredDat() %>% filter(obs == input$var, cat == input$level)
      output <- wardSF %>% right_join(., temp, by=c("WD23CD"="ward"))
      return(output)
    })

    getMap <- reactive({
      centre <- list(lon = mean(mapDat()$LONG),
                     lat = mean(mapDat()$LAT))
      #print(centre)

      fig <- plotly::plot_mapbox(
        mapDat(),
        #type = 'scattermapbox',
        split = ~WD23NM,
        color = ~value,
        hovertemplate = ~paste0(round(value, 1),"%"),
        showlegend = FALSE
      ) %>% plotly::add_sf()

      fig <- fig %>%
        plotly::layout(
          mapbox = list(style = "carto-positron",
                        center = centre,
                        zoom = 10
                        ))

      return(fig)
    })

    output$map <- plotly::renderPlotly(getMap())

  })
}

## To be copied in the UI
# mod_pt_AreaMap_ui("pt_AreaMap_1")

## To be copied in the server
# mod_pt_AreaMap_server("pt_AreaMap_1")
