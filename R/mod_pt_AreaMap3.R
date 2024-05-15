#' pt_AreaMap3 UI Function
#'
#' @description Create a choropleth map of a local area. Allow user to provide fill variable.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactives
#' @param varbl,categ selected variable and category (level)
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_AreaMap3_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

#' pt_AreaMap3 Server Functions
#'
#' @noRd
mod_pt_AreaMap3_server <- function(id, r, varbl, categ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # selection <- reactiveValues()
    #
    # #print(is.reactive(varbl))
    #
    if(is.reactive(varbl)){
      varblR <- varbl
    } else {
      varblR <- reactive(varbl)
    }

    if(is.reactive(categ)){
      categR <- categ
    } else {
      categR <- reactive(categ)
    }

    mapDat <- reactive({
      wardSF |>
        dplyr::filter(ward %in% lookup_wd_lad$ward[lookup_wd_lad$lad==r$profile]) |>
        sf::st_transform(crs = "WGS84")
    })

    leafletMap <- reactive({
      leaflet::leaflet(mapDat()) |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        #leaflet::addPolylines(weight = 2, color= "#CCCCCC") |>
        leaflet::addPolygons(fillColor = isolate(fillVals()$colr), fillOpacity=0.8, weight = 2, color= "#CCCCCC", label = isolate(fillVals()$labl)) |>
        leaflet::fitBounds(min(mapDat()$LONG), min(mapDat()$LAT), max(mapDat()$LONG), max(mapDat()$LAT),
                           options = list(padding = c(50,50))) |>
        leaflet::addLegend(
          position = "topright",
          colors = c("#E69F00", "#005CBA"),  # Custom colors
          labels = c("Lower than GB average", "Higher than GB average"),          # Custom labels
          #title = "Custom Legend"
        )
    })


    output$map <- leaflet::renderLeaflet(
      leafletMap()
    )

    # update fill

    fillDat <- reactive({
      filtered <- wardDat #|>
        # dplyr::filter(sex=="both",
        #               age=="all ages")
      output <- mapDat() |> dplyr::select(ward, ward_name) |> dplyr::left_join(filtered, by=c("ward" = "area"))
      return(output) #[match(mapDat()$ward, filtered$area),])
    })

    fillVals <- reactive({
      data <-  fillDat() |>
          dplyr::filter(obs == varblR(),
                        cat == categR())

      scaledValues <- pmax(pmin((data$scaled/10)+0.5, 1), 0)


      colr <- as.data.frame(colorRamp(c("#E69F00", "white", "#005CBA"))(scaledValues)) |>
        purrr::pmap_chr(~ifelse(is.na(..1), rgb(0.5,0.5,0.5), rgb(..1,..2,..3, maxColorValue = 255)))

      labl <- paste0(mapDat()$ward_name, ": ", data$labelled)

      return(list("colr"=colr, "labl"=labl))
    })

    # update colour when selections change

    observeEvent(fillVals(),
                 ignoreInit = T,
                 {
                     leaflet::leafletProxy("map", data = mapDat()) |>
                        leaflet::clearShapes() |>
                        leaflet::addPolygons(fillColor = fillVals()$colr, fillOpacity=0.8, weight = 2, color= "#CCCCCC", label = fillVals()$labl)

                  }
    )

  })
}

## To be copied in the UI
# mod_pt_AreaMap3_ui("pt_AreaMap3_1")

## To be copied in the server
# mod_pt_AreaMap3_server("pt_AreaMap3_1")
