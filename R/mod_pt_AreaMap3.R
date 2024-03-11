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
      wardSF %>%
        dplyr::filter(ward %in% lookup_wd_lad$ward[lookup_wd_lad$lad==r$profile]) %>%
        sf::st_transform(crs = "WGS84")
    })

    output$map <- leaflet::renderLeaflet(
      leaflet::leaflet(mapDat()) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolylines(weight = 2, color= "#CCCCCC") %>%
        leaflet::fitBounds(min(mapDat()$LONG), min(mapDat()$LAT), max(mapDat()$LONG), max(mapDat()$LAT),
                           options = list(padding = c(50,50)))
    )

    # update fill

    fillDat <- reactive({
      filtered <- wardDat %>%
        dplyr::filter(sex=="both",
                      age=="all ages")
      output <- mapDat() %>% select(ward, ward_name) %>% left_join(., filtered, by=c("ward" = "area"))
      return(output) #[match(mapDat()$ward, filtered$area),])
    })

    observeEvent({
      varblR()
      categR()
      fillDat()
      }, {

      #if(!is.null(varbl)&&!is.null(categ)){
        values <- fillDat() %>%
          dplyr::filter(obs == varblR(),
                        cat == categR()) %>%
          dplyr::pull(value)

        print(paste0("expected n wards = ",
                     nrow(mapDat()),
                     ".  rows in fill data =",
                     nrow(fillDat())))

        print(paste0("vals length = ", length(values)))


        colr <- as.data.frame(colorRamp(c("white", "#005CBA"))(values/max(values))) %>%
          purrr::pmap_chr(~rgb(..1,..2,..3, maxColorValue = 255))

        if(codebook$mean[codebook$obs==varbl()]==T){
          labl <- paste0(mapDat()$ward_name, ": ", round(values, 2))
        } else {
          labl <- paste0(mapDat()$ward_name, ": ", round(values, 1), "%")
        }

        leaflet::leafletProxy("map", data = mapDat()) %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(fillColor = colr, fillOpacity=0.8, weight = 2, color= "#CCCCCC", label = labl)
      #}
    })

  })
}

## To be copied in the UI
# mod_pt_AreaMap3_ui("pt_AreaMap3_1")

## To be copied in the server
# mod_pt_AreaMap3_server("pt_AreaMap3_1")
