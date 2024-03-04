#' pt_AreaMap2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_AreaMap2_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))

  )
}

#' pt_AreaMap2 Server Functions
#'
#' @noRd
mod_pt_AreaMap2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filteredDat <- reactive({
      wardDat %>%
        dplyr::filter(sex=="both",
                      age_dv=="all_ages",
                      ward %in% lookup_wd_lad$ward[lookup_wd_lad$lad==r$profile]) %>%
        tidyr::pivot_wider(names_from = c(obs, cat), values_from = value)
    })

    mapDat <- reactive({
      temp <- filteredDat() #%>% dplyr::filter(obs == r$var_selection[["var"]], cat == r$var_selection[["level"]])
      output <- wardSF %>% dplyr::right_join(., temp, by=c("ward"="ward")) %>% st_transform(crs = "WGS84")
      return(output)
    })


    output$map <- leaflet::renderLeaflet(
      leaflet::leaflet(mapDat()) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::fitBounds(min(mapDat()$LONG), min(mapDat()$LAT), max(mapDat()$LONG), max(mapDat()$LAT),
                           options = list(padding = c(50,50)))
    )

    observe({

      varName <- paste0(r$var_selection[["var"]], "_", r$var_selection[["level"]])

      if(varName %in% colnames(filteredDat())){

        varCol <- dplyr::pull(filteredDat(), varName)

        colr <- as.data.frame(colorRamp(c("white", "#005CBA"))(varCol/100)) %>%
          purrr::pmap_chr(~rgb(..1,..2,..3, maxColorValue = 255))

        labl <- paste0(mapDat()$ward_name, ": ", round(varCol, 1), "%")

        leaflet::leafletProxy("map", data = mapDat()) %>%
          leaflet::clearShapes() %>%
          leaflet::addPolygons(fillColor = colr, fillOpacity=0.8, weight = 2, color= "#CCCCCC", label = labl)
      }

    })

  })
}

## To be copied in the UI
# mod_pt_AreaMap2_ui("pt_AreaMap2_1")

## To be copied in the server
# mod_pt_AreaMap2_server("pt_AreaMap2_1")
