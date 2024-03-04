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
    plotly::plotlyOutput(ns("map"), fill = T)
  )
}

#' pt_AreaMap Server Functions
#'
#' @noRd
mod_pt_AreaMap_server <- function(id, r){
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
      output <- wardSF %>% dplyr::right_join(., temp, by=c("ward"="ward"))
      return(output)
    })

    centre <- reactive({
      list(lon = mean(mapDat()$LONG),
           lat = mean(mapDat()$LAT))})

    getMap <- reactive({

      dat <- mapDat()

      #selected_var <- paste0(r$var_selection[["var"]], "_", r$var_selection[["level"]])

      fig <- plotly::plot_mapbox(
        source= "map",
        dat,
        #type = 'scattermapbox',
        split = ~ward_name,
        color = I("gray"),
        #color = as.formula(paste0("~", selected_var)),
        #text = as.formula(paste0("~", selected_var)),
        #hoverinfo = "text",
        #hovertemplate = "%{text:.3}%",
        showlegend = FALSE
      ) %>% plotly::add_sf()

      fig <- fig %>%
        plotly::layout(
          mapbox = list(style = "carto-positron",
                        center = centre(),
                        #bounds = bounds
                        zoom = 8
                        ))

      return(fig)
    })

    selectedVar <- reactive(paste0(r$var_selection[["var"]], "_", r$var_selection[["level"]]))

    observeEvent(r$var_selection[["level"]],{

      message("var = ", selectedVar())

      if(selectedVar() %in% colnames(filteredDat())){

        varCol <- dplyr::pull(filteredDat(), selectedVar())

        colr <- as.data.frame(colorRamp(c("white", "#005CBA"))(varCol/100)) %>%
          purrr::pmap_chr(~paste0("rgba(", ..1, ",", ..2, ",", ..3, ",0.5)"))

        labl <- paste0(mapDat()$ward_name, ": ", round(varCol, 1), "%")

        message("restyle")

        plotly:: plotlyProxy("map", session) %>%
          plotly::plotlyProxyInvoke(
            "restyle",
            list(fillcolor = colr,
                 hoverinfo = "text",
                 hovertext = labl)
          ) %>%
          plotly::plotlyProxyInvoke(
            "relayout",
            list(center = centre(),
                 zoom = 8)
          )
      }
    })

    output$map <- plotly::renderPlotly(getMap())

  })
}

## To be copied in the UI
# mod_pt_AreaMap_ui("pt_AreaMap_1")

## To be copied in the server
# mod_pt_AreaMap_server("pt_AreaMap_1")
