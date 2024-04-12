#' page_MapExplore UI Function
#'
#' @description Map explore page for Sipgher dashboard.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r global reactiveValues
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_MapExplore_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_sidebar(
        #row_heights = 1,
        #fill = T,
        #col_widths =  c(6,6),
        sidebar = bslib::sidebar(
          padding=0,
          width = "50%",
          open = "always",
          mod_pt_MapSelect2_ui(ns("pt_MapSelect_1"))
          ),
        bslib::layout_columns(
          fill = T,
          #row_heights = c("auto", 6),
          col_widths = 12,
          bslib::navset_hidden(
            id="graph_card",
            bslib::nav_panel_hidden("hide",
                                    div(style = "margin-top:30vh"),
                                    p(style="text-align: center; color: #005398; font-size: 40px; font-weight: bold;",
                                      "Select an area to begin")
                                    ),
            bslib::nav_panel_hidden("show",
                                    # selected areas card
                                    bslib::card(
                                      bslib::accordion(
                                        open = FALSE,
                                        bslib::accordion_panel("View area profiles",
                                                               mod_pt_AreaSelections_ui(ns("pt_AreaSelections_1"))
                                        )
                                      )
                                    ),
                                    # comparison plot card
                                    bslib::navset_card_pill(
                                      #full_screen = T,
                                      #height = "65vh",
                                      bslib::nav_panel(
                                        "Constraints",
                                        icon = bslib::tooltip(
                                                  icon("circle-question"),
                                                  "Variables used to construct the Synthetic Population",
                                                  placement = "left"
                                                ),
                                        h5("Ethnicity"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_1")),
                                        h5("Marital status"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_2")),
                                        h5("General health"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_3")),
                                        h5("Employment status"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_4")),
                                        h5("Highest qualification"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_5")),
                                        h5("Household composition"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_6")),
                                        h5("Housing tenure"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_7"))
                                      ),
                                      bslib::nav_panel(
                                        "Health & wellbeing"

                                      ),
                                      bslib::nav_panel(
                                        "Income & employment",
                                        #mod_pt_ComparisonGraph_ui("pt_ComparisonGraph_1")
                                      )
                                    )
            )
          )
        )
      )
    )
  )
}

#' page_MapExplore Server Functions
#'
#' @noRd
mod_page_MapExplore_server <- function(id, r, parent.session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(r$selected_area,
                 once = T,
                 {
      if(!is.null(r$selected_area)){
        bslib::nav_select(id="graph_card", selected = "show", session = parent.session)
      }
    })

    comparisonData <- reactive({
      ladDat %>%
        dplyr::filter(sex=="both", age=="all ages", area %in% r$selected_area) %>%
        dplyr::left_join(., sf::st_drop_geometry(ladSF[, c("lad", "lad_name")]), by=c("area" = "lad"))
      })

    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1", r=r, var="racel_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_2", r=r, var="marstat", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_3", r=r, var="scsf1", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_4", r=r, var="jbstat", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_5", r=r, var="hiqual_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_6", r=r, var="hhtype_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_7", r=r, var="tenure_dv", dat=comparisonData)

    mod_pt_MapSelect2_server("pt_MapSelect_1", r = r)
    mod_pt_AreaSelections_server("pt_AreaSelections_1", r = r)

  })
}

## To be copied in the UI
# mod_page_MapExplore_ui("page_MapExplore_1")

## To be copied in the server
# mod_page_MapExplore_server("page_MapExplore_1")
