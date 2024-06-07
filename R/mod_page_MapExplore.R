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
        fillable = T,
        #col_widths =  c(6,6),
        sidebar=bslib::sidebar(
          gap = 0,
          padding=c(0, 20, 0, 0),
          width = "50%",
          open = "always",
          class = "html-fill-container",
          div(style = "margin-top:-24px"),
          div(mod_pt_MapSelect2_ui_box(ns("pt_MapSelect_1")),
              style = "margin: 15px 0 0px 20px"),
          div(style = "margin-top:-15px"),
          mod_pt_MapSelect2_ui_map(ns("pt_MapSelect_1"))
        ) |> (\(x) {
          x$tag$children[[1]] <- x$tag$children[[1]] |> bslib::as_fill_carrier()
          x})(),
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
                                      padding=0,
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
                                        "Demographics",
                                        icon = bslib::tooltip(
                                                  icon("circle-question"),
                                                  "These variables were used to construct the Synthetic Population",
                                                  placement = "left"
                                                ),
                                        #h5("Ethnicity"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_1")),
                                        #h5("Marital status"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_2")),
                                        #h5("General health"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_3")),
                                        #h5("Employment status"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_4")),
                                        #h5("Highest qualification"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_5")),
                                        #h5("Household composition"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_6")),
                                        #h5("Housing tenure"),
                                        mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_7"))
                                      ),
                                      bslib::nav_panel(
                                        "Health & wellbeing",
                                        purrr::map(domain_vars("health", categorical_only = T),
                                                   ~mod_pt_ComparisonGraph_ui(ns(paste0("ComparisonGraph_", .x))))
                                      ),
                                      bslib::nav_panel(
                                        "Income & employment",
                                        purrr::map(domain_vars("income", categorical_only = T),
                                                   ~mod_pt_ComparisonGraph_ui(ns(paste0("ComparisonGraph_", .x))))
                                      ),
                                      bslib::nav_panel(
                                        "Housing & households",
                                        purrr::map(domain_vars("housing", categorical_only = T),
                                                   ~mod_pt_ComparisonGraph_ui(ns(paste0("ComparisonGraph_", .x))))
                                      ),
                                      bslib::nav_panel(
                                        "Lifestyle, diet & nutrition",
                                        purrr::map(domain_vars("lifestyle", categorical_only = T),
                                                   ~mod_pt_ComparisonGraph_ui(ns(paste0("ComparisonGraph_", .x))))
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
      ladDat |>
        dplyr::filter(sex=="both", age=="all_ages", area %in% r$selected_area) |>
        dplyr::left_join(sf::st_drop_geometry(ladSF[, c("lad", "lad_name")]), by=c("area" = "lad"))
      })

    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1",  var="racel_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_2",  var="marstat", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_3",  var="scsf1", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_4",  var="jbstat", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_5",  var="hiqual_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_6",  var="hhtype_dv", dat=comparisonData)
    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_7",  var="tenure_dv", dat=comparisonData)

    purrr::map(domain_vars("health", categorical_only = T),
                ~mod_pt_ComparisonGraph_server(paste0("ComparisonGraph_", .x),  var=.x, dat=comparisonData))
    purrr::map(domain_vars("income", categorical_only = T),
               ~mod_pt_ComparisonGraph_server(paste0("ComparisonGraph_", .x),  var=.x, dat=comparisonData))
    purrr::map(domain_vars("lifestyle", categorical_only = T),
               ~mod_pt_ComparisonGraph_server(paste0("ComparisonGraph_", .x),  var=.x, dat=comparisonData))
    purrr::map(domain_vars("housing", categorical_only = T),
               ~mod_pt_ComparisonGraph_server(paste0("ComparisonGraph_", .x),  var=.x, dat=comparisonData))

    mod_pt_MapSelect2_server("pt_MapSelect_1", r = r)
    mod_pt_AreaSelections_server("pt_AreaSelections_1", r = r)

  })
}

## To be copied in the UI
# mod_page_MapExplore_ui("page_MapExplore_1")

## To be copied in the server
# mod_page_MapExplore_server("page_MapExplore_1")
