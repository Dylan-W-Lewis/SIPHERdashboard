#' page_DataDownload UI Function
#'
#' @description Page to download selected data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_DataDownload_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_columns(
        col_widths = bslib::breakpoints(
          lg = 12,
          xl = c(-1,10,-1),
          xxl = c(-2, 8, -2)),
        fill = F,
        gap=0,
        h2(strong("Data download"), style="color:#005398"),
        #p("You can download data from..."),
        br(),

        bslib::card(
          bslib::card_header("Filter and preview data"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(open = "always",
                              bslib::accordion(
                                open = FALSE,

                bslib::accordion_panel("Areas",
                                 #class="graph-controls",
                                 selectizeInput(ns("las"), label = "Filter by area",
                                                multiple = T,
                                                choices = list(#"Country averages" = unique(ctrDat$area),
                                                  "Local authorities" = c("All areas" = "", sort_by_name(setNames(ladSF$lad,ladSF$lad_name))))
                                                ),
                                 checkboxInput(ns("include_wards"), "Include ward-level estimates"),
                                 ),
                bslib::accordion_panel("Variables",
                                   #class="graph-controls",
                                   selectizeInput(ns("var"), label = "Filter by variable",
                                                  multiple = T,
                                                  choices = c("All variables" = "",setNames(unique(ladDat$obs), translate_codes(unique(ladDat$obs))))
                                                  ),
                                 ),
                bslib::accordion_panel("Population",
                                   #class="graph-controls",
                                   checkboxInput(ns("include_age"), "Age-specific estimates"),
                                   checkboxInput(ns("include_sex"), "Sex-specific estimates"),
                                 )
                )),
            tableOutput(ns("tab"))

          )
        ),

        br(),

        downloadButton(ns("download"), "Download data")


      )
    )
  )
}

#' page_DataDownload Server Functions
#'
#' @noRd
mod_page_DataDownload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    areaFilt <- reactive({

      lads <- ladDat |> dplyr::left_join(unique(lookup_wd_lad[,c("lad","lad_name")]), by = c("area" = "lad"))

      if(input$include_wards == T){

        lads <- lads |>
          dplyr::mutate("ward_name" = "(all wards)")

        wards <- wardDat |>
          dplyr::left_join(lookup_wd_lad[!duplicated(lookup_wd_lad$ward),c("ward", "ward_name", "lad_name")], by = c("area" = "ward")) |>
          dplyr::mutate(sex = "both", age = "all_ages")

        if(!(identical(input$las, "") | is.null(input$las))){ #if filtering by area

          lads <- lads |> dplyr::filter(area %in% input$las)

          areaWards <- lookup_wd_lad$ward[lookup_wd_lad$lad %in% input$las]
          wards <- wards |> dplyr::filter(area %in% areaWards)
        }

        dat <- dplyr::bind_rows(lads, wards) |> dplyr::relocate("ward_name", .before = "area")
      }

      if(input$include_wards == F) {

        if(identical(input$las, "") | is.null(input$las)){ #if filtering by area

          dat <- lads

        } else { #if filtering by area

          dat <- lads |> dplyr::filter(area %in% input$las)

        }
      }

      dat <-  dat |>
        dplyr::rename("area_code" = "area", "local_authority" = "lad_name") |>
        dplyr::relocate("local_authority", .before = "area_code") |>
        dplyr::mutate("variable" = translate_codes(obs),
                      "category" = dplyr::if_else(obs %in% reference$obs[reference$categorical], cat, "(continuous)")) |>
        dplyr::relocate(c("variable", "category"), .before = "obs")

      return(dat)
    })


    varFilt <- reactive({

      dat <- areaFilt()


      if(!(identical(input$var, "") | is.null(input$var))){ #if filtering by area

        dat <- dat |> dplyr::filter(obs %in% input$var)

      }

      return(dat)
    })

    finalFilt <- reactive({

      dat <- varFilt() |> dplyr::select(-obs, -cat, -labelled, -scaled)

      if(!isTRUE(input$include_sex)){
        dat <- dat |> dplyr::filter(sex == "both") |> dplyr::select(-sex)
      }


      if(!isTRUE(input$include_age)){
        dat <- dat |> dplyr::filter(age == "all_ages") |> dplyr::select(-age)
      }

      return(dat)

    })



    output$tab <- renderTable(head(finalFilt()))

    output$download <- downloadHandler(filename = "SIPHER_synthetic_population_extract.csv",
                                       content = function(file){
                                         write.csv(finalFilt(), file, row.names = F)
                                       } )


  })
}

## To be copied in the UI
# mod_page_DataDownload_ui("page_DataDownload_1")

## To be copied in the server
# mod_page_DataDownload_server("page_DataDownload_1")
