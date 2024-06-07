#' pt_BYOG_parcoords UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_BYOG_parcoords_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = "30%",
          open = "always",
          bslib::navset_card_tab(
            bslib::nav_panel("Areas",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               selectInput(ns("las"), label = "Local Authority", choices = sort_by_name(setNames(ladSF$lad,ladSF$lad_name))),
                               div(style = "text-align: center;",
                                   actionButton(ns("add_la"), "Add LA", class= "btn-sm"),
                                   actionButton(ns("add_wards"), "Add wards", class= "btn-sm"),
                                   actionButton(ns("resetArea"), "Clear areas", class= "btn-danger btn-sm")
                               )
                             )

            ),
            bslib::nav_panel("Variables",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               mod_pt_VarLevelSelect_ui(ns("select"), vars = reference$obs[!is.na(reference$categorical)], inline=F),
                               div(style = "text-align: center;",
                                   actionButton(ns("add_var"), "Add variable", class= "btn-sm"),
                                   actionButton(ns("resetVars"), "Clear variables", class= "btn-danger btn-sm"))
                             )
            )
          ),
          bslib::card(
            bslib::card_header("Selections"),
            bslib::card_body(
              class="graph-controls",
              htmlOutput(ns("selections"))
            )
          )
        ),
        bslib::card(mod_pt_ParCoord_ui(ns("par")),
                    full_screen = T,
                    max_height = 800),
        div(style = "text-align: center;",
            actionButton(ns("open_download"), "Download graph...", class= "btn-sm")
        )

      )
    )
  )
}

#' pt_BYOG_parcoords Server Functions
#'
#' @noRd
mod_pt_BYOG_parcoords_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### Setup reactiveValues to contain filter selections
    filt <- reactiveValues(vars = data.frame("obs"=vector("character"), "cat"=vector("character"), "varLabels"=vector("character")),
                           areas = data.frame("area"=vector("character"), "geo"=vector("character")))

    #### variable selection

    selection <- mod_pt_VarLevelSelect_server("select")

    observeEvent(input$add_var, {
      new <- rbind(filt$vars, data.frame("obs" = selection()$var,
                                         "cat" = selection()$level,
                                         "varLabels" = make_var_labels(selection()$var, selection()$level)))
      if(!(anyDuplicated(new))){
        filt$vars <- new
      }
    })

    observeEvent(input$resetVars, {
      filt$vars <- data.frame("obs"=vector("character"), "cat"=vector("character"), varLabels = vector("character"))
    })

    ##### area selections

    observeEvent(input$add_la, {
      if(! input$las %in% filt$areas$area){
        filt$areas <- rbind(filt$areas, data.frame("area" = input$las, "geo" = ladSF$lad_name[ladSF$lad == input$las]))
      }
    })

    observeEvent(input$add_wards, {
      wards <- lookup_wd_lad$ward[lookup_wd_lad$lad==input$las]
      if(! any(wards %in% filt$areas$area)){
        filt$areas <- rbind(filt$areas,
                            data.frame("area" = wards, "geo" = paste0(ladSF$lad_name[ladSF$lad == input$las], " (Wards)")))
      }
    })

    observeEvent(input$resetArea, {
      filt$areas <- data.frame("area"=vector("character"), "geo"=vector("character"))
    })

    ##### view selections

    output$selections <- renderUI(
      HTML(paste0(c("<b>Variables:</b>", ifelse(length(filt$vars$varLabels)>0, paste(filt$vars$varLabels, collapse = ", "), "None"),
                    "<br><b>Areas:</b>", ifelse(length(filt$areas$geo)>0, paste(unique(filt$areas$geo), collapse = ", "), "None")
      ))))

    ##### create dataset

    # join data from LAD and Ward datasets
    areaDat <- reactive({
      lads <- ladDat |>
        dplyr::filter(age == "all_ages",
               sex == "both",
               area %in% filt$areas$area) |>
        dplyr::select(-c(age, sex))
      wards <- wardDat |>
        dplyr::filter(area %in% filt$areas$area)
      out <- rbind(lads, wards) |>
        dplyr::left_join(filt$areas, by="area")
      return(out)
    })
    #filter to only include selected
    filteredDat <- reactive({
      areaDat() |>
        dplyr::right_join(filt$vars, by=dplyr::join_by(obs, cat))
    })
    # data for plotting that only updates with button press
    # (workaround for complicated reactivity with ParCoords module)
    updateDat <- reactive({
      req(nrow(filteredDat()>0))
      filteredDat()
    })

    ##### create graph


    mod_pt_ParCoord_server("par", updateDat)


    ##### download data

    plot <- eventReactive(input$open_download,
                          mod_pt_ParCoord_server("gg_par", filteredDat, output_type = "ggplot"))

    downloadDialog <- modalDialog(mod_pt_DownloadGraph_ui(ns("pt_DownloadGraph_1")),
                                  size = "s",
                                  easyClose = T,
                                  footer = NULL)

    observeEvent(input$open_download, {
      showModal(downloadDialog)
      mod_pt_DownloadGraph_server("pt_DownloadGraph_1",
                                  plot)
      })

  })
}

## To be copied in the UI
# mod_pt_BYOG_parcoords_ui("pt_BYOG_parcoords_1")

## To be copied in the server
# mod_pt_BYOG_parcoords_server("pt_BYOG_parcoords_1")
