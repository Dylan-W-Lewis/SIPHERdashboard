#' pt_BYOG_dembar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_BYOG_dembar_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fillable(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = "30%",
          open = "always",
          bslib::navset_card_tab(
            bslib::nav_panel("Select areas",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               selectInput(ns("las"), label = "Local Authority", choices = setNames(ladSF$lad,ladSF$lad_name)),
                               div(style = "text-align: center;",
                                   actionButton(ns("add_la"), "Add area", class= "btn-sm")
                               )
                             )

            ),
            bslib::nav_panel("Select variables",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               mod_pt_VarLevelSelect_ui(ns("select"), vars = reference$obs[!is.na(reference$categorical)], inline=F),
                               div(style = "text-align: center;",
                                   actionButton(ns("add_var"), "Add variable", class= "btn-sm"))
                             )
            )
          ),
          bslib::card(
            bslib::card_body(
              #max_height = "100px",
              class="graph-controls",
              htmlOutput(ns("selections")),
              actionButton(ns("go"), "Generate graph", class= "btn-sm"),
              actionButton(ns("open_download"), "Download graph...", class= "btn-sm"),
              div(style = "text-align: center;",
                  actionButton(ns("resetArea"), "Clear areas", class= "btn-danger btn-sm"),
                  actionButton(ns("resetVars"), "Clear variables", class= "btn-danger btn-sm"))
              )
            )
          ),
        bslib::card(mod_pt_DemographicsBar_ui(ns("bar")),
                    full_screen = T,
                    max_height = 600)
      )
    )
  )
}

#' pt_BYOG_dembar Server Functions
#'
#' @noRd
mod_pt_BYOG_dembar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    filt <- reactiveValues(vars = data.frame("obs"=vector("character"), "cat"=vector("character"), "varLabels" = vector("character")),
                           areas = data.frame("area"=vector("character"), "geo"=vector("character")))

    selection <- mod_pt_VarLevelSelect_server("select")

    observeEvent(input$add_var, {
      new <- data.frame("obs" = selection()$var, "cat" = selection()$level) |> dplyr::mutate(varLabels = make_var_labels(obs, cat))
      comb <- rbind(filt$vars, new)
      if(!(anyDuplicated(comb))){
        filt$vars <- comb
      }
    })

    observeEvent(input$resetVars, {
      filt$vars <- data.frame("obs"=vector("character"), "cat"=vector("character"), varLabels = vector("character"))
    })


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


    areaDat <- reactive({
      lads <- ladDat |>
        dplyr::filter(area %in% filt$areas$area) |>
        dplyr::left_join(filt$areas, by="area")

    })

    filteredDat <- reactive({
      areaDat() |>
        dplyr::right_join(filt$vars, by=dplyr::join_by(obs, cat))
    })

    updateDat <- eventReactive(input$go,{
      filteredDat()
    })

    varbl <- eventReactive(input$go,{
      filt$vars$obs
    })

    categ <- eventReactive(input$go,{
      filt$vars$cat
    })

    observeEvent(updateDat, {
      mod_pt_DemographicsBar_server("bar", updateDat, varbl, categ)
    })


    output$selections <- renderUI(
      HTML(paste0(c("<b>Variables:</b>", make_var_labels(filt$vars$obs, filt$vars$cat), "<b>Areas:</b>", unique(filt$areas$geo)),
                  "<br/>"))
    )


    plot <- eventReactive(input$open_download,
                          mod_pt_DemographicsBar_server("bar", updateDat, varbl, categ, output_type = "ggplot"))

    downloadDialog <- modalDialog(mod_pt_DownloadGraph_ui("pt_DownloadGraph_1"),
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
# mod_pt_BYOG_dembar_ui("pt_BYOG_dembar_1")

## To be copied in the server
# mod_pt_BYOG_dembar_server("pt_BYOG_dembar_1")
