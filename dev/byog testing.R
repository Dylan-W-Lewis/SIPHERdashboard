library(shiny)
library(dplyr)

ui <- fluidPage(
  tags$head(tags$style('.card{overflow: visible !important;}'),
            tags$style('.card-body{overflow: visible !important;}')),
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = "30%",
        open = "always",
        bslib::card(
          style = "position:relative; z-index:1000;",
          fillable = F,
          selectInput("test", label = "Local Authority", choices = setNames(ladSF$lad,ladSF$lad_name)),
        ),
        bslib::navset_card_tab(
          bslib::nav_panel("Select area",
                           fillable = F,
                           selectInput("las", label = "Local Authority", choices = setNames(ladSF$lad,ladSF$lad_name)),
                           div(style = "text-align: center;",
                             actionButton("add_la", "Add area", class= "btn-sm"),
                             actionButton("add_wards", "Add all wards in area", class= "btn-sm"))
                           ),
          bslib::nav_panel("Select variables",
                           fillable = F,
                           mod_pt_VarLevelSelect_ui("select", vars = reference$obs[!is.na(reference$categorical)], inline=F),
                           div(style = "text-align: center;",
                            actionButton("add_var", "Add variable", class= "btn-sm"))
                           )
          ),
        bslib::card(
          htmlOutput("selections"),
          actionButton("go", "Generate graph", class= "btn-sm"),
          actionButton("open_download", "Download graph...", class= "btn-sm"),
          div(style = "text-align: center;",
              actionButton("resetArea", "Clear areas", class= "btn-danger btn-sm"),
              actionButton("resetVars", "Clear variables", class= "btn-danger btn-sm")
              )
        ),
        # bslib::card(
        #   mod_pt_DownloadGraph_ui("pt_DownloadGraph_1")
        # )
      ),
      bslib::card(mod_pt_ParCoord_ui("par"),
                  full_screen = T,
                  max_height = 600),
      plotOutput("test")
    )
  )
)

server <- function(input, output, session) {

  filt <- reactiveValues(vars = data.frame("obs"=vector("character"), "cat"=vector("character")),
                         varLabels = vector("character"),
                         areas = data.frame("area"=vector("character"), "geo"=vector("character")))

  selection <- mod_pt_VarLevelSelect_server("select")

  observeEvent(input$add_var, {
    new <- rbind(filt$vars, data.frame("obs" = selection()$var, "cat" = selection()$level))
    if(!(anyDuplicated(new))){
      filt$vars <- new
    }
  })

  observeEvent(input$resetVars, {
    filt$vars <- data.frame("obs"=vector("character"), "cat"=vector("character"))
    filt$varLabels <- vector("character")
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
      filter(age == "all_ages",
             sex == "both",
             area %in% filt$areas$area) |>
      select(-c(age, sex))
    wards <- wardDat |>
      filter(area %in% filt$areas$area)
    out <- rbind(lads, wards) |>
      dplyr::left_join(filt$areas, by="area")
    return(out)
  })

  filteredDat <- reactive({
     areaDat() |>
      dplyr::right_join(filt$vars, by=dplyr::join_by(obs, cat))
  })

  updateDat <- eventReactive(input$go,{
    filteredDat()
  })

  observeEvent(updateDat, {
    mod_pt_ParCoord_server("par", updateDat)
  })


  output$selections <- renderUI(
    HTML(paste0(c("<b>Variables:</b>", make_var_labels(filt$vars$obs, filt$vars$cat), "<b>Areas:</b>", unique(filt$areas$geo)),
                "<br/>"))
    )

  plot <- eventReactive(input$open_download,
                        mod_pt_ParCoord_server("gg_par", filteredDat, output_type = "ggplot"))

  downloadDialog <- modalDialog(mod_pt_DownloadGraph_ui("pt_DownloadGraph_1"),
                                size = "s",
                                easyClose = T,
                                footer = NULL)

  observeEvent(input$open_download, {
    showModal(downloadDialog)
    mod_pt_DownloadGraph_server("pt_DownloadGraph_1",
                                plot)
  })

  # ggPar <- eventReactive(input$open_download, mod_pt_ParCoord_server("gg_par", filteredDat, output_type = "ggplot"))



 # mod_pt_DownloadGraph_server("pt_DownloadGraph_1", ggPar)
}


shinyApp(ui, server)
