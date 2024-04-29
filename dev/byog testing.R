library(shiny)
library(dplyr)

ui <- fluidPage(
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = "30%",
        open = "always",
        bslib::navset_card_tab(
          bslib::nav_panel("Select area",
              selectInput("las", label = NULL, choices = setNames(ladSF$lad,ladSF$lad_name)),
              actionButton("add_la", "Add area"),
              actionButton("add_wards", "Add all wards in area")),
          bslib::nav_panel("Select variables",
            mod_pt_VarLevelSelect_ui("select", vars = reference$obs[!is.na(reference$categorical)], inline=F),
            actionButton("add_var", "Add variable"),
            actionButton("reset", "Reset"))
          ),
        bslib::card(
          htmlOutput("selections"),
          actionButton("go", "Create graph")
        )
      ),
      bslib::card(mod_pt_ParCoord_ui("par"),
                  full_screen = T)#,
      #tableOutput("tab")
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

  observeEvent(input$reset, {
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


  area_dat <- reactive({
    lads <- ladDat %>%
      filter(age == "all_ages",
             sex == "both",
             area %in% filt$areas$area) %>%
      select(-c(age, sex))
    wards <- wardDat %>%
      filter(area %in% filt$areas$area)
    out <- rbind(lads, wards) %>%
      dplyr::left_join(., filt$areas, by="area")
    return(out)
  })

  dat <- reactive({
     area_dat() %>%
      dplyr::right_join(., filt$vars, by=dplyr::join_by(obs, cat))
  })

  observeEvent(input$go, {
    plotDat <- dat
    mod_pt_ParCoord_server("par", plotDat, make_var_labels(filt$vars$obs, filt$vars$cat))
  })

  #output$tab <- renderTable(dat())
  output$selections <- renderUI(
    HTML(paste0(c("<b>Variables:</b>", make_var_labels(filt$vars$obs, filt$vars$cat), "<b>Areas:</b>", unique(filt$areas$geo)),
                "<br/>"))
    )
}


shinyApp(ui, server)
