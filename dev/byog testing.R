library(shiny)
library(dplyr)

ui <- fluidPage(
  bslib::page_fillable(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = "30%",
        open = "always",
        bslib::card(
          bslib::card_header("Select variables"),
          mod_pt_VarLevelSelect_ui("select", vars = reference$obs[!is.na(reference$categorical)]),
          actionButton("add_var", "Add variable"),
          actionButton("reset", "Reset"),
          textOutput("selections")
        )
      ),
      tableOutput("tab")
    )
  )
)

server <- function(input, output, session) {
  filt <- reactiveValues(vars = data.frame("obs"=vector("character"), "cat"=vector("character")),
                         varLevels = vector("character"))

  selection <- mod_pt_VarLevelSelect_server("select")

  observeEvent(input$add_var, {
    new <- rbind(filt$vars, data.frame("obs" = selection()$var, "cat" = selection()$level))
    if(!(anyDuplicated(new))){
      filt$vars <- new

      if(selection()$var==selection()$level) {
        lab <- translate_codes(selection()$var)}
      else {
        lab <- paste0(translate_codes(selection()$var),
                      ": ",
                      translate_codes(selection()$level))
      }

      filt$varLabels <- c(filt$varLabels, lab)
    }
  })

  observeEvent(input$reset, {
    filt$vars <- data.frame("obs"=vector("character"), "cat"=vector("character"))
  })

  area_dat <- reactive({
    ladDat %>%
      filter(age == "all_ages",
             sex == "both")
  })

  dat <- reactive({
     area_dat() %>%
       dplyr::right_join(., filt$vars, by=dplyr::join_by(obs, cat))
  })

  output$tab <- renderTable(dat())
  output$selections <- renderText(filt$varLabels)

}


shinyApp(ui, server)
