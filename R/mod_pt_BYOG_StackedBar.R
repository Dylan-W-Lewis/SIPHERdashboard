#' pt_BYOG_StackedBar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_BYOG_StackedBar_ui <- function(id){
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
                                   actionButton(ns("add_la"), "Add area", class= "btn-sm"),
                                   actionButton(ns("resetArea"), "Clear areas", class= "btn-danger btn-sm"),
                               )
                             )

            ),
            bslib::nav_panel("Variable",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               selectInput(ns("var"), label = "Variable",
                                           choices = setNames(reference$obs, translate_codes(reference$obs))[purrr::map_lgl(reference$categorical, isTRUE)]
                                           ),
                               div(style = "text-align: center;",
                                   actionButton(ns("add_var"), "Select variable", class= "btn-sm"),
                                   actionButton(ns("reset_var"), "Clear variable", class= "btn-danger btn-sm")
                                   )
                             )
            ),
            bslib::nav_panel("Population",
                             fillable = F,
                             bslib::card_body(
                               class="graph-controls",
                               selectInput(ns("age"),
                                           label = "Age",
                                           choices = sort_by_name(
                                             setNames(c("all_ages", reference$cats[reference$obs=="age"][[1]][[1]]),
                                                      c("All ages", translate_codes(reference$cats[reference$obs=="age"][[1]][[1]]))
                                                      )
                                             )
                                           ),
                               selectInput(ns("sex"),
                                           label = "Gender",
                                           choices = setNames(c("both", reference$cats[reference$obs=="sex"][[1]][[1]]),
                                                              c("Male or female", translate_codes(reference$cats[reference$obs=="sex"][[1]][[1]]))
                                                              )
                                           ),
                               div(style = "text-align: center;",
                                   actionButton(ns("apply_filt"), "Apply filter", class= "btn-sm"),
                                   actionButton(ns("reset_filt"), "Clear filter", class= "btn-danger btn-sm")
                               )

                             )
            )
          ),
          bslib::card(
            bslib::card_body(
              #max_height = "100px",
              class="graph-controls",
              htmlOutput(ns("selections"))
            )
          )
        ),
        bslib::card(mod_pt_ComparisonGraph_ui(ns("pt_ComparisonGraph_1"), fill=TRUE),
                    full_screen = T,
                    max_height = 800),
        div(style = "text-align: center;",
            actionButton(ns("open_download"), "Download graph...", class= "btn-sm"))

      )
    )
  )
}

#' pt_BYOG_StackedBar Server Functions
#'
#' @noRd
mod_pt_BYOG_StackedBar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filt <- reactiveValues(vars = data.frame("obs"=vector("character"), "varLabels" = vector("character")),
                           areas = data.frame("area"=vector("character"), "geo"=vector("character")),
                           pop = data.frame("age" = "all_ages", "sex"= "both"))


    observeEvent(input$add_var,
                 ignoreInit = T,  {
      filt$vars <- data.frame("obs" = input$var) |> dplyr::mutate(varLabels = translate_codes(obs))
    })

    observeEvent(input$reset_var,
                 ignoreInit = T, {
                   filt$vars <- data.frame("obs"=vector("character"), "varLabels" = vector("character"))
                 })

    observeEvent(input$apply_filt,
                 ignoreInit = T,  {
                   filt$pop <- data.frame("age" = input$age, "sex" = input$sex)
                 })

    observeEvent(input$reset_filt,
                 ignoreInit = T, {
                   filt$pop <- data.frame("age" = "all_ages", "sex"= "both")
                 })

    observeEvent(input$add_la,
                 ignoreInit = T, {
      if(! input$las %in% filt$areas$area){
        filt$areas <- rbind(filt$areas, data.frame("area" = input$las, "geo" = ladSF$lad_name[ladSF$lad == input$las]))
      }
    })

    observeEvent(input$resetArea,
                 ignoreInit = T, {
      filt$areas <- data.frame("area"=vector("character"), "geo"=vector("character"))
    })


    areaDat <- reactive({
      lads <- ladDat |>
        dplyr::right_join(filt$areas, by="area") |>
        dplyr::mutate("lad_name"=geo)

    })

    varDat <- reactive({
      areaDat() |>
        dplyr::right_join(filt$vars, by=dplyr::join_by(obs))
    })

    filteredDat <- reactive({
      varDat() |>
        dplyr::filter(age==filt$pop$age, sex==filt$pop$sex)
    })

    updateDat <- reactive(filteredDat())

    varbl <- reactive(filt$vars$obs)

    filtText <- reactive({
      age <- if(filt$pop$age=="all_ages") NULL else paste("Age", translate_codes(filt$pop$age))
      sex <- if(filt$pop$sex=="both") NULL else translate_codes(filt$pop$sex)
      stringr::str_c(sex, age, sep=", ")
    })



    mod_pt_ComparisonGraph_server("pt_ComparisonGraph_1",
                                  dat = updateDat, var = varbl, subtitle = filtText)




    output$selections <- renderUI(
      HTML(paste0(c("<b>Variable:</b>", ifelse(length(filt$vars$varLabels)>0, filt$vars$varLabels, "None"),
                    "<br><b>Areas:</b>", ifelse(length(filt$areas$geo)>0, paste(filt$areas$geo, collapse = ", "), "None"),
                    "<br><b>Filter:</b>", ifelse(length(filtText())>0, filtText(), "None")
                    )))
    )


    plot <- eventReactive(input$open_download,
                          mod_pt_ComparisonGraph_server("ComparisonGraph_download",
                                                        dat = updateDat,
                                                        var = varbl,
                                                        subtitle = filtText,
                                                        ggoutput=TRUE))

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
# mod_pt_BYOG_StackedBar_ui("pt_BYOG_StackedBar_1")

## To be copied in the server
# mod_pt_BYOG_StackedBar_server("pt_BYOG_StackedBar_1")
