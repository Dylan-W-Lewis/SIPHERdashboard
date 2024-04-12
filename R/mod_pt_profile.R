#' pt_profile UI Function
#'
#' @description Produces a profile of a local authority for a given set of variables.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param topic Description of profile topic
#' @param vars Variables to be used
#' @param varNames Optional user-friendly variable names
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_profile_ui <- function(id, topic, vars, varNames){
  ns <- NS(id)
  tagList(
    p(textOutput(ns("intro_text"))),
    bslib::card(
      bslib::card_title("Overview", padding=c(16,16,0)),
      bslib::card_body(mod_pt_ParCoord_ui(ns("pt_ParCoord")),
                       min_height = 150),
    ),
    bslib::card(
      bslib::card_title(paste0(stringr::str_to_sentence(topic), " by area"), padding=c(16,16,0)),
      bslib::card_body(radioButtons(ns("map_choices"),
                                    "",
                                    choiceValues=vars,
                                    choiceNames =varNames,
                                    inline = T
      )),
      bslib::card_body(mod_pt_AreaMap3_ui(ns("pt_AreaMap3")),
                       min_height = 150)),
    bslib::card(
      bslib::card_title(paste0(stringr::str_to_sentence(topic), " by age and gender"), padding=c(16,16,0)),
      bslib::card_body(radioButtons(ns("bar_choices"),
                                    "",
                                    choiceValues=vars,
                                    choiceNames =varNames,
                                    inline = T
      )),
      bslib::card_body(mod_pt_DemographicsBar_ui(ns("pt_DemographicsBar")),
                       min_height = 150))

  )
}

#' pt_profile Server Functions
#'
#' @noRd
mod_pt_profile_server <- function(id, topic, vars, varNames= NULL, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ##### setup

    if(is.null(varNames)){varNames<-make_var_labels(vars)}

    area_name <- reactive(unique(lookup_wd_lad$lad_name[lookup_wd_lad$lad==r$profile]))

    profileDatWard <- reactive({
      wardDat %>% dplyr::filter(area %in% lookup_wd_lad$ward[lookup_wd_lad$lad==r$profile],
                                obs %in% vars)
    })
    profileDatLA <- reactive({
      ladDat %>% dplyr::filter(area == r$profile,
                               obs %in% vars)
    })
    profileDatGB <- reactive({
      gbDat %>% dplyr::filter(obs %in% vars)
    })

    compareDat <- reactive({
      dat <- list(profileDatWard(),  profileDatLA(),  profileDatGB())
      names(dat) <- c("Wards", area_name(), "GB average")
      dat <- purrr::imap(dat,
                         ~.x %>%
                           dplyr::filter(if_any(matches("sex"), ~.x=="both")) %>%
                           dplyr::filter(if_any(matches("age"), ~.x=="all_ages")) %>%
                           dplyr::mutate(geo = .y)) %>%
        purrr::list_rbind() %>%
        dplyr::left_join(., reference[, c("obs", "cat_reference")], by="obs") %>%
        dplyr::filter(cat==cat_reference)
      # tidyr::pivot_wider(names_from = obs, values_from = c(cat, value), values_fn = first) %>%
      # dplyr::rename_with(~stringr::str_remove(.x, "value_"), .cols = contains("value"))
      return(dat)
    })


    #####
    #text


    intro <- reactive({
      paste0("This page provides an overview of the ",
             topic,
             " of people living in ",
             area_name(),
             ". The SIPHER Synthetic Population aggregated dataset currently includes ",
             length(vars),
             " variables relating to this domain: ",
             var_names(varNames),
             ". Below you can explore how these variables differ between the different areas within ",
             area_name(),
             ", as well as how they vary by age and gender.", collapse = "")
    })

    output$intro_text <- renderText(intro())


    ##### graphs


    mod_pt_AreaMap3_server("pt_AreaMap3",
                           r=r,
                           varbl = reactive(input$map_choices),
                           reactive(get_cats(input$map_choices, ref_only = T))
    )

    mod_pt_ParCoord_server("pt_ParCoord", dat=compareDat, varNames)

    mod_pt_DemographicsBar_server("pt_DemographicsBar",
                                  dat=profileDatLA,
                                  varbl = reactive(input$bar_choices),
                                  reactive(get_cats(input$bar_choices, ref_only = T))
    )

  })
}

## To be copied in the UI
# mod_pt_profile_ui("pt_profile_1")

## To be copied in the server
# mod_pt_profile_server("pt_profile_1")
