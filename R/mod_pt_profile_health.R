#' pt_profile_health UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pt_profile_health_ui <- function(id){
  ns <- NS(id)
  tagList(
    p(textOutput(ns("intro_text"))),
    bslib::card(
      bslib::card_title("Overview", padding=c(16,16,0)),
      bslib::card_body(mod_pt_ParCoord_ui(ns("pt_ParCoord")),
                       min_height = 150),
    ),
    bslib::card(
      bslib::card_title("Health by area", padding=c(16,16,0)),
      bslib::card_body(radioButtons(ns("health_map_choices"),
                                    "",
                                    choiceValues=c(#"general health",
                                      "SF-12 Physical Component Summary (mean)",
                                      "SF-12 Mental Component Summary (mean)",
                                      "Subjective wellbeing (GHQ): Likert (mean)",
                                      "health limits moderate activities",
                                      "feeling lonely"),
                                    choiceNames = c(#"General health",
                                      "Physical health (SF-12)",
                                      "Mental health (SF-12)",
                                      "Psychological distress (GHQ)",
                                      "Health limits moderate activites: No (%)",
                                      "Feeling lonely: Never/hardly ever (%)"),
                                    inline = T
      )),
      bslib::card_body(mod_pt_AreaMap3_ui(ns("pt_AreaMap3_1")),
                       min_height = 150)),
    bslib::card(
      bslib::card_title("Health by age and gender", padding=c(16,16,0)),
      bslib::card_body(radioButtons(ns("bar_choices"),
                                    "",
                                    choiceValues=c(#"general health",
                                      "SF-12 Physical Component Summary (mean)",
                                      "SF-12 Mental Component Summary (mean)",
                                      "Subjective wellbeing (GHQ): Likert (mean)",
                                      "health limits moderate activities",
                                      "feeling lonely"),
                                    choiceNames = c(#"General health",
                                      "Physical health (SF-12)",
                                      "Mental health (SF-12)",
                                      "Psychological distress (GHQ)",
                                      "Health limits moderate activites: No (%)",
                                      "Feeling lonely: Never/hardly ever (%)"),
                                    inline = T
      )),
      bslib::card_body(mod_pt_DemographicsBar_ui(ns("pt_DemographicsBar")),
                       min_height = 150))

  )
}

#' pt_profile_health Server Functions
#'
#' @noRd
mod_pt_profile_health_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vars <- c(#"general health",
              "SF-12 Physical Component Summary (mean)",
              "SF-12 Mental Component Summary (mean)",
              "Subjective wellbeing (GHQ): Likert (mean)",
              "health limits moderate activities",
              "feeling lonely")

    niceVars <- c(#"General health",
                  "physical health (SF-12)",
                  "mental health (SF-12)",
                  "psychological distress (GHQ)",
                  "health limits moderate activities",
                  "feeling lonely")

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
                         ~dplyr::filter(.x, sex=="both", age=="all ages") %>%
                           dplyr::mutate(geo = .y) %>%
                           dplyr::group_by(obs) %>%
                           dplyr::filter(cat == get_cats(dplyr::first(obs))[1])) %>%
        purrr::list_rbind() #%>%
      # tidyr::pivot_wider(names_from = obs, values_from = c(cat, value), values_fn = first) %>%
      # dplyr::rename_with(~stringr::str_remove(.x, "value_"), .cols = contains("value"))

      return(dat)
    })


    #####
    #text

    area_name <- reactive(unique(lookup_wd_lad$lad_name[lookup_wd_lad$lad==r$profile]))

    intro <- reactive({
      paste0("This page provides an overview of the health and wellbeing of people living in ",
             area_name(),
             ". The SIPHER Synthetic Population aggregated dataset currently includes ",
             length(vars),
             " variables relating to this domain: ",
             var_names(niceVars),
             ". Below you can explore how these variables differ between the different areas within ",
             area_name(),
             ", as well as how they vary by age and gender.", collapse = "")
    })

    output$intro_text <- renderText(intro())





    mod_pt_AreaMap3_server("pt_AreaMap3_1",
                           r=r,
                           varbl = reactive(input$health_map_choices),
                           reactive(get_cats(input$health_map_choices)[1])
    )

    mod_pt_ParCoord_server("pt_ParCoord", dat=compareDat)

    mod_pt_DemographicsBar_server("pt_DemographicsBar",
                           dat=profileDatLA,
                           varbl = reactive(input$bar_choices),
                           reactive(get_cats(input$bar_choices)[1])
    )


  })
}

## To be copied in the UI
# mod_pt_profile_health_ui("pt_profile_health_1")

## To be copied in the server
# mod_pt_profile_health_server("pt_profile_health_1")
