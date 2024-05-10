#' page_Info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_page_Info_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::navset_pill_list(
      id = ns("tabset"),
      widths = c(3, 9),
      well = FALSE,

      bslib::nav_panel("Overview", value = "dash",
                       fluidRow(

                         mainPanel(

                           h1(strong(style="color:#005398","Overview")),

                           p("This dashboard provides an aggregated version of the SIPHER Synthetic
                        Population for Individuals 2019-2021. By providing a “digital twin” of the adult
                        population in Great Britain, the SIPHER Synthetic Population supports a wide
                        range of applications across different fields of policy and research. These
                        applications include, for example, simulations of “what if” scenarios through
                        static/dynamic microsimulation models which require high-quality information on
                        individuals and areas at a granular spatial. Capturing different key life
                        domains, the dashboard allows for an intuitive exploration of spatial patterns
                        at different layers of geography. The SIPHER Synthetic Population, the dataset
                        underpinning this dashboard is available via the [LINK EXTERNAL: Available via
                        the UK Data Service] - subject to the End User Licence Agreement terms and
                        conditions."),

                        p("Key life domains captured in this dashboard include"),

                        tags$ul(

                          tags$li("Constraint dimensions (= sociodemographic information used  when
                           creating the SIPHER Synthetic Population)"),

                          tags$li("Health & wellbeing"),

                          tags$li("Satisfaction with life, occupation"),

                          tags$li("Income and finances"),

                          tags$li("Deprivation"),

                          tags$li("Housing"),

                          tags$li("Lifestyle, diet, and nutrition"),

                          tags$li("Caring responsibilities")

                        ),

                        p("You are permitted to use data obtained from this dashboard in your outputs,
                        including reports or presentations. Please see Citation & Acknowledgements
                        for information on the suggested citation."),

                        p("Further technical information on the SIPHER Synthetic Population can be found
                        in the respective User Guide – [LINK EXTERNAL: Available via the UK Data Service]."),

                        p("Please direct questions or feedback marked 'Synthetic Population Dashboard'
                        to sipher@glasgow.ac.uk"),

                         )
                       )
                       ),

      bslib::nav_panel("About the Data", value= "background",

                       fluidRow(

                         mainPanel(

                           h1(strong(style="color:#005398","The SIPHER Synthetic Population Dataset")),

                           div(
                             style = "text-align: center",
                             HTML('<iframe width="514" height="289" src="https://www.youtube.com/embed/CkiORY7GSLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                           ),

                           p("The conditions in which we are born, grow, live, work, and age are key drivers of health and health
                                       inequalities. To allow for these complex real-world relationships and interdependencies to be explored,
                                       the SIPHER has developed an innovative systems science approach. This approach offers a powerful toolbox
                                       which enables researchers and policymakers to explore of diverse policies that shape our health and wellbeing."),

                           p("One key product of this toolbox is the SIPHER Synthetic Population for Individuals in Great Britain 2019-2021.
                                      As a “digital twin” of the adult population in GB, the SIPHER Synthetic Population supports a wide range of applications
                                      which relay on high-quality individual-level data at a granular spatial resolution.
                                      The SIPHER Synthetic Population is available for full independent use [LINK EXTERNAL via the UK Data Service] "),


                         ) # main panel
                       ) #fluidRow


      ), # bslib::nav_panel


      bslib::nav_panel("Data and Interpretation", value = "SyntheticPopNotes",

                fluidRow(

                  mainPanel(

                    h1(strong(style="color:#005398","Interpretation of Results")),

                    p("All data presented in this dashboard have been obtained from the SIPHER
                        Synthetic Population through the aggregation of individual-level data. The
                        SIPHER Synthetic Population has been created using a spatial microsimulation
                        algorithm (simulated annealing), and via the Flexible Modelling Framework [LINK REQUIRED]. We
                        therefore advise users of this Dashboard  to treat all data obtained from this
                        dashboard as “model output” obtained from a synthetic dataset."),

                    p("In the dashboard, we provide point estimates only and do not cover any
                        uncertainty estimates (e.g., 95% Confidence Intervals). For applications where
                        uncertainty estimates are required, we strongly encourage to obtain these
                        estimates directly from the underlying SIPHER Synthetic Population dataset."),

                    p("While no uncertainty estimates are provided, it can be assumed that the
                        uncertainty of point estimates is likely to be higher across areas with a
                        smaller population size (e.g., electoral wards) compared to areas with a larger
                        population size (e.g., local authorities). In particular for areas with small
                        population sizes, the uncertainty surrounding point estimates could be high."),

                    h1(strong(style="color:#005398","Disclaimer")),

                    p("All information provided in this Dashboard is provided by the SIPHER
                        Consortium on an 'as is' basis, and without any warranty or liability."),

                    p("The creation of the SIPHER Synthetic Population is based on data of the
                        Understanding Society survey. As such, the underlying SIPHER Synthetic
                        Population dataset does not reflect any 'real' individuals. We strongly advise
                        using the original Understanding Society survey datasets wherever this is
                        possible, in particular when seeking to understand associations or causal
                        structures, cross-sectionally or over time for the UK population. We generally
                        recommend to  If no granular area-level resolution is required [TYPO]"),

                    p("The dashboard captures aggregate-level information only. No information
                        provided in this dashboard would allow the identification of 'real' individuals
                        – neither directly nor indirectly. No inferences can and should ever be made
                        about 'real' individuals, including Understanding Society survey respondents,
                        based on the aggregate-level data provided in this dashboard."),


                    h1(strong(style="color:#005398","Boundary Definitions")),

                    p("We applied the following boundary definitions throughout:"),

                    tags$ul(

                      tags$li("LSOAs and MSOAs in 2021 boundaries"),

                      tags$li("Electoral Wards in 2022 boundaries"),

                      tags$li("Local Authorities in 2021 boundaries")

                    ),

                    p("All geography look-up and shape files were obtained from Open Geography and
                        are subject to an Open Government License (OGL 3.0). Please see the separate
                        reproducibility pack for further information and link to all utilised files."),


                    h1(strong(style="color:#005398","Reproducibility Pack")),

                    p("We have created a reproducibility pack for this dashboard. The
                        reproducibility pack contains all aggregate-level data provided in this
                        dashboard, alongside all code which we have developed for this dashboard. The
                        reproducibility pack is [LINK EXTERNAL: available via the Open Science
                        Framework]."),

                    br(),

                    br(),

                    img(src='sipher_logo.jpeg', height = "90px", align = "centre"),

                    br(),

                    br(),

                    br(),

                  ),

                ) # fluid row bracket

      ), # tabpanel bracket

      bslib::nav_panel("Citation and Acknowledgements", value = "citation",

                fluidRow(

                  mainPanel(

                    h1(strong(style="color:#005398","Suggested Citation")),

                    p("We suggest the following citation for the dashboard:
                        Dylan Lewis, Emma Comrie, Andreas Hoehn, David Innes, and Petra Meier (2024):
                        An R-Shiny Dashboard for the SIPHER Synthetic Population for Individuals in
                        Great Britain 2019-2021. DOI: TBD. Data extracted on [DATA, TIME]."),

                    p("In addition, we strongly encourage the citation of the two key datasets which
                        have been utilised to create this dashboard. These two key datasets are:"),

                    tags$ul(

                      tags$li("University of Essex, institute for social and economic research. (2022).
                        Understanding society: Waves 1-12, 2009-2021 and harmonised BHPS: Waves 1-18,
                        1991-2009. [Data collection]. 17th edition. UK Data Service. SN: 6614,
                        http://doi.org/10.5255/UKDA-SN-6614-18."),

                      tags$li("[SIPHER Synthetic Population: exact citation available after acceptance]")

                    ),

                    h1(strong(style="color:#005398","Acknowledgements")),

                    p("This research was conducted as part of the Systems Science in Public Health
                        and Health Economics Research - SIPHER Consortium and we thank the whole team
                        for valuable input and discussions that have informed this work. Understanding
                        Society is an initiative funded by the Economic and Social Research Council and
                        various Government Departments, with scientific leadership by the Institute for
                        Social and Economic Research, University of Essex, and survey delivery by the
                        National Centre for Social Research (NatCen) and Verian (formerly Kantar Public)."),

                    h1(strong(style="color:#005398","Funding")),

                    p("This work by the SIPHER Consortium was supported by the UK Prevention Research
                        Partnership (MR/S037578/2), which is funded by the British Heart Foundation,
                        Cancer Research UK, Chief Scientist Office of the Scottish Government Health and
                        Social Care Directorates, Engineering and Physical Sciences Research Council,
                        Economic and Social Research Council, Health and Social Care Research and
                        Development Division (Welsh Government), Medical Research Council, National
                        Institute for Health Research, Natural Environment Research Council, Public
                        Health Agency (Northern Ireland), The Health Foundation and Wellcome."),

                    br(),

                    br(),

                    div(style = "margin: auto; text-align: center",
                        span(img(src='www/sipher_logo.png', height = "90px", align = "centre"),
                             img(src='www/UKPRP.png', height = "90px", align = "centre"))
                    ),

                    br(),

                    br()

                  ),

                ) # fluid row bracket

      ), # bslib::nav_panel bracket

      # bslib::nav_panel("Dashboard Functionality and Features", value= "features",
      #
      #           fluidRow(
      #
      #             mainPanel(
      #               "In development"
      #
      #
      #             ) # main panel
      #           ) #fluidRow
      #
      #
      # ) # bslib::nav_panel
    )

  )
}

#' page_Info Server Functions
#'
#' @noRd
mod_page_Info_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$tabset, {
      r$info_tab <- input$tabset
    })

    observeEvent(r$info_tab, {
      if(!identical(r$info_tab, input$topBar)){
        #message(paste("change page to", r$info_tab, "from", input$topBar))
        bslib::nav_select(id = "tabset",
                          selected = r$info_tab)
      }
    })

  })
}

## To be copied in the UI
# mod_page_Info_ui("page_Info_1")

## To be copied in the server
# mod_page_Info_server("page_Info_1")
