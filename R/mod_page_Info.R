#' page_Info UI Function
#'
#' @description About page for SIPHER synthetic population.
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

             p("This dashboard enables users to explore the SIPHER Synthetic
             Population for Individuals in Great Britain, 2019-2021. For this
             purpose, we have linked the SIPHER Synthetic Population dataset with
             Understanding Society survey data for individuals and households,
             and created an aggregated version of the resulting linkage. Once
             linked with Understanding Society survey data, the SIPHER Synthetic
             Population provids a “digital twin” of the adult population in Great
             Britain. This enables the dataset to support a wide range of applications
             across policy and research. These applications can include
             exploratory analyses of small areas such as census output areas or
             electoral wards, supplementing traditional administrative sources
             of data. In addition, the SIPHER Synthetic Population can serve as
             an input for microsimulation models through the provision of
             high-quality information on individuals and areas at a granular
             spatial resolution. Capturing multiple key life domains, this
             dashboard allows for an intuitive exploration of spatial patterns
             at different layers of geography. Please note that this dashboard
             does not contain or present any individual-level data."),

             p("The SIPHER Synthetic Population is now available for full
               independant use. The dataset is accompanied by an extensive user
               guide and can be accessed via the ", a("UK Data Service [SN 9277]",
               href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277",
               target="_blank"), " - subject to the End User Licence (EUL) Agreement."),

             p("Key life domains captured in this dashboard include:"),

               tags$ul(

               tags$li("Sociodemographic information which were used when creating
               the SIPHER Synthetic Population"),

               tags$li("Health and wellbeing"),

               tags$li("Income and employment"),

               tags$li("Housing and households"),

               tags$li("Lifestyle, diet, and nutrition")

                        ),

               p("You are permitted to use data obtained from this
               dashboard in your outputs, including reports or presentations.
               In case you would like to use any of the provided data or
               visualisations, please ensure that the dashboard is cited
               correctly. In addition, we recommend to also cite the underlying
               UK Data Service data collections underpinning this
               dashboard,",
                 a("SN 9277",
                   href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277",
                   target="_blank"), "and",
                 a("SN 6614",
                   href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614",
                   target="_blank"), ".",
               "Please see Citation & acknowledgements
               for further guidance on the recommended citation."),

               p("A brief introduction to the SIPHER Synthetic Population can be
                 found in the ",
                  a("SIPHER Product Guide",
                  href="https://www.gla.ac.uk/research/az/sipher/products/syntheticpopulation/",
                  target="_blank"),"."),

             p("A detailed",
               a("User Guide",
                 href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277#!/documentation",
                 target="_blank"),
               "for the SIPHER Synthetic Population is available via the UK Data Service."),

               p("Please direct all questions or feedback marked 'SIPHER Synthetic
                 Population - Dashboard' to sipher@glasgow.ac.uk. Our team is happy
                 to provide support with the interpretation of results and discuss
                 potential project ideas and applications with you."),

                         )
                       )
                       ),

      bslib::nav_panel("About the data", value= "background",

        fluidRow(

          mainPanel(

          h1(strong(style="color:#005398","SIPHER Synthetic Population for Individuals in Great Britain, 2019-2021")),

            div(
            style = "text-align: center",
              HTML('<iframe width="514" height="289" src="https://www.youtube.com/embed/CkiORY7GSLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                       ),

          p("The conditions in which we are born, grow, live, work, and age are
          key drivers of health and health inequalities. To allow for these
          complex real-world relationships and interdependencies to be explored,
          SIPHER has developed an innovative systems science approach. As part
          of our innovative systems science approach, we offer a powerful
          toolbox containing data and methods which enables researchers and
          policymakers to explore existing and prospective policies that shape
          our health and wellbeing."),

          p("One key element of this toolbox is the SIPHER Synthetic Population
          for Individuals in Great Britain, 2019-2021. As a “digital twin” of
          the adult population in Great Britain, the SIPHER Synthetic Population
          supports a wide range of applications which rely on high-quality
          individual-level data at a granular spatial resolution. The SIPHER
          Synthetic Population is now available for full independent use."),
            p("Discover the dataset via the ", a("UK Data Service [SN 9277].",
              href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277",
              target="_blank")
                             ),


                         ) # main panel
                       ) #fluidRow


      ), # bslib::nav_panel


      bslib::nav_panel("Usage and interpretation", value = "SyntheticPopNotes",

            fluidRow(

            mainPanel(

            h1(strong(style="color:#005398","Interpretation of results")),

            p("All data presented in this dashboad has been obtained through the
            aggregation of individual-level data from the SIPHER Synthetic
            Population. For this purpose, we have  linked the SIPHER Synthetic
            Population dataset with Understanding Society survey data for individuals
            and households. Please note that the SIPHER Synthetic Population has
            been created through spatial microsimulation ",
                a("(Flexible Modelling Framework).",
                    href = "https://eprints.ncrm.ac.uk/id/eprint/3177/",
                    target = "_blank"),
            "We therefore advise users of this dashboard to treat all obtained
            results as model output."),

            p("Please note that we provide point estimates only and do not cover
            any uncertainty estimates (e.g., 95% confidence intervals). For
            applications requiring uncertainty estimates, we strongly encourage
            users to obtain these estimates directly from the underlying SIPHER
            Synthetic Population dataset."),

            p("While no uncertainty estimates are provided, it can be assumed that
            the uncertainty of point estimates is likely to be higher across areas
            with a smaller population size (e.g., electoral wards) compared to
            areas with a larger population size (e.g., local authorities). Please
            be aware that in particular for areas with a small population size, the
            uncertainty surrounding point estimates could be high. In addition, we
            recommend that users of this dashboard acknowledge that all results were
            obtained from a synthetic data source, which is subject to a conceptual
            uncertainy due to its statistical creation process."),

            h1(strong(style="color:#005398","Disclaimer")),

            p("All information provided in this dashboard are provided by the SIPHER
            consortium on an 'as is' basis, and without any warranty or liability.
            Except where otherwise noted all SIPHER Synthetic Population Dashboard
            outputs are licensed under ", a("CC BY 4.0 DEED",
                 href="https://creativecommons.org/licenses/by-sa/4.0/",
                            target="_blank"), "."),

            p("The SIPHER Synthetic Population is a synthetic dataset and subject
            to a statistical creation process. As such, the SIPHER Synthetic
            Population dataset does not reflect any 'real' individuals and their
            'true' place of residence. While the dataset represents a novel source
            of data for a range of specific applications, we strongly recommend
            to return to the original Understanding Society survey datasets -
            wherever possible. This recomendation applies in particular with respect
            to standard statistical analyses (e.g., regression analysis, correlations,
            longitudinal analyses), typically performed with the Understanding
            Society survey datasets."),

            p("This dashboard captures aggregate-level information only. No
            aggregate-level information provided through this dashboard would
            ever allow the identification of 'real' individuals and their 'true'
            place of residence – either directly or indirectly. In addition, no
            inferences can or should be made about 'real' survey participants based
            on the aggregate-level data provided in this dashboard."),

            h1(strong(style="color:#005398","Geographic boundary definitions")),

            p("The following boundary definitions were applied throughout:"),

            tags$ul(

            tags$li("Electoral Wards in 2022 boundaries"),

            tags$li("Local Authorities (lower tier; district level) in 2021 boundaries")

                    ),

            p("All geography look-up and shape files were obtained from Open Geography and
            are subject to an Open Government License (OGL 3.0). Please see the separate
            reproducibility pack for further information and links to all utilised files."),


            h1(strong(style="color:#005398","Dashboard reproducibility Pack")),

            p("A reproducibility pack has been created for this dashboard. The reproducibility
            pack contains all aggregate-level data provided in this dashboard, alongside
            all code which was developed for this dashboard."),

            p("A reproducibility pack for this dashboard is available",
              a("via GitHub:",
                href="https://github.com/Dylan-W-Lewis/SIPHERdashboard",
                target="_blank"),"."),

                  ),

                ) # fluid row bracket

      ), # tabpanel bracket

      bslib::nav_panel("Citation and acknowledgements", value = "citation",

          fluidRow(

          mainPanel(

          h1(strong(style="color:#005398","Citation")),

          p("We recommend the following citation for this dashboard: Dylan Lewis,
          Emma Comrie, Andreas Hoehn, Nikolas Lomax, Alison Jane Heppenstall,
          Robin Charles Purshouse, Kashif Zia, Petra Sylvia Meier. (2024). SIPHER
          Synthetic Population for Individuals in Great Britain, 2019-2021 -
          Interactive R-Shiny Dashboard. Data extracted on [DATA, TIME],
          DOI: http://dx.doi.org/10.36399/gla.pubs.328260"),

          p("In addition, we strongly encourage the incusion of citations for the
          two key datasets utilised for the creation of this dashboard:"),

          tags$ul(

          tags$li("Lomax, N., Hoehn, A., Heppenstall, A., Purshouse, R., Wu, G.,
          Zia, K., Meier, P. (2024). SIPHER Synthetic Population for Individuals
          in Great Britain, 2019-2021. [data collection]. University of Essex,
          Institute for Social and Economic Research, Office for National
          Statistics, [original data producer(s)]. University of Essex, Institute
          for Social and Economic Research. SN: 9277,
          DOI: http://doi.org/10.5255/UKDA-SN-9277-1"),

          tags$li("University of Essex, Institute for Social and Economic
          Research. (2023). Understanding Society: Waves 1-13, 2009-2022 and
          Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 18th
          Edition. UK Data Service. SN: 6614,
          DOI: http://doi.org/10.5255/UKDA-SN-6614-19")

                    ),

          h1(strong(style="color:#005398","Acknowledgements")),

          p("This research was conducted as part of the Systems Science in Public Health
            and Health Economics Research - SIPHER Consortium and we thank the whole team
            for valuable input and discussions that have informed this work. We are very
            grateful for the opportunity to work with the UK Household Longitudinal Study
            (Understanding Society). Understanding Society is an initiative funded by the
            Economic and Social Research Council and various Government Departments, with
            scientific leadership by the Institute for Social and Economic Research,
            University of Essex, and survey delivery by the National Centre for Social
            Research (NatCen) and Verian (formerly Kantar Public)."),

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
                        img(src='www/UKPRP_SIPHER_Logo.png', height = "90px", align = "centre")
                ),

            br(),

            br()

              ),

                ) # fluid row bracket

      )
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
