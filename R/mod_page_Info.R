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

            h5(strong(style="color:#005398","Dashboard overview")),

             p("This dashboard enables independent exploration of a spatially
               aggregated version of the SIPHER Synthetic Population for a
               selected set of key life domains and variables. The dashboard
               enables a user to explore the spatially aggregated dataset using
               interactive dashboard features."),

             p("Key life domains include:"),

               tags$ul(

               tags$li("Sociodemographic information (used to create the SIPHER Synthetic Population)"),

               tags$li("Health and wellbeing"),

               tags$li("Income and employment"),

               tags$li("Housing and households"),

               tags$li("Lifestyle, diet, and nutrition")

                        ),

             p("Geographic resolutions available:"),

             tags$ul(

               tags$li("Electoral wards"),

               tags$li("Local authorities"),

               tags$li("Countries"),

               tags$li("Great Britain"),

             ),


             h5(strong(style="color:#005398","User guidance")),

             p("Except where otherwise noted, all SIPHER Synthetic Population
             Dashboard outputs are licensed under ", a("CC BY 4.0 DEED",
             href="https://creativecommons.org/licenses/by-sa/4.0/",
             target="_blank"), "."),

             p("Data and visualisations obtained from this dashboard may be
             included in outputs, such as reports or presentations provided
             that proper acknowledgements are given. Please see our advice on
             citation and acknowledgements for further guidance."),


             h5(strong(style="color:#005398","Dataset")),

             p("Explore the SIPHER Synthetic Population underlying dataset
             and access further resources available for independent use via ",
               a("the UK Data Service",
                 href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277",
                 target="_blank"), "."),

            h5(strong(style="color:#005398","Contact us")),

             p("For dashboard support, assistance  with the interpretation of
             results, user feedback and/or to discuss potential project ideas and
             applications please direct enquiries marked 'SIPHER Synthetic
             Population - Dashboard' to sipher@glasgow.ac.uk.")

                         )
                       )
                       ),

      bslib::nav_panel("About the data", value= "background",

        fluidRow(

          mainPanel(

          h5(strong(style="color:#005398","The SIPHER Synthetic Population")),

            div(
            style = "text-align: center",
              HTML('<iframe width="514" height="289" src="https://www.youtube.com/embed/CkiORY7GSLc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                       ),

          h5(strong(style="color:#005398","Background")),


          p("The conditions in which we are born, grow, live, work, and age are
          key drivers of health and health inequalities. To allow for these
          complex real-world relationships and interdependencies to be explored,
          SIPHER has developed an innovative systems science approach. This
          approach has seen the development of a powerful toolbox containing
          data and methods which enables researchers, analysts, and policymakers
          to explore existing and prospective policies that shape our health and
          wellbeing."),

          h5(strong(style="color:#005398","Applications")),

          p("The SIPHER Synthetic Population offers researchers and policymakers
          access to high-quality data for individuals allowing the identification
          of emerging issues and needs, and the assessment of policy impact.
          The dataset can support a wide range of applications across many
          areas of policy and research. Potential applications include:"),

          tags$ul(

            tags$li("Intuitive spatial analyses of small areas such as census
                    output areas or electoral wards, supplementing traditional
                    administrative sources of data, capturing key life domains."),

            tags$li("Simulations of “what if” scenarios through microsimulation
                    models by providing high-quality information on individuals
                    and areas at a granular spatial resolution."),

          ),

          h5(strong(style="color:#005398","Data acccess")),


          p("The SIPHER Synthetic Population dataset is available for full
          independant use an can be accessed through the ", a("UK Data Service",
              href="https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=9277",
              target="_blank"), "."),



                         ) # main panel
                       ) #fluidRow


      ), # bslib::nav_panel


      bslib::nav_panel("Usage and interpretation", value = "SyntheticPopNotes",

            fluidRow(

            mainPanel(

            h5(strong(style="color:#005398","Interpretation of results")),

            p("All dashboard data has been aggregated from individual-level data
              representing synthetic individuals."),

            p("We advise treating all obtained results as model outputs. Please
            note that we provide point estimates only and do not include uncertainty
            estimates (e.g., 95% confidence intervals). For applications requiring
            uncertainty estimates, we strongly encourage obtaining these directly
            from the underlying SIPHER Synthetic Population dataset."),

            p("While no uncertainty estimates are provided, it can be assumed
            that the uncertainty of point estimates is likely to be higher across
            areas with a smaller population size (e.g., electoral wards) compared
            to areas with a larger population size (e.g., local authorities). For
            areas with a small population size, the uncertainty surrounding point
            estimates could be high. In addition, we recommend acknowledging that
            all results were obtained from a synthetic data source, which is subject
            to a conceptual uncertainty due to its statistical creation process."),

            p("For dashboard support and assistance with the interpretation of
            results, direct enquiries marked 'SIPHER Synthetic Population -
            Dashboard' to: sipher@glasgow.ac.uk."),

            h5(strong(style="color:#005398","Geographic boundary definitions")),

            p("The following boundary definitions were applied throughout:"),

            tags$ul(

            tags$li("Electoral wards in 2022 boundaries"),

            tags$li("Local authorities (lower tier; district level) in 2021 boundaries")

                    ),

            p("All geography look-up and shape files were obtained from Open
            Geography and are subject to an Open Government License (OGL 3.0).
            The SIPHER Synthetic Population Dashboard reproducibility pack provides
            further information and links to all utilised files."),


            h5(strong(style="color:#005398","Disclaimer")),

            p("All information provided in this dashboard is supplied by the
            SIPHER Consortium on an 'as is' basis, and without any warranty or
            liability. Except where otherwise noted, all SIPHER Synthetic
            Population Dashboard outputs are licensed under ",
              a("CC BY 4.0 DEED", href="https://creativecommons.org/licenses/by-sa/4.0/",
                target="_blank"), "."),

            p("This dashboard presents aggregate-level information only, and
            does not capture any existing individuals and their true place of
            residence. No inferences can or should be made about real individuals
            or Understanding Society survey participants with all results
            understood and treated as 'model outputs'."),

            p("The SIPHER Synthetic Population is a synthetic dataset created
            through a statistical process and does not represent any 'real'
            individuals or their 'true' place of residence. While the dataset
            is a novel source of data for a range of specific applications, we
            strongly recommend using the original Understanding Society survey
            datasets for all standard statistical analyses (e.g., regression
            analysis, correlations, longitudinal analyses) whenever possible."),


            h5(strong(style="color:#005398","Dashboard reproducibility pack")),

            p("A reproducibility pack has been created for this dashboard. The
            reproducibility pack contains all aggregate-level data provided in
            this dashboard, alongside all code which was developed to support
            dashboard functionality. The reproducibility pack is available
            via", a("Zenodo:",
                href="https://github.com/Dylan-W-Lewis/SIPHERdashboard",
                target="_blank"),"."),

                  ),

                ) # fluid row bracket

      ), # tabpanel bracket

      bslib::nav_panel("Citation and acknowledgements", value = "citation",

          fluidRow(

          mainPanel(

          h5(strong(style="color:#005398","Citation")),

          p("We kindly request that all outputs derived using the SIPHER Synthetic
          Population Dashboard are clearly acknowledged"),

          p("Informal output utilising the dashboard's data and visualisations
          please include the following statement: 'Generated using the SIPHER
          Consortium Synthetic Population Dashboard funded by the UK Prevention
          Research Partnership'"),

          p("For all formal output, please utilse the following citation:
          'D. Lewis, E. Comrie, A. Hoehn, N. Lomax, A. Heppenstall, R. Purshouse,
          K. Zia, P. Meier. (2024). SIPHER Synthetic Population for Individuals
          in Great Britain, 2019-2021 - Interactive R-Shiny Dashboard. Data
          extracted on [DATA, TIME], DOI'"),

          p("In addition, for all formal output, we strongly encourage the
          inclusion of citations for the two key datasets utilised in the
          creation of this dashboard:"),

          tags$ul(

          tags$li("Lomax, N., Hoehn, A., Heppenstall, A., Purshouse, R., Wu, G.,
          Zia, K., Meier, P. (2024). SIPHER Synthetic Population for Individuals
          in Great Britain, 2019-2021. [data collection]. University of Essex,
          Institute for Social and Economic Research, Office for National
          Statistics, [original data producer(s)]. University of Essex, Institute
          for Social and Economic Research. SN: 9277,
          DOI:",
          a(" http://doi.org/10.5255/UKDA-SN-9277-1 ",
                    href="http://doi.org/10.5255/UKDA-SN-9277-1",
                    target="_blank")),

          tags$li("University of Essex, Institute for Social and Economic
          Research. (2023). Understanding Society: Waves 1-13, 2009-2022 and
          Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 18th
          Edition. UK Data Service. SN: 6614,
          DOI:",
            a(" http://doi.org/10.5255/UKDA-SN-6614-19 ",
                    href="http://doi.org/10.5255/UKDA-SN-6614-19",
                    target="_blank")),
          ),


          h5(strong(style="color:#005398","Acknowledgements")),

          p("This research was conducted as part of the Systems Science in
          Public Health and Health Economics Research - SIPHER Consortium and
          we thank the whole team for valuable input and discussions that have
          informed this work. We are very grateful for the opportunity to work
          with the UK Household Longitudinal Study (Understanding Society).
          Understanding Society is an initiative funded by the Economic and
          Social Research Council and various Government Departments, with
          scientific leadership by the Institute for Social and Economic Research,
          University of Essex, and survey delivery by the National Centre for
          Social Research (NatCen) and Verian (formerly Kantar Public)."),

          h5(strong(style="color:#005398","Funding")),

          p("This work by the",
          a(" SIPHER Consortium ",
                href="https://www.gla.ac.uk/research/az/sipher/",
                target="_blank"),
          "was supported by the",
          a(" UK Prevention Research Partnership ",
            href="https://ukprp.org/",
            target="_blank"),
          "(MR/S037578/2), which is funded by the British
          Heart Foundation, Cancer Research UK, Chief Scientist Office of the
          Scottish Government Health and Social Care Directorates, Engineering
          and Physical Sciences Research Council, Economic and Social Research
          Council, Health and Social Care Research and Development Division
          (Welsh Government), Medical Research Council, National Institute for
          Health Research, Natural Environment Research Council, Public Health
          Agency (Northern Ireland), The Health Foundation and Wellcome.")

              ),

        ) # fluid row bracket

      )
    ),


    div(style = "margin: auto; text-align: center",
        img(src='www/UKPRP_SIPHER_Logo.png', height = "200rem", align = "centre")
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
