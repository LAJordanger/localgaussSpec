#' Shiny app for a local Gaussian inspection of time series.
#'
#' This function opens an interactive Shiny-application that enables
#' an inspection of the stored data.
#'
#' @template main_dir_arg
#'
#' @param data_dir The directory containing the data of interest.
#'     When this argument is \code{NULL}, then the investigation
#'     starts at the top level without attempting to select a
#'     particular part of the file-hierarchy.
#'
#' @param default_type Specifies whether "par_five" or "par_one" will
#'     be used, i.e. if the local Gaussian auto-correlation should be
#'     based on an approximation based on five or one parameters.
#'     This argument will be ignored when only one alternative is
#'     present.
#'
#' @return An interactive shiny application will be loaded (if
#'     \code{data_dir} points to a relevant directory containing the
#'     desired data).
#'
#' @export

LG_shiny <- function(
    main_dir,
    data_dir = NULL,
    default_type = "par_five") {
###-------------------------------------------------------------------
    ##  Create a spy_report, and use 'LG_bookkeeping' (and thus
    ##  'LG_sanity_checks') to validate the arguments - which also
    ##  loads the object 'TS_content' into the present environment.
    spy_report <- spy()
    LG_bookkeeping(spy_report)
    kill(spy_report)
###-------------------------------------------------------------------
  ##  Define the 'ui'-argument to be delivered to 'shinyApp'.
  ui <- shinyUI(fluidPage(
      ## Application title
      titlePanel("Local Gaussian Correlations and Spectral Densities"),
      ## Define the side-panel
      fluidRow(
          column(
              width = 3,
              conditionalPanel(
                  condition = "(input.switch1 + input.switch2) % 2 == 0",
                  wellPanel(
                      ##  Switch to hide path-controls.
                      actionButton("switch1",
                                   label = "Hide path-controls"),
                      br(),
                      br(), 
                      ##  Present a hoodwinked-message if the
                      ##  'TS_content' file exist without any data.
                      uiOutput("Hoodwinked"),
                      ##  When data exists, add interface for
                      ##  'TS_key', 'TS', 'Approx' and 'Boot_Approx'.
                      uiOutput("TS_key"),
                      uiOutput("TS"), 
                      uiOutput("Approx"),
                      uiOutput("Boot_Approx")
                  )
              ),
              conditionalPanel(
                  condition = "(input.switch2 + input.switch1) % 2 == 1",
                  wellPanel(
                      ##  Switch to show path-controls.
                      actionButton("switch2",
                                   label = "Show path-controls"))
              ),
              ##  A panel for the control of the graphical details,
              ##  i.e. the actionButtons, radioButtons and
              ##  sliderInputs needed for the fine tuning of the
              ##  graphical controls.
              wellPanel(
                  "Select the data to inspect...",
                  ##  Add actionButtons for 'TS_graphic',
                  ##  'Approx_graphic' and 'Spectra_graphic.
                  uiOutput("TCS_type"),
                  ##  Add actionButtons to select the type of spectrum
                  ##  to investigate.
                  uiOutput("spectrum_type"),
                  ##  Add radioButtons for selection of variables 'Vi'
                  ##  and 'Vj' (for multivariate time series) and
                  ##  "local versus global" data.
                  uiOutput("var_local"),
                  ##  Add radioButtons for number of parameters 'p' in
                  ##  the Local Gaussian approximations, point type
                  ##  (on or off diagonal) and selection of
                  ##  bandwidths.
                  uiOutput("p_diag_bw"),
                  ##  Add the 'sliderInput'-interface to be used when
                  ##  selecting the levels/points to be investigated,
                  uiOutput("levels"),
                  ##  Add interface for the selection of the arguments
                  ##  needed in order to compute the different
                  ##  spectral densities.
                  uiOutput("spectrum_arguments"),
                  uiOutput("second_graphical")
              ),
              ###-------------------------------------------------------------------
              ##  Additional buttons.
              wellPanel(
                  ## ##  Explain details about the interface.
                  ## checkboxInput(inputId = "explain_interface",
                  ##               label = "Explanation interface",
                  ##               value = FALSE),
                  ##  Explain details about the plots.
                  checkboxInput(inputId = "explain_plot",
                                label = "Explanation plot",
                                value = TRUE),
                  ##  Get the code for the plots.
                  checkboxInput(inputId = "get_code",
                                label = " Show code for plot",
                                value = FALSE),
              ## ),
              ## ##  A "show the innards of shiny"-marker.
              ## wellPanel(
              ##     checkboxInput(inputId = "show_shiny",
              ##                   label = " Show shiny innards",
              ##                   ## value = FALSE)),
              ##                   value = TRUE)),
              ##  Termination of application.
              ## wellPanel(
                  checkboxInput(inputId = "quit",
                                label = "Quit",
                                value = FALSE),
                  uiOutput("insist_quit"))
          ),
          column(
              width = 9,
              conditionalPanel(
                  condition = "input.get_code == false",
                  plotOutput("graphs")
              ),
              conditionalPanel(
                  condition = "input.get_code == true",
                  "The following code can be used to create the plot in e.g. an article/paper:",
                  verbatimTextOutput("graphs_call")
              ),
              conditionalPanel(
                  condition = "input.explain_interface == true",
                  htmlOutput("Explain_Interface")
              ),
              conditionalPanel(
                  condition = "input.explain_plot == true",
                  htmlOutput("Explain_Plot")
              ),
              conditionalPanel(
                  condition = "input.show_shiny == true",
                  verbatimTextOutput("internal_status")
              )
          )
      )
  ))
    ## This concludes the definition of 'ui'.
###-------------------------------------------------------------------
    ##  Define the 'server'-argument to be delivered to 'shinyApp'.
    server <-
        shinyServer(function(input, output, session) {
            ##  Code to ensure that the app is terminated if the
            ##  web-page is closed without using the stop-button.
            session$onSessionEnded(stopApp)
            ##  Add an insist "I want to quit"-button.
            output$insist_quit <- renderUI(
                if (input$quit) {
                    br()
                    actionButton("insist_quit",
                                 label = "Yes, I DO want to quit!")
                } else {
                    return()
                })
            ##  Perform the termination when required
            observe({
                if(isTRUE(input$insist_quit > 0)) 
                    stopApp()
            })
            ##  Let the interface-master take care of all the details
            ##  related to the updating of the interface.
            observe({
                .Env <- pryr::where("main_dir")
                .Env2 <- pryr::where("output")
                LG_shiny_interface_Master(
                    .env = .Env,
                    .env2 = .Env2)
            })
        })
    ## This concludes the definition of 'server'.
###-------------------------------------------------------------------
    ##  Run 'shinyApp'
    shinyApp(ui = ui,
             server = server)
}
