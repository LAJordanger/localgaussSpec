################################################################################
#'
#' Shiny app for a local Gaussian inspection of time series.
#'
#' This function opens an interactive Shiny-application that can be
#' used for an inspection of the stored data.  If I succeed in what I
#' want to do, the function should also be able to update the
#' corresponding information, such that functions later on can take as
#' arguments stuff saved from a previous interactive inspection.
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


#####  2014-11-19
##  After the realisation that it should be quite feasible for this
##  program to read and save files at the specified positions, I now
##  think about creating a unified inspection-function, that hopefully
##  could deal with all the cases of interest, and also be able to
##  update the info-object at desired places.  I think I would like to
##  get this to deal with three levels of data, starting from the top
##  with the file 'TS_content.Rda", then have the possibility to
##  select a time series for closer inspection and go to that time
##  series "info.Rda" - where hopefully it can be listed some more
##  information about the grisly data files at the bottom level for
##  which we might want to perform a closer inspection.

LG_shiny <- function(
    main_dir,
    data_dir = NULL,
    default_type = "par_five") {
###-------------------------------------------------------------------
    ##  Create a spy_report, and use 'LG_bookkeeping' (and thus
    ##  'LG_sanity_cheks') to validate the arguments - which also
    ##  loads the object 'TS_content' into the present environment.
    spy_report <- spy()
    LG_bookkeeping(spy_report)
    kill(spy_report)
#####  TASK?: Add functionality to 'LG_bookkeeping' and
#####  'LG_sanity_checks' that can investigate if an ordinary
#####  directory has been given (no names), and attempting to rectify
#####  the lack of information available when necessary.
###-------------------------------------------------------------------
    ##  Create a logical object to check that data is present.
    data_exists <- length(TS_content) > 0
    ##  Extract a list of keys and time-series from 'TS_content'.
    TS_overview <- structure(
        .Data = lapply(
            X = seq_along(TS_content),
            FUN = function(i) 
                names(TS_content[[i]])),
        .Names = names(TS_content))
###-------------------------------------------------------------------
    ##  Extract information about the pre-selected stuff specified in
    ##  'data_dir'.  Note: Adjust the names to allow subsetting based
    ##  on the values stored in 'LG_default', i.e. "TS", "Approx",
    ##  "Spectra", "Boot_Approx" and "Boot_Spectra".
    pre_selected <- structure(
        .Data = as.list(data_dir),
        .Names = LG_default$folder_defaults[names(data_dir)])
    ##  Add 'TS_key' and 'Approx_branch' when relevant.
    if (! is.null(data_dir))
        pre_selected$TS_key <- local(expr = {
            .match <- vapply(
                X = TS_overview,
                FUN = function(x) 
                    pre_selected$TS %in% x,
                FUN.VALUE = logical(1))
            names(.match)[.match]
        })
    ##  Add value for 'Approx_branch', when relevant.
    if (! is.null(pre_selected[["Spectra"]])) 
        pre_selected$Approx_branch <- "Spectra"
    if (! is.null(pre_selected[["Boot_Approx"]])) 
        pre_selected$Approx_branch <- "Bootstrap data"
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  In order to ease the update of the interface, a wrapper function
###  will be created to take care of the creation of the
###  'selectInput'-functions.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create the wrapper around 'selectInput'.
    wrapper_selectInput <- function(
        inputId,
        names,
        pre_selected) {
        ##  Initiate basic keywords.
        word <- switch(
            EXPR = inputId,
            TS_key = "group",
            TS = "time series",
            Approx = "approximation",
            Spectra = "spectra",
            Boot_Approx = "bootstrap approximation",
            Boot_Spectra = "bootstrap spectra")
        ##  Create the descriptive text to be used in selector.
        descriptive_text <- paste(
            "Select",
            word,
            sep = " ")
        ##  Create the label to be used (text above selector).
        label <- ifelse(
            test = length(names) == 1,
            yes = paste(
                "Auto-selected the only available",
                word,
                sep = " "),
            no = paste(
                length(names),
                " ",
                word,
                if (inputId %in% c("TS_key", "Approx", "Boot_Approx"))
                    "s",
                " available, pick your choice",
                sep = ""))
        ##  Create the default selected value.
        if (! is.null(pre_selected[[inputId]])) {
            selected <- pre_selected[[inputId]]
        } else 
            selected <- ifelse(
                test = length(names) == 1,
                yes = names,
                no = descriptive_text)
        ##  Create the value for the 'choices'-argument.
        choices <- c(descriptive_text, names)
        ##  Override the previous values when no data was found.
        if (length(names) == 0) {
            label <- paste(
                "No ",
                word,
                if (inputId %in% c("TS_key", "Approx", "Boot_Approx"))
                    "s",
                " detected",
                sep = "")
            choices <- "Nothing here to select..."
            selected <- NULL
        }
        ## Return the desired function
        return(selectInput(
            inputId = inputId,
            label = label,
            choices = choices,
            selected = selected,
            width = 333))
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The user interface is dynamic and created from the 'server', the
###  rule of naming is that the same names should occur in both
###  'input' and 'output', i.e. we should have "TS", "Approx",
###  "Spectra", "Boot_Approx" and "Boot_Spectra" as common components.
#############---------------------------------------------------------
###-------------------------------------------------------------------
  ##  Define the 'ui'-argument to be delivered to 'shinyApp'.
  ui <- shinyUI(fluidPage(
      ## Application title
      titlePanel("Local Gaussian Spectral Densities"),
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
                      ##  When data exists, add interface for 'TS_key'.
                      uiOutput("TS_key"),
                      ##  When valid 'TS_key', add interface for 'TS'.
                      uiOutput("TS"), 
                      ##  When 'info' loaded, add interface for 'Approx'.
                      uiOutput("Approx"),
                      ##  Add a branching selector for 'Approx'-result.
                      uiOutput("Approx_branch"),
                      ##  When branch selected as 'Spectra'.
                      uiOutput("Spectra"),
                      ##  When branch selected as 'Boot_Approx'.
                      uiOutput("Boot_Approx"),
                      ##  Add selector for 'Boot_Spectra'.
                      uiOutput("Boot_Spectra")
                  )
              ),
              conditionalPanel(
                  condition = "(input.switch2 + input.switch1) % 2 == 1",
                  wellPanel(
                      ##  Switch to show path-controls.
                      actionButton("switch2",
                                   label = "Show path-controls"))
              ),
              ##  A panel for the control of the graphical details.
              wellPanel(
                  "Select the data to inspect...",
                  fluidRow(
                      ##  Add actionButton for 'TS_graphic'.
                      uiOutput("TS_graphic"), 
                      ##  Add actionButton for 'Approx_graphic'.
                      uiOutput("Approx_graphic"),
                      ##  Add actionButton for 'Spectra_graphic'.
                      uiOutput("Spectra_graphic"),
                      ##  Add actionButton for 'Boot_Approx_graphic'.
                      uiOutput("Boot_Approx_graphic"),
                      ##  Add actionButton for 'Boot_Spectra_graphic'.
                      uiOutput("Boot_Spectra_graphic") ),
                  ##  The next part gives a list of graphical
                  ##  controls, that will depend on the selected data.
                  uiOutput("graphical_controls")
              ),
###-------------------------------------------------------------------
              ##  Additional buttons.
              wellPanel(
                  ##  Explain details about the interface.
                  checkboxInput(inputId = "explain_interface",
                                label = "Explanation interface",
                                value = FALSE),
                  ##  Explain details about the plots.
                  checkboxInput(inputId = "explain_plot",
                                label = "Explanation plot",
                                value = TRUE),
                  ##  Get the code for the plots.
                  checkboxInput(inputId = "get_code",
                                label = " Show code for plot",
                                value = FALSE)),
              ##  A "show the innards of shiny"-marker.
              wellPanel(
                  checkboxInput(inputId = "show_shiny",
                                label = " Show shiny innards",
                                value = FALSE)),
              ##  Termination of application.
              wellPanel(
                  checkboxInput(inputId = "quit",
                                label = "Enough, stop this crap!",
                                value = FALSE),
                  uiOutput("insist_quit"))
          ),
          column(
              width = 9,
              conditionalPanel(
                  condition = "input.get_code == false",
                  ##  The next part will depend on the selected data.
                  plotOutput("graphs")
              ),
              conditionalPanel(
                  condition = "input.get_code == true",
                  "The following code can be used to create the plot in e.g. an article/paper:",
                  verbatimTextOutput("graphs_call")
              ),
              conditionalPanel(
                  condition = "input.explain_interface == true",
                  ##  Present information about the interface
                  htmlOutput("Explain_Interface")
                  ## verbatimTextOutput("Explain_Interface")
              ),
              conditionalPanel(
                  condition = "input.explain_plot == true",
                  ##  Present information about the plot.
                  htmlOutput("Explain_Plot")
                  ## verbatimTextOutput("Explain_Interface")
              ),
              conditionalPanel(
                  condition = "input.show_shiny == true",
                  verbatimTextOutput("internal_status")
              )
          )
      )
  ))
    ## This concludes the definition of 'ui'.
#################################-------------------------------------    
    ##  Define the 'server'-argument to be delivered to 'shinyApp'.
  server <-
      shinyServer(function(input, output, session) {
          ##  Code to ensure that the app stoppes if the web-page is
          ##  closed without using the stop-button.
          session$onSessionEnded(stopApp)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The aim of this function is to make this interactive application
###  able to deal with all the cases of interest, which implies that
###  the user interface must be completely dynamic.  In order to make
###  the code a tad bit more transparent, the naming conventions will
###  be to mirror 'input' and 'output' to a high degree.  Moreover, to
###  avoid a lot of "select the only available option"-steps in the
###  interface, the aim is to auto-select when no alternatives are
###  available.
#############---------------------------------------------------------
###-------------------------------------------------------------------            

###-------------------------------------------------------------------
#############---------------------------------------------------------
###  In order to get stuff working, we need to instruct 'observe' with
###  regard to the priorities to use, and we need a vector specifying
###  these in order to ease the testing of what order we should use.
          observe_priorities <- c(
              graph_changes = 50,
              TS_graphical_changes = -1,
              path_changes = 40,
              backup_show_graphical = 1000,
              create_input_copy = 9999,
              show_graphical = 20,
              show_graphical_2 = 20)

          
###-------------------------------------------------------------------
#############---------------------------------------------------------
### Let's start with the end: Code for termination of the application.
### Use 'renderUI' to add a new button to verify that a termination of
### the present application indeed is the desired action.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Add an insist "I want to quit"-button.
          output$insist_quit <- renderUI(
              if (input$quit) {
                  br()
                  actionButton("insist_quit",
                               label = "Yes, I DO want to quit!")
              } else {
                  return()
              })
          ##  Perform the termination.
          observe({
              nested_if( 
                  if_list = list(
                      ! identical(input$insist_quit, NULL),
                      (input$insist_quit > 0)), 
                  expr_all_TRUE = stopApp())
          })


###-------------------------------------------------------------------
#############---------------------------------------------------------
### Messages related to lack of data in the specified target directory
### can be useful, and those components are collected here.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Hoodwinked information, there's no data there.
          output$Hoodwinked <- renderText({
              if (! data_exists)
                  paste("You have been hoodwinked!",
                        br(),
                        "An empty list was found in the file '",
                        paste(c(main_dir,
                                LG_default$content_file_name),
                              collapse = .Platform$file.sep),
                        "'.",
                        sep = "")
          })
          
          
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  To make the code a tiny bit more transparent, a bunch of reactive
###  logical values are collected here.  The naming convention is that
###  values ending with '_include' turns on and off different parts of
###  the interface, whereas those ending with '_load' governs whether
###  or not information will be loaded from files.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Logic value: Interface 'TS'.
          TS_include <- reactive(
              nested_if(
                  if_list = list(
                      ! identical(input$TS_key, NULL),
                      any(names(TS_content) == input$TS_key)),
                  expr_not_all_TRUE = FALSE) )
          
          ##  Logic value: Load 'info'-object.
          info_load <- reactive(
              nested_if(
                  if_list = list(
                      ! identical(input$TS, NULL),
                      ! str_detect(string = toupper(input$TS),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )

          ##  Logic value: Interface 'Approx'.
          Approx_include <- reactive(
              nested_if(
                  if_list = list(
                      ! identical(input$TS_key, NULL),
                      ! str_detect(string = toupper(input$TS_key),
                                   pattern = toupper("Select")),
                      info_load()),
                  expr_not_all_TRUE = FALSE) )
          
          ##  Logic value:  Interface 'Approx_branch' 
          Approx_branch_include <- reactive(
              nested_if(
                  if_list = list(
                      Approx_include(),
                      ! identical(input$Approx, NULL),
                      ! str_detect(string = toupper(input$Approx),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )

          ##  Logic value: Interface 'Spectra'
          Spectra_include <- reactive(
              nested_if(
                  if_list = list(
                      Approx_branch_include(),
                      ! identical(input$Approx_branch, NULL),
                      str_detect(string = input$Approx_branch,
                                 pattern = "Spectra")),
                  expr_not_all_TRUE = FALSE) )

          ##  Logic value: Interface 'Boot_Approx'
          Boot_Approx_include <- reactive(
              nested_if(
                  if_list = list(
                      Approx_branch_include(),
                      ! identical(input$Approx_branch, NULL),
                      str_detect(string = input$Approx_branch,
                                 pattern = "Bootstrap data")),
                  expr_not_all_TRUE = FALSE) )
          
          ##  Logic value: Interface 'Boot_Spectra'
          Boot_Spectra_include <- reactive(
              nested_if(
                  if_list = list(
                      Boot_Approx_include(),
                      ! identical(input$Boot_Approx, NULL),
                      ! str_detect(string = toupper(input$Boot_Approx),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )
          

#####-----------------
###  Buttons to inspect the data by graphical means, When this has
###  been tested, they can be inserted at the natural positions in the
###  list above.
#####-----------------

          ##  Logic value: Interface 'TS_graphic'
          TS_graphic_include <- reactive(
              Approx_include() )
          
          ##  Logic value: Interface 'Approx_graphic'
          Approx_graphic_include <- reactive(
              Approx_branch_include() )
          
          ##  Logic value: Interface 'Spectra_graphic'
          Spectra_graphic_include <- reactive(
              nested_if(
                  if_list = list(
                      Spectra_include(),
                      ! identical(input$Spectra, NULL),
                      ! str_detect(string = toupper(input$Spectra),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )
          
          ##  Logic value: Interface 'Boot_Approx_graphic'
          Boot_Approx_graphic_include <- reactive(
              nested_if(
                  if_list = list(
                      Boot_Approx_include(),
                      ! identical(input$Boot_Approx, NULL),
                      ! str_detect(string = toupper(input$Boot_Approx),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )
          
          ##  Logic value: Interface 'Boot_Spectra_graphic'
          Boot_Spectra_graphic_include <- reactive(
              nested_if(
                  if_list = list(
                      Boot_Spectra_include(),
                      ! identical(input$Boot_Spectra, NULL),
                      ! str_detect(string = toupper(input$Boot_Spectra),
                                   pattern = toupper("Select"))),
                  expr_not_all_TRUE = FALSE) )
          
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The logical values defined above will now be used in order to
###  create the interface and load different files.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Select input based on TS_content.
          output$TS_key <- renderUI(
              expr =
                  if (data_exists)
                      wrapper_selectInput(
                          inputId = "TS_key",
                          names = names(TS_content),
                          pre_selected = pre_selected))
          
          ##  Create selector for 'TS', i.e. time series.
          output$TS <- renderUI(
              if (TS_include()) 
                  wrapper_selectInput(
                      inputId = "TS",
                      names = names(TS_content[[input$TS_key]]),
                      pre_selected = pre_selected) )

          ##  When a valid 'TS' is selected, load the info-object.
          info <- reactive(
              if (info_load()) {
                  load(file = file.path(
                           paste(c(main_dir,
                                   input$TS),
                                 collapse = .Platform$file.sep),
                           LG_default$info_file_name))
                  ##  This gives us 'info' to return to the work-flow.
                  info
              })
          
          ##     observe({
          ##         print("****")
          ##         nested_if(
          ##             if_list = list(
          ##                 ! identical(input$TS, NULL),
          ##                 ! str_detect(string = input$TS,
          ##                              pattern = "Select")),
          ##             expr_all_TRUE = print(names(info()$App)))
          ## })

          
          ## observe(print(names(input)))

          ##  User interfaces for Approx
          output$Approx <- renderUI(
              if (Approx_include()) {
                  approx_names <-
                      names(info())[str_detect(
                          string = names(info()),
                          pattern = "Approx")]
                  wrapper_selectInput(
                      inputId = "Approx",
                      names = approx_names,
                      pre_selected = pre_selected)
              })
          
          ##  User interface for branching point.
          output$Approx_branch <- renderUI(
              if (Approx_branch_include()) {
                  ##  Find the content of the branchings.
                  Approx_content <-
                      names(info()[[input$Approx]])
                  ##  Count 'Spectra' and 'Boot_Approx'.
                  S <- sum(str_count(
                      string = Approx_content,
                      pattern = "Spectra"))
                  BA <- sum(str_count(
                      string = Approx_content,
                      pattern = "Boot_Approx"))
                  ##  Create 'label', 'choices' and
                  ##  'selected' to be used in the selector.
                  if (S + BA == 0) {
                      label <- "Detected neither spectra nor bootstrap-data"
                      choices <- "Nothing to select"
                      selected <- NULL
                  } else {
                      if (S * BA == 0) {
                          if (BA == 0) {
                              label <- "No bootstrap data detected, auto-selected spectra"
                              choices <- c(
                                  "Select a branch",
                                  "Spectra")
                              selected <- "Spectra"
                          } else {
                              label <- "No spectra detected, auto-selected bootstrap data"
                              choices <- c(
                                  "Select a branch",
                                  "Bootstrap data")
                              selected <- "Bootstrap data"
                          }
                      } else {
                          label <- "Inspect spectra or bootstrap data?"
                          choices <- c(
                              "Select a branch",
                              "Spectra",
                              "Bootstrap data")
                          selected = NULL
                      }
                  }
#####  TASK: How to only use the pre_selected stuff when relevant?  If
#####  the present path coincide with the path from from 'data_dir',
#####  then use default - otherwise use the one selected above.

#####  HERE!!! Dealing with 'pre_selected' directly.
###  This short intermezzo seems to be the only place where a direct
###  investigation of 'pre_selected' is made.
                  
                  if (! is.null(pre_selected$Approx_branch))
                      ##  Compare present and pre-selected
                      if (prod(
### Remember: Single-bracket indexing of reactive-values object is not
### allowed.
                          c(input$TS_key,
                            input$TS,
                            input$Approx) ==
                                pre_selected[c("TS_key", "TS", "Approx")]))
                          ## Use pre_selected when matching.
                          selected <- pre_selected$Approx_branch
#####  WHAT TO DO HERE?  Always using the pre_selected when available?

#####  HERE!!! Dealing with 'pre_selected' directly.

                  
#####  Doesn't look quite satisfying.
                  selectInput(
                      inputId = "Approx_branch",
                      label = label,
                      choices = choices,
                      selected = selected)
              })

          ##  User interface for spectra.
          output$Spectra <- renderUI(
              if (Spectra_include()) {
                  ##  Find the available spectra.
                  Approx_content <-
                      names(info()[[input$Approx]])
                  ##---
                  spectra_names <- 
                      Approx_content[str_detect(
                          string = Approx_content,
                          pattern = "Spectra")]
                  ##  Create the selector.
                  wrapper_selectInput(
                      inputId = "Spectra",
                      names = spectra_names,
                      pre_selected = pre_selected)
              })

          ##  User interface for bootstrap data (when selected).
          output$Boot_Approx <- renderUI(
              if (Boot_Approx_include()) {
                  ##  Find the available bootstrap data.
                  Approx_content <-
                      names(info()[[input$Approx]])
                  ##---
                  bootstrap_names <- 
                      Approx_content[str_detect(
                          string = Approx_content,
                          pattern = "Boot_Approx")]
                  ##  Create the selector.
                  wrapper_selectInput(
                      inputId = "Boot_Approx",
                      names = bootstrap_names,
                      pre_selected = pre_selected)
              })

          ##  User interface for bootstrap spectra.
          output$Boot_Spectra <- renderUI(
              if (Boot_Spectra_include()) {
                  ##  Find the available bootstrap spectra.
                  Bootstrap_content <-
                      names(info()[[c(input$Approx,
                                      input$Boot_Approx)]])
                  ##---
                  Bootstrap_spectra_names <- 
                      Bootstrap_content[str_detect(
                          string = Bootstrap_content,
                          pattern = "Boot_Spectra")]
                  ##  Create the selector.
                  wrapper_selectInput(
                      inputId = "Boot_Spectra",
                      names = Bootstrap_spectra_names,
                      pre_selected = pre_selected)
              })

###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Controls with regard to graphical interface will be placed in a
###  separate window.

#####  TASK: It might be that a modified version of the part where
#####  these stuff are added to the interface would simplify the
#####  definitions here, but then the code would be more messy another
#####  place instead.

          ##  Graphical select 'TS'
          output$TS_graphic <- renderUI(
              if (TS_graphic_include())
                  column(width = 2,
                         actionButton(
                             inputId = "TS_graphic",
                             label = "TS") ))
          ##  Graphical select 'Approx'
          output$Approx_graphic <- renderUI(
              if (Approx_graphic_include())
                  column(width = 2,
                         actionButton(
                             inputId = "Approx_graphic",
                             label = "Approx") ))
          ##  Graphical select 'Spectra'
          output$Spectra_graphic <- renderUI(
              if (Spectra_graphic_include())
                  column(width = 2,
                         actionButton(
                             inputId = "Spectra_graphic",
                             label = "Spectra")) )
          ##  Graphical select 'Boot_Approx'
          output$Boot_Approx_graphic <- renderUI(
              if (Boot_Approx_graphic_include())
                  column(width = 3,
                         actionButton(
                             inputId = "Boot_Approx_graphic",
                             label = "Boot Approx")) )
          ##  Graphical select 'Boot_Spectra'
          output$Boot_Spectra_graphic <- renderUI(
              if (Boot_Spectra_graphic_include())  
                  column(width = 3,
                         actionButton(
                             inputId = "Boot_Spectra_graphic",
                             label = "Boot Spectra")) )

###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Additional controls with regard to the inspection of the
###  TS-object, i.e. we want to see the plot of the time series
###  itself, and related plots like 'acf', 'pacf', 'spec.pgram' and so
###  on, and in particular I want to add an option where we can
###  quickly see if the Local Gaussian auto-covariances for the
###  different lags differ by a significant amount.


          ##  TS graphical select 'plot'
          output$TS_plot <- renderUI(
              column(width = 2,
                     actionButton(
                         inputId = "TS_plot",
                         label = "plot") ))
          ##  TS graphical select 'acf'
          output$TS_acf <- renderUI(
              column(width = 2,
                     actionButton(
                         inputId = "TS_acf",
                         label = "acf") ))
          ##  TS graphical select 'pacf'
          output$TS_pacf <- renderUI(
              column(width = 2,
                     actionButton(
                         inputId = "TS_pacf",
                         label = "pacf")) )
          ##  TS graphical select 'spec.pgram'
          output$TS_spec.pgram <- renderUI(
              column(width = 3,
                     actionButton(
                         inputId = "TS_spec.pgram",
                         label = "spec.pgram")) )
          ##  TS graphical select 'lags'
          output$TS_lags <- renderUI(
              column(width = 3,
                     actionButton(
                         inputId = "TS_lags",
                         label = "lags")) )


###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Define triggers for use later on.
          

          ##  We need a reactive 'trigger' that can sense when a part
          ##  of the 'select graphical'-interface is adjusted.
          graphic_trigger <- reactive(
              paste(input$TS_graphic,
                    input$Approx_graphic,
                    input$Spectra_graphic,
                    input$Boot_Approx_graphic,
                    input$Boot_Spectra_graphic) )


          ##  We need a reactive 'trigger' that can sense when a part
          ##  of the path-interface is adjusted.
          path_trigger <- reactive(
              paste(input$TS_key,
                    input$TS,
                    input$Approx,
                    input$Spectra,
                    input$Boot_Approx,
                    input$Boot_Spectra,
                    input$Approx_branch) )

#####  HERE !!!!
          ##  We need a reactive 'trigger' that can sense when a part
          ##  of the TS-specific plots are desired.
          TS_graphic_trigger <- reactive(
              paste(input$TS_plot,
                    input$TS_acf,
                    input$TS_pacf,
                    input$TS_spec.pgram,
                    input$TS_lags) )
              
          
          
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  In order to decide which kind of graphical display that should be
###  shown, we need to take into account the values returned from the
###  action-buttons and we must also consider the effect of
###  adjustments in the path-selections.  In order to figure out what
###  plots to show when, we will start out by the creation of two
###  intermediate logical vectors that then will be used to figure out
###  reasonable changes to the graphical setup.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Initiate 'graph_selection_matrix'.
          graph_selection_matrix <- matrix(
              data = 0,
              nrow = 2,
              ncol = 5,
              dimnames = list(
                  c("old", "new"),
                  c("TS", "Approx", "Spectra",
                    "Boot_Approx", "Boot_Spectra")))
          
          ## graph_new_selection_vector <- reactive(
          ##     c(TS = ifelse(
          ##           test = identical(input$TS_graphic, NULL),
          ##           yes  = 0,
          ##           no   = input$TS_graphic),
          ##       Approx = ifelse(
          ##           test = identical(input$Approx_graphic, NULL),
          ##           yes  = 0,
          ##           no   = input$Approx_graphic),
          ##       Spectra = ifelse(
          ##           test = identical(input$Spectra_graphic, NULL),
          ##           yes  = 0,
          ##           no   = input$Spectra_graphic),
          ##       Boot_Approx = ifelse(
          ##           test = identical(input$Boot_Approx_graphic, NULL),
          ##           yes  = 0,
          ##           no   = input$Boot_Approx_graphic),
          ##       Boot_Spectra = ifelse(
          ##           test = identical(input$Boot_Spectra_graphic, NULL),
          ##           yes  = 0,
          ##           no   = input$Boot_Spectra_graphic)) )
              

          
          ##  Use 'observe' to update 'graph_selection_matrix', and
          ##  create/update a logical vector that reveals the most
          ##  recent changes due to the action-buttons.
          observe(
              priority = observe_priorities["graph_changes"],
              x = {
                  tmp <- graph_selection_matrix
                  tmp["old", ] <- tmp["new", ]
                  ##---
                  tmp["new", ] <-
###                          graph_new_selection_vector()
                      c(TS = ifelse(
                            test = identical(input$TS_graphic, NULL),
                            yes  = 0,
                            no   = input$TS_graphic),
                        Approx = ifelse(
                            test = identical(input$Approx_graphic, NULL),
                            yes  = 0,
                            no   = input$Approx_graphic),
                        Spectra = ifelse(
                            test = identical(input$Spectra_graphic, NULL),
                            yes  = 0,
                            no   = input$Spectra_graphic),
                        Boot_Approx = ifelse(
                            test = identical(input$Boot_Approx_graphic, NULL),
                            yes  = 0,
                            no   = input$Boot_Approx_graphic),
                        Boot_Spectra = ifelse(
                            test = identical(input$Boot_Spectra_graphic, NULL),
                            yes  = 0,
                            no   = input$Boot_Spectra_graphic))
                  ##  Update 'graph_selection_matrix'
                  assign(x = "graph_selection_matrix",
                         value = tmp,
                         envir = where("graph_selection_matrix"))
                  ##  Create/update logial vector.
                  assign(x = "graph_changes",
                         value = tmp["new", ] > tmp["old", ],
                         envir = where("graph_selection_matrix"))
              })



###-------------------------------------------------------------------
#############---------------------------------------------------------
###  At a 'deeper level' in the interface, we need to keep track of
###  the specified selection of inspection tool for the time series
###  under consideration, and we thus need a similar setup as the one
###  dealing with 'graph_selection_matrix' and 'graph_changes'.
#############---------------------------------------------------------
###-------------------------------------------------------------------

          ##  Initiate 'TS_graphical_matrix', and add an attribute to
          ##  give better control with when it should be updated.
          TS_graphical_matrix <- matrix(
              data = 0,
              nrow = 2,
              ncol = 5,
              dimnames = list(
                  c("old", "new"),
                  c("TS_plot", "TS_acf", "TS_pacf",
                    "TS_spec.pgram", "TS_lags")))
          ##---
          attr(x = TS_graphical_matrix,
               which = "ignore_next_update") <- FALSE
          

#####
          ## ##  Initiate an initial value of 'TS_graphical_changes',
          ## ##  that later on will be updated to a logical vector
          ## ##  revealing the status of the most recently selected
          ## ##  action-button.  Need to define this here in order to get
          ## ##  simpler code inside of 'LG_shiny_helper'.
          
          ##          TS_graphical_changes <- NULL
#####  TASK: The part above turned out to be superfluous when I used a
#####  negative value on the priority, which is kind of interesting...
          
          ##  Use 'observe' to update 'TS_graphical_matrix', and
          ##  create/update a logical vector that reveals the most
          ##  recent changes due to the action-buttons.

          observe(
              priority = observe_priorities["TS_graphical_changes"],
              x = {
                  tmp <- TS_graphical_matrix
                  if (attributes(tmp)$ignore_next_update) {
                      attr(x = tmp,
                           which = "ignore_next_update") <- FALSE
                  } else {
                      tmp["old", ] <- tmp["new", ]
                      ##---
                      tmp["new", ] <-
###                          graph_new_selection_vector()
                          c(TS_plot = ifelse(
                                test = identical(input$TS_plot, NULL),
                                yes  = 0,
                                no   = input$TS_plot),
                            TS_acf = ifelse(
                                test = identical(input$TS_acf, NULL),
                                yes  = 0,
                                no   = input$TS_acf),
                            TS_pacf = ifelse(
                                test = identical(input$TS_pacf, NULL),
                                yes  = 0,
                                no   = input$TS_pacf),
                            TS_spec.pgram = ifelse(
                                test = identical(input$TS_spec.pgram, NULL),
                                yes  = 0,
                                no   = input$TS_spec.pgram),
                            TS_lags = ifelse(
                                test = identical(input$TS_lags, NULL),
                                yes  = 0,
                                no   = input$TS_lags))
                  }
                  ##  Update 'TS_graphical_matrix'
                  assign(x = "TS_graphical_matrix",
                         value = tmp,
                         envir = where("TS_graphical_matrix"))
                  ##  Create/update logial vector.
                  assign(x = "TS_graphical_changes",
                         value = tmp["new", ] > tmp["old", ],
                         envir = where("TS_graphical_matrix"))
              })

          


###-------------------------------------------------------------------
#############---------------------------------------------------------
          
          ##  Initiate information about the path-selection.
          path_selection_list <- list(
              old = pre_selected,
              new = pre_selected)
          
          ##  Create a help-function to compare 'old' and 'new' from
          ##  'path_selection_list' (Reminder: Can't 'unlist' and
          ##  compare as vectors, since the list can contain 'NULL').
          path_changes_FUN <- function(path_selection_list) {
              .list <- path_selection_list
              .result <- vector(
                  mode = "logical",
                  length = length(.list[[1]]))
              names(.result) <- names(.list[[1]])
              for (.name in names(.list[[1]]))
                  .result[.name] <- ! identical(
                      x = .list[[c("old", .name)]],
                      y = .list[[c("new", .name)]])
              ##  Return the result to the workflow
              .result
          }
          
          ##  Use 'observe' to update 'path_selection_list' with new
          ##  values from the path-controls, and create/update a
          ##  logical vector revealing the most recent modifications.
          observe(priority = observe_priorities["path_changes"],
                  x = {
                      ##  Update 'path_selection_list'.
                      assign(x = "path_selection_list",
                             value = list(
                                 old = path_selection_list[["new"]],
                                 new = list(
                                     TS_key = input$TS_key,
                                     TS = input$TS,
                                     Approx = input$Approx,
                                     Spectra = input$Spectra,
                                     Boot_Approx = input$Boot_Approx,
                                     Boot_Spectra = input$Boot_Spectra,
                                     Approx_branch = input$Approx_branch)),
                             envir = where("path_selection_list"))
                      ##  Create/update logical vector.
                      assign(x = "path_changes",
                             value = path_changes_FUN(path_selection_list),
                             envir = where("path_selection_list"))
                  })


###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Based on the values in the matrix, we should now create the
###  required vector of logical values that will decide the selection
###  of the interface and the plot to be shown.  We will start out
###  with those adjustment triggered by the 'select-graphical'
###  interface, and then adjustments due to the 'select-path'
###  interface will be taken into account.
#############---------------------------------------------------------
###-------------------------------------------------------------------
          
          ##  Initiate logical vector 'show_graphical'.
          show_graphical <- vector(
              mode = "logical",
              length = dim(graph_selection_matrix)[2])
          names(show_graphical) <- colnames(graph_selection_matrix)

          ##  To reduce the risk of "strange behaviour" due to
          ##  combined updates of path- and graphical-selectors, let's
          ##  ensure that we have a copy of the existing graphical
          ##  setting before we start messing around with it.

          ##  Create a backup of 'show_graphical', triggered by any
          ##  changes from the interface in the present "iteration" of
          ##  the server-script.

          observe(
              priority = observe_priorities["backup_show_graphical"],
              x = {
                  graphic_trigger()
                  path_trigger()
                  ##---
                  assign(x = "backup_show_graphical",
                         value = show_graphical,
                         envir = where("show_graphical"))
              })
         
 
          
          ##  Update 'show_graphical' based on 'graph_changes'
          observe(
              priority = observe_priorities["show_graphical"],
              x = {
                  graphic_trigger()
                  ##  If 'graph_changes' equals 'show_graphical',
                  ##  then a plot has been turned on and off, and
                  ##  all values in 'show_graphical' should be set
                  ##  to FALSE -- otherwise use 'graph_changes'
                  ##  for the update.
                  tmp <- graph_changes
                  if (identical(show_graphical, graph_changes))
                      tmp[] <- FALSE
                  ##---
                  assign(x = "show_graphical",
                         value = tmp,
                         envir = where("show_graphical"))
              })

          ##  Update 'show_graphical' based on 'path_changes',
          ##  ignoring any changes due to 'graph_changes'.
          observe(
              priority = observe_priorities["show_graphical_2"],
              x = {
                  path_trigger()
                  tmp <- backup_show_graphical
                  ##  Register the main (i.e. first) path-change.
                  adjusted_part <- names(path_changes)[path_changes][1]
                  ##  Update 'tmp' based on 'adjusted_part'.
                  if (adjusted_part %in% c("TS_key", "TS"))
                      tmp[] <- FALSE
                  if (adjusted_part == "Approx")
                      tmp[names(tmp) != "TS"] <- FALSE
                  if (adjusted_part %in% c("Spectra", "Boot_Spectra"))
                      tmp[adjusted_part] <- FALSE
                  if (adjusted_part %in% c("Boot_Approx", "Approx_branch"))
                      tmp[c("Boot_Approx", "Boot_Spectra")] <- FALSE
                  ##  Update 'show_graphical' with 'tmp'.
                  assign(x = "show_graphical",
                         value = tmp,
                         envir = where("show_graphical"))
              })

          
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Time to create an interface showing the controls for the
###  different plots, at the moment only a minor test.

          ##  Create 'graphical_controls' that will contain a list of
          ##  controls for the showing of the details (at least if the
          ##  idea I have got in mind is feasible to use).


          ##  Create a nonreactive copy of 'input', to avoid problems
          ##  when outsourcing the creation of parts of the interface.
          nonreactive_input <- NULL
          observe(
              priority = observe_priorities["create_input_copy"],
              x = {
                  ##  The triggers needed in order to ensure that the
                  ##  updates are performed when necessary.
                  graphic_trigger()
                  path_trigger()
                  TS_graphic_trigger()
                  input$point_type
                  input$Vi
                  input$Vj
                  input$spectrum_variant
                  input$od_c_a_sqc
                  ##  A nonreactive version of 'input'
                  .input <- vector(mode = "list",
                                   length = length(input))
                  for (.name in isolate(names(input))) {
                      .input[[.name]] <- isolate(input[[.name]])
                  }
                  assign(x = "nonreactive_input",
                         value = .input,
                         envir = where("nonreactive_input"))
              }
          )


          output$graphical_controls  <- renderUI({
              ##  Update when changes in the selection-menus.
              { graphic_trigger()
                path_trigger()
                ##  Triggers created by 'LG_shiny_helper' (store in a
                ##  collected trigger later on when testing finished?)
                ## input$type ##  Specific for 'TS'

                ##  Stuff related to the main investigation, only
                ##  include those that might require an update of the
                ##  interface!

                ## ## input$type
                input$point_type
                input$Vi
                input$Vj
                input$spectrum_variant
                input$od_c_a_sqc
                ## ## input$window
                ## ## input$confidence_interval
                ## ## input$bw_points
                ## ## input$cut
                ## ## input$levels
                ## ## input$levels_Horizontal
                ## ## input$levels_Vertical
                ## ## input$levels_Line
                ## ## input$omega_range
                ## ## input$Y_range
                
                
#####  I'm not quite sure if this can work...  Better test it before I
#####  have made to many changes that depends upon this.

#####  Some details not quite as I had wished for, the playing of the
#####  plots got messed up, but I can think more upon that later on.
                
                TS_graphic_trigger()
            }
              ##  Use 'LG_shiny_helper' to take care of the interface
              ##  and the loading of the data.


################################################################################
#####  2017-01-13: After a lot of messing around, it finally worked.
#####  The strategy (for this particular function) is to ensure that
#####  the stuff of interest is no longer reactive.  I wonder if
#####  capture_env can be adjusted into this setting with some use of
#####  the tricks below, and an extended rule for capturing previous
#####  environments...  Well, that's not an issue to pursue at the
#####  moment.
              
              .info <- vector(mode = "list", length = length(info()))
              for (.name in isolate(names(info()))) {
                  .info[[.name]] <- isolate(info()[[.name]])
                  
              }                  
              .input <- vector(mode = "list", length = length(input))
              for (.name in isolate(names(input))) {
                  .input[[.name]] <- isolate(input[[.name]])
              }
              Path_selection_list <- path_selection_list
              Show_graphical <- show_graphical
              Default_type <- ifelse(
                  test = identical(.input$type, NULL),
                  yes  = default_type,
                  no   = input$type)
              .Env <- pryr::where("show_graphical")
              .Env2 <- pryr::where("TS_graphical_matrix")

              nonreactive_input <- nonreactive_input
              
              LG_shiny_helper_call <- create_call(
                  .cc_fun = LG_shiny_helper,
                  info = .info,
                  input = nonreactive_input,
                  ## input = .input,
                  path_selection_list = Path_selection_list,
                  show_graphical = Show_graphical,
                  default_type = Default_type,
                  .env = .Env,
                  .env2 = .Env2)

              ## if (! is.null(input$point_type))
              ##     if (input$point_type == "off_diag")
              ##         capture_env() 

              ## if (any(show_graphical))
              ##     capture_env() 
                      
################################################################################
              
              LG_shiny_helper(
                  info = info(),
                  input = nonreactive_input,
                  ## input = input,
                  path_selection_list = path_selection_list,
                  show_graphical = show_graphical,
                  default_type = 
                      ifelse(
                          test = identical(input$type, NULL),
#####  TASK: This solution sucks, I must add an extra layer to ensure
#####  that the argument is reset if the path changes, otherwise it's
#####  no point having 'default_type' as an argument in 'LG_shiny'.
                          yes  = default_type,
                          no   = input$type),
                  .env = pryr::where("show_graphical"),
                  .env2 = pryr::where("TS_graphical_matrix"))
          })
          

          

###-------------------------------------------------------------------
          ##  Can the problem with the production of that damned plot
          ##  for the lagged pairs be resolved by means of an external
          ##  observer with a suitably selected priority?

          ## ## ## ## ##  Initiate plot-data.
          TS_lag_arguments <- NULL
          

          ##  Create a pointer to the present environment.
          ...env... <- pryr::where("show_graphical")
          ##  For inspection later on, define 'my_global_env' outside
          ##  of this function, and use the line below.
          
          ## my_global_env <- ...env...

          
###-------------------------------------------------------------------

          ##  How to add the plots...
          output$graphs <- renderPlot({
              ##  Update when changes in the selection menus.
              {## graphic_trigger()
                  ## path_trigger()

                  ##  2017-01-17: Not updating as expected...
                  input$global_local
                  input$type
                  input$point_type
                  input$Vi
                  input$Vj
                  input$spectrum_variant
                  input$od_c_a_sqc
                  input$window
                  input$confidence_interval
                  input$bw_points
                  input$cut
                  input$levels
                  input$levels_Horizontal
                  input$levels_Vertical
                  input$levels_Line
                  input$omega_range
                  input$Y_range

                  

                  ##  stuff specific for 'TS', store in separate trigger
                  ##  when testing has been performed.
                  TS_graphic_trigger()
#####  HERE
                  ##---
                  input$lag_slider
                  input$lag_point
           }

              ##  Create the plot when 'input$get_code' is 'FALSE'.
              LG_plot_helper(
                  main_dir = main_dir,
                  input = input,
                  show_graphical = show_graphical,
                  path_selection_list = path_selection_list,
                  .env = pryr::where("show_graphical"))
          })
          

###-------------------------------------------------------------------
          
          ##  How to add the code required to create a specified
          ##  plot in e.g. a paper.
          output$graphs_call <- renderPrint({
              ##  Update when changes in the selection menus.
              {## graphic_trigger()
               ## path_trigger()
                  input$global_local
                  input$type
                  input$point_type
                  input$Vi
                  input$Vj
                  input$spectrum_variant
                  input$od_c_a_sqc
                  input$window
                  input$confidence_interval
                  input$bw_points
                  input$cut
                  input$levels
                  input$levels_Horizontal
                  input$levels_Vertical
                  input$levels_Line
                  input$omega_range
                  input$Y_range                  

                  
               ##  stuff specific for 'TS', store in separate trigger
               ##  when testing has been performed.
               TS_graphic_trigger()
               ##---
               input$lag_slider
               input$lag_point
               input$get_code
              }

              ##  Create the code for the plot.
              .plot_code <- LG_plot_helper(
                  main_dir = main_dir,
                  input = input,
                  show_graphical = show_graphical,
                  path_selection_list = path_selection_list,
                  .env = pryr::where("show_graphical"))
              ##  Include a test to avoid errors when switching back
              ##  and forth between the plot and its required code.
              if (! is.list(.plot_code))
                  cat(.plot_code,
                      sep  = "\n")
          })


###-------------------------------------------------------------------
          output$Explain_Interface <- renderPrint({
              ##  Update when changes in the selection menus.
              {   graphic_trigger()
                  path_trigger()
                  input$global_local
                  input$type
                  input$point_type
                  input$Vi
                  input$Vj
                  input$spectrum_variant
                  input$od_c_a_sqc
                  input$window
                  input$confidence_interval
                  input$bw_points
                  input$cut
                  input$levels
                  input$levels_Horizontal
                  input$levels_Vertical
                  input$levels_Line
                  input$omega_range
                  input$Y_range                  

                  
                  ##  stuff specific for 'TS', store in separate trigger
                  ##  when testing has been performed.
                  TS_graphic_trigger()
                  ##---
                  input$lag_slider
                  input$lag_point
                  input$get_code
              }


################################################################################
                            
              ## .info <- vector(mode = "list", length = length(info()))
              ## for (.name in isolate(names(info()))) {
              ##     .info[[.name]] <- isolate(info()[[.name]])
                  
              ## }                  
              ## .input <- vector(mode = "list", length = length(input))
              ## for (.name in isolate(names(input))) {
              ##     .input[[.name]] <- isolate(input[[.name]])
              ## }
              ## Path_selection_list <- path_selection_list
              ## Show_graphical <- show_graphical
              ## Default_type <- ifelse(
              ##     test = identical(.input$type, NULL),
              ##     yes  = default_type,
              ##     no   = input$type)
              ## .Env <- pryr::where("show_graphical")
              ## .Env2 <- pryr::where("TS_graphical_matrix")
              ## main_dir <- main_dir

              ## nonreactive_input <- nonreactive_input
              
              ## LG_shiny_explain_interface_call <- create_call(
              ##     .cc_fun = LG_shiny_explain_interface,
              ##     main_dir = main_dir,
              ##     ## ## ## ## ## info = .info,
              ##     input = nonreactive_input,
              ##     ## input = .input,
              ##     show_graphical = Show_graphical,
              ##     path_selection_list = Path_selection_list,
              ##     ## default_type = Default_type,
              ##     .env = .Env)

              
              ## capture_env() 

################################################################################

              
              
              LG_shiny_explain_interface(
                  main_dir = main_dir,
                  input = input,
                  show_graphical = show_graphical,
                  path_selection_list = path_selection_list,
                  .env = pryr::where("show_graphical"))
              
              
          })


          output$Explain_Plot <- renderPrint({
              ##  Update when changes in the selection menus.
              {   graphic_trigger()
                  path_trigger()
                  input$type
                  input$point_type
                  input$Vi
                  input$Vj
                  input$spectrum_variant
                  input$od_c_a_sqc
                  input$window
                  input$confidence_interval
                  input$bw_points
                  input$cut
                  input$levels
                  input$levels_Horizontal
                  input$levels_Vertical
                  input$levels_Line
                  input$omega_range
                  input$Y_range                  

                  
                  ##  stuff specific for 'TS', store in separate trigger
                  ##  when testing has been performed.
                  TS_graphic_trigger()
                  ##---
                  input$lag_slider
                  input$lag_point
                  input$get_code
              }


################################################################################
                            
              .info <- vector(mode = "list", length = length(info()))
              for (.name in isolate(names(info()))) {
                  .info[[.name]] <- isolate(info()[[.name]])
                  
              }                  
              .input <- vector(mode = "list", length = length(input))
              for (.name in isolate(names(input))) {
                  .input[[.name]] <- isolate(input[[.name]])
              }
              Path_selection_list <- path_selection_list
              Show_graphical <- show_graphical
              Default_type <- ifelse(
                  test = identical(.input$type, NULL),
                  yes  = default_type,
                  no   = input$type)
              .Env <- pryr::where("show_graphical")
              .Env2 <- pryr::where("TS_graphical_matrix")
              main_dir <- main_dir

              ## nonreactive_input <- nonreactive_input
#####  OBS: nonreactive_input not suited for this part!
              
              LG_shiny_explain_plots_call <- create_call(
                  .cc_fun = LG_shiny_explain_plots,
                  main_dir = main_dir,
                  ## ## ## ## ## info = .info,
                  ## input = nonreactive_input,
                  input = .input,
                  show_graphical = Show_graphical,
                  path_selection_list = Path_selection_list,
                  ## default_type = Default_type,
                  .env = .Env)

              ## capture_env() 
              
              ## if (! is.null(.input$levels_Horizontal))
              ##     if (.input$levels_Horizontal != .input$levels_Vertical)
              ##         capture_env() 

################################################################################
              
              LG_shiny_explain_plots(
                  main_dir = main_dir,
                  input = input,
                  show_graphical = show_graphical,
                  path_selection_list = path_selection_list,
                  .env = pryr::where("show_graphical"))

              
          })          
          
          
          
          help_fun <- function(.list) {
              .names <- names(.list)
              paste("\n\t",
                    paste(.names, " = ", .list, collapse = "\n\t"),
                    sep = "")
          }


          help_fun2 <- function(.env) {
              help_fun(reactiveValuesToList(.env))
          }
          

          
          output$internal_status <- renderPrint(
              cat("Some information about the values ",
                  "\n",
                  "Pre selected:",
                  help_fun(pre_selected), "\n\n",
                  "Values from input:",
                  help_fun2(input),
                  "\n\n",
                  "Some stuff from the 'server'-environment:",
                  {
                      ##  Remove the problematic stuff i.e.  'output'
                      ##  and 'session'.  Ignore 'input' and 'info'
                      ##  (treated separately further down).
                      .pattern <- setdiff(
                          ls(...env...,
                             all.names = TRUE),
                          c("input", "output", "session", "info"))
                      ##  Get rid of names pointing to functions.
                      .pattern <- .pattern[
                          vapply(X = .pattern,
                                 FUN = function(x) {
                                     ! is.function(...env...[[x]])},
                                 FUN.VALUE = logical(1))]
                      ##  Present the 'str'-information, for the
                      ##  desired objects.
                      help_fun(vapply(X = .pattern,
                                      FUN = function(x) {
                                          paste(capture.output(str(
                                              object = ...env...[[x]])),
                                              collapse = "\n\t")
                                      },
                                      FUN.VALUE = character(1)))
                  },
                  "\n\n",
                  "The info object...",
                  help_fun(info()$TS_info),
                  if (object.size(info()) > object.size(1:1000)) {
                      "\nNot showing info object, too large, lots of messy text."
                  } else
                      help_fun(info())
                  )
          )

          

          
          
      })
    ## This concludes the definition of 'server'.

    
###-------------------------------------------------------------------
    ##  Define the 'options'-argument to be delivered to 'shinyApp',
    ##  which again can contain an 'options'-argument to be sent to
    ##  'runApp' -- and the latter might be nice to keep in mind under
    ##  development in order to inspect the interaction between the
    ##  different components.
    options <- list(
        test = "TEST",
        options = list(display.mode = "showcase"))
#####  This did not work like I thought at all, and it might be that
#####  the problem is due to lack of maturity of the 'shinyApp' and
#####  'runApp'...  I can't find anything at the web either, so I will
#####  just have to make an attempt without "showcase" for the time
#####  being.  I suppose I could create an ordinary app to experiment
#####  with if all else fail.  It should probably be more or less
#####  sufficient to add the stuff in 'ui' and 'server' into a folder
#####  to get a work-around for it.
###-------------------------------------------------------------------
    ##  Run 'shinyApp'
    shinyApp(ui = ui,
             server = server,
             options = options)
}
