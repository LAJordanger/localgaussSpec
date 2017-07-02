################################################################################
#####  2015-01-27

#' Deal with the details that concerns the investigation of the data.
#'
#' The purpose of this function is to create the selection part of the
#' interactive interface, i.e. the part that gives selectors that
#' extracts subsets of the data suited for plotting.  Note that the
#' loading of the data is taken care of in other functions.
#'
#' @param info The information object containing all the data about
#'     the content and whereabouts of the required files.
#'
#' @param input The list that contains the present status of selected
#'     values in the interactive interface.
#' 
#' @param show_graphical A logic vector that specifies which kind of
#'     data that should be investigated.
#'
#' @param path_selection_list A list containing the components
#'     required to identifying the correct part of \code{info} to work
#'     upon.
#'
#' @param default_type Specifies whether "par_five" (default value) or
#'     "par_one" (i.e. Local Gaussian auto-correlation or Local
#'     Gaussian auto-covariance) will be used when both alternatives
#'     are available in the data.
#' 
#' @param .env An environment needed in order to get hold of the
#'     correct place to read and update some arguments.
#' 
#' @param .env2 Another environment needed in order to get hold of the
#'     correct place to read and update some arguments.
#' 
#' @return The result of this function will be that a suitable
#'     interface is created, and if I can get it to work, then it will
#'     in addition load the data into the correct environment.
#'
#' @export



LG_shiny_helper <- function(
    info,
    input,
    show_graphical,
    path_selection_list,
    default_type,
    .env,
    .env2) {
###-------------------------------------------------------------------
    ##  Sanity-checks?
###-------------------------------------------------------------------
    ##  Return a message if no graphical component has been selected.
    if (! any(show_graphical))
        return("Please select data to inspect.")
###-------------------------------------------------------------------
    ##  The cases where a selection has been made.
###-------------------------------------------------------------------
    ##  Identify the selected component.
    .selected <- names(show_graphical)[show_graphical]
    kill(show_graphical)
###-------------------------------------------------------------------
    ##  Use 'LG_lookup' to get hold of '.input$look_up', a list based
    ##  on extractions (and computations) of the content of 'input',
    ##  with values needed for the creation of the different
    ##  interfaces.  Reminder: The point of the 'look_up'-list is to
    ##  avoid problems due to the way 'input' is updated in the
    ##  reactive shiny-interface.
    LG_lookup(.info = info,
              .selected = .selected,
              .path_selection_list = path_selection_list,
              .input = input)
    ##  Extract the part of interest from '.input' to get more compact
    ##  code later on.
    look_up <- .input$look_up
    kill(.input)
###-------------------------------------------------------------------
    if (.selected == "TS") {
        ##  Get a local copy of 'TS_graphical_changes'.
        TS_graphical_changes <- get(
            x = "TS_graphical_changes",
            envir = .env)
        ##  If it's not the initial 'NULL', extract the key that
        ##  identifies the latest selected button.
        if (! identical(TS_graphical_changes, NULL))
            TS_graphical_changes <-
                names(TS_graphical_changes)[TS_graphical_changes]
        ##  If we have the option 'TS_lags' we would like to add
        ##  additional buttons to allow us to loop through the lagged
        ##  pairs showing the result of 'localgauss' used upon them,
        ##  and moreover, I think it would be nice to add the points
        ##  on top of them.
        extra_interface <- 
            if (identical(TS_graphical_changes, "TS_lags")) {
                list("Inspection of the lagged pairs.",
                     sliderInput(
                         inputId = "lag_slider",
                         label = "Inspection of lagged pairs.",
                         min = 1,
                         max = min(100, ceiling(0.9 * length(info$TS_info$TS))),
                         value = 1,
                         step = 1,
                         animate = 
                             animationOptions(interval = 500, loop = TRUE)),
                     actionButton(
                         inputId = "lag_point",
                         label = "Show/hide the lagged points."))
            } else
                NULL
        ##  When necessary, update the 'ignore_next_update'-attribute
        ##  of 'TS_graphical_matrix
        if (identical(TS_graphical_changes, "TS_lags")) {
#####  Reminder: This was a pain to get working, I didn't get 'eval'
#####  to work with the specified environment...
            attr(x = .env2$TS_graphical_matrix,
                   which = "ignore_next_update") <- TRUE
        }
#####  TASK: I don't think this solution works as it should, it seems
#####  to get stuck on lags, and that was not the intention.
        ##  Add an interface for a further inspection of 'TS'.
        return(c(
            list(
                "Select the desired graphical interface",
                fluidRow(
                ##  Add actionButton for 'TS_plot'.
                    uiOutput("TS_plot"), 
                    ##  Add actionButton for 'TS_acf'.
                    uiOutput("TS_acf"),
                    ##  Add actionButton for 'TS_pacf'.
                    uiOutput("TS_pacf"),
                    ##  Add actionButton for 'TS_spec.pgram'.
                    uiOutput("TS_spec.pgram"),
                    ##  Add actionButton for 'TS_lags'.
                    uiOutput("TS_lags"))),
            extra_interface))
    }
    kill(.env, .env2)
###-------------------------------------------------------------------
#############---------------------------------------------------------
    ##  Create the common parts of the interface, i.e. those parts
    ##  that occur regardless of the investigated component.
    ##  Procedure: First create a list '.interface' that contains the
    ##  information for all the possible components (even those not
    ##  needed).  From this list create the shiny-code needed for the
    ##  interface, and finaly use some logical values (based on a
    ##  feedback-loop from 'input') to decide which components that
    ##  the present setting requires.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Two minor help-functions for the slider-interfaces.
    .range <- function(vec)
        range(seq_along(vec))
    .center <- function(vec)
        ceiling(length(vec)/2)
    .interface <- list(
        Vi = list(
            what = "radioButtons",
            label = "Var 1",
            choices =  info$TS_info$.variables,
            selected =
                if (is.null(input$Vi)) {
                    info$TS_info$.variables[1]
                } else
                    input$Vi,
            inline = TRUE), 
        Vj = list(
            what = "radioButtons",
            label = "Var 2",
            choices =  info$TS_info$.variables,
            selected =
                if (is.null(input$Vj)) {
                    info$TS_info$.variables[2]
                } else
                    input$Vj,
            inline = TRUE),
        point_type = list(
            what = "radioButtons",
            label = "Select branch for points",
            choices =  LG_default$result$hierarchy$point,
            selected =
                if (is.null(input$point_type)) {
                    LG_default$result$hierarchy$point[1]
                } else
                    input$point_type,
            inline = TRUE),
        type = local({
            ##  Create more informative names, and ensure that only
            ##  one type is returned when only one is present.
            LG_spectra_names <- c(
                'LGA_five' = "par_five",
                'LGA_one' = "par_one")
            LG_spectra_names <-
                LG_spectra_names[LG_spectra_names %in% look_up$.available_types]
            list(what = "radioButtons",
                 label = "Local Gaussian approximation",
                 choices = LG_spectra_names,
                 selected =
                     if (is.null(input$type)) {
                         LG_spectra_names[LG_spectra_names == default_type]
                     } else
                         input$type,
                 inline = TRUE)
        }), 
        bw_points = list(
            what = "radioButtons",
            label = "Local bandwidth solution",
            choices =  look_up$.dimnames[["bw_points"]],
            selected =
                if (is.null(input$bw_points)) {
                    tail(look_up$.dimnames[["bw_points"]], 1)
                } else
                    input$bw_points), 
        levels = list(  ##  on-diagonal case
            what = "sliderInput",
            label = "Index for points on diagonal",
            min = 1,
            max = length(look_up$.dimnames$levels),
            value = 
                if (is.null(input$levels)) {
                    .center(look_up$.dimnames$levels)
                } else
                    input$levels,
            step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)), 
        levels_Line = list(  ##  off-diagonal, line.
            what = "sliderInput",
            label = "Index for points on line",
            min = 1,
            max = length(look_up$.Horizontal),
            value = 
                if (is.null(input$levels_Line)) {
                    .center(look_up$.Horizontal)
                } else
                    input$levels_Line,
            step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)),
        levels_Horizontal = list(  ##  off-diagonal, rectangle.
            what = "sliderInput",
            label = "Index for horizontal points",
            min = 1,
            max = length(look_up$.Horizontal),
            value = 
                if (is.null(input$levels_Horizontal)) {
                    .center(look_up$.Horizontal)
                } else
                    input$levels_Horizontal,
            step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)),
        levels_Vertical = list(  ##  off-diagonal, rectangle.
            what = "sliderInput",
            label = "Index for vertical points",
            min = 1,
            max = length(look_up$.Vertical),
            value = 
                if (is.null(input$levels_Vertical)) {
                    .center(look_up$.Vertical)
                } else
                    input$levels_Vertical,
            step = 1,
            animate = animationOptions(interval = 1000, loop = TRUE)),
        global_local = list(
            what = "radioButtons",
            label = "Local or global data",
            choices =  c("global", "local"),
            selected = "local")
    )



################################################################################
    
    ## if (.selected == "Boot_Spectra") {
    if (.selected %in% c("Spectra", "Boot_Spectra")) {
        .interface <- c(
            .interface,
            list(
                spectrum_variant = local({
                    ##  Interface for 'spectrum_variant', based on the
                    ##  arguments of 'LG_extract_df', with adjustments for
                    ##  univariate and multivariate time series.
                    .cross_choices <- setdiff(
                        x = eval(formals(LG_extract_df)$.spectrum),
                        y = "auto")
                    .cross_selected <- "co"
                    list(what = "selectInput",
                         label = "Select spectrum",
                         choices =  .cross_choices,
                         selected =
                             if (is.null(input$spectrum_variant)) {
                                 .cross_selected
                             } else
                                 input$spectrum_variant)
                }),
                window = list(
                    what = "selectInput",
                    label = "Window-type",
                    choices =  look_up$.dimnames[["window"]],
                    selected =
                        if (is.null(input$window)) {
                            look_up$.dimnames[["window"]][1]
                        } else
                            input$window), 
                confidence_interval =
                    if (.selected == "Boot_Spectra")
                        local({
                            ##  Interface for selection of 'confidence_interval',
                            ##  based on the 'content'-dimension of the array.
                            ##  Reminder: Not possible to send a vector, in order to
                            ##  get the pieces patched together we collapse with ",".
                            .content <- look_up$.dimnames[[LG_default$dimnames$main]]
                            ##  Find the available percentiles
                            .CI_percentiles <- str_sub(
                                string = .content[str_detect(
                                    string = .content,
                                    pattern = "low_")],
                                start = nchar("low_") + 1)
                            ##  Initiate a vector to be used for the confidence intervals
                            .confidence_intervals <- c()
                            ##  Add the required information.
                            for (.percentile in .CI_percentiles) {
                                .percentile_match <- str_detect(
                                    string = .content,
                                    pattern = .percentile)
                                ##---
                                .percentile_names <-
                                    .content[.percentile_match]
                                ##---
                                .CI_name <-paste(
                                    .percentile,
                                    "%",
                                    sep = "")
                                ##---
                                .CI_value <- paste(
                                    LG_default$sample.prefix,
                                    paste(.percentile_names,
                                          collapse = ","),
                                    sep = ",") 
                                ##---
                                add_to_vec_quote <- bquote(
                                    .confidence_intervals[.(.CI_name)] <- .(.CI_value))
                                ##---
                                eval(add_to_vec_quote)
                            }
                            ##  Add 'min-max' when both are present.
                            if (all (c("min", "max") %in% .content)) {
                                .confidence_intervals <- c(
                                    .confidence_intervals,
                                    "min-max" =
                                        paste(LG_default$sample.prefix,
                                              "min",
                                              "max",
                                              sep = ",") )
                            }
                            list(what = "radioButtons",
                                 label = if (look_up$.block) {
                                             "Limits for \"block-based confidence interval\""
                                         } else
                                             "Limits for \"bootstrapped confidence interval\"",
                                 choices =  .confidence_intervals,
                                 selected =
                                     if (is.null(input$confidence_interval)) {
                                         .confidence_intervals[1]
                                     } else
                                         input$confidence_interval,
                                 inline = TRUE)
                        }),
                ##  The values for selection a part from the analogue of the
                ##  squared coherence for the complex-valued case 
                ##  "off_diag"+"cross"+"squared_coherence".
                od_c_a_sqc  = local({
                    .selected <- if ("amplitude" %in% look_up$.od_cross) {
                                     "amplitude"
                                 } else
                                     look_up$.od_cross[1]
                    list(what = "selectInput",
                         label = "Select component",
                         choices =  look_up$.od_cross,
                         selected =
                             if (is.null(input$od_c_a_sqc)) {
                                 .selected
                             } else
                                 input$od_c_a_sqc)
                }),
                ##  Interface for sliders with one value selected.
                cut = list(
                    what = "sliderInput",
                    label = "Truncation point (+1)",
                    min = 1,
                    max = length(look_up$.dimnames$cut),
                    value = 
                        if (is.null(input$cut)) {
                            .center(look_up$.dimnames$cut)
                        } else
                            input$cut,
                    step = 1,
                    animate = animationOptions(interval = 1000, loop = TRUE)), 
                ##  Interface for sliders with two values selected.
                omega_range = list(
                    what = "sliderInput",
                    label = "Zoom control for frequencies (X-axis)",
                    min = 1,
                    max = length(look_up$.dimnames$omega),
                    value = 
                        if (is.null(input$omega_range)) {
                            .range(look_up$.dimnames$omega)
                        } else
                            input$omega_range,
                    step = 1), 
                Y_range = list(  ##  NB: Based on omega-dimension!
                    what = "sliderInput",
                    label = "Zoom control for results (Y-axis)",
                    min = 1,
                    max = length(look_up$.dimnames$omega),
                    value = 
                        if (is.null(input$Y_range)) {
                            .range(look_up$.dimnames$omega)
                        } else
                            input$Y_range,
                    step = 1)
            ))
    }

################################################################################



    
    kill(default_type, .center, .range)
###-------------------------------------------------------------------
    ##  Create logical values to be used when selecting relevant
    ##  components.  Reminder: When this function is called the first
    ##  time, the logical values will depend upon the default values
    ##  used in the initial setting.
    .auto_pair <- if (look_up$.multivariate) {
                      if (! is.null(input$Vj)) {
                          input$Vi == input$Vj
                      } else  ##  Initial setting.
                          .interface$Vi$selected == .interface$Vj$selected
                  } else
                      TRUE
    .on_diagonal <-
        if (! is.null(input$point_type)) {
            input$point_type == "on_diag"
        } else  ## Initial setting.
            .interface$point_type$selected == "on_diag"
    .off_diagonal <- ! .on_diagonal
    .spectrum_variant <- ! all(.auto_pair, .on_diagonal)
    .od_c_a_sqc <- all(
        if (! is.null(input$spectrum_variant)) {
            input$spectrum_variant == "squared_coherence"
        } else ##  Initial setting.
            .interface$spectrum_variant$selected == "squared_coherence",
        ! .auto_pair,
        .off_diagonal)
    kill(input)
################################################################################
#####  The investigation below, if stored, can now probably be better
#####  formulated relatively the logical values above.
    ## ## ## ##  Conditional tests to be used when it the investigation, need to replace
    ## ## ## ##  reactive object too.
    ## ## ## if (! is.null(input$Vj))
    ## ## ##     if (input$Vj == input$Vi) {
    ## ## ##         .input <- vector(mode = "list", length = length(input))
    ## ## ##         for (.name in isolate(names(input))) {
    ## ## ##             .input[[.name]] <- isolate(input[[.name]])
    ## ## ##         }
    ## ## ##         input <- .input
    ## ## ##         capture_env()
    ## ## ##     }
################################################################################
###-------------------------------------------------------------------    
    ##  Select the parts (and orders of) 'interface' to be used in the
    ##  counstruction of the graphical interface.  Use the information
    ##  from the feedback-loop to remove superfluous components at the
    ##  end before returning the result to the workflow.  NB: A
    ##  similar setup should be present in 'LG_plot_helper' when the
    ##  "return code for plot" is constructed.
    .controls <- c(if (.selected %in% c("Approx", "Boot_Approx"))
                       "global_local",
                   "type",
                   "point_type",
                   if (look_up$.multivariate) {
                       c("Vi",
                         "Vj")
                   } else
                       "Vi",  ## To be overwritten later on.
                   if (.spectrum_variant)
                       "spectrum_variant",
                   if (.od_c_a_sqc)
                       "od_c_a_sqc",
                   "window",
                   "confidence_interval",
                   "bw_points",
                   "cut",
                   if (.on_diagonal) {
                       "levels"
                   } else {
                       if (look_up$.Shape == "rectangle") {
                           c("levels_Horizontal",
                             "levels_Vertical")
                       } else  ##  Adjust later on if 'look_up$.Shape' in fact
                               ##  is equal to 'point'.
                           "levels_Line"
                   },
                   "omega_range",
                   "Y_range")
    ##  Loop over the part of 'controls' needed and create a list with
    ##  the code for the different interfaces.  Use solution that
    ##  gives 'NULL' for superfluous parts, and remove those
    ##  afterwards.
    result <- lapply(
        X = .controls,
        FUN = function(x) {
            .data <- .interface[[x]]
            if (is.null(.data))
                return(NULL)
            .not_what <- setdiff(x = names(.data), y = "what")
            do.call(
                what = .data$what,
                args = c(list(inputId = x),
                         .data[.not_what]))
        })
    names(result) <- .controls
    kill(.controls, .interface)
    result <- result[! vapply(
        X = result,
        FUN = is.null,
        FUN.VALUE = logical(1))]
    ##  Update/remove superfluous parts.
    if (length(info$TS_info$.variables) == 1) {
        result$Vi <- list(HTML("Univariate time series<br>"))
        if (.selected ==  (.selected %in% c("Spectra", "Boot_Spectra")))
            if (.on_diagonal)
                result$spectrum_variant <- list(HTML("Showing auto-spectrum"))
    }
    if (look_up$.only_diagonal) 
        result$point_type <- list(HTML("Only diagonal points<br>"))
    if (look_up$.Shape == "points") {
        .one_point_text <- list(HTML("Only one point present"))
        if (.off_diagonal) {
            result$levels_Line <- .one_point_text
        } else {
            if (length(union(x = look_up$.Horizontal,
                             y = look_up$.Vertical)) == 1)
                result$levels <- .one_point_text
        }
    }
###-------------------------------------------------------------------
    if (.selected %in% c("Spectra", "Boot_Spectra")) {
        if (length(look_up$.dimnames[["window"]]) == 1)
            result$window <- list(HTML(
                paste("Only one available window: ",
                      look_up$.dimnames[["window"]],
                      "<br>",
                      sep = "")))
        if (all(.od_c_a_sqc, length(look_up$.od_cross) == 1)) 
            result$od_c_a_sqc <- list(HTML(
                paste("Inspection of ",
                      look_up$.od_cross,
                      "-part<br>",
                      sep = "")))
    }
    ##  Return the result to the workflow.
    result
}
