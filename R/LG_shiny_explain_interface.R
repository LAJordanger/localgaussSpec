################################################################################
#'
#' Create the instructions for \code{LG_shiny}.
#'
#' This is a helper function for \code{LG_shiny}, that should return
#' explanations with regard to the user interface.  This version is
#' still in its infancy, and is at the moment mostly intended as a
#' placeholder.
#'
#'
#' first extracts
#' the desired array from file and then creates the plot of interest.
#' The function are used as an helper in \code{LG_shiny}, in which
#' case it also will take care of the restrictions in a manner that
#' attempts to avoid the major restrictions when possible.  The
#' interactive setting can trigger the creation of code needed in
#' order to recreate a plot in a non-reactive setting too (e.g. as an
#' example in a paper/presentation), in which case some of the
#' internal workings of the function will be adjusted to allow for the
#' function call to work without the \code{shiny}-package being
#' loaded.
#'
#' @param main_dir The part of the \code{data_dir}-argument from the
#'     calling function \code{LG_shiny}, that specifies the main path
#'     to where the information is stored.
#'
#' @param input A list containing the arguments needed in order to
#'     perform the extraction.
#'
#' @param show_graphical A logical vector that specifies the kind of
#'     data that should be investigated.  Required in order to
#'     identify the correct array to work upon (since more than one
#'     can be available).
#'
#' @param path_selection_list A list containing the components needed
#'     in order to identify the correct array to work upon (since more
#'     than one can be available).
#' 
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @return An explanation will be returned.
#' 
#' @export


LG_shiny_explain_interface <- function(
    main_dir,
    input,
    show_graphical,
    path_selection_list,
    .env) {
###-------------------------------------------------------------------
    ##  Create assorted information about the interface.  Strategy:
    ##  Create a vector and convert it to html.
###-------------------------------------------------------------------
    ##  Create helper functions for the markdown-syntax.
    .italics <- function(.text) paste("_", .text, "_", sep = "")
    .bold <- function(.text) paste("__", .text, "__", sep = "")
    .item <- function(n = 1, .text) {
        .indentation <- paste(
            paste(rep(x = " ", times = 4*(n-1)),
                  collapse = ""),
            ifelse(test = {n == 1},
                   yes  = "* ",
                   no   = "+ "),
            collapse = "")
        paste(.indentation,
              .text,
              sep = "")
    }
    .header <- function(n, .text = "") 
        paste(
            paste(rep("#", n), collapse = ""),
            " ",
            .text,
            sep = "")
###-------------------------------------------------------------------
    ##  Create information about the interface.
    .main_header <- .header(
        n = 3,
        .text = "Explanation of interface:")
    TS_item <- .item(
        n = 1,
        .text = paste(
            .bold("TS:"),
            "Properties of the (pseudo-normalised) time series"))
    TS_sub_item <- .item(n=2,
                        .text = "This is a test...  ")
###-------------------------------------------------------------------
    ##  Create  the desired info-vector.
    .info_vector <- c(
        .main_header,
        TS_item,
        TS_sub_item,
        "further testing...")
    ##  Convert it to html-code
    .result <- HTML(renderMarkdown(
        text = .info_vector,
        encoding = "UTF-8"))

    
    return(.result)

#####  2017-04-02: Leave this as it is for the moment, the importan
#####  detail for the moment is the creation of explanation of the
#####  plots.
    
    
    ##  Information about the buttons.
    .TS_button <- paste(
        "The ",
        sQuote("TS"),
        "-button gives information about the (pseudo-normalised) version ",
        "of time series under investigation.",
        "()")
    
###-------------------------------------------------------------------
    ##  If no graphical component has been selected, explain the
    ##  available options.
    if (! any(show_graphical)) {

        ## capture_env() 
    }
    
    
    
    .result <- HTML(renderMarkdown(
        text = .info_vector,
        encoding = "UTF-8"))

    


    return(cat("Select a component to inspect!"))

    

    
    ## ## ## ## ## ## ## ## ## ## ## ## return(cat("Some case-spesific explanation"))
###-------------------------------------------------------------------
    ##  Avoid the requirement that the `shiny`-package must be loaded
    ##  when we only want a non-reactive plot (e.g. in a paper).
    if (! "reactivevalues" %in% is(input)) {
        isolate <- function(x) x
        is.reactive <- function(x) FALSE
        input$get_code <- FALSE
        if (missing(.env))
            .env <- new.env()
    }
###-------------------------------------------------------------------
    ##  Identify the unique graphical component to investigate.
    .selected <- names(show_graphical)[show_graphical]
###-------------------------------------------------------------------
    ##  An adjustment to ensure that this function can accept both
    ##  reactive and non-reactive arguments, with the added benefit
    ##  that it's easier to work with it in the development phase.
    .input <- vector(mode = "list", length = length(input))
    for (.name in isolate(names(input)))
        .input[[.name]] <- isolate(input[[.name]])
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  The following cases are still not implemented.
## ## ## ##     if (.selected %in% c("Approx", "Spectra", "Boot_Approx"))
## ## ## ##         return()
###-------------------------------------------------------------------
    ##  Get hold of `info`.  Ensure that we use a non-reactive version
    ##  (when this function is called from `LG_shiny`) or load it from
    ##  file in the non-reactive case.  Note: The present construction
    ##  extracts `NULL` from `.env` in the non-reactive case.
    .info <- .env[["info"]]  
    if (is.reactive(.info)) {
        .info <- isolate(.info())
    } else {
        ##  Load from file in the non-interactive case, using the name
        ##  `.info` to match the interactive case.
        LG_load(.file = file.path(
                    paste(main_dir,
                          collapse = .Platform$file.sep),
                    path_selection_list$new$TS,
                    LG_default$info_file_name),
                .name = ".info")
    }



    
    ## capture_env() 

#####  Reminder, 2017-03-31.
###  Use LG_lookup to get more details, but keep some of the stuff
###  below to get specific information about cases like "get_code".
    
    
## ###-------------------------------------------------------------------
##     ##  If the `get_code` check-box has been marked, return code that
##     ##  can be used to create the plot in a paper.
##     if (.input$get_code) {
##         ##  Create code that can be used to call this function with
##         ##  non-interactive values.  Strategy, capture and revise the
##         ##  present call, then include the bare minimum of details
##         ##  needed in order to get this up and running.
##         .test <- match.call()
##         ##  Get rid of redundant parts.
##         .test[[".env"]] <- NULL
##         ##  Only the `new`-component of `path_selection_list` is
##         ##  needed for the revised call, prune away the rest of it.
##         path_selection_list$old <- NULL
##         ##  Use 'LG_lookup' to find (among others) the three values
##         ##  '.only_diagonal', '.multivariate' and '.Shape'.
##         LG_lookup(.info = .info,
##                   .selected = .selected,
##                   .path_selection_list = path_selection_list)
##         ##  Extract the parts of inputs that is required for
##         ##  `LG_plot_load`, i.e. those with the following names (based
##         ##  on a similar setup from 'LG_shiny_helper').
## ###-------------------------------------------------------------------
##         ##  Create logical values for the selecting of components.
##         .auto_pair <- if (.multivariate) {
##                           .input$Vi == .input$Vj
##                       } else
##                           TRUE
##         .on_diagonal <- if (.only_diagonal) {
##                             TRUE
##                         } else
##                             .input$point_type == "on_diag"
##         .off_diagonal <- ! .on_diagonal
##         .spectrum_variant <- ! all(.auto_pair, .on_diagonal)
##         .od_c_a_sqc <- all(
##             if (.spectrum_variant) {
##                 .input$spectrum_variant == "squared_coherence"
##             } else
##                 FALSE,
##             ! .auto_pair,
##             .off_diagonal)
##         .window <- ! is.null(.input$window)
## ###-------------------------------------------------------------------    
##         ##  Select the parts (and orders of) 'interface' needed for
##         ##  the computation.  NB: This is a similar setup to the one
##         ##  used in 'LG_plot_helper', but with all tests collected at
##         ##  the initiation of the names.
##         .controls <- c(
##             "type",
##             if (! .only_diagonal)
##                 "point_type",
##             if (.multivariate) 
##                 c("Vi",
##                   "Vj"),
##             if (.spectrum_variant)
##                 "spectrum_variant",
##             if (.od_c_a_sqc)
##                 "od_c_a_sqc",
##             if (.window)
##                 "window",
##             "confidence_interval",
##             "bw_points",
##             "cut",
##             if (.on_diagonal) {
##                 if (length(union(x = .Horizontal,
##                                  y = .Vertical)) > 1)
##                     "levels"
##             } else {
##                 switch(EXPR = .Shape,
##                        points = NULL,
##                        line = "levels_Line",
##                        rectangle = c("levels_Horizontal",
##                                      "levels_Vertical"))
##             },
##             "omega_range",
##             "Y_range")
##         input <- .input[.controls]
##         return(c(capture.output(dump(list = c("main_dir",
##                                               "show_graphical",
##                                               "input",
##                                               "path_selection_list"),
##                                      file = stdout())),
##                  deparse(.test)))
##     }
##     kill(.name, input, show_graphical)
## ###-------------------------------------------------------------------
##     ##  The case of interest for the second iteration of this function
##     ##  is the part concerning 'TS', in which case we need to pick out
##     ##  the desired time series from the present 'info'-object.
##     if (.selected == "TS") {
##         ##  Get a local copy of 'TS_graphical_changes' 
##         TS_graphical_changes <- get(
##             x = "TS_graphical_changes",
##             envir = .env)
##         ##  Terminate if no selection has been made.
##         if (! any(TS_graphical_changes))
##             return()
## ###-------------------------------------------------------------------
##         ## Use the key that identifies the latest selected button.
##         TS_graphical_changes <-
##             names(TS_graphical_changes)[TS_graphical_changes]
##         ##  Use the `TS`-info from '.info' to load `TS`.
## ###-------------------------------------------------------------------
##         ##  The 'TS'-argument refers to a file, and it might also be
##         ##  an attribute 'TS_for_analysis' that should be used.  Some
##         ##  tweaking is needed in order to load this into the system.
##         load(file = paste(c(main_dir,
##                             .info$TS_info$TS),
##                           collapse = .Platform$file.sep))
##         ##  If 'TS' originates from a 'TS_LG_object', use the
##         ##  attribute 'TS_for_analysis' instead of TS.
##         if (! identical(x = attributes(TS)$TS_for_analysis,
##                         y = NULL)) 
##             TS <- attributes(TS)$TS_for_analysis
##         ##  Ensure the plot is given as a time-series.
##         TS <- as.ts(x = TS)
## #####  TASK: This mess stuff up when blocks are considered.
## ###-------------------------------------------------------------------
##         ##--- Create an 'xlim'-argument for 'acf' and 'pacf'
##         .lag_max <- min(100,
##                         ceiling(0.9 * length(TS)))
##         ##  Create the desired graphical display
##         result <- switch(
##             EXPR = TS_graphical_changes,
##             TS_plot = plot(TS, las = 1, ylab = ""),
##             TS_acf = acf(TS, lag.max = .lag_max, las = 1, ylab = ""),
##             TS_pacf = pacf(TS, lag.max = .lag_max, las = 1, ylab = ""),
##             TS_spec.pgram = spec.pgram(TS))
##         ##  Do the required computations if it's 'TS_lags' that has
##         ##  been selected.
##         if (TS_graphical_changes == "TS_lags") {
##             a <- ifelse(
##                 test = identical(.input$lag_slider, NULL),
##                 yes  = 1,
##                 no   = .input$lag_slider)
##             ##---
##             .x <- head(as.vector(TS), n = -a)
##             .y <- tail(as.vector(TS), n = -a)
##             .localgauss_result <- localgauss(
##                 x = .x,
##                 y = .y)
##             result <- plot(
##                 .localgauss_result,
##                 plot.text = FALSE,
##                 plot.points = TRUE)
##         }
##         ##  Return the graphical display.
##         return(result)
##     }
## ###-------------------------------------------------------------------
##     ##  The 'Boot_Spectra'-case, i.e. confidence intervals.
## ###-------------------------------------------------------------------
##     ##  Ensure that the required data is available, in a manner that
##     ##  avoids redoing more computations than necessary.  This is
##     ##  taken care of by `LG_plot_load`, that updates `.env` and
##     ##  compares any adjustments of the selected values to figure out
##     ##  what has to be done.  Note: This function will add the
##     ##  following components to the present environment: `.aes_xy`,
##     ##  `.aes_min_max`, `.canvas`, `data.local`, '.selected_lag'
##     ##  `.selected_percentile` and '.ylim'.

##     LG_plot_load_call <- create_call(
##         .cc_fun = LG_plot_load,
##         .selected = .selected,
##         .path_selection_list = path_selection_list,
##         .input = .input,
##         .info = .info,
##         .env = .env)


##     ## capture_env() 
    
## #####  The solution below, can with some tweaking give me a much more
## #####  efficient method of inspecting these things.  In particular,
## #####  use 'local = TRUE' in order to allow the code to run on, and
## #####  then store the resulting values into the global environment
## #####  directly.
##     ## I <- get(x = "I",
##     ##          envir = .GlobalEnv)
##     ## .GlobalEnv$I <- I + 1
    
##     ## capture_env(
##     ##     global_name = paste("captured_env", I, sep = "")
##     ## ) 



    
## ## ## ## ################################################################################
## ## ## ##     ##  Intermezzo to better capture the call at a specified time.
## ## ## ##     LG_lookup(.info = .info,
## ## ## ##               .selected = .selected,
## ## ## ##               .path_selection_list = path_selection_list)
    
## ## ## ##     ##  Create logical values for the selecting of components.
## ## ## ##     .auto_pair <- if (.multivariate) {
## ## ## ##                       .input$Vi == .input$Vj
## ## ## ##                   } else
## ## ## ##                       TRUE
## ## ## ##     .on_diagonal <- if (.only_diagonal) {
## ## ## ##                         TRUE
## ## ## ##                     } else
## ## ## ##                         .input$point_type == "on_diag"
## ## ## ##     .off_diagonal <- ! .on_diagonal
    
## ## ## ##     if (.off_diagonal)
## ## ## ##         capture_env() 

##     LG_plot_load(.selected = .selected,
##                  .path_selection_list = path_selection_list,
##                  .input = .input,
##                  .info = .info,
##                  .env = .env)
## ###-------------------------------------------------------------------
##     ## LG_plot_call <- create_call(
##     ##     .cc_fun = LG_plot,
##     ##     .data = .data_local,
##     ##     .lag = .selected_lag,
##     ##     .percentile = .selected_percentile,
##     ##     .select = "add",
##     ##     .canvas = .canvas,
##     ##     .aes_xy = .aes_xy,
##     ##     .aes_min_max = .aes_min_max,
##     ##     .sanity_checks = FALSE)

    
##     ##  Return the desired plot to the workflow.
##     LG_plot(
##         .data = .data_local,
##         .lag = .selected_lag,
##         .percentile = .selected_percentile,
##         .select = "add",
##         .canvas = .canvas,
##         .aes_xy = .aes_xy,
##         .aes_min_max = .aes_min_max,
##         .sanity_checks = FALSE)  +  ##  Adjust the ylimit
##         coord_cartesian(ylim = .ylim)
}
