################################################################################
#'
#' Helper for \code{LG_plot}.
#'
#' This is a helper function for \code{LG_plot}, that first extracts
#' the desired array from file and then creates the plot of interest.
#' The function are used as an helper in \code{LG_shiny}, in which
#' case it also will take care of the restrictions in a manner that
#' attempts to avoid the major restrictions when possible.  The
#' interactive setting can trigger the creation of code needed in
#' order to re-create a plot in a non-reactive setting too (e.g. as an
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
#' @return A plot will be returned.
#' 
#' @export


LG_plot_helper <- function(
    main_dir,
    input,
    show_graphical,
    path_selection_list,
    .env) {
###-------------------------------------------------------------------
    ##  It's assumed that the sanity-checks of the arguments have been
    ##  performed by the calling function.
###-------------------------------------------------------------------
    ##  Do nothing if no graphical component has been selected.
    if (! any(show_graphical))
        return()
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
    ##  Ensure that 'main_dir' is contained in .env
    .env$main_dir <- main_dir
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
    kill(input)
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
###-------------------------------------------------------------------
    ##  Use 'LG_lookup' to update '.input' with the two nodes
    ##  'look_up' and 'details' to be used later on.  Reminder:
    ##  'look_up' is used for the loading of the desired data, whereas
    ##  'details' is used by 'LG_explain_plot' to give information
    ##  about the content of the plot created by the selected values.

    ## LG_lookup_call <- create_call(
    ##     .cc_fun = LG_lookup,
    ##     .info = .info,
    ##     .selected = .selected,
    ##     .path_selection_list = path_selection_list,
    ##     .input = .input)
    ## capture_env() 

    LG_lookup(.info = .info,
              .selected = .selected,
              .path_selection_list = path_selection_list,
              .input = .input)
    ##  Extract the detail of interest from '.input'.
    look_up <- .input$look_up



    
###-------------------------------------------------------------------
    ##  If the `get_code` check-box has been marked, return code that
    ##  can be used to create the plot in a paper.
    if (look_up$get_code) {
        ##  Create code that can be used to call this function with
        ##  non-interactive values.  Strategy, capture and revise the
        ##  present call, then include the bare minimum of details
        ##  needed in order to get this up and running.
        .test <- match.call()
        ##  Get rid of redundant parts.
        .test[[".env"]] <- NULL
        ##  Only the `new`-component of `path_selection_list` is
        ##  needed for the revised call, prune away the rest of it.
        path_selection_list$old <- NULL
###-------------------------------------------------------------------
        ##  Extract/create logical values for the selection of those
        ##  components from '.input' that ## Extract the parts of '.input'
        ##  that is required for `LG_plot_load`, i.e. those with the
        ##  following names (based on a similar setup from
        ##  'LG_shiny_helper').
        .only_diagonal <- look_up$.only_diagonal
        .multivariate <- look_up$.multivariate
        .spectrum_variant <- look_up$.spectrum_variant
        .od_c_a_sqc <- look_up$.od_c_a_sqc
        .window <- ! is.null(look_up$window)
        .on_diagonal <- look_up$.on_diagonal
        .Horizontal <- look_up$.Horizontal
        .Vertical <- look_up$.Vertical
        .Shape <- look_up$.Shape
###-------------------------------------------------------------------    
        ##  Select the parts (and orders of) 'interface' needed for
        ##  the computation.  NB: This is a similar setup to the one
        ##  used in 'LG_shiny_helper', but with all tests collected at
        ##  the initiation of the names.
        .controls <- c(
            "type",
            if (! .only_diagonal)
                "point_type",
            if (.multivariate) 
                c("Vi",
                  "Vj"),
            if (.spectrum_variant)
                "spectrum_variant",
            if (.od_c_a_sqc)
                "od_c_a_sqc",
            if (.window)
                "window",
            "confidence_interval",
            "bw_points",
            "cut",
            if (.on_diagonal) {
                if (length(union(x = .Horizontal,
                                 y = .Vertical)) > 1)
                    "levels"
            } else {
                switch(EXPR = .Shape,
                       points = NULL,
                       line = "levels_Line",
                       rectangle = c("levels_Horizontal",
                                     "levels_Vertical"))
            },
            "omega_range",
            "Y_range")
        ##  Read the desired arguments from 'look_up'.
        input <- look_up[.controls]
        ##  There might still be some redundancy in the returned
        ##  result, so a furhter restriction might be needed.
        if (.selected == "Approx") {
            .Approx_input <-
                c("type", "point_type", "Vi", "Vj", "bw_points",
                  "levels", "levels_Line", "Levels_Horizontal",
                  "Leveles_Vertical")
            .Approx_restriction <- intersect(
                x = names(input),
                y = .Approx_input)
            input <- input[.Approx_restriction]
        }
        ##  Return the desired code to be returned.
        return(c(capture.output(dump(list = c("main_dir",
                                              "show_graphical",
                                              "input",
                                              "path_selection_list"),
                                     file = stdout())),
                 "",
                 deparse(.test)))
    }
    kill(.name, show_graphical)

    
###-------------------------------------------------------------------
    ##  The case of interest for the second iteration of this function
    ##  is the part concerning 'TS', in which case we need to pick out
    ##  the desired time series from the present 'info'-object.
    if (.selected == "TS") {
        ##  Get a local copy of 'TS_graphical_changes' 
        TS_graphical_changes <- get(
            x = "TS_graphical_changes",
            envir = .env)
        ##  Terminate if no selection has been made.
        if (! any(TS_graphical_changes))
            return()
###-------------------------------------------------------------------
        ## Use the key that identifies the latest selected button.
        TS_graphical_changes <-
            names(TS_graphical_changes)[TS_graphical_changes]
        ##  Use the `TS`-info from '.info' to load `TS`.
###-------------------------------------------------------------------
        ##  The 'TS'-argument refers to a file, and it might also be
        ##  an attribute 'TS_for_analysis' that should be used.  Some
        ##  tweaking is needed in order to load this into the system.
        load(file = paste(c(main_dir,
                            .info$TS_info$TS),
                          collapse = .Platform$file.sep))
        ##  If 'TS' originates from a 'TS_LG_object', use the
        ##  attribute 'TS_for_analysis' instead of TS.
        if (! identical(x = attributes(TS)$TS_for_analysis,
                        y = NULL)) 
            TS <- attributes(TS)$TS_for_analysis
        ##  Ensure the plot is given as a time-series.
        TS <- as.ts(x = TS)
#####  TASK: This mess stuff up when blocks are considered.
###-------------------------------------------------------------------
        ##--- Create an 'xlim'-argument for 'acf' and 'pacf'
        .lag_max <- min(100,
                        ceiling(0.9 * length(TS)))
        ##  Create the desired graphical display
        result <- switch(
            EXPR = TS_graphical_changes,
            TS_plot = plot(TS, las = 1, ylab = ""),
            TS_acf = acf(TS, lag.max = .lag_max, las = 1, ylab = ""),
            TS_pacf = pacf(TS, lag.max = .lag_max, las = 1, ylab = ""),
            TS_spec.pgram = spec.pgram(TS))
        ##  Do the required computations if it's 'TS_lags' that has
        ##  been selected.
        if (TS_graphical_changes == "TS_lags") {
            a <- ifelse(
                test = identical(look_up$lag_slider, NULL),
                yes  = 1,
                no   = look_up$lag_slider)
            ##---
            .x <- head(as.vector(TS), n = -a)
            .y <- tail(as.vector(TS), n = -a)
            .localgauss_result <- localgauss(
                x = .x,
                y = .y)
            result <- plot(
                .localgauss_result,
                plot.text = FALSE,
                plot.points = TRUE)
        }
        ##  Return the graphical display.
        return(result)
    }
###-------------------------------------------------------------------
    ##  The 'Boot_Spectra'-case, i.e. confidence intervals.
###-------------------------------------------------------------------
    ##  Ensure that the required data is available, in a manner that
    ##  avoids redoing more computations than necessary.  This is
    ##  taken care of by `LG_plot_load`, that updates `.env` and
    ##  compares any adjustments of the selected values to figure out
    ##  what has to be done.  Note: This function will add different
    ##  components into the present environment, depeding of the value
    ##  of '.selected': 'Approx' gives '.xlim', '.ylim', '.aes_xy',
    ##  '.lag_local_data_frame', '.selected_percentile'.
    ##  'Boot_Spectra' gives `.aes_xy`, `.aes_min_max`, `.canvas`,
    ##  `data.local`, '.selected_lag' `.selected_percentile` and
    ##  '.ylim';

    
#####  The solution below, can with some tweaking give me a much more
#####  efficient method of inspecting these things.  In particular,
#####  use 'local = TRUE' in order to allow the code to run on, and
#####  then store the resulting values into the global environment
#####  directly.
    ## I <- get(x = "I",
    ##          envir = .GlobalEnv)
    ## .GlobalEnv$I <- I + 1
    
    ## capture_env(
    ##     global_name = paste("captured_env", I, sep = "")
    ## ) 
    
    
    ## LG_plot_load_call <- create_call(
    ##     .cc_fun = LG_plot_load,
    ##     .look_up = look_up,
    ##     .env = .env)
    ## capture_env() 

    LG_plot_load(.look_up = look_up,
                 .env = .env)
}
