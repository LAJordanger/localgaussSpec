#' Helper-function for \code{LG_plot}.
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
#' NOTE: This function is exported in order for interesting plots to
#' be recreated outside of the shiny-application, in which case the
#' returned plot-objects also will contain attributes needed for the
#' proper identification of the content of the plots.  The idea is
#' that the required parameters of this function will be filled in by
#' the shiny-application, and even though it is possible to adjust the
#' input parameters manually, it is not recommended.
#'
#' @param main_dir The part of the \code{data_dir}-argument from the
#'     calling function \code{LG_shiny}, that specifies the main path
#'     to where the information is stored.  Note that this argument
#'     only is required when the function is called outside of the
#'     shiny application.  (Inside of the shiny-application, the
#'     relevant information will be extracted from \code{.env}.)
#'
#' @param input A list with the arguments needed in order to extract
#'     the desired data for the plot of interest.
#'
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @return A plot-object will be returned, with attributes describing
#'     the content. The information can be extracted from the
#'     plot-object by the help of \code{LG_explain_plot}.
#' 
#' @export

LG_plot_helper <- function(
    main_dir,
    input,
    .env) {
###-------------------------------------------------------------------
    ##  A minor adjustment to deal with the case when this function is
    ##  called outside of the interactive shiny-setup.
    if (missing(.env)) {
        ##  Update 'input' with 'get_code' in order for correct path
        ##  to be selected later on.
        input$get_code <- FALSE
        .env <- new.env()
        .env$main_dir <- main_dir
        .env$input <- input
        ##  Need to load the 'info'-file from the desired node
        load(file = file.path(
                 paste(c(main_dir,
                         input$TS,
                         LG_default$info_file_name),
                       collapse = .Platform$file.sep)),
             envir = .env)
        ##  Create the 'Approx'-level environment.
        .AB_env <- LG_shiny_interface_1_helper(
            .env = .env,
            .approx = input$Approx)
        ##  REMINDER, 2019-08-23: The result of the above function
        ##  turned out to be a list. It is used other places to, so I
        ##  guess I for the time being will use this approach as a
        ##  workaround. Perhaps add an additional argument to deal
        ##  with this case?
        if (is.list(.AB_env))
            .AB_env <- .AB_env[[1]]
    } else {
        ##  Create a link to the 'Approx'-level environment.
        .AB_env <- .env$TS_logging[[unlist(.env$input[c("TS_key", "TS", "Approx")])]]
    }
###-------------------------------------------------------------------
    ##  Do nothing if no graphical component has been selected.
    if (is.na(input$TCS_type))
        return()
###-------------------------------------------------------------------
    ##  If the `get_code` check-box has been marked, return code that
    ##  can be used to create the plot in a paper.
    if  (input$get_code) {
        ##  Create the code required for this function to work outside
        ##  of the shiny-application.  Strategy: Create some quotes
        ##  that specifies the desired arguments (from '.env' and
        ##  'input') and the functions of interest (i.e.
        ##  'LG_plot_helper' and 'LG_explain_plot').  The
        ##  'digest'-function is applied in order to ensure that
        ##  unique names are used.
        ##  Reminder, 2019-08-26: The selection below is not optimal.
        .controls <- c("TCS_type", "window", "Boot_Approx", "TS_key",
                       "confidence_interval", "levels_Diagonal",
                       "bw_points", "cut", "frequency_range", "type",
                       "levels_Horizontal", "TS", "TS_lags", "S_type",
                       "levels_Line", "point_type", "Approx", "Vi",
                       "Vj", "levels_Vertical", "global_local")
        ..input <- input[.controls]
        .digest_value <- digest::digest(..input)
        ..input_name <- as.name(sprintf("input_%s", .digest_value))
        ..plot_name <-  as.name(sprintf("plot_%s", .digest_value))
        ..explanation_name <- as.name(sprintf("explanation_%s", .digest_value))
        main_dir_quote <- bquote(..main_dir <- .(.env$main_dir))
        input_quote <- bquote(.(..input_name) <- .(..input))
        LG_plot_helper_quote <- bquote(
            .(..plot_name) <- LG_plot_helper(
                main_dir = ..main_dir,
                input = .(..input_name)))
        LG_explain_plot_quote <- bquote(
            .(..explanation_name) <- LG_explain_plot(
                .plot_details = .(..plot_name)))
        ##  Return the code needed for the creation of the plot and
        ##  for the investigation of the content of the plot.
        return(c(deparse(main_dir_quote),
          deparse(input_quote),
          "",
          deparse(LG_plot_helper_quote),
          "",
          deparse(LG_explain_plot_quote)))
    }
    kill(.name)
###-------------------------------------------------------------------
    ##  The case of interest for the second iteration of this function
    ##  is the part concerning 'TS', in which case we need to pick out
    ##  the desired time series from the present 'info'-object.
    if (input$TCS_type == "T") {
        return()
    }
###-------------------------------------------------------------------
    ##  The function 'LG_plot_load' takes care of the loading and
    ##  computations required for the desired plots to be computed, we
    ##  need to extract the desired 'look_up'-information first.
###-------------------------------------------------------------------
    ##  Use 'LG_lookup' to create the lookup-information.
    look_up <- LG_lookup(input = input,
                         .AB_env = .AB_env)
    LG_plot_load(.look_up = look_up,
                 .env = .env)
}
