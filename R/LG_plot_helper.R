#' Helper-function for \code{LG_plot}.
#'
#' @description This function extracts the desired array of local
#'     Gaussian (auto- or cross-) correlations from file, and then it
#'     sends those to the relevant plot-function in order to create
#'     the plot of interest.
#'
#' @details This function is used as a helper in in the interactive
#'     setup of \code{LG_shiny}, but it can also be called in a
#'     non-reactive environment.  The interactive interface can be
#'     used to trigger the creation of the code needed in order to use
#'     this function in a non-reactive setting (which can be of
#'     interest e.g. when an example should be used in a
#'     paper/presentation).
#'
#' @details The plots created by this function when it is used in a
#'     non-reactive setting, will contain an attribute \code{details}
#'     that can be used to extract information about the content.  The
#'     function \code{LG_extract_details} can be used to obtain the
#'     same information that was available in the interactive
#'     interface, or the user can extract the components of interest
#'     in order to produce other presentations too.
#'
#' @param main_dir The part of the \code{data_dir}-argument from the
#'     function \code{LG_shiny}, that specifies the main path to where
#'     the information is stored.  Note that this argument only is
#'     required when the function is called outside of the shiny
#'     application.  (Inside of the shiny-application, the relevant
#'     information will be extracted from \code{.env}.)
#'
#' @param input A list with the arguments needed in order to extract
#'     the desired data for the plot of interest.
#'
#' @param input_curlicues A list that can be used when this function
#'     is used outside of the \code{shiny}-application.  This can be
#'     used to fine-tune details related to the annotated information.
#'     It is also possible to drop annotation from the plot, which
#'     might be of interest when many plots are to be included in a
#'     grid-based setup.  The default value \code{NULL} will ensure
#'     that the plot will be created with the same setting that was
#'     used in the \code{shiny}-application (this might however not
#'     produce decent-looking plots when a grid of plots is created
#'     and saved to file).
#'
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @return The default behaviour is that a plot-object will be
#'     returned, with attributes describing the content. The
#'     information can be extracted from the plot-object by the help
#'     of \code{LG_explain_plot}.
#' 
#' @export

LG_plot_helper <- function(
    main_dir,
    input,
    input_curlicues = NULL,
    .env) {
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
        ##  REMINDER: The result of the above function turned out to
        ##  be a list. It is used other places too, so for the time
        ##  being I will use this approach as a workaround. Perhaps
        ##  add an additional argument to deal with this case?
        if (is.list(.AB_env))
            .AB_env <- .AB_env[[1]]
        ##  Add a logical value to reveal that the function is called
        ##  in a non-interactive environment.
        input$non_interactive <- TRUE
        ##  Extract the formals for the function that generated the
        ##  data.  Reminder: Such a function is not present for real
        ##  data, and this requires a minor test to avoid warnings
        ##  from the 'formals'-function.
        input$fun_formals <-
            if (is.function(.env$info$TS_info$TS_data$fun)) {
                formals(.env$info$TS_info$TS_data$fun)
            } else {
                NULL
            }
    } else {
        ##  Create a link to the 'Approx'-level environment.
        .AB_env <- .env$TS_logging[[unlist(.env$input[c("TS_key", "TS", "Approx")])]]
        input$non_interactive <- FALSE
    }
    ##  Add information about the curlicues that might have been given
    ##  by the user in the non-interactive case.
    input$input_curlicues <- input_curlicues
    ##  Do nothing if no graphical component has been selected.
    if (is.na(input$TCS_type))
        return()
    ##  If the 'get_code' check-box has been marked, return code that
    ##  can be used to create the plot in a paper.
    if  (input$get_code) {
        ##  Create the code required for this function to work outside
        ##  of the shiny-application.  Strategy: Create some quotes
        ##  that specifies the desired arguments (from '.env' and
        ##  'input') and the functions of interest (i.e.
        ##  'LG_plot_helper' and 'LG_explain_plot').  The
        ##  'digest'-function is applied in order to ensure that
        ##  unique names are used.  REMINDER: This part must be
        ##  updated!
        .controls <- c("TCS_type", "window", "Boot_Approx", ## "TS_key",
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
    ##  The case of interest for the second iteration of this function
    ##  is the part concerning 'TS', in which case we need to pick out
    ##  the desired time series from the present 'info'-object.
    if (input$TCS_type == "T") {
        return()
    }
    ##  The function 'LG_plot_load' takes care of the loading and
    ##  computations required for the desired plots to be computed, we
    ##  need to extract the desired 'look_up'-information first.
    look_up <- LG_lookup(input = input,
                         .AB_env = .AB_env)
    LG_plot_load(look_up = look_up,
                 .env = .env,
                 .extract_LG_data_only = FALSE)
}
