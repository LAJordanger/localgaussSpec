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
#' @param main_dir The part of the \code{data_dir}-argument from the
#'     calling function \code{LG_shiny}, that specifies the main path
#'     to where the information is stored.
#'
#' @param input A list containing the arguments needed in order to
#'     perform the extraction.
#'
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @param .env2 Another environment...
#' 
#' @return A plot will be returned.
#' 
#' @export

LG_plot_helper <- function(
    main_dir,
    input,
    .env,
    .env2) {
###-------------------------------------------------------------------
    ##  A minor adjustment to deal with the case when this function is
    ##  called outside of the interactive shiny-setup.
    if (missing(.env)) {
        .env <- new.env()
        .env$main_dir <- main_dir
        .env$input <- input
        ##  Need to load the 'info'-file from the desired node
        load(file = file.path(
                 paste(c(main_dir,
                         unlist(input[c("TS_key", "TS")]),
                         collapse = .Platform$file.sep),
                       LG_default$info_file_name),
                 envir = .env))
        ##  Create the 'Approx'-level environment.
        .AB_env <- LG_shiny_interface_1_helper(
            .env = .env,
            .approx = input$Approx)
    } else {
        ##  Create a link to the 'Approx'-level environment.
        .AB_env <- .env$TS_logging[[unlist(.env$input[c("TS_key", "TS", "Approx")])]]
    }
###-------------------------------------------------------------------
    ##  Do nothing if no graphical component has been selected.
    if (is.na(input$TCS_type))
        return()
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
                    paste(.env$main_dir,
                          collapse = .Platform$file.sep),
                    input$TS,
                    LG_default$info_file_name),
                .name = ".info")
    }
###-------------------------------------------------------------------
    ##  Use 'LG_lookup' to create the lookup-information.
    look_up <- LG_lookup(input = input,
                         .AB_env = .AB_env)
###-------------------------------------------------------------------
    ##  If the `get_code` check-box has been marked, return code that
    ##  can be used to create the plot in a paper.
    if  (input$get_code) {
        ##  This part must be updated based on the new approach, for
        ##  the time being simply inform about that.
        return("This part must be updated relative the new approach!")
        
        ##  Create code that can be used to call this function with
        ##  non-interactive values.  Strategy, capture and revise the
        ##  present call, then include the bare minimum of details
        ##  needed in order to get this up and running.
        .test <- match.call()
        ##  Get rid of redundant parts.
        .test[[".env"]] <- NULL
###-------------------------------------------------------------------
        
        ##  Extract/create logical values for the selection of those
        ##  components from '.input' that extract the parts of
        ##  '.input' that is required for `LG_plot_load`,
        ##  i.e. those with the following names (based on a similar
        ##  setup from 'LG_shiny_helper').
        .window <- ! is.null(look_up$window)
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
            if (! look_up$is_only_diagonal)
                "point_type",
            if (look_up$is_multivariate) 
                c("Vi",
                  "Vj"),
            if (.window)
                "window",
            "confidence_interval",
            "bw_points",
            "cut",
            if (look_up$is_on_diagonal) {
                if (length(union(x = .Horizontal,
                                 y = .Vertical)) > 1)
                    "levels_Diagonal"
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
        ##  result, so a further restriction might be needed.
        if (input$TCS_type == "C") {
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
                                              "input"),
                                     file = stdout())),
                 "",
                 deparse(.test)))
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
    ##  computations required for the desired plots to be computed.
    LG_plot_load(.look_up = look_up,
                 .env = .env)
}
