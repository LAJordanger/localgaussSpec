#' Create graphical parameters for the \code{LG_shiny}-interface.
#'
#' @description This internal function will, based on the values in
#'     the \code{TCS_type} part of the \code{input_triggers}, create
#'     the next part which is \code{var_local}.  This function will
#'     also take care of some of the bookkeeping.
#' 
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This interface-function creates another graphical
#'     parameters part of the \code{LG_shiny}-application.
#'
#' @keywords internal

LG_shiny_TCS_input  <- function(
    .env,
    .env2) {
    on.exit(expr = {.env$TS_logging$update$worker$TCS_type <- FALSE})
    ##  In order to avoid unnecessary updates when an actionButton has
    ##  been pushed more than once, it is necessary to have a logical
    ##  value that decides if an update should be performed.
    ##  Moreover, also need to initiate if it is necessary to remove
    ##  some nodes due to insufficient information
    .update_required  <- FALSE
    .remove_these_nodes <- c()
    ##  Create a shortcut to the environment of interest.
    ..env <- LG_shiny_interface_0_RW_log(.env, .env2, .RW="L")
    ##  Identify old and new values for the triggers.
    .old_values <- .env$TS_logging$update$input_triggers$TCS_type
    .new_values <- .env$input[names(.old_values)]
    ##  Update the stored values.
    .env$TS_logging$update$input_triggers$TCS_type <- .new_values
    ##  In the initiation-phase everything will be '0' in
    ##  '.new_values', and the desired trigger must then be extracted
    ##  from 'TS_logging'. This will be 'NA' the first time around,
    ##  but later on it will be a recording of the value used the last
    ##  time the node was visited.
    if (sum(unlist(.new_values)) == 0) {
        ###  REMINDER: Switch the if-test back to the difference
        ###  between new and old when the update of TS_logging is
        ###  performed (i.e. when the stored values are used again).
        .trigger <- ..env$.derived_graphical$TCS_type[1]
        .env$TS_logging$update$input_triggers$
            derived_graphical["TCS_type"] <- .trigger
        ##  Update the information in the 'input'-lists based on the
        ##  present content of the saved data.
        LG_shiny_set_values(.env, .env2)
        ##  If '.trigger' is 'NA', then some of the nodes later on
        ##  should be hidden away.
        if (is.na(.trigger)) {
            ##  Identify the node-parts to remove.
            .tmp <- setdiff(x = names(.env$output_nodes),
                            y = c("TS_info", "TCS"))
            .remove_these_nodes <- unlist(.env$output_nodes[.tmp])
        ##  Use 'eval' + 'bquote' + '.()' construction to update the
        ##  'output'-list in '.env2', which in this case implies that
        ##  the nodes will be set to 'NULL' in order to hide them.
            for (.node in .remove_these_nodes) 
                eval(expr = bquote(output[[.(.node)]] <- NULL),
                     envir = .env2)
            return(invisible(NULL))
        } else
            .update_required <- TRUE
    } else {
        ##  Detect the position with the difference, i.e. the position
        ##  where the value increased with one.  Use the name of that
        ##  position to select a key that describes if the desired
        ##  graph should be related to the time series itself (T),
        ##  based on the local Gaussian correlations (C), or based on
        ##  the local Gaussian spectra (S)
        .trigger <- switch(
            EXPR = names(.old_values)[as.logical(unlist(.new_values) - unlist(.old_values))],
            TS_graphic = "T",
            Approx_graphic = "C",
            Spectra_graphic = "S")
        ##  Use the trigger to update the information in '..env'
        ..env$.derived_graphical$TCS_type <- c(
            .trigger,
            ..env$.derived_graphical$TCS_type[1])
        ##  Check if this requires an actual update of the graphical
        ##  interface, i.e. if a new value has been selected.
        if (! identical(x = ..env$.derived_graphical$TCS_type[1],
                        y = ..env$.derived_graphical$TCS_type[2])) {
            .update_required  <- TRUE
         }
    }
    ##  Update the information in the input-lists too.
    .env$TS_logging$update$input_triggers$derived_graphical["TCS_type"] <-
        .trigger
    LG_shiny_set_values(.env, .env2,
                        .part = "derived_graphical")
    ##  Terminate if nothing should be done, otherwise continue.
    if (!.update_required) {
        return(NULL)
    } else
        kill(.update_required, .old_values, .new_values)
    ##  Still running?  Then a switch has happened, and it is (most
    ##  likely) necessary to update the log-information.
    LG_shiny_interface_0_RW_log(.env, .env2, .RW = "W")
    ##  Use '.trigger'-value to call the functions that creates the
    ##  required trigger-specific interfaces.
    if (.trigger != "T") {
        ##  Initiate the 'CS'-part of the interface.
        LG_shiny_CS_input(.env, .env2, .part = "var_local")
    } else {
        ##  Initiate the 'T'-part of the interface.
        LG_shiny_T_input(.env, .env2)
    }
    return(invisible(NULL))
}
