#' Set some values in the \code{shiny}-application
#'
#' @param .env The environment where the \code{TS_logging}-object
#'     lives.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @param .part A reference to the part(s) of the
#'     \code{input_triggers} that we want to use as basis for the
#'     updating.  The possible values are \code{c("TS_info",
#'     "TCS_type", "var_local", "second_graphical",
#'     "plots", "explanations", "derived_graphical")}.  The default is
#'     that all these will be updated.
#'
#' @return This helper-function will take values found in the
#'     \code{TS_logging}-object and use them to update both the
#'     reactive and non-reactive copies of the \code{input}-list that
#'     lives in the \code{shiny}-application.
#'
#' @keywords internal

LG_shiny_set_values <- function(.env, .env2,
                                .part = c("TS_info",
                                          "TCS_type",
                                          "var_local",
                                          "p_diag_bw",
                                          "spectrum_type",
                                          "second_graphical",
                                          "plots", "explanations",
                                          "derived_graphical")) {
    ###-------------------------------------------------------------------
    ## Reminder: In order for the updating to work, it was necessary
    ## to do the trick with an 'as.environment'-copy of '.env2$input',
    ## otherwise the updating of the reactive 'input'-version failed
    ## to work in a nonreactive context.
    .input_triggers <- .env$TS_logging$update$input_triggers
    .copy <- as.environment(.env2$input)
    for (.name in .part) {
        for (.key in names(.input_triggers[[.name]])) {
            ##  Update the reactive version.
            .copy$impl$set(key = .key,
                           value = unname(.input_triggers[[.name]][[.key]]))
            ##  Update the initial nonreactive copy.
            .env$input[[.key]] <- unname(.input_triggers[[.name]][[.key]])
        }
    }
}
##  ## Reminder: Inspection of the updated values can be done by
##  ls.str(as.environment(.env2$input)$impl$.values, all = TRUE)

