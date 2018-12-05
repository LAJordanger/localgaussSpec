#' Create the TS-info part of the dynamic \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This interface-function creates the \code{TS_info}-part of
#'     the dynamic structure of the \code{LG_shiny}-application, and
#'     it also loads the required \code{info}-object.
#'
#' @keywords internal

LG_shiny_interface_1_TS_info <- function(
    .env,
    .env2) {
    on.exit(expr = {.env$TS_logging$update$worker$TS_info <- FALSE})
###-------------------------------------------------------------------
    ##  Let the scribe take care of the updating of the
    ##  'TS_logging'-information.
    LG_shiny_interface_1_TS_info_scribe(.env, .env2)
    ## Update the 'input'-values based on the work of the scribe.
    LG_shiny_set_values(.env, .env2, .part = "TS_info")
    ##  Reminder: When this function is called during a switch between
    ##  branches, then it is of interest to ensure that any residual
    ##  triggers from the previous branch is ignored.  In particular,
    ##  set the values of all the workers to 'FALSE', and let the
    ##  chain of interface-functions take care of the rest.
    .env$TS_logging$update$worker[] <- FALSE
###-------------------------------------------------------------------
    ##  Use 'input_triggers' and 'select_names' to create the
    ##  'selectInputs'.  Reminder: The presence of 'NA'-values in the
    ##  'input_triggers$TS_info' implies that parts of the interface
    ##  should be hidden away.
    .not_NA <- ! is.na(.env$TS_logging$update$input_triggers$TS_info)
    .names <- names(.env$TS_logging$update$input_triggers$TS_info)[.not_NA]
    for (.name in names(.not_NA)[!.not_NA])
        eval(expr = bquote(output[[.(.name)]] <- NULL),
             envir = .env2)
    .result <- structure(
        .Data = lapply(
            X = .names,
            ## X = names(.env$TS_logging$update$input_triggers$TS_info)[.not_NA],
            FUN = function(x) {
                selectInput(inputId = x,
                            label = .env$TS_logging$update$select_Input[[x]]$label,
                            choices = .env$TS_logging$update$select_Input[[x]]$choices,
                            selected = .env$TS_logging$update$input_triggers$TS_info[x],
                            width = 333)
            }),
        .Names = .names)
    kill(.names)
###-------------------------------------------------------------------
    ##  Use the 'bquote' + '.()' construction to update the
    ##  'output'-list that lives in '.env2'
    for (.name in names(.result))
        eval(expr = bquote(output[[.(.name)]] <-
                               renderUI(.(bquote(.(.result)[[.(.name)]])))),
             envir = .env2)
    kill(.name, .result)
###-------------------------------------------------------------------
    ##  If enough information is present, we should now update the
    ##  'input_triggers' and the 'input'-lists with information from
    ##  the newly selected branch.  We should also add the top-level
    ##  actionButtons that selects the type of plot to be shown: 'TS',
    ##  'Correlations' or 'Spectra'.
    if (.not_NA["Approx"]) {
        .result <- list(
            TS_graphic = 
                actionButton(
                    inputId = "TS_graphic",
                    label = "TS"),
            Approx_graphic = 
                actionButton(
                    inputId = "Approx_graphic",
                    label = "Correlation"),
            Spectra_graphic = 
                actionButton(
                    inputId = "Spectra_graphic",
                    label = "Spectra"))
        ##  Create the row containing the buttons.  Reminder: In order for
        ##  the row to work properly (buttons on one line instead of three
        ##  lines), it had to be defined as a quote in this function.
        .row_quote <- bquote(fluidRow(
            ##  Add actionButton for 'TS_graphic'.
            .(.result$TS_graphic), 
            ##  Add actionButton for 'Approx_graphic'.
            .(.result$Approx_graphic),
            ##  Add actionButton for 'Spectra_graphic'.
            .(.result$Spectra_graphic) ))
        ##  Use the 'bquote' + '.()' construction to update the
        ##  'output'-list that lives in '.env2'
        eval(expr = bquote(output$TCS_type <-
                               renderUI(.(.row_quote))),
             envir = .env2)
        kill(.result, .row_quote)
        ##  Update the worker-information so the input from
        ##  'TCS_type' information can be used.
        .env$TS_logging$update$ worker$TCS_type <- TRUE
    }
###-------------------------------------------------------------------
    ##  It might happen that we have a configuration of the above
    ##  interface where a "Select..."-option is present, waiting for
    ##  further selections from the user.  In this case the desired
    ##  behaviour is to remove all the latter parts of the interface
    ##  until a proper value has been selected.
    .Select_problem <- vapply(
        X = c("TS_key", "TS", "Approx"),
        FUN = function(x) 
            if (!is.na(.env$input[[x]])) {
                stringr::str_detect(string = .env$input[[x]],
                                    pattern = "[Ss]elect")
            } else 
                TRUE,
        FUN.VALUE = logical(1))
    if (any(.Select_problem)) {
        ##  Figure out which nodes that should be included.
        .nodes_to_keep <- 1:which(.Select_problem)[1]
        .remove_these_nodes <- unlist(.env$output_nodes)[-.nodes_to_keep]
        ##  Use 'eval' + 'bquote' + '.()' construction to update the
        ##  'output'-list in '.env2', which in this case implies that
        ##  the nodes will be set to 'NULL' in order to hide them.
        for (.node in .remove_these_nodes) 
            eval(expr = bquote(output[[.(.node)]] <- NULL),
                 envir = .env2)
        ##  Update the worker-information to ensure that
        ##  'TCS_type' is not called in this case.
        .env$TS_logging$update$ worker$TCS_type <- FALSE
    }
    invisible(NULL)
}
