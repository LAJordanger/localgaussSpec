#' Explain details about the plots created by \code{LG_shiny}.
#'
#' This helper function for \code{LG_shiny} creates text that can
#' explain the content of the plots, i.e. details related to whether
#' the data is simulated or based on observations, and some summaries
#' with regard to the parameters used in the computation.
#'
#' @param input A list containing the arguments needed in order to
#'     perform the extraction.
#'
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @return An explanation will be returned.
#'
#' @keywords internal

LG_shiny_explain_plots <- function(input,
                                   .env) {
###-------------------------------------------------------------------    
    ##  This function will be triggered when the 'checkboxInput' for
    ##  it is 'TRUE'.  However, no plots will exist in the
    ##  intermediate states that occur when the investigation switches
    ##  from one branch to another.  It is thus necessary to check
    ##  that the available information actually corresponds to some
    ##  plot.  That is to say, none of the values at any of the three
    ##  first main branching points can contain 'Select' or 'NA'.
    .Select_problem <- any(vapply(
        X = c("TS_key", "TS", "Approx", "TCS_type"),
        FUN = function(x) 
            if (!is.na(.env$input[[x]])) {
                stringr::str_detect(string = .env$input[[x]],
                                    pattern = "[Ss]elect")
            } else 
                TRUE,
        FUN.VALUE = logical(1)))
    ##  Terminate if a select or 'NA' problem is present.
    if (any(.Select_problem))
        return(invisible(NULL))
    ###-------------------------------------------------------------------
    ##  Create a link to the 'Approx'-level environment, and use
    ##  'LG_lookup' to create the lookup-information.
    .AB_env <- .env$TS_logging[[unlist(.env$input[c("TS_key", "TS", "Approx")])]]
    .lookup <- LG_lookup(input = input,
                         .AB_env = .AB_env)
    ## Let 'LG_explain_plot' create the desired information, and
    ## return it to the workflow in the format needed for the
    ## shiny-interface.
    HTML(renderMarkdown(
        text = LG_explain_plot(.lookup$details),
        encoding = "UTF-8"))
}
