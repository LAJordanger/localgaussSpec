#' Create the spectrum-specific part of the \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function creates the \code{actionButtons} needed for
#'     the spectrum-specific part of the \code{LG_shiny}-interface.
#'     (Not implemented yet.)
#'
#' @keywords internal

LG_shiny_T_input <- function(.env,.env2) {
    ##  Hide nodes related to the CS-part of the interface.  At the
    ##  present time of development this is simply the crude task of
    ##  hiding all the stuff after the selection of the main graphical
    ##  triggers.
    .tmp <- setdiff(x = names(.env$output_nodes),
                            y = c("TS_info", "TCS"))
    .remove_these_nodes <- unlist(.env$output_nodes[.tmp])
    ##  Use 'eval' + 'bquote' + '.()' construction to update the
    ##  'output'-list in '.env2', which in this case implies that the
    ##  nodes will be set to 'NULL' in order to hide them.
    for (.node in .remove_these_nodes) 
        eval(expr = bquote(output[[.(.node)]] <- NULL),
             envir = .env2)
###-------------------------------------------------------------------
    ##  Create a link to the environment of interest, where the
    ##  information needed for the fine tuning of the interface can be
    ##  found.  (Needed in order to simplify the interface when
    ##  e.g. only one value is present.)
    ..env <- LG_shiny_interface_0_RW_log(.env, .env2, .RW = "L")
    ## ## START TS_investigation, where we want to compare the original
    ## ## and the normalised time series, where we would like to compare
    ## ## the ordinary spectral densities based on these, and where we
    ## ## also might want to plot some lagged pairs in order to see how
    ## ## things behave.  It might in addition be of interest to have
    ## ## some investigation of the number of points within some
    ## ## bandwidth-boxes
    print("Time series interface not implemented yet.")
}
