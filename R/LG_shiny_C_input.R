#' Create the correlation-specific part of the \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function removes those parts of the
#'     \code{LG_shiny}-interface that is superfluous when the target
#'     of the investigation is the correlations.
#'     
#'
#' @keywords internal

LG_shiny_C_input <- function(.env,.env2) {
###-------------------------------------------------------------------
    ##  Hide those parts of the interface that are related to the
    ##  investigation of the spectra.
    .hide_these <- c("spectrum_arguments", "spectrum_type")
    ##  Use the 'eval' + 'bquote' + '.()' construction to update the
    ##  'output'-list that lives in '.env2'
    for (h in .hide_these) 
        eval(expr = bquote(output[[.(h)]]  <- NULL),
             envir = .env2)
    return(invisible(NULL))
}
