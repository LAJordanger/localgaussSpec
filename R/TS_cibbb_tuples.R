#' Circular index-based block bootstrap for tuples
#'
#' @param .indices The initial vector of (resampled) indices.
#'
#' @param .lag The lag-value that should be used.
#'
#' @return This helper function returns a list with two vectors, one
#'     named \code{first} and one named \code{second}.  These are the
#'     indices needed for the circular index-based block bootstrap for
#'     tuples.
#'
#' @keywords internal

TS_cibbb_tuples <- function(.indices, .lag) {
    .n <- length(.indices)
    ##  The subsetting used in the local Gaussian spectra-papers
    ##  implies that the first subsetting shoud be with regard to 't'
    ##  and the the second subsetting with regard to 't+h'.
    .second <- head(x = .indices, n = .n - .lag)
    .tmp <- .second + .lag
    .first <- .tmp  - .n * floor((.tmp - 1)/.n)
    return(list(first = .first,
                second = .second))
}
