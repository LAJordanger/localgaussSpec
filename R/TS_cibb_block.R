#' Circular index-based block-of-blocks adjustment
#'
#' @param .indices The initial vector of (resampled) indices.
#'
#' @param .lag The lag-value that should be used.
#'
#' @return This helper function returns a list with two vectors, one
#'     named \code{first} and one named \code{second}.  The indices in
#'     \code{second} are those needed in order for the circular
#'     index-based block-of-blocks adjustment.
#'
#' @keywords internal

TS_cibb_block <- function(.indices, .lag) {
    .n <- length(.indices)
    .first <- head(x = .indices, n = .n - .lag)
    .tmp <- .first + .lag
    .second <- .tmp  - .n * floor((.tmp - 1)/.n)
    return(list(first = .first,
                second = .second))
}
