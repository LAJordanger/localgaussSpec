#' @param lag_max The number of lags to include in the analysis.  The
#'     default value \code{ceiling(3*sqrt(length(TS)))} will probably
#'     in most cases include more lags than actually required.  It
#'     might thus be worthwhile to run some test and see if a smaller
#'     value can be used instead.
