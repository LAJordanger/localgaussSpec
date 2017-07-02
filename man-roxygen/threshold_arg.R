#' @param threshold An integer, default value 500 (measured in MB),
#'     that tells the program when a computation should be divided
#'     into smaller chunks.  This reduces the chance of memory-related
#'     problems, but as the present incarnation of \code{LG_splitter}
#'     are rather stupid such problems might still occur for long
#'     time-series where a huge number of lags are included.
