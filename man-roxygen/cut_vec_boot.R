#' @param cut_vec The truncation-points to be used for the functions
#'     selected in \code{window}.  This must be a subset of the values
#'     used when analysing the original time series.  If the default
#'     value \code{NULL} is used, then the function will look up the
#'     \code{cut_vec}-argument from the original local Gaussian
#'     spectral analysis.  A restriction of \code{cut_vec} to a
#'     relevant subset (based on an inspection of the original result)
#'     might be a good idea, since the computational time then can be
#'     reduced to a fraction.
