#' @param log_ A logic argument with default value \code{FALSE}.  This
#'     decides if the statistics should be computed based on the
#'     logarithmic values of our replicates (when that makes sense,
#'     i.e. when we have nonzero values).  It might be preferable to
#'     use \code{log_=TRUE}, but as we have no guarantee that it
#'     occasionally might not occur negative values for the estimated
#'     local Gaussian spectra, the default has nevertheless been set
#'     to \code{FALSE}.
