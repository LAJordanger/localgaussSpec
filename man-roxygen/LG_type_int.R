#' @param LG_type One of \code{c("par_five", "par_one")}, i.e. should
#'     the Local Gaussian Autocorrelations be based on the approach
#'     using five parameters or the simplified approach using only one
#'     parameter.  Note that even though \code{approx_data} contains
#'     information about two cases, the code later on will only work
#'     upon one case at the time (due to memory restrictions) and it's
#'     thus necessary to specify the part of interest.
