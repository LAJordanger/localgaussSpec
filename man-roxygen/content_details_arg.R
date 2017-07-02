#' @param content_details A character string from \code{c("rho_only",
#'     "rho_log.fun", "rho_all")}, of which the first will be selected
#'     if the user doesn't specify something else.  The purpose of
#'     this argument is to decide the amount of details that should be
#'     stored from the estimates governed by the argument
#'     \code{LG_type}.  The default \code{rho_only} is the one needed
#'     for the computation of the Local Gaussian Spectral Densities,
#'     whereas \code{rho_log.fun} in addition returns the estimated
#'     values of the logarithm of the density.  The option
#'     \code{rho_all} will in addition return all the parameters when
#'     \code{LG_type} is given as \code{par_five}.
