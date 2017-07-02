#' @param omega_vec A vector of frequencies.  When the default value
#'     \code{NULL} is used, the function will use
#'     \code{omega_length_out} to create the frequency-vector.  Note
#'     that the values given here should be in the interval from
#'     \code{0} to \code{1/2}, since they later on will be multiplied
#'     with \code{2*pi} in the computation.  This makes the baseline
#'     for white noise \code{1} instead of \code{1/2pi}, which is
#'     preferable when creating the plots later on.
