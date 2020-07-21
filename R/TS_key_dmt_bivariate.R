#' "Local trigonometric" bivariate samples based on cosine-functions
#'
#' @description The \code{TS_key_dmt_bivariate}-function is based on a
#'     a simple wrapper of the \code{TS_key_dmt}-function, which
#'     enables a bivariate "local trigonometric" example to be
#'     generated.  The basic idea is that two components are computed
#'     in succession by the help of \code{TS_key_dmt}, and that the
#'     arguments of the second component only differs by a
#'     phase-adjustment of the \code{theta}-argument.
#'
#' @note This internal function has been added to the
#'     \code{TS_families}-list, and its arguments should thus be given
#'     to \code{TS_sample}, which will send them to this function by
#'     the internal helper-function \code{TS_sample_helper}.
#'
#' @param n The number of samples to create.
#'
#' @param first_dmt A list with all the arguments for the
#'     \code{TS_key_dmt}-function.
#'
#' @param phase_adjustment The numerical value(s) given here will be
#'     added to the \code{theta}-value(s) in the
#'     \code{first_dmt}-value.  A single number will imply that all
#'     the cosine-functions will have the same phase-adjustment,
#'     whereas different numbers for each \code{theta}-value in
#'     \code{first_dmt} will give individual phase-adjustments.
#'
#' @return A bivariate "local trigonometric" sample is returned.

TS_key_dmt_bivariate <- function(n,
                          first_dmt,
                          phase_adjustment = 0) {
    ##  Join 'n' and 'first_dmt' into one list, and then use 'dmt' to
    ##  get the first component
    first_dmt <- c(
        list(n = n),
        first_dmt)
    .first_part <- do.call(
        what = dmt,
        args = first_dmt)
    ##  Extract the (possibly revised) arguments, adjust with
    ##  'phase_adjustment', and create the second component.
    second_dmt <- attributes(.first_part)$args
    second_dmt$theta <- second_dmt$theta + phase_adjustment
    .second_part <- do.call(
        what = dmt,
        args = second_dmt)
    ##  Combine the two pieces.
    cbind(X1 = .first_part,
          x2 = .second_part)
}

###------------------------------------------------------###

##  Add the new function to "TS_families".  Initiate it if it does not
##  exists.

if (! exists(x="TS_families", inherits = FALSE))
    TS_families <- list()

##  Add the new function to "TS_families".
TS_families$dmt_bivariate  <- 
    list(package = "localgaussSpec",
         fun = "TS_key_dmt_bivariate",
         args = list(first_dmt =
                         list(A = rbind(c(-2, -1, 0, 1),
                                        c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                              delta = c(1.0, 0.5, 0.3, 0.5),
                              delta_range = NULL,
                              alpha = c(pi/2, pi/6, pi/3, pi/4),
                              theta = NULL,
                              wn = list(type = "rnorm",
                                        args = list(mean = 0,
                                                    sd = 1),
                                        adjust = 0)),
                     phase_adjustment = pi/6),
         size_name = "n")
