#' Local Gaussian approximations for bootstrapped replicates
#'
#' This is a wrapper for the function \code{LG_approx}, to be used
#' when we want to use bootstrap-based statistics in our analysis of
#' e.g. its local Gaussian spectra.
#'
#' @details This function can be called manually from the work-space,
#'     but the intention is that it only should be called from
#'     \code{LG_boot_approx_scribe}, since that will ensure that the
#'     arguments are properly recorded and that the result are saved
#'     to appropriately named files.  In order to dissuade users from
#'     calling this (often quite time consuming) function directly, no
#'     default values have been specified for the arguments.
#'
#' @template save_dir_arg
#' @template TS_boot_boot
#' @template lag_max_boot
#' @template LG_points_boot
#' @template bws_mixture_arg
#' @template bw_points_arg
#' @template bws_fixed_arg
#' @template bws_fixed_only_arg
#' @template content_details_arg
#' @template LG_type_arg
#'
#####  TASK: Update the stuff below, when things are in a more settled
#####  version.  It should be fairly similar to the stuff from
#####  'LG_approx', and roxygen-templates might be preferable.
#' @return data.0, a data-frame with a column \code{levels}, which for
#'     each value is matched with \code{bw_points} (the prescribed
#'     numbers of observations), \code{bw} the corresponding local
#'     bandwidths, and then for each replicate in \code{TS_boot} the
#'     estimated local Gaussian approximation parameters \code{mu} and
#'     \code{sig} obtained when \code{loclik2} has been used on the
#'     specified \code{level}.  The estimated value of the density at
#'     the point of interest, \code{f.est} are also included.  (Note:
#'     The square of \code{sig} gives the value we will need later on
#'     in the quest for the local Gaussian spectra based on local
#'     Gaussian auto-correlations.)
#' 
#' @return data.h, a data-frame with columns \code{lag} and
#'     \code{levels} that specify the points of interest, and for each
#'     combination of these we have information about \code{bw_points}
#'     (the prescribed numbers of observations), \code{bw} the
#'     corresponding local bandwidths, and then for each replicate in
#'     \code{TS_boot} we have from \code{localgauss} the coefficients
#'     \code{mu_1}, \code{mu_2}, \code{sig_1}, \code{sig_2} and
#'     \code{rho} for the local bivariate Gaussian approximation.  The
#'     column \code{par_one} gives the extracted local Gaussian
#'     auto-covariance that will be used in the computation of the
#'     local Gaussian spectra, and \code{f.est} gives the estimated
#'     density.
#'
#' @return eflag, \code{vector}, which gives the sum of the exit flags
#'     from \code{localgauss}.  If this is something else than 0, then
#'     it means that some value(s) for that bootstrap-replicate not
#'     should be trusted.  (To find the actual problematic values,
#'     pick out the relevant replicate(s) and create a
#'     \code{TS.object} based on it, then run \code{LG_approx} on that
#'     object.)
#' 
#' @export

LG_boot_approx <- function(
    save_dir = NULL,
    TS_boot,
    lag_max,
    LG_points,
    .bws_mixture,
    bw_points,
    .bws_fixed,
    .bws_fixed_only,
    content_details,
    LG_type) {
###-------------------------------------------------------------------
    ##  Create a spy-report.
    spy_report <- spy()
###-------------------------------------------------------------------
     ##  Add 'TS' to the content to simplfiy the creation of the call.
    spy_report$envir$TS <- spy_report$envir$TS_boot
###-------------------------------------------------------------------
    ##  Create and evaluate a call to 'LG_approx', and return the
    ##  result to the workflow.
#####  TASK: This doesn't work to good on large samples, so some
#####  alternative should be implemented that can avoid the problems
#####  with too large objects.
    LG_approx_call <- create_call(
        .cc_fun = LG_approx,
        spy_report$envir,
        .cc_list = TRUE)
    eval(LG_approx_call)
}
