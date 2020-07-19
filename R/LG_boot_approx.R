#' Approximations of the local Gaussian auto- and cross-correlations for bootstrapped replicates
#'
#' @description This function is in essence a wrapper for the function
#'     \code{LG_approx}, and it is called when we need to investigate
#'     bootstrap-based replicates in our analysis of the local
#'     Gaussian spectral densities.
#'
#' @details This function can be called manually, but the intention is
#'     that it only should be called from
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
#' @return This function is a wrapper around \code{LG_approx}, and the
#'     result is thus the same as specified for that function.
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
    ##  Create a spy-report.
    spy_report <- spy()
     ##  Add 'TS' to the content to simplify the creation of the call.
    spy_report$envir$TS <- spy_report$envir$TS_boot
    ##  Create and evaluate a call to 'LG_approx', and return the
    ##  result to the workflow.
    LG_approx_call <- create_call(
        .cc_fun = LG_approx,
        spy_report$envir,
        .cc_list = TRUE)
    eval(LG_approx_call)
}
