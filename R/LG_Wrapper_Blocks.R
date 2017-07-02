################################################################################
#####  2016-01-22

#' Wrapper-function for Local Gaussian inspection of a time series by blocks.
#'
#' This function wraps around the code needed to do an analysis based
#' on a collection of independent samples from some known
#' distribution.  In particular, it will perform the computation of
#' the Local Gaussian Approximations for each series, and compute
#' Local Gaussian Spectral Densities based on these values.
#' 
#' @template main_dir_arg
#' @template data_dir_arg
#' @template TS_arg
#' @template lag_max_arg
#' @template LG_points_arg
#' @template bws_mixture_arg
#' @template bw_points_arg
#' @template bws_fixed_arg
#' @template bws_fixed_only_arg
#' @template omega_vec_arg
#' @template omega_length_out_arg
#' @template window_arg
#' @template LG_type_arg
#' @template cut_vec_arg
#'
#' @return This function will compute local Gaussian approximations
#'     and local Gaussian spectra in accordance with the given
#'     arguments.  The results of these computations will be saved in
#'     the file-hierarchy and the info-file will be updated with
#'     details about the arguments.  The latter part is done in order
#'     to keep track of previous computations, such that we avoid
#'     redoing stuff that has been performed before (in the present
#'     \code{data_dir}).  Details about the saved files are returned
#'     to the work-flow, in a list with the two components
#'     \code{approx_note} and \code{spectra_note}.
#'
#' @export


LG_Wrapper_Blocks <- function(
    main_dir,
    data_dir,
    TS,
    lag_max = ceiling(3*sqrt(length(TS))),
    LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = c(25, 35),
    .bws_fixed = NULL,
    .bws_fixed_only = FALSE,
    omega_vec = NULL,
    omega_length_out = 2^6,
    window =
        c("Tukey", "Parzen", "Bartlett"),
    LG_type = c("par_five", "par_one"),
#####  TASK:  Need arguments to give to 'LG_boot_spectra_scribe', i.e. with regard to what stuff to compute.
    cut_vec = NULL) {
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(data_dir, TS, lag_max, .bws_mixture, bw_points, .bws_fixed,
         .bws_fixed_only, omega_vec, omega_length_out, window,
         LG_type, cut_vec)
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments.  (No check against
    ##  previous computations in the wrapper.)
    LG_bookkeeping(spy_report = spy_report)
    ## books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
    ##  Create a call for 'LG_approx_scribe' based on 'spy_report'.
    LG_approx_scribe_call <- create_call(
        .cc_fun = LG_approx_scribe,
        spy_report$envir,
        .cc_list = TRUE)
###-------------------------------------------------------------------
    ##  Evaluate 'LG_approx_scribe_call', to get a block of local
    ##  Gaussian estimates.
    block_approx_note <- eval(LG_approx_scribe_call)
    kill(LG_approx_scribe_call)
###-------------------------------------------------------------------
    ##  Update the 'data_dir' value in 'spy_report$envir'.
    spy_report$envir$data_dir <-
        block_approx_note$data_dir
###-------------------------------------------------------------------
    ##  Create a call for 'LG_spectra_scribe' based on 'spy_report'.
    LG_spectra_scribe_call <- create_call(
        .cc_fun = LG_spectra_scribe,
        spy_report$envir,
        .cc_list = TRUE)
    ##  Evaluate 'LG_spectra_scribe_call', to get spectral densities
    ##  based on median/mean of the local Gaussian approximations.
    median_mean_spectra_note <- eval(LG_spectra_scribe_call)
    kill(LG_spectra_scribe_call)
###-------------------------------------------------------------------
    ##  Update/add values to 'spy_report$envir', then create and a
    ##  call to 'LG_boot_spectra'
    spy_report$envir$data_dir <- block_approx_note$data_dir
    spy_report$envir$spectra_dir <- median_mean_spectra_note$data_dir
    LG_boot_spectra_scribe_call <- create_call(
        .cc_fun = LG_boot_spectra_scribe,
        spy_report$envir,
        .cc_list = TRUE)
    ##  Evaluate 'LG_boot_spectra_scribe_call' to get confidence
    ##  intervals (and other stuff for the spectral densities computed
    ##  from the local Gaussian approximations).
    CI_spectra_note <- eval(LG_boot_spectra_scribe_call)
    kill(LG_boot_spectra_scribe_call, spy_report)
###-------------------------------------------------------------------
    ##  Return 'approx_note' and 'spectra_note' to the work-flow.
    return(list(
        block_approx_note = block_approx_note,
        median_mean_spectra_note = median_mean_spectra_note,
        CI_spectra_note = CI_spectra_note))
}
