#' Wrapper-function for the bootstrap part of local Gaussian
#' inspection of a time series.
#' 
#' @template main_dir_arg
#' @template spectra_dir_boot
#'
##### Arguments that deals with the type of bootstrap-statistics.
#' @template all_statistics_arg
#' @template log__arg
#'
#####  Arguments that goes to 'TS_boot_sample'.
#' @template nb_boot
#' @template boot_type_boot
#' @template block_length_boot
#' @template boot_seed_boot
#'
##### Arguments that goes to 'LG_boot_approx_scribe'.
#' @param lag_max The maximal number of lags we want to include in our
#'     analysis.  This number will also affect the
#'     'adjustment'-attribute on the bootstrapped time series.
#'
#' @template LG_points_boot
#' 
#' @param .bws_mixture One (or more) of the values in
#'     \code{c("mixture", "local", "global")}.  The alternatives
#'     \code{local} and \code{global} will respectively only select
#'     the nearest neighbour or global, whereas \code{mixture} selects
#'     the largest of the two alternatives. 
#'
#' @param bw_points The percentage(s) of the observations that we want
#'     inside the "bandwidth-square".  If \code{.bws_mixture} is
#'     selected to be \code{global}, then this argument will be
#'     ignored. and no nearest neighbours will be computed.
#' 
#' @param .bws_fixed A vector of non-negative real values, that can be
#'     used to specify fixed values for the bandwidths (which might be
#'     of interest to do in a preliminary analysis).
#'
#' @param .bws_fixed_only A logic value that can be used to drop the
#'     rather time-consuming data-driven estimation of bandwidths in
#'     favour of the simplified approach where fixed bandwidths are
#'     used instead.  Note that \code{.bws_fixed} must be specified
#'     when \code{.bws_fixed_only} are set to \code{TRUE}.
#'
#' @template content_details_boot
#' 
#' @template LG_type_boot
#' 
#####  Arguments that goes to 'LG_boot_spectra_scribe'.
#' @template omega_vec_boot
#' @template window_boot
#' @template cut_vec_boot
#'
#' @template threshold_arg  
#'
#' @return This function will compute local Gaussian approximations
#'     for all the bootstrap-replicates of the original time series
#'     \code{TS}, and then it will compute the local Gaussian spectra
#'     and extract information about them in accordance with
#'     \code{all_statistics}.  The results of these computations will
#'     be saved in the file-hierarchy and the info-file will be
#'     updated with details about the arguments.  The latter part is
#'     done in order to keep track of previous computations, such that
#'     we avoid redoing stuff that has been performed before.  Details
#'     about the saved files are returned to the work-flow, in a list
#'     with the two components \code{approx_note} and
#'     \code{spectra_note}.
#' 
#' @export
       


LG_Wrapper_Bootstrap <- function(
    main_dir,
    spectra_dir,
    nb = NULL,
    boot_type = NULL,
    block_length = NULL,
    boot_seed = NULL,
    all_statistics = FALSE,
    log_ = FALSE,
    lag_max = NULL,
    LG_points = NULL,
    .bws_mixture = NULL,
    bw_points = NULL,
    .bws_fixed = NULL,
    .bws_fixed_only = NULL,
    content_details = NULL,
    LG_type = NULL,
    omega_vec = NULL,
    window = NULL,
    cut_vec = NULL,
    threshold = 500) {
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments.  (No check against
    ##  previous computations in the wrapper.)
    books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
    ##  Create call for 'LG_boot_approx_scribe' based on 'spy_report'.
    LG_boot_approx_scribe_call <- create_call(
        .cc_fun = LG_boot_approx_scribe,
        spy_report$envir,
        .cc_list = TRUE)
###-------------------------------------------------------------------
    ##  Evaluate 'LG_boot_approx_call'
    boot_approx_note <- eval(LG_boot_approx_scribe_call)
#####  TASK: Include test that tells where the files are if they
#####  already has been computed, or should that be a part of the
#####  'scribe'.  Can be fixed later on.
###-------------------------------------------------------------------
    ##  Update the 'data_dir' value in 'spy_report$envir'.
    spy_report$envir$data_dir <-
        boot_approx_note$data_dir
###------------------------------------------------------------------- 
   ##  Create call for 'LG_spectra_scribe' based on 'spy_report'.
    LG_boot_spectra_scribe_call <- create_call(
        .cc_fun = LG_boot_spectra_scribe,
        spy_report$envir,
        .cc_list = TRUE)
    ## capture_env() 
###-------------------------------------------------------------------
    ##  Evaluate 'LG_boot_spectra_call'
    boot_spectra_note <- eval(LG_boot_spectra_scribe_call)
#####  TASK: Include test that says where file exists if they already
#####  has been computed, or should that be a part of the 'scribe'.
###-------------------------------------------------------------------
    ##  Return 'approx_note' and 'spectra_note' to the work-flow.
    return(list(
        boot_approx_note = boot_approx_note,
        boot_spectra_note = boot_spectra_note))
}
    
