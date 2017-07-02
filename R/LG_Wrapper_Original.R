################################################################################
#####  2016-02-11

#' Wrapper-function for local Gaussian inspection of a time series
#'
#' @details This function will take care of the pesky details when it
#'     comes to calling the functions that based on a time series will
#'     analyse it by means of local Gaussian techniques, i.e. by
#'     computing a bunch of local Gaussian approximations for the lags
#'     and then use these to create local Gaussian spectra.
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


#####  NOTE: I thought I could leave out the specification of the
#####  values for the arguments of this wrapper, and then use update
#####  formals after it to get the desired defaults inherited from
#####  'LG_approx' and  'LG_spectra', i.e. using
## update_formals(.fun = LG_Wrapper_Original,
##                formals(LG_approx),
##                .list = TRUE)
## update_formals(.fun = LG_Wrapper_Original,
##                formals(LG_spectra),
##                .list = TRUE)
#####  That however, did not work the package was created.  I naively
#####  anticipated that it should due to lexicographical ordering of
#####  the source-files, but obviously there's something amiss.

###  UPDATE, 2015-12-18: Regarding the detail above, the problem is
###  related to the order the files are treated when the package is
###  created.  That is to say, this should thus be possible to resolve
###  in a manner like the one I initially had in mind.



LG_Wrapper_Original <- function(
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
    cut_vec = NULL) {
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments.  (No check against
    ##  previous computations in the wrapper.)
    books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
    ##  Create a call for 'LG_approx_scribe' based on 'spy_report'.
    LG_approx_scribe_call <- create_call(
        .cc_fun = LG_approx_scribe,
        spy_report$envir,
        .cc_list = TRUE)
###-------------------------------------------------------------------
    ##  Evaluate 'LG_approx_scribe_call'
    approx_note <- eval(LG_approx_scribe_call)
###-------------------------------------------------------------------
    ##  Update the 'data_dir' value in 'spy_report$envir'.
    spy_report$envir$data_dir <-
        approx_note$data_dir
###-------------------------------------------------------------------
    ##  Create a call for 'LG_spectra_scribe' based on 'spy_report'.
    LG_spectra_scribe_call <- create_call(
        .cc_fun = LG_spectra_scribe,
        spy_report$envir,
        .cc_list = TRUE)
###-------------------------------------------------------------------
    ##  Evaluate 'LG_spectra_scribe_call'
    spectra_note <- eval(LG_spectra_scribe_call)
###-------------------------------------------------------------------
    ##  Return 'approx_note' and 'spectra_note' to the work-flow.
    return(list(
        approx_note = approx_note,
        spectra_note = spectra_note))
}
