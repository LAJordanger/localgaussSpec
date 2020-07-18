#' Helper-function to create the \code{cache}-list of \code{look_up}
#'
#' @description This internal helper function prepares the unique
#'     cache-keys that enables a primitive caching-procedure to be
#'     used in the interactive \code{LG_shiny}-application.
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @return A \code{cache}-list will be returned to the calling
#'     function (i.e. \code{LG_lookup}), and this list contains the
#'     cache-keys that are used in order to avoid that already
#'     performed tasks are recomputed.
#'
#' @keywords internal

LG_lookup_cache <- function(look_up) {
    ##  Initiate the cache-list.
    cache <- list()
    ##  The main environment that the rest of the environments will be
    ##  stored in.
    cache$env_name <-
        digest::digest(look_up[c("TS_key", "TS", "Approx", "Boot_Approx", "type")])
    ##  Caching for the initial step.
    cache$initial$global <- digest::digest(look_up$restrict$initial$global)
    cache$initial$local <- digest::digest(look_up$restrict$initial$local)
    ##  Caching related to 'LG_shiny_correlation', names matching
    ##  those from 'look_up$restrict'.  The 'C'-node used in the
    ##  internal 'unfolding'-function of 'LG_shiny_correlation' must
    ##  be included, since that is the place specifying the point the
    ##  local Gaussian investigation works upon.
    cache$G_branch <- digest::digest(look_up$restrict$G_branch)
    cache$G_pairs <- digest::digest(c(cache$G_branch,
                                      look_up$restrict$G_pairs))
    cache$L_branch <- digest::digest(look_up$restrict$L_branch)
    cache$L_pairs <- digest::digest(c(cache$L_branch,
                                      look_up$restrict$L_pairs))
    cache$L_levels <- digest::digest(c(cache$L_branch,
                                       look_up$restrict$C$local))
    ##  Create the data-frame cache for the correlations, based on the
    ##  preceding cache-values.
    cache$correlation_df <-
        if (look_up$is_global_only) {
            digest::digest(cache$G_pairs)
        } else
            digest::digest(cache$L_levels)
    ##  Caching related to the spectral densities.  First, an array
    ##  with the product of the correlations and the desired
    ##  'exp^{-2*pi*i*omega*h}'-values.  Reminder: This array is based
    ##  on 'look_up$lag_vec' and 'look_up$omega_vec', where the first
    ##  always is the same and the latter one depends on
    ##  'look_up$frequency_range'.
    cache$exp <-
        digest::digest(look_up$frequency_range)
    ##  The weights to be used for the spectral density function, this
    ##  depends on the lag-window function given in 'look_up$window'.
    cache$weights_f <-
        digest::digest(look_up$window)
    ##  The weights to be used for the cumulative spectral density
    ##  function, which only will be computed when
    ##  'look_up$spectra_f_or_F' is equal to "F".
    cache$weights_F <-
        digest::digest(cache$weights_f)
    ##  The storage of the global and local spectra-summands, i.e. the
    ##  product of the correlations and the complex valued
    ##  'e^{-2*pi*i*omega}', where it then for the local summands is
    ##  necessary to also take into account the selected bandwidth.
    cache$spectra_summands_global  <-
        digest::digest(cache$exp)
    cache$spectra_summands_local  <- 
        digest::digest(c(cache$exp,
                         look_up[c("bw_points")]))
    ##  The storage of the spectra-values, which for both local and
    ##  global spectra depends on the available summands with weights
    ##  based on the desired lag-window function (for all the possible
    ##  cut-values), restricted to the pair of variables that are to
    ##  be inspected. Reminder: The sorting here is to avoid redoing
    ##  computations that can be resolved by the help of the diagonal
    ##  reflection property of the local Gaussian correlations.  The
    ##  storage of the spectra-summands for selected bandwidth.
    cache$spectra_global  <- 
        digest::digest(c(cache$spectra_summands_global,
                         cache$weights_f,
                         sort(unlist(look_up[c("Vi", "Vj")]))))
    cache$spectra_local  <- 
        digest::digest(c(cache$spectra_summands_local,
                         cache$weights_f,
                         sort(unlist(look_up[c("Vi", "Vj")]))))
    ##  The storage of the collection of all the available pointwise
    ##  confidence intervals for the selected cut.
    cache$CI_global <-
        digest::digest(c(cache$spectra_global,
                         look_up$spectra_type,
                         look_up$cut,
                         look_up$is_adjust_sign))
    cache$CI_local <-
        digest::digest(c(cache$spectra_local,
                         look_up$S_type,
                         look_up$cut,
                         look_up$levels_point,
                         look_up$is_adjust_sign))
    ##  The storage for the data-frame containing the data for the
    ##  spectral density of interest.
    cache$spectra_df <- 
        digest::digest(c(cache$CI_global,
                         cache$CI_local,
                         look_up$confidence_interval,
                         look_up$is_global_only))
    ##  The storage of the final environment, which contains the stuff
    ##   needed for the plots to look as desired.
    cache$plot_data <-
        digest::digest(ifelse(test = look_up$TCS_type == "S",
                              yes  = cache$spectra_df,
                              no   = cache$correlation_df))
    ##  Return the 'cache'-list, so 'LG_lookup' can add it as a new
    ##  node to the 'look_up'-list
    cache
}
