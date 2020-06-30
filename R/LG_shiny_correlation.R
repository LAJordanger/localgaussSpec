#' Extraction of the correlations.
#'
#' This helper function extracts the (local Gaussian) auto- and
#' cross-correlations into the shapes needed in order for the
#' plot-function to present the result.
#'
#' @param look_up A list created by \code{LG_lookup} (in the function
#'     \code{LG_plot_helper}), where the key details have been
#'     extracted (or constructed) from a (non-reactive copy) of the
#'     values defined by the \code{shiny}-interface.
#'
#' @param ..env The environment that contains the correlations, and
#'     also the environment in which the result will be stored.
#'
#' @return The environment \code{..env} will be updated with values
#'     needed for the plotting of the spectra.  Internal caching is
#'     used in order to avoid repetition of already performed tasks.
#'     The creation of data-frames are taken care of in
#'     \code{LG_create_plot_df}.
#'
#' @keywords internal

LG_shiny_correlation <- function(look_up,
                                 ..env) {
    ##  Define shortcuts for the caching and restriction.
    cache <- look_up$cache
    restrict <- look_up$restrict
    ###-------------------------------------------------------------------
    ##  Create a helper to take care of the unfolding, i.e. the
    ##  reverse of the folding that was done in order to avoid
    ##  redundant computations when the correlations where computed.
    unfold <- function(.data, .gl = "global",
                       look_up = look_up) {
        ##  Extract the positive and negative lags, and join them into
        ##  one array.
        pos_lags <- restrict_array(
            .arr = .data,
            .restrict = restrict$C[[.gl]]$pos_lags,
            .drop = TRUE,
            .never_drop = restrict$C[[.gl]]$.never_drop,
            .keep_attributes = FALSE)
        ##  Extract the negative lags too, when necessary (the result
        ##  becomes 'NULL' when requirements not satisfied).
        negative_lags <- 
            if  (look_up$is_negative_lags_needed) 
                local({
                    .tmp <- restrict_array(
                        .arr = .data,
                        .restrict = restrict$C[[.gl]]$neg_lags,
                        .drop = TRUE,
                        .never_drop = restrict$C[[.gl]]$.never_drop,
                        .keep_attributes = FALSE)
                    ##  Update the sign of the lag-dimension.
                    dimnames(.tmp)$lag <- paste(
                                      "-",
                                      dimnames(.tmp)$lag,
                                      sep = "")
                    .tmp
                })
        ##  Return the result
        my_abind(
            pos_lags,
            negative_lags)
    }
    ###-------------------------------------------------------------------
    ##  Restrict the attention to the main branches.
    if (!exists(x = cache$G_branch, envir = ..env)) {
        ##  The branch for the global correlations.
        ..env[[cache$G_branch]] <- restrict_array(
            .arr = ..env[[look_up$global_name]],
            .restrict = restrict$G_branch)
    }
    if (!exists(x = cache$L_branch, envir = ..env)) {
        ##  The branch for the local Gaussian correlations.
        .bm <- unlist(restrict$L_branch$bm)
        ##  Return the resticted array.
        ..env[[cache$L_branch]] <- restrict_array(
            .arr = ..env[[.bm[1]]][[.bm[2]]],
            .restrict = restrict$L_branch$rl)
        kill(.bm)
    }
    ###-------------------------------------------------------------------
    ##  Restrict the attention to the pairs of interest (it is
    ##  possible that a multivariate time series has been analysed).
    if (!exists(x = cache$G_pairs, envir = ..env)) {
        ##  Reminder: For the global case, no further restrictions are
        ##  needed, so the unfolding is taken care of here.
        ..env[[cache$G_pairs]] <- local({
            .data  <-  restrict_array(
                .arr = ..env[[cache$G_branch]],
                .restrict = restrict$G_pairs)
            ##  Return the data with information about limits.
            list(.data = unfold(.data, look_up = look_up),
                 .xlim = look_up$xlim,
                 .ylim = range(0, .data))
        })
    }
    if (!exists(x = cache$L_pairs, envir = ..env)) {
        ##  Reminder: The dimensions are not dropped at this stage,
        ##  need the full set for the unfolding in the next step.
        ..env[[cache$L_pairs]] <- local({
            .data  <-  restrict_array(
                .arr = ..env[[cache$L_branch]],
                .restrict = restrict$L_pairs)
            ##  Return the data with information about limits.
            .tmp <- restrict_array(
                .arr = .data,
                .restrict = list(variable = "rho"))
            list(.data = .data,
                 .xlim = look_up$xlim,
                 .ylim = range(0, .tmp))
        })
    }
    ###-------------------------------------------------------------------
    ##  Restrict the attention to the point(s) of interest for the
    ##  local Gaussian correlations, and unfold.
    if (!exists(x = cache$L_levels, envir = ..env)) {
        ..env[[cache$L_levels]] <- local({
            .data <- ..env[[cache$L_pairs]]$.data
            ##  Return the data with information about limits.
            .tmp <- restrict_array(
                .arr = .data,
                .restrict = list(variable = "rho"))
            list(.data = unfold(.data,
                                look_up = look_up,
                                .gl = "local"),
                 .xlim = look_up$xlim,
                 .ylim = range(0, .tmp))
        })
    }
}
