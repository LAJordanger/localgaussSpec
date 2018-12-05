#' Extraction of the correlations.
#'
#' This helper function extracts the (local Gaussian) auto- and
#' cross-correlations into the shapes needed in order for the
#' plot-function to present the result.
#'
#' @param .look_up A list created by \code{LG_lookup} (in the function
#'     \code{LG_plot_helper}), where the key details have been
#'     extracted (or constructed) from a (non-reactive copy) of the
#'     values defined by the \code{shiny}-interface.
#'
#' @param ..env The environment that contains the correlations, and
#'     also the environment in which the result will be stored.
#'
#' @return An update of \code{..env} will be performed with values
#'     needed for the plotting of the spectra.
#'
#' @keywords internal

LG_shiny_correlation <- function(.look_up,
                                 ..env)  {
    ##  A minor shortcut for the caching-part.
    cache <- .look_up$cache
    ###-------------------------------------------------------------------
    ##  Create a helper to take care of the unwrapping, i.e. the
    ##  reverse of the folding that was done in order to avoid
    ##  redundant computations when the correlations where computed.
    unwrap <- function(.data, .gl = "global") {
        ##  Create the restriction lists to be used in the unfolding
        ##  part.  Note: All combinations of the pairs are included,
        ##  which simplifies the situation since it now will be
        ##  sufficient to focus upon the levels.  Reminder: Some of
        ##  the values of interest are identical both for the global
        ##  and local case, which implies that this function for those
        ##  cases can use the values stored for the global situation.
        .restrict_pos_lags <- c(
            list(pairs = .look_up$pairs_ViVj),
            if (.look_up$is_lag_zero_needed) {
                ..env[[cache$.G_branch]]$non_negative_lags
            } else
                ..env[[cache$.G_branch]]$pos_lags,
            if (.gl == "local")
                list(levels = .look_up$levels_point_reflected))
        .restrict_neg_lags <-
            if (.look_up$is_negative_lags_needed)
                c(list(pairs = .look_up$pairs_VjVi),
                  ..env[[cache$.G_branch]]$pos_lags,
            if (.gl == "local")
                list(levels = .look_up$levels_point))
        ##  Perform the restrictions, and convert the results into the
        ##  desired data-frames.  Note that the negative lags will be
        ##  ignored in the plots when symmetry ensures that they are
        ##  equal to the positive lags.
        pos_lags <- restrict_array(
            .arr = .data,
            .restrict = .restrict_pos_lags,
            .drop = TRUE,
            .never_drop = c("lag", "content",
                            if (.gl == "local")
                                "variable"),
            .keep_attributes = FALSE)
        ##  Extract the negative lags too, when necessary (the result
        ##  becomes 'NULL' when requirements not satisfied).
        negative_lags <- 
            if  (.look_up$is_negative_lags_needed) 
                local({
                    ##  Need to modify the lag-values.
                    .tmp <- restrict_array(
                        .arr = .data,
                        .restrict = .restrict_neg_lags,
                        .drop = TRUE,
                        .never_drop = c("lag", "content",
                                        if (.gl == "local")
                                            "variable"),
                        .keep_attributes = FALSE)
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
    if (!exists(x = cache$.G_branch, envir = ..env)) {
        ##  Extract the desired branch of the global correlations.
        ..env[[cache$.G_branch]] <- local({
            .global_name <- .look_up$.global_name
            .bm <- .look_up$.LGC_restrict_global$branch
            .dn <- attributes(..env[[.global_name]])$original_dimnames
            .sub_dim <- attributes(..env[[.global_name]])$.sub_dim
            .data <- structure(
                .Data = ..env[[.global_name]][[.bm]],
                .Dimnames = .dn[.sub_dim])
            ##  Return data with information about lags.
            list(
                .data = .data,
                non_negative_lags = list(
                    lag = dimnames(.data)$lag),
                pos_lags = list(
                    lag = setdiff(x = dimnames(.data)$lag,
                                  y = "0")))
        })
    }
    if (!exists(x = cache$.L_branch, envir = ..env)) {
        ##  Extract the desired branch of the local Gaussian
        ##  correlations, based on point type (on or off diagonal )and bandwidth,
        ##  i.e. whether the point lies on or off the diagonal.
        ..env[[cache$.L_branch]] <- local({
            .local_name <- .look_up$.local_name
            .bm <- .look_up$.LGC_restrict_local$branch
            .dn <- attributes(..env[[.local_name]][[.bm[1]]])$original_dimnames
            .sub_dim <- attributes(..env[[.local_name]][[.bm[1]]])$.sub_dim
            .data <- structure(
                .Data = ..env[[.local_name]][[.bm]],
                .Dimnames = .dn[.sub_dim])
            ##  Return together with information about the lags needed
            ##  for the unfolding.
            list(
                .data = .data,
                non_negative_lags = list(
                    lag = dimnames(.data)$lag),
                pos_lags = list(
                    lag = setdiff(x = dimnames(.data)$lag,
                                  y = "0")))
        })
    }
    ###-------------------------------------------------------------------
    ##  Restrict the attention to the pairs of interest.  Reminder: Do
    ##  not drop any dimensions at this stage, need the full set for
    ##  the unfolding in the next step. 
    if (!exists(x = cache$.G_pairs, envir = ..env)) {
        ##  Adjust the restriction if the lag zero component also can
        ##  be dropped (for those cases where it is constant and equal
        ##  to one.)  Reminder: For the global case, no further
        ##  restrictions are needed, so the unwrapping is thus
        ##  performed here.
        if (!.look_up$is_lag_zero_needed)
            .look_up$.LGC_restrict_global$pairs <- c(
                .look_up$.LGC_restrict_global$pairs,
                ..env[[cache$.G_branch]]$pos_lags)
        ..env[[cache$.G_pairs]] <- local({
            .data  <-  restrict_array(
                .arr = ..env[[cache$.G_branch]]$.data,
                .restrict = .look_up$.LGC_restrict_global$pairs)
            ##  Return the data with information about the limits to
            ##  be used in the plot.
            .max_lag <- max(as.numeric(dimnames(.data)$lag))
            list(.data = unwrap(.data),
                 .xlim = if (.look_up$is_negative_lags_needed) {
                             c(-1, 1) * .max_lag
                         } else
                             range(0, .max_lag),
                 .ylim = range(0, .data))
        })
    }
    if (!exists(x = cache$.L_pairs, envir = ..env)) {
        ##  Adjust the restriction if the lag zero component also can
        ##  be dropped (for those cases where it is constant and equal
        ##  to one.)
        if (!.look_up$is_lag_zero_needed)
            .look_up$.LGC_restrict_local$pairs <- c(
                .look_up$.LGC_restrict_local$pairs,
                ..env[[cache$.G_branch]]$pos_lags)
        ..env[[cache$.L_pairs]] <- local({
            .data  <-  restrict_array(
            .arr = ..env[[cache$.L_branch]]$.data,
            .restrict = .look_up$.LGC_restrict_local$pairs)
            ##  Return the data with information about the ylimit to
            ##  be used in the plot.
            .max_lag <- max(as.numeric(dimnames(.data)$lag))
            list(.data = .data,
                 .xlim = if (.look_up$is_negative_lags_needed) {
                             c(-1, 1) * .max_lag
                         } else
                             range(0, .max_lag),
                 .ylim = range(0, .data))
        })
    }
    ###-------------------------------------------------------------------
    ##  Restrict the attention to the point(s) of interest for the
    ##  local Gaussian correlations, and unfold.
    if (!exists(x = cache$.L_levels, envir = ..env)) {
        ..env[[cache$.L_levels]] <- local({
            .data <- restrict_array(
                .arr = ..env[[cache$.L_pairs]]$.data,
                .restrict = .look_up$.LGC_restrict_local$levels)
            ##  Return the data with information about the ylimit to
            ##  be used in the plot if only this particular point is
            ##  to be used as a reference for the result
            .max_lag <- max(as.numeric(dimnames(.data)$lag))
            list(.data = unwrap(.data, .gl="local"),
                 .xlim = if (.look_up$is_negative_lags_needed) {
                             c(-1, 1) * .max_lag
                         } else
                             range(0, .max_lag),
                 .ylim = range(0, .data))
        })
    }
}
