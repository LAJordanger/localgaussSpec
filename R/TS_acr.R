################################################################################
#'
#' Autocovariances and autocorrelations the old-fashioned way.
#'
#' This function use the "old-fashioned" way to compute the
#' autocorrelations for a given time series, i.e. the
#' summation-approach.  This is much slower than the fast Fourier
#' transform approach used by \code{acf}, but it seems more natural to
#' use this when computing the ordinary spectral density in the old
#' fashioned way (using window-functions instead of periodograms).
#'
#' @param .TS_info A list containing the three components \code{TS},
#'     \code{main_dir} and \code{save_dir}.
#'
#' @template lag_max_arg
#' 
#' @return A file will be created containing an array with the
#'     (ordinary) autocorrelations of \code{TS}.  If \code{save_dir}
#'     is \code{NULL}, then the array will be returned to the workflow
#'     directly, otherwise it will be saved to disk.
#' 
#' @keywords internal


#####  2017-01-05, Reminder: The code for this setup might most likely
#####  be simplified a bit by applying the "folding property" of the
#####  cross-correlations, i.e. an approach like the one used for the
#####  local Gaussian correlations might lead to a leaner setup.  Does
#####  not have time to mess around with this modification at the
#####  moment.

TS_acr <- function(.TS_info,
                   lag_max = quote(ceiling(3*sqrt(length(TS))))) {
###-------------------------------------------------------------------
    ##  Load `TS` and `save_dir` from the information in `.TS_info`.
    TS_load(.TS_info,
            save_dir = TRUE)
    ##  Identify if `TS` is related to bootstrapping.
    .bootstrap <-
        if (is.null(attributes(TS)$bootstrap)) {
            FALSE
        } else
            attributes(TS)$bootstrap
    ##  Identify the type based on `.bootstrap` (needed in order to
    ##  identify the correct path into the saved file-hierarchy).
    .acr_type <- paste("acr",
                       ifelse(
                           test = .bootstrap,
                               yes  = "_boot",
                               no   = ""),
                       sep = "")
    kill(.bootstrap)
###-------------------------------------------------------------------
    ##  If `lag_max` is the default call, then the `length(TS)`-part
    ##  should be replaced with the length of the dimension
    ##  `observations` (from `TS`), before it is evaluated.
    if (is.call(lag_max)) {
        lag_max[[c(2, 3, 2)]] <-
            length(dimnames(TS)$observations)
        lag_max <- eval(lag_max)
    }
###-------------------------------------------------------------------
    ##  Extract relevant attributes from 'TS'.
    .attr_from_TS <- local({
        .ignore <- c("dim", "dimnames", "TS_for_analysis")
        .keep <- ! names(attributes(TS)) %in% .ignore
        attributes(TS)[.keep]
    })
    TS <-
        if (! is.null(attributes(TS)$TS_for_analysis)) {
            structure(
                .Data = abind(TS,
                              attributes(TS)$TS_for_analysis,
                              along = length(dim(TS)) + 1),
                .Dimnames = c(dimnames(TS),
                              list(TS = c("TS_original", "TS_for_analysis"))),
                class = class(TS))
        } else
            structure(
                .Data = TS,
                .Dim = c(dim(TS), 1),
                .Dimnames = c(dimnames(TS),
                              list(TS = "TS_original")))
###-------------------------------------------------------------------
    ##  Help function to compute the auto- and cross-correlations on
    ##  each component.
    .acr_helper <- function(vec,
                            TS = TS,
                            lag_max = lag_max) {
        ##  Extract the desired part from 'TS'.
        .ts <- restrict_array(
            .arr = TS,
            .restrict = list(content = vec$content, TS = vec$TS),
            .drop = TRUE,
            .never_drop = c("observations", "variables"))
        ##  Split 'vec$pairs' for subsetting
        .pairs <- strsplit(x = vec$pairs, split = "_")[[1]]
        ##  Extract the components of interest.
        .first <- restrict_array(
            .arr = .ts,
            .restrict = list(variables = .pairs[1]),
            .drop = TRUE)
        .second <- restrict_array(
            .arr = .ts,
            .restrict = list(variables = .pairs[2]),
            .drop = TRUE)
        ##  Perform the mean-adjustment,
        .first <- .first - mean(.first)
        .second <- .second - mean(.second)
        ##  Find the "unscaled" denominator (the scaling factor occurs
        ##  both in numerator and denominator, and can be dropped.)
        .denom <- sqrt(sum(.first^2) * sum(.second^2))
        ##  Compute and return the result.
        structure(
            .Data = vapply(X = 0:lag_max,
                           FUN = function(lag) {
                               if (lag == 0) {
                                   .first %*% .second / .denom
                               } else
                                   tail(.first, -lag) %*% head(.second, -lag) / .denom
                           },
                           FUN.VALUE = numeric(1)),
            .Names = 0:lag_max)
    }
###-------------------------------------------------------------------
    ##  Compute the desired autocorrelations.
    arg_grid <- expand.grid(
        pairs = .attr_from_TS$.variable_pairs,
        TS = dimnames(TS)$TS,
        content = dimnames(TS)$content,
        stringsAsFactors = FALSE)
    .acr <- aaply(
        .data = arg_grid,
        .margins = 1,
        .fun = .acr_helper,
        TS = TS,
        lag_max = lag_max,
        .drop = FALSE)
    kill(arg_grid, .acr_helper, lag_max, TS)
    ## Add a name for the new dimension, and adjust the attributes.
    names(dimnames(.acr))[length(dimnames(.acr))] <- "lag"
    attributes(.acr) <- c(
        attributes(.acr),
        .attr_from_TS)
###-------------------------------------------------------------------
    ##  Save the result to file.
    LG_save(data = .acr,
            save_file.Rda = LG_default$global[.acr_type],
            save_dir = save_dir)
    ##  Return information to be added to the `info`-object.
    list(.acr_type = .acr_type,
         .acr_content = c(.TS_info$save_dir, LG_default$global[.acr_type]))
}



## ## ## TS_acr <- function(.TS_info,
## ## ##                    lag_max = quote(ceiling(3*sqrt(length(TS))))) {
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  Load `TS` and `save_dir` from the information in `.TS_info`.
## ## ##     TS_load(.TS_info,
## ## ##             save_dir = TRUE)
## ## ##     ##  Identify if `TS` is related to bootstrapping.
## ## ##     .bootstrap <-
## ## ##         if (is.null(attributes(TS)$bootstrap)) {
## ## ##             FALSE
## ## ##         } else
## ## ##             attributes(TS)$bootstrap
## ## ##     ##  Identify the type based on `.bootstrap` (needed in order to
## ## ##     ##  identify the correct path into the saved file-hierarchy).
## ## ##     .acr_type <- paste("acr",
## ## ##                        ifelse(
## ## ##                            test = .bootstrap,
## ## ##                                yes  = "_boot",
## ## ##                                no   = ""),
## ## ##                        sep = "")
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  If `lag_max` is the default call, then the `length(TS)`-part
## ## ##     ##  should be replaced with the length of the dimension
## ## ##     ##  `observations` (from `TS`), before it is evaluated.
## ## ##     if (is.call(lag_max)) {
## ## ##         lag_max[[c(2, 3, 2)]] <-
## ## ##             length(dimnames(TS)$observations)
## ## ##         lag_max <- eval(lag_max)
## ## ##     }
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  Extend TS if it contains an attribute `TS_for_analysis`, or
## ## ##     ##  just add one dimension.  NB: Record the attribute
## ## ##     ##  '.multivariate_TS' first, so it can be added to the final
## ## ##     ##  result, and also record the relevant attributes that will be
## ## ##     ##  needed later on when computing the spectral densities.
## ## ##     .multivariate_TS <- attributes(TS)$.multivariate_TS
## ## ##     .attr_from_TS <- local({
## ## ##         .ignore <- c("dim", "dimnames", "class", "TS_for_analysis")
## ## ##         .keep <- ! names(attributes(TS)) %in% .ignore
## ## ##         attributes(TS)[.keep]
## ## ##     })
## ## ##     TS <-
## ## ##         if (! is.null(attributes(TS)$TS_for_analysis)) {
## ## ##             structure(
## ## ##                 .Data = abind(TS,
## ## ##                               attributes(TS)$TS_for_analysis,
## ## ##                               along = length(dim(TS)) + 1),
## ## ##                 .Dimnames = c(dimnames(TS),
## ## ##                               list(TS = c("TS_original", "TS_for_analysis"))),
## ## ##                 class = class(TS))
## ## ##         } else
## ## ##             structure(
## ## ##                 .Data = TS,
## ## ##                 .Dim = c(dim(TS), 1),
## ## ##                 .Dimnames = c(dimnames(TS),
## ## ##                               list(TS = "TS_original")))
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  Help function to compute the autocorrelations on each
## ## ##     ##  component. Reminder: We don't need to scale with the number of
## ## ##     ##  observations since that number cancels when divided by the
## ## ##     ##  lag-zero estimate.
## ## ##     .acr_helper <- function(.TS,
## ## ##                             lag_max = lag_max) {
## ## ##         .TS <- .TS - mean(.TS)
## ## ##         ##  Compute the "unscaled" lag-zero estimate.
## ## ##         .L_var <- sum(.TS^2)
## ## ##         ##  Compute and return the result.
## ## ##         structure(
## ## ##             .Data = vapply(X = 1:lag_max,
## ## ##                            FUN = function(lag) {
## ## ##                                head(.TS, -lag) %*% tail(.TS, -lag) / .L_var
## ## ##                },
## ## ##                FUN.VALUE = numeric(1)),
## ## ##             .Names = 1:lag_max)
## ## ##     }
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  Compute the desired autocorrelations.
## ## ##     .TS_dim <- which(names(dimnames(TS)) %in% c("variables", "content", "TS"))
## ## ##     .acr <- structure(
## ## ##         .Data = aaply(.data = TS,
## ## ##                       .margins = .TS_dim,
## ## ##                       .fun = .acr_helper,
## ## ##                       lag_max = lag_max,
## ## ##                       .drop = FALSE,
## ## ##                       .parallel = TRUE),
## ## ##         .Dimnames = c(dimnames(TS)[.TS_dim], list(lag = 1:lag_max)),
## ## ##         class = LG_default$class$array)
## ## ##     ##  Add additional attributes from 'TS'
## ## ##     attributes(.acr) <- c(
## ## ##         attributes(.acr),
## ## ##         .attr_from_TS)
## ## ##     ##  Adjust the names of the variables so they reflect that the
## ## ##     ##  present data are autocorrelations.
## ## ##     dimnames(.acr)$variables <- paste(
## ## ##                       dimnames(.acr)$variables,
## ## ##                       dimnames(.acr)$variables,
## ## ##                       sep = "_")
## ## ##     kill(.acr_helper, .bootstrap, .TS_dim)
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  If the data are multivariate, estimate the cross-correlations
## ## ##     ##  too (including the lag zero case).  Keep in mind that we do
## ## ##     ##  not have a symmetric situation, so combinations like "Y1_Y2"
## ## ##     ##  and "Y2_Y1" must both be computed.  Strategy: Create a helper
## ## ##     ##  function to deal with a pair of values, such that lags are
## ## ##     ##  computed from '0' to 'lag_max'.  The '0'-part will be
## ## ##     ##  extracted afterwards and stored as an attribute.
## ## ##     if (.multivariate_TS) {
## ## ##         ##  Strategy: Find the desired cross-correlations by using
## ## ##         ##  "aaply" on the bivariate slices that contains
## ## ##         ##  "observations" and "variables".
## ## ##         ##
## ## ##         ##  Identify the dimensions needed for "aaply":
## ## ##         .TS_dim <- which(! names(dimnames(TS)) %in% c("observations", "variables")) 
## ## ##         ##  Identify which combinations that must be computed.
## ## ##         .variable_pairs <- combn(
## ## ##             x = dimnames(TS)$variables,
## ## ##             m = 2)
## ## ##         ##  A helper function for the cross-correlations.  Reminder:
## ## ##         ##  The adjustment to mean zero is redundant when used upon
## ## ##         ##  pseudo-normalised observations obtained from the ecdf, and
## ## ##         ##  in that case the estimated (unscaled) variance for each
## ## ##         ##  individual pseudo-normalised variable would also be the
## ## ##         ##  same.  It is possible to adjust the code to avoid some
## ## ##         ##  computations, but that is not a prioritised topic.
## ## ##         .ccr_helper <- function(.observations,
## ## ##                                 .lag_max = lag_max,
## ## ##                                 .variable_pairs = .variable_pairs) {
## ## ##             ##  Create the mean zero adjusted observations, using the
## ## ##             ##  transpose to get the proper recycling of vectors.
## ## ##             ## ## ## .observations <- t(t(.observations) - colMeans(.observations))
## ## ##             .observations <- t(.observations) - colMeans(.observations)
## ## ##             ##  Find the sum of squares (scaled estimated variances)
## ## ##             n_times_variances <- rowSums(.observations^2)
## ## ##             ##  Compute the desired range of cross-correlations for
## ## ##             ##  each variable-pair, using an internal helper function
## ## ##             ##  to take care of the messy components.
## ## ##             .bivariate_helper <- function(.pair,
## ## ##                                           .lag_max = .lag_max) {
## ## ##                 ##  Extract the infomation of interest.
## ## ##                 .V1 <- .observations[.pair[1], ]
## ## ##                 .V2 <- .observations[.pair[2], ]
## ## ##                 .n_times_denom <- sqrt(prod(n_times_variances[.pair]))
## ## ##                 ##  Return the desired cross-correlations.
## ## ##                 ##  Note: The scaling factors in numerator and
## ## ##                 ##  denominator cancels, and those has thus been
## ## ##                 ##  dropped from the computations.
## ## ##                 structure(
## ## ##                     .Data = vapply(
## ## ##                         X = (-.lag_max):.lag_max,
## ## ##                         FUN = function(lag) {
## ## ##                             if (lag < 0) {
## ## ##                                 head(.V2, -lag) %*% tail(.V1, -lag) /
## ## ##                                     .n_times_denom
## ## ##                             } else {
## ## ##                                 if (lag > 0) {
## ## ##                                 head(.V1, -lag) %*% tail(.V2, -lag) /
## ## ##                                     .n_times_denom
## ## ##                                 } else  ##  The zero-case.
## ## ##                                     .V1 %*% .V2 / .n_times_denom
## ## ##                             }
## ## ##                         },
## ## ##                         FUN.VALUE = numeric(1)),
## ## ##                     .Names = (-.lag_max):.lag_max)
## ## ##             }
## ## ##             ##  Return the result to the workflow.
## ## ##             structure(
## ## ##                 .Data = aaply(
## ## ##                     .data = .variable_pairs,
## ## ##                     .margins = 2,
## ## ##                     .fun = .bivariate_helper,
## ## ##                     .lag_max = .lag_max),
## ## ##                 .Dim = c(ncol(.variable_pairs), 2*.lag_max + 1),
## ## ##                 .Dimnames = list(
## ## ##                     variables = apply(
## ## ##                         X = .variable_pairs,
## ## ##                         MARGIN = 2,
## ## ##                         FUN = paste,
## ## ##                         collapse = "_"),
## ## ##                     lag = (-.lag_max):.lag_max)
## ## ##             )
## ## ##         }
## ## ##         ##  Compute the cross-correlations of interest.
## ## ##         .ccr <- structure(
## ## ##             .Data = aaply(.data = TS,
## ## ##                           .margins = .TS_dim,
## ## ##                           ##  Reminder: Code tested on simple case to verify
## ## ##                           ##  sanity when used with the identy function.
## ## ##                           .fun = .ccr_helper,
## ## ##                           .lag_max = lag_max,
## ## ##                           .variable_pairs = .variable_pairs,
## ## ##                           .drop = FALSE,
## ## ##                           .parallel = FALSE),
## ## ##             class = LG_default$class$array)
## ## ##         kill(.ccr_helper)
## ## ##         ##  "Fold" the '.ccr' to enable a joining with the univariate
## ## ##         ##  '.acr' into one single array.  The lag zero cases will be
## ## ##         ##  extracted and stored as the attribute '.ccr_zero'.  Start
## ## ##         ##  out by reordering the dimensions of '.ccr' to match those
## ## ##         ##  of 'acr', then extract the pieces of interest and bind
## ## ##         ##  them toghether along the "variables" dimension.
## ## ##         .ccr_zero <- restrict_array(
## ## ##             .arr = .ccr,
## ## ##             .restrict = list(lag = "0"))
## ## ##         .ccr_pos  <- restrict_array(
## ## ##             .arr = .ccr,
## ## ##             .restrict = list(lag = as.character(1:lag_max)))
## ## ##         .ccr_neg <- local({
## ## ##             .neg_lag <- paste("-", 1:lag_max, sep="")
## ## ##             ..ccr_neg <- restrict_array(
## ## ##                 .arr = .ccr,
## ## ##                 .restrict = list(lag = .neg_lag))
## ## ##             ##  Adjust the dimension-names (using "folding").
## ## ##             dimnames(..ccr_neg)$variables <- apply(
## ## ##                                    X = .variable_pairs,
## ## ##                                    MARGIN = 2,
## ## ##                                    FUN = function(x) 
## ## ##                                        paste(rev(x),
## ## ##                                              collapse = "_"))
## ## ##             dimnames(..ccr_neg)$lag <- 1:lag_max
## ## ##             ##  Return the result
## ## ##             ..ccr_neg
## ## ##         })
## ## ##         kill(.ccr)
## ## ##         ##  Create an extended version of '.acr' with updated
## ## ##         ##  attributes, note that `my_abind` is a wrapper for
## ## ##         ##  `abind::abind` (and thus slower) which removes the need
## ## ##         ##  for fiddling around with the dimension-names here
## ## ##         .attributes_for_new_acr <- c(
## ## ##             attributes(.acr)[! names(attributes(.acr)) %in% c("dim", "dimnames")],
## ## ##             list(ccr_zero = .ccr_zero))
## ## ##         .acr <- my_abind(.acr, .ccr_pos, .ccr_neg)
## ## ##         attributes(.acr) <- c(
## ## ##             attributes(.acr),
## ## ##             .attributes_for_new_acr)
## ## ##         kill(.ccr_neg, .ccr_zero, .ccr_pos, .attributes_for_new_acr)
## ## ##     }
## ## ##     kill(TS, .TS_dim, lag_max, .multivariate_TS)
## ## ## ###-------------------------------------------------------------------
## ## ##     ##  Save the result to file.
## ## ##     LG_save(data = .acr,
## ## ##             save_file.Rda = LG_default$global[.acr_type],
## ## ##             save_dir = save_dir)
## ## ##     ##  Return information to be added to the `info`-object.
## ## ##     list(.acr_type = .acr_type,
## ## ##          .acr_content = c(.TS_info$save_dir, LG_default$global[.acr_type]))
## ## ## }
