#' Computation of the spectra based on correlations.
#'
#' This function computes the spectra for a collection of (local
#' Gaussian) auto- and cross-correlations.  This function is an
#' internal function which is called from the functions that creates
#' the plots.
#'
#' @param look_up A list created by \code{LG_lookup} (in the function
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

LG_plot_df_spectra  <- function(look_up,
                              ..env) {
    ##  Some shortcuts to get the code more compact.
    cache <- look_up$cache
    restrict <- look_up$restrict
    global_name <- look_up$global_name
    local_name <- look_up$local_name
    ###-------------------------------------------------------------------    
    ##  The investigation of the spectra requires that an array
    ##  with the 'exp^{-2*pi*i*omega*h}'-values must be multiplied
    ##  with the available correlations (depends on bandwidth).
    ##  The 'exp'-array is computed first, and then in the next
    ##  part they are multiplied with the correlations.
    if (!exists(x = cache$exp, envir = ..env)) {
        ..env[[cache$exp]] <- structure(
            .Data = exp(outer(X = look_up$lag_vec,
                              Y = -2i*pi*look_up$omega_vec)),
            .Dimnames = list(lag = look_up$lag_vec,
                             omega = look_up$omega_vec),
            class = LG_default$class$array)
    }
    ###-------------------------------------------------------------------
    ##  Find the desired 'weights' to be used for the specified
    ##  windows functions.  Reminder: These depend on the "cut"-value,
    ##  which specifies the first term having a weight of zero.  This
    ##  is a bit mismatched with the way the 'm-truncation' refers to
    ##  the last term not having a weight of zero.  It is thus
    ##  necessary to adjust the cut-value a bit in the setup below.
    ##  Note: A "cut"-value of "1" implies that we only have the
    ##  lag-zero contribution left.  This is not really interesting,
    ##  but the corner case must still be treated later on in order to
    ##  avoid glitches during the interactive investigation.
    if (!exists(x = cache$weights_f, envir = ..env)) {
        ..env[[cache$weights_f]] <- list()
        for (.cut in look_up$lag_vec)
            ..env[[cache$weights_f]][[as.character(.cut + 1)]] <-
                myWindows[[look_up$window]](.cut = .cut + 1) 
    }
    ##  Add the weight-version needed when we want to investigate the
    ##  cumulative spectral density function.
    if (all(look_up$spectra_f_or_F == "F",
            !exists(x = cache$weights_F, envir = ..env))) {
        ..env[[cache$weights_F]] <- list()
        for (.cut in names(..env[[cache$weights_f]])) {
            ..env[[cache$weights_F]][[.cut]] <-
                1i/(2*pi) * ..env[[cache$weights_f]][[.cut]] /
                seq_along(..env[[cache$weights_f]][[.cut]])
        }
    }
    
    ##  Need to decide the task related to extraction of weighted
    ##  correlations, needded for the L2-norm computations.

    ####  Below: Did create crash in 'LG_create_plot_df'
    ## ## ###-------------------------------------------------------------------
    ## ## ##  End here if a distance-based is the target.
    ## ## if (any(look_up$L2_distance_normal,
    ## ##         look_up$L2_distance_percentages))
    ## ##     return(invisible(NULL))

    ###-------------------------------------------------------------------
    ##  Compute the unweighted product of the 'exp'-array and the
    ##  estimated ordinary (global) correlations.
    if (!exists(x = cache$spectra_summands_global, envir = ..env)) {
        .result <- list()
        for (.node in dimnames(..env[[global_name]])$TS) {
            .result[[.node]] <- list()
            .corr <- restrict_array(
                .arr = ..env[[global_name]],
                .restrict = restrict$S$spectra_summands_global(.node))
            .dn <- dimnames(.corr)
            ##  Extract the lag-zero component when required.
            ##  Reminder: In order for the addition of the arrays
            ##  to work properly later on, the "lag"-dimension
            ##  must be dropped.
            if ("0" %in% .dn$lag)  {
                .result[[.node]][["lag_zero"]] <-
                    restrict_array(
                        .arr = .corr,
                        .restrict = list(lag = "0"))
                .dn <- dimnames(.result[[.node]][["lag_zero"]])
                .lag_pos <- which(names(.dn) == "lag")
                dim(.result[[.node]][["lag_zero"]]) <-
                    dim(.result[[.node]][["lag_zero"]])[-.lag_pos]
                dimnames(.result[[.node]][["lag_zero"]]) <- .dn[-.lag_pos]
                kill(.lag_pos)
            }
            ##  Compute the desired product of the positive
            ##  lag-correlations and the 'exp'-values.
            .result[[.node]][["pos"]] <-
                multiply_arrays(
                    .arr1 = append_dimensions(
                        orig_arr = restrict_array(
                            .arr = .corr,
                            .restrict =
                                list(lag = as.character(look_up$lag_vec))),
                        added_dimnames =
                            list(omega = as.character(look_up$omega_vec))),
                    .arr2 = ..env[[cache$exp]])
        }
        ..env[[cache$spectra_summands_global]]  <- .result
        kill(.result, .node, .dn, .corr)
    }
    kill(global_name)
    ###-------------------------------------------------------------------
    ##  Compute the global spectra.
    if (!exists(x = cache$spectra_global, envir = ..env)) { 
        LG_plot_df_spectra_helper(..env, look_up, .gl = "global")
    }
    ###-------------------------------------------------------------------
    ##  Compute the product of the 'exp'-array and the estimated
    ##  local Gaussian correlations for the given bandwidth.
    if (!exists(x = cache$spectra_summands_local, envir = ..env)) {
        .result <- list()
        for (.node in names(..env[[local_name]])) {
            if (is.null(..env[[local_name]][[.node]]))
                next
            .result[[.node]] <- list()
            .corr <- restrict_array(
                .arr = ..env[[local_name]][[.node]],
                .restrict = restrict$S$spectra_summands_local)
            ##  Extract the lag-zero component when required.
            ##  Reminder: In order for the addition of the arrays
            ##  to work properly later on, the "lag"-dimension
            ##  must be dropped.
            if (look_up$is_lag_zero_included)  {
                .result[[.node]][["lag_zero"]] <-
                    restrict_array(
                        .arr = .corr,
                        .restrict = list(lag = "0"))
                .dn <- dimnames(.result[[.node]][["lag_zero"]])
                .lag_pos <- which(names(.dn) == "lag")
                dim(.result[[.node]][["lag_zero"]]) <-
                    dim(.result[[.node]][["lag_zero"]])[-.lag_pos]
                dimnames(.result[[.node]][["lag_zero"]]) <- .dn[-.lag_pos]
                kill(.lag_pos)
            }
            ##  Compute the desired product of the positive
            ##  lag-correlations and the 'exp'-values.
            .result[[.node]][["pos"]] <- 
                multiply_arrays(
                    .arr1 = append_dimensions(
                        orig_arr = restrict_array(
                            .arr = .corr,
                            .restrict =
                                list(lag = as.character(look_up$lag_vec))),
                        added_dimnames =
                            list(omega = as.character(look_up$omega_vec))),
                    .arr2 = ..env[[cache$exp]])
        }
        ..env[[cache$spectra_summands_local]]  <- .result
        kill(.result, .node, .dn, .corr)
    }
    kill(local_name)
    ###-------------------------------------------------------------------
    ##  Compute the collections of estimated spectra (for different
    ##  lags) given the selected lag-window function.
    if (!exists(x = cache$spectra_local, envir = ..env)) {
        LG_plot_df_spectra_helper(..env, look_up, .gl = "local")
    }
    ###-------------------------------------------------------------------
    ##  End here if it is the heatmap-case that is to be investigated.
    if (look_up$heatmap)
        return(invisible(NULL))
    ###-------------------------------------------------------------------
    ##  Extract data from 'cache$spectra_global' and
    ##  'cache$spectra_local'.  The idea is to use subsetting to
    ##  get hold of a two dimensional array with the "omega" and
    ##  "content" dimensions.  A minor help-function is needed in
    ##  order to deal with the actual computations.
    .CI_helper <- function(.arr) {
        ##  Check if any confidence intervals should be computed,
        ##  if not simply return '.arr' back to the workflow.
        if  (!look_up$is_CI_needed)
            return(.arr)
        ##  Still running? If so, keep in mind the difference
        ##  between the bootstrap case and the simulated blocks.
        ##  In the former, the content-component "orig" must be
        ##  treated separately.
        if (look_up$is_bootstrap) {
            .arr_original <- restrict_array(
                .arr = .arr,
                .restrict = list(content = "orig"))
            .arr <- restrict_array(
                .arr = .arr,
                .restrict = list(content = setdiff(
                                     x = dimnames(.arr)$content,
                                     y = "orig")))
        } else 
            .arr_original <- NULL
        ##  Return an array with the desired confidence
        ##  intervals, the min-max values and the median (and
        ##  "orig" for the bootstrap case).  The new values are
        ##  computed by the help of 'LG_boot_statistics' (which I
        ##  guess better be renamed in the revised approach).
        ##  Reminder: More statistics could be computed from this,
        ##  but the present interface does not deal with that yet
        ##  (and most likely never will).
        newnames <- LG_boot_statistics(names_only = TRUE)$content
        my_abind(
            .arr_original,
            plyr::aaply(
                      .data = .arr,
                      .margins = which(names(dimnames(.arr)) %in%  c("omega","bw_points")),
                      .fun = function(x)
                          array(data = LG_boot_statistics(x = x),
                                dim = c(length(newnames)),
                                dimnames = list(content = newnames)),
                      .drop = FALSE))
    }
    ###-------------------------------------------------------------------
    ##  The next helper extracts the relevant part of the range, with
    ##  some minor tweaks in order to get out the desired stuff.
    .ylim <- function(global_local, spectra_type) {
        ##  Identify the relevant source.
        .source <- switch(
            EXPR = global_local,
            global = cache$spectra_global,
            local  = cache$spectra_local)
        ##  Find the list with the range-values
        .ylim_node <- ..env[[.source]]$ylim
        ##  Extract into one vector the relevant 'min'- and
        ##  'max'-values from the different nodes, and then use the
        ##  range of that vector to find the overall desired limits.
        .min_max <- c()
        for (.node in names(.ylim_node))
            for (.mm in c("min", "max")) {
                if (spectra_type %in% dimnames(.ylim_node[[.node]])$S_type)
                    .min_max <- c(.min_max,
                                  restrict_array(
                                      .arr = .ylim_node[[.node]],
                                      .restrict = list(S_type = spectra_type,
                                                       range = .mm)))
            }
        ##  The range is not always equal to the desired 'ylim'-value
        ##  and some tweaks are thus included to ensure that we get
        ##  the desired output.
        switch(
            EXPR = spectra_type,
            Co = range(0, .min_max),
            Quad = range(0, .min_max),
            amplitude = range(0, .min_max),
            phase = c(-pi, pi))
    }
    ##  The content of the resulting lists (based on the setup below)
    ##  will in some cases require a change of sign.  The next helper
    ##  takes care of that.
    .change_sign <- function(.cache_code) {
        if (all(look_up$is_adjust_sign,
                look_up$spectra_type %in% c("Quad", "phase"))) {
            ..env[[.cache_code]]$.data <- 
                -1 * ..env[[.cache_code]]$.data
            ..env[[.cache_code]]$.ylim <-
                -1 * rev(..env[[.cache_code]]$.ylim)
        }   
    }
    ###-------------------------------------------------------------------
    ##  Read the relevant data for the two cases and perform the
    ##  computations.  Reminder: The idea is to construct a bookmark
    ##  '.bm' that extracts the desired node from the main list, and
    ##  then use 'restrict_array' to extract the desired part for the
    ##  present investigation.  From this stage on it should no longer
    ##  be necessary to keep track of the length one dimensions, and
    ##  these will henceforth be dropped in order to extract a two
    ##  dimensional array with the dimensions "omega" and "content".
    if (!exists(x = cache$CI_global, where = ..env)) {
        .bm <- restrict$S$CI_global$bm
        ##  When the target of interest is to consider a local
        ##  Gaussian auto-spectrum for a point off the diagonal, then
        ##  it is necessary to tweak the stored data a bit here.
        .arr <- ..env[[cache$spectra_global]][[.bm]]
        if (all(look_up$is_auto_pair,
                look_up$spectra_type != "Co")) {
            if (look_up$spectra_type == "amplitude")
                .arr <- Mod(.arr) 
            if (look_up$spectra_type == "Quad")
                .arr[] <- 0
            if (look_up$spectra_type == "phase")
                .arr <-  Arg(.arr)
            dimnames(.arr)$S_type <- look_up$spectra_type
            ##  Adjustments for the 'ylim'-case.
            ..ylim <-
                if (look_up$spectra_type == "amplitude") {
                    range(0, abs(.ylim(global_local = "global",
                                       spectra_type = "Co")))
                } else {
                    0
                }
        } else {
            ..ylim <- .ylim(global_local = "global",
                            spectra_type = look_up$spectra_type)
        }
        ##  Find the desired array.
        .tmp <- restrict_array(
            .arr = .arr,
            .restrict = restrict$S$CI_global$rl,
            .drop = TRUE,
            .never_drop = restrict$S$CI_global$never_drop)
        ..env[[cache$CI_global]] <- list(
            .data = .CI_helper(.arr = .tmp),
            .ylim = ..ylim)
        kill(.bm, .tmp, ..ylim)
        ##  Adjust the sign when required.
        .change_sign(cache$CI_global)
    }
    if (!exists(x = cache$CI_local, where = ..env)) {
        .bm <- restrict$S$CI_local$bm
        ##  An adjustment is needed for the univariate case when an
        ##  on-diagonal point is investigated by the framework where
        ##  the horizontal and vertical components can be adjusted
        ##  separately.  The reason for this adjustment is that the
        ##  computations earlier on then only have computed the
        ##  "Co"-part, since the result will be real-valued.
        .arr <- ..env[[cache$spectra_local]][[.bm]]
        if (all(look_up$is_auto_pair,
                length(dimnames(.arr)$S_type) == 1,
                look_up$spectra_type != "Co")) {
            if (look_up$spectra_type == "amplitude")
                .arr <- Mod(.arr) 
            if (look_up$spectra_type == "Quad")
                .arr[] <- 0
            if (look_up$spectra_type == "phase")
                .arr <-  Arg(.arr)
            dimnames(.arr)$S_type <- look_up$spectra_type
            ##  The adjustments of 'ylim' for the three cases
            ##  encountered here.
            ..ylim <- switch(
                EXPR = look_up$spectra_type,
                amplitude = range(0, abs(.ylim(global_local = "local",
                                               spectra_type = "Co"))),
                phase = c(-pi, pi),
                Quad = .ylim(global_local = "local",
                             spectra_type = "Quad"))
        } else {
            ..ylim <- .ylim(global_local = "local",
                            spectra_type = look_up$spectra_type)
        }
        .tmp <- restrict_array(
            .arr = .arr,
            .restrict = restrict$S$CI_local$rl,
            .drop = TRUE,
            .never_drop = restrict$S$CI_local$never_drop)
        ..env[[cache$CI_local]] <- list(
            .data = .CI_helper(.arr = .tmp),
            .ylim = ..ylim)
        kill(.bm, .tmp)
        ##  Adjust the sign when required.
        .change_sign(cache$CI_local)
    }
    ##  Nothing to return from this function, since the environment
    ##  '..env' now contains all the desired content.
    invisible(NULL)
}
