#' Create the data needed when plotting the local Gaussian
#' correlations and spectra.
#'
#' This function created a list with the data needed for the plots of
#' the estimated local Gaussian correlations and spectra.  A simple
#' caching is performed in order to avoid having computations more
#' than once.
#'
#' @param .look_up The list created by \code{LG_lookup}, containing
#'     the information needed in order to decide what kind of
#'     data-frame that is required.
#'
#' @param ..env The environment containing the required information,
#'     and also the environment where the resulting data-frame will be
#'     stored.
#'
#' @return The purpose of this function is to create a list with the
#'     data needed for the function \code{LG_plot}.  The result will
#'     be stored in the environment \code{..env}, and the unique name
#'     (computed from the input parameters used in the construction of
#'     it) will then be returned to the workflow.  This solution
#'     enables a simple form of caching, since the computations only
#'     will be performed when required.
#'
#' @keywords internal

LG_create_plot_df <- function(.look_up,
                                  ..env) {
    ##  A minor shortcut for the caching-part.
    cache <- .look_up$cache
    ##  Return the name of the plot-list if the computation already
    ##  has been performed.
    if (exists(x = cache$.plot_list, envir = ..env)) {
        return(cache$.plot_list)
    }
    ##  Create the data-frame to be used when it is the estimated
    ##  local Gaussian correlations themselves that will be
    ##  investigated.
    if ((.look_up$TCS_type == "C")) {
        if (!exists(x = cache$.correlation_df, envir = ..env)) {
            ##  Identify the relevant part.
            .main <- 
                if (.look_up$is_global_only) {
                    ..env[[cache$.G_pairs]]$.data
                } else
                    restrict_array(
                        .arr =  ..env[[cache$.L_levels]]$.data,
                        .restrict = list(variable = "rho"),
                        .drop = TRUE,
                        .never_drop = c("content", "lag"))
            ##  Adjust the bootstrap-case, so the "orig"-value is
            ##  taken out. This prevents it from interfering with
            ##  those computations that only should deal with
            ##  bootstrapped values.
            if (.look_up$is_bootstrap) {
                .not_orig <- which(! dimnames(.main)$content %in% "orig")
                .main <-
                    leanRcoding::restrict_array(
                                     .arr = .main,
                                     .restrict = list(content = .not_orig))
                kill(.not_orig)
            }
            ##  Create the desired data-frame for the investigation of
            ##  the correlations.
            ..env[[cache$.correlation_df]] <- local({
                .tmp <- reshape2::melt(data = .main)
                ##  Identify if a box-plot is desired, i.e. check if
                ##  the data is from a simulated block or from a
                ##  bootstrapped investigation.
                .boxplot <- any(.look_up$is_block,
                                .look_up$is_bootstrap )
                if (! .boxplot) {
                    .formula <- quote(lag ~ content)
                    .tmp <- reshape2::dcast(
                                          data = .tmp,
                                          formula = eval(.formula))
                }
                attributes(.tmp)$lag_data <- TRUE
                attributes(.tmp)$boxplot <- .boxplot
                .tmp
            })
        }
        ##  Extract the values to be added in the final list.
        .data_list <- list(
            correlation = ..env[[cache$.correlation_df]])
        ##  Specify the values to be used when extracting the
        ##  aesthetics.  Reminder: This should perhaps rather be taken
        ##  care of in 'LG_lookup'
        .aes_xy <- if (any(.look_up$is_block,
                           .look_up$is_bootstrap)) {
                       aes(x = lag,
                           y = value)
                   } else
                       eval(bquote(
                           aes(x = lag,
                               y = .(as.symbol(LG_default$sample.prefix)))))
        .aes_min_max <- NULL
        ##  Specify the limits to be used.
        .source <- ifelse(
            test = .look_up$is_global_only,
            yes  = cache$.G_pairs,
            no   = cache$.L_pairs)
        .xlim  <- ..env[[.source]]$.xlim
        .ylim  <- ..env[[.source]]$.ylim
        .aes_list <- list(xy = .aes_xy,
                          min_max = .aes_min_max)
    }
###-------------------------------------------------------------------    
    ##  The investigation of the local Gaussian spectra, will be based
    ##  on the estimates of the global and local estimates that have
    ##  been done in the earlier functions.  In particular, it only
    ##  remains to extract the desired array and then create a
    ##  suitable data-frame based on the result.  The process will be
    ##  divided into a first step that computes the full collection of
    ##  different pointwise confidence intervals (when that is
    ##  required), and a second step that extracts the final
    ##  data-frame from this.  Note that some configurations of the
    ##  input parameters requires that minor tweaks (originating from
    ##  an underlying complex conjugation) must be done in order to
    ##  get the correct sign.
    if  (.look_up$TCS_type == "S") {
        if (!exists(x = cache$.spectra_df, envir = ..env)) {
            ##  Identify which parts of the data-frame that should be
            ##  extracted.  Reminder: We always need one of "median"
            ##  or "orig" (both of them for the bootstrap-case).
            ##  Include the confidence intervals when required.
            .CI_low_high <-
                if (.look_up$is_CI_needed)
                    if (.look_up$confidence_interval == "min_max") {
                        c("min", "max")
                    } else {
                        paste(c("low", "high"),
                              .look_up$confidence_interval,
                              sep = "_")
                    }
            .extract_these <- c(if (.look_up$is_bootstrap)
                                    "orig",
                                ifelse(test = .look_up$is_CI_needed,
                                       yes  = "median",
                                       no   = "orig"),
                                if (.look_up$is_CI_needed)
                                    .CI_low_high)
            ##  Extract the global and local arrays.
            .arr <- list(
                global = restrict_array(
                    .arr = ..env[[cache$.CI_global]]$.data,
                    .restrict = list(content = .extract_these)),
                local = restrict_array(
                    .arr = ..env[[cache$.CI_local]]$.data,
                    .restrict = list(content = .extract_these)))
            kill(.extract_these)
            ##  Convert to data-frames for the plot function, and add
            ##  some additional details needed for the configuration
            ##  of the plots.
            ..env[[cache$.spectra_df]] <- list(
                .data = structure(
                    .Data = lapply(X = names(.arr),
                                   FUN = function(x) 
                                       reshape2::dcast(
                                                     data = reshape2::melt(data = .arr[[x]]),
                                                     formula = omega~content)),
                    .Names = names(.arr)),
                .CI_low_high = .CI_low_high,
                .ylim = list(global = ..env[[cache$.CI_global]]$.ylim,
                             local = ..env[[cache$.CI_local]]$.ylim))
            ##  Add a "white-noise"-node to the resulting list.  This
            ##  shows the true value of the global spectrum in the
            ##  case where white noise is encountered.  Moreover, add
            ##  an indicator of the line-type to be used in the plot.
            ##  This should tell the viewer if the plot considers
            ##  simulated data or something based on bootstrapping of
            ##  a real time series.
            ..env[[cache$.spectra_df]]$.data <- c(
                ..env[[cache$.spectra_df]]$.data,
                .white_noise = switch(
                    EXPR = .look_up$spectra_type,
                    Co = ifelse(
                        test = .look_up$is_auto_pair,
                        yes  = 1,
                        no   = 0),
                    Quad = 0,
                    amplitude = ifelse(
                        test = .look_up$is_auto_pair,
                        yes  = 1,
                        no   = 0),
                    phase = 0),
                .lty = ifelse(test = .look_up$is_block,
                              yes  = 2,
                              no   = 1))
            kill(.arr, .CI_low_high)
        }
        ###-------------------------------------------------------------------
        ##  Extract the values to be added in the final list.
        .data_list <- ..env[[cache$.spectra_df]]$.data
        .xlim <- .look_up$frequency_range
        ##  Reminder: For 'ylim' we need to check if only global data
        ##  should be included.
        .ylim <-
            if (.look_up$is_global_only) {
                ..env[[cache$.spectra_df]]$.ylim$global
            } else {
                range(..env[[cache$.spectra_df]]$.ylim)
            }
        ##  Specify the aesthetics to be used.  Reminder: Some of the
        ##  nodes in this list will be 'NULL', and that is intended
        ##  since test based on that will be used to figure out which
        ##  components that should be included in the final plot.
        .aes_list <- list(
            xy = 
                if (.look_up$is_block) {
                    aes(x = omega,
                        y = median)
                } else
                    aes(x = omega,
                        y = orig),
            min_max = if (.look_up$is_CI_needed)
                local({
                    ##  Identify the limits of the confidence interval.
                    .aes_ymin <- as.symbol( ..env[[cache$.spectra_df]]$.CI_low_high[1])
                    .aes_ymax <- as.symbol( ..env[[cache$.spectra_df]]$.CI_low_high[2])
                    ##  Create the desired result.
                    eval(bquote(
                        aes(ymin = .(.aes_ymin),
                            ymax = .(.aes_ymax))))
                }))
        ##  Add additional nodes (that can become 'NULL' to deal with
        ##  the different layers to be added.)  Note that the present
        ##  setup always includes data from the global investigation
        ##  (using ordinary correlations), in order for us to compare
        ##  the local spectrum against something.  Those parts
        ##  referring to global will thus always be included, whereas
        ##  the other ones requires that we are not only interested in
        ##  the global case.
        .aes_list$.geom_line_global <-
            if (!.look_up$is_block)
                aes(x = omega, y = orig)
        .aes_list$.geom_line_global_me <-
            if (.look_up$is_block)
                aes(x = omega, y = median)
        .aes_list$.geom_ribbon_global <-
            if (.look_up$is_CI_needed)
                .aes_list$min_max
        ## 
        .aes_list$.geom_line_local <-
            if (!.look_up$is_global_only)
                .aes_list$xy
        .aes_list$.geom_ribbon_local  <- 
            if (all(!.look_up$is_global_only,
                    .look_up$is_CI_needed))
                .aes_list$min_max
    }
    ##  Add the desired content
    ..env[[cache$.plot_list]] <- list(
        .data_list = .data_list,
        .lag = .look_up$details$.selected_lag,
        .percentile = .look_up$details$.selected_percentile,
        .xlim = .xlim,
        .ylim = .ylim,
        .aes_list = .aes_list,
        ##  Reminder: This .plot_label' might better be stored in the
        ##  'look_up'-part.
        ## .plot_label = paste(
        ##     toupper(substr(x = .look_up$details$text$plot_type,
        ##                    start = 1,
        ##                    stop = 1)),
        ##     substr(x = .look_up$details$text$plot_type,
        ##            start = 2,
        ##            stop = nchar(.look_up$details$text$plot_type)),
        ##     .look_up$details$text$plot_type_YiYj,
        ##     sep = "")
        .plot_label = .look_up$details$.plot_label,
        .annotate_label = .look_up$details$text$trust_the_result)
    ##  Return the name of this list to the workflow
    cache$.plot_list
}

