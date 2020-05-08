#' Create the data needed when plotting the local Gaussian
#' correlations and spectra.
#'
#' This function created an environment with the data needed for the
#' plots of the estimated local Gaussian correlations and spectra.  A
#' simple caching is performed in order to avoid having computations
#' more than once.
#'
#' @param look_up The list created by \code{LG_lookup}, containing
#'     the information needed in order to decide what kind of
#'     data-frame that is required.
#'
#' @param ..env The environment containing the required information,
#'     and also the environment where the resulting environment will
#'     be stored.
#'
#' @return This function adds a new environment to the environment
#'     \code{..env}, which contains the data needed for the function
#'     \code{LG_plot} to work as desired.
#'
#' @keywords internal

LG_create_plot_df <- function(look_up,
                              ..env) {
    ##  Some shortcuts for the caching and restriction.
    cache <- look_up$cache
    restrict <- look_up$restrict
    ##  Return the name of the plot-list if the computation already
    ##  has been performed.
    if (exists(x = cache$plot_data, envir = ..env)) {
        return(cache$plot_data)
    }
    ##  Create the data-frame to be used when it is the estimated
    ##  local Gaussian correlations themselves that will be
    ##  investigated.
    if ((look_up$TCS_type == "C")) {
        if (!exists(x = cache$correlation_df, envir = ..env)) {
            ##  Identify the relevant part.
            .main <- 
                if (look_up$is_global_only) {
                    ..env[[cache$G_pairs]]$.data
                } else
                    restrict_array(
                        .arr =  ..env[[cache$L_levels]]$.data,
                        .restrict = list(variable = "rho"),
                        .drop = TRUE,
                        .never_drop = c("content", "lag"))
            ##  Adjust the bootstrap-case, so the "orig"-value is
            ##  taken out. This prevents it from interfering with
            ##  those computations that only should deal with
            ##  bootstrapped values.
            if (look_up$is_bootstrap) {
                .not_orig <- which(! dimnames(.main)$content %in% "orig")
                .main <- restrict_array(
                    .arr = .main,
                    .restrict = list(content = .not_orig))
                kill(.not_orig)
            }
            ##  Create the desired data-frame for the investigation of
            ##  the correlations.
            ..env[[cache$correlation_df]] <- local({
                .tmp <- reshape2::melt(data = .main)
                ##  Identify if a box-plot is desired, i.e. check if
                ##  the data is from a simulated block or from a
                ##  bootstrapped investigation.
                .boxplot <- any(look_up$is_block,
                                look_up$is_bootstrap )
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
            correlation = ..env[[cache$correlation_df]])
        ##  Specify the values to be used when extracting the
        ##  aesthetics.  Reminder: This should perhaps rather be taken
        ##  care of in 'LG_lookup'
        .aes_xy <- if (any(look_up$is_block,
                           look_up$is_bootstrap)) {
                       aes(x = lag,
                           y = value)
                   } else
                       eval(bquote(
                           aes(x = lag,
                               y = .(as.symbol(LG_default$sample.prefix)))))
        .aes_min_max <- NULL
        ##  Specify the limits to be used.
        .source <- ifelse(
            test = look_up$is_global_only,
            yes  = cache$G_pairs,
            no   = cache$L_pairs)
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
    if  (look_up$TCS_type == "S") {
        if (!exists(x = cache$spectra_df, envir = ..env)) {
            ##  Extract the global and local arrays.
            .arr <- list(
                global = restrict_array(
                    .arr = ..env[[cache$CI_global]]$.data,
                    .restrict = restrict$S$spectra_df),
                local = restrict_array(
                    .arr = ..env[[cache$CI_local]]$.data,
                    .restrict = restrict$S$spectra_df))
            ##  Convert to data-frames for the plot function, and add
            ##  some additional details needed for the configuration
            ##  of the plots.
            ..env[[cache$spectra_df]] <- list(
                .data = structure(
                    .Data = lapply(X = names(.arr),
                                   FUN = function(x) 
                                       reshape2::dcast(
                                                     data = reshape2::melt(data = .arr[[x]]),
                                                     formula = omega~content)),
                    .Names = names(.arr)),
                .CI_low_high = look_up$.CI_low_high,
                .ylim = list(global = ..env[[cache$CI_global]]$.ylim,
                             local = ..env[[cache$CI_local]]$.ylim))
            kill(.arr)
        }
        ###-------------------------------------------------------------------
        ##  Extract the values to be added in the final list.
        .data_list <- ..env[[cache$spectra_df]]$.data
        .xlim <- look_up$xlim
        ##  Reminder: For 'ylim' we need to check if only global data
        ##  should be included.
        .ylim <-
            if (look_up$is_global_only) {
                ..env[[cache$spectra_df]]$.ylim$global
            } else {
                range(..env[[cache$spectra_df]]$.ylim)
            }
        ##  Specify the aesthetics to be used.  Reminder: Some of the
        ##  nodes in this list will be 'NULL', and that is intended
        ##  since test based on that will be used to figure out which
        ##  components that should be included in the final plot.
        .aes_list <- list(
            xy = 
                if (look_up$is_block) {
                    aes(x = omega,
                        y = mean)
                } else
                    aes(x = omega,
                        y = orig),
            min_max = if (look_up$is_CI_needed)
                          local({
                              ##  Identify the limits of the confidence interval.
                              .aes_ymin <- as.symbol( ..env[[cache$spectra_df]]$.CI_low_high[1])
                              .aes_ymax <- as.symbol( ..env[[cache$spectra_df]]$.CI_low_high[2])
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
            if (!look_up$is_block)
                aes(x = omega, y = orig)
        .aes_list$.geom_line_global_me <-
            if (look_up$is_block)
                aes(x = omega, y = mean)
        .aes_list$.geom_ribbon_global <-
            if (look_up$is_CI_needed)
                .aes_list$min_max
        ## 
        .aes_list$.geom_line_local <-
            if (!look_up$is_global_only)
                .aes_list$xy
        .aes_list$.geom_ribbon_local  <- 
            if (all(!look_up$is_global_only,
                    look_up$is_CI_needed))
                .aes_list$min_max
    }
    ##  Add the desired content in an environment.
    ..env[[cache$plot_data]] <- as.environment(list(
        .data_list = .data_list,
        .xlim = .xlim,
        .ylim = .ylim,
        .aes_list = .aes_list))
    ##  Nothing to return.
    invisible(NULL)
}
