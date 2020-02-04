#' Create plots to inspect different aspects of Local Gaussian
#' Spectral Densities and Local Gaussian Correlations.
#'
#' To get a joint setup for plots used in interactive sessions with
#' \code{LG_shiny} and plots used in a more static setting, the
#' intention of this function is to collect the creation of the
#' different plots in one main function.  Note that the arguments of
#' this function is not as such intended to be written out by the user
#' when the function is used in a static setting, instead they are
#' supposed to be given from the \code{LG_shiny}-interface.
#'
#' @param .look_up The list created by \code{LG_lookup}, which keep
#'     tracks of information over a wide range of helper-functions.
#'
#' @param ..env The environment with the information needed in order
#'     to create the desired plot.
#'
#' @return The first incarnation of this code will focus on the
#'     creation of plots depicting the local Gaussian spectral
#'     densities, with our without the information required to create
#'     the bootstrap based confidence intervals.  Other plots will
#'     probably be added later on.
#'
#' @keywords internal

LG_plot <- function(..env,
                    .look_up) {
    ##  Add a pointer to the environemnt containing the plot data.
    .plot_data <- ..env[[.look_up$cache$.plot_data]]
    ##  Find the default values for the curlicues, .i.e. information
    ##  about whether or not annotations should be included or not,
    ##  text sizes, colours, etc.
    .curlicues <- LG_plot_curlicues(
        new_curlicues = .look_up$curlicues)
    ##  When required, update '.curlicues' based on user defined
    ##  values (relevant outside of the 'shiny'-application.)
    if (!is.null(..env$user_curlicues)) {
        .curlicues <- LG_plot_curlicues(
            new_curlicues = ..env$user_curlicues,
            old_curlicues = .curlicues)
    }
    ###-------------------------------------------------------------------
    ##  Check if '.plot_data$.data_list' contains a 'correlation'-node, since
    ##  that implies that a plot of these are of interest.
    if (!is.null(.plot_data$.data_list$correlation)) {
        .data <- .plot_data$.data_list$correlation
        ##  Initiate the framework
        .result <- ggplot(data = .data,
                          mapping = .plot_data$.aes_list$xy) +
            coord_cartesian(xlim = .plot_data$.xlim,
                            ylim = .plot_data$.ylim,
                            default = TRUE) +
            geom_hline(yintercept = 0,
                       linetype = 2,
                       col = "brown",
                       alpha = .curlicues$correlation_plot$hline$alpha,
                       lwd =  .curlicues$correlation_plot$hline$lwd) +
            geom_vline(xintercept = 0,
                       linetype = 1,
                       col = "brown",
                       alpha = .curlicues$correlation_plot$vline$alpha,
                       lwd =  .curlicues$correlation_plot$vline$lwd) +
            ##  Add percentile when required (only local cases).
            if (.curlicues$v_value$include)
                eval(create_call(.cc_fun = "annotate",
                                 .curlicues$v_value$annotate,
                                 .cc_list = TRUE))
        ##  Add a quote for numerical convergence (only local cases).
            if (.curlicues$NC_value$include) 
                .result  <- .result +
                    eval(create_call(
                    .cc_fun = "annotate",
                    .curlicues$NC_value$annotate,
                    .cc_list = TRUE))
        ##  Add names to the layers
        names(.result$layers) <-
            c(".x_axis_line", ".y_axis_line",
              if (.curlicues$v_value$include)
                  ".annotate_percentile",
              if (.curlicues$NC_value$include)
                  ".annotate_convergence")
        ##  Add stuff based on the 'boxplot'-attribute.
        if (attributes(.data)$boxplot) {
            .result <- .result +
                geom_boxplot(aes(group = cut_width(
                                     x = lag,
                                     width = .curlicues$correlation_plot$boxplot$width,
                                     boundary = .curlicues$correlation_plot$boxplot$boundary)),
                             alpha = .curlicues$correlation_plot$boxplot$alpha,
                             size = .curlicues$correlation_plot$boxplot$size,
                             outlier.alpha = .curlicues$correlation_plot$boxplot$outlier.alpha,
                             outlier.size = .curlicues$correlation_plot$boxplot$outlier.size)
            ##  Add name to layers.
            names(.result$layers) <- c(
                head(x = names(.result$layers), n = -1),
                ".boxplot_lags")
        } else {
            .result <- .result +
                annotate(geom = "segment",
                         x = .data$lag,
                         xend = .data$lag,
                         y = 0,
                         yend = .data$orig,
                         colour = "black",
                         alpha = .curlicues$correlation_plot$segment$alpha,
                         size = .curlicues$correlation_plot$segment$size)
            ##  Add name to layers.
            names(.result$layers) <- c(
                head(x = names(.result$layers), n = -1),
                ".lag_lines_local")
        }
        ###-------------------------------------------------------------------
        ##  Some minor adjustments
        .result <- .result +
            xlab("h") +
            theme(axis.title.y = element_blank())
        ##  Add the title when relevant
        if (.curlicues$title$include) {
            .result  <- .result +
                ggplot2::ggtitle(label = .curlicues$title$label) +
                ggplot2::theme(plot.title = eval(create_call(
                                   .cc_fun = "element_text",
                                   .curlicues$title$element_text,
                                   .cc_list = TRUE)))
        }
        ##  Add the details as an attribute.
        attributes(..env$.lag_plot)$details <- .look_up$details
        ##  Return the result to the workflow
        return(.result)
    }
    ###-------------------------------------------------------------------
    ##  The spectra cases.
    ##  -------------------------------------------------------------------
    ##  Start with a '.canvas' based on the global data, since those
    ##  always will be included.  The horizontal line refers to the
    ##  expected result when white-noise is encountered.
    .canvas <- ggplot(data = .plot_data$.data_list$global,
                      mapping = .plot_data$.aes_list$xy) +
        coord_cartesian(xlim = .plot_data$.xlim,
                        ylim = .plot_data$.ylim,
                        default = TRUE) +
        if (.curlicues$spectra_plot$WN_line$include) {
            geom_hline(yintercept = .plot_data$.data_list$.white_noise,
                       size = .curlicues$spectra_plot$WN_line$size,
                       alpha = .curlicues$spectra_plot$WN_line$alpha,
                       linetype = 3)
        }
    if (.curlicues$spectra_plot$WN_line$include)
        names(.canvas$layers) <- "horizontal_line"
    ###-------------------------------------------------------------------
    ##  Create a list of logical values to decide what content to
    ##  include in the plot.
    spec_include <- list(
        .geom_line_local = all(
            ! is.null(.plot_data$.aes_list$.geom_line_local),
            .curlicues$spectra_plot$local$line.include),
        .geom_line_global_me = all(
            ! is.null(.plot_data$.aes_list$.geom_line_global_me),
            .curlicues$spectra_plot$global$line.include),
        .geom_line_global = all(
            ! is.null(.plot_data$.aes_list$.geom_line_global),
            .curlicues$spectra_plot$global$line.include),
        .geom_ribbon_global = all(
            ! is.null(.plot_data$.aes_list$.geom_ribbon_global),
            .curlicues$spectra_plot$global$ribbon.include),
        .geom_ribbon_local = all(
            ! is.null(.plot_data$.aes_list$.geom_ribbon_local),
            .curlicues$spectra_plot$local$ribbon.include))
    ###-------------------------------------------------------------------
    ##  Create a list containing quoted expressions for the layers.
    ##  Some of these might become `NULL`, which made `ggplot`
    ##  complain at the time I initially wrote this part of the code.
    ##  This pesky problem seems to be solved for the present version
    ##  of 'ggplot', so I guess it would be preferable to rewrite it
    ##  now to avoid these quotations...
    .layers <- list(
        ##  A quote for the selected "central" local line.
        .geom_line_local =
            if (spec_include$.geom_line_local)
                quote(geom_line(
                    mapping = .plot_data$.aes_list$.geom_line_local,
                    data = .plot_data$.data_list$local,
                    size = .curlicues$spectra_plot$local$line.size,
                    alpha = .curlicues$spectra_plot$local$line.alpha,
                    col = "blue",
                    lty = .plot_data$.data_list$.lty)),
        ##  A line dealing with the mean in the `block`-case.
        .geom_line_global_me =
            if (spec_include$.geom_line_global_me)
                quote(geom_line(
                    mapping = .plot_data$.aes_list$.geom_line_global_me,
                    data = .plot_data$.data_list$global,
                    size = .curlicues$spectra_plot$global$line.size,
                    alpha = .curlicues$spectra_plot$global$line.alpha,
                    col = "brown",
                    lty = .plot_data$.data_list$.lty)),
        ##  A line giving the original global spectrum (when relevant).
        .geom_line_global =
            if (spec_include$.geom_line_global)
                quote(geom_line(
                    mapping = .plot_data$.aes_list$.geom_line_global,
                    data = .plot_data$.data_list$global,
                    size = .curlicues$spectra_plot$global$line.size,
                    alpha = .curlicues$spectra_plot$global$line.alpha,
                    col = "red",
                    lty = .plot_data$.data_list$.lty)),
        ##  Create quotes for the confidence bands too.
        .geom_ribbon_global =
            if (spec_include$.geom_ribbon_global)
                quote(geom_ribbon(
                    mapping = .plot_data$.aes_list$.geom_ribbon_global,
                    data = .plot_data$.data_list$global,
                    alpha = .curlicues$spectra_plot$global$ribbon.alpha,
                    fill = "red")),
        ##
        .geom_ribbon_local =
            if (spec_include$.geom_ribbon_local)
                quote(geom_ribbon(
                    mapping = .plot_data$.aes_list$.geom_ribbon_local,
                    data = .plot_data$.data_list$local,
                    alpha = .curlicues$spectra_plot$local$ribbon.alpha,
                    fill  = "blue")),
        ##  Add a quote for 'lag', when given.
        .annotate_lag =
            if (.curlicues$m_value$include)
                create_call(.cc_fun = "annotate",
                            .curlicues$m_value$annotate,
                            .cc_list = TRUE),
        ##  Add a quote for `.percentile`, when given.
        .annotate_percentile = 
            if (.curlicues$v_value$include)
                create_call(.cc_fun = "annotate",
                            .curlicues$v_value$annotate,
                            .cc_list = TRUE),
        ##  Add a quote for numerical convergence.
        .annotate_convergence =
            if (.curlicues$NC_value$include) 
                create_call(
                    .cc_fun = "annotate",
                    .curlicues$NC_value$annotate,
                    .cc_list = TRUE))
    ###-------------------------------------------------------------------
    ##  Create the sum of layers for `ggplot`, without any
    ##  `NULL`-values messing up the result.
    .result <- as.symbol(".canvas")
    .template <- quote(1 + 1)
    for (i in seq_along(.layers))
        if (! is.null(.layers[[i]]))
            .result <- local({
                ##  Adjust `.template`.
                .template[[2]] <- .result
                .template[[3]] <- .layers[[i]]
                ##  Update `.result`
                .template
            })
    ###-------------------------------------------------------------------
    ##  Evaluate '.result' and add names to the layer-part.
    .result <- eval(.result)
    names(.result$layers) <- c(
        names(.canvas$layers),
        names(.layers)[! vapply(X = .layers,
                                FUN = is.null,
                                FUN.VALUE = logical(1))])
    ###-------------------------------------------------------------------
    ##  Some minor adjustments
    .result <- .result +
        xlab(quote(omega)) +
        theme(axis.title.y = element_blank())
    ##  Add the title when relevant
    if (.curlicues$title$include) {
        .result  <- .result +
            ggplot2::ggtitle(label = .curlicues$title$label) +
            ggplot2::theme(plot.title = eval(create_call(
                               .cc_fun = "element_text",
                               .curlicues$title$element_text,
                               .cc_list = TRUE)))
    }
    ##  Add the details as an attribute.
    attributes(.result)$details <- .look_up$details
    ###-------------------------------------------------------------------
    ##  Only in non-interactive setup: Add information attributes
    ##  specifying the ylim-values for the different cases, to make it
    ##  easier to deal with the rescaling.  Moreover, when required,
    ##  add an environment-attribute with information about 'data' and
    ##  'mapping'.  This is of interest when a hybrid-plot is desired,
    ##  where details from twp plots should be meshed together.
    if (..env$non_interactive) {
        ##  Find the range of those components that have been used
        ##  from the global and local cases.
        tmp_ylim_list <- list()
        ##  Reminder: A rather messy ad hoc solution, the keys should
        ##  have been taken care of in 'LG_lookup'.
        if (spec_include$.geom_line_global) {
            key <- "orig"
            tmp_ylim_list$.geom_line_global <-
                range(.plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_line_local) {
            key <- ifelse(
                test = .look_up$is_block,
                yes  = "mean",
                no   = "orig")
            tmp_ylim_list$.geom_line_local <-
                range(.plot_data$.data_list$local[key])
        }
        if (spec_include$.geom_line_global_me) {
            key <- "mean"
            tmp_ylim_list$.geom_line_global_me <-
                range(.plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_ribbon_global) {
            key <- ..env[[.look_up$cache$.spectra_df]]$.CI_low_high
            tmp_ylim_list$.geom_ribbon_global <-
                range(.plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_ribbon_local) {
            key <- ..env[[.look_up$cache$.spectra_df]]$.CI_low_high
            tmp_ylim_list$.geom_ribbon_local <-
                range(.plot_data$.data_list$local[key])
        }
        ##  Add the desired limit as an attribute to the plot, with a
        ##  tiny added width at the top and bottom.
        .yrange <- range(tmp_ylim_list)
        attributes(.result)$ylim_list <- list(
                               ylim_full = .plot_data$.ylim,
                               ylim_restricted = .yrange + c(-1,1)/40 * diff(.yrange))
        ##  Investigate if a data-environment should be added, and if
        ##  so extract only the relevant parts.
        if (any(unlist(.curlicues$data_extraction))) {
            ##  Identify the parts of interest, and note that it might
            ##  be necessary to ignore cases that does not work (e.g
            ##  asking for ribbon-data when those are not available).
            .extract_these <- names(.curlicues$data_extraction)[
                as.logical(unlist(.curlicues$data_extraction) *
                           unlist(spec_include))]
            ##  Create a temporary list to use for the extraction.
            .tmp_list <- list(
                .geom_line_local = quote(list(
                    mapping = .plot_data$.aes_list$.geom_line_local,
                    data = .plot_data$.data_list$local)),
                .geom_line_global_me = quote(list(
                    mapping = .plot_data$.aes_list$.geom_line_global_me,
                    data = .plot_data$.data_list$global)),
                .geom_line_global = quote(list(
                    mapping = .plot_data$.aes_list$.geom_line_global,
                    data = .plot_data$.data_list$global)),
                .geom_ribbon_global = quote(list(
                    mapping = .plot_data$.aes_list$.geom_ribbon_global,
                    data = .plot_data$.data_list$global)),
                .geom_ribbon_local = quote(list(
                    mapping = .plot_data$.aes_list$.geom_ribbon_local,
                    data = .plot_data$.data_list$local)))
            if (length(.extract_these) > 0) {
                ##  Create the environment and add it to the result.
                data_env <- new.env()
                for (.name in .extract_these) {
                    data_env[[.name]] <- eval(.tmp_list[[.name]])
                }
                attributes(.result)$data_env <- data_env
            }
            kill (.extract_these, .tmp_list, .name)
        }
    }
    ###-------------------------------------------------------------------
    ##  Return the plot to the workflow.
    .result

    
    
}
