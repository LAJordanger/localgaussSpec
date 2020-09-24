#' Create plots of local Gaussian correlations and spectral densities
#'
#' @description This internal function creates the plots of the local
#'     Gaussian correlations and spectral densities.
#'
#' @note The function \code{LG_plot_helper} is the one that should be
#'     used when it is of interest to reproduce the plots in a
#'     non-reactive environment, in particular since it is there it is
#'     possible to fine-tune the size of the annotated text.  This
#'     latter detail is of importance when several plots are to be
#'     included in a grid, see the scripts for examples (use
#'     \code{LG_extract_scripts} in order to access them).
#'
#' @param look_up The environment created by \code{LG_lookup}, which
#'     keep tracks of information for a wide range of
#'     helper-functions.
#'
#' @param ..env The environment with the information needed in order
#'     to create the desired plot.
#'
#' @return A plot is returned to the work-flow.
#'
#' @keywords internal

LG_plot <- function(..env,
                    look_up) {
    ##  Return a heatmap when that is desired.
    if (look_up$heatmap) {
        return(LG_plot_heatmap(..env, look_up))
    }
    ##  Return a distance-plot when that is desired.
    if (look_up$L2_distance_plot) {
        return(LG_plot_distance(..env, look_up))
    }
    ##  Return a complex-type plot when that is desired.
    if (look_up$complex) {
        return(LG_plot_complex(..env, look_up))
    }
    ##  Still running? Then an ordinary plot of lags or spectra should
    ##  be created.  Start with a few shortcuts to make the code later
    ##  on a bit more compact.
    plot_data <- ..env[[look_up$cache$plot_data]]
    curlicues <- look_up$curlicues
    ##  Check if 'plot_data$.data_list' contains a 'correlation'-node, since
    ##  that implies that a plot of these are of interest.
    if (!is.null(plot_data$.data_list$correlation)) {
        .data <- plot_data$.data_list$correlation
        ##  Initiate the framework
        .result <- ggplot(data = .data,
                          mapping = plot_data$.aes_list$xy) +
            coord_cartesian(xlim = plot_data$.xlim,
                            ylim = plot_data$.ylim,
                            default = TRUE) +
            geom_hline(yintercept = 0,
                       linetype = 2,
                       col = "brown",
                       alpha = curlicues$correlation_plot$hline$alpha,
                       lwd =  curlicues$correlation_plot$hline$lwd) +
            geom_vline(xintercept = 0,
                       linetype = 1,
                       col = "brown",
                       alpha = curlicues$correlation_plot$vline$alpha,
                       lwd =  curlicues$correlation_plot$vline$lwd)
        ##  Add stuff based on the 'boxplot'-attribute.
        if (attributes(.data)$boxplot) {
            .result <- .result +
                geom_boxplot(aes(group = cut_width(
                                     x = lag,
                                     width = curlicues$correlation_plot$boxplot$width,
                                     boundary = curlicues$correlation_plot$boxplot$boundary)),
                             alpha = curlicues$correlation_plot$boxplot$alpha,
                             colour = curlicues$correlation_plot$boxplot$colour,
                             fill = curlicues$correlation_plot$boxplot$fill,
                             size = curlicues$correlation_plot$boxplot$size,
                             outlier.alpha = curlicues$correlation_plot$boxplot$outlier.alpha,
                             outlier.size = curlicues$correlation_plot$boxplot$outlier.size)
        } else {
            .result <- .result +
                annotate(geom = "segment",
                         x = .data$lag,
                         xend = .data$lag,
                         y = 0,
                         yend = .data$orig,
                         colour = "black",
                         alpha = curlicues$correlation_plot$segment$alpha,
                         size = curlicues$correlation_plot$segment$size)
        }
        ##  Add details about content when required.  Reminder: It can
        ##  for some non-interactive cases be necessary to tweak the
        ##  size of the labels, and it can then be possible to
        ##  experiment directly with the content stored in the
        ##  'look_up'-attachment of the plot.
        if (isFALSE(look_up$drop_annotation)) {
            if (!is.null(curlicues$text$annotated))
                .result <- .result +
                    eval(curlicues$text$annotated)
        }
        ##  Some minor adjustments
        .result <- .result +
            xlab("h") +
            theme(axis.title.y = element_blank())
        ##  Add the title when relevant
        if (curlicues$title$include) {
            .result  <- .result +
                ggtitle(label = curlicues$title$label) +
                theme(plot.title = eval(create_call(
                          .cc_fun = "element_text",
                          curlicues$title$element_text,
                          .cc_list = TRUE)))
        }
        ##  Add 'details' and 'curlicues' as attributes.
        attributes(.result)$details <- look_up$details
        attributes(.result)$curlicues <- look_up$curlicues
        ##  Return the result to the workflow
        return(.result)
    }
    ##  The spectra cases: Start with a '.canvas' based on the global
    ##  data, since those always will be included.  The horizontal
    ##  line refers to the expected result when white-noise is
    ##  encountered.
    .canvas <- ggplot(data = plot_data$.data_list$global,
                      mapping = plot_data$.aes_list$xy) +
        coord_cartesian(xlim = plot_data$.xlim,
                        ylim = plot_data$.ylim,
                        default = TRUE) +
        if (curlicues$spectra_plot$WN_line$include) {
            geom_hline(yintercept = curlicues$spectra_plot$WN_line$yintercept,
                       size = curlicues$spectra_plot$WN_line$size,
                       alpha = curlicues$spectra_plot$WN_line$alpha,
                       linetype = curlicues$spectra_plot$WN_line$linetype)
        }
    if (curlicues$spectra_plot$WN_line$include)
        names(.canvas$layers) <- "horizontal_line"
    ##  Create a list of logical values to decide what content to
    ##  include in the plot.
    spec_include <- list(
        .geom_line_local = all(
            ! is.null(plot_data$.aes_list$.geom_line_local),
            curlicues$spectra_plot$local$line.include),
        .geom_line_global_me = all(
            ! is.null(plot_data$.aes_list$.geom_line_global_me),
            curlicues$spectra_plot$global$line.include),
        .geom_line_global = all(
            ! is.null(plot_data$.aes_list$.geom_line_global),
            curlicues$spectra_plot$global$line.include),
        .geom_ribbon_global = all(
            ! is.null(plot_data$.aes_list$.geom_ribbon_global),
            curlicues$spectra_plot$global$ribbon.include),
        .geom_ribbon_local = all(
            ! is.null(plot_data$.aes_list$.geom_ribbon_local),
            curlicues$spectra_plot$local$ribbon.include))
    ##  Create a list containing quoted expressions for the layers.
    ##  Some of these might become 'NULL', which made 'ggplot'
    ##  complain at the time I initially wrote this part of the code.
    ##  This pesky problem seems to be solved for the present version
    ##  of 'ggplot', so I guess it would be preferable to rewrite it
    ##  now to avoid these quotations...
    .layers <- list(
        ##  A quote for the selected "central" local line.
        .geom_line_local =
            if (spec_include$.geom_line_local)
                quote(geom_line(
                    mapping = plot_data$.aes_list$.geom_line_local,
                    data = plot_data$.data_list$local,
                    size = curlicues$spectra_plot$local$line.size,
                    alpha = curlicues$spectra_plot$local$line.alpha,
                    col = "blue",
                    linetype = curlicues$spectra_plot$local$linetype)),
        ##  A line dealing with the mean in the 'block'-case.
        .geom_line_global_me =
            if (spec_include$.geom_line_global_me)
                quote(geom_line(
                    mapping = plot_data$.aes_list$.geom_line_global_me,
                    data = plot_data$.data_list$global,
                    size = curlicues$spectra_plot$global$line.size,
                    alpha = curlicues$spectra_plot$global$line.alpha,
                    col = "brown",
                    linetype = curlicues$spectra_plot$local$linetype)),
        ##  A line giving the original global spectrum (when relevant).
        .geom_line_global =
            if (spec_include$.geom_line_global)
                quote(geom_line(
                    mapping = plot_data$.aes_list$.geom_line_global,
                    data = plot_data$.data_list$global,
                    size = curlicues$spectra_plot$global$line.size,
                    alpha = curlicues$spectra_plot$global$line.alpha,
                    col = "red",
                    linetype = curlicues$spectra_plot$local$linetype)),
        ##  Create quotes for the confidence bands too.
        .geom_ribbon_global =
            if (spec_include$.geom_ribbon_global)
                quote(geom_ribbon(
                    mapping = plot_data$.aes_list$.geom_ribbon_global,
                    data = plot_data$.data_list$global,
                    alpha = curlicues$spectra_plot$global$ribbon.alpha,
                    fill = "red")),
        ##
        .geom_ribbon_local =
            if (spec_include$.geom_ribbon_local)
                quote(geom_ribbon(
                    mapping = plot_data$.aes_list$.geom_ribbon_local,
                    data = plot_data$.data_list$local,
                    alpha = curlicues$spectra_plot$local$ribbon.alpha,
                    fill  = "blue")),
        ##  Add details about content when required.  Reminder: It can
        ##  for some non-interactive cases be necessary to tweak the
        ##  size of the labels, and it can then be possible to
        ##  experiment directly with the content stored in the
        ##  'look_up'-attachment of the plot.
        .annotated_text =
            if (isFALSE(look_up$drop_annotation)) {
                if (!is.null(curlicues$text$annotated))
                    curlicues$text$annotated
                })
    ##  Create the sum of layers for 'ggplot', without any
    ##  'NULL'-values messing up the result.
    .result <- as.symbol(".canvas")
    .template <- quote(1 + 1)
    for (i in seq_along(.layers))
        if (! is.null(.layers[[i]]))
            .result <- local({
                ##  Adjust '.template'.
                .template[[2]] <- .result
                .template[[3]] <- .layers[[i]]
                ##  Update '.result'
                .template
            })
    ##  Evaluate '.result' and add names to the layer-part.
    .result <- eval(.result)
    names(.result$layers) <- c(
        names(.canvas$layers),
        names(.layers)[! vapply(X = .layers,
                                FUN = is.null,
                                FUN.VALUE = logical(1))])
    ##  Some minor adjustments
    .result <- .result +
        xlab(quote(omega)) +
        theme(axis.title.y = element_blank())
    ##  Add the title when relevant
    if (curlicues$title$include) {
        .result  <- .result +
            ggtitle(label = curlicues$title$label) +
            theme(plot.title = eval(create_call(
                      .cc_fun = "element_text",
                      curlicues$title$element_text,
                      .cc_list = TRUE)))
    }
    ##  Add 'details' and 'curlicues' as attributes.
    attributes(.result)$details <- look_up$details
    attributes(.result)$curlicues <- look_up$curlicues
    ##  Only in non-interactive setup: Add information attributes
    ##  specifying the ylim-values for the different cases, to make it
    ##  easier to deal with the rescaling.  Moreover, when required,
    ##  add an environment-attribute with information about 'data' and
    ##  'mapping'.  This is of interest when a hybrid-plot is desired,
    ##  where details from twp plots should be meshed together.
    if (look_up$non_interactive) {
        ##  Find the range of those components that have been used
        ##  from the global and local cases.
        tmp_ylim_list <- list()
        ##  Reminder: A rather messy ad hoc solution, the keys should
        ##  have been taken care of in 'LG_lookup'.
        if (spec_include$.geom_line_global) {
            key <- ifelse(
                test = look_up$is_block,
                yes  = "mean",
                no   = "orig")
            tmp_ylim_list$.geom_line_global <-
                range(plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_line_local) {
            key <- ifelse(
                test = look_up$is_block,
                yes  = "mean",
                no   = "orig")
            tmp_ylim_list$.geom_line_local <-
                range(plot_data$.data_list$local[key])
        }
        if (spec_include$.geom_line_global_me) {
            key <- "mean"
            tmp_ylim_list$.geom_line_global_me <-
                range(plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_ribbon_global) {
            key <- ..env[[look_up$cache$spectra_df]]$.CI_low_high
            tmp_ylim_list$.geom_ribbon_global <-
                range(plot_data$.data_list$global[key])
        }
        if (spec_include$.geom_ribbon_local) {
            key <- ..env[[look_up$cache$spectra_df]]$.CI_low_high
            tmp_ylim_list$.geom_ribbon_local <-
                range(plot_data$.data_list$local[key])
        }
        ##  Add the desired limit as an attribute to the plot, with a
        ##  tiny added width at the top and bottom.
        .yrange <- range(tmp_ylim_list)
        attributes(.result)$ylim_list <- list(
                               ylim_full = plot_data$.ylim,
                               ylim_restricted = .yrange + c(-1,1)/40 * diff(.yrange))
        ##  Investigate if a data-environment should be added, and if
        ##  so extract only the relevant parts.
        if (any(unlist(curlicues$data_extraction))) {
            ##  Identify the parts of interest, and note that it might
            ##  be necessary to ignore cases that does not work (e.g
            ##  asking for ribbon-data when those are not available).
            .extract_these <- names(curlicues$data_extraction)[
                as.logical(unlist(curlicues$data_extraction) *
                           unlist(spec_include))]
            ##  Create a temporary list to use for the extraction.
            .tmp_list <- list(
                .geom_line_local = quote(list(
                    mapping = plot_data$.aes_list$.geom_line_local,
                    data = plot_data$.data_list$local)),
                .geom_line_global_me = quote(list(
                    mapping = plot_data$.aes_list$.geom_line_global_me,
                    data = plot_data$.data_list$global)),
                .geom_line_global = quote(list(
                    mapping = plot_data$.aes_list$.geom_line_global,
                    data = plot_data$.data_list$global)),
                .geom_ribbon_global = quote(list(
                    mapping = plot_data$.aes_list$.geom_ribbon_global,
                    data = plot_data$.data_list$global)),
                .geom_ribbon_local = quote(list(
                    mapping = plot_data$.aes_list$.geom_ribbon_local,
                    data = plot_data$.data_list$local)))
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
    ##  Return the plot to the workflow.
    .result
}
