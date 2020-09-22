#' Heatmap-plots for local Gaussian correlations and spectra
#'
#' @description This internal function creates heatmap-based plots
#'     based on the estimated local Gaussian correlations and spectra.
#'
#' @param ..env The environment containing the desired information
#'     from which the data should be extracted
#'
#' @param look_up The environment containing the details needed for
#'     the investigation.
#'
#' @return A heatmap-plot is returned to the workflow.
#'
#' @keywords internal

##  Reminder: This function has been created in order to remove
##  internal functions from the scripts, but it is for the time being
##  alas rather messy.  Reminder: This function does not call the
##  'LG_spectrum_norm'-function for the computations, but it might be
##  an idea to check that out later on.

LG_plot_distance <- function(..env, look_up) {
    ##  Check if we have an investigation along the diagonal, and
    ##  return a "missing implementation"-message for other cases.
    if (! look_up$point_type_branch == "on_diag")
        return("Not implemented outside of diagonal")
    ##  The investigating of the effect of varying the point along the
    ##  diagonal, or the varying of the bandwidth.
    if (look_up$distance_plot_b_v_m_L %in% c("b", "v")) {
        ##  Create a compactified reference.
        b_or_v <- look_up$distance_plot_b_v_m_L
        ##  Extract the relevant data in this case.
        ..lag_values <- names(..env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
        ##  Create the restrict list suitable for the present case.
        ..restrict <- list(variable = "rho",
                           lag = ..lag_values)
        if (b_or_v == "b")
            ..restrict$levels = look_up$levels_point
        if (b_or_v == "v")
            ..restrict$bw_points = look_up$bw_points
        ##  Extract the relevant data.
        .data <- restrict_array(
            .arr = ..env[["LGC_array_local"]]$on_diag,
            .restrict = ..restrict,
            .drop = TRUE,
            .never_drop = c("lag", "bw_points"))
        .global_data <- restrict_array(
            .arr = ..env[["LGC_array_global"]],
            .restrict = list(TS = "TS_for_analysis",
                             lag = ..lag_values),
            .drop = TRUE,
            .never_drop = c("lag", "content"))
        rm(..lag_values, ..restrict)
        ## Compute the product of the correlations with the weights.
        .weighted_data <- multiply_arrays(
            .arr1 = .data,
            .arr2 = ..env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
        .global_weighted_data <- multiply_arrays(
            .arr1 = .global_data,
            .arr2 = ..env[[look_up$cache$weights_f]][[as.character(look_up$cut)]])
        ##  Next: Compute the norms.  In this "on_diagonal"-case for local
        ##  Gaussian autocorrelations, the squared norms are obtained by
        ##  squaring the '.weighted_data', summing over the lags,
        ##  multiplying with 2 (due to folding) and add 1 (to include the
        ##  lag-zero-term).  Take the square root to get the norms.
        .the_norms <- sqrt(
            1 + 2 * my_apply(
                        X = .weighted_data^2,
                        MARGIN = switch(EXPR = b_or_v,
                                        v = "levels",
                                        b = "bw_points"),
                        FUN = sum))
        .the_global_norm <- sqrt(
            1 + 2 * my_apply(
                        X = .global_weighted_data^2,
                        MARGIN = "content",
                        FUN = sum))
        ##  The part below is for the v-case, where we want to adjust
        ##  the dimension-names for '.the_norms' --- i.e. we want to
        ##  use the percentages the diagonal points correspond to.
        if (b_or_v == "v") {
            .quantile_levels <- vapply(
                X = strsplit(x = dimnames(.the_norms)$levels, 
                             split = "_"), FUN = function(..x) pnorm(as.numeric(..x[1])), 
                FUN.VALUE = numeric(1))
            dimnames(.the_norms)$levels <- .quantile_levels
            rm(.quantile_levels)
        }
        ##  Create the data-frame needed for the plot.
        .df <- reshape2::melt(data = .the_norms)
        ##  Specify stuff to be included in the plot.
        distance_plot_title <- sprintf(
            "%s vs. norm for the m=%s local Gaussian autospectrum",
            switch(EXPR = b_or_v,
                   b = "Banwidth",
                   v = "Percentiles"),
            look_up$m_selected)
        .aes_mapping <- switch(
            EXPR = b_or_v,
            b = aes(x = bw_points, y = value),
            v = aes(x = levels, y = value))
        ##  Specification of the xlimit.  For the case with diagonal
        ##  points, this should normally be percentiles between 0 and
        ##  1, but it can also be cases where it is of interest to
        ##  restrict this.  For the bandwidths-case it should in
        ##  general be based on the available data.
        .xlim <-
            if (is.null(look_up$curlicues$limits$xlim)) {
                if (b_or_v == "b") {
                    c(0, max(.df$bw_points))
                } else {
                    c(0,1)
                }
            } else {
                look_up$curlicues$limits$xlim
            }
        ##  Create the plot.
        distance_plot <-
            ggplot(data=.df,
                   mapping = .aes_mapping) +
            geom_line(size = 0.1,
                      colour = "brown") +    
            ##  Remove the labels.
            xlab(label = NULL) +
            ylab(label = NULL) +
            ##  Add the title.
            ggtitle(label = distance_plot_title) +
            theme(plot.title = element_text(hjust = 0.5,
                                            vjust = 0,
                                            size = 8,
                                            colour = "brown"))
        ##  Adjust the limits, and for the v-case it should be used
        ##  percentages.
        distance_plot <-
            if  (b_or_v == "b") {
                distance_plot +
                    scale_x_continuous(limits = .xlim)
            } else {
                distance_plot +
                    scale_x_continuous(limits = .xlim,
                                       labels = scales::percent)
            }
        ##  When required, add points that highlights some of the
        ##  'v'-values, i.e. typically those used in the basic plots
        ##  of the local Gaussian spectral densities.
        if (b_or_v == "v") {
            if (!is.null(look_up$curlicues$distance_plot$add_points_at_levels)) {
                .levels <- look_up$curlicues$distance_plot$add_points_at_levels
                ##  Check if some of the given levels might not be
                ##  included, and if so return a warning.
                .OK_levels <- .levels %in% .df$levels
                if (!all(.OK_levels)) {
                    .problems <- .levels[!.OK_levels]
                    warning(
                        sprintf("%s %s\n%s%s: %s",
                                "It is only possible to add a point for a level",
                                "when a distance value has been computed for it.",
                                "Ignoring level",
                                ifelse(test = {length(.problems) > 1},
                                       yes  = "s",
                                       no   = ""),
                                paste(.problems,
                                      collapse = ", ")))
                    kill(.problems)
                }
                ##  Add points (when possible), include specifications
                ##  given to the curlicues-list.
                .point_details <- look_up$curlicues$distance_plot
                if (any(.OK_levels)) {
                    .point_details$add_points_at_levels <- NULL
                    .point_details$geom  <- "point"
                    .point_details$x <- .levels
                    .point_details$y <- .df$value[which(.df$levels %in% .levels)]
                    distance_plot <-
                        distance_plot +
                        do.call(what = "annotate", args = .point_details)
                }
                rm(.levels, .point_details)
            }
        }
        if (b_or_v == "b") {
            if (!is.null(look_up$curlicues$distance_plot$add_points_at_bws)) {
                .bws <- look_up$curlicues$distance_plot$add_points_at_bws
                ##  Check if some of the given levels might not be
                ##  included, and if so return a warning.
                .OK_bws <- .bws %in% .df$bw_points
                if (!all(.OK_bws)) {
                    .problems <- .bws[!.OK_bws]
                    warning(
                        sprintf("%s %s\n%s%s: %s",
                                "It is only possible to add a point for a bandwidth",
                                "when a distance value has been computed for it.",
                                "Ignoring bandwidth",
                                ifelse(test = {length(.problems) > 1},
                                       yes  = "s",
                                       no   = ""),
                                paste(.problems,
                                      collapse = ", ")))
                    kill(.problems)
                }
                ##  Add points (when possible), include specifications
                ##  given to the curlicues-list.
                .point_details <- look_up$curlicues$distance_plot
                if (any(.OK_bws)) {
                    .point_details$add_points_at_bws <- NULL
                    .point_details$geom  <- "point"
                    .point_details$x <- as.numeric(.bws)
                    .point_details$y <- .df$value[which(.df$bw_points %in% .bws)]
                    distance_plot <-
                        distance_plot +
                        do.call(what = "annotate", args = .point_details)
                }
                rm(.bws, .point_details)
            }
        }
        ##  Add information about the value of the global norm.
        .global_label <- sprintf("D*(f^'%s'*(omega)) == '%s'",
                                 look_up$m_selected,
                                 .the_global_norm)
        distance_plot <- distance_plot +
            ##  Add a line for the norm of the global spectrum.
            geom_hline(yintercept = .the_global_norm,
                       size = 0.1,
                       colour = "red",
                       lty = 1)  +
            ##  Add the label with the value.
            annotate(
                geom = "text",
                x = .xlim[1],
                y = .the_global_norm,
                size = 2,
                label = .global_label,
                col = "red",
                vjust = -0.3,
                hjust = "inward",
                parse = TRUE)
        ##  Extract the initial values for the annotation.
        annotate_norm <- look_up$curlicues$text
        ## Adjust the limits when relevant.
        annotate_norm$annotated$x <- local({
            .x <- annotate_norm$annotated$x
            .scaling <-
                (.x - min(.x)) / diff(range(.x))
            .xlim[1] + .scaling * diff(.xlim)
        })
        ## Tweak the size of the annotated text so it looks decent
        ##  after the grid-plot has been saved.
        .scale <- 0.4
        annotate_norm$annotated$size <- 
            .scale * annotate_norm$annotated$size
        ##  Adjust the stamp for the distance-based plot.
        annotate_norm$annotated$label[1] <-
            sprintf("D*(%s)",
                    annotate_norm$annotated$label[1])
        distance_plot <-
            distance_plot +
            eval(annotate_norm$annotated)
        ##  Adjust the way the axis-labels are given.
        size_v <- annotate_norm$annotated_df["NC_value", "size"] * .scale
        v_just_v <- annotate_norm$annotated_df["NC_value", "vjust"]
        distance_plot <- distance_plot +
            annotate(geom = "text",
                     label = b_or_v,
                     parse = TRUE,
                     x = Inf,
                     y = -Inf,
                     size = size_v,
                     hjust = "inward",
                     vjust = v_just_v)
        ##  Return the plot to the workflow.
        return(distance_plot)
    }
}
