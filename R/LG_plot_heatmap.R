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

LG_plot_heatmap <- function(..env, look_up) {
    ##  Check if we have an investigation along the diagonal, and
    ##  return a "missing implementation"-message for other cases.
    if (! look_up$point_type_branch == "on_diag")
        return("Not implemented outside of diagonal")
    ##  Specify the dimensions that never should be dropped in the
    ##  present case of interest.
    .never_drop <- c(
        switch(EXPR = look_up$TCS_type,
               C    = "lag",
               S    = "omega"),
        switch(EXPR = look_up$heatmap_b_or_v,
               b    = "bw_points",
               m    = "m",
               v    = "levels"))
    ##  Initiate the restriction list, and add to it later on if
    ##  required.  (Should probably be taken care of in some other
    ##  function).
    .restrict_list <-
        if (look_up$heatmap_b_or_v == "b") {
            list(variable = "rho",
                 levels = look_up$levels_point)
        } else {
            if (look_up$heatmap_b_or_v == "v") {
                list(variable = "rho",
                     bw_points = look_up$bw_points)
            } else {
                list(variable = "rho",
                     levels = look_up$levels_point,
                     bw_points = look_up$bw_points)
            }
        }
    ##  Extract the data of interest, and specify assorted arguments
    ##  needed later on.
    if (look_up$TCS_type == "C") {
        ##  The correlation case.
        ..the_data <- 
            if (look_up$heatmap_b_or_v == "b") {
                local({
                    not_null <- which(! dimnames(..env[[look_up$local_name]]$on_diag)$lag %in% "0")
                    .data <- restrict_array(
                        .arr = ..env[[look_up$local_name]]$on_diag,
                        .restrict = list(lag = not_null),
                        .drop = TRUE,
                        .never_drop = c("lag", "levels", "bw_points"))
                })
            } else {
                ..env[[look_up$cache$L_pairs]]$.data
            }
        .midpoint <- 0
        .aes_mapping <-
            if (look_up$heatmap_b_or_v == "b") {
                aes(x = lag,
                    y = bw_points,
                    fill = value,
                    z = value)
            } else {
                aes(x = lag,
                    y = levels,
                    fill = value,
                    z = value)
            }
        .limits_gradient <- c(-1,1)
        .xlab_expression <- "h"
        .plot_title <- sprintf(
            "Heatmap for the m=%s local Gaussian %scorrelations (%s)",
            look_up$m_selected,
            ifelse(test = look_up$is_auto_pair,
                   yes  = "auto",
                   no   = "cross"),
            ifelse(test = {look_up$heatmap_b_or_v == "b"},
                   yes  = "bandwidth",
                   no   = "along digaonal"))
    } else {
        ###  The spectra case.
        if (look_up$is_multivariate) {
            .restrict_list <- c(
                list(S_type = look_up$spectra_type),
                .restrict_list)
        }
        ##  Get hold of the desired data.
        .bm_1 <- look_up$cache$spectra_local
        .bm_2 <- look_up$.bm_CI_local_spectra
        ..the_data <-
            if (look_up$heatmap_b_or_v %in% c("b", "v")) {
                ..env[[.bm_1]][[.bm_2]]
            } else {
                local({
                    .temp <- lapply(
                        X = ..env[[.bm_1]][[.bm_2[1]]],
                        FUN = function(x)
                            x[[.bm_2[3]]])
                    ##  Reminder: The first node, 'cut=1', corresponds
                    ##  to the 'm=0'-truncation, and can be removed.
                    .TST <- my_abind(
                        .temp[-1],
                        .list = TRUE)
                    ##  The 'cut'-dimension should be adjusted to
                    ##  'm'-dimension.
                    .pos <- which(names(dimnames(.TST)) == "cut")
                    names(dimnames(.TST))[.pos] <- "m"
                    dimnames(.TST)$m <- as.numeric(dimnames(.TST)$m) - 1
                    ##  Return the desired data.
                    .TST
                })
            }
        .midpoint <-
            if (look_up$is_univariate) {
                if (look_up$spectra_type %in% c("Quad", "phase")) {
                    0
                } else {
                    ifelse(test = {look_up$spectra_f_or_F == "F"},
                           yes  = 0.75,
                       no   = 1)
                }
            } else {
                0
            }
        .aes_mapping <-
            if (look_up$heatmap_b_or_v == "b") {
                aes(x = omega,
                    y = bw_points,
                    fill = value,
                    z = value)
            } else {
                if (look_up$heatmap_b_or_v == "v") {
                    aes(x = omega,
                        y = levels,
                        fill = value,
                        z = value)
                } else {
                    aes(x = omega,
                        y = m,
                        fill = value,
                        z = value)
                }
            }
        .limits_gradient <- NULL
        .xlab_expression <- expression(omega)
        .plot_title <-
            if (look_up$heatmap_b_or_v %in% c("b", "v")) {
                sprintf("Heatmap for the m=%s local Gaussian %sspectrum (%s)",
                        look_up$m_selected,
                        ifelse(test = look_up$is_univariate,
                               yes  = "auto",
                               no   = look_up$spectra_type),
                        ifelse(test = {look_up$heatmap_b_or_v == "b"},
                               yes  = "bandwidth",
                               no   = "along digaonal"))
            } else {
                "Heatmap-visualisation for diferent truncation levels"
            }        
    }
    ###------------------------------------------------------###
    ##  If the length of the 'content'-dimension is larger than one,
    ##  then it is first necessary to compute the mean of the
    ##  values. Note: For local Gaussian spectral densities, this is
    ##  the same as the result obtained when the means of the local
    ##  Gaussian autocorrelations are used in the computation.
    ###------------------------------------------------------###
    ##  Reminder: We might also like to loop over the individual
    ##  samples in a block, since that could give us a brief idea with
    ##  regard to the variability between different samples.  However,
    ##  this will require an additional loop-argument (that I would
    ##  like to include for other plots too), and it is then also
    ##  necessary to consider in more detail the limits that should be
    ##  used on the legend-colours.
    ###------------------------------------------------------###
    if (length(dimnames(..the_data)$content) > 1) {
        ##  In the bootstrap-case, the 'content'-dimension contains
        ##  the "orig"-values, but those should not be included if we
        ##  want a mean of the bootstrapped values.
        if (look_up$is_bootstrap) {
            restrict_array(
                .arr=..the_data,
                .restrict = list(content = setdiff(
                                     x = dimnames(..the_data)$content,
                                     y = "orig")))
        }
        ##  Compute the desired mean-values.
        ..the_data <- my_apply(
            X = ..the_data,
            MARGIN = which(names(dimnames(..the_data))!="content"),
            FUN = mean)
        ##  Update '.restrict_list'
        .restrict_list <- c(.restrict_list,
                            list(mean = "mean"))
    }
    ##  Restrict the attention to the data of interest.
    ..step_1 <- restrict_array(
        .arr= ..the_data,
        .restrict = .restrict_list,
        .drop = TRUE,
        .never_drop = .never_drop)
    if (look_up$TCS_type == "C") {
        ##  Use the diagonal reflection property when dealing with the
        ##  plots of the local Gaussian cross-correlations.
        if (any(look_up$is_multivariate,
                look_up$is_off_diagonal))  {
            ##  Reminder: This part should perhaps be stored in a
            ##  separate helper function.
            .levels <- dimnames(..step_1)$levels
            .levels_reflected <- vapply(
                X = strsplit(x = .levels,
                             split = "_"),
                FUN = function(x)
                    sprintf("%s_%s",
                            x[2],
                            x[1]),
                FUN.VALUE = character(1))
            .pos_part <- restrict_array(
                .arr = ..step_1,
                .restrict = list(
                    lag = c(
                        if (look_up$is_lag_zero_needed)
                            "0",
                        as.character(look_up$lag_vec)),
                    levels = .levels,
                    pairs = look_up$pairs_ViVj),
                .drop = TRUE,
                .never_drop = c("lag", "levels"))
            .neg_part <- restrict_array(
                .arr = ..step_1,
                .restrict = list(
                    lag = as.character(look_up$lag_vec),
                    levels = .levels_reflected,
                    pairs = look_up$pairs_VjVi),
                .drop = TRUE,
                .never_drop = c("lag", "levels"))
            kill(.levels, .levels_reflected)
            ##  Adjust the dimnames
            dimnames(.neg_part)$lag <- sprintf(
                                   "-%s",
                                   dimnames(.neg_part)$lag)
            dimnames(.neg_part)$levels <- dimnames(.pos_part)$levels
            dimnames(.neg_part)$pairs <- dimnames(.pos_part)$pairs
            ##  Update '..step_1'
            ..step_1 <- my_abind(
                .pos_part,
                .neg_part)
            kill(.pos_part, .neg_part)
        }
    }
    ##  Identify the y-limits to be used.
    if (look_up$heatmap_b_or_v == "b") {
        .y_values <- as.numeric(dimnames(..the_data)$bw_points)
        .ylim <- c(0, max(.y_values, .5) * 1.05)
    }
    if (look_up$heatmap_b_or_v == "m") {
        ##  TESTING, adjust to subset of the given data
        if (!is.null(look_up$heatmap_m_restrict)) {
            ..step_1 <- restrict_array(
                .arr= ..step_1,
                .restrict = look_up$heatmap_m_restrict,
                .drop = TRUE,
                .never_drop = .never_drop)
        }
        .y_values <- as.numeric(dimnames(..step_1)$m)
        .ylim <-
            if (!is.null(look_up$heatmap_m_restrict)) {
                range(.y_values) * 1.05
            } else
                c(0, max(.y_values) * 1.05)
    }
    if (look_up$heatmap_b_or_v == "v") {
        ##  Need to do tweak the names used for the 'levels', in order
        ##  for the adjustment later on to work as desired (need
        ##  numerical values).  Reminder: This adjustment is based on
        ##  the assumption that we are on the diagonal!
        .y_values <- vapply(
            X = strsplit(
                x = dimnames(..step_1)$levels,
                split = "_"),
            FUN = function(..x)
                pnorm(as.numeric(..x[1])),
            FUN.VALUE = numeric(1))
        dimnames(..step_1)$levels <- .y_values
        .ylim <- c(0,1)
    }
    ##  Create the data-frame needed for the heatmap-plot.
    .df <- reshape2::melt(data = ..step_1)
    ##  Avoid abysmally misleading plots, where everything seems to
    ##  behave in exactly the same manner due to too wide strips for
    ##  the few levels that are present.  Strategy: If the distances
    ##  between the levels are larger than the specified threshold,
    ##  then add a suitable number of "NA-levels" in between.
    ##  Moreover: If only one level has been investigated, then
    ##  squeeze it between two "NA-levels".
    if (look_up$heatmap_b_or_v %in% c("b", "v")) {
        .threshold_diff <- 0.02
        .the_diffs <- diff(.y_values)
        .add_NA <- any(.the_diffs > .threshold_diff,
                       length(.the_diffs) == 0)
        if (.add_NA) {
            ##  Need to add some new NA-values, not necessarily the same
            ##  number between each existing level.  Add two outer values
            ##  if only one y-value is present.
            .new_y_values <- c()
            if (length(.the_diffs) == 0) {
                .new_y_values  <- .y_values + c(-1, 1) * .threshold_diff
            } else {
                .tmp <- ceiling(.the_diffs / .threshold_diff)
                for (i in seq_along(.the_diffs)) {
                    if (.tmp[i] == 1)
                        next
                    .new_y_values <- c(
                        .new_y_values,
                        .y_values[i] + .the_diffs[i] / .tmp[i] * 1:(.tmp[i]-1))
                }
            }
            ##  Create a matrix of NA-values, convert to data-frame and
            ##  combine with existing data-frame '.df'.
            .new_dimnames <- local({
                y.part <-
                    if (look_up$heatmap_b_or_v == "b") {
                        list(bw_points = .new_y_values)
                    } else {
                        list(levels = .new_y_values)
                    }
                x.part <-
                    if (look_up$TCS_type == "C") {
                        list(lag = dimnames(..the_data)$lag)
                    } else {
                        list(omega = dimnames(..the_data)$omega)
                    }
                c(x.part, y.part)
            })
            .TMP <- matrix(data = NA_real_,
                           nrow = length(.new_dimnames[[1]]),
                           ncol = length(.new_dimnames[[2]]),
                           dimnames = .new_dimnames)
            .df <- rbind(.df, reshape2::melt(data = .TMP))
        }
    }
    if (look_up$heatmap_b_or_v == "m") {
        .add_NA <- TRUE
        .m_values <- as.numeric(dimnames(..step_1)$m)
        .half_lags <- c(.m_values - .5,
                        max(.m_values) + .5)
        .new_dimnames <-
            list(m = .half_lags,
                 omega = dimnames(..the_data)$omega)
        .TMP <- matrix(data = NA_real_,
                       nrow = length(.new_dimnames[[1]]),
                       ncol = length(.new_dimnames[[2]]),
                       dimnames = .new_dimnames)
        .df <- rbind(.df, reshape2::melt(data = .TMP))
    }
    ##  Need some extra tweaking of the plots that shows the local
    ##  Gaussian autocorrelations (along the diagonal) for the
    ##  collection of lag-values, i.e. add some NA-values to emphasise
    ##  that we do not look on something that is continuous in the
    ##  lag-argument!
    if (look_up$TCS_type == "C") {
        .lag_vec <- sort(as.numeric(dimnames(..step_1)$lag))
        .half_lags <- c(.lag_vec - .5,
                        max(.lag_vec) + .5)
        .new_dimnames <-
            if (look_up$heatmap_b_or_v == "b") {
                list(bw_points = unique(.df$bw_points),
                     lag = .half_lags)
            } else {
                list(levels = unique(.df$levels),
                     lag = .half_lags)
            }
        .TMP <- matrix(data = NA_real_,
                       nrow = length(.new_dimnames[[1]]),
                       ncol = length(.new_dimnames[[2]]),
                       dimnames = .new_dimnames)
        .df <- rbind(.df, reshape2::melt(data = .TMP))
    }
    ##  Create the plot.
    low_col <- "blue"
    high_col  <- "red"
    .plot <- ggplot(data=.df,
                    mapping = .aes_mapping) +
        geom_tile()  +
        scale_fill_gradient2(midpoint = .midpoint,
                             limits = .limits_gradient,
                             na.value = NA,
                             low = low_col,
                             high = high_col)  +
        ##  Adjust legend and labels.
        labs(fill = NULL) +
        xlab(.xlab_expression) +
        ylab(look_up$heatmap_b_or_v) +
        theme(axis.title.y = element_text(angle = 0))  +
        ## ## theme(axis.title.x = element_text(hjust = 0.97, vjust = 0, size = 20),
        ## ##       axis.title.y = element_text(vjust = 0.97, hjust = 0, size = 20, angle = 0))  +
        ##  Add the title
        ggtitle(label = .plot_title) +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 15,
                                        colour = "brown"))
    ##  Adjust the y-axis based on the type of plot, in particular,
    ##  for the inspection of the levels along the diagonal, the axis
    ##  should reflect that the value is in fact a percentage.  The
    ##  bandwidth investigation should have 0 as its lower limit.
    if (look_up$heatmap_b_or_v %in% c("b", "m")) {
        .plot <- .plot +
            ylim(.ylim)
    } else {
        .plot <- .plot +
            scale_y_continuous(
                limits = .ylim,
                labels = scales::percent)
    }
    ##  Add contour lines for the spectra case, but only when no
    ##  NA-values where added.  Perhaps also do this conditional on
    ##  some selected input parameter?  Moreover, add information
    ##  about the m-value.
    if (all(look_up$TCS_type == "S",
            ! .add_NA)) {
        .plot <- .plot +
            geom_contour(bins = 50,
                         colour = "brown",
                         lwd = .25,
                         na.rm = TRUE)
    }
    ##  Add details about content when required.  Reminder: It can for
    ##  some non-interactive cases be necessary to tweak the size of
    ##  the labels, and it can then be possible to experiment directly
    ##  with the content stored in the 'look_up'-attachment of the
    ##  plot.
    if (isFALSE(look_up$drop_annotation)) {
        if (!is.null(look_up$curlicues$text$annotated))
            .plot <- .plot +
                eval(look_up$curlicues$text$annotated)
    }
    ##  Add 'details' and 'curlicues' as attributes.
    attributes(.plot)$details <- look_up$details
    attributes(.plot)$curlicues <- look_up$curlicues
    ##  Return the plot to the workflow
    .plot
}
