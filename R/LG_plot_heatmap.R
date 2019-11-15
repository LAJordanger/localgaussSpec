#' Heatmap-plots for LGSD-data
#'
#' @param ..env The environment containing the desired information
#'     from which the data should be extracted
#'
#' @param .look_up The usual list of details needed for the
#'     investigation.
#'
#' @return A heatmap-plot of either the estimated local Gaussian
#'     spectral density estimates for a combination of
#'     points-frequency, or points-lags.
#'
#' @keywords internal

LG_plot_heatmap <- function(..env, .look_up) {
    ##  Check if we have an investigation along the diagonal, and if
    ##  not return a message stating that the function has not been
    ##  implemented for that case.
    if (! .look_up$point_type_branch == "on_diag")
        return("Not implemented outside of diagonal")
    ##  Extract the data of interest, and specify assorted arguments
    ##  needed later on.
    if (.look_up$TCS_type == "C") {
        ##  The correlation case.
        ..the_data <- ..env[[.look_up$cache$.L_pairs]]$.data
        .never_drop = c("lag", "levels")
        .midpoint <- 0
        .aes_mapping <- aes(x = lag,
                            y = levels,
                            fill = value,
                            z = value)
        .limits_gradient <- c(-1,1)
        .xlab_expression <- "h"
        .plot_title <- "Heatmap for the local Gaussian autocorrelations (along diagonal)"
        .plot_stamp <- "rho[v]*(h)"
        .x_stamp <- 1 + diff(range(.look_up$lag_vec))/2
    } else {
        ##  The spectra case.
        ..the_data <- ..env[[.look_up$cache$.spectra_local]][[.look_up$.bm_CI_local_spectra]]
        .never_drop <- c("omega", "levels")
        .midpoint <- 1
        .aes_mapping <- aes(x = omega,
                            y = levels,
                            fill = value,
                            z = value)
        .limits_gradient <- NULL
        .xlab_expression <- expression(omega)  
        .plot_title <- sprintf("Heatmap for the m=%s local Gaussian autospectrum (along diagonal)",
                               as.numeric(.look_up$.bm_CI_local_spectra[2])-1)
        .plot_stamp <- sprintf("f[v]^%s*(omega)",
                               as.numeric(.look_up$.bm_CI_local_spectra[2])-1)
        .x_stamp <- diff(range(.look_up$omega_vec))/2
    }
    ## str(..the_data)



    ##  If the length of the 'content'-dimension is larger than one,
    ##  then it is first necessary to compute the mean of the
    ##  values. Note: For local Gaussian spectral densities, this is
    ##  the same as the result obtained when the means of the local
    ##  Gaussian autocorrelations are used in the computation.

    ##  Reminder: We might also like to loop over the individual
    ##  samples in a block, since that could give us a brief idea with
    ##  regard to the variability between different samples.  However,
    ##  this will require an additional loop-argument (that I would
    ##  like to include for other plots too), and it is then also
    ##  necessary to consider in more detail the limits that should be
    ##  used on the legend-colours.


    ## ## any(.look_up$is_block, .look_up$is_bootstrap)
    if (length(dimnames(..the_data)$content) > 1) {
        ##  In the bootstrap-case, the 'content'-dimension contains
        ##  the "orig"-values, but those should not be included if we
        ##  want a mean of the bootstrapped values.
        if (.look_up$is_bootstrap) {
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
        .restrict_list <- list(mean = "mean",
                               variable = "rho")
    } else {
        .restrict_list <- list(variable = "rho")
    }


    ##  Restrict the attention to one of the combinations
    ##  "omega"+"levels", "lag"+"levels".  I guess some test for which
    ##  part to restrict to would be nice at this point.
    ..step_1 <- restrict_array(
        .arr=..the_data,
        .restrict = .restrict_list,
        .drop = TRUE,
        .never_drop = .never_drop)
    ## str(..step_1)


    ##  Need to do tweak the names used for the 'levels', in order for
    ##  the adjustment later on to work as desired (need numerical
    ##  values).  Reminder: This adjustment is based on the assumption
    ##  that we are on the diagonal!

    .quantile_levels <- vapply(
        X = strsplit(
            x = dimnames(..step_1)$levels,
            split = "_"),
        FUN = function(..x)
            pnorm(as.numeric(..x[1])),
        FUN.VALUE = numeric(1))

    dimnames(..step_1)$levels <- .quantile_levels
    ##  Create the data-frame needed for the heatmap-plot.
    .df <- reshape2::melt(data = ..step_1)
    ##  str(.df)

    ##  Avoid abysmally misleading plots, where everything seems to
    ##  behave in exactly the same manner due to do wide strips for
    ##  the few levels that are present.  Strategy: If the distances
    ##  between the levels are larger than the specified threshold,
    ##  then add a suitable number of "NA-levels" in between.  Moreover:
    ##  If only one level has been investigated, then squeeze it
    ##  between two "NA-levels".
    #

    .threshold_diff <- 0.02
    .the_diffs <- diff(.quantile_levels)
    .add_NA <- any(.the_diffs > .threshold_diff,
                      length(.the_diffs) == 0)
    
    if (.add_NA) {
        ##  Need to add some NA-levels, not necessarily the same
        ##  number between each existing level.  Will add two outer
        ##  levels if only one level is present.
        .new_levels <- c()
        if (length(.the_diffs) == 0) {
            .new_levels  <- .quantile_levels + c(-1, 1) * .threshold_diff
        } else {
            .tmp <- ceiling(.the_diffs / .threshold_diff) 
            for (i in seq_along(.the_diffs)) {
                if (.tmp[i] == 1)
                    next
                .new_levels <- c(
                    .new_levels,
                    .quantile_levels[i] + .the_diffs[i] / .tmp[i] * 1:(.tmp[i]-1))
            }
        }
        ##  Create a matrix of NA-values, convert to data-frame and
        ##  combine with existing data-frame '.df'.
        .new_dimnames <-
            if(.look_up$TCS == "C") {
                list(levels = .new_levels,
                     lag = dimnames(..the_data)$lag)
            } else {
                list(levels = .new_levels,
                     omega = dimnames(..the_data)$omega)
            }
        .TMP <- matrix(data = NA_real_,
                       nrow = length(.new_levels),
                       ncol = length(.new_dimnames[[2]]),
                       dimnames = .new_dimnames)
        .df <- rbind(.df, reshape2::melt(data = .TMP))
    }



    

    ##  Create the plot.
    low_col <- "blue"; high_col  <- "red"
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
        ylab("v") +
        theme(axis.title.x = element_text(hjust = 0.97, vjust = 0, size = 20),
              axis.title.y = element_text(vjust = 0.97, hjust = 0, size = 20, angle = 0))  +
        ##  Add the title
        ggtitle(label = .plot_title) +
        theme(plot.title = element_text(hjust = 0.5,
                                        vjust = 0,
                                        size = 15,
                                        colour = "brown"))  +
        ##  Use standardised ylim, so it is easy to see from the
        ##  heatmap where our levels along the diagonal belongs.
        ylim(0,1)

    ##  Add contour lines for the spectra case, but only when no
    ##  NA-values where added.  Perhaps also do this conditional on
    ##  some selected input parameter?
    if (all(.look_up$TCS_type == "S",
            ! .add_NA)) {
        .plot <- .plot +
            geom_contour(bins = 50,
                         colour = "brown",
                         lwd = .25,
                         na.rm = TRUE) }

    
    ##  Add vertical lines for the correlations case, to emphasise
    ##  that we do not have a situation with a continuous values along
    ##  the lag-axis.
    if (.look_up$TCS_type == "C") {
        .plot <- .plot +
            geom_vline(
                xintercept = c(.look_up$lag_vec - 0.5,
                               max(.look_up$lag_vec) + 0.5),
                colour = "grey",
                lty = 2,
                size = .5, 
                alpha = .75)
    }
    ##  Return the plot with a suitable stamp on it.
    .plot + 
        annotate(geom = "text",
                 label = .plot_stamp,
                     x = .x_stamp,,
                     y = 0.5,
                     parse = TRUE,
                     size = 10,
                     alpha = 0.5,
                     colour = "brown",
                     hjust = 0.5,
                 vjust = 0.5)
}
