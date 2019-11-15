#' Complex-valued-plots for LGSD-data
#'
#' @param ..env The environment containing the desired information
#'     from which the data should be extracted
#'
#' @param .look_up The usual list of details needed for the
#'     investigation.
#'
#' @param .selection Specifies the type of plot to be used.  This
#'     should be a temporary argument, as it seems natural that it
#'     should be defined as a part of the shiny-interface.
#'
#' @return A heatmap-plot of either the estimated local Gaussian
#'     spectral density estimates for a combination of
#'     points-frequency, or points-lags.
#'
#' @keywords internal

LG_plot_complex <- function(..env,
                            .look_up,
                            .selection = c("polar", "Cartesian", "zoom")) {
    ##  Restrict attention to the cases having complex-valued data.
    if (! any(.look_up$is_cross_pair,
              all(.look_up$is_off_diagonal,
                  .look_up$is_auto_pair)))
        return("We need complex-valued data for this plot to be created")
    ##  Restrict to default value for '.selection'.
    .selection <- .selection[1]
    ##  Extract the desired chunk of data.
    .data <- ..env[[cache$.spectra_local]][[.look_up$.bm_CI_local_spectra]]
    ##  For this plot, we want (for a given point) to loop over the
    ##  frequency omega, so we need to restrict with that in mind.
    #####
    ##  Reminder: Remove the line below when the shiny-interface has
    ##  been updated, and this is taken care of in 'LG_lookup'.
    .look_up$omega_complex_investigation <- dimnames(.data)$omega[15]
    .restrict_list <- list(
        levels = .look_up$levels_point,
        omega = .look_up$omega_complex_investigation)
    .data2 <- restrict_array(
        .arr = .data,
        .restrict = .restrict_list,
        .drop = TRUE,
        .never_drop = "content")
    kill(.restrict_list, .data)
    ## Create a data-frame with the real and imaginary parts of
    ## interest, i.e. positive "Co" and negative "Quad".
    points_df <- data.frame(
        x = as.vector(restrict_array(
            .arr = .data2,
            .restrict = list(S_type = "Co"))),
        y = -1 * as.vector(restrict_array(
                     .arr = .data2,
                     .restrict = list(S_type = "Quad"))))
    ##  Extract the "amplitude"- and "phase"-vectors.
    .amplitude <- as.vector(restrict_array(
        .arr = .data2,
        .restrict = list(S_type = "amplitude")))
    .phase <- as.vector(restrict_array(
        .arr = .data2,
        .restrict = list(S_type = "phase")))
    kill(.data2)
    ##  We will need limits for a rectangular plot, since a circle
    ##  will be added.  Reminder: If a plot is to be dynamic with
    ##  regard to the lag-dimension, then '..limits' should also take
    ##  that dimension into account.
    ..limits <- c(-1, 1) * max(.amplitude)
    ##  We need a minor helper function for the creation of imaginary
    ##  units on the second axis of the plot.
    ..imaginary <- function(x)
        paste(x, "i", sep = "")
    ##  Create a basic rectangular plot for the complex valued local
    ##  Gaussian spectral densities at the given combination of
    ##  truncation level, point and frequency (including information
    ##  about the numerical convergence).
    .lag_omega_info <- paste(
        gsub(pattern = " = ",
             replacement = " == ",
             x =.look_up$details$.selected_lag),
        "~~~omega == ",
        formatC(x = .look_up$omega_complex_investigation,
                digits = 3),
        sep = "")
    .canvas <- ggplot(
        data = points_df,
        mapping = aes(x = x,
                      y = y)) +
        geom_point(
            size = 1,
            alpha = 0.5,
            na.rm = TRUE) +
        coord_fixed() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        annotate(geom = "text",
                 x = -Inf,
                 y = -Inf,
                 size = 5,
                 label = .look_up$details$text$trust_the_result,
                 col = ifelse(
                     test = attributes(.look_up$details$text$trust_the_result)$convergence,
                     yes  = "darkgreen",
                     no   = "red"),
                 vjust = -.5,
                 hjust = -0.1) + 
        annotate(geom = "text",
                 x = Inf,
                 y = Inf,
                 size = 5,
                 label = .look_up$details$.selected_percentile,
                 col = "brown",
                 vjust = 1,
                 hjust = 1) + 
        annotate(geom = "text",
                 x = -Inf,
                 y = Inf,
                 size = 5,
                 label = .lag_omega_info,
                 parse = TRUE,
                 col = "brown",
                 vjust = 1,
                 hjust = -0.1)
    kill(.lag_omega_info)
    ##  When required, return the zoom version.
    if (.selection == "zoom") {
        .canvas_zoom <- .canvas +
            scale_y_continuous(
                labels = ..imaginary)
        return(.canvas_zoom)
    }
    ##  Update '.canvas' with new limits (with complex valued y-axis),
    ##  and add coordinate-axes.
    .canvas <- .canvas +
        scale_x_continuous(
            limits = ..limits) +
        scale_y_continuous(
            limits = ..limits,
            labels = ..imaginary) +
        geom_hline(
            yintercept = 0,
            col = "black",
            alpha = 0.25,
            size = .75) +
        geom_vline(
            xintercept = 0,
            col = "black",
            alpha = 0.25,
            size = .75)
    kill(..limits, ..imaginary)
    ##  We want to add lines (for the selected confidence interval)
    ##  based on the median/quantiles of the different
    ##  spectral-values.
    .probs <- local({
        if (.look_up$details$CI_percentage == "min_max") {
            c(0, 0.5, 1)
        } else {
            .CI_percentage <- .look_up$details$CI_percentage / 100
            .lower <- (1-.CI_percentage)/2
            .upper <- .CI_percentage + .lower
            c(.lower, 0.5, .upper)
        }
    })
    ..x_lines <- quantile(x = points_df$x,
                          probs = .probs)
    ..y_lines <- quantile(x = points_df$y,
                          probs = .probs)
    ..radii <- quantile(x = .amplitude,
                        probs = .probs)
    ..phases <- quantile(x = .phase,
                         probs = .probs)
    kill(points_df, .probs, .phase)
    ##  Specify the type, size, colour and alpha-values to be used for
    ##  these lines.
    .line_type <- c(3, 2, 3)
    .line_size <- 0.3
    .line_col <- c("blue", "red", "blue")
    .line_alpha <- 0.5
    ##  When required, return the Cartesion version.
    if (.selection == "Cartesian") {
        .canvas_Cartesian <- .canvas +
            geom_vline(
                xintercept = ..x_lines,
                linetype = .line_type,
                col = .line_col,
                size = .line_size,
                alpha = .line_alpha) +
            geom_hline(
                yintercept = ..y_lines,
                linetype = .line_type,
                col = .line_col,
                size = .line_size,
                alpha = .line_alpha) +
            annotate(
                geom = "text",
                x = - Inf,
                y = Inf,
                label = paste(
                    "\"Cartesian, \"",
                    "z == x + i*y",
                    sep = "*"),
                col = "brown",
                parse = TRUE,
                hjust = -0.1,
                vjust = 2,
                size = 5)
        return(.canvas_Cartesian)
    }
    ##  When required, return the Polar version.
    if (.selection == "polar") {
        ..frequencies <-
            seq(from = 0,
                to = 2 * pi,
                length.out = 200)
        ..amplitude <-
            seq(from = 0,
                to = 2 * max(.amplitude),
                length.out = 200)
        kill(.amplitude)
        .canvas_polar <- .canvas
        for (.i in seq_along(..radii)) {
            .canvas_polar <- .canvas_polar +
                annotate(
                    geom = "path",
                    x = ..radii[.i] * cos(..frequencies),
                    y = ..radii[.i] * sin(..frequencies),
                    linetype = .line_type[.i],
                    col = .line_col[.i],
                    size = .line_size,
                    alpha = .line_alpha,
                    na.rm = TRUE) +
                annotate(
                    geom = "path",
                    x = ..amplitude * cos(..phases[.i]),
                    y = ..amplitude * sin(..phases[.i]),
                    linetype = .line_type[.i],
                    col = .line_col[.i],
                    size = .line_size,
                    alpha = .line_alpha,
                    na.rm = TRUE)
        }
        kill(.i, ..amplitude, ..frequencies, ..phases, ..radii,
             .line_alpha, .line_size, .line_col, .line_type, .canvas)
        .canvas_polar <- .canvas_polar +
            annotate(
                geom = "text",
                x = -Inf,
                y = Inf,
                label = paste(
                    "\"Polar, \"",
                    "z == re^{i*theta}",
                    sep = "*"),
                col = "brown",
                parse = TRUE,
                hjust = -0.2,
                vjust = 2,
                size = 5)
        return(.canvas_polar)
    }
}
