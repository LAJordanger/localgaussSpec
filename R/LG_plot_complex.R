#' Complex-valued-plots for the local Gaussian spectra
#'
#' @description This internal function plots the complex-valued
#'     spectra (for a given frequency) in the complex plane.
#'
#' @param ..env The environment containing the desired information
#'     from which the data should be extracted
#'
#' @param look_up The environment containing the details needed for
#'     the investigation.
#'
#' @return A plot is returned to the workflow.  This plot shows the
#'     complex-valued estimates of the local Gaussian spectra for the
#'     given combination of frequency, point, truncation level and
#'     bandwidth.  The resulting (loud of )points can be presented in
#'     a zoomed in version, or they can be inspected in a zoomed out
#'     version with confidence intervals that can be based either on a
#'     Cartesian or polar representation of the complex numbers.
#'
#' @keywords internal

LG_plot_complex <- function(..env,
                            look_up) {
    ##  Restrict attention to the cases having complex-valued data.
    if (! any(look_up$is_cross_pair,
              all(look_up$is_off_diagonal,
                  look_up$is_auto_pair)))
        return("We need complex-valued data for this plot to be created")
    ##  Extract the desired chunk of data.
    .data <- ..env[[look_up$cache$spectra_local]][[look_up$.bm_CI_local_spectra]]
    ##  For this plot, we want (for a given point) to loop over the
    ##  frequency omega, so we need to restrict with that in mind.
    .restrict_list <- list(
        levels = look_up$levels_point,
        omega = as.character(look_up$complex_frequency))
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
    ##  Create the basic plot for the complex valued local Gaussian
    ##  spectral densities at the given combination of truncation
    ##  level, point and frequency
    .plot <- ggplot(
        data = points_df,
        mapping = aes(x = x,
                      y = y)) +
        geom_point(
            size = .3,
            alpha = 0.25,
            na.rm = TRUE) +
        coord_fixed() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank())
    ##  We need a minor helper function for the creation of imaginary
    ##  units on the second axis of the plot.
    ..imaginary <- function(x)
        paste(x, "i", sep = "")
    ##  Get hold of the quoted annotated values, and update them to
    ##  suit the present case of investigation.
    .annotated <- look_up$curlicues$text$annotated
    ##  The addition of an extra line of information for the Cartesian
    ##  and polar cases implies that the position of the plot-stamp
    ##  should be adjusted too.  This requires an adjustment in the
    ##  first component of the 'vjust'-part of '.annotated'.
    .annotated$vjust[1]  <- 1.5 * .annotated$vjust[1]
    ##  Reduce the text-size to half of the original.  Reminder: This
    ##  is a quick-fix that should be resolved in some other way.
    .annotated$size <- 0.5 * .annotated$size
    ##  It is also necessary to adjust the x-position of the
    ##  annotations, and the updates depends in this case on whether
    ##  or not a zoomed version of the plot is investigated.  The key
    ##  idea is that all the plots should be quadratic and having
    ##  fixed coordinates.  The quadratic requirement is due for the
    ##  need for circles to be added for the polar case.
    if (look_up$complex_c_or_p_or_z == "z")  {
        ##  The zoom case: Find the limits and use them to update the
        ##  x-values of the annotations.
        ..range <- max(diff(range(points_df$x)),
                       diff(range(points_df$y)))
        ..x_limit <- mean(points_df$x) +
            c(-1, 1) * 0.5 * ..range
        ..y_limit <- mean(points_df$y) +
            c(-1, 1) * 0.5 * ..range
        .annotated$x <- local({
            .scaling <-
                (.annotated$x - min(.annotated$x)) / diff(range(.annotated$x))
            ..x_limit[1] + .scaling * diff(..x_limit)
        })
        ##  Adjust the limits of '.plot' and add imaginary units on
        ##  the second axis.
        .plot <- .plot   +
            scale_x_continuous(
                limits = ..x_limit) +
            scale_y_continuous(
                limits = ..y_limit,
                labels = ..imaginary)
        kill(.range, ..x_limit, ..y_limit)
    } else {
        ##  The Cartesian and polar cases: Find the limits and use
        ##  them to update the x-values of the annotations.
        ..limits <-
            if (look_up$complex_c_or_p_or_z == "z") {
                range(points_df$x) 
            } else {
                c(-1, 1) * max(.amplitude)
            }
        .annotated$x <- local({
            .scaling <-
                (.annotated$x - min(.annotated$x)) / diff(range(.annotated$x))
            ..limits[1] + .scaling * diff(..limits)
        })
        ##  Adjust the limits of '.plot' and add imaginary units on
        ##  the second axis.  In addition, add lines to represent the
        ##  x- and y-axes.
        .plot <- .plot +
            scale_x_continuous(
                limits = ..limits) +
            scale_y_continuous(
                limits = ..limits, labels = ..imaginary) +
            geom_hline(
                yintercept = 0,
                col = "black",
                alpha = 0.25,
                size = .5) +
            geom_vline(
                xintercept = 0,
                col = "black",
                alpha = 0.25,
                size = .5)
        kill (..limits)
    }
    kill(..imaginary)
    ##  Find the probabilities that should be used in order to decide
    ## upon the lines/circles that will indicate the mean and the
    ## confidence intervals for the Cartesian and polar cases.
    .probs <- local({
        if (look_up$details$CI_percentage == "min_max") {
            c(0, 0.5, 1)
        } else {
            .CI_percentage <- look_up$details$CI_percentage / 100
            .lower <- (1-.CI_percentage)/2
            .upper <- .CI_percentage + .lower
            c(.lower, 0.5, .upper)
        }
    })
    ##  Specify the type, size, colour and alpha-values to be used for
    ##  these lines/circles.
    .line_type <- c(3, 2, 3)
    .line_size <- 0.3
    .line_col <- c("blue", "red", "blue")
    .line_alpha <- 0.5
    ##  Add the confidence lines for the Cartesian case.
    if (look_up$complex_c_or_p_or_z == "c")  {
        ..x_lines <- quantile(x = points_df$x,
                              probs = .probs)
        ..y_lines <- quantile(x = points_df$y,
                              probs = .probs)
        .plot <- .plot +
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
                alpha = .line_alpha)
        kill(..x_lines, ..y_lines)
    }
    ##  Add the confidence lines/circles for the polar case.
    if (look_up$complex_c_or_p_or_z == "p")  {
        ..radii <- quantile(x = .amplitude,
                            probs = .probs)
        ..phases <- quantile(x = .phase,
                             probs = .probs)
        ..frequencies <-
            seq(from = 0,
                to = 2 * pi,
                length.out = 200)
        ..amplitude <-
            seq(from = 0,
                to = 2 * max(.amplitude),
                length.out = 200)
        for (.i in seq_along(..radii)) {
            .plot <- .plot +
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
        kill(.i, ..frequencies, ..phases, ..radii)
    }
    kill(points_df, .probs, .amplitude, .phase,
         .line_alpha, .line_size, .line_col, .line_type)
    ##  Add the annotations to the plot.
    .plot <- .plot +
        eval(.annotated)
    ##  Return the annotated plot to the workflow
    .plot
}
