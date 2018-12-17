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
#' @param .data_list A list that with the data-frames that contains
#'     the desired information needed by the \code{data}-argument in
#'     the function \code{ggplot}.
#'
#' @param .lag A character-string, which when specified will add the
#'     lag that is under inspection.  The default value \code{NULL}
#'     turns of this part of the plot.
#'
#' @param .percentile A character-string, which when specified will
#'     add the percentile under investigation (in the upper right
#'     corner).  The default value \code{NULL} turns of this part of
#'     the plot.
#'
#' @param .select An argument from the vector \code{c("all", "canvas",
#'     "add")} that selects the desired plot.  The first alternative
#'     is the one selected by default if no specification is made by
#'     the user.  The two other alternatives are intended used when
#'     this function is called from \code{\link{LG_shiny}}, in order
#'     to reduce the amount of data that must be updated in order to
#'     create the interactive visualisation.
#' 
#' @param .canvas To be used when adding details to a plot produced
#'     earlier on.  This argument is needed when \code{LG_plot} is
#'     called from \code{\link{LG_shiny}}.  The default value
#'     \code{NULL} can be left unchanged in the general case.  Note
#'     that this argument will be ignored unless \code{.select} is
#'     given as \code{add}.
#'
#' @param .title The title to be used in the plot.
#'
#' @param .xlim Numeric vector of length two, giving the
#'     \code{xlim}-value to be used in \code{ggplot}.  The default
#'     value \code{c(0, 0.5)} has been selected for the visualisation
#'     of the local Gaussian spectra.  The value \code{NULL} can be
#'     used to allow \code{ggplot} to select reasonable values based
#'     on the values in \code{.data}.
#'
#' @param .ylim Numeric vector of length two, giving the
#'     \code{ylim}-value to be used in \code{ggplot}.  The default
#'     value \code{NULL} will allow \code{ggplot} to select reasonable
#'     values based on the values in \code{.data}.
#'
#' @param .aes_list A list containing the different \code{aes}-expressions
#'     needed for the plots.
#'
#' @return The first incarnation of this code will focus on the
#'     creation of plots depicting the local Gaussian spectral
#'     densities, with our without the information required to create
#'     the bootstrap based confidence intervals.  Other plots will
#'     probably be added later on.
#'
#' @keywords internal

LG_plot <- function(.data_list,
                    .lag = NULL,
                    .percentile = NULL,
                    .select = c("all", "canvas", "add"),
                    .canvas = NULL,
                    .title = NULL,
                    .xlim = c(0, 0.5),
                    .ylim = NULL,
                    .aes_list = NULL) {
    ###-------------------------------------------------------------------
    ##  Restrict '.select' to one value (needed when unspecified).
    .select = .select[1]
    ##  Check if '.data_list' contains a 'correlation'-node, since
    ##  that implies that a plot of these are of interest.
    if (!is.null(.data_list$correlation)) {
        .data <- .data_list$correlation
        ##  Initiate the framework
        .result <- ggplot(data = .data,
                          mapping = .aes_list$xy) +
            coord_cartesian(xlim = .xlim,
                            ylim = .ylim) +
            labs(title = .title) + 
            geom_hline(yintercept = 0,
                       linetype = 2,
                       col = "brown",
                       alpha = 0.8,
                       lwd = 0.5) +
            geom_vline(xintercept = 0,
                       linetype = 1,
                       col = "brown",
                       alpha = 0.25,
                       lwd = 0.5) +
            ##  Add percentile when required (only local cases).
            if (! is.null(.percentile))
                annotate(geom = "text",
                         x = Inf,
                         y = Inf,
                         size = 10,
                         label = .percentile,
                         col = "brown",
                         vjust = 1.2,
                         hjust = 1.2)
        ##  Add names to the layers
        names(.result$layers) <-
            c(".x_axis_line", ".y_axis_line",
              if (! is.null(.percentile))
                  ".annotate_percentile")
        ##  Add stuff based on the 'boxplot'-attribute.
        if (attributes(.data)$boxplot) {
            .result <- .result +
                geom_boxplot(aes(group = cut_width(x = lag,
                                                   width = 0.01,
                                                   boundary = 0.1)),
                             alpha = 0.5,
                             outlier.size = 0.5,
                             outlier.alpha = 0.5,
                             size = 0.5)
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
                         size = .25,
                         alpha = 0.8)
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
        return(.result)
    }
    ###-------------------------------------------------------------------
    ##  The spectra cases.
    ##  -------------------------------------------------------------------
    ##  Create a '.canvas' when necessary.  Reminder: Use the global
    ##  data as default here, since those always will be included.
    ##  Moreover, the The horizontal line added at this initial stage
    ##  refers to the expected result if white-noise is encountered.
    if (.select != "add") {
        .canvas <- ggplot(data = .data_list$global,
                          mapping = .aes_list$xy) +
            coord_cartesian(xlim = .xlim,
                            ylim = .ylim) +
            geom_hline(yintercept = .data_list$.white_noise,
                       linetype = 3) +
            labs(title = .title)
        names(.canvas$layers) <- "horizontal_line"
    }
    ###-------------------------------------------------------------------
    ##  Return '.canvas' when '.select' is given as "canvas".
    if (.select == "canvas")
        return(.canvas)
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
            if (! is.null(.aes_list$.geom_line_local))
                quote(geom_line(
                    mapping = .aes_list$.geom_line_local,
                    data = .data_list$local,
                    col = "blue",
                    lty = .data_list$.lty)),
        ##  A line dealing with the median in the `block`-case.
        .geom_line_global_me =
            if (! is.null(.aes_list$.geom_line_global_me))
                quote(geom_line(
                    mapping = .aes_list$.geom_line_global_me,
                    data = .data_list$global,
                    col = "brown",
                    lty = .data_list$.lty)),
        ##  A line giving the original global spectrum (when relevant).
        .geom_line_global =
            if (! is.null(.aes_list$.geom_line_global)) 
                quote(geom_line(
                    mapping = .aes_list$.geom_line_global,
                    data = .data_list$global,
                    col = "red",
                    lty = .data_list$.lty)),
        ##  Create quotes for the confidence bands too.
        .geom_ribbon_global =
            if (! is.null(.aes_list$.geom_ribbon_global)) 
                quote(geom_ribbon(
                    mapping = .aes_list$.geom_ribbon_global,
                    data = .data_list$global,
                    alpha = .3,
                    fill = "red")),
        ##
        .geom_ribbon_local =
            if (! is.null(.aes_list$.geom_ribbon_local))
                quote(geom_ribbon(
                    mapping = .aes_list$.geom_ribbon_local,
                    data = .data_list$local,
                    alpha = .3,
                    fill  = "blue")),
        ##  Add a quote for 'lag', when given.
        .annotate_lag =
            if (! is.null(.lag))
                quote(annotate(
                    geom = "text",
                    x = -Inf,
                    y = Inf,
                    size = 10,
                    label = .lag,
                    col = "brown",
                    vjust = 1.3,
                    hjust = -0.4)),
        ##  And a quote to add `.percentile`, when given.
        .annotate_percentile = 
            if (! is.null(.percentile)) 
                quote(annotate(
                    geom = "text",
                    x = Inf,
                    y = Inf,
                    size = 10,
                    label = .percentile,
                    col = "brown",
                    vjust = 1.2,
                    hjust = 1.2)))
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
    ###-------------------------------------------------------------------
    ##  Return the plot to the workflow.
    .result
}
