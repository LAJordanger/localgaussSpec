################################################################################
#'
#' Create plots to inspect different aspects of Local Gaussian
#' Spectral Densities.
#'
#' To get a joint setup for plots used in interactive sessions with
#' \code{LG_shiny} and plots used in a more static setting, the
#' intention of this function is to collect the different plots in one
#' main function.
#'
#' In most cases, the data will be extracted from one or more files
#' into an array (due to memory restraints we might need a solution
#' that can do this in several parts) and based on a collection of
#' inputs we will then need to extract a relevant part into a
#' data-frame in order for ggplot to have the desired format to work
#' upon.  In this first iteration of the code, I think I will restrict
#' attention to the case where we assume the data already has been
#' extracted.
#'
#' @param .data A data-frame containing the desired information needed
#'     by the argument \code{data} in the function \code{ggplot}.
#'
#' @param .global A list with the two components \code{.block} and
#'     \code{.data}, that can be used to add information about the
#'     ordinary spectral density.  The default value \code{NULL} turns
#'     of this part of the plot.
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
#'     of the local Gaussina spectra.  The value \code{NULL} can be
#'     used to allow \code{ggplot} to select reasonable values based
#'     on the values in \code{.data}.
#'
#' @param .ylim Numeric vector of length two, giving the
#'     \code{ylim}-value to be used in \code{ggplot}.  The default
#'     value \code{NULL} will allow \code{ggplot} to select reasonable
#'     values based on the values in \code{.data}.
#' 
#' @param .aes_xy A call of \code{aes} that will be used \code{ggplot}
#'     requires an \code{aes} specifying \code{x} and \code{y}.  The
#'     default value \code{NULL} will trigger an error if unspecified.
#'
#' @param .aes_min_max A quoted expression to be used when calling
#'     \code{ggplot} with a mapping that requires us to give
#'     \code{aes} with a specification of \code{min} and \code{max}.
#'     The default value \code{NULL} will stop the execution of any
#'     layers depending on \code{min} and \code{max}.
#'
#' @param .sanity_checks Logic value, default \code{TRUE}, that
#'     decides whether or not a sequence of sanity-checks will be run
#'     on the arguments.  When used in \code{LG_shiny}, it's
#'     preferable to be able to turn of these tests.
#'
#' @return The first incarnation of this code will focus on the
#'     creation of plots depicting the local Gaussian spectral
#'     densities, with our without the information required to create
#'     the bootstrap based confidence intervals.  Other plots will
#'     probably be added later on.
#'
#' @export


LG_plot <- function(
    .data,
    .global = NULL,
    .lag = NULL,
    .percentile = NULL,
    .select = c("all", "canvas", "add"),
    .canvas = NULL,
    .title = NULL,
    .xlim = c(0, 0.5),
    .ylim = NULL,
    .aes_xy = NULL,
    .aes_min_max = NULL,
    .sanity_checks = TRUE) {
###-------------------------------------------------------------------
    ##  Restrict '.select' to one value (needed when unspecified).
    .select = .select[1]
    ##  Perform the overall sanity check that must be satisfied used
    ##  regardless of the value of `.sanity_checks`.
    if (all(is.null(.data),
              is.null(.global$.data)))
        error(.argument = c(".data", ".global"),
              fun_stack = FALSE,
              c("At least one of the arguments must contain",
                "some data..."))
    if (all(is.null(.data),
            is.null(.canvas),
            .select != "add"))
        error(.argument = c(".data", ".canvas", ".select"),
              fun_stack = FALSE,
              paste("A canvas must be specified when ",
                    sQuote(".data"),
                    " is given as ",
                    sQuote("NULL"),
                    ", since it's necessary to have something to add ",
                    "the global information upon.",
                    sep = ""))
###-------------------------------------------------------------------
    ##  Perform sanity-checks of the arguments, when required.
    if (.sanity_checks) {
        ##  Find the names we must compare against.
        .names <-
            if (is.null(.data)) {
                names(.global$.data)
            } else
                names(.data)
###-------------------------------------------------------------------
        ##  Check that something is given to the argument '.aes_xy'.
        if (is.null(.aes_xy))
            error(.argument = ".aes_xy",
                  "No value detected for this mandatory argument.")
###-------------------------------------------------------------------
        ##  Check that the names of '.aes_xy' are correct.
        .aes_names <- names(.aes_xy)
        ##---
        if (! all(.aes_names %in% c("x", "y")))
            error(.argument = ".aes_xy",
                  fun_stack = FALSE,
                  c("The names of",
                  sQuote(".aes_xy"),
                  "must contain",
                  sQuote("x"),
                  "and",
                  sQuote("y")))
###-------------------------------------------------------------------
        ##  Check that '.aes_xy' points to something in '.data'
        .aes_content <- unlist(lapply(.aes_xy, as.character))
        ##---
        if (! all(.aes_content %in% .names))
            error(.argument = ".aes_xy",
                  fun_stack = FALSE,
                  c("Mismatch between",
                    sQuote(".aes_xy"),
                    "and the content of",
                    sQuote(".data")))
###-------------------------------------------------------------------
        ##  When given, check the names and content of '.aes_min_max'.
        if (! is.null(.aes_min_max)) {
###-------------------------------------------------------------------
            ##  Check that the names of '.aes_min_max' are correct.
            .aes_names <- names(.aes_min_max)
            ##---
            if (! all(.aes_names %in% c("ymin", "ymax")))
                error(.argument = ".aes_names",
                      fun_stack = FALSE,
                      c("The names of",
                        sQuote(".aes_names"),
                        "must contain",
                        sQuote("ymin"),
                        "and",
                        sQuote("ymax")))
###-------------------------------------------------------------------
            ##  Check that '.aes_min_max' points to something in '.data'
            .aes_content <- unlist(lapply(.aes_min_max, as.character))
            ##---
            if (! all(.aes_content %in% .names))
                error(.argument = ".aes_min_max",
                      fun_stack = FALSE,
                      c("Mismatch between",
                        sQuote(".aes_min_max"),
                        "and the content of",
                        sQuote(".data")))
        } #  Check of ".aes_min_max' finished.
        kill(.aes_names, .aes_content)
###-------------------------------------------------------------------
        ##  Check that '.select' given as "add" has something to add on.
        if (.select == "add")
            if (is.null(.canvas))
                error(.argument = c(".canvas", ".select"),
                      fun_stack = FALSE,
                      c("A",
                        sQuote(".canvas"),
                        "must be given when",
                        sQuote(".select"),
                        "equals",
                        sQuote("add")))
    } #  Sanity-checks finished.
###-------------------------------------------------------------------
    ##  Check if '.data' contains the attribute 'lag_data', since that
    ##  indicates an investigation of the estimated local Gaussian
    ##  correlations.

    
    .approx_case <- ! is.null(attributes(.data)$lag_data)
    if (.approx_case) {
        ##  Initiate the framework
        .result <- ggplot(data = .data,
                          mapping = .aes_xy) +
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
            annotate(geom = "text",
                     x = Inf,
                     y = Inf,
                     size = 10,
                     label = .percentile,
                     col = "brown",
                     vjust = 1.2,
                     hjust = 1.2)
        ##  Add names to the layers
        names(.result$layers) <- c(".x_axis_line", ".y_axis_line", ".annotate_percentile")
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
    return(.result)
    }
    
###-------------------------------------------------------------------
    ##  The other boot_spectra case.
###-------------------------------------------------------------------
    ##  Create a '.canvas' when necessary.
    if (.select != "add") {
        .canvas <- ggplot(data = .data,
                          mapping = .aes_xy) +
            coord_cartesian(xlim = .xlim,
                            ylim = .ylim) +
            geom_hline(yintercept = 1,
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
    ##  Some of these might become `NULL`, which makes `ggplot`
    ##  complain - but that is resolved later on.
#####  TASK: Remove the need for this ad hoc solution, by
#####  updating/extending the interface.
    if (.aes_xy$y != as.name("orig")) {
        .aes_xy <- aes(x = omega, y = block_median)
        .lty = 2
    } else
        .lty = 1
    .layers <- list(
        ##  A quotes for the selected "central" local line.
        .geom_line_local =
            if (! is.null(.data))
                quote(geom_line(
                    mapping = .aes_xy,
                    data = .data,
                    col = "blue",
                    lty = .lty)),
        ##  A line dealing with the median in the `block`-case.
        .geom_line_global_me =
            if (! is.null(.global))
                if (.global$.block)
                    quote(geom_line(
                        mapping = aes(x = omega, y = orig_median),
                        data = .global$.data,
                        col = "brown",
                        lty = 2)),
        ##  A line giving the original global spectrum (when relevant).
        .geom_line_global =
            if (! is.null(.global)) 
                if (! .global$.block) {
                    .aes_global <- eval(bquote(aes(
                        x = omega,
                        y = .(as.name(LG_default$sample.prefix)))))
                    quote(geom_line(
                        mapping = .aes_global,
                        data = .global$.data,
                        col = "red",
                        lty = 1))
                },
        ##  Create quotes for the confidence bands too.
        .geom_ribbon_global =
            if (! is.null(.global)) 
                quote(geom_ribbon(
                    mapping = .aes_min_max,
                    data = .global$.data,
                    alpha = .3,
                    fill = "red")),
        ##
        .geom_ribbon_local =
            if (! is.null(.aes_min_max))
                if (! is.null(.data))
                    quote(geom_ribbon(
                        mapping = .aes_min_max,
                        data = .data,
                        alpha = .3,
                        fill  = "blue")),
        ##  Add a quote ofr 'lag', when given.
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
    ##  Remove the ribbon-cases when not needed.
    if (is.null(.aes_min_max)) {
        .drop <- stringr::str_detect(
                              string = names(.layers),
                              pattern = "ribbon")
        .layers[.drop] <- NULL
    }
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
    ##  Return the plot to the workflow.
    .result
}

## .tmp + annotate(
##            geom = "text",
##            x = -Inf,
##            y = Inf,
##            size = 10,
##            label = .lag,
##            col = "brown",
##            vjust = 1.3,
##            hjust = -0.4)
