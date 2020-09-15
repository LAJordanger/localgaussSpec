#' Plot a (subset of a) time series.
#'
#' @description This helper function creates a simple plot of the time
#'     series under investigation, and some minor details related to
#'     the points of investigation can in addition also be added.
#'
#' @param .env The environment containing the required details for the
#'     extraction and presentation of the sample of interest.
#'
#' @return A plot of the time series under investigation will be
#'     returned.
#'
#' @keywords internal

##  REMINDER: This is a "quick fix" that only deals with the
##  univariate time series that is needed in the scripts.  The main
##  aim is to remove internal functions from the scripts.  A proper
##  implementation in the shiny-application is a task for later on.

LG_plot_TS <- function(.env) {
    ##  Find the path to the time series of interest.
    .TS_path <- file.path(
        paste(.env$main_dir,
              collapse = .Platform$file.sep),
        .env$input$TS,
        LG_default$global["TS"])
    ##  Load the time series into the present environment under the
    ##  name '.TS_example'.
    LG_load(.file = .TS_path, .name = ".TS_example")
    kill(.TS_path)
    ##  Use the loaded object if the input-node '.TS_type_or.pn' is
    ##  equal to "original", otherwise extract the pseudo-normalised
    ## version from the attributes.
    if (.env$input$TS_type_or.pn != "original") {
        .TS_example <-
            attributes(.TS_example)$TS_for_analysis
    }
    ##  Extract the desired sample.
    .sample <- restrict_array(
        .arr = .TS_example,
        .restrict = .env$input$TS_restrict,
        .drop = TRUE,
        .never_drop = c("observations", "variables"),
        .keep_attributes = FALSE)
    ##  Create a data-frame to be used in the plot.
    .df <- data.frame(
        x = if (!is.null(.env$input$TS_restrict$observations)) {
                .env$input$TS_restrict$observations
            } else {
                seq_along(dimnames(.sample)$observations)
            },
        y = as.vector(.sample))
    ##  Extract the part of the curlicues relevant for the plot of the
    ##  time series itself.  These will be used if no user-specified
    ##  values are given in the non-interactive setting.
    TS_curlicues <- LG_default$curlicues$TS_plot
    ##  Extract the details related for the plot of the time series
    ##  itself, and use these to create the core for the plot.
    if (!is.null(.env$input_curlicues$TS_plot$line)) {
        .line <- LG_update_list(
            .new = .env$input_curlicues$TS_plot$line,
            .old = TS_curlicues$line)
    } else {
        .line <- TS_curlicues$line
    }
    ##  Create the plot of interest.
    .plot <- ggplot(
        data = .df,
        mapping = aes(x = x, y = y)) +
        geom_line(
            mapping = aes(x = x, y = y),
            alpha = .line$alpha,
            lwd = .line$lwd) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank())
    kill(.TS_example, .sample, .df, .line)
    ##  Add horizontal lines when relevant.
    if (!is.null(.env$input_curlicues$TS_plot$hline$yintercept)) {
        .hline <- LG_update_list(
            .new = .env$input_curlicues$TS_plot$hline,
            .old = TS_curlicues$hline)
        .plot <- .plot +
            do.call(what = "geom_hline",
                    args = .hline)
    }
    ##  Add a description when relevant.
    if (!is.null(.env$input_curlicues$TS_plot$description)) {
        .description <- LG_update_list(
            .new = .env$input_curlicues$TS_plot$description,
            .old = TS_curlicues$description)
        ##  Check if a restriction of the time series might require an
        ##  adjustment of the 'x'-position.  Only do this for those
        ##  cases where no user-data is given for this part.
        if (!is.null(.env$input$TS_restrict$observations)) {
            if (all(is.null(.env$input_curlicues$TS_plot$description$x),
                    .env$input$TS_restrict$observations[1] > 1))
                .description$x <- .env$input$TS_restrict$observations[1]
        }
        .plot <- .plot +
            do.call(what = "geom_text",
                    args = .description)
    }
    ##  Add an attribute with the formals of the function that
    ##  generated the samples (will be 'NULL' for real data).
    ##  Reminder: The same basic structure should be used here as the
    ##  one one used for the other plots.
    attributes(.plot)$details$fun_formals <-
                        formals(.env$info$TS_info$TS_data$fun)
    ##  Return the result to the workflow.
    .plot
}
