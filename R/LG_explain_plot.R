#' Explain the content of a plot.
#'
#' This function gives information about the different plots created
#' by this package.  It is used internally in the
#' \code{shiny}-application, and it can also be used when a plot is
#' desired included in an article/presentation.  In the latter case
#' the plot must first be saved to an object, and that object can then
#' be given to this function in the argument \code{.plot_details}.
#' Note that the returned text is based on the information that the
#' saved plot-object has in the attribute named \code{details}, and it
#' is thus of course possible to create customised presentations
#' directly based on the information found there.
#' 
#' @param .plot_details This can either be a saved plot created by this
#'     package, or it can be the details-list directly.  (The latter
#'     alternative is included to simplify the use of this function
#'     inside of the internal \code{shiny}-application.)
#'
#' @param .mode Specify the mode that the result will be returned in.
#'     Two modes are supported, "markdown" and "latex".  The default
#'     value \code{NULL} will trigger the selection of "latex" if
#'     \code{.plot_details} is given as a saved \code{ggplot2}-object,
#'     whereas "markdown" will be selected when \code{.plot_details}
#'     is given as a list.
#'
#' @param .digits_for_points An integer that specifies the number of
#'     decimals to include when presenting a point.  The default value
#'     is \code{2}.
#'
#' @param .digits_for_percentiles An integer that specifies the number
#'     of decimals to include when presenting the percentiles (of the
#'     standard normal distribution) that corresponds to the point.
#'     The default value is \code{0}.
#'
#' @return The returned result will be a description of the plot in
#'     \code{.plot_details}, in the mode specified by \code{.mode}.
#'
#' @export


LG_explain_plot <- function(.plot_details,
                            .mode = NULL,
                            .digits_for_points = 2,
                            .digits_for_percentiles = 0) {
###-------------------------------------------------------------------
    ##  Check the properties of '.plot_details', and extract the
    ##  details-attribute if necessary.
    .plot_argument <- "ggplot" %in% class(.plot_details)
    if (.plot_argument) {
        details <- attributes(.plot_details)$details
    } else
        details <- .plot_details
    kill(.plot_details)
###-------------------------------------------------------------------
    ##  Check if the mode must be updated (based on '.plot_details')
    ##  or if it is given check that it is a valid alternative.
    if (is.null(.mode)) {
        .mode <- if (.plot_argument) {
                     "latex"
                 } else
                     "markdown"
    } else {
        ##  Check the validity of mode.
        if (! .mode %in% c("markdown", "latex"))
            error(.argument = ".mode",
                  "The mode must be one of: NULL, markdown, latex.")
    }
    kill(.plot_argument)
######################################################################
    ##  Reminder of temporary solution for the case not implemented
    if (details$TCS_type == "T")
        return("Sorry: This has not been implemented yet")
######################################################################
###-------------------------------------------------------------------
    ##  Create helper functions to take care of the selected values.
    ##  These must be adjusted depending on the mode.
    if (.mode == "markdown") {
        ##  Helper functions for the markdown-syntax.
        .italics <- function(.text)
            paste("_", .text, "_", sep = "")
        .bold <- function(.text)
            paste("__", .text, "__", sep = "")
        .item <- function(.text, .head = NULL, n = 1) {
            if (is.null(.text))
                return(NULL)
            .indentation <- paste(
                paste(rep(x = " ", times = 4*(n-1)),
                      collapse = ""),
                ifelse(test = {n == 1},
                       yes  = "* ",
                       no   = "+ "),
                collapse = "")
            paste(c(.indentation,
                    if (! is.null(.head))
                        c(.bold(.head),
                          " "),
                    .text),
                  collapse = "")
        }
        .header <- function(.text = "", n) 
            paste(
                paste(rep("#", n), collapse = ""),
                " ",
                .text,
                sep = "")
        .percent <- function(.text)
            paste(.text,
                  "%",
                  sep = "")
        .text_adjust <- function(.text) .text
    } else {
        ##  Helper functions for the latex-syntax.
        .italics <- function(.text)
            paste("\\textit{",
                  .text,
                  "}",
                  sep = "")
        .bold <- function(.text)
            paste("\\textbf{",
                  .text,
                  "}",
                  sep = "")
        .item <- function(.text, .head = NULL, n=1) {
            if (is.null(.text))
                return(NULL)
            paste(c("\\item  ",
                    if (! is.null(.head))
                        .bold(.head),
                    .text),
                  collapse = " ")
        }
        ##  A very primitve solution at the moment for this part.
        .header <- function(.text = "" ,n)
            paste("{\\Large ",
                  .text,
                  "} ",
                  sep = "")
        .percent <- function(.text)
            paste(.text,
                  "\\%",
                  sep = "")
        .text_adjust <- function(.text) {
            ##  Adjustment for '\'-character.
            .text <- gsub(pattern = "\\",
                          replacement = "\\\\",
                          x = .text,
                          fixed = TRUE)
            ##  Adjustment for special characters.
            .special_characters <- c("_", "^", "$", "{", "}", "%", "&", "#")
            for (.char in .special_characters)
                .text <- gsub(pattern = .char,
                              replacement = paste(
                                  "\\",
                                  .char,
                                  sep = ""),
                              x = .text,
                              fixed = TRUE)
            ##  Adjustment for '~'-character.
            .text <- gsub(pattern = "~",
                          replacement = "\\~{}",
                          x = .text,
                          fixed = TRUE)
            ##  Return the revised text.
            .text
        }
    }
    ##  An additional helper function to take care of the itemising.
    .itemising <- function(...) {
        ##  Capture evaluated versions of the dotsMethod (anticipated
        ##  here to be be character-vectors inside of '.item').
        .the_dots <- lapply(X = pryr::dots(...),
                            FUN = eval,
                            envir = parent.frame())
        ##  Return a vector with the code of interest.
        c(if (.mode == "latex")
              "\\begin{itemize}  ",
          unlist(.the_dots),
          if (.mode == "latex")
              "\\end{itemize}  ")
    }
###-------------------------------------------------------------------
    ##  Fix the details needed for the points and percentiles, with
    ##  the desired level of digits.
    .points <- list(
        coordinates = as.character(
            round(x = details$.point_coord,
                  digits = .digits_for_points)),
        percentiles = paste(
            .percent(round(
                x = pnorm(q = details$.point_coord) * 100,
                digits = .digits_for_percentiles)),
            sep = ""))
    kill(.digits_for_percentiles, .digits_for_points)
###-------------------------------------------------------------------
    ##  Note that some of the details already has been stored in a
    ##  somewhat crude form in 'details$text', but that the present
    ##  function nevertheless will produce a full collection.  The
    ##  idea is to produce an output based on an itemised
    ##  presentation, so the pieces will be configured with that in
    ##  mind.
###-------------------------------------------------------------------
    ##  Collect information about source, content, computations,
    ##  confidence intervals and so on (depending on what has been
    ##  selected.)
    plot_info <- list()
    plot_info$Source <- paste(
        ifelse(test = details$is_block,
               yes  = paste(details$nr_simulated_samples,
                            " independent ",
                            .italics(.text_adjust(details$TS_key)),
                            "-simulated samples",
                            sep = ""),
               no   = "Real data"),
        " of length ",
        details$N,
        ", ",
        ifelse(test = details$.nr_variables == 1,
               yes  = "univariate",
               no   = paste(
                   details$.nr_variables,
                   "-variate",
                   sep = "")),
        " observations ",
        if (details$.nr_variables == 1) {
            details$.variables
        } else
            paste("(",
                  paste(details$.variables,
                        collapse = ", "),
                  ")",
                  sep = ""),
        ".",
        sep = "")
###-------------------------------------------------------------------
    ##  Information about the content, with details about variables
    ##  and the point, and a comment that states that only the
    ##  positive lags are needed.

    ## capture_env() 

    ## toTitleCase

    plot_info$Content <-  paste(
        c("A ",
          ifelse(test = all(details$TCS_type == "C",
                            any(details$is_block,
                                details$is_bootstrap )),
                 yes  = "boxplot",
                 no   = "plot"),
          " of the ",
          if  (details$TCS_type == "S")
              c("lag ",
                details$truncation_level,
                " truncated (smoothed with the ",
                details$window,
                " lag-window kernel) "),
          .italics(details$text$plot_type),
          details$text$plot_type_YiYj,
          if (details$is_local) {
              c(" at the point (",
                paste(.points$coordinates,
                      collapse = ", "),
                ").",
                "  The coefficients of this point corresponds respectively to the",
                " standard-normal percentiles ",
                paste(.points$percentiles,
                      collapse = " and "),
                ".")
          } else
              ".",
          if (details$TCS_type == "C")
              c(if (! details$is_lag_zero_needed)
                    c("  Note that the lag zero component always is one",
                      " in this case, and it has thus been dropped from the plot."),
                if (! details$is_negative_lags_needed)
                    c("  The ",
                      if (details$is_local)
                          "local ",
                      details$auto_cross, 
                      "-correlations are even in the lag-argument",
                      if (details$is_local)
                          " (since the point lies on the diagonal),",
                      " so only positive lags are shown in the plot.")),
          if (details$TCS_type == "S")
              c("  Note that the spectrum is ",
                ifelse(test = details$is_even_spectrum,
                       yes  = "even",
                       no   = "odd"),
                " in the",
                " frequency-argument.")),
        collapse = "")
    ##  Add description for the spectra (becomes 'NULL' when not
    ##  relevant.)
    plot_info$Explain_spectra <- details$spectrum_variant_cross_details
###-------------------------------------------------------------------
    ##  A description of the computations, i.e. bandwidth and type of
    ##  local Gaussian approximation.
    plot_info$Computations <-
        if (details$is_local)
            paste(
                c("The computations are based on the estimated correlations from",
                  " a local fitting, at the point (",
                  paste(.points$coordinates,
                        collapse = ", "),
                  "), of a ",
                  details$type,
                  "-parameter Gaussian approximation to the probability",
                  " density functions of the lagged pairs",
                  " (of pseudo-normalised observations).",
                  "  The product-normal kernel was used in the",
                  " estimation algorithm, with the bandwidth ",
                  details$bandwidth,
                  " (for all the lags)."),
                collapse = "")
###-------------------------------------------------------------------
    ##  Add warning if the one-parameter local Gaussian approximation
    ##  has been used (this will be 'NULL' when not relevant).
    if (all(details$type == "one",
            details$is_local))
        plot_info$Warning_one_parameter <- paste(
            "The one-parameter local Gaussian approximation will",
            " in general fail to capture the local properties",
            " of interest.  Use the five-parameter approach instead!",
            sep = "")
###-------------------------------------------------------------------
    ##  Add CI-text (when relevant, will be 'NULL' when not present).
    if (all(details$is_CI_needed,
            details$TCS_type == "S")) {
        plot_info$CI <- paste(
            if (details$CI_percentage == "min_max") {
                "Pointwise max and min values based on "
            } else
                paste(.percent(details$CI_percentage),
                      " pointwise confidence interval based on ",
                      sep = ""),
            if (details$is_block) {
                paste(details$nr_simulated_samples,
                      " independent samples.",
                      sep = "")
            } else
                paste(details$nb,
                      " ",
                      details$boot_type,
                      "-bootstrap replicates, having block-length ",
                      details$block_length,
                      ".",
                      sep = ""),
            sep = "")
    }
###-------------------------------------------------------------------
    ##  Add information about 'trustworthiness', i.e. if numerical
    ##  convergence for the five parameter local Gaussian approach was
    ##  obtained.
    plot_info$Numerical_convergence <- details$text$trust_the_result
#####  REMINDER, 2017-04-21: This is at the moment only a placeholder.
#####  Need to tweak the basic storage to the info-object before this
#####  can be properly presented.
###-------------------------------------------------------------------
    ##  A description of the colours/graphical cues.
    if (details$TCS_type == "S") {
        ##  Information about colours
        plot_info$Colours <- paste(
            c("The ",
            details$text$global_colour,
            " part is the estimate of the ordinary global ",
            details$auto_cross,
            "-spectrum",
            if (details$is_local) {
                c(" (included for comparison)",
                  " whereas the ",
                  details$text$local_colour,
                  " part shows the estimate of the spectra computed from the local Gaussian ",
                  details$auto_cross,
                  "-correlations.")
            } else
                "."),
            collapse  = "")
        ##  Information about lines
        plot_info$Lines <- paste(
            c("The ",
              if (details$is_local)
                  c(details$text$local_colour,
                    "/"),
              details$text$global_colour,
              " ",
              if (details$is_block) {
                  c(details$text$simulated_data_lty,
                    ifelse(test = details$is_local,
                           yes  = "s are ",
                           no   = " is "),
                    "the pointwise estimates of the lag ",
                    details$truncation_level,
                    ifelse(test = details$is_local,
                           yes  = " truncated local/global spectra,",
                           no   = " truncated global spectrum,"),
                    " based on the median of the individual ",
                    ifelse(test = details$is_local,
                           yes  = "local/",
                           no   = ""),
                    "global spectra for each of the ",
                    details$nr_simulated_samples,
                    " independent samples (all of length ",
                    details$N,
                    ").")
              } else
                  c(details$text$real_data_lty,
                    ifelse(test = details$is_local,
                           yes  = "s are ",
                           no   = " is "),
                    "the lag ",
                    details$truncation_level,
                    " truncated ",
                    ifelse(test = details$is_local,
                           yes  = "local/global spectra",
                           no   = "global spectrum"),
                    ", based on the available data (of length ",
                    details$N,
                    ").")),
            collapse  = "")
    }
###-------------------------------------------------------------------
    ##  Add information when 'details$details' (from the inital
    ##  setup) is considered.
    if (! is.null(details$details))
        plot_info$Details <- "TEXT TO BE ADDED HERE!"
###-------------------------------------------------------------------
    ##  Create a vector with the desired result.
    .result <- c(.header("Explanation of plot"),
      .itemising(
          .item(plot_info$Source, .head = "Source:"),
          .item(plot_info$Computations, .head = "Computations:"),
          .item(plot_info$Warning_one_parameter, .head = "WARNING:"),
          .item(plot_info$Content, .head = "Content:"),
          .item(plot_info$Explain_spectra, .head = "Spectrum:"),
          .item(plot_info$CI, .head = "Confidence interval:"),
          .item(plot_info$Colours, .head = "Colours:"),
          .item(plot_info$Lines, .head = "Lines:"),
          .item(plot_info$Details, .head = "Details:"),
          .item(plot_info$Numerical_convergence, .head = "Trustworthiness:")))
###-------------------------------------------------------------------
    ##  Collapse it to a single character string if it is to be used
    ##  in latex (to get it included using '\Sexpr{}')
    if (.mode == "latex")
        .result <- paste(.result, collapse = " ")
    ##  Return the result to the workflow.
    .result
}
