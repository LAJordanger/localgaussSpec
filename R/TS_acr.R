#' Autocovariances and autocorrelations the old-fashioned way.
#'
#' @description This internal function use the "old-fashioned" way to
#'     compute the autocorrelations (and cross-correlations) for a
#'     given time series, i.e. the summation-approach.  This is much
#'     slower than the fast Fourier transform approach used by
#'     \code{acf}, but it seems more natural to use this when
#'     computing the ordinary spectral density in the old fashioned
#'     way (using lag-window functions instead of periodograms).
#'
#' @param .TS_info A list containing the three components \code{TS},
#'     \code{main_dir} and \code{save_dir}.
#'
#' @template lag_max_arg
#' 
#' @description The \code{.TS_info}-argument is given to the internal
#'     function \code{TS_load}, which loads the \code{TS}-part from
#'     file when required. \code{TS_load} will also compute the name
#'     of the directory in which the result should be stored.
#' 
#' @return A file will be created containing an array with the
#'     (ordinary) autocorrelations (and cross-correlations) of
#'     \code{TS}.  A list with the following to components will be
#'     returned to the internal workflow, in order to be added to the
#'     'info'-object.
#'
#' \describe{
#'
#' \item{.acr_type}{This will either simply be "acr" or "acr_boot".
#'    The suffix "boot" is added when the computations are based on
#'    bootstrapped replicates of an original time series.  (The
#'    information about the bootstrap-status is available as an
#'    attribute of the \code{TS}-object the computation is based on.)}
#'
#' \item{.acr_content}{This will be a vector with the components
#'     needed in order to load the saved result back into the
#'     workflow.}
#'
#' }
#' 
#' @keywords internal

TS_acr <- function(.TS_info,
                   lag_max = quote(ceiling(3*sqrt(length(TS))))) {
    ##  Load 'TS' and 'save_dir' from the information in '.TS_info'.
    TS_load(.TS_info,
            save_dir = TRUE)
    ##  Identify if 'TS' is related to bootstrapping.
    .bootstrap <-
        if (is.null(attributes(TS)$bootstrap)) {
            FALSE
        } else
            attributes(TS)$bootstrap
    ##  Identify the type based on '.bootstrap' (needed in order to
    ##  identify the correct path into the saved file-hierarchy).
    .acr_type <- paste("acr",
                       ifelse(
                           test = .bootstrap,
                           yes  = "_boot",
                           no   = ""),
                       sep = "")
    kill(.bootstrap)
    ##  If 'lag_max' is the default call, then the 'length(TS)'-part
    ##  should be replaced with the length of the dimension
    ##  'observations' (from 'TS'), before it is evaluated.
    if (is.call(lag_max)) {
        lag_max[[c(2, 3, 2)]] <-
            length(dimnames(TS)$observations)
        lag_max <- eval(lag_max)
    }
    ##  Extract relevant attributes from 'TS'.
    .attr_from_TS <- local({
        .ignore <- c("dim", "dimnames", "TS_for_analysis")
        .keep <- ! names(attributes(TS)) %in% .ignore
        attributes(TS)[.keep]
    })
    TS <-
        if (! is.null(attributes(TS)$TS_for_analysis)) {
            structure(
                .Data = abind(TS,
                              attributes(TS)$TS_for_analysis,
                              along = length(dim(TS)) + 1),
                .Dimnames = c(dimnames(TS),
                              list(TS = c("TS_original", "TS_for_analysis"))),
                class = class(TS))
        } else
            structure(
                .Data = TS,
                .Dim = c(dim(TS), 1),
                .Dimnames = c(dimnames(TS),
                              list(TS = "TS_original")))
    ##  Help function to compute the auto- and cross-correlations on
    ##  each component. Reminder: The 'vec'-argument is based on the
    ##  rows of the 'arg_grid' defined below.
    .acr_helper <- function(vec,
                            TS = TS,
                            lag_max = lag_max) {
        ##  Extract the desired part from 'TS'.
        .ts <- restrict_array(
            .arr = TS,
            .restrict = list(content = vec$content, TS = vec$TS),
            .drop = TRUE,
            .never_drop = c("observations", "variables"))
        ##  Split 'vec$pairs' for subsetting
        .pairs <- strsplit(x = vec$pairs, split = "_")[[1]]
        ##  Extract the components of interest, and compute the
        ##  desired results. Reminder: We here need to separate out
        ##  the solution to be used in general from the one used for
        ##  the circular index-based block bootstrap for tuples.
        cibbb_case <- isTRUE(attributes(TS)$boot_type == "cibbb_tuples")
        if  (! cibbb_case) {
            .first <- restrict_array(
                .arr = .ts,
                .restrict = list(variables = .pairs[1]),
                .drop = TRUE)
            .second <- restrict_array(
                .arr = .ts,
                .restrict = list(variables = .pairs[2]),
                .drop = TRUE)
            ##  Perform the mean-adjustment,
            .first <- .first - mean(.first)
            .second <- .second - mean(.second)
            ##  Find the "unscaled" denominator (the scaling factor occurs
            ##  both in numerator and denominator, and can be dropped.)
            .denom <- sqrt(sum(.first^2) * sum(.second^2))
            ##  Compute and return the result.
            return(structure(
                .Data = vapply(X = 0:lag_max,
                               FUN = function(lag) {
                                   if (lag == 0) {
                                       .first %*% .second / .denom
                                   } else
                                       tail(.first, -lag) %*% head(.second, -lag) / .denom
                               },
                               FUN.VALUE = numeric(1)),
                .Names = 0:lag_max))
        } else {
            ## The "cibbb_tuples"-case, in which '.ts' contains the
            ## pivotal indices for the computation.
            .indices <- as.vector(.ts)
            .orig_TS <- attributes(TS)$orig_TS
            .n <- length(dimnames(.orig_TS)$observations)
            .first_orig <- restrict_array(
                .arr = .orig_TS,
                .restrict = list(variables = .pairs[1]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            .second_orig <- restrict_array(
                .arr = .orig_TS,
                .restrict = list(variables = .pairs[2]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            .first <- .first_orig[.indices]
            .second <- .second_orig[.indices]
            ##  Perform the mean-adjustment,
            .first <- .first - mean(.first)
            .second <- .second - mean(.second)
            ##  Find the "unscaled" denominator (the scaling factor occurs
            ##  both in numerator and denominator, and can be dropped.)
            .denom <- sqrt(sum(.first^2) * sum(.second^2))
            ##  Compute and return the result.
            return(structure(
                .Data = vapply(X = 0:lag_max,
                               FUN = function(lag) {
                                   if (lag == 0) {
                                       .first %*% .second / .denom
                                   } else {
                                       ##  Use 'cibbb_tuples'
                                       ##  adjustment for the
                                       ##  subsetting.
                                       .cibbb_tuples <- TS_cibbb_tuples(
                                           .indices = .indices,
                                           .lag = lag)
                                       .first_orig[.cibbb_tuples$first] %*%
                                           .second_orig[.cibbb_tuples$second] / .denom
                                   }
                               },
                               FUN.VALUE = numeric(1)),
                .Names = 0:lag_max))
        }
    }
    ##  Compute the desired autocorrelations.
    arg_grid <- expand.grid(
        pairs = .attr_from_TS$.variable_pairs,
        TS = dimnames(TS)$TS,
        content = dimnames(TS)$content,
        stringsAsFactors = FALSE)
    .acr <- aaply(
        .data = arg_grid,
        .margins = 1,
        .fun = .acr_helper,
        TS = TS,
        lag_max = lag_max,
        .drop = FALSE)
    kill(arg_grid, .acr_helper, lag_max, TS)
    ## Add a name for the new dimension, and adjust the attributes.
    names(dimnames(.acr))[length(dimnames(.acr))] <- "lag"
    attributes(.acr) <- c(
        attributes(.acr),
        .attr_from_TS)
    ##  Save the result to file.
    LG_save(data = .acr,
            save_file.Rda = LG_default$global[.acr_type],
            save_dir = save_dir)
    ##  Return information to be added to the 'info'-object.
    list(.acr_type = .acr_type,
         .acr_content = c(.TS_info$save_dir, LG_default$global[.acr_type]))
}
