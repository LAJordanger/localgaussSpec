################################################################################
#'
#' Compute global bandwidths for Local Gaussian Estimates.
#'
#' This function will check if a file with previous computations
#' exists, and if so rather read the results from that file than
#' recomputing the results.  If no file is found in the specified
#' directory, or if only some of the results are computed, then it
#' will do the required computations and create a file to be used
#' later on.
#'
#' @template save_dir_arg
#' 
#' @param TS The time series we want to investigate by means of local
#'     Gaussian approximation (and later on with local Gaussian
#'     spectra).  Note that it is assumed that this time series have
#'     normalised marginals.  The code anticipates that this time
#'     series should have a logical attribute \code{bootstrap} that
#'     decides some of the actions taken under the evaluation, if no
#'     such attribute are found, the default will be to assume that
#'     \code{bootstrap} should be \code{FALSE}.
#'
#' @param lag_min A non-negative integer, default value \code{0}, that
#'     specifies the lowest lagged pairs to compute the bandwidths
#'     for.  This allows the computation of the bandwidths to be
#'     performed in smaller chunks.
#' 
#' @param lag_max How many lags should be included in the analysis.
#' 
#' @return This function will return the desired bandwidths to the
#'     work-flow, and in addition save/update a file with the desired
#'     information.  If later on there's a need for a computation with
#'     a higher value for \code{lag_max}, then the values from the
#'     saved file will be used in order to avoid redoing previous
#'     computations.
#'
#' @keywords internal


##  TASK, 2015-12-18: This function should have an argument 'length-of-
##  leave-out-interval' to be used when dealing with observations from
##  time-series.

bws_global <- function(
    save_dir = NULL,
    TS,
    lag_min = 0,
    lag_max) {
###-------------------------------------------------------------------
    ##  If 'TS' originates from a 'TS_LG_object', it should have an
    ##  attribute 'TS_for_analysis' that should be used instead of TS.
    if (! identical(x = attributes(TS)$TS_for_analysis,
                    y = NULL)) {
        TS <- attributes(TS)$TS_for_analysis
    }
###-------------------------------------------------------------------
    ##  Initiate empty object for the result, and the list of lags
    ##  that must be computed if no previous computations are found.
    .bws_data <- NULL
    new_lags <- lag_min:lag_max
###-------------------------------------------------------------------
    ##  Find the path to the file.
    bws_file <- file.path(save_dir,
                          LG_default$bws_file)
###-------------------------------------------------------------------
    ##  Create a logic value to see if old data can (should) be used,
    ##  i.e. if there already has been performed computations that
    ##  either can be used directly or built upon if an extended
    ##  version is desired.
    old_data <- all(
        ! is.null(save_dir),
        file.exists(bws_file))
###-------------------------------------------------------------------
    ##  If the file already exists (and should be used), read it into
    ##  the workflow and check if any new computations is required.
    if (old_data) {
        ##  Load the previously computed '.bws_data' from file.
        load(bws_file) 
        ##  Investigate if there's a need for any new computations.
        any_new_lags <- setdiff(
            x= lag_min:lag_max,
            y = as.numeric(
                dimnames(.bws_data)$lag))
        ##  If no new computations are needed, return the desired part
        ##  to the workflow.
        if (length(any_new_lags) == 0)
            return(restrict_array(
                .arr = .bws_data,
                .restrict = list(lag = as.character(new_lags))))
        ##  Code still running? Then we need to do the computations
        ##  for the new stuff.
        new_lags <- any_new_lags
        kill(any_new_lags)
    }
###-------------------------------------------------------------------
    ##  Create an argument grid for the required computations.
    argument_grid <- expand.grid(
        lag = new_lags,
        ## lag = setdiff(x = new_lags, y = 0),
        pairs = attributes(TS)$.variable_pairs,
        content = dimnames(TS)$content,
        stringsAsFactors = FALSE)
    ##  Create a help-function to use upon the argument grid.
    ##  capture_env() 
#####  2017-01-04: This first iteration does not consider the
#####  multivariate case, where negative lags and lag zero must be
#####  considered. !!!!!
    .help_fun <- function(row, TS) {
        ##  Find the required time series (as a bivariate array).
        .ts <- restrict_array(
            .arr = TS,
            .restrict = list(content = row$content),
            .drop = TRUE,
            .never_drop = c("observations", "variables"))
        ##  Find the lagged pairs.
        .pair <- structure(
            .Data = strsplit(x= row$pairs, split = "_")[[1]],
            .Names = c("first", "second"))
        .first <- restrict_array(
            .arr = .ts,
            .restrict = list(variables = .pair["first"]),
            .drop = TRUE)
        .second <- restrict_array(
            .arr = .ts,
            .restrict = list(variables = .pair["second"]),
            .drop = TRUE)
        .lagged <- if (row$lag == 0) {
            cbind(.first,
                  .second)
        } else
            cbind(
                head(x = .first,  n = - row$lag),
                tail(x = .second, n = - row$lag))
        ##  Compute the desired bandwidths information.  Reminder:
        ##  Only 'NA' will be returned for the case lag = 0 when
        ##  '.first' and '.second' are identical.
        find_bws(data = .lagged)
    }
###-------------------------------------------------------------------
    ##  Use 'aaply' and 'find_bws' on argument.grid
    .new_data <- aaply(
        .data = argument_grid,
        .margins = 1,
        .fun = .help_fun,
        TS = TS,
        .drop = FALSE, ##  Keep this to get an array!
        .parallel = TRUE)
    class(.new_data) <- LG_default$class$array
###-------------------------------------------------------------------
    ##  Adjust the dimension and the dimension names.
    .d <- dim(.new_data)
    .dn <-dimnames(.new_data) 
    ##---
    dim(.new_data) <- head(.d, n = -1)
    dimnames(.new_data) <- head(.dn, n = -1)
    names(dimnames(.new_data))[4] <- "find_bws"
###-------------------------------------------------------------------
    ##  Combine new stuff and old stuff, when necessary.
    if (is.null(.bws_data)) {
        .bws_data <- .new_data
    } else {
        ##  Combine old and new data.
        .bws_data <- abind(
            .bws_data,
            .new_data,
            along = 1)
        ##  Add the names on the dimensions.
        names(dimnames(.bws_data)) <- names(dimnames(.new_data))
    }
###-------------------------------------------------------------------
    ##  Save to file, when required.
    if (! is.null(save_dir))
        save(.bws_data, file = bws_file)
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    .bws_data
}

