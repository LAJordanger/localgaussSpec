#' Create a normalised time series from the original ones.
#'
#' @details A normalisation of the time series under consideration is
#'     preferable when an analysis based on the local Gaussian
#'     approximations are of interest.  The intention of this function
#'     is to allow for easier adjustment of the normalisation regime,
#'     i.e if the normalisation should be based on the cumulative
#'     density function (or other options that might be added later
#'     on).  This function will also add some attributes to the
#'     result, with the required instruction for the finite-sample
#'     adjustments to be used when computing the local Gaussian
#'     autocorrelations.
#'
#' @param TS The time series object to normalise.  It's assumed that
#'     this is structured in an array, with one dimension-name being
#'     "content".
#' 
#' @param .remove_ties A logical value, default \code{TRUE}, in which
#'     case the presence of ties will trigger a minor perturbation of
#'     the data.  Note: Whenever this happens, \code{set.seed(1)}
#'     will be used in order to ensure reproducibility.
#'
#' @return A normalised version of the time series of interest.
#'
#' @export


LG_normalisation_adjustment <- function(
             TS,
             .remove_ties = TRUE) {
    ##  Perform the desired normalisation for each combination of
    ##  variable and content.
    .not_observations <- ! names(dimnames(TS)) %in% "observations"
    .arg_grid <- expand.grid(
        dimnames(TS)[.not_observations],
        stringsAsFactors = FALSE)
    TS_new <- aaply(.data = .arg_grid,
                    .margins = 1,
                    .fun = function(x)  {
                        ##  Identify the desired part.
                        .var <- x[["variables"]]
                        .con <- x[["content"]]
                        .TS_x <- restrict_array(
                            .arr = TS,
                            .restrict = list(
                                variables = .var,
                                content = .con))
                        ##  Get rid of ties, if present, and asked for.
                        if (.remove_ties) {
                            ##  Adjust if any ties are present.
                            if (length(.TS_x) != length(unique(.TS_x))) {
                                set.seed(seed = 1)
                                .TS_x <- .TS_x + diff(range(.TS_x)) *
                                    rnorm(n = length(.TS_x), sd = 1e-8)
                            }
                        }
                        ##  Return the required normalised version.
                        qnorm((rank(.TS_x) - 0.5)/length(.TS_x))
                    },
                    .drop = FALSE,
                    .parallel = TRUE)
    ##  Adjust the dimnames.
    dimnames(TS_new) <- c(
        dimnames(TS)[.not_observations],
        dimnames(TS)["observations"])
    ##  Adjust the shape of 'TS_new' so it matches that of 'TS'
    ##  (needed for the code later on.)
    TS_new <- restrict_array(
        .arr = TS_new,
        .restrict = dimnames(TS),
        ## .restrict = dimnames(TS_new)[c("observations", "variables", "content")],
    .permute = TRUE)
###-------------------------------------------------------------------
    ##  Add additional attributes to 'TS_new'.
    attributes(TS_new) <- c(
        attributes(TS_new),
        list(.remove_ties = .remove_ties,
             class = LG_default$class$array))
    ##  Return the result to the workflow.
    TS_new
}

