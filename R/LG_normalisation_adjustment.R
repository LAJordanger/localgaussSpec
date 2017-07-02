################################################################################
#####  2016-02-09

#' Create normalised time series from the original ones.
#'
#' @details A normalisation of the time series under consideration is
#'     preferable when an analysis based on the local Gaussian
#'     approximations are of interest.  The intention of this function
#'     is to allow for easier adjustment of the normalisation regime,
#'     i.e if the normalisation should be based on the cumulative
#'     density function, the logspline-density (or other options that
#'     might be added later on).  This function will also add some
#'     attributes to the result, with the required instruction for the
#'     finite-sample adjustments to be used when computing the local
#'     Gaussian autocorrelations.
#'
#' @param TS The time series object to normalise.  It's assumed that
#'     this is structured in an array, with one dimension-name being
#'     "content".
#' 
#' @param .normalisation_rule A value from \code{c("ecdf",
#'     "logspline")} that decides which kind of normalisation rule to
#'     use upon the data.  The "logspline"-alternative seems to give a
#'     more faithful representation of the data, but can encounter
#'     problems if there's "to much data" in some regions (which can
#'     occur for a long time series if some really extreme
#'     observations are present).  The "ecdf" doesn't encounter such
#'     problems, and it will also be much quicker to compute, which
#'     for larger time series might be an important aspect to keep in
#'     mind.
#'
#' @param .adjustment_rule Either a non-negative number, or "data".
#'     This will be added as an attribute to the result, and later on
#'     it will decide if any finite-sample adjustment should be used
#'     for the estimated local Gaussian autocorrelations.  Note that
#'     no adjustments will be performed when
#'     \code{.adjustment_rule=0}.
#'
#' @param .remove_ties A logical value, default \code{TRUE}, in which
#'     case the presence of ties will trigger a minor perturbation of
#'     the data.  Note: Whenever this happens, \code{set.seed(1)}
#'     will be used in order to ensure reproducibility.
#'
#' @return A normalised version of the time series, with attributes
#'     specifying how to adjust later on due to the effect of finite-
#'     samples.
#'
#' @export


LG_normalisation_adjustment <- function(
             TS,
             .normalisation_rule = c("ecdf", "logspline"),
             .adjustment_rule = "data",
             .remove_ties = TRUE) {
    ##  Only allow one '.normalisation_rule'
    .normalisation_rule <- .normalisation_rule[1]
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
                                ## variables = x["variables"],
                                ## content = x["content"]))
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
                        if (.normalisation_rule == "logspline") {
                            qnorm(plogspline(
                                q = .TS_x,
                                fit = logspline(.TS_x)))
                        } else
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
    ##  Add additional attributes to 'TS_new', i.e. the
    ##  '.adjustment_rule' that later on will decide if/how
    ##  finite-samples effects should be accounted for.
    attributes(TS_new) <- c(
        attributes(TS_new),
        list(.normalisation_rule = .normalisation_rule,
             .adjustment_rule = .adjustment_rule,
             .remove_ties = .remove_ties,
             class = LG_default$class$array))
    ##  Return the result to the workflow.
    TS_new
}

