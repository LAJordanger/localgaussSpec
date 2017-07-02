################################################################################
#'
#' Statistics based on the bootstrapped replicates.
#'
#' This function specifies the kind of statistics we want to extract
#' from an array of estimated local Gaussian spectral densities,
#' e.g. either when performing an analysis based upon a bunch of
#' samples from a known distribution, or when bootstrapping is used to
#' find confidence intervals.
#'
#' @param x A vector extracted from the array of values that we want
#'     to investigate.
#'
#' @template all_statistics_arg
#' @template log__arg
#'
#' @param names_only \code{logic}, default \code{FALSE}.  When this is
#'     used, no computations are performed, and only the names to be
#'     used on the resulting vector will be returned.
#'
#' @return This function can produce quite different outputs depending
#'     on the value \code{names_only}.  It will either be a vector
#'     that only gives the dimension-names (depending on
#'     \code{all_statistics} and \code{log_}) or it might be an
#'     _unnamed_ vector with the computed values.  This strategy has
#'     been chosen in order keep the intermediate objects as small as
#'     possible (but I have not tested this to see if there should be
#'     any difference in performance, so it might perhaps be an inane
#'     choice).  Anyway, we do need the names of the content if we
#'     should need to split our computation into pieces, since the
#'     chosen solution then requires that we need to create the matrix
#'     the resulting pieces should be inserted into.
#'
#' @export


LG_boot_statistics <- function(x,
                               all_statistics=FALSE,
                               log_ = FALSE,
                               names_only = FALSE) {
###-------------------------------------------------------------------
    ##  Accept missing 'x' when 'names_only' equals TRUE
    if (missing(x) & names_only)
        x <- 1  #  Just a dummy value.
###-------------------------------------------------------------------
    ##  If 'log_' equals TRUE, replace x with log(x)
    if (log_)
        x <- log(x)
###-------------------------------------------------------------------
    ##  Based on the value of 'names_only', either return the names or
    ##  the estimated values.
    if (names_only) {
###-------------------------------------------------------------------
        ##  Introduce the list of names to be used on the resulting array.
        stat_names <- c("min",
                        "low_99",
                        "low_95",
                        "low_90",
                        "median",
                        "high_90",
                        "high_95",
                        "high_99",
                        "max")
        if (all_statistics) 
            stat_names <- c(stat_names,
                            c("mean",
                              "sd",
                              "skewness",
                              "kurtosis",
                              "mad"))
        stat_names <- structure(
            .Data = list(stat_names),
            .Names = LG_default$dimnames$main)
        ## stat_names <- list(statistics = stat_names)
        ##  Add "log" to the name of 'stat_names' if 'log_' equals TRUE.
        if (log_)
            names(stat_names) <-
                paste("log.", names(stat_names), sep="")
        ##  Return 'stat_names'.
        return(stat_names)
    } else {
###-------------------------------------------------------------------         
        ##  Compute the statistics
        result <-
            c(quantile(x = x,
                       probs=c(0, 0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995, 1),
                       names = FALSE))
        if (all_statistics) 
            result  <-
                c(result, 
                  c(mean(x),
                    sd(x),
                    skewness(x),
                    kurtosis(x),
                    mad(x)) )
        ##  Return the result
        return(result)
    }
}
