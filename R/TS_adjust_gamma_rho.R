################################################################################
#####  2015-03-10

#' Adjustments for \eqn{$\gamma$} and $\eqn{$\rho$}.
#'
#' The estimation formulas used for autocovariances and
#' autocorrelations at lag \eqn{$h$} are connected to the estimation
#' formulas of the covariances and correlation of the corresponding
#' lagged pairs.  This function computes the scaling and adjustments
#' components that we need in other functions later on. 
#'
#' @param TS The time series object that we want to work upon.  It's
#'     assumed that this has been converted into an array, with one
#'     dimension-name equal to "content".
#'
#' @param .adjustment_rule Either a non-negative number, or "data".
#'     This will be added as an attribute to the normalised time
#'     series, and later on it will decide if any finite-sample
#'     adjustment should be used for the estimated local Gaussian
#'     autocorrelations.  Note that no adjustments will be performed
#'     when \code{.adjustment_rule=0}.
#'
#' @param lag_max Integer that decides the number of scaling and
#'     adjustment-terms that will be returned.  The default value
#'     \code{NULL} will imply that these terms are included from lag 0
#'     to \code{length(TS)-2}.  (In practise this will be way above
#'     what will be used in the estimates, so this argument should be
#'     specified in order to avoid wasting computational resources.)
#'
#' @param type One of \code{c("rho", "gamma")}, i.e. should
#'     the adjustment-terms be computed for data based on local
#'     Gaussian correlations or local Gaussian covariances.  If no
#'     selections are made, both alternatives will be computed.
#' 
#' @param all_details Logic value, default \code{FALSE}, that
#'     influences the amount of information returned to the workflow.
#'
#' @return An array will be returned giving the scaling and adjustment
#'     terms required to go from estimates covariances and
#'     correlations to the estimates of autocovariances and
#'     autocorrelations.  If \code{all_details} is \code{TRUE}, then
#'     some intermediate estimates will be included in the result.
#'
#' @export

TS_adjust_gamma_rho <- function(
    TS,
    lag_max = NULL,
    type = c("rho", "gamma"),
    .adjustment_rule = "data",
    all_details = FALSE) {
###-------------------------------------------------------------------
    ##  Sanity check '.adjustment_rule'.
    .sanity_check <- if (is.character(.adjustment_rule)) {
        .adjustment_rule == "data"
    } else if (is.numeric(.adjustment_rule)) {
        .adjustment_rule >= 0
    } else
        FALSE
    if (! .sanity_check)
        stop("\n\t",
             "Erroneous argument in 'TS_adjust_gamma_rho'.",
             "\n\t",
             "The argument '.adjustment_rule' was given as '",
             .adjustment_rule,
             "',\n\t",
             "but it must either be 'data' or a non-negative number.",
             "\n",
             call. = FALSE)
    ##  Sanity check 'type'.
    if (! all(type %in% c("gamma", "rho")))
        stop("\n\t",
             "Erroneous argument in 'TS_adjust_gamma_rho'.",
             "\n\t",
             "The argument 'type' can only take the values 'gamma' and 'rho'.",
             "\n",
             call. = FALSE)
##-------------------------------------------------------------------
    ##  Pick a procedure based upon '.adjustment_rule', trivial case.
    if (.adjustment_rule == 0) {
        .result <- structure(
            .Data = 1,
            .adjustment_rule = .adjustment_rule)
        return(.result)
    }
###-------------------------------------------------------------------
    ##  Find the number of observations.
    .N <- length(dimnames(TS)$observations)
    ##  When required, define 'lag_max'.
    if (identical(lag_max, NULL))
        lag_max <- .N - 2
    ##  Procedure for  '.adjustment_rule', simple case.
    if (is.numeric(.adjustment_rule)) {
        .result <- structure(
            .Data = ((.N - 0:lag_max)/.N)^.adjustment_rule,
            .Names = 0:lag_max,
            .adjustment_rule = .adjustment_rule)
        return(.result)
    }
###-------------------------------------------------------------------
    ##  Code still running?  Then '.adjustment_rule = "data"'.
    ##  Proceed by creating a help-function to use upon each
    ##  "content"-component of 'TS'.
    .help_fun <- function(.TS) {
        .TS <- as.vector(.TS)
        ##  Estimate stuff related to 'tails' and 'heads'
        mean_sd <- array(
            data = NA_real_,
            dim = c((lag_max), 4),
            dimnames = list(
                h = 1:lag_max,
                type = c(
                    "tail_mean", "head_mean",
                    "tail_sd", "head_sd")))
        ##---
        for (h in 1:(lag_max)) {
            h_tail <- tail(.TS, -h)
            h_head <- head(.TS, -h)
            ## Estimates 'sd'-values only, when necessary.
            mean_sd[as.character(h), ] <-
                c(c(mean(h_tail), mean(h_head)),
                  if ("rho" %in% type) {
                      c(sd(h_tail), sd(h_head))
                  } else
                      c(NA_real_, NA_real_)
                  )
        }
        ##  Create an array to store the results in, the size of it
        ##  might be reduced at the end depending on the values used
        ##  for 'type' and 'all_details'.
        result <- array(
            data = NA_real_,
            dim = c(lag_max + 1, 4, 
                    ifelse(test = identical(type, "gamma"),
                           yes  = 1,
                           no   = 2)),
            dimnames = list(
                lag = 0:lag_max,
                estimates = c("orig", "auto", "scale", "adjust"),
                type = {
                    if (identical(type, "gamma")) {
                        "gamma"
                    } else
                        c("gamma", "rho")
                }))
        ##  We always need the estimates of "orig" and "auto" for the
        ##  case 'type' equal to "gamma".  (In this case "orig" is
        ##  'covariance of lagged pairs' and "auto" is the
        ##  autocovariance.)
        result["0", "orig", "gamma"] <- var(.TS)
        result["0", "auto", "gamma"] <- sum((.TS - mean(.TS))^2) / .N
        ##---
        .mean_.TS <- mean(.TS)
        for (h in 1:(lag_max)) {
            result[as.character(h), "orig", "gamma"] <- 
                sum({head(.TS, -h) -
                         mean_sd[as.character(h), "head_mean"] } * 
                    {tail(.TS, -h) -
                         mean_sd[as.character(h), "tail_mean"]}) /
                {.N - h - 1}
            ##---
            result[as.character(h), "auto", "gamma"] <-
                sum({head(.TS, -h) - .mean_.TS} * 
                    {tail(.TS, -h) - .mean_.TS}) / .N
        }
        ##  Add 'scale' and 'adjust' to the 'gamma' part of the result.
        if ("gamma" %in% type) {
            result[, "scale", "gamma"] <-
                {{.N - 1} - 0:(lag_max)} / .N
            ##---
            result[, "adjust", "gamma"] <-
                result[, "auto", "gamma"] -
                result[, "orig", "gamma"] *
                result[, "scale", "gamma"]
        }
        ##  Add stuff to the 'rho' part of the result, when relevant.
        ##  (Now "orig" refers to correlations and "auto" to
        ##  autocorrelations.
        if ("rho" %in% type) {
            tail_head_sd_product <- mean_sd[, "tail_sd"] * mean_sd[, "head_sd"]
            ##---
            result[, "orig", "rho"] <-  
                result[, "orig", "gamma"] /
                c(result["0", "orig", "gamma"],
                  tail_head_sd_product)
            ##---
            result[, "auto", "rho"] <-
                result[, "auto", "gamma"] /
                result["0", "auto", "gamma"]
            ##  Add "scale and "adjust".
            result[, "scale", "rho"] <-
                c(1,
                {{.N - 1} - 1:(lag_max)} / .N * 
                tail_head_sd_product /
                result["0", "auto", "gamma"])
            ##---
            result[, "adjust", "rho"] <-
                result[, "auto", "rho"] -
                result[, "orig", "rho"] *
                result[, "scale", "rho"]
        }
        ##  Investigate if any restrictions are required.
        .restrict_list <- c(
            if (! all_details)
                list(estimates = c("scale", "adjust")),
            if (identical(type, "rho"))
                list(type = "rho") )
        ##---
        if (! identical(.restrict_list, NULL))
            result <- restrict_array(
            .arr = result,
            .restrict = .restrict_list)
        ##  Return the result.
        result
    }
###-------------------------------------------------------------------
    ##  Return the desired result to the workflow.
    structure(
        .Data = aaply(
            .data = TS,
            .margins = which(names(dimnames(TS)) == "content"),
            .fun = .help_fun,
            .parallel = TRUE,
            .drop = FALSE),
        .adjustment_rule = .adjustment_rule,
        class = LG_default$class$array)
}
