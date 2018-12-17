################################################################################
#####  2015-04-29

#' Estimate bandwidths based on local likelihood...
#'
#' @details The defaults for this function has been selected after an
#' investigation of how different settings affected the final values,
#' with an emphasis on getting "good enough" values within a shortest
#' possible computational time.  Keep in mind that the \code{data}
#' must have normalised marginals.
#' 
#' @param data A matrix containing the multivariate observations, one
#' observation in each row.  Note that the marginals of these
#' observations must be normalised in order for the assumptions of
#' this code to be satisfied.
#'
#' @param .kernel Specification of the kernel to use, either
#' \code{normal} or \code{uniform}, with default being the
#' \code{normal} one if none is selected.  Note: The
#' \code{uniform}-kernel takes much longer to compute (a factor of ten
#' when used on a small sample of size \eqn{$n=200$}).
#'
#' @param use_boundaries Logic value, default \code{TRUE}, that
#' decides whether or not the bandwidths will be selected within the
#' restrictions given by the argument \code{boundary_percentiles}
#' 
#' @param boundary_percentiles A vector specifying two percentiles,
#' default \code{c(0.1, 0.9)}, that will be used on the range of the
#' individual margins in order to ensure that the returned bandwidths
#' are within a "reasonable" range.  This argument will be ignored
#' when \code{use_boundaries} are given as \code{FALSE}.
#'
#' @param tol Convergence tolerance, default value \code{1e-04}. The
#' iteration is terminated when the absolute difference in function
#' value between successive iteration is below \code{tol}.
#'
#' @return The result of this function is a matrix that for each
#' bivariate combination from the points given in \code{data} will
#' return the bandwidths obtained when the function
#' \code{link{find_rho}} has been optimised with regard to the selected
#' bivariate subset of the data.  In addition to the bandwidths, there
#' will also be a \code{convergence}-column that should contain
#' \code{0} if the numerical combination of the bandwidths succeeded
#' without any problems.
#' 
#' @keywords internal

find_bws <- function(
    data,
    .kernel = c("normal", "uniform"),
    use_boundaries = TRUE,
    boundary_percentiles = c(0.1, 0.9),
    tol = 1e-4) {
###-------------------------------------------------------------------
    ##  Identify the number 'n' of observations and the dimension 'd'
    ##  of the specified observations.
    n <- dim(data)[1]
    d <- dim(data)[2]
###-------------------------------------------------------------------
    ##  Specify the desired information to keep or compute.
    desired_information <-
        c("bi", "bj", "convergence", "tol", "value", "feval", 
          "use_boundaries", "low_percentile", "high_percentile",
          "low_i", "high_i", "low_j", "high_j")
###-------------------------------------------------------------------
    ##  An adjustment to "do nothing" for the case where 'd' is equal
    ##  to '2' and the two components are identical (to avoid a messy
    ##  "corner-case" argument in other functions later on).
    if (d == 2)
        if (identical(x = data[, 1], y = data[, 2]))
            return(structure(
                .Data = rep(x = NA_real_,
                            times = 2 + length(desired_information)),
                .Dim = c(2 + length(desired_information), 1),
                .Dimnames = list(
                    c("xi", "xj", desired_information),
                    NULL)
            ))
###-------------------------------------------------------------------
    ## Initialise the matrix to store stuff in.
    joint_bandwidths <- matrix(
        data = NA_real_,
        nrow = 2 + length(desired_information),
        ncol = choose(d, 2),
        dimnames = list(
            c("xi", "xj", desired_information),
            NULL))
    ##  Specify the bivariate combinations that we need to inspect.
    joint_bandwidths[c("xi", "xj"), ] <- combn(1:d, 2)
###-------------------------------------------------------------------
    ##  Add the stuff that is common for all the combinations.
    joint_bandwidths["tol", ] <- tol
    joint_bandwidths["use_boundaries", ] <- use_boundaries
    joint_bandwidths[c("low_percentile", "high_percentile"), ] <-
        boundary_percentiles
    
###-------------------------------------------------------------------
    ##  Calculate the bandwidths for each pair of observations
    for(i in 1:dim(joint_bandwidths)[2]) {
        biv.data <- data[, joint_bandwidths[c('xi', 'xj'), i]]
        range_ij <- apply(X = biv.data,
                        MARGIN = 2,
                        FUN = function(x) {
                            diff(range(x))
                        })
        .lower <- range_ij * boundary_percentiles[1]
        .upper <- range_ij * boundary_percentiles[2]
        .initial_par <- range_ij * mean(boundary_percentiles)
        ##---
        CV.bivariate <- function(b) {
            obj <- function(j) {
                find_rho(data = biv.data[-j,],
                         grid = biv.data[j, , drop = FALSE],
                         b = b,
                         .kernel = .kernel)$log_f.est
            }
            -mean(x = unlist(lapply(
                      X = as.list(1:n),
                      FUN = obj)),
                  na.rm = TRUE)
        }
###-------------------------------------------------------------------
        ##  Select a method based on the value of 'use_boundaries',
        ##  and compute the desired bandwidths with all the other
        ##  information returned by the selected optimiser.
        .tmp_bandwidths <-
            if (use_boundaries) {
                dfoptim::nmkb(
                    par = .initial_par,
                    lower = .lower,
                    upper = .upper,
                    fn = CV.bivariate,
                    control = list(tol = tol))
            } else
                dfoptim::nmk(
                    par = c(1, 1),
                    fn = CV.bivariate,
                    control = list(tol = tol))
###-------------------------------------------------------------------
        ##  Extract the desired information and update the storage.
        joint_bandwidths[c("low_i", "low_j"), i] <- .lower
        joint_bandwidths[c("high_i", "high_j"), i] <- .upper
        ##---
        .extract <- c("bi", "bj", "convergence", "value", "feval")
        joint_bandwidths[.extract, i] <- unlist(
            .tmp_bandwidths[c("par", "convergence", "value", "feval")])
    }
###-------------------------------------------------------------------
    ##  Add an attribute to 'joint_bandwidths' to reveal if all the
    ##  numerical convergences was successful.
    attr(x = joint_bandwidths,
         which = "numerical_convergence") <- 
             sum(joint_bandwidths["convergence", ]) == 0
    
###-------------------------------------------------------------------
    ##  Write a warning when necessary
    if (! attributes(joint_bandwidths)$numerical_convergence)
        warning("\t",
                "One or more of the numerical convergences failed!",
                "\n\t",
                "Don't trust the result from 'find_bws', proceed with care.",
                call. = FALSE,
                immediate. = TRUE)
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    joint_bandwidths
}

##  And finally, compile the function in order to get it a bit faster.

## require(compiler)
## find_bws <- cmpfun(find_bws)
#####  Skip this when the package is installed.
