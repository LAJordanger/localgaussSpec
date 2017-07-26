################################################################################
##  Function to be added to 'TS_families'.  Procedure: First define
##  the function - and then update 'TS_families' with the new object.

#' Samples built upon cosine.
#'
#' The \code{dmt}-function can create a sample where different
#' trigonometric functions live at separate levels.  This function is
#' at the moment rather simplistic, as its main intention is to just
#' create an example for ISNPS-2016 (and I'm running low on time)
####  TASK:  Update this crap later on.
#'
#' @param n The number of samples to create.
#'
#' @param A A matrix whose first row should be the positions and
#'     second row should be the probabilites of getting there.  The
#'     cases of main interest to investigate is the case where this
#'     describes a process having expectation \code{0}.
#'
#' @param delta Either a single number, or a vector whose length
#'     equals the number of columns in \code{A}.  This will be used as
#'     the amplitudes for the cosine-terms.
#'
#' @param delta_range Either \code{NULL} (the default) or a vector
#'     similiar to \code{delta}.  When this is different from
#'     \code{NULL} the effect will be that the values for the deltas
#'     will be sampled uniformly based on the intervals that the
#'     corresponding components of \code{delta} and \code{delta_range}
#'     span.
#'
#' @param alpha Either a single number, or a vector whose length equals
#'     the number of columns in \code{A}.  This will be used as the
#'     frequencies in the cosine-terms.
#'
#' @param theta Either a single number, or a vector whose length
#'     equals the number of columns in \code{A}.  This will be used as
#'     the phase in the cosine-terms.  The default value \code{NULL}
#'     will trigger a procedure that draws these numbers uniformly
#'     between \code{0} and \code{2pi}.
#'
#' @param wn A list that specifies how to sample from some
#'     distribution.  The default is to use standard normal white
#'     noise.  Note: It's assumed that this should be white noise, but
#'     there's no test that prevents a user from selecting something
#'     different.  The value \code{NULL} can be used to set this term
#'     to zero.
#'
#' @param seed An integer to be used for the specification of a seed,
#'     the default value NULL will trigger a seed to be selected at
#'     random.
#'
#' @return A sample of length \code{n} based on the specifications
#'     given in the other arguments
#' 
#' @name dmt
#' @keywords internal


dmt <- function(
    n,
    A,
    delta,
    delta_range = NULL,
    alpha,
    theta = NULL,
    wn = list(type = "rnorm",
           args = list(mean = 0,
                       sd = 1),
           adjust = 0),
    seed = NULL) {
###-------------------------------------------------------------------
    ##  Sanity check `A`.
    if (! is.matrix(A))
        error(.argument = "A",
              c(sQuote("A"),
                "must be a matrix."))
    if (nrow(A) != 2)
        error(.argument = "A",
              c(sQuote("A"),
                "must be a matrix with two rows."))
    if (any(A[2, ] < 0))
        error(.argument = "A",
              c("The second row of ",
                sQuote("A"),
                "represents probabilities.  Negative values are not allowed."))
    if (! all.equal(sum(A[2,]), 1))
        error(.argument = "A",
              c("The second row of ",
                sQuote("A"),
                "represents probabilities, and they must sum to 1."))
###-------------------------------------------------------------------
    ##  Sanity check `delta`.
    .error_text_delta <- c(
        sQuote("delta"),
        "must either be a single number or a vector corresponding to",
        sQuote("A"),
        " - i.e having one component for each column.") 
    if (! is.numeric(delta))
        error(.argument = "delta",
              .error_text_delta)
    if (length(delta) == 1) {
        delta <- rep(delta, ncol(A))
    } else
        if (length(delta) != ncol(A))
            error(.argument = "delta",
                  .error_text_delta)
    kill(.error_text_delta)
###-------------------------------------------------------------------
    ##  Sanity check `delta_range` when relevant.
    if (! is.null(delta_range)) {
        .error_text_delta_range <- c(
            sQuote("delta_range"),
            "must either be a single number or a vector corresponding to",
            sQuote("A"),
            " - i.e having one component for each column.") 
        if (! is.numeric(delta_range))
            error(.argument = "delta_range",
                  .error_text_delta_range)
        if (length(delta_range) == 1) {
            delta_range <- rep(delta_range, ncol(A))
        } else
            if (length(delta_range) != ncol(A))
                error(.argument = "delta_range",
                      .error_text_delta_range)
        kill(.error_text_delta_range)
    }
###-------------------------------------------------------------------
    ##  Sanity check `alpha`.
    if (! is.numeric(alpha))
        error(.argument = "alpha",
              c(sQuote("alpha"),
                "must be a number."))
    if (length(alpha) == 1) {
        alpha <- rep(alpha, ncol(A))
    } else
        if (length(alpha) != ncol(A))
            error(.argument = "alpha",
                  c(sQuote("alpha"),
                    "must either be a single number or a vector corresponding to",
                    sQuote("A"),
                    " - i.e having one component for each column."))
###-------------------------------------------------------------------
    ##  Sanity check `theta`
    .error_text_theta <- c(
        sQuote("theta"),
        "must either be a single number or a vector corresponding to",
        sQuote("A"),
        " - i.e having one component for each column.  Or it could also be",
        "given as ",
        sQuote("NULL"),
        "(in which case values will be generated uniformly between 0 and 2pi.")
    if (is.null(theta))
        theta <- runif(n = ncol(A), min = 0, max = 2*pi)
    if (! is.numeric(theta))
        error(.argument = "theta",
              .error_text_theta)
    if (length(theta) == 1) {
        theta <- rep(theta, ncol(A))
    } else
        if (length(theta) != ncol(A))
            error(.argument = "theta",
                  .error_text_theta)
    kill(.error_text_theta)
###-------------------------------------------------------------------
    if (is.null(seed))
        seed <- as.integer(paste(
            sample(x = 0:9, size = 9, replace = TRUE),
            collapse = ""))
    set.seed(seed)
###-------------------------------------------------------------------
    ##  Create the basic part of our time series (no `wn` yet).
    t_vec <- 1:n
    ##  Identify the levels for the different parts.
    .levels <- sample(
        x = 1:ncol(A),
        size = n,
        replace = TRUE,
        prob = A[2, ])
    ##  Creating the delta ranges when relevant
    if (! is.null(delta_range))
        .delta_ranges <- vapply(
            X = seq_len(ncol(A)),
            FUN = function(i) 
                range(c(delta[i],
                        delta_range[i])),
            FUN.VALUE = c(min = 0, max = 1))
    ##  Create the resulting vector
    .result <- vapply(
        X = seq_len(n),
        FUN = function(.n) {
            i <- .levels[.n]
            .delta <- ifelse(
                test = is.null(delta_range),
                yes  = delta[i],
                no   = runif(n = 1,
                             min = .delta_ranges["min", i],
                             max = .delta_ranges["max", i]))
            A[1, i] + .delta * cos(alpha[i] * t_vec[.n] + theta[i])
        },
        FUN.VALUE = numeric(1))
###-------------------------------------------------------------------
    ##  Add `wn` when required.
    if (! is.null(wn)) {
        ##  Helper function (copied from other function, perhaps
        ##  create a separate function for this one?)
        .dist_result <- function(.n, dist_info) {
            if (is.null(dist_info))
                return(0)
            ##  Return 0 if '.n' is zero (can happen the way this
            ##  function is called).
            if (.n == 0)
                return(0)
            ##  Create a call for the desired distribution.
            dist_call <- create_call(
                .cc_fun = dist_info$type,
                c(list(n = .n),
                  dist_info$args),
                .cc_list = TRUE)
            ##  Check if this works:
            dist_result <- try(
                expr = eval(dist_call),
                silent = TRUE)
            ##  Stop if someting fishy:
            if ("try-error" %in% class(dist_result))
                stop("\t",
                     "Erroneous argument in '",
                     .this_function,
                     "'.  Not able to evaluate:\n\t\t'",
                     capture.output(dist_call),
                     "'\n",
                     call. = FALSE)
            ##  Return the result to the workflow.
            dist_result - dist_info$adjust
        }
        ##  Add `wn` to the result.
        .result <- .result + .dist_result(.n = n, dist_info = wn)
    }
###-------------------------------------------------------------------
    ##  Return `.result` to the workflow, with an attribute to help
    ##  the creation of an additional time series when a simple
    ##  bivariate case is of interest too, see 'dmt_bivariate'.
    attributes(.result)$args <- list(
                           n = n,
                           A = A,
                           delta = delta,
                           delta_range = delta_range,
                           alpha = alpha,
                           theta = theta,
                           wn = wn,
                           seed = seed)
    .result
}


################################################################################


##  Add the new function to "TS_families".
TS_families <- c(
    TS_families,
    list(dmt =
             list(package = "localgaussSpec",
                  fun = "dmt",
                  args = list(A = rbind(c(-2, -1, 0, 1),
                                        c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                              delta = c(1.0, 0.5, 0.3, 0.5),
                              delta_range = NULL,
                              alpha = c(pi/2, pi/6, pi/3, pi/4),
                              theta = NULL,
                              wn = list(type = "rnorm",
                                        args = list(mean = 0,
                                                    sd = 1),
                                        adjust = 0)),
                              size_name = "n")))


################################################################################
##  Add a simple twist of 'dmt', that enables a bivariate example to
##  be generated.  The basic idea for this initial test is that the
##  two components are computed in succession by the help of 'dmt', I
##  guess that implies that I need a first list of arguments that is
##  equal to the one in 'dmt', and then a second list of arguments
##  that might partially be an adjusted version of the arguments from
##  the first case.  The simplest approach would be to simply phase
##  adjust all of the values a tiny bit...

dmt_bivariate <- function(n,
                          first_dmt,
                          phase_adjustment = NULL) {
    ##  Join 'n' and 'first_dmt' into one list, and then use 'dmt' to
    ##  get the first component
    first_dmt <- c(
        list(n = n),
        first_dmt)
    .first_part <- do.call(
        what = dmt,
        args = first_dmt)
    ##  Extract the (possibly revised) arguments, adjust with
    ##  'phase_adjustment', and create the second component.
    second_dmt <- attributes(.first_part)$args
    second_dmt$theta <- second_dmt$theta + phase_adjustment
    .second_part <- do.call(
        what = dmt,
        args = second_dmt)
    ##  Combine the two pieces.
    cbind(X1 = .first_part,
          x2 = .second_part)
}


##  Add the new function to "TS_families".
TS_families <- c(
    TS_families,
    list(dmt_bivariate =
             list(package = "localgaussSpec",
                  fun = "dmt_bivariate",
                  args = list(first_dmt =
                                  list(A = rbind(c(-2, -1, 0, 1),
                                                 c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                                       delta = c(1.0, 0.5, 0.3, 0.5),
                                       delta_range = NULL,
                                       alpha = c(pi/2, pi/6, pi/3, pi/4),
                                       theta = NULL,
                                       wn = list(type = "rnorm",
                                                 args = list(mean = 0,
                                                             sd = 1),
                                                 adjust = 0)),
                              phase_adjustment = pi/6),
                  size_name = "n")))


## ################################################################################
## #####  2017-04-18: Create a simple function that use rmgarch-code to
## #####  produce simulated values based on some given set of data.
## #####  Example based on the code in 'rmgarch.test3e'.

## #####   Move this crap into a separate file...


## ##  The following example use the alternative 'cgarchsim',
## ##  i.e. "Copula GARCH simulation".  Other simulation alternatives are
## ##  also available, .i.e 'dccsim' (Dynamical Conditional Correlation)
## ##  and 'gogarchsim' (Generalised Orthogonal).  I think these
## ##  alternatives follows a rather similar basic logic with regard to
## ##  the computations, and as such it migt be feasible to make
## ##  adjustments that focus upon these too.

## ##  For the present case, it seems to be that the basic specification
## ##  of the model is due to the functions 'ugarchspec' and 'multispec'
## ##  from the 'rugarch'-package.  These are then for this case used
## ##  together with 'cgarchspec', 'cgarchfit' and 'cgarchsim'.  I
## ##  presume something similar is present for the other cases.

## ##  For the 'dcc'-case, an inspection of the code in the file
## ##  'rmgarch.test5.R' indicates that the the sequence should be
## ##  'dccspec', 'dccfit' and 'dccsim'.  The setup for 'gogarch' (from
## ##  the file 'rmgarch.test1.R') seems to be similar, i.e. it is also
## ##  there 'gogarchspec', 'gogarchfit' and 'gogarchsim', but I wonder
## ##  if the argument to 'gogarchspec' differs a bit from the other two
## ##  cases.  The difference seems to be that the 'gogarchspec' does not 

sample_rmgarch <- function(n.sim = 100,
                           m.sim = 10,
                           uspec = NULL,
                           data = NULL,
                           rseed = NULL) {
    if (! requireNamespace("rmgarch", quietly = TRUE))
        error(c("The package",
                sQuote("rmgarch"),
                "must be loaded for this function to work."))
###-------------------------------------------------------------------
    ##  Sanity-check 'data'
    if (is.null(data))
        error(.argument = "data",
              c("This argument can not be ",
                sQuote("NULL")))
###-------------------------------------------------------------------
    ##  Reminder: 'uspec' should be created in advance, by means of
    ##  'multispec' and 'ugarchspec', but if they are missing, then
    ##  the code below gives a simple default.
    if (is.null(uspec)) {
        uspec <- rugarch::ugarchspec(
                              mean.model = list(armaOrder = c(0,0)),
                              variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                              distribution.model = "norm")
        uspec <- rugarch::multispec(replicate(n = min(dim(data)),
                                              expr = uspec))
    }
###-------------------------------------------------------------------
    ##  Create the object needed for the fitting
    spec1 <- rmgarch::cgarchspec(uspec = uspec,
                                 VAR = TRUE,
                                 robust = FALSE,
                                 lag = 2,
                                 lag.max = NULL, 
                                 lag.criterion = c("AIC", "HQ", "SC", "FPE"),
                                 external.regressors = NULL,
                                 robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                                 dccOrder = c(1,1),
                                 asymmetric = FALSE,
                                 distribution.model = list(copula = c("mvnorm", "mvt")[1], 
                                                           method = c("Kendall", "ML")[2],
                                                           time.varying = TRUE, 
                                                           transformation = c("parametric", "empirical", "spd")[1]))
###-------------------------------------------------------------------
    ##  Find the optimal parameters for the fit to the given data.
    fit1 <- rmgarch::cgarchfit(spec = spec1,
                               data = data,
                               cluster = NULL,
                               fit.control = list(eval.se = FALSE))
###-------------------------------------------------------------------
    ##  Simulate the desired data
    sim1 <- rmgarch::cgarchsim(fit = fit1,
                               n.sim = n.sim,
                               m.sim = m.sim,
                               startMethod = "unconditional",
                               rseed = rseed,
                               cluster = NULL)
###-------------------------------------------------------------------
    ##  Extract and return the part with the simulated data, stuff
    ##  everything into one array, such that the content is given as
    ##  the first dimension, the observations as the second and the
    ##  variables as the third.  The dimension-names will be added
    ##  when this function is called from 'TS_sample'.
    do.call(what = abind::abind,
            args = c(sim1@msim$simRes,
                     along = 0))
}

##  Add the new function to "TS_families".
TS_families <- c(
    TS_families,
    list(sample_rmgarch =
             list(package = "localgaussSpec",
                  fun = "sample_rmgarch",
                  args = list(uspec = NULL,
                              data = NULL,
                              rseed = NULL),
                  size_name = "n.sim")))



## ## ## ## ################################################################################
## ## ## ## #####  Similar setup to the previous one, but now with dccgarch
## ## ## ## #####  instead of 'cgarch'

## #####   Reminder, 2017-04-20: This failed miserably, as the result
## #####   from 'dccsim' only returned 'NULL' where 'cgarchsim' returned
## #####   time series...  The same problem was present in the example in
## #####   the 'rmgarch'-package too, so I will not pursue this any more
## #####   at the moment.


## ## ## ## sample_dccgarch <- function(n.sim = 100,
## ## ## ##                            m.sim = 10,
## ## ## ##                            uspec = NULL,
## ## ## ##                            data = NULL,
## ## ## ##                            rseed = NULL) {
## ## ## ##     if (! requireNamespace("rmgarch", quietly = TRUE))
## ## ## ##         error(c("The package",
## ## ## ##                 sQuote("rmgarch"),
## ## ## ##                 "must be loaded for this function to work."))
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Sanity-check 'data'
## ## ## ##     if (is.null(data))
## ## ## ##         error(.argument = "data",
## ## ## ##               c("This argument can not be ",
## ## ## ##                 sQuote("NULL")))

    
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Reminder: 'uspec' should be created in advance, by means of
## ## ## ##     ##  'multispec' and 'ugarchspec', but if they are missing, then
## ## ## ##     ##  the code below gives a simple default.
## ## ## ##     if (is.null(uspec)) {
## ## ## ##         uspec <- rugarch::ugarchspec(
## ## ## ##                               mean.model = list(armaOrder = c(0,0)),
## ## ## ##                               variance.model = list(garchOrder = c(1,1),
## ## ## ##                                                     model = "sGARCH"), 
## ## ## ##                               distribution.model = "norm")
## ## ## ##         uspec <- rugarch::multispec(replicate(n = min(dim(data)),
## ## ## ##                                               expr = uspec))
## ## ## ##     }
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Create the object needed for the fitting
## ## ## ##     spec1 <- rmgarch::dccspec(uspec = uspec,
## ## ## ##                               dccOrder = c(1,1),
## ## ## ##                               model = "FDCC", 
## ## ## ##                               groups = 1:min(dim(data)),
## ## ## ##                               distribution = "mvnorm")
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Find the optimal parameters for the fit to the given data.
## ## ## ##     fit1 <- rmgarch::dccfit(spec = spec1,
## ## ## ##                             data = data,
## ## ## ##                             solver = "solnp", 
## ## ## ##                             cluster = NULL,
## ## ## ##                             fit.control = list(eval.se = FALSE))
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Simulate the desired data
## ## ## ##     sim1 <- rmgarch::dccsim(fit = fit1,
## ## ## ##                             n.sim = n.sim,
## ## ## ##                             m.sim = m.sim,
## ## ## ##                             startMethod = "unconditional",
## ## ## ##                             rseed = rseed,
## ## ## ##                             cluster = NULL)
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  Extract and return the part with the simulated data, stuff
## ## ## ##     ##  everything into one array, such that the content is given as
## ## ## ##     ##  the first dimension, the observations as the second and the
## ## ## ##     ##  variables as the third.  The dimension-names will be added
## ## ## ##     ##  when this function is called from 'TS_sample'.
## ## ## ##     do.call(what = abind::abind,
## ## ## ##             args = c(sim1@msim$simRes,
## ## ## ##                      along = 0))
## ## ## ## }
    

## ## ## ## ##  Add the new function to "TS_families".
## ## ## ## TS_families <- c(
## ## ## ##     TS_families,
## ## ## ##     list(sample_dccgarch =
## ## ## ##              list(package = "localgaussSpec",
## ## ## ##                   fun = "sample_dccgarch",
## ## ## ##                   args = list(uspec = NULL,
## ## ## ##                               data = NULL,
## ## ## ##                               rseed = NULL),
## ## ## ##                   size_name = "n.sim")))

