################################################################################
##  Function to be added to 'TS_families'.  Procedure: First define
##  the function - and then update 'TS_families' with the new object.

#' Samples built upon cosine.
#'
#' The \code{dmt_extended}-function can create a sample where different
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
#' @param phase_adjustment A vector whose length equals the number of
#'     columns in \code{A}.  This can be used for individual
#'     phase-adjustments.  The default value \code{NULL} triggers only
#'     zeros to be used.
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
#' @name dmt_extended
#' @keywords internal


dmt_extended <- function(
    n,
    A,
    delta,
    delta_range = NULL,
    alpha,
    theta = NULL,
    phase_adjustment = NULL,
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
    ##  Sanity check `phase_adjustment`
    .error_text_phase_adjustment <- c(
        sQuote("phase_adjustment"),
        "must either be a vector (of frequencies) corresponding to",
        sQuote("A"),
        " - i.e having one component for each column.  Or it should be",
        "given as ",
        sQuote("NULL"),
        "(in which zeros are used as default.")
    if (is.null(phase_adjustment))
        phase_adjustment <- rep(0, times = ncol(A))
    if (! is.numeric(phase_adjustment))
        error(.argument = "phase_adjustment",
              .error_text_phase_adjustment)
    if (length(phase_adjustment) != ncol(A))
        error(.argument = "phase_adjustment",
              .error_text_phase_adjustment)
    kill(.error_text_phase_adjustment)
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
            A[1, i] + .delta * cos(alpha[i] * t_vec[.n] + phase_adjustment[i] + theta[i])
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
    ##  bivariate case is of interest too.
    attributes(.result)$args <- list(
                           n = n,
                           A = A,
                           delta = delta,
                           delta_range = delta_range,
                           alpha = alpha,
                           theta = theta,
                           phase_adjustment = phase_adjustment,
                           wn = wn,
                           seed = seed)
    .result
}


################################################################################


##  Add the new function to "TS_families".
TS_families <- c(
    TS_families,
    list(dmt_extended =
             list(package = "LocalCode",
                  fun = "dmt_extended",
                  args = list(A = rbind(c(-2, -1, 0, 1),
                                        c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                              delta = c(1.0, 0.5, 0.3, 0.5),
                              delta_range = NULL,
                              alpha = c(pi/2, pi/6, pi/3, pi/4),
                              theta = NULL,
                              phase_adjustment = NULL,
                              wn = list(type = "rnorm",
                                        args = list(mean = 0,
                                                    sd = 1),
                                        adjust = 0)),
                              size_name = "n")))

