################################################################################
#####  2015-04-28

##  TODO: Adjustment needed in order to cope with the
##  'leave-out-interval' algorithm that should be used when dealing
##  with dependent data.

#' Estimate local correlation \eqn{$\rho$} between two variables.
#'
#' This function use the local likelihood function proposed by Hjort &
#' Jones (1996) to find an estimate of the local correlation
#' \eqn{$\rho$} given \eqn{$n$} bivariate observations and a vector
#' \eqn{$b$} with the specified bandwidths.  Note that the code use
#' the simplifying assumption that the the marginals are standard
#' normal, which reduces the number of parameters to estimate from
#' five to one.  This function is based on code from H{\\aa}kon
#' Otneim.
#'
#' @details In the computation, a product kernel that either is based
#' on the standard normal distribution or the uniform distribution is
#' used.  The default kernel is the normal one, since it seems to
#' behave better than the uniform kernel with regard to being able to
#' get a proper result (i.e. not \code{NA}) and since it also seems to
#' compute a bit faster when a computation is possible.  Note that the
#' code computes the required densities (related to the normal
#' distribution) directly instead of using \code{dnorm} and
#' \code{dmvnorm}, since that gives a speed gain.
#'
#' @references Hjort, N. L., and Jones, M. C.: "Locally parametric
#' nonparametric density estimation.", The Annals of Statistics
#' (1996): 1619-1647.
#'
#' @param data A matrix containing the observations, one bivariate
#' observation in each row.
#'
#' @param grid A matrix containing the points where we want to find
#' the local correlations, one bivariate point in each row.
#'
#' @param b A vector containing the two bandwidhts to use.
#'
#' @param .kernel Specification of the kernel to use, either
#' \code{normal} or \code{uniform}, with default being the
#' \code{normal} one if none is selected.  Note: The
#' \code{uniform}-kernel takes much longer to compute (a factor of
#' ten when used on a small sample of size \eqn{$n=200$}).
#'
#' @param return_arguments Logical argument, default \code{FALSE},
#' that can be used to include the three arguments \code{data},
#' \code{grid} and \code{b} in the result from this function.
#'
#' @return The result of this function is a list.  For each grid-point
#' there will be an estimate of the local correlation and the
#' log-density of the corresponding local Gaussian approximation at
#' that point.  When \code{return_arguments} is \code{TRUE}, the three
#' arguments \code{data}, \code{grid} and \code{b} will also be
#' included.
#' 
#' @keywords internal


#####  2015-06-12: TODO:
##  This function should have the option to run the computations on
##  the grid in parallel, but it's not something to use time on today.

find_rho <- function(data,
                     grid,
                     b,
                     .kernel = c("normal", "uniform"),
                     return_arguments = FALSE) {
###-------------------------------------------------------------------
    ##  Restrict to the first alternative of '.kernel'.
    .kernel <- .kernel[1]
###-------------------------------------------------------------------
    ##  Extract the data and the bandwidths from 'data' and 'b'.
    x <- data[,1]
    y <- data[,2]
    ##---
    b1 <- b[1]
    b2 <- b[2]
###-------------------------------------------------------------------
    ##  If 'x' and 'y' are identical, then no computation should be
    ##  performed (all the points lies on the diagonal).  In this case
    ##  return the value '1' for 'par.est' and a 'NA' for 'log_f.est'
    if (identical(x, y)) 
        return(list(
            par.est = 1,
            log_f.est = NA_real_))
###-------------------------------------------------------------------
    ##  Compute some values that we don't need to compute every time
    ##  we want to consider a new grid-point.
    xx_yy <- x^2 + y^2
    xy <- x * y
###-------------------------------------------------------------------
    ##  Create the key-function that maximises the desired local
    ##  likelihood function for a given grid-point.
    maximise.likelihood <- function(grid.point) {
        ##  Extract the coordinates.
        x0 <- grid.point[1]
        y0 <- grid.point[2]
        ##  Compute a bunch of constants (not depending on rho) that
        ##  will be used repeatedly in the optimisation.  Reminder:
        ##  There's a cost of looking up values too, so the gain from
        ##  this step might be questionable since these are "trivial"
        ##  operations...
        x0x0 <- x0^2
        y0y0 <- y0^2
        x0y0.2 <- 2*x0*y0
        .log2pi <- log(2*pi)
###-------------------------------------------------------------------
        ##  Stuff to use when '.kernel' is given as "normal".
        if (identical(.kernel, "normal")) {
            .b1_2 <- 1 + b1^2
            .b2_2 <- 1 + b2^2
            pi.2 <- 2 * pi
            ##  Some weights.
            W <- exp(-(((x-x0)/b1)^2 + ((y-y0)/b2)^2)/2) / (2 * pi * b1 * b2)
            m1 <- mean(W)
            m23 <- mean(W * xx_yy)
            m4.2 <- 2 *  mean(W * xy)
        } else {
            ##  The case when '.kernel' is given as "uniform".
            inside_bandwidth_rectangle <- all(
                abs(x - x0) <= b1,
                abs(y - y0) <= b2)
            ##  Some weights.
            m1 <- 1/ dim(data)[1]
            m23 <- m1 * sum(xx_yy[inside_bandwidth_rectangle])
            m4.2 <- 2 * m1 * sum(xy[inside_bandwidth_rectangle])
        }
        ##  The likelihood function, depending on '.kernel'.
        lik <-
             if (identical(.kernel, "normal")) {
                 function(rho) {
                     .rho2 <- 1-rho^2
                     .tmp <- .b1_2 * .b2_2 - rho^2
                     ##---
                     (- .log2pi - 0.5*log(.rho2)) * m1 - (m23 - rho*m4.2)/(2*.rho2) -
                         exp(-(.b2_2*x0x0 + .b1_2*y0y0 - x0y0.2*rho)/(2*.tmp))/
                             (pi.2 * sqrt(.tmp))
                 }
             } else
                 ##  The case "uniform", ignore common scaling factor
                 ##  of '1/(4*b1*b2)' from the computation.
                 function(rho) {
                     .rho2 <- 1-rho^2
                     ##---
                     (- .log2pi - 0.5*log(.rho2)) * m1 - (m23 - rho*m4.2)/(2*.rho2) -
                         pmvnorm(lower = c(x0 - b1, y0 - b2),
                                 upper = c(x0 + b1, y0 + b2),
                                 mean = c(0,0),
                                 corr = matrix(
                                     data = c(1, rho, rho, 1),
                                     nrow = 2))
                 }
        ##  Optimise the likelihood.
        opt <- try(optimise(lik,
                            lower = -1,
                            upper= 1,
                            maximum = TRUE,
                            tol = .Machine$double.eps^0.25/1e4),
                   silent=TRUE)
        ##  Return the maximum of the likelihood and the log-density
        ##  estimate when successfully computed, otherwise return
        ##  'NA'-values to the work-flow.
        if(class(opt) != "try-error") {
            c(opt$maximum,
              dmvnorm(x = c(x0, y0),
                      mean = c(0,0),
                      sigma = matrix(
                          data = c(1, opt$maximum, opt$maximum, 1),
                          nrow = 2),
                      log = TRUE))
        } else {
            c(NA_real_, NA_real_)
        }
    }
###-------------------------------------------------------------------
    ##  Use 'maximise.likelihood' on all the grid-points.
    est <- apply(
        X = grid,
        MARGIN = 1,
        FUN = maximise.likelihood)
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    c(list(par.est = c(rho = est[1, ]),
           log_f.est = est[2, ]),
      if (return_arguments)
          list(kernel = kernel,
               b = b,
               grid = grid,
               data = data))
}


##  And finally, compile the function in order to get it a bit faster.

## require(compiler)
## find_rho <- cmpfun(find_rho)
#####  Skip this when the package is installed.

#####  Reminder: I'm a bit nonplussed with regard to the possibility
#####  that a lot of NA-values turns up here, but I guess it's due to
#####  the possibility that the optimisation might attempt to test
#####  values outside of the specified boundaries (althoug I think
#####  that seems a tad bit strange that such a thing should occur).
#####  It might be of interest to investiage the effect of other
#####  optimisers too, e.g. 'nls' (non-linear least squares) but that
#####  will have to wait until another time.  For the time being I
#####  think this is sufficient.

################################################################################
#####  2015-05-29, reminder

##  I naively thought using optim with an expression for the gradient
##  might be a bit faster, but that turned out to be a pipe-dream.  It
##  did instead increase the computational time quite a bit.  I do
##  however, find the solution I used worthwhile to write down, as it
##  in a rather nice manner took care of a lot of the pesky detail, in
##  particular if I later on would like to make an investigation with
##  regard to how stuff behaves if we want to investigate the severity
##  of the assumption of normalised marginals.

##  The main point is to find an expression that spits out the
##  gradients, without the need for hours of computations by hand.
##  The key to this is to use 'deriv' to create a function that spits
##  out the value of the function we want to optimise, with the
##  gradients as an attribute.  By the help of a simple
##  storage-environment, the rest is then very simple.


################################################################################
#####  It might be that I can get stuff to work in a decent manner by
#####  using a piece of creativity when it comes to the creation of
#####  this code.


##  To test the principle, I guess I need to play around with a
##  somewhat simpler example, where a bunch of the values are simply
##  given from scratch.


###  Step one, create an expression for the desired function (taken
###  from the body of 'find_rho').

## .fun_expression <- expression(
##     - .log2pi   - 0.5*log((1 - rho^2)) * m1 - (m23 - rho*m4.2)/(2*(1 - rho^2)) - exp(-(.b2_2*x0x0 + .b1_2*y0y0 - x0y0.2*rho)/(2*(.b1_2 * .b2_2 - rho^2)))/ (pi.2 * sqrt((.b1_2 * .b2_2 - rho^2))) )

##  Create a function from this, using 'deriv':
## .fun_test <- deriv(
##     expr = .fun_expression,
##     namevec = "rho",
##     function.arg = TRUE)

###  Test it
## .fun_result <- .fun_test(0.4)

###  Create a help-environment to get stuff to work like I want.
## store_env <- new.env()



#####  Create a function to do the main computation, including an
#####  update of the storage-environment.
## fr <- function(rho, env) { 
##     env$result <- .fun_test(rho)
##     as.vector(env$result)
## }

#####  Test that it works.
## fr(rho = .4, env = store_env)

## store_env$result


#####  Creata a function for the gradient, where 'rho' now simply is
#####  needed in order to fit into the machinery, whereas 'env' is the
#####  key.  Pick out the desired value from the attriubte "gradient".

## gr <- function(rho, env) { 
##     attributes(env$result)$gradient
## }

#####  Test that it works
## gr(.4, env = store_env)

#####  Test that it works.
## optim(par = 0,
##       fn = fr,
##       gr = gr,
##       env = store_env,
##       method = "L-BFGS-B",
##       lower = -.995,
##       upper = .995,
##       control = list(fnscale = -1))

#####  This gave a very simple way of writing the code, but as it used
#####  longer time than the old version, it will not be implemented
#####  when there's only one parameter to estimate.  However, I think
#####  I will return to this later on in order to make a more advanced
#####  version that might be used to estimate all of the parameters
#####  without the assumption of normalised marginals.  This will
#####  however, as far as I can see, only be a good idea to attempt
#####  for the case of a "normal" kernel, but that is as such not a
#####  problem.

#####  2015-12-18: Update: The reasoing above did give a method for
#####  computing this using all the parameters, but its way more
#####  efficient to use 'localgauss' if that is the goal...
