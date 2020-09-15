#' A simple multivariate GARCH-models from the \code{rmgarch}-package.
#'
#' @description The \code{TS_key_rmgarch}-function is in essence a
#'     wrapper around the \code{cgarchsim}-function from the
#'     \code{rmgarch}-package, i.e. this is based on the
#'     "Copula GARCH simulation".  The key idea is that the user can
#'     feed this wrapper the multivariate sample of interest (the
#'     \code{data}-argument), and a specification of the univariate
#'     GARCH-model, and then this function will take care of the rest.
#'
#' @note This internal function has been added to the
#'     \code{TS_families}-list, and its arguments should thus be given
#'     to \code{TS_sample}, which will send them to this function by
#'     the internal helper-function \code{TS_sample_helper}.
#'
#' @note The \code{rmgarch}-package also contains other simulation
#'     alternatives, but these have not been implemented.  These
#'     alternatives are 'dccsim' (Dynamical Conditional Correlation)
#'     and 'gogarchsim' (Generalised Orthogonal).
#'
#' @param n.sim The simulation horizon.  Default value 100.
#'
#' @param m.sim The number of simulations.  Default value 10.
#'
#' @param uspec The specification of the GARCH-model for the
#'     univariate margins. Default value \code{NULL}.  If this is not
#'     specified, then \code{ugarchspec} from the
#'     \code{rugarch}-package will be used to specify a trivial model.
#'
#' @param data The multivariate time series sample.  The parameters of
#'     interest for the multivariate GARCH-model will be extracted
#'     from the model fitted to this sample.
#'
#' @param rseed Optional seeding value(s) for the random number
#'     generator.  This should be of length equal to m.sim.
#'
#' @return A multivariate GARCH-type time series is returned.
#'
#' @keywords internal

TS_key_rmgarch <- function(n.sim = 100,
                    m.sim = 10,
                    uspec = NULL,
                    data = NULL,
                    rseed = NULL) {
    if (! requireNamespace("rmgarch", quietly = TRUE))
        error(c("The package",
                sQuote("rmgarch"),
                "must be loaded for this function to work."))
    ##  Sanity-check 'data'
    if (is.null(data))
        error(.argument = "data",
              c("This argument can not be ",
                sQuote("NULL")))
    ##  Reminder: 'uspec' should be created in advance, by means of
    ##  'multispec' and 'ugarchspec', but if they are missing, then
    ##  the code below gives a simple default.
    if (is.null(uspec)) {
        uspec <- rugarch::ugarchspec(
            mean.model = list(armaOrder = c(0,0)),
            variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
            distribution.model = "norm")
        uspec <- rugarch::multispec(
            replicate(n = min(dim(data)),
                      expr = uspec))
    }
    ##  Create the object needed for the fitting
    spec1 <- rmgarch::cgarchspec(
        uspec = uspec,
        VAR = TRUE,
        robust = FALSE,
        lag = 2,
        lag.max = NULL, 
        lag.criterion = c("AIC", "HQ", "SC", "FPE"),
        external.regressors = NULL,
        robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
        dccOrder = c(1,1),
        asymmetric = FALSE,
        distribution.model = list(
            copula = c("mvnorm", "mvt")[1], 
            method = c("Kendall", "ML")[2],
            time.varying = TRUE, 
            transformation = c("parametric", "empirical", "spd")[1]))
    ##  Find the optimal parameters for the fit to the given data.
    fit1 <- rmgarch::cgarchfit(
        spec = spec1,
        data = data,
        cluster = NULL,
        fit.control = list(eval.se = FALSE))
    ##  Simulate the desired data
    sim1 <- rmgarch::cgarchsim(
        fit = fit1,
        n.sim = n.sim,
        m.sim = m.sim,
        startMethod = "unconditional",
        rseed = rseed,
        cluster = NULL)
    ##  Extract and return the part with the simulated data, stuff
    ##  everything into one array, such that the content is given as
    ##  the first dimension, the observations as the second and the
    ##  variables as the third.  The dimension-names will be added
    ##  when this function is called from 'TS_sample'.
    do.call(what = abind::abind,
            args = c(sim1@msim$simRes,
                     along = 0))
}

###------------------------------------------------------###

##  Add the new function to "TS_families".  Initiate it if it does not
##  exists.

if (! exists(x="TS_families", inherits = FALSE))
    TS_families <- list()

TS_families$rmgarch  <- 
    list(package = "localgaussSpec",
         fun = "TS_key_rmgarch",
         args = list(uspec = NULL,
                     data = NULL,
                     rseed = NULL),
         size_name = "n.sim")
