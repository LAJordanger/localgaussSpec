#' Time Series Families
#'
#' @description This is a list of lists.  The sublists contain the key
#'     details needed for a simple unified setup for all the
#'     implemented models, i.e. that the resulting samples will be
#'     presented in a standardised format.  In addition, this setup
#'     ensures that all the arguments (both given and default values)
#'     will be properly recorded in the corresponding info-file.
#'
#' @format A list of lists, the names at the top-level will be used as
#'     the available options for the \code{TS_key}-argument in the
#'     \code{TS_sample}-function.  The sublist for a given
#'     \code{TS_key} contains the parts \code{fun}, \code{package},
#'     \code{args}, \code{size_name} and \code{method}, as described
#'     below:
#'
#' \describe{
#'
#' \item{fun}{The name of the desired function, as a
#'     character-string.}
#'
#' \item{package}{The name of the package that contains \code{fun}, as
#'     a character-string.}
#'
#' \item{args}{A list containing the default arguments to be used in
#'     \code{fun}.  Reminder: In order for the package to compile, it
#'     might be necessary to quote the list if there are references to
#'     packages not imported into this package.}
#'
#' \item{size_name}{The name of the argument of \code{fun} that
#'     specifies the number of samples to be created.}
#'
#' \item{method}{A character string that has to be specified if
#'     \code{fun} needs to be "extracted" from some deeper level of
#'     \code{package}.  Simply drop this part when it is superfluous.}
#'
#' }
#'
#' @name TS_families
#'
#' @keywords internal

TS_families <- list()

###------------------------------------------------------###

#' The univariate Gaussian case for \code{TS_key}
#'
#' @description The \code{TS_key_rnorm}-case part of \code{TS_families}
#'     is simply an implementation of \code{rmnorm} from the
#'     \code{stats}-package.
#'
#' @note The arguments \code{mean} and \code{sd} should be given to
#'     \code{TS_sample}, which will send them to this function by the
#'     internal helper-function \code{TS_sample_helper}.
#'
#' @name TS_key_rnorm

TS_families$rnorm <- list(
    package = "stats",
    fun = "rnorm",
    args = list(mean = 0,
                sd = 1),
    size_name = "n")

###------------------------------------------------------###

#' The bivariate Gaussian case for \code{TS_key}
#'
#' @description The \code{TS_key_rmvnorm}-case part of
#'     \code{TS_families} is simply an implementation of
#'     \code{rmvnorm} from the \code{mvtnorm}-package.
#'
#' @note The arguments \code{mean}, \code{sigma}, \code{method} and
#'     \code{pre0.9_9994} should be given to \code{TS_sample}, which
#'     will send them to this function by the internal helper-function
#'     \code{TS_sample_helper}.
#'
#' @name TS_key_rmvnorm

TS_families$rmvnorm <- list(
    package = "mvtnorm",
    fun = "rmvnorm",
    args = list(mean = rep(0, length(as.name("sigma"))),
                sigma = diag(length(as.name("mean"))),
                method = c("eigen", "svd", "chol"),
                pre0.9_9994 = FALSE),
    size_name = "n")

###------------------------------------------------------###

#' The univariate GARCH case for \code{TS_key}
#'
#' @description The \code{TS_key_rugarch}-case part of
#'     \code{TS_families} is simply an implementation of
#'     \code{ugarchpath} from the \code{rugarch}-package.
#'
#' @note The arguments \code{spec}, \code{n.start}, \code{m.start},
#'     \code{presigma}, \code{preresiduals}, \code{rseed},
#'     \code{custom.dist}, \code{mexsimdata} and \code{vexsimdata}
#'     should be given to \code{TS_sample}, which will send them to
#'     this function by the internal helper-function
#'     \code{TS_sample_helper}.
#'
#' @name TS_key_rugarch

TS_families$rugarch <- list(
    package = "rugarch",
    fun = "ugarchpath",
    args = list(spec = quote(ugarchspec(
                    variance.model = list(
                        model="sGARCH",
                        garchOrder=c(1,1)),
                    mean.model=list(
                        armaOrder=c(0,0),
                        include.mean=TRUE),
                    distribution.model="sstd",
                    fixed.pars=list(mu=0.001,
                                    omega=0.00001,
                                    alpha1=0.05,
                                    beta1=0.90,
                                    shape=4,
                                    skew=2))),
                n.sim = 1000,
                n.start = 0,
                m.sim = 1,
                presigma = NA,
                prereturns = NA,
                preresiduals = NA,
                rseed = NA,
                custom.dist = quote(list(name = NA, distfit = NA)),
                mexsimdata = NULL,
                vexsimdata = NULL),
    size_name = "n.sim",
    method = "uGARCHspec")

###------------------------------------------------------###

#' The bivariate Gaussian case for \code{TS_key}
#'
#' @description The \code{TS_key_arima.sim}-case part of
#'     \code{TS_families} is simply an implementation of
#'     \code{arima.sim} from the \code{stats}-package.
#'
#' @note The arguments \code{model} and \code{rand.gen} should be
#'     given to \code{TS_sample}, which will send them to this
#'     function by the internal helper-function
#'     \code{TS_sample_helper}.
#'
#' @name TS_key_arima.sim


TS_families$arima.sim <- list(
    package = "stats",
    fun = "arima.sim",
    args = list(model = list(
                    ar = 0.5,
                    ma = 0.8),
                rand.gen = rnorm),
    size_name = "n")
