################################################################################
#####  2014-07-30
##  The following list of time series and their default values will
##  hopefully simplify the use later on.  (At the time being, this is
##  rather boring, since it only contains one element...)

################################################################################
##  I think an invisible list of internal help-functions might be
##  helpful with regard to the construction of the list "TS_families",
##  and might moreover simplify later functions...

.TS <- list(
    add.new <- function(key, description, TS.fun){
        ## For the addition of new functions to 'TS_families'    
    },
    valid <- function(){
        ##  Code to create a list of valid keys and descriptions
    }
    )        


#####  2016-02-01
##  An ad hoc function to investigate a simple ARCH-model

.ARCH <- function(N,
                  error_type = "rnorm",
                  error_args = list(mean = 0,
                                    sd = 1),
                  x0 = runif(n = 1, min = -1, max = 1),
                  c = 0.5) {
    ##  Create the "error"" vector.
    error_fun <- get(error_type)
    error_call <- create_call(
        .cc_fun = error_fun,
        c(error_args,
          list(n = N)),
        .cc_list = TRUE)
    e_vec <- eval(error_call)
    ##  Create the vector of observations
    x_vec <- e_vec
    x_vec[1] <- e_vec[1] * sqrt(1 + c * x0^2)
    for (i in 2:N)
        x_vec[i] <- e_vec[i] * sqrt(1 + c * x_vec[i-1]^2)
    ##  Return the result to the work-flow.
    x_vec
}

##  An alternative version: X_{t-1} = e_t * h_t, with h_t^2 = a + b*h_{t-1}^2 + c*x_{t-1}^2
###  Proposed values from D.T: b = 0.95, c = 0.02, but those did not
###  give a clear dependence for the squared values...

.ARCH2 <- function(N,
                   error_type = "rnorm",
                   error_args = list(mean = 0,
                                     sd = 1),
                   x0 = runif(n = 1, min = -1, max = 1),
                   h0 = runif(n = 1, min = 0.5, max = 1),
                   a = 0.1,
                   b = 0.5,
                   c = 0.5) {
    ##  Create the "error"" vector.
    error_fun <- get(error_type)
    error_call <- create_call(
        .cc_fun = error_fun,
        c(error_args,
          list(n = N)),
        .cc_list = TRUE)
    e_vec <- eval(error_call)
    ##  Create the vector of observations
    x_vec <- h_vec <- rep(NA_real_, length.out = length(e_vec))
    h_vec[1] <- sqrt(a + b * h0^2 + c * x0^2)
    x_vec[1] <- e_vec[1] * h_vec[1]
    for (i in 2:N) {
        h_vec[i] <- sqrt(a + b * h_vec[i-1]^2 + c * x_vec[i-1]^2)
        x_vec[i] <- e_vec[i] * h_vec[i]
    }
    ##  Return the result to the work-flow.
    x_vec
}



#####  Not quite sure about the setting of the parameters.

## x <- 3:52/52
## for (i in 1:49) {
##     a <- .5
##     b <- min(x[i], 0.99999)
##     c <- max(0.97 - b, 0.00001)
##     print(c(b = b, c = c))
##     test <- .ARCH2(N = 1100, a = a, b = b, c = c)[-c(1:100)]
##     ## test <- .ARCH2(N = 10100, a = a, b = b, c = c)[-c(1:100)]
##     par(mfrow = c(2, 2))
##     plot(as.ts(test))
##     acf(test, lag.max = 100)
##     acf(test^2, lag.max = 100)
##     acf(abs(test), lag.max = 100)
##     par(mfrow = c(1, 1))
##     Sys.sleep(.5)
## }


##  For the time being, this is to simply be a placeholder for this.
################################################################################


## ################################################################################
## #####  2016-02-21 There's some details with my '.ARCH2' that (should
## #####  have been named 'GARCH') that indicates that I might have
## #####  messed up something.  The package 'rugarch' seems to avoid the
## #####  strange behaviour encountered for '.ARCH2', and it also
## #####  included a method to compute the halflife of something in the
## #####  fitted objects.  I suspect that might be connected to this.

## ################################################################################

## #' Working upon fitted models from \code{rugarch}.
## #'
## #' This is a function that is intented to simplify the code in this
## #' package, so I can focus solely on using \code{ugarchpath} and not
## #' \code{ugarchsim}.
## #'
## #' @param fit The result of the function \code{ugarchfit}.
## #'
## #' @return A specification that can be used by \code{ugarchpath}.
## #'
## #' @export

## LG_rucharch_fitting <- function(fit) {
##     ##  Extract specification used when 'fit' was created.
##     .spec <- rugarch::getspec(object = fit)
##     ##  Extract the coeffisients.
##     .coef <- coef(object = fit)
##     ##  Update the 'pars' part of 'model'.
##     .pars <- .spec@model$pars
##     .pars[names(.coef), "Level"] <- .coef
##     .pars[, "Fixed"] <- .pars[, "Fixed"] + .pars[, "Estimate"]
##     .pars[, "Estimate"] <- 0
##     .spec@model$pars <- .pars
##     ##  Update/create the component 'fixed.pars' of 'model'.
##     .spec@model$fixed.pars <- as.list(.coef)
##     ##  Return the result to the work-flow.    
## }


#####  Some of the most basic stuff should be moved to the initiation
#####  given in the file 'LGSD.R'


TS_families <-
    list(WNG = list(package = "stats",
                    fun = "rnorm",
                    args = list(mean = 0,
                                sd = 1),
                    size_name = "n"),
         ARCH = list(package = "localgaussSpec",
                    fun = ".ARCH",
                    args = list(error_type = "rnorm",
                                error_args = list(mean = 0,
                                                  sd = 1),
                                x0 = runif(n = 1, min = -1, max = 1),
                                c = 0.5),
                    size_name = "N"),
         ARCH2 = list(package = "localgaussSpec",
                      fun = ".ARCH2",
                      args = list(error_type = "rnorm",
                                error_args = list(mean = 0,
                                                  sd = 1),
                                x0 = runif(n = 1, min = -1, max = 1),
                                a = 0.1,
                                b = 0.5,
                                c = 0.5),
                    size_name = "N"),
         rmvnorm = list(package = "mvtnorm",
                        fun = "rmvnorm",
                        args = list(mean = rep(0, length(as.name("sigma"))),
                                    sigma = diag(length(as.name("mean"))),
                                    method = c("eigen", "svd", "chol"),
                                    pre0.9_9994 = FALSE),
                        size_name = "n"),
         rugarch = list(package = "rugarch",
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
         )



##  A function to take care of some testing purposes.

wacky_sample <- function(n,
                         omega = 0.2,
                         delta = 1,
##                         seed = NULL,
                         width1 = 2,
                         width2 = 0.05,
                         wn_mean = 0,
                         wn_sd = 0.5) {
    ##  Take care of the randomness part.
    ## ## ## if (is.null(seed))
    ## ## ##     seed <- sample(x = 0:9, )
    ## ## ## set.seed(seed = seed)
##### Reminder: The setting of the seed inside of the function messed
##### up big time, and I guess that has to be included as a reminder somewhere.

    
## ToolBox::set_seed(
## 	.seed = NULL,
## 	 kind = "Mersenne-Twister",
## 	 normal.kind = "Inversion",
## 	 vstr = "3.2.2")

    e_vec <- sample(x = c(-1,1) * width1, size = n, replace = TRUE) +
        runif(n = n, min = -width2, max = width2)
    wn_vec <- rnorm(n = n, mean = wn_mean, sd = wn_sd)
    ##  Create and return the sample
    t_vec <- 1:n
    e_vec * sin(omega * t_vec  + delta) + wn_vec
}
##   Reminder: This did not give a stationary time series, since the
##   variance depends on the times in t_vec.


##  Extend "TS_families" with new stuff.
TS_families <- c(
    TS_families,
    list(wacky  =  list(package = "localgaussSpec",
                       fun = "wacky_sample",
                       args = list(omega = 0.2,
                                   delta = 1,
                                   seed = 1,
                                   width1 = 2,
                                   width2 = 0.05,
                                   wn_mean = 0,
                                   wn_sd = 0.5),
                       size_name = "n")))



##   NOTE: Alternativ solution implemented.  See
##   "TS_family_wacky_baccy.R" for new function.


TS_families <- c(
    TS_families,
    list(arma  =  list(package = "stats",
                       fun = "arima.sim",
                       args = list(model = list(
                                       ar = 0.5,
                                       ma = 0.8),
                                   rand.gen = rnorm),
                       size_name = "n")))





## TS_families <- list(
##     WNG = list(
##         fun = rnorm,
##         name=  "rnorm",
##         args = list(
##             mean = 0,
##             sd = 1)),
##     AR = list(
##         fun = arima.sim,
##         name = "arima.sim",
##         args = list(
##             order = c(1, 0 ,0 ),
##             ar = 0.5))
##     )

#####  Reminder: The innovations in arima.sim comes from N(0,1), and
#####  as such I think that should also be reflected in the code
#####  (later on).




#####---------------------------------------------------------------------------

#' My Time Series
#'
#' @details Given a key for a time series in \code{TS_families}, and
#'     relevant parameters for it (delivered by the
#'     dotsMethods-argument \code{...}), this function will return the
#'     function that will be used in \code{myTS.sample} to generate
#'     the desired sample from the specified time series.
#'
#' @param TS_key character, key that selects one of the time series
#'     from \code{TS_families}
#' 
#' @param ...  dotsMethods (i.e. optional), any arguments to be
#'     delivered to the function selected by \code{TS_key}.
#' 
#' @keywords time series
#' 
#' @return \code{fun}, a function that can generate the desired time
#'     series.  Note that the formals of function selected by
#'     \code{TS_key} will have been updated with any the parameters
#'     given via the \code{dotsMethods}-argument.
#' 
#' @return \code{args.i}, a list of the (updated) default values that
#'     are used by the function in \code{fun}.
#' 
#' @return \code{args.f}, a character-string of the default values,
#'     that can be used in the file-name for the data-file generated
#'     from the time series.
#' 
#' @export

##  Note: The function contains a couple of sanity-checks, i.e. it
##  checks that we have a valid TS_key and a check of the names of
##  the parameters given by the dotsMethods is also performed.

myTS <- function(TS_key, ...){
    if (missing(TS_key))
        error(.argument = "TS_key",
              c("Specify a time series!",
                "Valid time series are:",
                paste(names(TS_families), collapse  = ", ")))
    ##  Find the correct time series
    fam <- pmatch(TS_key, names(TS_families), -1)
    if (fam == -1)
        error(.argument = "TS_key",
              c("Unknown value!",
                "Valid keys are:",
                paste(names(TS_families), collapse  = ", ")))
    .fun_data <- TS_families[[fam]]
#####  Reminder: For 'rugarch' the 'spec' will here be the default,
#####  which is not optimal.  This and the other explicit features
#####  should be fixed in this function, and not in the functions
#####  later on.  However, that can be done later on.
    ##  Investigate if the package is available, and perform action.
    .package_available <- eval(bquote(
        requireNamespace(package = .(.fun_data$package),
                         quietly = TRUE)))
    if (.package_available) {
        ##  Create a local copy of the desired function.
        .fun <- do.call(what = ":::",
                        list(.fun_data$package,
                             .fun_data$fun))
        ##  Record the default values for parameters
        args <- formals(.fun)
        ##  Update 'args' with args from 'fun_data'.
        for (.name in names(.fun_data$arg))
            if (is.null(.fun_data$arg[[.name]])) {
                args[.name] <- list(NULL)
            } else
                args[[.name]] <- .fun_data$arg[[.name]]
        ##  Extract the function component for the desired method when
        ##  an S4 object is encountered.
        if (! is.null(.fun_data$method)) {
            .fun <- findMethods(.fun)[[.fun_data$method]]
        }
    } else
        error(.argument = "TS_key",
              c("The package",
                sQuote(.fun_data$package),
                "must be installed when",
                sQuote("TS_key"),
                "is given as",
                sQuote(TS_key)))
    kill(fam, .package_available)
    ##  Investigate any arguments given to this function.
    given.args <- list(...)
    ##  Ensure that no defaults are present for the sample-size.
    args[[.fun_data$size_name]] <- quote(expr = )
    given.args[[.fun_data$size_name]] <- NULL
    ## 
    if (length(given.args) > 0) {
        ##  Sanity check
        if (prod(names(given.args) %in%
                 names(args)) == 0) {
            extra.args <- given.args
            for (par in names(args)) extra.args[[par]] <- NULL
            stop("\t",
                 "In 'myTS', the argument",
                 ifelse(test = length(extra.args) == 1,
                        yes = "",
                        no = "s "),
                 paste(names(extra.args),
                       extra.args, sep="=", collapse=", "),
                 "\n\t",
                 "doesn't work with the selection '",
                 TS_key,
                 "'.\n",
                 call. = FALSE)
        }
###############
#####  Ad hoc solution for the arg_list arguments of `wacky_baccy` and
#####  `chillum` only for the case with the value of `beta`
        if (.fun_data$fun %in% c("wacky_baccy", "chillum"))
            for (i in seq_along(given.args$args_cos_sin)) {
                ##  If `beta` unspecified, add value from `args`
                .default_beta <- args$args_cos_sin[[1]]$beta
                if (! "beta" %in% names(given.args$args_cos_sin[[i]]))
                    given.args$args_cos_sin[[i]]$beta <- .default_beta
            }
###############
        ##  Create list of arguments to be used in info and file-name.
        for (par in names(given.args))
            if (is.null(given.args[[par]])) {
                args[par] <- list(NULL)
            } else
                args[[par]] <- given.args[[par]]
        kill(par)
    }
    ##  Update the formals of the chosen function.
    formals(.fun) <- args
    ##  Create args.i and args.f, remove the part corresponding to the
    ##  undefined sample-size.
    args.i <- args[names(args) != .fun_data$size_name]
    args.f <- paste(names(args.i), args.i, sep="=", collapse=".")
    ##  Remove any blank characters.
    args.f <- str_replace_all(
        string = args.f,
        pattern = "\ ",
        replacement = "")
###############
    ##  Adjustment when 'rugarch'
    if (TS_key == "rugarch")
        .fun_data$args$spec <- given.args$spec
    ##  Adjustment when 'sample_rmgarch' or 'sample_dccgarch'.
    ## ## ## ## if (TS_key == "sample_rmgarch") {
    if (TS_key %in% c("sample_rmgarch", "sample_dccgarch")) {
        .fun_data$args$uspec <- given.args$uspec
        .fun_data$args$data <- given.args$data
    }
    ## Return the function with additional information to the workflow.
    list(fun = .fun,
         args.i = args.i,
         args.f = args.f,
         TS_key = TS_key,
         fun_data = .fun_data)
}
