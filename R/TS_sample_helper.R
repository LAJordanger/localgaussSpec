#' Helper-function for \code{TS_sample}
#'
#' @description This internal function is a helper-function for
#'     \code{TS_sample}, which based on the \code{TS_key} and the
#'     specified arguments performs an initial sanity test and then
#'     returns the function needed for the desired simulation.
#'
#' @details Given a \code{TS_key} (the name of a sublist of
#'     \code{TS_families}), and relevant parameters for it (delivered
#'     by the dotsMethods-argument \code{...}), this function will
#'     return the function that will be used in \code{TS_sample} to
#'     generate the desired sample from the specified time series.
#'
#' @param TS_key character, key that selects one of the time series
#'     from \code{TS_families}
#'
#' @param ...  dotsMethods (i.e. optional), any arguments to be
#'     delivered to the function selected by \code{TS_key}.
#'
#' @keywords time series
#'
#' @return This function returns a list with the following five nodes
#'     to the internal workflow of \code{TS_sample}:
#'
#' \describe{
#'
#' \item{fun}{A local copy of the desired function, which can generate
#'     the desired time series.  Note that the formals of the function
#'     selected by \code{TS_key} will have been updated with the
#'     parameters given via the \code{dotsMethods}-argument.}
#'
#' \item{args.i}{A list with the (updated) default values that
#'     are used by the function in \code{fun}.}
#'
#' \item{args.f}{A character-string of the given values.}
#'
#' \item{TS_key}{The \code{TS_key}-value given to this function.}
#'
#' \item{fun_data}{An updated version of the \code{TS_key}-sublist of
#'     \code{TS_families}, in which the arguments have been updated
#'     based on the values given to the \code{...}-argument.}
#'
#' }
#'
#' @keywords internal

TS_sample_helper <- function(TS_key, ...) {
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
    ##  Investigate if the required package is available.
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
    if (length(given.args) > 0) {
        ##  Sanity check
        if (prod(names(given.args) %in%
                 names(args)) == 0) {
            extra.args <- given.args
            for (par in names(args)) extra.args[[par]] <- NULL
            stop("\t",
                 "In 'TS_sample_helper', the argument",
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
    ##  Adjustment when 'rugarch'.
    if (TS_key == "rugarch")
        .fun_data$args$spec <- given.args$spec
    ##  Adjustment when 'rmgarch'.
    if (TS_key == "rmgarch") {
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
