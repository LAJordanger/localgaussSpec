################################################################################
#####  Last revised 2015-12-18

#' Extract data needed for Local Gaussian Spectral Densities.
#'
#' The estimated Local Gaussian Approximations, created by
#' \code{LG_approx} and \code{LG_boot_approx}, include a lot of
#' information that we don't need in the computation of the Local
#' Gaussian Spectral Densities.  In order to keep the code later on as
#' simple as possible, this function takes care of the extraction of
#' the relevant values, i.e. the Local Gaussian Correlations.
#'
#' @param approx_data A list with Local Gaussian Approximations,
#'     which in general contains more information than needed for the
#'     computation of the Local Gaussian Spectral Densities.
#'
#' @template LG_type_int
#' 
#' @param .method One or more of the values in \code{c("all", "mean",
#'     "median")}, but the argument will only have an effect if the
#'     length of the \code{content}-dimension in the \code{LG_type}
#'     part of \code{approx_data} is larger than one.  The idea is
#'     that this argument will be used with the values "mean" and
#'     "median" while investigating blocks, and the value "all" when
#'     doing a bootstrap-investigation.
#'
#' @return A list with two components, a list \code{info} and an array
#'     \code{pos_lag}. The list \code{info} contains stuff related to
#'     reproducibility, whereas the array \code{pos_lag} contains the
#'     required Local Gaussian Autocorrelations.
#'
#' @export


LG_approx_extract <- function(
    approx_data,
    LG_type = c("par_one", "par_five"),
    .method = c("all", "mean", "median")) {
###-------------------------------------------------------------------
    ##  Enable an iterative approach to this computation.
    .iterative <- TRUE
###-------------------------------------------------------------------
    ##  Restrict to default if no choice is made for 'LG_type'
    LG_type <- LG_type[1]
###-------------------------------------------------------------------
    ##  Sanity check that the desired data exist and can be extracted.
    if (! LG_type %in% approx_data$LG_type)
        error(.argument = "LG_type",
              c("Extraction not possible.",
                "Not present in incoming data."))
    ##  Create the node for extraction of relevant data.
    LG_data <- paste(LG_type, "data", sep = "_")
###-------------------------------------------------------------------
    ##  Use the iterative approach on the "top level".
    if (.iterative) {
        ##  Create a local copy of this function and revise its body
        ##  to avoid an infinite loop when calling it recursively.
        fun_copy <- LG_approx_extract
        body(fun_copy)[[2]] <- quote(.iterative <- FALSE)
        ##  Use 'fun_copy' on "adjusted nodes" of a copy of the list
        ##  'approx_data', with a setup that ensures that 'NULL' will
        ##  be returned when required.
        .node_on_d <- c(LG_data, "on_diag")
        .node_off_d <- c(LG_data, "off_diag") 
        new_data <- approx_data
        ##  Return the result.
        return({
            list(on_diag =
                     if (! is.null(approx_data[[.node_on_d]])) {
                         new_data[[LG_data]] <- approx_data[[.node_on_d]]
                         fun_copy(approx_data = new_data,
                                  LG_type = LG_type,
                                  .method = .method)
                     },
                 off_diag =
                     if (! is.null(approx_data[[.node_off_d]])) {
                         new_data[[LG_data]] <- approx_data[[.node_off_d]]
                         fun_copy(approx_data = new_data,
                                  LG_type = LG_type,
                                  .method = .method)
                     })
        })
    }
    ##  Below: The code that will work upon each of the nodes.
###-------------------------------------------------------------------
    ##  Make a local copy of the autocorrelations from the data.
    pos_lag <- approx_data[[LG_data]]
    ##  Restrict to the autocorrelations.
    pos_lag <- restrict_array(
        .arr = pos_lag,
        .restrict = list(variable = "rho"))
    ##  Adjust names/dimnames to ensure compatibility when using
    ##  'multiply_arrays' and 'add_arrays'.
    names(dimnames(pos_lag))[
             match("variable", names(dimnames(pos_lag)))] <- "type"
###-------------------------------------------------------------------
    ##  Extract attributes, to ensure that they are passed on to the
    ##  next function.
    .keep <- ! names(attributes(pos_lag)) %in% c("dim", "dimnames")
    .attr_for_pos_lag <- attributes(pos_lag)[.keep]
    kill(.keep)
###-------------------------------------------------------------------
    ##  Extract the '.adjustment_rule', and use it for finite-samples
    ##  adjustments.  That is: Do nothing if the value is '0',
    ##  otherwise extract relevant parts and do adjustments.
    .adjustment_rule <- attributes(
        approx_data$adjustment)$.adjustment_rule
    ##  Figure out if an extraction is required.
    .extract <- .adjustment_rule != 0
    ##  Do adjustments for the relevant cases.
    if (.extract) {
        ##  In the numerical case: Modify format to simplify code.
        if (is.numeric(.adjustment_rule))
            approx_data$adjustment <- array(
                data = approx_data$adjustment,
                dim = c(length(approx_data$adjustment), 1, 1),
                dimnames = list(
                    lag = names(approx_data$adjustment),
                    type = "rho",
                    estimates = "scale"))
        ##  Extract the required scale-information, and fine-tune it.
        scale <- restrict_array(
            .arr = approx_data$adjustment,
            .restrict = list(
                lag = dimnames(pos_lag)$lag,
                type = "rho",
                estimates = "scale"))
        ##  Identify the dimension to drop when fine-tuning.
        .drop_dim <-
            seq_along(dimnames(scale))[
                         names(dimnames(scale)) %in% c("estimates")]
        ##  Fine-tune 'scale' to enable use of 'multiply_arrays'.
        scale <- adrop(
            x = scale,
            drop = .drop_dim)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Reminder: The '.drop' argument of 'restrict_array' can be set to
###  'TRUE', and in most cases that would avoid the need for 'adrop'.
###  However, if another dimension also should happen to contain only
###  one element, then the code later on might return an error that
###  would be mighty hard to track down.  That's not desirable.
#############---------------------------------------------------------
###-------------------------------------------------------------------
        ##  Adjust 'pos_lag' with 'scale'.
        pos_lag <- multiply_arrays(
            .arr1 = pos_lag,
            .arr2 = scale,
            keep_shape = TRUE)
        ##  For the "data"-case of '.adjust_rule', do additive
        ##  modification too.
        if (.adjustment_rule == "data") {
            add <- restrict_array(
                .arr = approx_data$adjustment,
                .restrict = list(
                    lag = dimnames(pos_lag)$lag,
                    type = "rho",
                    estimates = "adjust"))
            ##  Fine-tune 'add' to enable use of 'add_arrays'.
            add <- adrop(
                x = add,
                drop = .drop_dim)
            ##  Adjust 'pos_lag' with 'scale'.
            pos_lag <- add_arrays(
                .arr1 = pos_lag,
                .arr2 = add,
                keep_shape = TRUE)
        }
        kill(approx_data, .drop_dim, scale, add)
    }
    kill(.extract, .adjustment_rule)
###-------------------------------------------------------------------
    ##  If only one time-series is present, set '.method' to "all".
    if (length(dimnames(pos_lag)$content) == 1)
        .method <- "all"
    ##  Do nothing if '.method' is equal to "all", else compute
    ##  revised version.
    if (! identical(.method, "all")) {
        ##  Define function and names based on '.method'.
        .mean_median <- setdiff(x = .method, y = "all")
        .mean_median_names <- paste(LG_default$contiguous.prefix,
                                    c(if ("mean" %in% .method)
                                          "mean",
                                      if ("median" %in% .method)
                                          "median"),
                                    sep = "_")
        .mean_median_fun <-
            if (all(c("mean", "median") %in% .method)) {
                function(x) c(mean(x), median(x))
            } else if ("mean" %in% .method) {
                function(x) mean(x)
            } else
                function(x) median(x)
        ##  Find the 'content'-margin
        .margin_content <- which(names(dimnames(pos_lag)) == "content")
        ##  Compute new values
        new_data <- aaply(
            .data = pos_lag,
            .margins = setdiff(1:length(names(dimnames(pos_lag))),
                               .margin_content),
            .fun = .mean_median_fun,
            .drop = FALSE)
        ##  Update the dimension-names on the new values.
        dimnames(new_data) <- c(
            head(dimnames(new_data), -1),
            list(content = .mean_median_names))
        ##  Update/replace 'pos_lag' depending on '.method'.
        if ("all" %in% .method) {
            pos_lag <- my_abind(
                pos_lag,
                new_data)
        } else
            pos_lag <- new_data
    }
    ##  Adjust names/dimnames to include information about 'LG_type'
    dimnames(pos_lag)$type[match("rho", dimnames(pos_lag)$type)] <- LG_type
    ##  Ensure that 'lag' is along the first dimension.
    if (names(dimnames(pos_lag))[1] != "lag")
        pos_lag <- 
            aperm(a = pos_lag,
                  perm =
                      c("lag",
                        setdiff(x=names(dimnames(pos_lag)),
                                "lag")))
###-------------------------------------------------------------------
    ##  Ensure that all relevant attributes are pased along.
    attributes(pos_lag) <- c(
        attributes(pos_lag)[c("dim", "dimnames")],
        .attr_for_pos_lag)
###-------------------------------------------------------------------
    ##  Create 'info' about 'lag_max' and 'LG_type'.
    info <-  list(lag_max = max(as.integer(dimnames(pos_lag)$lag)),
                  LG_type = LG_type)
    ##   return the list containing `info' and `pos_lag'.
    return(list(info = info,
                pos_lag = pos_lag))
}
