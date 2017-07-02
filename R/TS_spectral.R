################################################################################
#'
#' Compute old-fashioned spectral estimates
#'
#' This function computes estimates of the spectral densities for a
#' time series the "old-fashioned" way, i.e. using window-functions in
#' order to smooth the estimates.  This is done in order to create a
#' global base-functions, against which the local Gaussian spectral
#' densities can be compared.  Note that this functions is kind of a
#' wrapper around \code{LG_spectra}, where the core of the computation
#' is performed.
#' 
#'
#' @param acr_data A list giving the information required in order to
#'     read the estimated autocorrelations from file, and moreover
#'     compute the path at which the estimated spectral densities
#'     should be saved.
#'
#' @param omega_vec A vector of frequencies
#'
#' @param window The window-function(s) to be used.  The available
#'     values are \code{c("Tukey", "Parzen", "Bartlett")}.  All the
#'     window-types will be computed if no selection is made.  (This
#'     might be modified in a latter incarnation of the code.)
#' 
#' @template cut_vec_arg
#' 
#' @param .method One or more of the values in \code{c("all", "mean",
#'     "median")}, but the argument will only have an effect if the
#'     length of the \code{content}-dimension in the \code{LG_type}
#'     part of \code{approx_data} is larger than one.  The idea is
#'     that this argument will be used with the values "mean" and
#'     "median" while investigating blocks, and the value "all" when
#'     doing a bootstrap-investigation.
#' 
#' @return A file will be saved to disk, and the function will return
#'     information to be used when updating the corresponding
#'     \code{info}-object that is maintaned by the calling function.
#'
#' @export

TS_spectral <- function(acr_data,
                        omega_vec,
                        window = c("Tukey", "Parzen", "Bartlett"),
                        cut_vec = NULL,
                        .method = c("all", "mean", "median")) {
    ##  Find the path to use when reading data.
    .acr_path <- paste(c(acr_data$main_dir,
                         acr_data$acr),
                       collapse = .Platform$file.sep)
    ##  Adjust the `save_dir` part of `acr_data` to ensure that it's
    ##  format suits the portability-requirement.
    acr_data$save_dir <- strsplit(
        x = gsub(
            pattern =
                paste(acr_data$main_dir,
                      .Platform$file.sep,
                      sep = ""),
            replacement = "",
            x = acr_data$save_dir),
        split = .Platform$file.sep)[[1]]
###-------------------------------------------------------------------
    ## Load `acr` into a temporary environment.
    .tmp_env <- new.env()
    load(file = .acr_path,
         envir = .tmp_env)
    ##  Add it to the present envionment under the name `acr`.
    acr <- .tmp_env[[ls(.tmp_env, all.names  = TRUE)]]
    kill(.acr_path, .tmp_env)
    ##  Compute the save_dir to be used when saving the result.
    save_dir <- paste(c(acr_data$main_dir,
                        acr_data$save_dir),
                      collapse = .Platform$file.sep)
###-------------------------------------------------------------------
    ##  Ensure that 'lag' is along the first dimension, so
    ##  `LG_spectra` doesn't complain.
    if (names(dimnames(acr))[1] != "lag") {
        .permutation <- c("lag",
                          setdiff(x = names(dimnames(acr)),
                                  y = "lag"))
        ##  Update the positive lags
        acr <- restrict_array(
            .arr = acr,
            .restrict = dimnames(acr)[.permutation],
            .permute = TRUE)
    }
###-------------------------------------------------------------------
    ##  Identify if `acr` is related to bootstrapping.
    .bootstrap <-
        if (is.null(attributes(acr)$bootstrap)) {
            FALSE
        } else
            attributes(acr)$bootstrap
    ##  Identify the type based on `.bootstrap`.
    .spectral_type <- paste("spectral",
                       ifelse(
                           test = .bootstrap,
                           yes  = "_boot",
                           no   = ""),
                       sep = "")
###-------------------------------------------------------------------
    ##  If not `.bootstrap`, investigate `.method` and if necessary
    ##  update `acr`.
    if (! .bootstrap) {
        ##  If only one time-series is present, set '.method' to "all".
        if (length(dimnames(acr)$content) == 1)
            .method <- "all"
        ##  Do nothing if '.method' is equal to "all", else compute
        ##  revised version.
        if (! identical(.method, "all")) {
            ##  Record the attributes, that must be added back at the
            ##  end (due to them being lost in the update).
            .ignore <- c("dim", "dimnames")
            .keep <- ! names(attributes(acr)) %in% .ignore
            .attributes_acr <- attributes(acr)[.keep]
            kill(.ignore, .keep)
            ##  Define function and names based on '.method'.
            .mean_median <- setdiff(x = .method, y = "all")
            .mean_median_names <- paste(LG_default$sample.prefix,
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
            .margin_content <- which(names(dimnames(acr)) == "content")
            ##  Compute new values
            new_data <- structure(
                .Data = aaply(
                    .data = acr,
                    .margins = seq_along(dimnames(acr))[-.margin_content],
                    .fun = .mean_median_fun,
                    .drop = FALSE),
                .Dimnames = c(dimnames(acr)[-.margin_content],
                              list(content = .mean_median_names)))
            ## ## ## if (.attributes_acr$.multivariate_TS)
            ## ## ##     new_data_zero <- structure(
            ## ## ##     .Data = aaply(
            ## ## ##         .data = .attributes_acr$ccr_zero,
            ## ## ##         .margins = seq_along(dimnames(.attributes_acr$ccr_zero))[-.margin_content],
            ## ## ##         .fun = .mean_median_fun,
            ## ## ##         .drop = FALSE),
            ## ## ##     .Dimnames = c(dimnames(.attributes_acr$ccr_zero)[-.margin_content],
            ## ## ##                   list(content = .mean_median_names)))
            ##  Update/replace based on '.method'.
            if ("all" %in% .method) {
                acr <- my_abind(
                    acr,
                    new_data)
                ## ## if (.attributes_acr$.multivariate_TS)
                ## ##     .attributes_acr$ccr_zero <- my_abind(
                ## ##     .attributes_acr$ccr_zero,
                ## ##     new_data_zero)
            } else {
                acr <- new_data
                ## ## ## if (.attributes_acr$.multivariate_TS)
                ## ## ##     .attributes_acr$ccr_zero <- new_data_zero
            }
            ##  Update the attributes.
            ## ## ## class(.attributes_acr$ccr_zero) <- LG_default$class$array
            attributes(acr) <- c(
                attributes(acr),
                .attributes_acr)
            kill(.attributes_acr)
        }    
    }
    kill(.bootstrap)
###-------------------------------------------------------------------
    ##  Estimate the spectral densities.

    ## LG_spectra_call <- create_call(
    ##     .cc_fun = LG_spectra,
    ##     LG_approx_extract_data = list(
    ##         on_diag = list(pos_lag = acr,
    ##                        info = list(lag_max = max(as.integer(dimnames(acr)$lag))))),
    ##     omega_vec = omega_vec,
    ##     window = window,
    ##     cut_vec = cut_vec)
    ## capture_env() 
    .spectra <- LG_spectra(
        LG_approx_extract_data = list(
            on_diag = list(pos_lag = acr,
                           info = list(lag_max = max(as.integer(dimnames(acr)$lag))))),
        omega_vec = omega_vec,
        window = window,
        cut_vec = cut_vec)$on_diag
    kill(acr, omega_vec, window, cut_vec)
    ##  The subsetting removed the attributes with information about
    ##  the array-nodes, this must be added again.
    .spectra <- list_array_dims(.list = .spectra)
###-------------------------------------------------------------------
    ##  Save the result to file.
    LG_save(data = .spectra,
            save_file.Rda = LG_default$global[.spectral_type],
            save_dir = save_dir)
    ##  Return information to be added to the `info`-object.
    list(.spectral_type = .spectral_type,
         .spectral_content = c(acr_data$save_dir, LG_default$global[.spectral_type]))
}
