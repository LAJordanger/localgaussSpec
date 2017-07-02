################################################################################
#####  2014-09-17

#' Bootstrap-estimated statistics for Local Gaussian Spectral
#' Densities
#'
#' Based on the local Gaussian spectra obtained from the
#' bootstrap-procedure, this function returns a variety of statistics.
#' The estimated confidence intervals are the primary motivation for
#' this function, but it can also return information about the
#' distribution of the bootstrap-estimated spectra.  This function
#' first wraps around \code{LG_spectra} in order to get the
#' bootstrap-estimated spectra, then it extract the statistics from
#' the resulting array.  This function is a part of the bootstrap
#' wrapper, that attempts to ensure that the arguments delivered to it
#' are in small enough chunks to avoid out of memory errors.
#'
#' @template LG_approx_extract_data_arg
#' @template window_int
#' @template omega_vec_int
#' @template cut_vec_int
#' 
#' @template all_statistics_arg
#' @template log__arg
#' 
#' @param storage_only A logic argument with default value
#'     \code{FALSE}.  When this is used, only a \code{quote}-object
#'     for a dummy-array of the correct format (dimension and
#'     dimension-names) for the result are returned.  This will be
#'     useful when we for some reason in advance of the actual
#'     computation need to know the configuration of the
#'     dimension-names.  Furthermore, when we see that the
#'     computations of the local Gaussian spectra will require
#'     intermediate objects too large for the memory to handle well --
#'     but where the final array still will be of a tractable size
#'     (since we only want to store a set of statistics based on the
#'     bootstrapped local Gaussian spectra, not the whole bunch of
#'     them) -- then the quote can be evaluated to a storage-array and
#'     we can split the computation into smaller chunks whose results
#'     are inserted into the final array one at a time.
#'
#' @param ... The \code{dotsMethods}, additional arguments that should
#'     be delivered to \code{my_apply} can be specified here.  The
#'     options being \code{.front}, \code{.parallel} and
#'     \code{.cores}.  See the documentation of \code{my_apply} for
#'     details.
#' 
#' @return The result of this function will depend on the value of
#'     \code{storage_only}.  It will either be a quote for a
#'     placeholder array to be filled in when the computation are
#'     divided into smaller chunks, or it will be an array -- which
#'     always will contain a bunch of empirical quantiles (in order to
#'     get confidence intervals), and, if \code{all_statistics} equals
#'     \code{TRUE}, it will also contain the mean, standard deviation,
#'     skewness, kurtosis and mad (median absolute deviation).  These
#'     later values might perhaps not make too much sense when we
#'     investigate the spectra instead of the logarithms of them, but
#'     we have no guarantee that negative values will not occur and I
#'     thus think I will let it be for the time being.
#'
#' @export


LG_boot_spectra <- function(LG_approx_extract_data,
                            window,
                            omega_vec,
                            cut_vec,
                            all_statistics = FALSE,
                            log_ = FALSE,
                            storage_only = FALSE,
                            ...) {
###-------------------------------------------------------------------
    ##  If storage only is FALSE, do the computations as usual.
    if (! storage_only) {
###-------------------------------------------------------------------
        ##  Compute the array with all the local Gaussian spectra
        ##  based on the bootstrapped values.  (The content will be
        ##  updated later on to match the name.)
        boot_spectra_CI <-
            LG_spectra(
                LG_approx_extract_data = LG_approx_extract_data,
                omega_vec = omega_vec,
                cut_vec = cut_vec,
                window = window)
        kill(omega_vec, cut_vec, window, LG_approx_extract_data)
###-------------------------------------------------------------------
        ##  Add arguments to 'my_apply' from '...' (A local version
        ##  will be created due to this.)
        update_formals(.fun = my_apply, ...)
        kill(...)
###-------------------------------------------------------------------
        ##  Create a help-functon based on 'LG_boot_statistics' and
        ##  the selected values for 'all_statistics' and 'log_'.
        helpFun <- function(x) {
            LG_boot_statistics(x = x,
                               all_statistics = all_statistics,
                               log_ = log_)
        }
###-------------------------------------------------------------------
        ## Extract the names to be used on the resulting array.
        newnames <-
            LG_boot_statistics(all_statistics = all_statistics,
                               log_ = log_,
                               names_only = TRUE)
###-------------------------------------------------------------------
        ##  Create a function to take care of the updating of the
        ##  arrays in '.spectra_CI'.
        .CI_arr_fun <- function(.arr) {
            az <- LG_default$result$.azero ##  Default for "empty" nodes.
            if (identical(x = .arr, y = az))
                return(az)
            ##  Identify the margins to operate along.
            margin <- seq_along(dim(.arr))[!names(dimnames(.arr)) %in%
                                            "content"]
            ##  Compute the result, adjust dimension-names and the class.
            .tmp <- suppressWarnings(
                my_apply(X = .arr,
                         MARGIN = margin,
                         FUN = helpFun,
                         .front = TRUE))
            dimnames(.tmp) <- c(
                newnames,
                tail(x = dimnames(.tmp), n = -1))
            class(.tmp) <- LG_default$class$array
            ##  Return the result.
            .tmp
        }
###-------------------------------------------------------------------
        ##  Use a loop to update all the nodes, check the dimnames to
        ##  see where the function should be used.
        .dimnames <- attributes(boot_spectra_CI)$list_array_dimnames
        for (.node in LG_default$result$array_nodes) {
            if (is.null(.dimnames[[.node]]))
                next
            boot_spectra_CI[[.node]] <-
                .CI_arr_fun(boot_spectra_CI[[.node]])
        }
        ##  Return the result to the workflow, with updated dimension
        ##  information about the nodes.
        return(list_array_dims(boot_spectra_CI))
    } else {
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
        ##  The case 'storage_only' equals 'TRUE'.
###-------------------------------------------------------------------
        ##  Capture the part from 'LG_approx_extract_data' that
        ##  will be used in the result.
        old.names.boolean <- {
            names(dimnames(LG_approx_extract_data$extract)) %in%
            c("levels", "bw_points", "variable")
        }
        old.dim <-
            dim(LG_approx_extract_data$extract)[old.names.boolean]
        old.dimnames <-
            dimnames(LG_approx_extract_data$extract)[old.names.boolean]
###-------------------------------------------------------------------
        ##  Extract information about the new statistics dimension.

        ## capture_env() 

        stat.dimnames <-
            LG_boot_statistics(
                all_statistics = all_statistics,
                log_ = log_,
                names_only = TRUE)
###-------------------------------------------------------------------
        ##  Create the dimension of the storage array (type "par_five").
        storage.dim <-
            c(length(stat.dimnames[[1]]),
              1, #  Due to 'spec' from 'LG_spectra'.
              length(cut_vec),
              length(window),
              length(omega_vec),
              old.dim)
###-------------------------------------------------------------------
        ##  Create the dimension-names of the storage array (type
        ##  "par_five").  Note, that we (in the present incarnation of
        ##  the code) need to convert the numerical vectors 'cut_vec'
        ##  and 'omega_vec' into characters and then sort them in
        ##  order to get the desired result.
        storage.dimnames <-
            c(stat.dimnames,
              list(spec = "spec"), #  Due to 'spec' from 'LG_spectra'.
              list(cut = sort(as.character(cut_vec)),
                   window = window,
                   omega = sort(as.character(omega_vec))),
              old.dimnames)
###-------------------------------------------------------------------
        ##  Create the quote of the storage array.  Note: Use 'bquote'
        ##  with '.()' to include the values computed abvoe.
        return(bquote(
            array(data = 1,
                  dim = .(storage.dim),
                  dimnames = .(storage.dimnames))))
    }
}
