################################################################################
#'
#' Compute confidence intervals based on spectral estimates
#'
#' This is a helper-function to deal with the computation of the
#' confidence intervals for the global spectral densities.  This is
#' done in order to create a global reference which the local Gaussian
#' spectral densities can be compared against.
#'
#' @template main_dir_arg
#'
#' @param spectral_content The path needed to open the file containing
#'     the estimated spectral densities.
#'
#' @template all_statistics_arg
#' @template log__arg
#'
#' @param ...  Additional arguments to be passed to the
#'     apply-functions.
#' 
#' @return A file will be saved to disk, and the function will return
#'     information to be used when updating the corresponding
#'     \code{info}-object that is maintaned by the calling function.
#' 
#' @export


TS_CI <- function(main_dir,
                  spectral_content,
                  all_statistics,
                  log_,
                  ...) {
    ##  Find the path to use when reading data.
    .spectra_path <- paste(c(main_dir,
                             spectral_content),
                           collapse = .Platform$file.sep)
    ##  Find the save directory to use when updating the info-object
    ##  and when saving the result.
    save_dir <- head(x = spectral_content, n = -1)
    save_path <- paste(c(main_dir,
                         save_dir),
                       collapse = .Platform$file.sep)
    kill(main_dir, spectral_content)
###-------------------------------------------------------------------
    ## Load `spectra` into the present environment under the name
    ##  `.spectra_CI`, the content will be updated later on.
    LG_load(.file = .spectra_path,
            .name = ".spectra_CI")
    kill(.spectra_path)
###-------------------------------------------------------------------
    ##  Identify if it makes sense to compute these statistics.
    .invalid <- length(dimnames(.spectra_CI$auto)$content) == 1
    ##  Return an error if not a bootstrap situation
    if (.invalid)
        error(c("Estimation of confidence-intervals attempted for",
                "a case where the dimension ",
                sQuote("content"),
                "has length 1."))
    kill(.invalid)
###-------------------------------------------------------------------
    ##  Add arguments to 'my_apply' from '...' (A local version will
    ##  be created due to this.)
    update_formals(.fun = my_apply, ...)
    kill(...)
###-------------------------------------------------------------------
    ##  Create a help-functon based on 'LG_boot_statistics' and the
    ##  selected values for 'all_statistics' and 'log_'.
    helpFun <- function(x) {
        LG_boot_statistics(x = x,
                           all_statistics = all_statistics,
                           log_ = log_)
    }
###-------------------------------------------------------------------
    ## Extract the names to be used on the new arrays
    newnames <-
        LG_boot_statistics(all_statistics = all_statistics,
                           log_ = log_,
                           names_only = TRUE)
###-------------------------------------------------------------------
    ##  Create a function to take care of the updating of the arrays
    ##  in '.spectra_CI'.
    .CI_arr_fun <- function(.arr) {
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
    ##  Use a loop to update all the nodes, check the dimnames to see
    ##  where the function should be used.
    .dimnames <- attributes(.spectra_CI)$list_array_dimnames
    for (.node in LG_default$result_orig$array_nodes) {
        if (is.null(.dimnames[[.node]]))
            next
        .spectra_CI[[.node]] <-
            .CI_arr_fun(.spectra_CI[[.node]])
    }
    kill(.node, .dimnames, helpFun, all_statistics, log_, .CI_arr_fun,
         my_apply, newnames)
###-------------------------------------------------------------------
    ##  Specify the type, and save the result to file, with updated
    ##  attributes describing the array nodes.
    .spectral_type <- paste("spectral", "_CI", sep = "")
    LG_save(data = list_array_dims(.spectra_CI),
            save_file.Rda = LG_default$global[.spectral_type],
            save_dir = save_path)
    ##  Return information to be added to the `info`-object.
    list(.spectral_type = .spectral_type,
         .spectral_content = c(save_dir, LG_default$global[.spectral_type]))
}
