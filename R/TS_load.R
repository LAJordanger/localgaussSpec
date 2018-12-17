################################################################################
#'
#' Load saved file and return \code{TS} and \code{save_dir}
#'
#' This is an internal helper-function that takes care of some pesky
#' details when loading and saving arrays related to time series.  The
#' point is that regardless of the name used upon the stored array, it
#' will be returned to the calling environment with the name
#' \code{TS}, and it's also possible to get information required for
#' the saving of the results to be computed from \code{TS}.
#'
#' @param .TS_info A list containing the three components \code{TS},
#'     \code{main_dir} and \code{save_dir}.
#'
#' @param save_dir Logical value, default \code{TRUE}.  To be used
#'     when it's of interest to have a path at which the next step in
#'     the computation should be stored.
#'
#' @return This function will add objects to the calling environment.
#' 
#' @keywords internal


TS_load <- function(.TS_info,
                    save_dir = TRUE) {
    ##  Identify the calling environment
    .parent_frame <- parent.frame()
    ##  Figure out if the `TS`-component is numeric or character, and
    ##  in the latter case read from disk.
    if (is.numeric(.TS_info$TS)) {
        assign(x = "TS",
               value = .TS_info$TS,
               envir = .parent_frame)
    } else {
        ##  Find the path to use when reading data.
        .TS_path <- paste(c(.TS_info$main_dir,
                            .TS_info$TS),
                          collapse = .Platform$file.sep)
        ## Load `TS` into a temporary environment.
        .tmp_env <- new.env()
        load(file = .TS_path,
             envir = .tmp_env)
        ##  Assign the result to `.parent_frame` under the name `TS`.
        assign(x = "TS",
               value = .tmp_env[[ls(.tmp_env, all.names  = TRUE)]],
               envir = .parent_frame)
    }
    ##  Add `save_dir` too, when asked for.
    if (save_dir) 
        assign(x = "save_dir",
               value = paste(c(.TS_info$main_dir,
                               .TS_info$save_dir),
                             collapse = .Platform$file.sep),
               envir = .parent_frame)
    ##  Return invisible `NULL` to the workflow.
    invisible(NULL)
}
