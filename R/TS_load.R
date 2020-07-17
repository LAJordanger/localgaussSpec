#' Load saved file and return \code{TS} and \code{save_dir}
#'
#' @description This internal function takes care of some pesky
#'     details when a local Gaussian investigation is to be performed
#'     on a time series.  It can also compute the \code{save_dir} that
#'     should be used when saving the results based on the given time
#'     series \code{TS}.
#'
#' @param .TS_info A list containing the three components \code{TS},
#'     \code{main_dir} and \code{save_dir}.  The \code{TS}-component
#'     can either be the time series of interest, or it could be the
#'     filename used to store the time series.
#'
#' @param save_dir Logical value, default \code{TRUE}.  To be used
#'     when it is of interest to have a path at which the next step in
#'     the computation should be stored.
#'
#' @return This function will always add an object named \code{TS} to
#'     the environment of the calling function.  Depending on the
#'     logical value given by \code{save_dir}, it might also add a
#'     character-valued \code{save_dir} to this environment.
#' 
#' @keywords internal

TS_load <- function(.TS_info,
                    save_dir = TRUE) {
    ##  Identify the calling environment.
    .parent_frame <- parent.frame()
    ##  Figure out if the 'TS'-component is numeric or character, and
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
        ## Load 'TS' into a temporary environment.
        .tmp_env <- new.env()
        load(file = .TS_path,
             envir = .tmp_env)
        ##  Assign the result to '.parent_frame' under the name 'TS'.
        assign(x = "TS",
               value = .tmp_env[[ls(.tmp_env, all.names  = TRUE)]],
               envir = .parent_frame)
    }
    ##  Add 'save_dir' too, when asked for.
    if (save_dir) 
        assign(x = "save_dir",
               value = paste(c(.TS_info$main_dir,
                               .TS_info$save_dir),
                             collapse = .Platform$file.sep),
               envir = .parent_frame)
    ##  Return invisible 'NULL' to the workflow.
    invisible(NULL)
}
