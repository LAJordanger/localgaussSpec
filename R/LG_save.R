################################################################################
#'
#' Save function for different Local Gaussian Information
#'
#' This function is primarily intended to simplify the code of the
#' wrapper-functions.  It will ensure that the data we want to store
#' will be saved with informative names that reveals what part of the
#' code the data originates from.
#'
#' @param data The data we want to store.
#'
#' @param save_file.Rda A file name to be used when saving
#'     \code{data}.  The default value \code{NULL} will imply that a
#'     name is computed based on the values given to the arguments
#'     \code{data_type}, \code{LG_type}, code{bootstrap} and
#'     \code{part}.
#'  
#' @param data_type One of \code{data_type = c("approx_save",
#'     "spectra_save")}.  This tells us whether \code{data} comes from
#'     the intermediate step where the Local Gaussian Approximations
#'     are computed, or if \code{data} concerns the estimates of the
#'     Local Gaussian Spectral Densities.  This argument will be
#'     ignored when \code{save_file.Rda} is different from
#'     \code{NULL}.
#' 
#' @param LG_type One of \code{c("par_five", "par_one")}, only used
#'     when \code{data_type="spectra_save"}. This tells us whether the
#'     data is dealing with local Gaussian auto-correlations
#'     ("par_five") or with local Gaussian auto-covariances
#'     ("par_one").  This argument will be ignored when
#'     \code{save_file.Rda} is different from \code{NULL}.
#'
#' @param bootstrap This tells us whether our data have been computed
#'     from the original time series or from bootstrap-replicates of
#'     it.  Default value \code{FALSE}.  This argument will be ignored
#'     when \code{save_file.Rda} is different from \code{NULL}.
#'
#' @param part This will be used if memory-issues requires that a
#'     computation must be partitioned into smaller chunks, in which
#'     case a "part_a_of_b" will be added at the end of the
#'     file-names.  This argument will be ignored when
#'     \code{save_file.Rda} is different from \code{NULL}.
#'
#' @param save_dir The directory into which the data will be saved.
#'     The default is ".", i.e. the working directory will be used.
#'     The path to the directory can be delivered as a vector
#'     containing the different pieces of the path, and this is the
#'     way the main functions will do it.  This strategy has been
#'     selected in order to simplify the bookkeeping in the central
#'     info-file, where the folder hierarchy will be stored in a
#'     sequence of nested lists.
#' 
#' @param compression_level This argument will be delivered to the
#'     \code{save}-function, in order to reduce the needed disk-space
#'     required for the saving of the files.  The default value is
#'     \code{9}, i.e. the highest level of compression will be used.
#'
#' @return This function will save \code{data} under a descriptive
#'     name in a corresponding named file.  The file-name will be
#'     returned to the workspace/parent function.
#'
#' @keywords internal

LG_save <- function(
    data,
    save_file.Rda = NULL,
    data_type = c("approx_save", "spectra_save"),
    LG_type = c("par_five", "par_one"),
    bootstrap = FALSE,
    part = " ",
    save_dir = ".",
    compression_level = 9) {
    ##  Include sanity-checks?  Check that the formals are in
    ##  accordance with the names used in 'LG_default'?
    ##  If 'save_dir' is given as a vector, convert it to a path.
    if (length(save_dir) > 1)
        save_dir <- paste(save_dir,
                          collapse = .Platform$file.sep)
    ##  Create the directory if it does not already exists.
    if (! file.exists(save_dir))
        dir.create(save_dir)
    ##  Compute `save_dir.Rda` when necessary.
    if (is.null(save_file.Rda)) {
        ##  Look up 'LG_default' for the stem of the save information.
        save_info <- LG_default[[c(data_type, LG_type)]]
        ##  Add information based on 'bootstrap', and 'part'.
        save_info <-
            paste(ifelse(test = bootstrap,
                         yes =
                             paste(LG_default$boot.prefix,
                                   "_",
                                   sep = ""),
                         no = ""),
                  save_info,
                  ifelse(test = (part == " "),
                         yes = "",
                         no =
                             paste("_",
                                   part,
                                   sep = "")),
                  sep = "")
        ##  Create the name of the save-file.
        save_file.Rda <- paste(save_info, ".Rda", sep = "")
    } else {
        ##  Use the stem of `save_file.Rda` as `save_info`.
        save_info <- gsub(
            pattern = ".Rda$",
            replacement = "",
            x = save_file.Rda)
    }
    ##  Proceed to save to file.
    save_file <- file.path(
        save_dir,
        save_file.Rda)
    ## Save 'data' under the name 'save_info' into the file
    ## 'save_file', using '.()' and 'bquote' for the construction.
    save_quote <- bquote({
        .(save_info) <- data
        save(.(save_info),
             file = save_file,
             compression_level = compression_level)
        ##  Inform the user that the file was created.
        file_created(save_file)
    })
    ##  Evaluate the quote to perform the saving step.
    eval(save_quote)
    ##  Return the name of the save
    return(save_file.Rda)
}
