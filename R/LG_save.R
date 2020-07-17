#' Save function for local Gaussian related information
#'
#' @description This internal function simplifies the code of the
#'     scribe-functions.  It will ensure that the data will be saved
#'     with informative names that reveals what part of the code the
#'     data originates from.
#'
#' @param data The data we want to store.
#'
#' @param save_file.Rda A file name to be used when saving
#'     \code{data}.  The default value \code{NULL} will imply that a
#'     name is computed based on the values given to the arguments
#'     \code{LG_type}, \code{bootstrap} and \code{part}.
#'  
#' @param LG_type One of \code{c("par_five", "par_one")}.  This tells
#'     us whether the data is based on the local Gaussian
#'     autocorrelations estimated from the five parameter bivariate
#'     Gaussian probability density distribution ("par_five"), or if
#'     they have been obtained by the help of the simplified one
#'     parameter bivariate Gaussian probability density distribution
#'     ("par_one").  This argument will be ignored when
#'     \code{save_file.Rda} is different from \code{NULL}.
#'
#' @note Regarding the case where the \code{LG_type}-argument is equal
#'     to "par_one": The author of this package has always considered
#'     the "par_one"-approach to be reasonable when the aim of the
#'     investigation is to estimate a density at a given point.
#'     However, the extraction of the correlation value from the
#'     resulting density-estimate will in general not capture the
#'     local geometrical properties of the targeted distribution at
#'     the point of investigation.  The "par_one"-approach is as such
#'     (in general) a complete waste of computation resources.
#'
#' @param bootstrap This tells us whether our data have been computed
#'     from an original time series, or from bootstrap-replicates of
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
    ##  Compute 'save_dir.Rda' when necessary.
    if (is.null(save_file.Rda)) {
        ##  Compute the 'save_info'
        save_info <- sprintf(
            "%s_approx",
            LG_type)
        ##  Create the name of the save-file.
        save_file.Rda <- sprintf(
            "%s%s%s.Rda",
            ifelse(test = bootstrap,
                   yes  = sprintf("%s_", LG_default$boot.prefix),
                   no   = ""),
            save_info,
            ifelse(test = {part == " "},
                   yes = "",
                   no  = sprintf("_%s", part)))
    } else {
        ##  Use the stem of 'save_file.Rda' as 'save_info'.
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
