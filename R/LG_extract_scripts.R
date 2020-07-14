#' Extraction of scripts
#'
#' @description This function extracts the scripts from the internal
#'     file hierarchy of the \code{localgaussSpec}-package to a target
#'     directory selected by the user.
#'
#' @details If the \code{target_dir}-argument is missing, then the
#'     function will print a message that points to this help-page.
#'     If \code{target_dir} is a non-existing directory, then the user
#'     will be asked for permission to create it.  If
#'     \code{target_dir} exists but are non-empty, then it is required
#'     that the \code{overwrite}-argument must be \code{TRUE} in order
#'     for the scripts to be extracted.
#'
#' @param target_dir The path to the directory where the scripts are
#'     to be stored.  The user will be asked for permission to create
#'     the directory if it does not already exist.
#'
#' @param overwrite Logical value, default \code{FALSE}.  The default
#'     behaviour will be that this function stops if it encounters a
#'     non-empty directory.  This is done in order to avoid that
#'     previously extracted (and potentially modified) versions of the
#'     scripts are overwritten.  Set \code{overwrite} to \code{TRUE}
#'     in order to override this requirement.
#' 
#' @return The result of this function is that copies of the scripts
#'     occur in \code{target_dir}, and the user can then run them (or
#'     modify them) as desired.  Read the \code{ReadMe.txt}-file in
#'     the extracted folder for further details about the structure of
#'     the extracted scripts.
#'
#' @export

LG_extract_scripts <- function(target_dir, overwrite = FALSE) {
    ##  Return information about the help-documentation if this
    ##  function is called without a 'target_dir'-argument.
    if (missing(target_dir)) {
        cat(sprintf(
            "\n\nFor information about the extraction of scripts, use:\n\n%s",
            "help(LG_extract_scripts)\n\n"))
        return(invisible(NULL))
    }
    ##  Identify the internal source path.
    .source_path <- local({
        .package <- "localgaussSpec"
        .script_dir <- "scripts"
        file.path(
            find.package(package = .package),
            .script_dir)
    })    
    ##  Check that the internal source-path is found, and if not
    ##  inform the user that this is a development problem.
    if (!dir.exists(paths = .source_path))
        error(c("Did not find the internal source folder that should contain the scripts!",
                "Please inform the package maintainer, laj@hvl.no, about the problem."))
    ##  Check that 'target_dir' exists.
    if (!dir.exists(paths = target_dir)) {
        cat(sprintf(
            "\n\nThe directory %s does not exist.\nWould you like to create it now?\n",
            sQuote(target_dir)))
        .answer <- readline(
            prompt = sprintf(
                "Please enter %s if you want to create this directory.\n%s%s: ",
                sQuote("yes"),
                "Any other input will be considered as ",
                sQuote("no")))
        if (isTRUE(.answer == "yes")) {
            dir.create(path = target_dir,
                       recursive = TRUE)
        } else {
            return(cat("\n\nExtraction of scripts stopped.\n\n"))
        }
    }
    ##  Check that 'target_dir' is empty.
    .existing_content <- list.files(
        path = target_dir,
        full.names = TRUE,
        recursive = TRUE)
    if (length(.existing_content)!=0) {
        if (overwrite) {
            cat(sprintf(
                "\n\nThe directory %s exists, but it is not empty.\n%s %s was called with %s.",
                sQuote(target_dir),
                "Will overwrite files since",
                sQuote("LG_extract_scripts"),
                sQuote(sprintf("%s = TRUE",
                               "overwrite"))))
        } else {
            cat(sprintf(
                "\n\nThe directory %s exists, but it is not empty.\n%s\nCall %s with %s to overwrite existing content.\n\n",
                sQuote(target_dir),
                "Extraction of scripts stopped.",
                sQuote("LG_extract_scripts"),
                sQuote(sprintf("%s = TRUE",
                               "overwrite"))))
            return(invisible(NULL))
        }
    }
    kill(.existing_content, overwrite)
    ## Copy the required files.
    file.copy(
        from = .source_path,
        to = target_dir,
        recursive = TRUE)
    ##  Inform the reader about the number of files that have been
    ##  copied.
    .nr_files <- length(list.files(
        path = target_dir,
        full.names = TRUE,
        recursive = TRUE))
    cat(sprintf(
        "\n%i script-files copied to the directory %s.\nRead %s for further details.\n\n",
        .nr_files,
        sQuote(target_dir),
        sQuote(file.path(target_dir,
                         "ReadMe.txt"))))
}

## Reminder from the testing:
## target_dir <- "delete_this_after_use"
## dir.create(path = target_dir)
## LG_extract_scripts(target_dir = target_dir)
## unlink(x = target_dir, recursive = TRUE, force = TRUE)
