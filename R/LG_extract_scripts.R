#' Extraction of scripts.
#'
#' This function extracts the scripts from the internal file hierarchy
#' to a target directory selected by the user.  
#'
#' @param target_dir The path to the directory where the scripts are
#'     to be added.  Note that the target directory must exist and be
#'     empty in order for this function to work.  The
#'     existence-requirement is included in order to avoid that
#'     directories are accidentally created, and the empty-requirement
#'     is included in order to avoid that previously extracted (and
#'     potentially modified) versions of the scripts are overwritten.
#'
#' @return If successful, the scripts are moved into \code{target_dir},
#'     and the user can then run them (or modify them) as desired.
#'
#' @export

LG_extract_scripts <- function(target_dir) {
    ##  Identify the source path.
    .package <- "localgaussSpec"
    .script_dir <- "localgaussSpec_scripts"
    .source_path <- file.path(
        find.package(package = .package),
        .script_dir)
    kill(.package, .script_dir)
    ##  Check that there is a directory in the package with the
    ##  desired name.
    if (!dir.exists(paths = .source_path))
        error(c("The specified internal source directory does not exist!",
                "Please inform the package maintainer about the problem."))
    ##  Check that 'target_dir' exists.
    if (!dir.exists(paths = target_dir))
        error(.argument = "target_dir",
              c("The target directory",
                sQuote(target_dir),
                "does not exist!",
                "Create it first, and then try again."))
    ##  Check that 'target_dir' is empty.
    .existing_content <- list.files(
        path = target_dir,
        full.names = TRUE,
        recursive = TRUE)
    if (length(.existing_content)!=0)
        error(.argument = "target_dir",
              c("The target directory",
                sQuote(target_dir),
                "must be empty!"))
    kill(.existing_content)
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
    cat(sprintf("\n%i script-file%s copied to the directory %s.\n",
                .nr_files,
                ifelse(test = .nr_files>1,
                       yes  = "s",
                       no   = ""),
                sQuote(target_dir)))
}

## Reminder from the testing:
## target_dir <- "delete_this_after_use"
## dir.create(path = target_dir)
## extract_scripts(target_dir = target_dir)
## unlink(x = target_dir, recursive = TRUE, force = TRUE)


