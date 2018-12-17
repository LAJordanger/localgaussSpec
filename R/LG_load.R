################################################################################
#'
#' Load objects into environments.
#'
#' This function loads a file into an environment, and gives the
#' content the specified name.  It's required that the file only
#' contains one single object.
#'
#' @param .file The path to the file to be loaded.  An error will be
#'     returned if more than one object is contained in \code{.file}.
#'
#' @param .env The environment, default \code{parent.frame()}, where
#'     the result should be added under the name given to the argument
#'     \code{.name}.
#'
#' @param .name A character-string giving the name to be used for the
#'     object loaded from \code{.file}.  The default value \code{NULL}
#'     will trigger the original name of the loaded object, i.e. the
#'     name it was saved under in \code{.file}.
#'
#' @param .attributes_extra A list containing additional attributes to
#'     be added to the loaded object.  The default value \code{NULL}
#'     will skip this part.
#'
#' @return An object named \code{.name} will be added from
#'     \code{.file} to \code{.env}, with the possible extension of
#'     attributes as given to \code{.attributes_extra}.
#'
#' @keywords internal

LG_load <- function(.file,
                    .env = parent.frame(), 
                    .name = NULL,
                    .attributes_extra = NULL) {
    ##  Sanity-check the filename.
    if (! file.exists(.file))
        error(.argument = ".file",
              c("Could not find the file ",
                sQuote(.file)))
    ##  Load `.file` into a temporary environment
    .tmp_env <- new.env()
    load(file = .file,
         envir = .tmp_env)
    ##  Check the content.
    .content <- ls(envir = .tmp_env, all.names = TRUE)
    ##  Sanity-check the number of elements.
    if (length(.content) != 1)
        error(.argument = ".file",
              "The file must contain only one object!")
    ##  Update `.name`, when necessary.
    if (is.null(.name))
        .name <- .content
    ##  Add attributes when relevant.
    if (! is.null(.attributes_extra))
        attributes(.tmp_env[[.content]]) <- c(
            attributes(.tmp_env[[.content]]),
            .attributes_extra)
    ##  Assign the result to `.env`
    assign(x = .name,
           value = .tmp_env[[.content]],
           envir = .env)
    ##  Return nothing to the workflow.
    invisible()
}
