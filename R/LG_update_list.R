#' Updating of internal lists.
#'
#' @description This internal function is primarily used to update the
#'     \code{curlicues}-list.
#' 
#' @param .new A list that should be used to update (or add) content
#'     to the list given in \code{.old}
#'
#' @param .old The list to be updated with the content from the list
#'     given in \code{.new}.
#'
#' @return The result of this function will be an updated version of
#'     the list given in \code{.old}.  Things in \code{.new} that is
#'     not present in \code{.old} will be added, whereas those parts
#'     that occur both places will be updated.
#'
#' @keywords internal

LG_update_list <- function(.new, .old) {
    ##  Check that the arguments have the correct properties.
    .compatible <-  identical(x = is.list(.new),
                              y = is.list(.old))
    if (!.compatible)
        error("Update failed due to mismatch between new and old stuff")
    ##  Identify new and old parts.  It is here assumed that any
    ##  'new stuff' can be concatenated at the end of 'old stuff'.
    .new_names <- names(.new)
    .completely_new <- which(! .new_names %in% names(.old))
    .update_these <- which(.new_names %in% names(.old))
    ##  Update when required
    if (length(.update_these) > 0) {
        for (.name in .new_names[.update_these]) {
            if (is.list(.new[[.name]])) {
                .old[[.name]] <- LG_update_list(
                    .new = .new[[.name]],
                    .old = .old[[.name]])
            } else
                .old[.name] <- .new[.name]
        }
    }
    ##  Concatenate completely new stuff at the end.
    is (length(.completely_new) > 0)
    .old <- c(.old,
              .new[.completely_new])
    ##  Return the revised version of '.old' to the workflow.
    .old
}
