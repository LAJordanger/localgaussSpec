#' Keep control over the curlicues to be used on the plots
#'
#' @param new_curlicues A list that contains values that should be
#'     used to update the content of \code{old_curlicues}.
#'
#' @param old_curlicues The list to be updated with the content from the list
#'     given in \code{new_curlicues}.
#'
#' @return The result of this function will be that the information in
#'     \code{old_curlicues} will be updated based on the information
#'     from \code{new_curlicues}.  If both arguments are equal to the
#'     default value \code{NULL}, then te result will be a copy of the
#'     default values stored in \code{LG_default}.
#'
#' @keywords internal

LG_plot_curlicues <- function(new_curlicues = NULL, old_curlicues = NULL) {
    ##  Get hold of the default-values.
    .result <-
        if (is.null(old_curlicues)) {
            LG_default$curlicues
        } else {
            old_curlicues
        }
    ##  Update the values of interest.
    if (! is.null(new_curlicues)) {
        .result <- LG_update_list(.new = new_curlicues,
                                  .old = .result)
    }
    ##  A special adjustment is needed if the 'NC_value'-node
    ##  specifies a short label.
    if (identical(x = .result$NC_value$short_or_long_label,
                  y = "short")) {
        .result$NC_value$annotate$label =
            attributes(.result$NC_value$annotate$label)$short
    }
    ##  Return the result to the work-flow.
    .result
}
