################################################################################

#' Extend select points for the local Gaussian inspections.
#'
#' This function is an internal helper function, that adds diagonally
#' related points to the computation.  In paticular if a point (v1,v2)
#' is present, then this function will add the three points (v1,v1),
#' (v2,v2) and (v2,v1). 
#' 
#' @param LG_points The result of the function 'LG_select_points'.
#'
#' @return An extended version of the array from \code{LG_points} will
#'     be returned, with column names "v1" and "v2" and row names
#'     which identifies the points of interest (to be used in the
#'     argument grids).  The returned array has been screened for
#'     duplicates, and only contains unique points.
#'
#' @export

LG_extend_points <- function(LG_points) {
    ##  Sanity check the argument.
    if (! LG_default$class$points %in% class(LG_points))
        error(.argument = "LG_points",
              c("Argument must be created by",
                sQuote("LG_select_points"),
                "and it must have the class attribute",
                sQuote(LG_default$class$points)))
    ##  Extract the attributes from 'LG_points'.
    .attributes <- attributes(LG_points)
    ##  Find the on-diagonal points of interest.
    .on_diagonal <- local({
        .tmp <- sort(union(x = .attributes$Horizontal,
                           y = .attributes$Vertical))
        structure(
            .Data = cbind(.tmp, .tmp),
            .Dim = c(length(.tmp), 2),
            .Dimnames = list(
                paste(.tmp, .tmp, sep = "_"),
                colnames(LG_points)))
    })
    ##  Find the off-diagonal points of interest.
    .off_diagonal <- local({
        .tmp_reflected <- LG_points[, c(2,1)]
        .tmp_all <- rbind(LG_points,
                          .tmp_reflected)
        ##  Remove points that lies on the diagonal.
        .diagonal <- .tmp_all[, 1] == .tmp_all[, 2]
        .tmp_all <- .tmp_all[! .diagonal, ]
        ##  Remove duplicates, by exploting the way subsetting by
        ##  names works when duplicates are present.
        rownames(.tmp_all) <- paste(.tmp_all[, 1],
                                    .tmp_all[, 2],
                                    sep = "_")
        .tmp_all[unique(rownames(.tmp_all)), ]
    })
    ##  Combine the results to one array.
    .result <- rbind(
        .on_diagonal,
        .off_diagonal)
    ##  Add attributes needed later on.
    attributes(.result) <- c(
        attributes(.result),
        local({
            .select_old <- setdiff(
                x = names(.attributes),
                y = c("dim", "dimnames"))
            .attributes[.select_old]
        }),
        list(on_diagonal = rownames(.on_diagonal),
             off_diagonal = rownames(.off_diagonal)))
    ##  Return the result to the workflow.
    .result
}
