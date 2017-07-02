################################################################################
#####  2017-01-16:

#' Compute values based on the cross- and auto-spectra.
#'
#' This function computes assorted spectra based on list-arrays
#' containing the cross- and auto-spectra, i.e. the phase-, amplitude
#' and squared coherence can be computed by this approach.  It is also
#' assumed that the arrays has been stored using the "folding" that
#' enables redundant information to be dropped, i.e. for the pairs
#' indexed "Yi_Yj" with "i" and "j" integers, it will always be
#' assumed that "i" is smaller than "j".  Finally, since this function
#' works as an internal helper in \code{LG_spectra}, it will also be
#' asumed that the list contains information about the variable pairs.
#'
#' @param .list_array The list whose nodes contains the arrays of
#'     interest.  Note, it is assumed that this list has depth two,
#'     and that the following branches must be present: "auto/cross"
#'     and "Re/Im" at respectively first an second level.
#'
#' @param .branch Either "on_diag" or "off_diag".  This selects the
#'     main branch of \code{.list_array} that is to be operated upon.
#'     If unspecified, the default will be to use "on_diag".
#'
#' @param .type Either "auto" or "cross", decides which main branch of
#'     \code{.list_array} that is to be operated upon.  If
#'     unspecified, the default will be to use "auto".
#'
#' @param .spectrum One or more of the values "phase", "amplitude" and
#'     "squared_coherence".
#'
#' @return The result of this function will be a list of arrays
#'     related to the combination of the arguments \code{.branch},
#'     \code{.type} and \code{.spectrum}.  The content of some of the
#'     lists are a bit misleading, as they are placeholders (without
#'     the correct content) that is needed in order for the code later
#'     on to work as required.  This is done in order to avoid wasting
#'     storage place on entities that can be computed directly later
#'     on. Moreover, this function is written with the intention of
#'     being used in a loop over the available arguments, so some
#'     combinations of the arguments (.i.e. "on_diag" and "auto") only
#'     contains placeholders witout any interesting content.
#' 
#' @export


LG_auto_cross_computations <- function(
             .list_array,
             .branch = c("on_diag", "off_diag"),
             .type = c("auto", "cross"),
             .spectrum = c("phase", "amplitude", "squared_coherence")) {
###-------------------------------------------------------------------
    ##  Restrict '.branch' and '.type' to length 1.
    .branch <- .branch[1]
    .type <- .type[1]
    .node <- c(.branch, .type)
    kill(.branch, .type)
    ##  Additional nodes of interest for the computation.
    .node_Re <- c(.node, "Re")
    .node_negIm <- c(.node, "negIm")
    .on_auto_Re <- c("on_diag", "auto", "Re")
    .off_auto_Re <- c("off_diag", "auto", "Re")
    .off_auto_negIm <- c("off_diag", "auto", "negIm")
###-------------------------------------------------------------------
    ##  The default placeholder to be used when no computuations are
    ##  to be performed at this stage.
    .default_list <- list(
        phase = LG_default$result$.azero,
        amplitude = LG_default$result$.azero,
        squared_coherence = LG_default$result$.azero)
###-------------------------------------------------------------------
    ##  Investigate and return suitable lists for the different cases
    ##  that can occur. 
    if (all(.node == c("on_diag", "auto")))
        return(.default_list)
###-------------------------------------------------------------------
    if (all(.node == c("off_diag", "auto"))) {
        ##  Off diagonal points might not be present; if so only
        ##  return placeholders.  Test by considering the "Re"-part.
        if (identical(x = .list_array[[.node_Re]],
                      y =  LG_default$result$.azero))
            return(.default_list)
###-------------------------------------------------------------------
        ##  Compute the arrays of interest (or leave placeholders).
        .complex_arr <- add_arrays(
            .arr1 = .list_array[[.node_Re]],
            .arr2 = - 1i * .list_array[[.node_negIm]])
        .phase_arr <-
            if ("phase" %in% .spectrum) {
                Arg(.complex_arr)
            } else
                LG_default$result$.azero
        .amplitude_arr <-
            if (any(c("amplitude", "squared_coherence") %in% .spectrum)) {
                Mod(.complex_arr)
            } else
                LG_default$result$.azero
        kill(.complex_arr)
        .squared_coherence_arr <-
            if ("squared_coherence" %in% .spectrum) {
                local({
                    ##  Find the '.marginal_product_arr' to be used as
                    ##  the denominator in the squared coherence.
                    ##  Three steps: Find and extract the diagonally
                    ##  associated points (v1,v1) and (v2,2), rename
                    ##  them to enable the product to be taken.
                    .levels_off <- dimnames(.list_array[[.node_Re]])$levels
                    ##  Need to split these, and create the vectors
                    ##  needed for the subsetting from 'node_auto'
                    .levels_on <- vapply(
                        X = .levels_off,
                        FUN = function(x) {
                            .tmp <- strsplit(x = x, split = "_")[[1]]
                            c(.first = paste(.tmp[c(1,1)],
                                             collapse = "_"),
                              .second = paste(.tmp[c(2,2)],
                                              collapse = "_"))
                        },
                        FUN.VALUE = character(2))
                    ##  Extract the two denominator-arrays
                    ..first_array <- restrict_array(
                        .arr = .list_array[[.on_auto_Re]],
                        .restrict = list(levels = .levels_on[".first", ]))
                    ..second_array <- restrict_array(
                        .arr = .list_array[[.on_auto_Re]],
                        .restrict = list(levels = .levels_on[".second", ]))
                    ##  Adjust the dimension-names to enable the use of
                    ##  'multiply_arrays'.
                    dimnames(..first_array)$levels <- .levels_off
                    dimnames(..second_array)$levels <- .levels_off
                    ##  Return the array of interest, which in this case
                    ##  is the squared amplitude divided by the product of
                    ##  '..first_array' and '..second_array'.
                    multiply_arrays(
                        .arr1 = .amplitude_arr^2,
                        .arr2 = 1 / multiply_arrays(
                                        .arr1 = ..first_array,
                                        .arr2 = ..second_array))
                })
            } else
                LG_default$result$.azero
        ##  Reset the value of 'amplitude_arr' if only
        ##  'squared_coherence_arr' was the one asked for.
        if (! "amplitude" %in% .spectrum)
            .amplitude_arr <- LG_default$result$.azero
        kill(.node_Re, .node_negIm, .spectrum, .on_auto_Re)
        ##  NB: The list of arrays are returned at the end.
    }
###-------------------------------------------------------------------
    if (all(.node == c("on_diag", "cross"))) {
        ##  Cross-terms might not be present; if so only return
        ##  placeholders.  Test by considering the "Re"-part.
        if (identical(x = .list_array[[.node_Re]],
                      y =  LG_default$result$.azero))
            return(.default_list)
###-------------------------------------------------------------------
        ##  Compute the arrays of interest (or leave placeholders).
        .complex_arr <- add_arrays(
            .arr1 = .list_array[[.node_Re]],
            .arr2 = - 1i * .list_array[[.node_negIm]])
        .phase_arr <-
            if ("phase" %in% .spectrum) {
                Arg(.complex_arr)
            } else
                LG_default$result$.azero
        .amplitude_arr <-
            if (any(c("amplitude", "squared_coherence") %in% .spectrum)) {
                Mod(.complex_arr)
            } else
                LG_default$result$.azero
        kill(.complex_arr)
        .squared_coherence_arr <-
            if ("squared_coherence" %in% .spectrum) {
                local({
                    ##  Find the '.marginal_product_arr' to be used as
                    ##  the denominator in the squared coherence.
                    ##  Three steps: Indexing, renaming, product.
                    .pairs_cross <- dimnames(.list_array[[.node_Re]])$pairs
                    ##  Need to split these, and create the vectors
                    ##  needed for the subsetting from 'node_auto'
                    .pairs_auto <- vapply(
                        X = .pairs_cross,
                        FUN = function(x) {
                            .tmp <- strsplit(x = x, split = "_")[[1]]
                            c(.first = paste(.tmp[c(1,1)],
                                             collapse = "_"),
                              .second = paste(.tmp[c(2,2)],
                                              collapse = "_"))
                        },
                        FUN.VALUE = character(2))
                    ##  Extract the two denominator-arrays
                    ..first_array <- restrict_array(
                        .arr = .list_array[[.on_auto_Re]],
                        .restrict = list(pairs = .pairs_auto[".first", ]))
                    ..second_array <- restrict_array(
                        .arr = .list_array[[.on_auto_Re]],
                        .restrict = list(pairs = .pairs_auto[".second", ]))
                    ##  Adjust the dimension-names to enable the use of
                    ##  'multiply_arrays'.
                    dimnames(..first_array)$pairs <- .pairs_cross
                    dimnames(..second_array)$pairs <- .pairs_cross
                    ##  Return the array of interest, which in this case
                    ##  is the squared amplitude divided by the product of
                    ##  '..first_array' and '..second_array'.
                    multiply_arrays(
                        .arr1 = .amplitude_arr^2,
                        .arr2 = 1 / multiply_arrays(
                                        .arr1 = ..first_array,
                                        .arr2 = ..second_array))
                })
            } else 
                LG_default$result$.azero        
        ##  Reset the value of 'amplitude_arr' if only
        ##  'squared_coherence_arr' was the one asked for.
        if (! "amplitude" %in% .spectrum)
            .amplitude_arr <- LG_default$result$.azero
        kill(.node_Re, .node_negIm, .spectrum, .on_auto_Re)
        ##  NB: The list of arrays are returned at the end.
    }
###-------------------------------------------------------------------
    if (all(.node == c("off_diag", "cross"))) {
        ##  Off diagonal points might not be present; if so only
        ##  return placeholders.  Test by considering the "Re"-part.
        if (identical(x = .list_array[[.node_Re]],
                      y =  LG_default$result$.azero))
            return(.default_list)
        ##  Reminder: In this case, the formula for the local Gaussian
        ##  analogue of the squared coherence will in fact be a
        ##  complex value.  To enable the code later on to digest
        ##  everything in the same manner, the result for that case
        ##  will be stored as an array having a dimension containing
        ##  the spectra
###-------------------------------------------------------------------
        ##  Compute the arrays of interest (or leave placeholders).
        .complex_arr_cross <- add_arrays(
            .arr1 = .list_array[[.node_Re]],
            .arr2 = - 1i * .list_array[[.node_negIm]])
        .phase_arr <-
            if ("phase" %in% .spectrum) {
                Arg(.complex_arr_cross)
            } else
                LG_default$result$.azero
        .amplitude_arr <-
            if ("amplitude" %in% .spectrum) {
                Mod(.complex_arr_cross)
            } else
                LG_default$result$.azero
        .squared_coherence_arr <-
            if ("squared_coherence" %in% .spectrum) {
                local({
                    ##  Find four complex-valued arrays corresponding
                    ##  to the objects in the numerator and
                    ##  denominator of the "squared coherence".  For
                    ##  the denominator: Find the relevant
                    ##  auto-spectra, i.e. from (Yi,Yj) create (Yi,Yi)
                    ##  and (Yj,Yj), extract the parts and rename them
                    ##  to enable the product to be taken.  For the
                    ##  numerator, the term (Yj,Yi) can be found by
                    ##  "flipping" to the reflected points and then
                    ##  rename the result.
                    .pairs_cross <- dimnames(.list_array[[.node_Re]])$pairs
                    ##  Split '.pairs_cross' and create the vectors
                    ##  needed for the subsetting from 'node_auto'
                    .pairs_auto <- vapply(
                        X = .pairs_cross,
                        FUN = function(x) {
                            .tmp <- strsplit(x = x, split = "_")[[1]]
                            c(.first = paste(.tmp[c(1,1)],
                                             collapse = "_"),
                              .second = paste(.tmp[c(2,2)],
                                              collapse = "_"))
                        },
                        FUN.VALUE = character(2))
                    ##  Find the two complex-valued arrays in the
                    ##  denominator.  Reminder: The present solution
                    ##  is used since 'add_arrays' became a bit
                    ##  "unhappy" since the subsetting created
                    ##  repeated names in the 'pairs'-dimension when
                    ##  it was used with 'restrict_array' inside of
                    ##  its arguments.
                    ..first_array_Re <- restrict_array(
                        .arr = .list_array[[.off_auto_Re]],
                        .restrict = list(pairs = .pairs_auto[".first", ]))
                    ..first_array_negIm <- restrict_array(
                        .arr = .list_array[[.off_auto_negIm]],
                        .restrict = list(pairs = .pairs_auto[".first", ]))
                    dimnames(..first_array_Re)$pairs <- .pairs_cross
                    dimnames(..first_array_negIm)$pairs <- .pairs_cross
                    ..first_array <- add_arrays(
                        .arr1 = ..first_array_Re,
                        .arr2 = - 1i * ..first_array_negIm)
                    kill(..first_array_Re, ..first_array_negIm)
                    ..second_array_Re <- restrict_array(
                        .arr = .list_array[[.off_auto_Re]],
                        .restrict = list(pairs = .pairs_auto[".second", ]))
                    ..second_array_negIm <- restrict_array(
                        .arr = .list_array[[.off_auto_negIm]],
                        .restrict = list(pairs = .pairs_auto[".second", ]))
                    dimnames(..second_array_Re)$pairs <- .pairs_cross
                    dimnames(..second_array_negIm)$pairs <- .pairs_cross
                    ..second_array <- add_arrays(
                        .arr1 = ..second_array_Re,
                        .arr2 = - 1i * ..second_array_negIm)
                    kill(..second_array_Re, ..second_array_negIm)
                    ##  The first array to be used in the numerator is
                    ##  '.complex_arr_cross' (already computed), the
                    ##  second array can be created from this by
                    ##  replacing level-points with their diagonally
                    ##  reflected versions, and then taking the
                    ##  complex conjugate of the new array.  
                    .levels_off <- dimnames(.list_array[[.node_Re]])$levels
                    ##  Need to split these, and create the vectors
                    ##  needed for the subsetting from 'node_auto'
                    .levels_off_reflected <- vapply(
                        X = .levels_off,
                        FUN = function(x) {
                            .tmp <- strsplit(x = x, split = "_")[[1]]
                            paste(.tmp[c(2,1)],
                                  collapse = "_")
                        },
                        FUN.VALUE = character(1))
                    .complex_arr_cross_reflected <- Conj(
                        z = restrict_array(
                            .arr = .complex_arr_cross,
                            .restrict = list(levels = .levels_off_reflected)))
                    dimnames(.complex_arr_cross_reflected)$levels <- .levels_off
                    ##  Create the analogue of the squared coherence,
                    ##  i.e. the complex array constructed from the
                    ##  four complex arrays at hand.
                    .analogue_squared_coherence <- multiply_arrays(
                        .arr1 = multiply_arrays(
                            .arr1 = .complex_arr_cross,
                            .arr2 = .complex_arr_cross_reflected),
                        .arr2 = 1 / multiply_arrays(
                                        .arr1 = ..first_array,
                                        .arr2 = ..second_array))
                    kill(..first_array, ..second_array,
                         .complex_arr_cross,
                         .complex_arr_cross_reflected)
                    ##  Create the final result as an array that
                    ##  contains "Re", "negIm", "amplitude" and "phase"
                    ##  of '.analogue_squared_coherence'.  Reminder:
                    ##  If only these derived values were of interest,
                    ##  they could have been computed on the fly later
                    ##  on when needed.  However, in the quest for
                    ##  confidence intervals the 'quantile'-function
                    ##  will be used on the stored values, and then
                    ##  nonsense would result if only the complex
                    ##  numbers were stored!
                    .along <- which(names(dimnames(.analogue_squared_coherence)) == "spec")
                    .new_names <- dimnames(.analogue_squared_coherence)
                    .new_names$spec <- c("Re", "negIm", "amplitude", "phase")
                    result <- abind(
                        Re(.analogue_squared_coherence),
##### TASK: Should it be "-1 * Im()" here ?
                        Im(.analogue_squared_coherence),
                        Mod(.analogue_squared_coherence),
                        Arg(.analogue_squared_coherence),
                        along = .along,
                        new.names = .new_names,
                        use.dnns = TRUE)
                    class(result) <- class(.analogue_squared_coherence)
                    ##  Return the result to the workflow.
                    result
                })
            } else
                LG_default$result$.azero
        kill(.node_Re, .node_negIm, .spectrum, .on_auto_Re)
    }
###-------------------------------------------------------------------
    ##  All cases covered, return the results to the workflow.
    list(
        amplitude = .amplitude_arr,
        phase = .phase_arr,
        squared_coherence = .squared_coherence_arr)
}
