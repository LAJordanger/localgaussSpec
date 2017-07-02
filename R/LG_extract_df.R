################################################################################
#####  2017-01-16:

#' Final computations of assorted spectra.
#'
#' This function takes care of the extraction (and final adjustment)
#' needed in \code{LG_plot_load} when the list-arrays loaded from
#' different files is to be modified to a data frame which can be
#' digested by the graphical functions.
#'
#' @param .list_array The list whose nodes contains the arrays of
#'     interest.
#'
#' @param .global Logical value, default \code{TRUE}, which decides if
#'     the operation is the one for the ordinary (global) spectral
#'     densities, or if a variant needed for the local Gaussian
#'     versions should be employed.  (This argument is needed since
#'     the depth of the \code{.list_array} is different for the local
#'     and global data.)
#'
#' @param .diag One of the values "on_diag", "off_diag".  Only used
#'     when \code{.global} is \code{FALSE}, i.e. it will for the local
#'     Gaussian cases specify which branch(es) of \code{.list_array}
#'     that should be used in the computation.
#'
#' @param .spectrum One of the values "auto", "co", "quad", "phase",
#'     "amplitude", "squared_coherence".  This specifies the
#'     extraction procedure to be performed.
#'
#' @param .adjust_result Logical value, default \code{FALSE}, which
#'     can be used when there is a need for some minor tweaking of the
#'     result to get the correct version.  This is needed since
#'     "folding" has been used on the saved data to reduce the
#'     memory-load, so for some extractions it will be necessary to
#'     tweak the result a bit.  When this argument is \code{TRUE}, the
#'     effect will be a change of sign when \code{.spectrum} is equal
#'     to "quad" and "phase".
#'
#' @param .branch A list of logical values needed for those cases
#'     where off-diagonal points are encountered.
#'
#' @param .Y_range A vector containing to integers, which can be used
#'     when it is desirable to compute a restricted range where the
#'     most extreme observations have been ignored.  The default value
#'     \code{NULL} takes the full range. 
#'
#' @return The result of this function will either be a restricted
#'     range (when \code{.Y_range} has been specified, or else it will
#'     be a list with two nodes, one node being the full range and the
#'     other node a data-frame (to be used for the plot-functions).
#' 
#' @export

LG_extract_df <- function(.list_array,
                     .global = TRUE,
                     .diag = c("on_diag", "off_diag"),
                     .spectrum = c("auto", "co", "quad",
                                   "phase", "amplitude", "squared_coherence"),
                     .adjust_result = FALSE,
                     .branch = list(),
                     .Y_range = NULL) {
###-------------------------------------------------------------------
    ##  Sanity-check: Only one selected spectrum
    if (length(.spectrum) != 1)
        error(.argument = ".spectrum",
              c("Select one (and only one) of the following values:",
                "\"auto\", \"co\", \"quad\"",
                "\"phase\", \"amplitude\", \"squared_coherence\""))
###-------------------------------------------------------------------
    ##  Restrict to one branch (when possible) for the local cases.
    if (! .global )
        if (length(.diag) == 1)
            .list_array <- .list_array[[.diag]]
    kill(.diag)
###-------------------------------------------------------------------
    ##  Investigate if it is necessary to adjust some of the arguments
    ##  (this might occur due to the way 'LG_load_plot' use a generic
    ##  solution when calling this function - and in particular since
    ##  it was considered better to perform the fine-tuning at this
    ##  stage rather than in the body of the calling function.)
    if (.branch$auto_pair & ! .branch$on_diagonal) 
        if (.global) {
            ##  Ensure that values are present that works for the
            ##  global case when no cross-spectra are present.
            .spectrum <- "auto"
            .adjust_result <- FALSE
        } else
            .adjust_result <- TRUE
#####  REMINDER, 2017-01-30: The comparison between global and local
#####  values is a bit dubious for the off-diagonal case when
#####  autospectra are considered.  Need to tweak this later on.
###-------------------------------------------------------------------
    ##  Define the subsetting to be used for '.spectrum', i.e. if the
    ##  data lives on the "auto" or "cross" branch of '.list_array'.
    au_cr <- "cross"  ##  Default value
    if (.branch$auto_pair & ! .branch$on_diagonal) 
        au_cr <- "auto"  ##  Exceptional case.
###-------------------------------------------------------------------
    ##  The effect of '.adjust_result' will mainly be that a complex
    ##  conjugation will be needed, which for some cases implies that
    ##  there will be a change of sign for the imaginary part. This
    ##  change will only affect the sign of the
    ##  '.spectrum'-alternatives "quad" and "phase"
    .sign <- (-1) * ifelse(test = .adjust_result,
                           yes  = -1,
                           no   = 1)
    kill(.adjust_result)
###-------------------------------------------------------------------
    ##  Extract the result of the selected computation as an
    ##  intermediate array.
    .arr <-
        switch(
        EXPR  = .spectrum,
        auto  = .list_array[[c("auto", "Re")]],
        co    = .list_array[[c(au_cr, "Re")]],
        quad  = if (.sign == 1) {
                    .list_array[[c(au_cr, "negIm")]]
                } else
                    .sign * .list_array[[c(au_cr, "negIm")]],
        phase = if (.sign == 1) {
                    .list_array[[c(au_cr, "phase")]]
                } else
                    .sign * .list_array[[c(au_cr, "phase")]],
        amplitude = .list_array[[c(au_cr, "amplitude")]],
        squared_coherence = .list_array[[c(au_cr, "squared_coherence")]])
    kill(.list_array, .spectrum)
###-------------------------------------------------------------------
    ##  An additional step for the "off-diagonal cross"-case and the
    ##  "squared_coherence" alternative.  The array extracted in the
    ##  previous step will then either be the "Re", "negIm", "amplitude"
    ##  or the "phase"-component of the complex-valued result.  For
    ##  the cases "negIm" and "phase" the result should be multiplied
    ##  with '-1' to get it in line with the other cases.
    if (.branch$od_c_a_sqc)
        if (.branch$RIAP %in% c("negIm", "phase"))
            .arr <- -1 * .arr
###-------------------------------------------------------------------
    if (! is.null(.Y_range)) {
        ##  Compute and return the restricted range.
        .MARGIN = setdiff(x = names(dimnames(.arr)),
                          y = "omega")
        ##  NB: Check if a restriction is needed, and drop the
        ##  restriction step if it is superfluous.
        .full_range <- range(seq_along(dimnames(.arr)$omega))
        .range <-
            if (identical(x = .Y_range, y = .full_range)) {
                range(.arr)
            } else
                range(apply(
                    X = .arr,
                    MARGIN = .MARGIN,
                    FUN = function(x) 
                        sort(x)[.Y_range]))
        return(.range)
    } else {
        ##  Figure out the formula to be used for the creation of the
        ##  data-frame (always end with "omega ~ content").
        .components <- names(dimnames(.arr))
        .formula <- 
            if (length(.components) == 2) {
                quote(omega ~ content)
            } else {
                parse(text = paste(
                          paste(
                              c(setdiff(x = .components,
                                        y = c("omega", "content")),
                                "omega"),
                              collapse = " + "),
                          "content",
                          sep  = " ~ "))
            }
        kill(.components)
        ##  Return the result as a list with the desired data-frame
        ##  and the range of the data.
        list(df = reshape2::dcast(
                 data = reshape2::melt(
                     data = .arr),
                 formula = eval(.formula)),
             range = range(.arr))
    }
}
