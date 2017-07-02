#' @param cut_vec The truncation-points to be used for the functions
#'     selected in \code{window}.  If the default value \code{NULL} is
#'     used, the whole range of truncation points from \code{1} to the
#'     highest available (i.e. the highest recorded lag plus one) will
#'     be computed and returned to the work-flow.  A restriction of
#'     \code{cut_vec} to a subset might in particular be a good idea
#'     to do when bootstrap is used to estimate confidence intervals.
