#' @param .method An argument to be used by \code{LG_approx_extract}.
#'     This has to be one or more of the values in \code{c("all",
#'     "mean", "median")}, but the argument will only have an effect
#'     if the length of the \code{content}-dimension in the
#'     \code{LG_type} part of \code{approx_data} is larger than one.
#'     The idea is that this argument will be used with the values
#'     "mean" and "median" while investigating blocks, and the value
#'     "all" when doing an ordinary investigation.
