#' @param LG_type One of \code{c("par_five", "par_one")}, i.e. should the
#'     Local Gaussian Autocorrelations be based on the approach using
#'     five parameters or the simplified approach using only one
#'     parameter.  The default choice is \code{"par_five"}.

#' @note Regarding the case where the \code{LG_type}-argument is equal
#'     to "par_one": The author of this package has always considered
#'     the "par_one"-approach to be reasonable when the aim of the
#'     investigation is to estimate a density at a given point.
#'     However, the extraction of the correlation value from the
#'     resulting density-estimate will in general not capture the
#'     local geometrical properties of the targeted distribution at
#'     the point of investigation.  The "par_one"-approach is as such
#'     (in general) a complete waste of computation resources.
#'
