#' localgaussSpec
#'
#' A package for the investigation of time series by means of
#' Local Gaussian Spectral Densities.
#'
#' The localgaussSpec package contains the functions needed for the
#' investigation by means of local Gaussian approximations.  Moreover,
#' it also contains copies of the scripts needed for a reproduction of
#' results mentioned in ...  See <helper-function> for details.
#'
#' @section Simulating data:
#' Some stuff about the functions used for simulations, and the
#' format of the result.
#'
#' @section Computation of local Gaussian approximations:
#' blah blah...
#'
#' @section Computation of local Gaussian spectra:
#' blah blah...
#'
#' @section Computation of bootstrap-based confidence intervals:
#' blah blah...
#' 
###############
## Add details here with regard to packages/functions to import.
## #' @import foreach
## #' @import shiny
## #' @import stringr
## #' @importFrom abind abind adrop
## #' @importFrom alabama auglag
## #' @importFrom ggplot2 ggplot coord_cartesian geom_hline geom_vline
## #'     labs geom_line aes annotate theme geom_ribbon geom_boxplot
## #'     cut_width
## #' @importFrom graphics hist plot
## #' @importFrom localgauss localgauss
## #' @importFrom logspline logspline dlogspline plogspline
## #' @importFrom mvtnorm dmvnorm pmvnorm
## #' @importFrom magrittr and or divide_by
## #' @importFrom markdown renderMarkdown
## #' @importFrom methods findMethods is
## #' @importFrom plyr aaply
## #' @importFrom pryr where dots
## #' @importFrom stats dnorm pnorm qnorm rnorm acf as.ts coef kernel lag
## #'     mad median optim optimise optimize pacf quantile runif sd
## #'     spec.pgram var
## #' @importFrom timeDate kurtosis skewness
## #' @importFrom tseries tsbootstrap
## #' @importFrom ToolBox spy create_call restrict_array
## #'     append_dimensions split_vector my_apply my_abind aa_restrict
## #'     multiply_arrays add_arrays append_dimensions
## #'     compare_spy_reports update_formals dimnames_union
## #'     dimnames_intersect update_array nested_if skeleton_list
## #'     aa_where_and_who my_aaply file_created array_from_files
## #'     set_seed error kill this_function list_array_join
## #'     list_array_dims array_to_list_array
## #' @importFrom utils capture.output combn head object.size str tail

#' @importFrom abind abind adrop
#' @importFrom foreach %do% %dopar% foreach getDoParWorkers
#' @importFrom digest digest
#' @importFrom leanRcoding add_arrays append_dimensions
#'     compare_spy_reports create_call dimnames_intersect error
#'     file_created list_array_dims list_array_join kill
#'     multiply_arrays my_aaply my_abind my_apply nested_if
#'     restrict_array set_seed skeleton_list split_vector spy
#'     this_function update_array update_formals
#' @importFrom localgauss localgauss
#' @importFrom logspline logspline plogspline
#' @importFrom magrittr and or divide_by
#' @importFrom methods findMethods
#' @importFrom mvtnorm dmvnorm pmvnorm
#' @importFrom plyr aaply
#' @importFrom stats kernel mad median optimise qnorm quantile rnorm
#'     runif sd var
#' @importFrom stringr str_detect str_replace str_replace_all
#'     str_split str_sub
#' @importFrom timeDate kurtosis skewness
#' @importFrom tseries tsbootstrap
#' @importFrom utils capture.output combn head object.size tail

###############

#' @docType package
#' @name localgaussSpec
NULL


################################################################################
#####  A list of the available (reproducible) time series, that will
#####  be populated during the compilation of the package.
#'
#' Time Series Families
#'
#' The function that computes the samples will use keywords to select
#' a relevant part from the list \code{TS_families}.  The idea is that
#' this should help document all the parameters used in the sampling
#' procedure, such that proper calls can be stored that later on can
#' be resurrected if a reproduction of the computation is desired.
## #'
## #' To ensure documentation of the parameters used when
## #' sampling, and to The idea with this list is tasks later on should
## #' be simplified, in particular with regard to numerical
## #' investigations where we would like to re-sample some time series a
## #' lot of times with assorted values for its parameters.
## #'
## #' This is a list of lists containing four mandatory parts, and an
## #' additional field to be used for cases where a method must be
## #' specified.
## #'
## #'
## #'
## #' 1) The name of the package that contains the desired generating function.
## #' 2) The name of the desired function.
## #' 3) A list with the default values for its arguments.
## #' 4) The name of the argument that decides the number of samples.
## #' 5) Method needed to extract the correct part from some other package.
#'
#' @format A list of lists, the names at the top-level will be used as
#'     "keys" that selects the relevant sub-list.  The sublists
#'     contanins the parts \code{fun}, \code{package}, \code{args},
#'     \code{size_name} and \code{method}, as described below:
#'
#' \describe{
#' 
#' \item{fun}{The name of the desired function, as a
#'     character-string.}
#' 
#' \item{package}{The name of the package that contains \code{fun}, as
#'     a character-string.}
#'
#' \item{args}{A list containing the default arguments to be used in
#'     \code{fun}.  Reminder: In order for the package to compile, it
#'     might be necessary to quote the list if there are references to
#'     packages not imported into this package.}
#'
#' \item{size_name}{The name of the argument of \code{fun} that
#'     specifies the number of samples to be created.}
#'
#' \item{method}{A character string that has to be specified if
#'     \code{fun} needs to be "extracted" from some deeper level of
#'     \code{package}.  Simply drop this part when it is superfluous.}
#' 
#' }
#'
## #' @usage This list is used as a reference for other functions.
#' @name TS_families
#' @keywords internal

TS_families <- if (exists("TS_families", inherits = FALSE)) {
                   TS_families
               } else
                  list()


###  REMINDER: This list will be created when the files starting with
###  "TS_family_" are read during the loading of the package.
