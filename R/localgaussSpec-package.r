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
#' @import shiny
#' @importFrom abind abind adrop
#' @importFrom foreach %do% %dopar% foreach getDoParWorkers
#' @importFrom digest digest
#' @importFrom ggplot2 ggplot coord_cartesian labs geom_line
#'     geom_hline geom_vline geom_boxplot geom_contour geom_tile
#'     geom_ribbon aes annotate theme cut_width element_blank xlab
#'     ylab scale_fill_gradient2 element_text ggtitle ylim rel
#' @importFrom graphics plot
#' @importFrom leanRcoding add_arrays append_dimensions
#'     compare_spy_reports create_call dimnames_intersect
#'     dimnames_union error file_created kill multiply_arrays my_aaply
#'     my_abind my_apply nested_if restrict_array set_seed
#'     skeleton_list split_vector spy this_function update_array
#' @importFrom localgauss localgauss
#' @importFrom magrittr and or divide_by
#' @importFrom markdown renderMarkdown
#' @importFrom methods findMethods is
#' @importFrom mvtnorm dmvnorm pmvnorm
#' @importFrom plyr aaply
#' @importFrom pryr where
#' @importFrom scales percent
#' @importFrom stats kernel lag mad median optimise pnorm qnorm
#'     quantile rnorm runif sd var acf as.ts pacf spec.pgram
#' @importFrom stringr str_detect str_replace
#'     str_replace_all str_split str_sub
#' @importFrom timeDate kurtosis skewness
#' @importFrom tseries tsbootstrap
#' @importFrom tools toTitleCase
#' @importFrom utils capture.output combn head object.size str tail



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
