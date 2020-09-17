#' localgaussSpec
#'
#' @description A package for the investigation of univariate and
#'     multivariate time series by means of Local Gaussian (auto- and
#'     cross-) correlations, and the corresponding spectral densities.
#'
#' @section Overview:
#'
#' The \code{localgaussSpec}-package investigates strictly stationary
#' time series by the help of a local Gaussian approach.  This implies
#' that local Gaussian auto- and cross-correlations are computed for a
#' selection of lags and a selection of points -- and based on this it
#' is then possible to investigate the corresponding (m-truncated)
#' local Gaussian spectra.  The local Gaussian correlation coincides
#' with the ordinary correlation when a Gaussian structure is
#' investigated, which implies that it is of interest to compare the
#' ordinary spectra and the local Gaussian spectra since that can
#' reveal the presence of non-Gaussian dependency structures in the
#' time series under investigation.
#' 
#' @section The scripts for reproducibility of results:
#'
#' The scripts that are included in this package provide a simple way
#' to see how the different key-functions should be put together in
#' order to investigate both simulated and real examples.  These
#' scripts are included in order to allow interested readers to
#' reproduce the results and figures in the papers based on this local
#' Gaussian approach, but they can easily be modified in order to
#' investigate similar investigations for other cases.
#'
#' @section Workflow for simulated time series:
#'
#' The scripts for the simulated time series are based on the
#' following sequence of steps and key functions.  The
#' \code{localgaussSpec} package must be loaded, and a
#' \code{main_dir}-argument must be specified.  The function
#' \code{TS_sample} is then called in order to create the desired
#' collection of samples, and this is done by the help of a
#' \code{TS_key}-argument and parameters suitable for this particular
#' key. The required generating functions is part of an internal list
#' called \code{TS_family}, and this list can be extended on demand.
#' Based on the generated time series, a unique \code{save_dir} is
#' computed, and the function \code{TS_LG_object} is then used to
#' initiate a file-hierarchy, in which the results of the local
#' Gaussian investigation will be stored.  The next step is then to
#' specify some details related to the tuning parameters for the
#' estimation algorithm, and for this part the function
#' \code{LG_select_points} can be used to specify different
#' configurations of the points to be investigated (e.g. points along
#' the diagonal or points in a rectangular grid).  After this, the
#' scribe-function \code{LG_approx_scribe} is called in order to
#' perform assorted tasks related to the computation, including a
#' simple check that can prevent previously computed tasks to be
#' recomputed.  If a new computation is encountered, then the
#' scribe-function will save the resulting data into the file
#' hierarchy. The scribe will return a \code{main_dir}-argument and a
#' \code{data_dir}-argument, which is needed in order for
#' \code{LG_shiny} to start a \code{shiny}-application that enables an
#' interactive inspection of the resulting local Gaussian auto- and
#' cross-correlations, together with different graphical
#' visualisations of the corresponding local Gaussian spectra.
#'
#' @section Workflow for real data:
#'
#' The scripts for the real samples are quite similar to those
#' described for the simulated time series.  An obvious difference is
#' that \code{TS_sample} should not be used, since a sample after all
#' is present.  This real sample is sent into \code{TS_LG_object},
#' which creates a unique \code{save_dir} based on the given
#' observations.  After this the same procedure is applied with regard
#' to selecting the tuning parameters for the local Gaussian
#' estimation, and \code{LG_approx_scribe} is used on the single
#' sample.  After this, the function \code{LG_boot_approx_scribe} can
#' be used to produce resampled versions of the time series, and based
#' on the local Gaussian estimates from these resampled versions it is
#' then possible to obtain pointwise confidence intervals for the
#' estimates based on the original sample.  This scribe-function will
#' also return a \code{main_dir}-argument and a
#' \code{data_dir}-argument, in order for the \code{LG_shiny}-function
#' to start the interactive \code{shiny}-investigation.
#' 

## Add details here with regard to packages/functions to import.
#' @import shiny
#' @importFrom abind abind adrop
#' @importFrom foreach %do% %dopar% foreach getDoParWorkers
#' @importFrom digest digest
#' @importFrom ggplot2 ggplot coord_cartesian coord_fixed labs
#'     geom_line geom_hline geom_vline geom_boxplot geom_contour
#'     geom_point geom_tile geom_ribbon aes annotate theme cut_width
#'     element_blank xlab ylab scale_fill_gradient2 element_text
#'     ggtitle ylim rel scale_y_continuous scale_x_continuous
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

#' @docType package
#' @name localgaussSpec

NULL
