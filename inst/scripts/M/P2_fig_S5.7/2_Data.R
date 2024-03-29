###----------------------------------------------------------------###
##  The EuStockMarkets-example from P2_fig_S5.7.

##  An investigation of the effect on the estimated pointwise
##  confidence intervals as the block length varies.

##  The basic idea in this script is to use a loop-construction to do
##  the computation for the block lengths of interest. The same seeds
##  will be used for each block length, in order to remove that as a
##  source of variation.

##  Load the required library

library(localgaussSpec)

###----------------------------------------------------------------###

##  Specify the directory where the file-hierarchy will be stored.

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_S5.7_S5.8")

###----------------------------------------------------------------###

##  Compute the daily log-returns to be used in the computation.

.first <- head(EuStockMarkets, n = -1)
.second <- tail(EuStockMarkets, n = -1)
.TS <- log(.second/.first)
rm(.first, .second)


##  Restricts to the first and third components (DAX and CAC), in
##  order to reduce the computational load.

.TS <- .TS[, c("DAX", "CAC")]

##  Save to file and update file-hierarchy.

set.seed(136)
.TS_LG_object <- TS_LG_object(
    TS_data = .TS,
    main_dir = main_dir)
rm(.TS, main_dir)

###----------------------------------------------------------------###

##  Compute the local Gaussian autocorrelations.

##  This requires a specification of the desired points, the bandwidth
##  and the number of lags. WARNING: The type of approximation must
##  also be specified, i.e. the argument 'LG_type', where the options
##  are "par_five" and "par_one".  The "five" and "one" refers to the
##  number of free parameters used in the approximating bivariate
##  local Gaussian density.  The results should be equally good for
##  Gaussian time series, but the "par_one" option will in general
##  produce dubious/useless results.  Only use "par_one" if it is of
##  interest to compare the result with "par_five", otherwise avoid it
##  as it most likely will be a waste of computational resources.

.LG_type <- "par_five"
.LG_points <- LG_select_points(
    .P1 = 0.1,
    .P2 = 0.9,
    .shape = 3)
lag_max <- 10 ## 15

.b <- 0.6

##  Do the main computation.

LG_AS <- LG_approx_scribe(
    main_dir = .TS_LG_object$TS_info$main_dir,
    data_dir = .TS_LG_object$TS_info$save_dir,
    TS = .TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,
    LG_type = .LG_type)
rm(.TS_LG_object, lag_max, .LG_points, .b, .LG_type)

##  Specify the details needed for the construction of the
##  bootstrapped pointwise confidence intervals, and do the
##  computations.  Note that the default for the 'boot_type'-argument
##  is "cibbb_tuples", i.e. the circular index based block bootstrap
##  for tuples discussed in paper P1.

nb <- 100
block_length_vec <- 10:69

for (block_length in block_length_vec) {
    set.seed(1421236)
    LG_BS <- LG_boot_approx_scribe(
        main_dir        = LG_AS$main_dir,
        data_dir        = LG_AS$data_dir,
        nb              = nb,
        boot_type       = "cibbb_tuples",
        block_length    = block_length,
        boot_seed       = NULL,
        lag_max         = NULL,
        LG_points       = NULL,
        .bws_mixture    = NULL,
        bw_points       = NULL,
        .bws_fixed      = NULL,
        .bws_fixed_only = NULL,
        content_details = NULL,
        LG_type         = NULL,
        threshold       = 100)
}
rm(nb, block_length, block_length_vec, LG_AS)

##  The 'NULL'-arguments ensures that the same values are used as in
##  the computation based on the original sample. (These 'NULL'-values
##  are the default values for these arguments, and it is thus not
##  necessary to specify them.)  It is possible to restrict these
##  arguments to a subset (of the original one) if that is desirable.
##  In particular: It might not be too costly to compute the local
##  Gaussian spectral density for a wide range of input parameters
##  when only the original sample is considered, and it could thus be
##  of interest to first investigate that result before deciding upon
##  which subsets of the selected parameter-space that it could be
##  worthwhile to look closer upon.

###----------------------------------------------------------------###

##  Send code to terminal that can be used to start the interactive
##  inspection based on the shiny-application 'LG_shiny'.  It might be
##  of interest to save this to a file so an inspection later on does
##  not require this script.  Note that there are some tests in the
##  code that try to prevent things that have already been computed
##  from being computed once more, but the initial computation of
##  '.TS_sample' will be performed every time this script is used.

LG_shiny_writeLines(
    main_dir = LG_BS$main_dir,
    data_dir = LG_BS$data_dir)

##  Start the shiny application for an interactive inspection of the
##  result.  The use of 'shiny::runApp' is needed in order to start
##  the shiny-application when this script is sourced.

shiny::runApp(LG_shiny(
    main_dir = LG_BS$main_dir,
    data_dir = LG_BS$data_dir))

###----------------------------------------------------------------###
