###----------------------------------------------------------------###
##  The dmbp-example from P1_fig_08.

##  This script computes the data needed for the combined heatmap- and
##  distance-based plot.

##  Load the required library

library(localgaussSpec)

###----------------------------------------------------------------###

##  Specify the directory where the file-hierarchy will be stored.

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_10.D3")

## Note that 'main_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.

###----------------------------------------------------------------###

## Access the 'dmbp'-data from the 'localgaussSpec'-pakcage (this is a
## copy of the data in the 'rugarch'-package.)

data(dmbp)

.TS <- dmbp[, "V1"]
rm(dmbp)

##  Save to file and update file-hierarchy.

set.seed(136)
tmp_TS_LG_object <- TS_LG_object(
    TS_data = .TS,
    main_dir = main_dir)
rm(.TS)

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
    .P1 = 0.05,
    .P2 = 0.95,
    .shape = 91)
lag_max <- 30

.b <- 0.5

##  Do the main computation.

LG_AS <- LG_approx_scribe(
    main_dir = main_dir,
    data_dir = tmp_TS_LG_object$TS_info$save_dir,
    TS = tmp_TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,
    LG_type = .LG_type)
rm(tmp_TS_LG_object, lag_max, .LG_points, .b, .LG_type)

###----------------------------------------------------------------###

##  Extract the directory information needed for 'LG_shiny'.

data_dir_for_LG_shiny <- LG_AS$data_dir
rm(LG_AS)

##  Send code to terminal that can be used to start the interactive
##  inspection based on the shiny-application 'LG_shiny'.  It might be
##  of interest to save this to a file so an inspection later on does
##  not require this script.  Note that there are some tests in the
##  code that try to prevent things that have already been computed
##  from being computed once more, but the initial computation of
##  '.TS_sample' will be performed every time this script is used.

LG_shiny_writeLines(
    main_dir = main_dir,
    data_dir = data_dir_for_LG_shiny)

##  Start the shiny application for an interactive inspection of the
##  result.  The use of 'shiny::runApp' is needed in order to start
##  the shiny-application when this script is sourced.

shiny::runApp(LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir_for_LG_shiny))

###----------------------------------------------------------------###
