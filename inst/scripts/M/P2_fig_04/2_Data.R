###----------------------------------------------------------------###
##  The "bivariate local trigonometric"-example, with individual phase
##  adjustment from P2_fig_04.

##  Load the required library

library(localgaussSpec)

###----------------------------------------------------------------###

##  Specify the directory where the file-hierarchy will be stored.

main_dir <- c("~", "LG_DATA_scripts", "P2_fig_04_S3.4")

## Note that 'main_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.

###----------------------------------------------------------------###

##  Simulate the data to be investigated.

##  Simulate 'nr_samples' samples of length 'N' from the time series
##  corresponding to 'TS_key', and save it into the file-hierarchy.

nr_samples <- 1
N <- dim(EuStockMarkets)[1] - 1 ## = 1859
TS_key <- "dmt_bivariate"

.seed_for_sample <- 124552
set.seed(.seed_for_sample)

##  Generate the sample.

.TS_sample <- TS_sample(
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    first_dmt = list(A = rbind(c(-2, -1, 0, 1),
                               c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                     delta = c(1.0, 0.5, 0.3, 0.5),
                     delta_range = c(0.5, 0.2, 0.2, 0.6),
                     alpha = c(pi/2, pi/8, 4/5 * pi, pi/2) + {
                         set.seed(12)
                         runif(n = 4, min = 0.1, max = 0.2)},
                     theta = NULL,
                     wn = NULL),
    phase_adjustment = c(pi/3, pi/4, 0, pi/2),
    .seed = NULL)
rm(nr_samples, N, .seed_for_sample)

##  Create a unique 'save_dir' and save 'TS_sample' to the
##  file-hierarchy.

save_dir <- paste(TS_key,
                  digest::digest(.TS_sample$TS),
                  sep = "_")

##  Save to file and update file-hierarchy.

.TS_LG_object <- TS_LG_object(
    TS_data = .TS_sample,
    main_dir = main_dir,
    save_dir = save_dir,
    .remove_ties = TRUE)
rm(TS_key, .TS_sample, save_dir, main_dir)

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

.b <- 0.6

.point_L  <- 0.05
.point_U  <- 0.95

##  Note: The '.shape'-argument below must be an integer that is large
##  enough to ensure that the desired heatmap- and distance-plots have
##  a decent resolution.  Moreover: The three points at 0.10, 0.50 and
##  0.90 should also be part of the values computed (since they are to
##  be highlighting in the resulting distance plots).

.shape <- round(1 + (.point_U - .point_L)/0.005)

.LG_type <- "par_five"
.LG_points <- LG_select_points(
    .P1 = c(.point_L, .point_L),
    .P2 = c(.point_U, .point_U),
    .shape = .shape)
rm(.point_L, .point_U, .shape)
lag_max <- 10

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

###----------------------------------------------------------------###

##  Send code to terminal that can be used to start the interactive
##  inspection based on the shiny-application 'LG_shiny'.  It might be
##  of interest to save this to a file so an inspection later on does
##  not require this script.  Note that there are some tests in the
##  code that try to prevent things that have already been computed
##  from being computed once more, but the initial computation of
##  '.TS_sample' will be performed every time this script is used.

LG_shiny_writeLines(
    main_dir = LG_AS$main_dir,
    data_dir = LG_AS$data_dir)

##  Start the shiny application for an interactive inspection of the
##  result.  The use of 'shiny::runApp' is needed in order to start
##  the shiny-application when this script is sourced.

shiny::runApp(LG_shiny(
    main_dir = LG_AS$main_dir,
    data_dir = LG_AS$data_dir))

###----------------------------------------------------------------###
