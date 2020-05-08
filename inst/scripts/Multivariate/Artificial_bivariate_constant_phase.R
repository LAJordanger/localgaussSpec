#' An investigation for bivariate artificial trigonometric stuff,
#' based on an univariate case that did possess the desired peaks.

###############
##  NOTE: This script is a part of the package 'localgaussSpec'.  Its
##  purpose is to provide the code needed for the reproduction of one
##  or more of the examples used in the two papers "Nonlinear spectral
##  analysis via the local Gaussian correlation" and "Nonlinear
##  cross-spectrum analysis via the local Gaussian correlation".  The
##  present setup contains a wider range of input parameters than
##  those used in the figures of the previous mentioned papers, which
##  enables the interested reader to see how the result varies based
##  on these input parameters.
#####
##  WARNING: The user that want to adjust this script in order to
##  investigate other time series (or a wider range of input
##  parameters for the present example) should keep in mind that the
##  selected input parameters must take into account the length of the
##  investigated example. In particular, see the before mentioned
##  papers for a discussion of how small-sample variation might occur
##  even for long samples if the coordinates of the point of interest
##  correspond to low or high quantiles.
###############

##############################

###############
## Check that the required package(s) are available.
.required_packages <- c("localgaussSpec")
.successful <- vapply(
    X = .required_packages,
    FUN = requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1))
if (! all(.successful)) {
    stop(sprintf(
        fmt = "\nThe following package%s must be installed for this script to work: %s.",
        ifelse(test = sum(!.successful) > 1,
               yes  = "s",
               no   = ""),
        paste(paste("\n\t",
                    names(.successful[!.successful]),
                    sep = ""),
              collapse = ", ")))
}
rm(.required_packages, .successful)
###############

##############################

###############
##  Add comment(s) related to the need for a parallel-backend suitable
##  for the operative system.
###############

###############
##  Specify the directory in which the resulting file-hierarchy will
##  be stored. The default directory "LG_DATA" will be created if it
##  does not exist, whereas other storage-alternatives must be created
##  manually before this script is called.

main_dir <- "~/LG_DATA"
###############

##############################

###############
##  Simulate 'nr_samples' samples of length 'N' from the time series
##  corresponding to 'TS_key', and save it into the file-hierarchy. (Contact the
##  package-maintainer if additional models are of interest to
##  investigate.)

nr_samples <- 100
N <- dim(EuStockMarkets)[1] - 1 ## = 1859
TS_key <- "dmt_bivariate"
.seed_for_sample <- 4624342
set.seed(.seed_for_sample)
##  Generate the sample.  (See the help page for the given key for
##  details about the arguments.)
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
    phase_adjustment = pi/3,
    .seed = NULL)
rm(nr_samples, N, .seed_for_sample)
##  Create a unique 'save_dir' and save 'TS_sample' to the
##  file-hierarchy.  (Note: )
save_dir <- paste(TS_key,
                  digest::digest(.TS_sample$TS),
                  sep = "_")
##  Save to file and update file-hierarchy.
tmp_TS_LG_object <- TS_LG_object(
    TS_data = .TS_sample,
    main_dir = main_dir,
    save_dir = save_dir,
    .remove_ties = TRUE)
rm(TS_key, .TS_sample, save_dir)
###############

##############################

###############
##  Compute the local Gaussian correlations.  This requires a
##  specification of the desired points, the bandwidth and the number
##  of lags. WARNING: The type of approximation must also be
##  specified, i.e. the argument 'LG_type', where the options are
##  "par_five" and "par_one".  The "five" and "one" refers to the
##  number of free parameters used in the approximating bivariate
##  local Gaussian density.  The results should be equally good for
##  Gaussian time series, but the "par_one" option will in general
##  produce dubious/useless results.  Only use "par_one" if it is of
##  interest to compare the result with "par_five", otherwise avoid it
##  as it most likely will be a waste of computational resources.

.LG_type <- "par_five"
.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(3, 3))
lag_max <- 15
##  Reminder: length 1859, b = 1.75 * (1859)^(-1/6) = 0.4990662.  This
##  indicates that a bandwidth of '0.5' should be used.  For the
##  univariate case the three bandwidths 0.5, 0.75, 1 was
##  investigated, but due to the increased number of computations
##  needed for the multivariate case, only one bandwidth will be
##  considered here. The value '0.6' has been selected based on the
##  impression that '0.5' might not be appropriate to use for the
##  points having coefficients in the tails of the margins.
.b <- 0.6
        
##  Do the main computation.  
.tmp_LG_approx_scribe <- LG_approx_scribe(
    main_dir = main_dir,
    data_dir = tmp_TS_LG_object$TS_info$save_dir,
    TS = tmp_TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,
    LG_type = .LG_type)
rm(tmp_TS_LG_object, lag_max, .LG_points, .b, .LG_type)
###############

##  Extract the directory information needed for 'LG_shiny'.
data_dir_for_LG_shiny <- .tmp_LG_approx_scribe$data_dir
rm(.tmp_LG_approx_scribe)

##  Start the shiny application for an interactive inspection of the
##  result.

shiny::runApp(LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir_for_LG_shiny))

################################################################################
###### NOTE:
###  The interaction with the file-hierarchy contains tests that
###  prevents previously computed local Gaussian correlations to be
###  computed all over again if this script is sourced a second time,
###  but the initial computation of '.TS_sample' will be performed
###  every time the script is sourced.  It can thus be of interest to
###  note that the 'dump'-function might be used to capture the
###  'data_dir'-argument, such that the call to the shiny-application
###  can be done without the need for the script to be sourced
###  directly.  The result for the present script (based on the
###  original input parameters) are given below.

## dump("data_dir_for_LG_shiny", stdout())
## data_dir_for_LG_shiny <-
##     c(ts.dir = "dmt_bivariate_6a6f6d4cbc41e96083d61d26469b7ac6", 
##       approx.dir = "Approx__1")

#####
## Note that 'data_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.
################################################################################
