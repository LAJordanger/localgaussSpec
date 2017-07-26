#' One cosine and a bit of white noise, length 1974 (the same length
#' as the 'dmbp'-example).  This example is the basis for figure 6 of
#' "Nonlinear spectral analysis via the local Gaussian correlation".



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
N <- 1974
TS_key <- "dmt"
.seed_for_sample <- 12435
set.seed(.seed_for_sample)
##  Generate the sample.  (See the help page for the given key for
##  details about the arguments.)
.TS_sample <- TS_sample(
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    A = rbind(c(0),
              c(1)),
    delta = c(1.0),
    delta_range = c(1),
    alpha = c(2/5 * pi + 0.64),
    theta = NULL,
    wn = list(type = "rnorm",
              args = list(mean = 0,
                          sd = .75),
              adjust = 0),
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
##  Compute the local Gaussian spectral densities.  This requires
##  first that the local Gaussian correlations of interest must be
##  computed, which implies that the points of interest must be
##  selected together with information about the bandwidth and the
##  number of lags. WARNING: The type of approximation must also be
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
lag_max <- 20
##  Reminder: length 1974, b = 1.75 * (1974)^(-1/6) = 0.4940985.  Thus
##  use bandwidths 0.5, 0.75, 1 and see how it fares for the different
##  alternatives.  
.b <- c(0.5, 0.75, 1)


##  Some input parameters that gives the frequencies to be
##  investigated and the smoothing to be applied when the estimates of
##  the local Gaussian spectra are computed.

omega_length_out <- 2^6
window <- "Tukey"

##  Do the main computation.  
.tmp_uc_LG_Wrapper_Blocks <- LG_Wrapper_Blocks(
    main_dir = main_dir,
    data_dir = tmp_TS_LG_object$TS_info$save_dir,
    TS = tmp_TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,
    omega_length_out = omega_length_out,
    window = window,
    LG_type = .LG_type)
rm(tmp_TS_LG_object, lag_max, .LG_points, .b, omega_length_out, window, .LG_type)
###############

##############################

###############
##  Collect the required pieces

data_dir_for_LG_shiny <- LG_collect_blocks(
    main_dir = main_dir,
    data_dir = .tmp_uc_LG_Wrapper_Blocks$CI_spectra_note$data_dir)
rm(.tmp_uc_LG_Wrapper_Blocks)

##  And start the shiny application for an interactive inspection of
##  the result.

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


##  dump("data_dir_for_LG_shiny", stdout())
## data_dir_for_LG_shiny <-
## structure(c("dmt_1e848c18302705844bb4c1bb75982c18", "Approx__1", 
## "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))

#####
## Note that 'data_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.
################################################################################
