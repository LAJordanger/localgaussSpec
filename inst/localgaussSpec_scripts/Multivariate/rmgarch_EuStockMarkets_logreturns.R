#' rmgarch-test based on 'EUStockMarkets' 1860 observations, DAX, SMI,
#' CAC, FTSE, compound returns.  NOTE: This example is solely included
#' in order to show how the computations can be done.  In particular,
#' the model used in this script is the simplest one that is
#' available, i.e. no attempt has been made in order to search for a
#' multivariate GARCH-type model that gives the best fit to the data.

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
.required_packages <- c("localgaussSpec", "rmgarch")
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
##  Specify the model to be used based on the log-returns of the
##  'EuStockMarkets'-data.  NOTE: This approach use the simplest
##  available model, as the purpose of this script is solely to
##  present the setup for the computation.

###############
##  Compute the daily log-returns to be used as 'data' in the
##  computation.

.first <- head(EuStockMarkets, n = -1)
.second <- tail(EuStockMarkets, n = -1)
data <- log(.second/.first)
rm(.first, .second)

##  Specify the model (using the default marginals).
uspec <- rugarch::ugarchspec(
                      mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "norm")
uspec <- rugarch::multispec(replicate(n = dim(data)[2],
                                      expr = uspec))
###############

##############################

###############
##  Simulate 'nr_samples' samples of length 'N' from the time series
##  corresponding to 'TS_key', and save it into the file-hierarchy. (Contact the
##  package-maintainer if additional models are of interest to
##  investigate.)

nr_samples <- 100
N <- dim(data)[1] ## = 1859
TS_key <- "sample_rmgarch"


.seed_for_sample <- 245
set.seed(.seed_for_sample)
##  Generate the sample.  (See the help page for the given key for
##  details about the arguments.)
.TS_sample <- TS_sample(
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    uspec = uspec,
    data = data,
    rseed = NULL)
rm(nr_samples, N, .seed_for_sample, uspec, data)
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
## structure(c("sample_rmgarch_2da91e64ccc3a843cde07f56a91733a0", 
## "Approx__1", "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", 
## "approx.dir", "boot.approx.dir", "boot.spectra.dir"))


#####
## Note that 'data_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.
################################################################################


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


require(localgaussSpec)

##  Specify the directory to use (must be an existing directory).

main_dir <- "~/LG_DATA"

##  Specify the adjustment rule to use.
.adjustment_rule <- 0
## .adjustment_rule <- "data"
## .adjustment_rule <- 2^(-1/sqrt(3))

##  Specify the normalisation rule to use.
.normalisation_rule <- "ecdf"


###-------------------------------------------------------------------
##  Get hold of the time series of interest,
library(MTS)
data("mts-examples")  ## This gives access to 'ibmspko'.

## Daily Closing Prices of Major European Stock Indices, 1991-1998
## Description:
##      Contains the daily closing prices of major European stock indices:
##      Germany DAX (Ibis), Switzerland SMI, France CAC, and UK FTSE.  The
##      data are sampled in business time, i.e., weekends and holidays are
##      omitted.
## Usage:
##      EuStockMarkets
## Format:
##      A multivariate time series with 1860 observations on 4 variables.
##      The object is of class ‘"mts"’.
## Source:
##      The data were kindly provided by Erste Bank AG, Vienna, Austria.
##  Create an array (without the 'date') from ibmspko


##  Compute the daily log-returns to be used in the actual computation.

## head(EuStockMarkets)
## head(tail(EuStockMarkets, n = -1))
## tail(EuStockMarkets)
## tail(head(EuStockMarkets, n = -1))

.first <- head(EuStockMarkets, n = -1)
.second <- tail(EuStockMarkets, n = -1)

## https://quantivity.wordpress.com/2011/02/21/why-log-returns/
## https://www.youtube.com/watch?v=PtoUlt3V0CI

##  Should linear returns $V_{t+1}/V_{t} - 1$ or compound returns (aka
##  log-returns) $ln(V_{t+1}/V_{t})$ be used in the analysis.

.TS_linear <- .second/.first - 1
.TS_compound <- log(.second/.first)

##  And would the difference be of any interest...
.TS_diff <- .TS_compound - .TS_linear

## plot(as.ts(.TS_linear))
## plot(as.ts(.TS_compound))
## plot(as.ts(.TS_diff))



##  For this test, consider the log-return
.TS <- .TS_compound


########################################
##  Use the code based on 'rmgarch.test3.R' to get a simulation object
##  of the desired format based on '.TS' as given above.

TS_key <- "sample_rmgarch"

##  The code below gives the (present) default in the function, but
##  better include it here to get it precise.

data <- .TS

uspec <- rugarch::ugarchspec(
                      mean.model = list(armaOrder = c(0,0)),
                      variance.model = list(garchOrder = c(1,1), model = "sGARCH"), 
                      distribution.model = "norm")
uspec <- rugarch::multispec(replicate(n = min(dim(data)),
                                      expr = uspec))


##  Create the samled object to be used later on.

n <- max(dim(data))
nr_samples <- 100


###-------------------------------------------------------------------

set.seed(245)
.TS_data <- TS_sample(
    TS_key = TS_key,
    N = n,
    nr_samples = nr_samples,
    uspec = uspec,
    data = data,
    rseed = NULL)


################################################################################
##  Initiate/update the file hierarchy

arg_list_TS_LG_object <- list(
    TS_data = .TS_data,
    main_dir = main_dir,
    save_dir = NULL,
    .normalisation_rule = .normalisation_rule,
    .adjustment_rule = .adjustment_rule,
    .remove_ties = TRUE)

TS_LG_object_call <- ToolBox::create_call(
   .cc_fun = TS_LG_object,
    arg_list_TS_LG_object,
    .cc_list = TRUE)

set.seed(635)
.TS_LG_object <- eval(TS_LG_object_call)

##  str(.TS_LG_object)

## ##  KIT
## rm(.TS_data, .adjustment_rule, .normalisation_rule,
##    arg_list_TS_LG_object, TS_LG_object_call)


################################################################################
##  Define the level-points to investigate.

.LG_points <- LG_select_points(
    .P1 = c(.10, .10),
    .P2 = c(.90, .90),
    .shape = c(3, 3))

################################################################################
##  Use 'LG_Wrapper_Blocks' on the data initiatied by the previous step.

.b <- c(0.6, 1)
lag_max <- 20
bw_points <- 25
omega_length_out <- 2^6
window <- "Tukey"


.LG_type <- "par_five"



arg_list_LG_Wrapper_Blocks <- list(
    main_dir = main_dir,
    data_dir = .TS_LG_object$TS_info$save_dir,
    TS = .TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = bw_points,
    .bws_fixed = .b, 
    .bws_fixed_only = TRUE,
    omega_vec = NULL,
    omega_length_out = omega_length_out,
    window = window,
    LG_type = .LG_type,
    cut_vec = NULL)

LG_Wrapper_Blocks_call <- ToolBox::create_call(
    .cc_fun = LG_Wrapper_Blocks,
    arg_list_LG_Wrapper_Blocks,
    .cc_list = TRUE)


.LG_Wrapper_Blocks <- eval(LG_Wrapper_Blocks_call)

## ## KIT
## rm(.TS_LG_object, lag_max, bw_points, .bws_fixed_only,
##    omega_length_out, window, arg_list_LG_Wrapper_Blocks,
##    LG_Wrapper_Blocks_call)


################################################################################
##  Collect the pieces.

## LG_collect_blocks_call <- create_call(
##     .cc_fun = LG_collect_blocks,
##     main_dir = main_dir,
##     data_dir = .LG_Wrapper_Blocks$CI_spectra_note$data_dir)
## capture_env()

## ## ## ## LG_merge_files_scribe_call <- create_call(
## ## ## ##     .cc_fun = LG_merge_files_scribe,
## ## ## ##     main_dir = main_dir,
## ## ## ##     data_dir = .LG_Wrapper_Blocks$CI_spectra_note$data_dir)
## ## ## ## capture_env() 


## ## ## ## data_dir_for_LG_shiny <- LG_merge_files_scribe(
## ## ## ##     main_dir = main_dir,
## ## ## ##     data_dir = .LG_Wrapper_Blocks$CI_spectra_note$data_dir)


data_dir_for_LG_shiny <- LG_collect_blocks(
    main_dir = main_dir,
    data_dir = .LG_Wrapper_Blocks$CI_spectra_note$data_dir)

## ##  KIT
## rm(.LG_Wrapper_Blocks)

################################################################################
##  Inspect the result:

## LG_shiny_call <- call(
##     name  = "LG_shiny",
##     main_dir = main_dir,
##     data_dir = data_dir_for_LG_shiny)
## capture_env()

shiny::runApp(LG_shiny(
    main_dir =  main_dir,
    data_dir = data_dir_for_LG_shiny))


##  Reminder: 'runApp' is used in order for the interactive
##  investigation to start when this file is sourced.  It's sufficient
##  to use 'LG_shiny' directly when doing this from the command line.

##  Hint: 'dump' used on 'data_dir_for_LG_shiny' gives the code needed
##  to recreate it, and this might be handy later on if one want to
##  start directly with 'LG_shiny' Se the code below for how this
##  looks like for this example.

##  dump("data_dir_for_LG_shiny", stdout())
## data_dir_for_LG_shiny <-
## structure(c("afe4f542df76820a0d69980e1bd01da8", "Approx__1", 
## "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))


## ##  KIT
## rm(data_dir_for_LG_shiny)

