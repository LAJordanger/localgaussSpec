###----------------------------------------------------------------###
##  The apARCH(2,3)-example from P1_fig_11.

##  Load the required libraries.

library(localgaussSpec)
library(rugarch)

###----------------------------------------------------------------###

##  Specify the directory where the file-hierarchy will be stored.

main_dir <- c("~", "LG_DATA_scripts", "P1_fig_09.11.F1")

## Note that 'main_dir' only contains the specification of the
## in-hierarchy part of the required path, and this path is stored as
## a vector.  The reason for this is that it should be possible to
## move the storage directory 'main_dir' to another location on your
## computer, or even move it to a computer using another OS than the
## one used for the original computation.

###----------------------------------------------------------------###

##  Specify the model to be used based on the 'dmbp'-data.  First
##  specify the model '.spec' without any parameters, then fit it to
##  'dmbp', and finally import the fitted coeffisients back into the
##  specification '.spec'

data("dmbp")
##  Specify the model:
.spec <- rugarch::ugarchspec(
    variance.model =
        list(model = "fGARCH",
             garchOrder = c(2, 3),
             submodel = "APARCH",
             external.regressors = NULL,
             variance.targeting = FALSE),
    mean.model =
        list(armaOrder = c(0, 0),
             include.mean = TRUE,
             archm = FALSE,
             archpow = 1,
             arfima = FALSE,
             external.regressors = NULL,
             archex = FALSE),
    distribution.model = "sstd",
    start.pars = list(),
    fixed.pars = list())
##  Fit the model to the observations:
.fitted <- rugarch::ugarchfit(
    spec =  .spec,
    data = dmbp[, "V1"],
    solver = "hybrid")

##  Update the coefficients:
rugarch::setfixed(.spec) <- rugarch::coef(.fitted)
rm(.fitted, dmbp)

##  NOTE: The present code shows how one particular GARCH-type model
##  can be fitted to the 'dmbp'-data.  The original investigation
##  included code that tested a wide variety of alternatives, before
##  the present apARCH(2,3)-model in the end was selected as the one
##  of interest to use in paper P1.

###----------------------------------------------------------------###

##  Simulate the data to be investigated.

##  Simulate 'nr_samples' samples of length 'N' from the time series
##  corresponding to 'TS_key', and save it into the file-hierarchy.

nr_samples <- 100
N <- 1974
TS_key <- "rugarch"

.seed_for_sample <- 4624342
set.seed(.seed_for_sample)

##  Generate the sample.

.TS_sample <- TS_sample(
    TS_key = TS_key,
    N = N, 
    n.start = 100,
    nr_samples = nr_samples,
    spec = .spec,
    .seed = NULL)
rm(nr_samples, N, .seed_for_sample, .spec)

##  Create a unique 'save_dir' and save 'TS_sample' to the
##  file-hierarchy.  (Note: )

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

.LG_type <- c("par_five", "par_one")
.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(3, 3))
lag_max <- 20

.b <- c(0.5, 0.75, 1)

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
