#' rmgarch-test based on 'EUStockMarkets' 1860 observations, DAX, SMI,
#' CAC, FTSE, compound returns.  ('ibmspko' from the 'MTS'-package.)

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

