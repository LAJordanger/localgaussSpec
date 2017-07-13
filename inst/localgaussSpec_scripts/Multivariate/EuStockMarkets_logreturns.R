#' Computations based on 'EUStockMarkets' 1860 observations, DAX, SMI,
#' CAC, FTSE, compound returns.  ('ibmspko' from the 'MTS'-package.)
#' NB: Only points on the diagonal!

require(localgaussSpec)

##  Specify the directory to use (must be an existing directory).

main_dir <- "~/LG_DATA"

##  Specify the adjustment rule to use.
.adjustment_rule <- 0
## .adjustment_rule <- "data"
## .adjustment_rule <- 2^(-1/sqrt(3))

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

###-------------------------------------------------------------------


.TS_object <- TS_LG_object(
    TS_data = .TS,
    main_dir = main_dir,
    .adjustment_rule = .adjustment_rule)

## str(.TS_object)

##  KIT
rm(.adjustment_rule, .TS)

.LG_points <- LG_select_points(
    .P1 = 0.1,
    .P2 = 0.9,
    .shape = c(3, 3))


arg_list_LG_Wrapper_Original <- list(
    main_dir         = main_dir,
    data_dir         = .TS_object$TS_info$save_dir,
    TS               = .TS_object$TS_info$TS,
    lag_max          = 25,
    LG_points        = .LG_points,
    .bws_mixture     = c("mixture", "local", "global"),
    bw_points        = c(25, 35),
    .bws_fixed       = c(0.6, 1),
    .bws_fixed_only  = TRUE,
    omega_vec        = NULL,
    omega_length_out = 2^7+1,
    window           = "Tukey",
    ## LG_type          = c("par_five", "par_one"),
    LG_type          = "par_five",
    cut_vec          = NULL)


LG_Wrapper_Original_call <- ToolBox::create_call(
    .cc_fun = LG_Wrapper_Original,
    arg_list_LG_Wrapper_Original,
    .cc_list = TRUE)

LG_WO <- eval(LG_Wrapper_Original_call)

##  str(LG_WO)

##  KIT
rm(.TS_object, arg_list_LG_Wrapper_Original, LG_Wrapper_Original_call)

################################################################################
##  The bootstrap-part, recycle all the arguments specified above.

arg_list_LG_Wrapper_Bootstrap <- list(
    main_dir        = main_dir,
    spectra_dir     = LG_WO$spectra_note$data_dir,
    nb              = 100,
    boot_type       = NULL,
    block_length    = 100,
    boot_seed       = NULL,
    all_statistics  = FALSE,
    log_            = FALSE,
    dl_vec          = NULL,
    lag_max         = NULL,
    .bws_mixture    = NULL,
    bw_points       = NULL,
    .bws_fixed      = NULL,
    .bws_fixed_only = NULL,
    content_details = NULL,
    LG_type         = NULL,
    omega_vec       = NULL,
    window          = NULL,
    cut_vec         = NULL,
    threshold       = 100)


LG_Wrapper_Bootstrap_call <- ToolBox::create_call(
    .cc_fun = LG_Wrapper_Bootstrap,
    arg_list_LG_Wrapper_Bootstrap,
    .cc_list = TRUE)


set.seed(141326)
LG_WB <- eval(LG_Wrapper_Bootstrap_call)

## str(LG_WB)

## KIT
rm(LG_WO, arg_list_LG_Wrapper_Bootstrap, LG_Wrapper_Bootstrap_call)

################################################################################
##  Collect the pieces.


## LG_collect_orig_and_boot_call <- create_call(
##     .cc_fun = LG_collect_orig_and_boot,
##     main_dir = main_dir,
##     data_dir = LG_WB$boot_spectra_note$data_dir)

## capture_env() 

## ## ## ## LG_merge_files_scribe_call <- create_call(
## ## ## ##     .cc_fun = LG_merge_files_scribe,
## ## ## ##     main_dir = main_dir,
## ## ## ##     data_dir = LG_WB$boot_spectra_note$data_dir)
## ## ## ## capture_env() 

## ## ## ## LG_merge_files_scribe(
## ## ## ##     main_dir = main_dir,
## ## ## ##     data_dir = LG_WB$boot_spectra_note$data_dir)


LG_collect_orig_and_boot(
    main_dir = main_dir,
    data_dir = LG_WB$boot_spectra_note$data_dir)


data_dir_for_LG_shiny <- LG_WB$boot_spectra_note$data_dir
##  KIT
rm(LG_WB)


################################################################################
##  Inspect the result:

## LG_shiny_call <- create_call(
##     .cc_fun = LG_shiny,
##     main_dir = main_dir,
##     data_dir = data_dir_for_LG_shiny)
## capture_env() 

shiny::runApp(LG_shiny(
    main_dir = main_dir,
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
## structure(c("14a56d251e5eceb427c4118789644916", "Approx__6", 
## "Boot_Approx__1", "Boot_Spectra__1"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))


## ##  KIT
## rm(data_dir_for_LG_shiny)
