#' Spurious correlations, Simulations from a multivariate normal
#' distribution, scaled with a common sample, off-diagonal
#' points. Mainly intended to see if the result looks like it ought to
#' do, or if it still might be details that should be fixed.
#' Bivariate samples of length 1000, 50 replications from model with
#' rho = 0.35.

require(localgaussSpec)

##  Specify the directory to use (must be an existing directory).

main_dir <- "~/LG_DATA"


.rho <- 0.35
.mean <- c(0,0)
sigma <- matrix(data = c(1, .rho, .rho, 1),
                   nrow = 2)
rm(.rho)

## A small sample based on 'rmvnorm'.
TS_key <- "rmvnorm"

N <- dim(EuStockMarkets)[1] - 1
nr_samples <- 100
.b <- 0.6

lag_max <- 15
bw_points <- 25
omega_length_out <- 2^5
window <- "Tukey"
.normalisation_rule <- "ecdf"
.adjustment_rule <- 0
.bws_fixed_only <- TRUE



################################################################################
##  Create the sample.

set.seed(245)
.TS_data <- TS_sample(
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    mean = .mean,
    sigma = sigma,
    .seed = NULL)



################################################################################


## ## ## ##  Add hoc solution for the creation of the spurrious correlations.

## ## ## set.seed(124534)
## ## ## .TS_divisor <- TS_sample(
## ## ##     TS_key = "WNG",
## ## ##     N = N,
## ## ##     nr_samples = nr_samples,
## ## ##     .seed = NULL)

## ## ## ##  Create the scaled version that gives the spurious correlations, by
## ## ## ##  using an intermediate step where the divisor is adjusted to get
## ## ## ##  the desired format needed for 'multiply_arrays' to work.

## ## ## .divisor <- structure(
## ## ##     .Data = .TS_divisor$TS,
## ## ##     .Dim = dim(.TS_divisor$TS)[1:2],
## ## ##     .Dimnames = dimnames(.TS_divisor$TS)[1:2],
## ## ##     class = class(.TS_divisor$TS))

## ## ## ##  Update .TS_data$TS to get the spurrious correlations.

## ## ## .TS_data$TS <- multiply_arrays(
## ## ##     .arr1 = .TS_data$TS,
## ## ##     .arr2 = 1 / .divisor)


## ## ## ##  A minor test

## ## ## ## .test <- restrict_array(
## ## ## ##     .arr = .TS_data$TS,
## ## ## ##     .restrict = list(content = "orig3"),
## ## ## ##     .drop = TRUE)

## ## ## ## plot(.test)


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

##  KIT
rm(.TS_data, .adjustment_rule, .normalisation_rule,
   arg_list_TS_LG_object, TS_LG_object_call)


################################################################################
##  Define the level-points to investigate.

.LG_points <- LG_select_points(
    .P1 = c(.10, .10),
    .P2 = c(.90, .90),
    .shape = c(5, 5))

################################################################################
##  Use 'LG_Wrapper_Blocks' on the data initiatied by the previous step.

arg_list_LG_Wrapper_Blocks <- list(
    main_dir = main_dir,
    data_dir = .TS_LG_object$TS_info$save_dir,
    TS = .TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = bw_points,
    .bws_fixed = .b, 
    .bws_fixed_only = .bws_fixed_only,
    omega_vec = NULL,
    omega_length_out = omega_length_out,
    window = window,
    LG_type = c("par_five", "par_one"),
    cut_vec = NULL)

LG_Wrapper_Blocks_call <- ToolBox::create_call(
    .cc_fun = LG_Wrapper_Blocks,
    arg_list_LG_Wrapper_Blocks,
    .cc_list = TRUE)


.LG_Wrapper_Blocks <- eval(LG_Wrapper_Blocks_call)

## KIT
rm(.TS_LG_object, lag_max, bw_points, .bws_fixed_only,
   omega_length_out, window, arg_list_LG_Wrapper_Blocks,
   LG_Wrapper_Blocks_call)


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

##  KIT
rm(.LG_Wrapper_Blocks)

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
## structure(c("84272391a5b1af8b4a231f3d755ae557", "Approx__1", 
## "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))



## ##  KIT
## rm(data_dir_for_LG_shiny)
