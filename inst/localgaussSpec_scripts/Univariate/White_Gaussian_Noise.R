#' A simple reference example based on white Gaussian noise.  This
#' version using 1974 observations.  Mimicking the `dmbp` length.

requireNamespace("localgaussSpec")

##  Specify the directory to use (must be an existing directory).

main_dir <- "~/LG_DATA/Simulated/WGN"
nr_samples <- 100


##  Reminder: length 1974, b = 1.75 * (1974)^(-1/6) = 0.4940985.  Thus
##  use bandwidths 0.5, 0.75, 1 and see how it fares for the different
##  alternatives.

.b <- c(0.5, 0.75, 1)


##  HERE

TS_key <- "WNG"
N <- 1974
lag_max <- 20
bw_points <- 25
omega_length_out <- 2^6
window <- "Tukey"
.normalisation_rule <- "ecdf"
.adjustment_rule <- 0
.bws_fixed_only <- TRUE



.LG_type <- "par_five"

#####  TEST new solution
.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(3, 3))


################################################################################
##  Create the sample.


set.seed(124)
TS_sample_call <- leanRcoding::create_call(
    .cc_fun = TS_sample,
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    .seed = NULL)


.TS_data <- eval(TS_sample_call)

rm(TS_key, N, nr_samples, TS_sample_call)



################################################################################
##  Initiate/update the file hierarchy

arg_list_TS_LG_object <- list(
    TS_data = .TS_data,
    main_dir = main_dir,
    save_dir = NULL,
    .normalisation_rule = .normalisation_rule,
    .adjustment_rule = .adjustment_rule,
    .remove_ties = TRUE)

TS_LG_object_call <- leanRcoding::create_call(
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
    LG_type = .LG_type,
    cut_vec = NULL)

LG_Wrapper_Blocks_call <- leanRcoding::create_call(
    .cc_fun = LG_Wrapper_Blocks,
    arg_list_LG_Wrapper_Blocks,
    .cc_list = TRUE)


.LG_Wrapper_Blocks <- eval(LG_Wrapper_Blocks_call)


## KIT
rm(.TS_LG_object, lag_max, dl_vec, dl_length_out, bw_points,
   .bws_fixed_only, omega_length_out, window,
   arg_list_LG_Wrapper_Blocks, LG_Wrapper_Blocks_call)


################################################################################
##  Collect the pieces.

data_dir_for_LG_shiny <- LG_collect_blocks(
    main_dir = main_dir,
    data_dir = .LG_Wrapper_Blocks$CI_spectra_note$data_dir)

##  KIT
rm(.LG_Wrapper_Blocks)

################################################################################
##  Inspect the result:



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

##  KIT
rm(data_dir_for_LG_shiny)
