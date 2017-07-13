#' An investigation of the 'dmbp'-data, length 1974.  This example is
#' used in figures 11-14 of
#' "Nonlinear spectral analysis via the local Gaussian correlation".

##  Reminder: length 1974, b = 1.75 * (1974)^(-1/6) = 0.4940985.  Thus
##  use bandwidths 0.5, 0.75, 1 and see how it fares for the different
##  alternatives.

.b <- c(0.5, 0.75, 1)

##   Number of bootstrap replicates and block length
nb <- 100
block_length <- 100

require(localgaussSpec)
require(rugarch)


data("dmbp")

##  Specify the directory to use (must be an existing directory).

main_dir <- "~/LG_DATA"

##  Specify the adjustment rule to use.
.adjustment_rule <- 0
## .adjustment_rule <- "data"
## .adjustment_rule <- 2^(-1/sqrt(3))


set.seed(136)
.TS <- dmbp[, "V1"]
.TS_object <- TS_LG_object(
    TS_data = .TS,
    main_dir = main_dir,
    .adjustment_rule = .adjustment_rule)

##  KIT
rm(.adjustment_rule, .TS, dmbp)

##  Define some values

lag_max <- 20

omega_length_out <- 2^6
##  Place 2/3 of the points between 0 and 0.1, and the rest can be
##  used in the intervall between 0.1 and 0.5.
omega_vec <- local({
    .first <- floor(2/3 * omega_length_out)
    unique(c(seq(from = 0,
                 to = 0.1,
                 length.out = .first),
             seq(from = 0.1,
                 to = 0.5,
                 length.out = omega_length_out - .first + 1)))
})
omega_length_out <- NULL


.LG_type <- "par_five"

#####  TEST new solution
.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(5, 5))


arg_list_LG_Wrapper_Original <- list(
    main_dir = main_dir,
    data_dir = .TS_object$TS_info$save_dir,
    TS = .TS_object$TS_info$TS,
    lag_max = lag_max,
    LG_points        = .LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = c(30, 50),
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,    
    omega_vec = omega_vec,
    omega_length_out = omega_length_out,
    window = "Tukey",
    LG_type = .LG_type,
    cut_vec = NULL)

##  KIT

rm(.TS_object, lag_max, omega_vec, omega_length_out, .LG_type)


LG_WO <- LG_Wrapper_Original(
    main_dir = arg_list_LG_Wrapper_Original$main_dir,
    data_dir = arg_list_LG_Wrapper_Original$data_dir,
    TS = arg_list_LG_Wrapper_Original$TS,
    lag_max = arg_list_LG_Wrapper_Original$lag_max,
    LG_points = arg_list_LG_Wrapper_Original$LG_points,
    .bws_mixture = arg_list_LG_Wrapper_Original$.bws_mixture,
    bw_points = arg_list_LG_Wrapper_Original$bw_points,
    .bws_fixed = arg_list_LG_Wrapper_Original$.bws_fixed,
    .bws_fixed_only = arg_list_LG_Wrapper_Original$.bws_fixed_only,
    omega_vec = arg_list_LG_Wrapper_Original$omega_vec,
    omega_length_out = arg_list_LG_Wrapper_Original$omega_length_out,
    window = arg_list_LG_Wrapper_Original$window,
    LG_type = arg_list_LG_Wrapper_Original$LG_type,
    cut_vec = arg_list_LG_Wrapper_Original$cut_vec)


##  str(LG_WO)

##  KIT
rm(arg_list_LG_Wrapper_Original)

################################################################################
##  The bootstrap-part, recycle all the arguments specified above.

arg_list_LG_Wrapper_Bootstrap <- list(
    main_dir        = main_dir,
    spectra_dir     = LG_WO$spectra_note$data_dir,
    nb              = nb,
    boot_type       = NULL,
    block_length    = block_length,
    boot_seed       = NULL,
    all_statistics  = FALSE,
    log_            = FALSE,
    lag_max         = NULL,
    LG_points       = NULL,
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

## rm(nb, block_length)

## LG_Wrapper_Bootstrap_call <- create_call(
##     .cc_fun = LG_Wrapper_Bootstrap,
##     arg_list_LG_Wrapper_Bootstrap,
##     .cc_list = TRUE)

## LG_Wrapper_Bootstrap_call <- ToolBox::create_call(
##     .cc_fun = LG_Wrapper_Bootstrap,
##     arg_list_LG_Wrapper_Bootstrap,
##     .cc_list = TRUE)





set.seed(1421236)
LG_WB <- LG_Wrapper_Bootstrap(
    main_dir        = main_dir,
    spectra_dir     = LG_WO$spectra_note$data_dir,
    nb              = nb,
    boot_type       = NULL,
    block_length    = block_length,
    boot_seed       = NULL,
    all_statistics  = FALSE,
    log_            = FALSE,
    lag_max         = NULL,
    LG_points       = NULL,
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


## str(LG_WB)

## KIT
rm(LG_WO, arg_list_LG_Wrapper_Bootstrap, LG_Wrapper_Bootstrap_call)

################################################################################
##  Collect the pieces, and find the argument needed by `LG_shiny`.

data_dir_for_LG_shiny <- LG_collect_orig_and_boot(
    main_dir = main_dir,
    data_dir = LG_WB$boot_spectra_note$data_dir)

##  KIT
rm(LG_WB)


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
## data_dir_for_LG_shiny <-
## structure(c("490635359ea5f932d4a3b7932e7934a6", "Approx__1", 
## "Boot_Approx__1", "Boot_Spectra__1"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))

##  KIT
rm(data_dir_for_LG_shiny)
