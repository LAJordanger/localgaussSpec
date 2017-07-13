#' An apARCH(2,3)-model fitted to the 'dmbp'-data, length 1974 (the
#' same length as the 'dmbp'-example).  The result from the fitted
#' model can be compared with those based on the original data, to get
#' a visual indicator of the suitability of the model (at the points
#' investigated).  This example is used in figures 10, 12-14 of
#' "Nonlinear spectral analysis via the local Gaussian correlation".

##  Reminder: length 1974, b = 1.75 * (1974)^(-1/6) = 0.4940985.  Thus
##  use bandwidths 0.5, 0.75, 1 and see how it fares for the different
##  alternatives.


.b <- c(0.5, 0.75, 1)


main_dir <- "~/LG_DATA"

nr_samples <- 100



require(localgaussSpec)
require(rugarch)
require(stringr)

##  Perhaps make a more direct analysis of this later on...

##  Avoid loading huge file if not necessary.
.loaded <- exists("env_f9563e9eab9aa395422585b15fcf00a4")
if (! .loaded)
    load("~/LG_DATA/rugarh_fitted_models/f9563e9eab9aa395422585b15fcf00a4_dmbp.Rda")

## ls(env_f9563e9eab9aa395422585b15fcf00a4$sub_env_e62ff2b8137d5b1c1a516f87be25859a,
##    all.names = TRUE)
## ##  [1] "arg_grid"               "arg_grid_ARFIMA"        "arg_grid_fGARCH"       
## ##  [4] "arg_grid_fGARCH_ARFIMA" ".coef"                  ".convergence"          
## ##  [7] ".error_report"          ".infocriteria"          ".likelihood"           
## ## [10] "spec"                   "TS_sample_spec"        

##  Create a shortcut to avoid to cumbersome code later on.

.._env <- env_f9563e9eab9aa395422585b15fcf00a4$sub_env_e62ff2b8137d5b1c1a516f87be25859a

###  For the purpose of the example to be used at ISNPS-2016, select
###  an ARFIMA(0,0,0)-model, and it might also be nice to inclue a
###  model using APARCH (I think).

##  A minor intermezzo to restrict to the models having these keys.
.key1 <- "APARCH"
.key2 <- "ARFIMA(0,0,0)"


.match_1 <- str_detect(string = names(.._env$.likelihood),
                       pattern = fixed(.key1))
.match_2 <- str_detect(string = names(.._env$.likelihood),
                       pattern = fixed(.key2))


sum(.match_1)
## [1] 1944
sum(.match_2)
## [1] 1474

.both <- as.logical(.match_1 * .match_2)

sum(.both)
## [1] 108

.targets <- .._env$.likelihood[.both]

##  What to select?  I guess it might be of interst to use one having

.targets[10]
## fGARCH(2,3)_APARCH_ARFIMA(0,0,0)_sstd 
##                             -976.3838 

.model_name <- "fGARCH(2,3)_APARCH_ARFIMA(0,0,0)_sstd"

## ##  Alternatively, use the one with the lowest value
## .model_name <- tail(names(.targets), n = 1)
## ##  Not promising.

## ##  The first one instead?
## .model_name <- head(names(.targets), n = 1)
## ##  Not looking that good either.

##  Extract the specification corresponding to this model

.spec <- .._env$TS_sample_spec[[.model_name]]

##  Set the name of the save dir
##  save_dir <- .model_nam
save_dir <- NULL


################################################################################
##  Time to test this for computations
TS_key <- "rugarch"
N <- 1974
lag_max <- 20
bw_points <- 25
omega_length_out <- 2^6
window <- "Tukey"
.normalisation_rule <- "ecdf"
.adjustment_rule <- 0

.seed_for_sample <- 4624342

.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(5, 5))

set.seed(.seed_for_sample)

##  Perform the standard sequence
arg_list_TS_LG_object <- list(
    TS_data = TS_sample(
        TS_key = TS_key,
        N = N,
        n.start = 100,
        nr_samples = nr_samples,
        spec = .spec,
        .seed = NULL),
    main_dir = main_dir,
    save_dir = save_dir,
    .normalisation_rule = .normalisation_rule,
    .adjustment_rule = .adjustment_rule,
    .remove_ties = TRUE)

##  Fix the saving to file.
tmp_TS_LG_object <- TS_LG_object(
    TS_data = arg_list_TS_LG_object$TS_data,
    main_dir = arg_list_TS_LG_object$main_dir,
    save_dir = arg_list_TS_LG_object$save_dir,
    .normalisation_rule = arg_list_TS_LG_object$.normalisation_rule,
    .adjustment_rule = arg_list_TS_LG_object$.adjustment_rule,
    .remove_ties = arg_list_TS_LG_object$.remove_ties)

##  Set up the arguments for the next step.
arg_list_LG_Wrapper_Blocks <- list(
    main_dir = main_dir,
    data_dir = tmp_TS_LG_object$TS_info$save_dir,
    TS = tmp_TS_LG_object$TS_info$TS,
    lag_max = lag_max,
    LG_points = .LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = bw_points,
    .bws_fixed = .b,
    .bws_fixed_only = TRUE,
    omega_vec = NULL,
    omega_length_out = omega_length_out,
    window = window,
    LG_type = "par_five",
    ## LG_type = c("par_five", "par_one"),
    cut_vec = NULL)
##  Do the main computation.
.tmp_uc_LG_Wrapper_Blocks <- LG_Wrapper_Blocks(
    main_dir = arg_list_LG_Wrapper_Blocks$main_dir,
    data_dir = arg_list_LG_Wrapper_Blocks$data_dir,
    TS = arg_list_LG_Wrapper_Blocks$TS,
    lag_max = arg_list_LG_Wrapper_Blocks$lag_max,
    LG_points = arg_list_LG_Wrapper_Blocks$LG_points,
    .bws_mixture = arg_list_LG_Wrapper_Blocks$.bws_mixture,
    bw_points = arg_list_LG_Wrapper_Blocks$bw_points,
    .bws_fixed = arg_list_LG_Wrapper_Blocks$.bws_fixed,
    .bws_fixed_only = arg_list_LG_Wrapper_Blocks$.bws_fixed_only,
    omega_vec = arg_list_LG_Wrapper_Blocks$omega_vec,
    omega_length_out = arg_list_LG_Wrapper_Blocks$omega_length_out,
    window = arg_list_LG_Wrapper_Blocks$window,
    LG_type = arg_list_LG_Wrapper_Blocks$LG_type,
    cut_vec = arg_list_LG_Wrapper_Blocks$cut_vec)

##  Collect the required pieces
data_dir <- .tmp_uc_LG_Wrapper_Blocks$CI_spectra_note$data_dir
data_dir_for_LG_shiny <- LG_collect_blocks(
    main_dir = main_dir,
    data_dir = data_dir)

##  dump("data_dir_for_LG_shiny", stdout())


##  Start a shiny application for the inspection of the result.
shiny::runApp(LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir_for_LG_shiny))


####   I wonder what's up with this stuff - I'm not at all confident
####   that the value at omega equal to zero won't fizz of to infinity
####   when more lags are added...


##  Reminder: 'runApp' is used in order for the interactive
##  investigation to start when this file is sourced.  It's sufficient
##  to use 'LG_shiny' directly when doing this from the command line.

##  Hint: 'dump' used on 'data_dir_for_LG_shiny' gives the code needed
##  to recreate it, and this might be handy later on if one want to
##  start directly with 'LG_shiny' Se the code below for how this
##  looks like for this example.

##  dump("data_dir_for_LG_shiny", stdout())
## data_dir_for_LG_shiny <-
## structure(c("1251eefebdd1e934f6960bfd9e4a561b", "Approx__1", 
## "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", "approx.dir", 
## "boot.approx.dir", "boot.spectra.dir"))
