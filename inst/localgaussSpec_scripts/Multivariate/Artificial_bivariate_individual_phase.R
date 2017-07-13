#' An investigation for bivariate artificial trigonometric stuff,
#' based on an univariate case that did possess the desired peaks.
#' 

##  Reminder: length 1974, b = 1.75 * (1974)^(-1/6) = 0.4940985.  Thus
##  use bandwidths 0.5, 0.75, 1 and see how it fares for the different
##  alternatives.

.b <- c(0.6, 1)


main_dir <- "~/LG_DATA"
nr_samples <- 100
save_dir <- "dmt_bivariate"

require(localgaussSpec)


################################################################################
##  Time to test this for computations
TS_key <- "dmt_bivariate"
N <- 1974
lag_max <- 20
dl_length_out <- 3
bw_points <- 25
omega_length_out <- 2^6
window <- "Tukey"
.normalisation_rule <- "ecdf"
.adjustment_rule <- 0

.seed_for_sample <- 124552


.LG_type <- "par_five"

#####  TEST new solution
.LG_points <- LG_select_points(
    .P1 = c(0.1, 0.1),
    .P2 = c(0.9, 0.9),
    .shape = c(3, 3))


set.seed(.seed_for_sample)

.TS_sample <- TS_sample(
    TS_key = TS_key,
    N = N,
    nr_samples = nr_samples,
    first_dmt = list(A = rbind(c(-2, -1, 0, 1),   ##  Reminder: Not zero mean, but that's not a problem I think.
                               c(1/20, 1/3 - 1/20, 1/3, 1/3)),
                     delta = c(1.0, 0.5, 0.3, 0.5),
                     delta_range = c(0.5, 0.2, 0.2, 0.6),
                     ##    delta_range = NULL,
                     alpha = c(pi/2, pi/8, 4/5 * pi, pi/2) + {
                         set.seed(12)
                         runif(n = 4, min = 0.1, max = 0.2)},
                     theta = NULL,
                     wn = NULL),
    phase_adjustment = c(pi/3, pi/4, 0, pi/2),
    .seed = NULL)




##   Update `save_dir` based on the sampled time-series

save_dir <- paste(save_dir,
                  digest::digest(.TS_sample$TS),
                  sep = "_")


##  Perform the standard sequence
arg_list_TS_LG_object <- list(
    TS_data = .TS_sample,
    main_dir = main_dir,
    save_dir = save_dir,
    .normalisation_rule = .normalisation_rule,
    .adjustment_rule = .adjustment_rule,
    .remove_ties = TRUE)

rm(.TS_sample)

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
    LG_type = .LG_type,
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
## data_dir_for_LG_shiny <-
## structure(c("dmt_bivariate_255a233b94912a0110688456938902c6", 
## "Approx__1", "Boot_Approx__1", "Boot_Spectra"), .Names = c("ts.dir", 
## "approx.dir", "boot.approx.dir", "boot.spectra.dir"))


##  Start a shiny application for the inspection of the result.
shiny::runApp(LG_shiny(
    main_dir = main_dir,
    data_dir = data_dir_for_LG_shiny))


####   I wonder what's up with this stuff - I'm not at all confident
####   that the value at omega equal to zero won't fizz of to infinity
####   when more lags are added...


