#' Approximations of the local Gaussian auto- and cross-correlations
#'
#' @description This function approximates the local Gaussian auto-
#'     and cross-correlations for the given combination of tuning
#'     parameters.
#'
#' @template save_dir_arg
#' @template TS_arg
#' @template lag_max_arg
#' @template LG_points_arg
#' @template bws_mixture_arg
#' @template bw_points_arg
#' @template bws_fixed_arg
#' @template bws_fixed_only_arg
#' @template content_details_arg
#' @template LG_type_arg
#'
#' @note It is not intended that this function should be called
#'     directly.  The idea is that the function
#'     \code{LG_approx_scribe} should be called, which then (after
#'     some sanity testing of the arguments) will call this function
#'     only when it is necessary, i.e. it will keep track of the
#'     bookkeeping and prevent previously computed results from being
#'     recomputed.
#'
#' @return This function will return a list with nodes as described
#'     below:
#' 
#' \describe{
#'
#' \item{LG_type}{The value of the argument \code{LG_type}.  This is
#'     included in order to simplify the code later on.}
#'
#' \item{.bws_mixture}{The value of the argument \code{.bws_mixture}.
#'     This is included in order to simplify the code later on.}
#'
#' \item{content_details}{The value of the argument
#'     \code{content_details}.  This is included in order to simplify
#'     the code later on.}
#'
#' \item{par_one_data}{This will be null if \code{LG_type} does not
#'     contain "par_one", otherwise it will contain information about
#'     the Local Gaussian approximation based on the one free
#'     parameter approach.  This can contain \code{NA}-values if some
#'     numerical convergence failed.}
#'
#' \item{par_five_data}{This will be null if \code{LG_type} does not
#'     contain "par_five", otherwise it will contain information about
#'     the Local Gaussian approximation based on the five free
#'     parameters approach.  This object contains an attribute
#'     \code{convergence} that reveals if the numerical convergence of
#'     the function \code{localgauss} was successful.}
#'
#' \item{level_points}{The attributes that the
#'     \code{LG_extend_points}-function added when it was given the
#'     \code{LG_points}-argument.}
#'
#' }
#'
#' @export

LG_approx <- function(
    save_dir = NULL,
    TS,
    lag_max = ceiling(3*sqrt(length(TS))),
    LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = c(25, 35),
    .bws_fixed = NULL,
    .bws_fixed_only = FALSE,
    content_details = c("rho_only", "rho_log.fun", "rho_all"),
    LG_type = c("par_five", "par_one")) {
    ##  Create a spy-report.
    spy_report <- spy()
    kill(save_dir, bw_points)
    ##  If necessary, restrict 'content_details' to one value.
    content_details <- content_details[1]
    ##  If 'TS' is a 'list', update 'TS' from disk.
    if (is.list(TS)) {
        TS_load(.TS_info = TS,
                save_dir = FALSE)
        spy_report$envir$TS <- TS
    }
    ##  If 'TS' originates from a 'TS_LG_object', it should have an
    ##  attribute 'TS_for_analysis' that should be used instead of TS.
    if (! identical(x = attributes(TS)$TS_for_analysis,
                    y = NULL)) {
        TS <- attributes(TS)$TS_for_analysis
        spy_report$envir$TS <- TS
    }
    ##  Add an attribute to TS that can be used to identify if the
    ##  investigation should be based on the circular index-based
    ##  block bootstrap for tuples, since that case must be treated
    ##  differently later on in the code.
    attributes(TS)$cibbb_case <- isTRUE(attributes(TS)$boot_type == "cibbb_tuples")
    ##  Extract relevant attributes from 'TS'.
    .attr_from_TS <- local({
        .ignore <- c("dim", "dimnames", "TS_for_analysis")
        .keep <- ! names(attributes(TS)) %in% .ignore
        attributes(TS)[.keep]
    })
    ##  Create the full set of points to be investigated, i.e. add the
    ##  diagonally related points when required.
    .level_points <- LG_extend_points(LG_points = LG_points)
    ##  Create helper functions to use with argument grids.
    par_one_helper <- function(vec, TS, .arr, .points,
                               content_details,
                               fixed_bws = FALSE) {
        ##  Extract the two bandwidths "bi" and "bj".
        bws <- if (fixed_bws) {
            c(bi = vec[["bw_points"]],
              bj = vec[["bw_points"]])
        } else
            restrict_array(
                .arr = .arr,
                .restrict = vec,
                .drop = TRUE)
        ##  Find numerical values of lag and level-point.
        lag <- as.integer(vec$lag)
        level_point <- .points[vec$levels, ]
        ##  Split 'vec$pairs' for subsetting
        .pairs <- strsplit(x = vec$pairs, split = "_")[[1]]
        ##  Get hold of the desired data, strategy depends on the
        ##  value of the 'cibbb_case'-attribute, i.e. if it is the
        ##  circular index-based block bootstrap for tuples that
        ##  should be applied.
        if (attributes(TS)$cibbb_case) {
            ##  Extract the desired indices for the the given
            ##  bootstrap-replicate.
            .indices <- restrict_array(
                .arr = TS,
                .restrict = vec["content"],
                .drop = TRUE,
                .never_drop = c("observations", "variables"),
                .keep_attributes = FALSE)
            ##  Find the adjusted cibbb-indices.
            .cibbb_tuples <- TS_cibbb_tuples(
                .indices = .indices,
                .lag = lag)
            ##  Extract the data of interest.
            .data <- local({
                .first <- restrict_array(
                    .arr = attributes(TS)$orig_TS,
                    .restrict = list(observations = .cibbb_tuples$first,
                                     variables = .pairs[1]),
                    .drop = TRUE)
                .second <- restrict_array(
                    .arr = attributes(TS)$orig_TS,
                    .restrict = list(observations = .cibbb_tuples$second,
                                     variables = .pairs[2]),
                    .drop = TRUE)
                cbind(.first,
                      .second)
            })
        } else {
            ##  Extract the desired part from 'TS'.
            .ts <- restrict_array(
                .arr = TS,
                .restrict = vec["content"],
                .drop = TRUE,
                .never_drop = c("observations", "variables"))
            ##  Extract the data of interest.
            .data <- local({
                .first <- restrict_array(
                    .arr = .ts,
                    .restrict = list(variables = .pairs[1]),
                    .drop = TRUE,
                    .keep_attributes = FALSE)
                .second <- restrict_array(
                    .arr = .ts,
                    .restrict = list(variables = .pairs[2]),
                    .drop = TRUE,
                    .keep_attributes = FALSE)
                if (lag == 0) {
                    cbind(.first,
                          .second)
                } else
                    cbind(
                        tail(.first, -lag),
                        head(.second, -lag))
            })
        }
        ##  Compute local Gaussian parameters from LGA(1).  Reminder:
        ##  If 'x' and 'y' are identical, i.e. when all the points
        ##  lies on the diagonal, then the 'find_rho' function simply
        ##  returns the value '1' for 'par.est' and a 'NA_real' for
        ##  'log_f.est'.
        tmp <- find_rho(
            data = .data,
            grid = matrix(data = level_point,
                          nrow = 1),
            b = bws)
        ##  Use content_details to decide what to return.
        if (content_details == "rho_only") {
            tmp$par.est
        } else
            if (content_details == "rho_log.fun") {
                c(tmp$par.est, log_fun = tmp$log_f.est)
            } else
                c(bws, tmp$par.est, log_fun = tmp$log_f.est)
    }
    ##  Create a helper-function to deal with the 'par_five'-cases.
    par_five_helper <- function(vec, TS, .arr, .points,
                                content_details,
                                fixed_bws = FALSE) {
        ##  Extract the two bandwidths "bi" and "bj".
        bws <- if (fixed_bws) {
            c(bi = vec[["bw_points"]],
              bj = vec[["bw_points"]])
        } else
            restrict_array(
                .arr = .arr,
                .restrict = vec,
                .drop = TRUE,
                .keep_attributes = FALSE)
        ##  Find numerical values of lag and level-point.
        lag <- as.integer(vec$lag)
        level_point <- .points[vec$levels, ]
        ##  Split 'vec$pairs' for subsetting
        .pairs <- strsplit(x = vec$pairs, split = "_")[[1]]
        ##  Get hold of the desired data, strategy depends on the
        ##  value of the 'cibbb_case'-attribute, i.e. if it is the
        ##  circular index-based block bootstrap for tuples that
        ##  should be applied.
        if (attributes(TS)$cibbb_case) {
            ##  Extract the desired indices for the the given
            ##  bootstrap-replicate.
            .indices <- restrict_array(
                .arr = TS,
                .restrict = vec["content"],
                .drop = TRUE,
                .never_drop = c("observations", "variables"),
                .keep_attributes = FALSE)
            ##  Find the adjusted cibbb-indices.
            .cibbb_tuples <- TS_cibbb_tuples(
                .indices = .indices,
                .lag = lag)
            ##  Extract the components of interest.
            .first <- restrict_array(
                .arr = attributes(TS)$orig_TS,
                .restrict = list(observations = .cibbb_tuples$first,
                                 variables = .pairs[1]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            .second <- restrict_array(
                .arr = attributes(TS)$orig_TS,
                .restrict = list(observations = .cibbb_tuples$second,
                                 variables = .pairs[2]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            ##  Drop the names.
            .x <- unname(.first)
            .y <- unname(.second)
        } else {
            ##  Extract the desired part from 'TS'.
            .ts <- restrict_array(
                .arr = TS,
                .restrict = vec["content"],
                .drop = TRUE,
                .never_drop = c("observations", "variables"))
            ##  Extract the components of interest.
            .first <- restrict_array(
                .arr = .ts,
                .restrict = list(variables = .pairs[1]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            .second <- restrict_array(
                .arr = .ts,
                .restrict = list(variables = .pairs[2]),
                .drop = TRUE,
                .keep_attributes = FALSE)
            ##  Make the required adjustment.
            if (lag == 0) {
                .x <- unname(.first)
                .y <- unname(.second)
            } else {
                .x <- unname(tail(.first, -lag))
                .y <- unname(head(.second, -lag))
            }
        }
        ##  Compute local Gaussian parameters from LGA(5).  Note: If
        ##  'x' and 'y' are identical, i.e. when all the points lies
        ##  on the diagonal, then the value '1' is to be used for the
        ##  local Gaussian correlation, whereas all the other values
        ##  are a bit "meaningless".
        tmp <- if (identical(.x, .y)) {
            ##  Create components similar to those extracted from the
            ##  result of the 'localgauss'-function (see below).
            param <- structure(
                .Data = c(NA_real_, NA_real_, NA_real_, NA_real_, 1),
                .Names = c("mu_1", "mu_2", "sig_1", "sig_2", "rho"))
            log_fun <- NA_real_
            eflag <- 0
        } else {
            ##  Compute local Gaussian parameters
            tmp <- localgauss(
                x = .x,
                y = .y,
                b1 = bws["bi"],
                b2 = bws["bj"],
                xy.mat = level_point)
            ##  Extract the parameters of interest.
            param <- structure(
                .Data = as.vector(tmp$par.est),
                .Names = colnames(tmp$par.est))
            eflag <- tmp$eflag
            kill(tmp)
            ##  When required, compute the logarithm of the density.
            if (content_details != "rho_only")
                log_fun <- dmvnorm(
                x = level_point,
                mean = param[c("mu_1", "mu_2")],
                sigma = {
                    sig11 <- param["sig_1"]^2
                    sig12 <- param["sig_1"] * param["sig_2"] * param["rho"]
                    sig22 <- param["sig_2"]^2
                    matrix(
                        data = c(sig11, sig12, sig12, sig22),
                        ncol = 2)},
                log = TRUE)
        }
        ##  Use content_details to decide what to return.
        .result <- if (content_details == "rho_only") {
            c(param["rho"], eflag = eflag)
        } else {
            if (content_details == "rho_log.fun") {
                c(param["rho"], log_fun = log_fun, eflag = eflag)
            } else
                c(bws, param, log_fun = log_fun, eflag = eflag)
        }
    }
    ##  If '.bws_fixed' are different from 'NULL', compute the
    ##  relevant Local Gaussian Approximations.
    if (is.null(.bws_fixed)) {
        par_one_fixed <- NULL
        par_five_fixed <- NULL
    } else {
        ##  Create an argument-grid for the computation later on.
        arg_grid_fixed <- expand.grid(
            lag = 0:spy_report$envir$lag_max,
            levels = rownames(.level_points),
            pairs = .attr_from_TS$.variable_pairs,
            content = dimnames(TS)$content,
            bw_points = .bws_fixed,
            stringsAsFactors = FALSE)
        ##  Compute data for the 'par_one'-case (this automatically
        ##  becomes 'NULL' if 'LG_type' doesn't contain "par_one").
        par_one_fixed <-
            if (any(LG_type == "par_one")) 
                my_aaply(.arg_grid = arg_grid_fixed,
                         .fun = par_one_helper,
                         .new_dnn = "variable",
                         TS = TS,
                         .arr = NULL,
                         .points = .level_points,
                         content_details = content_details,
                         fixed_bws = TRUE)
        ##  Compute data for the 'par_five'-case (this automatically
        ##  becomes 'NULL' if 'LG_type' doesn't contain "par_five").
        par_five_fixed <-
            if (any(LG_type == "par_five"))
                my_aaply(.arg_grid = arg_grid_fixed,
                         .fun = par_five_helper,
                         .new_dnn = "variable",
                         TS = TS,
                         .arr = NULL,
                         .points = .level_points,
                         content_details = content_details,
                         fixed_bws = TRUE)
        kill(arg_grid_fixed)
    }
    kill(.bws_fixed)
    ##  If '.bws_fixed_only' are 'TRUE', find the Local Gaussian
    ##  Approximations based on bandwidths computed from the data.
    if (.bws_fixed_only) {
        par_one_data <- par_one_fixed
        par_five_data <- par_five_fixed
        kill(spy_report, par_one_fixed, par_five_fixed)
    } else {
        ##  Find the two required bandwidths along the diagonal.
        spy_report$envir$levels <- .level_points
        LG_bandwidths_advanced_call <- create_call(
            .cc_fun = LG_bandwidths_advanced,
            spy_report$envir,
            .cc_list = TRUE)
        .mixed_bandwidths <- eval(LG_bandwidths_advanced_call)
        kill(spy_report, LG_bandwidths_advanced_call)
        ##  Investigate if recycling is possible to do for the mixtures.
        .recycling <- attributes(.mixed_bandwidths$h)$recycling
        ###------------------------------------------------------###
        ##  Compute the Local Gaussian Estimates at the specified
        ##  levels, by the help of an argument grid based upon the
        ##  dimension names found in the two parts of
        ##  '.mixed_bandwidths'.  A recycling principle will be used
        ##  for the "mixed" versions of these bandwidths (by looking
        ##  up the results from the "local" and "global" cases).
        ###------------------------------------------------------###
        ##  Create the argument grid for the lags.
        .extract_part <- setdiff(
            x = names(dimnames(.mixed_bandwidths$'h')),
            y = "find_bws")
        arg_grid_h <- expand.grid(
            dimnames(.mixed_bandwidths$'h')[.extract_part],
            stringsAsFactors = FALSE)
        ##  When possible, split in two parts to enable recycling.
        if (.recycling) {
            mixture_part <- str_detect(
                string = arg_grid_h[, "bw_points"],
                pattern = "mix_")
            ##---
            arg_grid_h_mix <- arg_grid_h[mixture_part, ]
            arg_grid_h <- arg_grid_h[! mixture_part, ]
            kill(mixture_part)
        }
        kill(.extract_part)
        ##  Compute data for the 'par_one'-case (this automatically
        ##  becomes 'NULL' if 'LG_type' doesn't contain "par_one").
        par_one_data <- if (any(LG_type == "par_one")) 
                            my_aaply(.arg_grid = arg_grid_h,
                                     .fun = par_one_helper,
                                     .new_dnn = "variable",
                                     TS = TS,
                                     .arr = .mixed_bandwidths$h,
                                     .points = .level_points,
                                     content_details = content_details,
                                     fixed_bws = FALSE)
        ##  Compute data for the 'par_five'-case (this automatically
        ##  becomes 'NULL' if 'LG_type' doesn't contain "par_five").
        par_five_data <- if (any(LG_type == "par_five")) 
                             my_aaply(.arg_grid = arg_grid_h,
                                      .fun = par_five_helper,
                                      .new_dnn = "variable",
                                      TS = TS,
                                      .arr = .mixed_bandwidths$h,
                                      .points = .level_points,
                                      content_details = content_details,
                                      fixed_bws = FALSE)
        kill(arg_grid_h)
        ##  The part below takes care of the "mixed" case, by adding
        ##  relevant stuff to the previously computed objects
        ##  'par_one_data' and 'par_five_data',
        if (.recycling) {
            ##  Extract the look-up table.
            .look_up_h <- 
                attributes(.mixed_bandwidths$h)$look_up
            ##  Need a helper-function to work upon 'arg_grid_h_mix',
            ##  using the look-up tables to avoid recomputing stuff
            ##  whenever possible by picking out the relevant values
            ##  from the previously computed ones.
            mix_helper <- function(vec, .fun_helper, TS, .arr,
                                   .points, .look_up, .prev,
                                   content_details) {
                ##  Find the indicator that reveals if a new computation
                ##  must be performed, or if the result can be picked from
                ##  those previously computed.
                .key <- restrict_array(
                    .arr = .look_up,
                    .restrict = vec,
                    .drop = TRUE)
                ##---
                if (.key == "m") {
                    ##  A new computation is necessary.
                    .result <- .fun_helper(
                        vec = vec,
                        TS = TS,
                        .points = .points,
                        content_details = content_details,
                        .arr = .arr)
                } else {
                    ##  Adjust 'vec' to find '.result' from '.prev', i.e.
                    ##  the result from the previous computations.
                    vec$bw_points <- switch(
                        EXPR = .key,
                        p = str_replace(
                        string = vec$bw_points,
                        pattern = "mix_",
                        replacement = ""),
                        g = "global")
                    ##---
                    .result <- restrict_array(
                        .arr = .prev,
                        .restrict = vec,
                    .drop = TRUE)
                }
                ##  Return '.result' to the work-flow.
                .result
            }
            ##  Find the mix-part for the 'par-one'-case (becomes
            ##  'NULL' when not asked for).
            par_one_data_mix <- 
                if (any(LG_type == "par_one")) 
                    my_aaply(
                        .arg_grid = arg_grid_h_mix,
                        .fun = mix_helper,
                        .new_dnn = "variable",
                        .new_dn = dimnames(par_one_data)$variable,
                        .fun_helper = par_one_helper,
                        TS = TS,
                        .arr = .mixed_bandwidths$h,
                        .points = .level_points,
                        content_details = content_details,
                        .look_up = .look_up_h,
                        .prev = par_one_data)
            ##  Find the mix-part for the 'par-five'-case (becomes
            ##  'NULL' when not asked for). 
            par_five_data_mix <-
                if (any(LG_type == "par_five"))
                    my_aaply(
                        .arg_grid = arg_grid_h_mix,
                        .fun = mix_helper,
                        .new_dnn = "variable",
                        .new_dn = dimnames(par_five_data)$variable,
                        .fun_helper = par_five_helper,
                        TS = TS,
                        .arr = .mixed_bandwidths$h,
                        .points = .level_points,
                        content_details = content_details,
                        .look_up = .look_up_h,
                        .prev = par_five_data)
            kill(.look_up_h, mix_helper)
        } else {
            ##  Create dummy-content for the 'collect'-step.
            par_one_data_mix <- NULL 
            par_five_data_mix <- NULL
        }  #  This ends the 'if-else'-statement for '.recycling'.
        kill(.recycling, .mixed_bandwidths, arg_grid_h_mix, TS)
        ##  Combine the pieces, using the 'my_abind' wrapper for
        ##  'abind'.  This adds a tiny overhead, but it makes for
        ##  simpler code here.  Also add the desired information about
        ##  class in order to get a compact format for print.
        if (any(LG_type == "par_one")) {
            par_one_data <- my_abind(par_one_data,
                                     par_one_data_mix,
                                     par_one_fixed)
            class(par_one_data) <- LG_default$class$array
            kill(par_one_data_mix, par_one_fixed)
        }
        if (any(LG_type == "par_five")) {
            par_five_data <- my_abind(par_five_data,
                                      par_five_data_mix,
                                      par_five_fixed)
            class(par_five_data) <- LG_default$class$array
            kill(par_five_data_mix, par_five_fixed)
        }
    }
    kill(par_one_helper, par_five_helper, .bws_fixed_only)
    ##  Add the attributes from the original TS.
    if (any(LG_type == "par_one"))
        attributes(par_one_data) <- c(
            attributes(par_one_data),
            .attr_from_TS)
    if (any(LG_type == "par_five"))
        attributes(par_five_data) <- c(
            attributes(par_five_data),
            .attr_from_TS)
    ##  Add an attribute to 'par_five_data' that reveals if all the
    ##  values from 'localgauss' originates from a proper numerical
    ##  convergence, i.e. test if all 'eflag' has the value '0'.
    if  (LG_type == "par_five") {
        attr(x = par_five_data, which = "convergence") <-
            0 == sum(restrict_array(
                     .arr = par_five_data,
                     list(variable = "eflag")))
        ##  Create a warning when necessary.
        if (! attributes(par_five_data)$convergence)
            cat(paste("\n\t",
                      "Nonzero exit-flags detected!",
                      "\n\t",
                      "Some optimisations failed, proceed with care.\n\n"))
    }
    ##  Divide the results into components suited for the next
    ##  functions in line.
    .on_diag_restriction <-
        list(levels = attributes(.level_points)$on_diagonal)
    .off_diag_restriction <-
        list(levels = attributes(.level_points)$off_diagonal)
    .off_diag_present <- length(.off_diag_restriction$levels) > 0
    par_one_data <-
        if (is.null(par_one_data)) {
            list(on_diag  = NULL,
                 off_diag = NULL)
        } else
            list(on_diag =
                     restrict_array(
                         .arr = par_one_data,
                         .restrict = .on_diag_restriction),
                 off_diag =
                     if (.off_diag_present)
                         restrict_array(
                             .arr = par_one_data,
                             .restrict = .off_diag_restriction))
    par_five_data <-
        if (is.null(par_five_data)) {
            list(on_diag  = NULL,
                 off_diag = NULL)
        } else
            list(on_diag =
                     restrict_array(
                         .arr = par_five_data,
                         .restrict = .on_diag_restriction),
                 off_diag =
                     if (.off_diag_present)
                         restrict_array(
                             .arr = par_five_data,
                             .restrict = .off_diag_restriction))
    kill(.on_diag_restriction, .off_diag_restriction,
         .off_diag_present)
    ##  Collect the pieces to return to the work-flow, i.e. 'LG_type',
    ##  '.bws_mixture', 'content_details', 'par_one_data',
    ##  'par_five_data' and 'level_points'.  Note: Depending on the
    ##  arguments to this function, some of these returned values
    ##  might be 'NULL', but that will be revealed by the information
    ##  in 'LG_type'.
    .result <- list(
        LG_type = LG_type,
        .bws_mixture = .bws_mixture,
        content_details = content_details,
        par_one_data = par_one_data,
        par_five_data = par_five_data,
        level_points = attributes(.level_points))
    ##  Return '.result' to the work-flow.
    .result
}
