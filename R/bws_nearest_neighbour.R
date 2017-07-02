################################################################################
#'
#' Compute nearest-neighbour bandwidths, (with file-storage).
#'
#' This function will check if a file with previous computations
#' exists, and if so rather read the results from that file than
#' recomputing the results.  If no file is found in the specified
#' directory, or if only some of the results are computed, then it
#' will do the required computations and create a file to be used
#' later on.
#'
#' @template save_dir_arg
#' 
#' @param TS The time series we want to investigate by means of local
#'     Gaussian approximation (and later on with local Gaussian
#'     spectra).  Note that it is assumed that this time series have
#'     normalised marginals.
#'
#' @param lag_min A non-negative integer, default value \code{0}, that
#'     specifies the lowest lagged pairs to compute the bandwidths
#'     for.  This allows the computation of the bandwidths to be
#'     performed in smaller chunks.
#' 
#' @param lag_max How many lags should be included in the analysis.
#' 
#' @param bw_points A vector, default \code{c(25, 35)}, that specifies
#'     the percentage of the observations that we want inside the
#'     "bandwidth-square".
#' 
#' @param levels The points at which we (for different lags) want
#'     to center "bandwidth-squares" that encapsulates the desired
#'     percentage (given by \code{bw_points} of the lagged pairs.  The
#'     format of \code{levels} must be a matrix with one row for
#'     each point of interest, and with columns named \code{c("v1",
#'     "v2")}.
#'
#' @return This function will return a list with the desired
#'     "nearest-neighbour" bandwidths to the work-flow, one part for
#'     the lag zero case and one part for positive lags.  In addition
#'     will there be created/update a file with the desired
#'     information.  If later on there's a need for a computation with
#'     a higher value for \code{lag_max}, then the values from the
#'     saved file will be used in order to avoid redoing previous
#'     computations.
#'
#' @export

#####  2017-01-04: The default value for 'lag_min' adjusted from '1'
#####  to '0', to get everything in one array.  This implies that some
#####  of the old stuff that are saved to file might now be
#####  obsolete...  Check this later on, and clean away superfluous
#####  stuff from the code.

bws_nearest_neighbour <- function(
    save_dir = NULL,
    TS,
    lag_min = 0,
    lag_max,
    bw_points = c(25, 35),
    levels) {
###-------------------------------------------------------------------
    ##  Create a spy report, to use the call recursively at the end.
    spy_report <- spy()
###-------------------------------------------------------------------
    ##  Find the path to the file.
    bws_file <- file.path(save_dir,
                          LG_default$bws_local_file)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The algorithm that computes the negative lags based on a given
###  grid-point use the same information as those found when
###  considering positive lags for the diagonal-symmetric grid-points.
###  To avoid to much fuzz in the code, the given grid-points will
###  thus be "diagonally-completed" in order to obtain all the values
###  of interest.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Record the occurrences of unique components in 'levels'.
    unique_grid_coeff <- unique(as.vector(levels))
###-------------------------------------------------------------------
    ##  Ensure that we compute the required data for both original and
    ##  diagonal-reflected grid-points (if the latter for some reason
    ##  should be missing from 'levels').
    levels <- rbind(
        levels,
        levels[, c("v2", "v1")])
    ##  Adjust the row names of 'levels'. 
    rownames(levels) <- paste(
        levels[, "v1"],
        levels[, "v2"],
        sep = "_")
    ##  Reduce the selection to unique points.
    levels <-
        levels[unique(rownames(levels)), , drop  = FALSE]
###-------------------------------------------------------------------
    ##  If 'TS' originates from a 'TS_LG_object', it should have an
    ##  attribute 'TS_for_analysis' that should be used instead of TS.
    if (! identical(x = attributes(TS)$TS_for_analysis,
                    y = NULL)) {
        TS <- attributes(TS)$TS_for_analysis
    }
###-------------------------------------------------------------------
    ##  Create a logical value to see if old data can (should) be
    ##  used, i.e. if there already has been performed computations
    ##  that either can be used directly or built upon if an extended
    ##  version is desired.
    old_data <- all(
        ! is.null(save_dir),
        file.exists(bws_file))
###-------------------------------------------------------------------
    ##  When old data exists, read them in and investigate if any new
    ##  computations must be performed.
    if (old_data) {
        ##  Load previous versions of objects '.bws_data',
        ##  '.ga_distances' and 'grid_lag_distances' to the work-flow.
        load(bws_file)
        ##  Check for new grid points.
        new_unique_grid_coeff <- setdiff(
            x = unique_grid_coeff,
            y = dimnames(.ga_distances)$grid)
        ##  Register any new grid points
        new_levels <- setdiff(
            x = rownames(levels),
            y = dimnames(.bws_data)$levels)
        ##  Check for new 'bw_points'.
        old_bw_points <- as.numeric(str_replace(
            string = dimnames(.bws_data)$bw_points,
            pattern = "%",
            replacement = ""))
        ##---
        new_bw_points <-
            setdiff(
                x = bw_points,
                y = old_bw_points)
        ##  Check for new lags.
        old_lags <-
            as.numeric(dimnames(.bws_data)$lag)
        new_lags <- setdiff(
            x = lag_min:lag_max,
            y = old_lags)
###-------------------------------------------------------------------
        ##  If no new computations are necessary, then simply extract
        ##  the desired result and return those to the work-flow.
        ##  (Note: A restriction is necessary, since the old
        ##  computation might be larger than what is needed in the
        ##  present case of interest.)
        if (all(c(length(new_unique_grid_coeff),
                  length(new_bw_points),
                  length(new_lags)) == 0)) {
            .bw_points <- paste(
                bw_points,
                "%",
                sep = "")
            return(restrict_array(
                .arr = .bws_data,
                .restrict = list(
                    lag = as.character(lag_min:lag_max),
                    levels = rownames(levels),
                    bw_points = .bw_points)))
        }
    } else {
        ##  Everything must be computed from scratch, so initiate the
        ##  required components.
        new_lags <- lag_min:lag_max
        new_bw_points <- bw_points
        new_unique_grid_coeff <- unique_grid_coeff
        new_levels <- rownames(levels)
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The code below is written in order to cope with the case that
###  'old_data' might be 'TRUE', and that new stuff must be added.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Compute/update "grid-adjusted" distances.
    new_.ga_distances <- structure(
        .Data = abs(outer(
            X = TS,
            Y = new_unique_grid_coeff,  
            FUN = '-')),    ##  Add identifying names
        .Dimnames = c(
            dimnames(TS),
            list(grid = new_unique_grid_coeff)),
        class = LG_default$class$array)
    ##  Compose with older values when old and new data exists.
    if (old_data) {
        if (length(new_unique_grid_coeff) > 0) {
            .ga_distances <- abind( 
                .ga_distances,
                new_.ga_distances,
                along = which(
                    names(dimnames(.ga_distances)) == "grid"))
            ##  Reinsert names of dimension-names.
            names(dimnames(.ga_distances)) <-
                names(dimnames(new_.ga_distances))
        }
    } else
        .ga_distances <- new_.ga_distances
    kill(new_.ga_distances)
###-------------------------------------------------------------------
    ##  Compute/update "grid_lag_distances" (for the old lags) with
    ##  new data if new grid points are present, or create a
    ##  skeleton-list if this is the first time the function is used.
    if (old_data) {
        if (length(new_unique_grid_coeff) > 0) {
            for (i in new_levels) {
                .tmp_v1 <- restrict_array(
                    .arr = .ga_distances,
                    .restrict = list(
                        grid = as.character(levels[i, "v1"])))        
#####  Reminder: 'grid' must be a character!
                .tmp_v2 <- restrict_array(
                    .arr = .ga_distances,
                    .restrict = list(
                        grid = as.character(levels[i, "v2"])))
                ##  Loop over 'old_lags' and add the distances.
                for (.lag in old_lags) 
                    grid_lag_distances[[i]][[.lag]] <- pmax(
                        tail_part = apply(
                            X = .tmp_v1,
                            MARGIN = "content",
                            FUN = tail,
                            n = - .lag),
                        head_part = apply(
                            X = .tmp_v2,
                            MARGIN = "content",
                            FUN = head,
                            n = - .lag))
            }
            kill(.tmp_v1, .tmp_v2)
        }
    } else
        ##  Create skeleton-list for storage.
        grid_lag_distances <- skeleton_list(
            names_list = list(
                levels = rownames(levels),
                lag = new_lags),
            add_names_list_as_attribute = FALSE)
###-------------------------------------------------------------------
    ##  Compute/update "grid_lag_distances" if new lags are present,
    ##  by extracting relevant data from '.ga_distances' in order to
    ##  compute the distances upon which 'bw_points' will be used.
    ##  (Reminder: The second loop will be empty if no new lags are
    ##  present, no need for additional testing.)
    for (i in rownames(levels)) {
        .tmp_v1 <- restrict_array(
            .arr = .ga_distances,
            .restrict = list(
                grid = as.character(levels[i, "v1"])))        
#####  Reminder: 'grid' must be a character!
        .tmp_v2 <- restrict_array(
            .arr = .ga_distances,
            .restrict = list(
                grid = as.character(levels[i, "v2"])))
        ##  Loop over all the lags and compute the distances.
        for (.lag in new_lags) 
            grid_lag_distances[[i]][[as.character(.lag)]] <- if (.lag == 0) {
                pmax(.tmp_v1,
                     .tmp_v2)
            } else
                pmax(tail_part = apply(
                         X = .tmp_v1,
                         MARGIN = "content",
                         FUN = tail,
                         n = - .lag),
                     head_part = apply(
                         X = .tmp_v2,
                         MARGIN = "content",
                         FUN = head,
                         n = - .lag))
    }
    kill(.tmp_v1, .tmp_v2)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Compute/update ".bws_data', with adjustments depending on the
    ##  presence of new grid points, new percentages (i.e. new values
    ##  in 'bw-points') or new lags.  Do this by using 'aaply' on an
    ##  argument grid, and the following help-function that simply
    ##  selects the desired quantiles.
    local_bw <- function(row, grid_lag_distances, bw_points) {
        ##  Extract the desired distances.
        .distances <-
            grid_lag_distances[[row$levels]][[as.character(row$lag)]]
        ##  Find distances based on the percentages from 'bw_points'.
        ##  Use 'aaply' to avoid degeneracies of the dimensions.
        aaply(.data = .distances,
              .margins = which(names(dimnames(.distances)) == "content"),
              .fun = quantile,
              probs = bw_points / 100,
              .parallel = TRUE,
              .drop = FALSE)
    }


###-------------------------------------------------------------------
    ##  Update '.bws_data' if 'old_data' is 'TRUE' and new grid points
    ##  where given.
    if (old_data) {
        if (length(new_unique_grid_coeff) > 0) {
            arg_grid <- expand.grid(
                lag = old_lags,
                levels = new_levels,
                stringsAsFactors = FALSE)
            new_grid_old_lag_data <- aaply(
                .data = arg_grid,
                .margins = 1,
                .fun = local_bw,
                grid_lag_distances = grid_lag_distances,
                bw_points = old_bw_points,
                .drop = FALSE,
                .parallel = TRUE)
            ##  Add 'bw_points' as a dimension-name.
            names(dimnames(new_grid_old_lag_data))[4] <-
                "bw_points"
            ##  Add new stuff to '.bws_data'.
            .bws_data <- abind(
                .bws_data,
                new_grid_old_lag_data,
                along = which(
                    names(dimnames(.bws_data)) == "levels"))
            ##  Reinsert the names on dimension names.
            names(dimnames(.bws_data)) <-
                names(dimnames(new_grid_old_lag_data))
            kill(new_grid_old_lag_data)
        }
###-------------------------------------------------------------------
        ##  Update '.bws_data' and '.bws_zero' if 'old_data' is 'TRUE'
        ##  and new percentages are present in 'bw_points'.
        if (length(new_bw_points) > 0) {
            arg_grid <- expand.grid(
                lag = old_lags,
                levels =  rownames(levels),
                stringsAsFactors = FALSE)
            new_grid_old_lag_data <- aaply(
                .data = arg_grid,
                .margins = 1,
                .fun = local_bw,
                grid_lag_distances = grid_lag_distances,
                bw_points = new_bw_points,
                .drop = FALSE,
                .parallel = TRUE)
            ##  Add 'bw_points' as a dimension-name.
            names(dimnames(new_grid_old_lag_data))[4] <-
                "bw_points"
            ##  Add new stuff to '.bws_data'.
            .bws_data <- abind(
                .bws_data,
                new_grid_old_lag_data,
                along = which(
                    names(dimnames(.bws_data)) == "bw_points"))
            ##  Reinsert the names on dimension names.
            names(dimnames(.bws_data)) <-
                names(dimnames(new_grid_old_lag_data))
            kill(new_grid_old_lag_data, old_lags)
            ##  Add stuff to '.bws_zero'.
        }
    }
###-------------------------------------------------------------------
    ##  Compute/update '.bws_data' for new lags, when present (that
    ##  might not be the case if old data exists).
    if (length(new_lags) > 0) {
        arg_grid <- expand.grid(
            lag = new_lags,
            pairs  = attributes(TS)$.variable_pairs,
            levels = rownames(levels),
            stringsAsFactors = FALSE)
###-------------------------------------------------------------------
        ##  Use 'aaply' and 'local_bws' on 'arg_grid'.
        new_.bws_data <- aaply(
            .data = arg_grid,
            .margins = 1,
            .fun = local_bw,
            grid_lag_distances = grid_lag_distances,
            bw_points = bw_points,
            .drop = FALSE,
            .parallel = TRUE)
###-------------------------------------------------------------------
        ##  Add the dimension name 'bw_points'
        names(dimnames(new_.bws_data))[5] <- "bw_points"
        ## names(dimnames(new_.bws_data))[4] <- "bw_points"
    } else
        new_.bws_data <- NULL
###-------------------------------------------------------------------
    ##  Compose this with with older values when necessary, otherwise
    ##  create new object '.ga_distances'.
    if (old_data) {
        if (length(new_lags) > 0) {
            .bws_data <- abind(
                .bws_data,
                new_.bws_data,
                along = which(names(dimnames(.bws_data)) == "lag"))
            ##  Reinsert names of dimension-names.
            names(dimnames(.bws_data)) <-
                names(dimnames(new_.bws_data))
        }
    } else
        .bws_data <- new_.bws_data
    kill(new_.bws_data)
    class(.bws_data) <- LG_default$class$array
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  When it comes to '.bws_zero' and 'old_data' is true, then we
###  might need to grow it based on new percentiles in 'bw_points' or
###  when new grid points have been added.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    if (old_data)
        if (length(new_levels) > 0) {
            new_levels_.bws_zero <- aaply(
                .data = restrict_array(
                    .arr = .ga_distances,
                    .restrict = list(
                        grid = as.character(new_unique_grid_coeff))),
                .margins = which(names(dimnames(.ga_distances)) %in%
                                     c("content", "grid")),
                .fun = quantile,
                probs = old_bw_points / 100,
                .drop = FALSE,
                .parallel = TRUE)
            ##  Add the dimension name 'bw_points'
            names(dimnames(new_levels_.bws_zero))[3] <- "bw_points"
            ##  Update '.bws_zero' with new content.
            .bws_zero <- abind(
                .bws_zero,
                new_levels_.bws_zero,
                along = which(names(dimnames(.bws_zero)) == "grid"))
            ##  Reinsert names of dimension-names.
            names(dimnames(.bws_zero)) <-
                names(dimnames(new_levels_.bws_zero))
            kill(new_levels_.bws_zero)
        }
###-------------------------------------------------------------------
    ##  Compute new lag zero information from '.ga_distances' for the
    ##  percentiles given in 'bw_points'.
    if (length(new_bw_points) > 0) {
        new_.bws_zero <- aaply(
            .data = .ga_distances,
            .margins = which(names(dimnames(.ga_distances)) %in%
                                 c("content", "grid")),
            .fun = quantile,
            probs = new_bw_points / 100,
            .drop = FALSE,
            .parallel = TRUE)
        ##  Add the dimension name 'bw_points'
        names(dimnames(new_.bws_zero))[3] <- "bw_points"
    }
###-------------------------------------------------------------------
    ##  Adjust based on 'old_data'.
    if (old_data) {
        if (length(new_bw_points != 0)) {
            .bws_zero <- abind(
                .bws_zero,
                new_.bws_zero,
                along = which(names(dimnames(.bws_zero)) == "bw_points"))
            ##  Reinsert names of dimension-names.
            names(dimnames(.bws_zero)) <-
                names(dimnames(new_.bws_zero))
        }
    } else
        .bws_zero <- new_.bws_zero
    kill(new_.bws_zero)
###-------------------------------------------------------------------
    ##  Save to file, when required.
    if (! is.null(save_dir))
        save(.bws_zero, .bws_data, .ga_distances,
             grid_lag_distances, file = bws_file)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The procedure used here when 'old_data' is TRUE will create an
###  object that might be larger than what we need.  Since stuff now
###  is saved to file, it will be sufficient to call this function
###  recursively to extract the desired components without repeating
###  the code.  However, that strategy can not be used when 'save_dir'
###  is 'NULL' -- but that's OK since no old data will mess up the
###  result in that case.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Return the result to the work-flow.
    if (is.null(save_dir)) {
        ## return(list('0' = .bws_zero,
        ##             h = .bws_data))
#####  TASK: Modify code later on to to deal with adjusted result,
#####  then clean away superfluous stuff from the present function.
        return(.bws_data)
    } else
        ##  Reminder: The evaluation of the call below will trigger
        ##  the 'return()' earlier in this function.  Using this,
        ##  there's no need to once more cope with a situation where
        ##  previous computations (stored on disk) contain more
        ##  information than those asked for in the present case.
        eval(spy_report$call)
}
