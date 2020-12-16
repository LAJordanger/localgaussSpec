#' Helper-function to deal with the details when the numerical convergence fails.
#'
#' @description This internal helper function is only called when one
#'     (or more) of the estimated local Gaussian (auto- or cross-)
#'     correlations fails to converge numerically.  In this case the
#'     underlying convergence information will be analysed in order to
#'     identify more details about the problem.
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @param .AB_env The environment that contains the desired
#'     information.  This argument is inherited from \code{LG_lookup}.
#'
#' @return An additional node will be added to the \code{details}-list
#'     in the calling function \code{LG_lookup_details}.  This list
#'     can be used to figure out if the observed issue regarding
#'     failed numerical convergence is present for the case under
#'     investigation.
#' 
#' @keywords internal

LG_lookup_details_NC_fail <- function(look_up,
                                      .AB_env) {
    ##  Check if the loading of the data has been performed before,
    ##  and if not take care of that part first.
    .NC_fail_name <- "NC_fail_details"
    if (!exists(x = .NC_fail_name, envir = .AB_env)) {
        ##  Load the data into the workflow.
        LG_load(.file = look_up$approx_file,
                .name = ".tmp")
        ##  Reminder: This is a list with two nodes, for each node
        ##  restrict the attention to the eflag-part, and store that
        ##  part in '.AB_env'
        .AB_env[[.NC_fail_name]] <- lapply(
            X = .tmp,
            FUN = function(x) {
                if (is.null(x)) {
                    return(NULL)
                }
                restrict_array(
                    .arr = x,
                    .restrict = list(variable = "eflag"),
                    .keep_attributes = FALSE)
            })
    }
    ##  Check if the status for the tuning parameters of the present
    ##  plot already has been investigate and stored in '.AB_env', and
    ##  if so get the result without new computations.  Reminder: This
    ##  basic approach focus on all the lags, which implies some
    ##  additional tweaking must be done for the m-truncated spectra.
    .save_key <- digest::digest(look_up$restrict$NC_check)
    if (exists(x = .save_key, envir = .AB_env)) {
        .result <- .AB_env[[.save_key]]
    } else {
        .result <- list()
        ##  Need to find the status for the given tuning parameters.
        ##  This might require both positive and negative lags, but
        ##  the negative lags are only included when required.
        .branch <- look_up$restrict$NC_check$CS$branch
        .pos_lags <- restrict_array(
            .arr = .AB_env[[.NC_fail_name]][[.branch]],
            .restrict = look_up$restrict$NC_check$CS$pos_lags)
        .lag <- function(x) {
            c(P = sum(x != 0))
        }
        .result$pos_lag_status <- my_apply(
            X = .pos_lags,
            MARGIN = setdiff(
                x = names(dimnames(.pos_lags)),
                y = "content"),
            FUN = .lag)
        ##  Check if a problem was present.
        .result$problem_present <- any(.result$pos_lag_status != 0)
        ##  Add negative lags when required.
        if (look_up$is_negative_lags_needed) {
            .neg_lags <- restrict_array(
                .arr = .AB_env[[.NC_fail_name]][[.branch]],
                .restrict = look_up$restrict$NC_check$CS$neg_lags)
            .result$neg_lag_status <- my_apply(
                X = .neg_lags,
                MARGIN = setdiff(
                    x = names(dimnames(.neg_lags)),
                    y = "content"),
                FUN = .lag)
            ##  Update 'problem_present'-status.
            .result$problem_present <- any(.result$problem_present,
                                           .result$neg_lag_status != 0)
        }
        kill(.NC_fail_name)
        ##  If a problem was detected, then it is necessary to find
        ##  out if the m-truncated spectra for low values of m might
        ##  still be OK, and it is also necessary to add information
        ##  that can be used to highlight the box-plots for the local
        ##  Gaussian auto/cross-correlations in such a manner that it
        ##  is easy to see where the problem is present.
        if  (.result$problem_present) {
            .pos_part <- structure(
                .Data = {cumsum(.result$pos_lag_status) != 0},
                .Names = dimnames(.result$pos_lag_status)$lag)
            if (look_up$is_negative_lags_needed) {
                .neg_part <- structure(
                    .Data = {cumsum(.result$neg_lag_status) != 0},
                    .Names = dimnames(.result$neg_lag_status)$lag)
                ##  For the spectra it is of interest to merge these
                ##  two into one vector, and some extra care is then
                ##  needed with regard to the lag-zero case (which
                ##  only is present among the positive lags).
                .pos_part[names(.neg_part)] <-
                    {.pos_part[names(.neg_part)] | .neg_part}
            }
            ##  Create a node with the convergence result for the
            ##  different truncation values of the spectra.
            .result$S_status <- .pos_part
            kill(.pos_part, .neg_part)
            ##  Create a vector that can be used to specify colours
            ##  and fill for the boxplots related to the local
            ##  Gaussian autocorrelations.  As before this starts with
            ##  the positive lags and then adds negative lags when
            ##  needed.  The names of this vector is only used in
            ##  order to see if the lag "0" case might need to be
            ##  removed in the auto-covariance cases.
            C_status <- structure(
                .Data = {as.vector(.result$pos_lag_status) != 0},
                .Names = dimnames(.result$pos_lag_status)$lag) 
            if (look_up$is_negative_lags_needed) {
                C_status <- c(
                    structure(
                        .Data = rev({as.vector(.result$neg_lag_status) != 0}),
                        .Names = sprintf("-%s",
                                         rev(dimnames(.result$neg_lag_status)$lag))),
                    C_status)
            }
            ##  Remove the lag zero component if this is related to
            ##  local Gaussian autocorrelations.
            if (look_up$is_auto_pair) {
                .keep <- setdiff(x = names(C_status),
                                 y = "0")
                C_status <- C_status[.keep]
            }
            ##  Create a node with the convergence result for the
            ##  local Gaussian correlations.
            .result$C_status <- C_status
            kill(C_status)
        }
        ##  Store the result in '.AB_env'.
        .AB_env[[.save_key]] <- .result
        kill(.save_key)
    }
    ##  Return a list to the workflow based on the observed status and
    ##  the selected type of plot.
    .R <- list()
    .R$problem_present <- .result$problem_present
    ##  Do some adjustment if a problem is detected
    if (.R$problem_present) {
        if (look_up$TCS_type == "S") {
            ##  Update status based on the truncation level.
            .R$problem_present  <-
                .result$S_status[as.character(look_up$m_selected)]
        } else {
            ##  Add 'C_status' to the result
            .R$C_status <- .result$C_status
        }
    }
    ##  Return the result to the workflow.
    .R
}
