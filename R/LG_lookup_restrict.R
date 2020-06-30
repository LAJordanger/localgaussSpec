#' Helper-function to create the restrict-list
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @return A \code{restrict} list will be returned to the calling
#'     function (i.e. \code{LG_lookup}), and this list contains the
#'     details needed for the slicing and dicing of the data loaded
#'     from file.
#' 
#' @keywords internal

LG_lookup_restrict <- function (look_up = look_up) {
    ##  Initiate the 'restrict'-list, and create a template-list with
    ##  the collection of standard restrictions, so the versions later
    ##  on can be created by subsetting the template.
    restrict <- list()
    .template <- list(
        global_branch = list(TS = ifelse(
                                 test = look_up$is_bootstrap,
                                 yes  = "TS_original",
                                 no   = "TS_for_analysis")),
        local_branch = list(bm = c(look_up$local_name,
                                   look_up$point_type_branch)),
        variable = list(variable = "rho"),
        bw_points = list(bw_points = look_up$bw_points),
        pairs_ViVj = list(pairs = look_up$pairs_ViVj),
        pairs_VjVi = list(pairs = look_up$pairs_VjVi),
        pairs = list(pairs = unique(c(look_up$pairs_ViVj,
                                      look_up$pairs_VjVi))),
        point_standard = list(levels = look_up$levels_point),
        point_reflected = list(levels = look_up$levels_point_reflected),
        point_both = list(levels = unique(c(look_up$levels_point,
                                            look_up$levels_point_reflected))),
        non_neg_lags = list(lag = as.character(c(0, look_up$lag_vec))),
        pos_lags = list(lag = if (look_up$is_lag_zero_needed) {
                                  as.character(c(0, look_up$lag_vec))
                              } else
                                  as.character(look_up$lag_vec)),
        neg_lags = list(lag = as.character(look_up$lag_vec)))
    ##  Define the initial restriction to be used for all cases.  The
    ##  reason the restriction for the local case is given as a
    ##  function is to have a function digest this properly later on.
    restrict$initial$global <- function(.node) {
        list(TS = .node)
    }
    restrict$initial$local <- function(.node) {
        c(if (look_up$heatmap) {
              if (look_up$heatmap_b_or_v == "b") {
                  .template$point_both
              } else {
                  .template$bw_points
              }
          },
          list(variable = "rho"))}
    ###-------------------------------------------------------------------
    ##  Different restrictions are needed at different stages of the
    ##  investigation.  The names used here should (if possible) be
    ##  reflected by those used in the caching of the results.  The
    ##  first bunch of restriction lists are aimed at those used in
    ##  'LG_shiny_correlation'.
    ###-------------------------------------------------------------------
    ##  Restrict attention to the main branches for the the global and
    ##  local data.  NB: Need an additional 'bookmark' for the initial
    ##  subsetting for the local case.
    restrict$G_branch <- .template$global_branch
    restrict$L_branch <- list(bm = .template$local_branch,
                              rl = .template$bw_points)
    ###-------------------------------------------------------------------
    ##  Restrict attention to the pairs, with an additional
    ##  restriction if the lag zero component also can be dropped
    ##  (i.e. the cases where it is constant and equal to one.)
    restrict$G_pairs <- c(.template$pairs,
                          if  (!look_up$is_lag_zero_needed)
                              .template$pos_lags)
    restrict$L_pairs <- restrict$G_pairs
    ###-------------------------------------------------------------------
    ##  The restrictions to be used in the internal 'unfold'-function
    ##  of 'LG_shiny_correlation'.  The unfolding is necessary in
    ##  order to get the correct plots for those cases where the local
    ##  Gaussian auto-correlations are not symmetric around zero.
    restrict$C$local$pos_lags <- c(.template$pairs_ViVj,
                                   .template$pos_lags,
                                   .template$point_reflected)
    restrict$C$global$pos_lags <- c(.template$pairs_ViVj,
                                    .template$pos_lags)
    if (look_up$is_negative_lags_needed) {
        restrict$C$local$neg_lags <-c(.template$pairs_VjVi,
                                      .template$neg_lags,
                                      .template$point_standard)
        restrict$C$global$neg_lags <- c(.template$pairs_VjVi,
                                        .template$neg_lags)
    }
    ##  Incude information about '.never_drop' for this case.
    restrict$C$local$.never_drop <- c("lag", "content", "variable")
    restrict$C$global$.never_drop <- c("lag", "content")
    ##  Reminder: The 'local'-part of the details above are used in
    ##  order to define the 'L_levels'-node of the cache.
    ###-------------------------------------------------------------------
    ###-------------------------------------------------------------------
    ##  The restrictions in the function 'LG_shiny_spectra' often
    ##  depends on values from internal loop-constructions, and
    ##  functions must thus be used for this part.  Reminder: Some
    ##  functions are completely trivial, but they are still included
    ##  here (instead of in the main code) in order for them to be
    ##  used when constructing the cache-names.  Reminder: The setup
    ##  below is based on a stepwise approach where
    ##  'spectra_summands_local' and 'spectra_summands_global' refers
    ##  to the product of the correlations and the complex valued
    ##  'e^{-2*pi*i*omega}', whereas 'spectra_local' and
    ##  'spectra_global' have included the lag-window-weighting
    ##  function and taken the relevant sum.  The 'CI_local' and
    ##  'CI_global' extracts the desires confidence intervals.
    restrict$S$spectra_summands_global <- function(.node) {
        list(TS = .node)
    }
    restrict$S$spectra_summands_local <- c(
        if (look_up$heatmap) {
            if (look_up$heatmap_b_or_v == "b") {
                .template$point_both
            } else {
                .template$bw_points
            }
        },
        list(variable = "rho"))
    restrict$S$spectra_global <- function(.cut) {
        list(lag = as.character(1:(as.numeric(.cut)-1)),
             pairs = unique(c(look_up$pairs_ViVj,
                              look_up$pairs_VjVi)))
    }
    restrict$S$spectra_local <- restrict$S$spectra_global
    restrict$S$CI_global <- list(
        bm = c("TS_original", look_up$cut, "spec"),
        rl = list(S_type = look_up$spectra_type),
        never_drop = c("omega", "content"))
    restrict$S$CI_local <- list(
        bm = look_up$.bm_CI_local_spectra,
        rl = list(S_type = look_up$spectra_type,
                  bw_points = look_up$bw_points,
                  levels = ifelse(
                      test = look_up$is_adjust_sign,
                      yes  = look_up$levels_point_reflected,
                      no   = look_up$levels_point)),
        never_drop  = c("omega", "content"))
    ###-------------------------------------------------------------------
    ##  The restriction to be used in 'LG_create_plot_df', i.e. the
    ##  last restrictions before data-frames for the plots are
    ##  created.  Reminder: For the spectra, we will always need one
    ##  of "mean" or "orig" (both of them for the bootstrap-case), and
    ##  the confidence intervals must be included when required.
    restrict$S$spectra_df <- list(
        content = c(if (look_up$is_bootstrap)
                        "orig",
                    ifelse(test = look_up$is_CI_needed,
                           yes  = "mean",
                           no   = "orig"),
                    if (look_up$is_CI_needed)
                        look_up$.CI_low_high))
    ###-------------------------------------------------------------------
    ##  The restrictions to be used when it turns out that at least
    ##  one of the local Gaussian estimates did not succeed, and it is
    ##  of interest to check if the problem is present for the tuning
    ##  parameters used to specify the plot of interest.  This is done
    ##  by recycling/combining information extracted above.
    restrict$NC_check$CS <- list(
        branch = look_up$point_type_branch,
        pos_lags = c(restrict$C$local$pos_lags,
                     bw_points = look_up$bw_points),
        neg_lags = c(restrict$C$local$neg_lags,
                     bw_points = look_up$bw_points))
    ##  Tweak the list to ensure that lag "0" also is included for the
    ##  positive lags.
    restrict$NC_check$CS$pos_lags$lag <- .template$non_neg_lags$lag
    ###-------------------------------------------------------------------
    ##  Include a helper function that will be used as the initial
    ##  step in several other functions.  Reminder: If all the
    ##  investigations starts with this, then it would have been
    ##  better to redo the way the data are stored to files in the
    ##  first place...
    restrict$F$initial <- function(..env, look_up) {
        for (.gl in c("global", "local")) {
            ##  Define the '.gl'-dependent details.
            .gl_name <- ifelse(
                test = {.gl == "local"},
                yes  = look_up$local_name,
                no   = look_up$global_name)
            .node_names <-
                if (.gl == "local") {
                    names(..env[[.gl_name]])
                } else {
                    dimnames(..env[[.gl_name]])$TS
                }
            .arr_node <- function(.node) {
                if (.gl == "local") {
                    ..env[[.gl_name]][[.node]]
                } else
                    ..env[[.gl_name]]
            }
            .initial_name <- look_up$cache$initial[[.gl]]
            ##  Jumpt to next case if stuff has been done already.
            if (exists(x = .initial_name, envir = ..env))
                next
            ##  Create the desired content.
            .result <- list()
            ##  Add details to the list.
            for (.node in .node_names) {
                if (.gl == "local") {
                    if (is.null(..env[[.gl_name]][[.node]]))
                        next
                }
                .result[[.node]] <- list()
                .corr <- restrict_array(
                    .arr = .arr_node(.node),
                    .restrict = look_up$restrict$initial[[.gl]](.node))
                ##  Check if it is necessary to extract the lag-zero
                ##  component for the case under investigation.
                .extract_lag_zero <- ifelse(
                    test =  {.gl == "local"},
                    yes  = look_up$is_lag_zero_included,
                    no   = {"0" %in% dimnames(.corr)$lag})
                ##  Extract when necessary. Reminder: In order for the
                ##  addition of the arrays to work properly later on,
                ##  the "lag"-dimension must be dropped.
                if (.extract_lag_zero) {
                    .result[[.node]][["lag_zero"]] <-
                        restrict_array(
                            .arr = .corr,
                            .restrict = list(lag = "0"))
                    .dn <- dimnames(.result[[.node]][["lag_zero"]])
                    .lag_pos <- which(names(.dn) == "lag")
                    dim(.result[[.node]][["lag_zero"]]) <-
                        dim(.result[[.node]][["lag_zero"]])[-.lag_pos]
                    dimnames(.result[[.node]][["lag_zero"]]) <- .dn[-.lag_pos]
                }
                ##  Store the positive lags.
                .result[[.node]][["pos"]] <- restrict_array(
                    .arr = .corr,
                    .restrict = list(lag = as.character(look_up$lag_vec)))
            }
            ##  Add the desired content to '..env'.
            ..env[[.initial_name]] <- .result
        }
        ##  Return nothing to the workflow.
        return(invisible(NULL))
    }
    ###-------------------------------------------------------------------
    ##  Return the 'restrict'-list, so 'LG_lookup' can add it as a new
    ##  node to the 'look_up'-list
    restrict
}
