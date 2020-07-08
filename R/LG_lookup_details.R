#' Helper-function to deal with the details.
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @param .AB_env The environment that contains the desired
#'     information.  This argument is inherited from \code{LG_lookup}.
#'
#' @return A \code{details} list will be returned to the calling
#'     function (i.e. \code{LG_lookup}), and this list contains
#'     details needed for the description of the content of the plots.
#' 
#' @keywords internal

LG_lookup_details <- function(look_up,
                              .AB_env) {
    ##  Add all the logcial 'is_'-values from 'look_up'.
    details <- look_up[str_detect(string = names(look_up),
                                  pattern = "is_")]
    ## Add information from 'look_up' and '.AB_env'.
    details$TS_key <- look_up$TS_key
    details$details <- .AB_env$details$details
    details$N <- .AB_env$details$N
    details$.variables <- .AB_env$details$.variables
    details$.nr_variables <- .AB_env$details$.nr_variables
    details$Vi <- look_up$Vi
    details$Vj <- look_up$Vj
    ##  Add details based on the value of 'is_block'
    if (details$is_block) {
        details$nr_simulated_samples <- .AB_env$details$nr_simulated_samples
    }
    ###-------------------------------------------------------------------
    ##  Add information about the content of the plot.
    details$TCS_type <- look_up$TCS_type
    details$text$length_variables <- paste(
        ifelse(test = details$is_block,
               yes  = "simulated",
               no   = "real"),
        " data of length ",
        details$N,
        ", ",
        ifelse(test = details$.nr_variables == 1,
               yes  = "univariate",
               no   = paste(
                   details$.nr_variables,
                   "-variate",
                   sep = "")),
        " observations.",
        sep = "")
    details$window <- look_up$window
    details$truncation_level <- look_up$m_selected
    ###-------------------------------------------------------------------
    ##  Investigate if information about convergence-status is
    ##  available, which only is relevant for the local cases when the
    ##  five-parameter approximation has been used.  Reminder: This
    ##  value can be 'NULL' in some cases, so it is preferable to test
    ##  the value with 'isTRUE'.
    details$convergence  <- 
        if (details$is_local) {
            if  (look_up$type == "par_five") {
                local({
                    ##  Extract convergence-information, with a minor
                    ##  adjustment to deal with the different names
                    ##  used for the bootstrap data.
                    .file <- ifelse(test = details$is_bootstrap,
                                    yes  = "boot_par_five_approx.Rda",
                                    no   = "par_five_approx.Rda")
                    .AB_env$details$convergence[[c(.file, look_up$point_type)]]
                })
            }
        }
    ###-------------------------------------------------------------------
    ##  Add information about the type of local Gaussian approximation
    ##  that was used in the construction, i.e. if the estimated local
    ##  Gaussian correlations stems from a one- or five-parameter
    ##  approximation.
    details$type <- gsub(pattern = "par_",
                         replacement = "",
                         look_up$type)
    ##  Add information about the variables, and if it is an auto- or
    ##  cross-case that is under investigation.  Note that the
    ##  'auto_cross' in this case should be anew, since the
    ##  'auto_cross' part of 'look_up' can contain both values.
    details$Vi <- look_up$Vi
    details$Vj <- look_up$Vj
    details$auto_cross <- ifelse(
        test = details$is_auto_pair,
        yes  = "auto",
        no   = "cross")
    ##  Extract information about the spectrum variant, to be used as building
    ##  block and in tests.
    if (look_up$TCS_type == "S") {
        ##  Find the parts of 'S_type'.
        .parts <- stringr::str_split(string = look_up$S_type,
                                     pattern = "_")[[1]]
        ##  Create a basic version of the spectrum variant.
        details$spectrum_variant <-
            ifelse(test = length(.parts) == 2,
                   yes  = details$auto_cross,
                   no   = .parts[3])
        ##  Create a more detailed version that takes into account the need for
        ##  the investigation to also treat complex valued local Gaussian
        ##  auto-spectra (for points that lies off the diagonal).
        details$spectrum_variant_detailed <-
            if (length(.parts) == 2) {
                    sprintf("%s-spectrum",
                            details$auto_cross)
            } else
                sprintf("(%s) %s-spectrum",
                        details$auto_cross,
                        .parts[3])
        ##  Add descriptive information about the spectrum in the case it is an
        ##  investigation of a complex valued entitiy.  In particular, this
        ##  explanation will not be included for the real-valued spectra.
        details$spectrum_variant_cross_details <-
            if (any(details$is_cross_pair,
                    all(details$is_auto_pair,
                        ! details$is_on_diagonal)))
                sprintf(
                    "The %s%s is %s of the complex-valued %scross-spectrum.%s",
                    ifelse(test = details$is_local,
                           yes  = "local Gaussian ",
                           no   = " "),
                    details$spectrum_variant_detailed,
                    switch(EXPR = details$spectrum_variant,
                           Co   = "the real part",
                           Quad = "minus one times the imaginary part",
                           amplitude = "the amplitude",
                           phase = "the phase"),
                    ifelse(test = details$is_local,
                           yes  = "local Gaussian ",
                           no   = ""),
                    ifelse(test = all(details$is_auto_pair,
                                      ! details$is_on_diagonal),
                           yes  = paste("  Note that the local Gaussian",
                                  " auto-spectrum is complex valued in this",
                                  " case since the point lies off the diagonal",
                                  sep = ""),
                           no   = ""))
        kill(.parts)
    }

    ##  Add information about the coordinates, both as numbers and
    ##  as percentiles of the standard normal distribution.
    details$.point_coord <- look_up$.point_coord
    details$.point_coord_percentile <- pnorm(details$.point_coord)
    details$.selected_percentile <- look_up$.selected_percentile
    ##  Add the bandwidth, as a character.  Note: This is not
    ##  necessarily a number, it could also represent a
    ##  percentage.  Some care must thus be taken when the text
    ##  version is to be created.
    details$bandwidth <- look_up$bw_points
    ###-------------------------------------------------------------------
    ##  If a bootstrap is present: Extract 'boot_type', 'block_length'
    ##  and 'nb' (number of bootstrap replicates).
    if (look_up$is_bootstrap) {
        for (.arg in c("boot_type", "block_length", "nb"))
            details [[.arg]] <- look_up[[.arg]]
        kill(.arg)
    }
    ###-------------------------------------------------------------------
    ##  If a confidence interval is needed, find the relevant values.
    if (details$is_CI_needed) 
        details$CI_percentage <- local({
            .min_max <- stringr::str_detect(
                                     string = look_up$confidence_interval,
                                     pattern = "min")
            if (.min_max) {
                "min_max"
            } else
                as.numeric(look_up$confidence_interval)
        })
    ###-------------------------------------------------------------------
    ##  Specify if the plot shows correlations or a spectrum.
    details$content <- switch(
        EXPR = look_up$TCS_type,
        C = "correlations",
        S = "spectrum",
        T = "time series")
    ###-------------------------------------------------------------------
    ##  Register the original variable names.
    details$.original_variable_names <-
        .AB_env$details$.original_variable_names
    ###-------------------------------------------------------------------
    ######################################################################
    ##  Add text-information based on 'details_values' to simplify the
    ##  explanation of the plots that is investigated.
    ######################################################################    
    ###-------------------------------------------------------------------
    ##  Add information about the line-types and colours that is used
    ##  for the different configurations.
    details$text$local_colour <- "blue"
    details$text$global_colour <- "red"
    details$text$simulated_data_lty <- "dashed line"
    details$text$real_data_lty <- "line"
    ###-------------------------------------------------------------------
    ##   Add a description of the content.  This specifies first
    ##   auto/cross and correlations/spectrum, then it mentions
    ##   the point (bot has ordinary coordinates and percentiles).
    ##   If the plot is of correlations, additional information
    ##   will be added based on 'lag_zero_needed' and
    ##   'negative_lags_needed'.


    .basic <- sprintf(
        "A plot of the %s %s-%s",
        ifelse(test = details$is_local,
               yes  = "local Gaussian",
               no   = "ordinary"),
        details$auto_cross,
        details$content)
    .spectra <-
        if (details$content == "spectrum")
            sprintf(
                " (truncated at lag %s using the %s lag-window kernel for smoothing)",
                details$truncation_level,
                details$window)
    .spectra_local_global <-
        if (all(details$content == "spectrum",
                details$is_local)) {
            sprintf(" at the point (%s).  The coefficients of this point corresponds to the standard-normal percentiles %s.",
                    paste(details$.point_coord, collapse = ", "),
                    details$.selected_percentile)
        } else
            "."
    .explanations <-
        if (details$content == "correlations") {
            paste(c(if (! details$is_lag_zero_needed)
                        c("  Note that the lag zero component always is one ",
                          " in this case, and it has thus been dropped from the plot."),
                    if (all(! details$is_negative_lags_needed,
                            details$is_local))
                        c("  The local correlations are even in the lag-argument",
                          " in this case (since the point lies on the diagonal),",
                          " so only positive lags are shown in the plot.")),
                  collapse = "")
        } else {
            if (details$is_local) {
                paste(c("  The ",
                        details$text$global_colour,
                        " part represents the ordinary global ",
                        details$auto_cross,
                        "-spectrum (included for comparison)",
                        " whereas the ",
                        details$text$local_colour,
                        " part shows the spectra computed from the local Gaussian ",
                        details$auto_cross,
                        "-correlations.  Note that this spectrum is ",
                        ifelse(test = look_up$is_even_spectrum,
                               yes  = "even",
                               no   = "odd"),
                        " in the",
                        " frequency-argument."),
                      collapse = "")
            } else {
                ""
            }
        }
    details$text$content <- sprintf("%s%s%s%s",
                                    .basic,
                                    .spectra,
                                    .spectra_local_global,
                                    .explanations)
    
    ###-------------------------------------------------------------------
    ##  Add information about bandwidth and type of local Gaussian
    ##  approximation.  Add a warning if the heinous one-parameter local
    ##  Gaussian approximations are used.
    details$text$computations <- 
        if (details$is_local) 
            paste(
                c("The computations are based on the correlations obtained from",
                  " a local fitting, at the point (",
                  paste(details$.point_coord,
                        collapse = ", "),
                  "), of a ",
                  details$type,
                  "-parameter Gaussian approximation to the probability",
                  " density functions of lagged pairs of pseudo-normalised observations,",
                  " where the bandwidth ",
                  details$bandwidth,
                  " was used in the estimation algorithm (for all the lags).",
                  if (details$type == "one")
                      c("  **WARNING:**  The one-parameter local Gaussian",
                        " approximation are included as an option, but it will",
                        " in general fail to capture the local properties",
                        " of interest.  Use the five-parameter approach instead!")),
                collapse = "")
            
            
    #####  Reminder: This needs to be tweaked a bit to cover the cases
    #####  where the bandwidth is given as a percentage...  Trigger below
    #####  to ensure that these cases will be taken care of.
    if (is.na(as.numeric(details$bandwidth)))
        error(c("The value in ",
                sQuote("look_up$bw_points"),
                "is not a numerical value.",
                "Update code for extraction in this case!"))
    ###-------------------------------------------------------------------
    ##  Add CI-text (when relevant)
    if (details$is_CI_needed) {
        details$text$CI <- paste(
            if (details$CI_percentage == "min_max") {
                "Pointwise max and min values based on "
            } else
                paste(details$CI_percentage,
                      "\\% pointwise confidence interval based on ",
                      sep = ""),
            if (details$is_block) {
                paste(details$nr_simulated_samples,
                      " independent samples.",
                      sep = "")
            } else
                paste(details$nb,
                      " ",
                      details$boot_type,
                      "-bootstrap replicates, having block length ",
                      details$block_length,
                      ".",
                      sep = ""),
            sep = "")
    }
    ###-------------------------------------------------------------------
    ##  Add information about the type of the plot, that covers both
    ##  correlations and spectra.
    details$text$plot_type <-  paste(
        c(ifelse(test = details$is_local,
                 yes  = "local Gaussian ",
                 no   = "ordinary "),
          if (look_up$TCS_type == "C") {
              c(details$auto_cross,
                "-")
          } else {
              c(if (look_up$is_auto_pair)
                    switch(EXPR = details$spectrum_variant,
                           auto = "",
                           Co   = "co",
                           Quad = "quadrature ",
                           amplitude = "amplitude ",
                           phase = "phase ",
                           "  **NO DESCRIPTION** "))
          },
          details$content),
        collapse = "")

    
    details$text$plot_type_YiYj <- paste(
        if (look_up$is_auto_pair) {
            c(" of ",
              details$Vi)
        } else c(" between ",
                 details$Vi,
                 " and ",
                 details$Vj),
        collapse = "")


    ##  Add information about the percentile when a local
    ##  investigation is performed.
    details$.selected_percentile <-
        if (! look_up$is_global_only)
            paste(
                paste(
                    round(x = pnorm(look_up$.point_coord),
                          digits = 3) * 100,
                    "%",
                    sep = ""),
                collapse = " :: ")
    ##  A detail needed for the plots in the spectra-case.
    if (look_up$TCS_type == "S") {
        details$.selected_lag <- sprintf(
            "m = %s",
            as.numeric(look_up$m_selected))
    }
    ##  The label to be used for the plots.
    details$.plot_label  <-  paste(
        toupper(substr(x = details$text$plot_type,
                       start = 1,
                       stop = 1)),
        substr(x = details$text$plot_type,
               start = 2,
               stop = nchar(details$text$plot_type)),
        details$text$plot_type_YiYj,
        sep = "")
    ###-------------------------------------------------------------------
    ##  When necessary, investigate the status regarding the
    ##  problematic numerical convergence.  The first test reports the
    ##  existence of some problem for some of the tuning parameters
    ##  used in the investigation, but it might happen that the
    ##  problem is not present for all the tuning parameters.
    if (details$is_local) {
        if (details$type == "five") {
            if (isFALSE(details$convergence)) {
                ##  Check the given combination of tuning parameters.
                details$NC_fail_details <-
                    LG_lookup_details_NC_fail(look_up,
                                              .AB_env)
                ##  Update convergence-status based on this.
                details$convergence <- ! details$NC_fail_details$problem_present
            }
        }
    }
    ###-------------------------------------------------------------------
    ##  Add information about whether or not the numerical convergence
    ##  should be trusted, i.e. if 'eflag' was 0 when the
    ##  five-parameter local Gaussian approach was used.  If for some
    ##  reason the heinous one-parameter local Gaussian approach is
    ##  used, simply state that the result is worthless crap that
    ##  never should be used.
    details$text$trust_the_result <-
        if (details$is_local) {
            if (details$type == "one") {
                structure(
                    .Data = "Computations based on the heinous 1-parameter approach.  Use 5-parameter instead!",
                    short = "Warning: 1-parameter approach!")
            } else {
                ## Create the text to be used, with convergence status
                ## as an attribute, and a short-version as attribute
                ## in case the plots later on should be included in
                ## some grid-based setup.
                structure(
                    .Data = ifelse(test = isTRUE(details$convergence),
                                   yes  = "NC = OK (numerical convergence verified)",
                                   no   = "NC = FAIL (numerical convergence failed)"),
                    convergence = isTRUE(details$convergence),
                    short = ifelse(test = isTRUE(details$convergence),
                                   yes  = "NC = OK",
                                   no   = "NC = FAIL"))
            }
        }
    ##  Return the 'details'-list, so 'LG_lookup' can add it as a new
    ##  node to the 'look_up'-list
    details
}
