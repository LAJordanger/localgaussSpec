################################################################################
#'
#' Load files to be used during the interactive inspection.
#'
#' This helper function that takes care of some details related to the
#' loading of files, i.e. with regard to the interactive inspection
#' required by \code{shiny}.
#'
#' @param .look_up A list created by \code{LG_lookup} (in the function
#'     \code{LG_plot_helper}), where the key details have been
#'     extracted (or constructed) from a (non-reactive copy) of the
#'     values defined by the \code{shiny}-interface.
#'
#' @param .env The environment in which the loaded stuff should be
#'     stored.
#'
#' @return The required data will be loaded from files into
#'     \code{.env}, but only if it hasn't been done before.  The
#'     resulting array will then be assigned to the environment of the
#'     calling function under the name given by \code{.result}.
#'
#' @export


LG_plot_load <- function(.look_up,
                         .env) {
###-------------------------------------------------------------------
#####  Reminder: Some of tweaks in this function is not relevant for
#####  all the different values of '.selected'.
###-------------------------------------------------------------------
    ##  Extract the value '.selected' from 'look_up' to get a more
    ##  compact code later on.
    .selected <- .look_up$.selected
###-------------------------------------------------------------------
    ##  Find the directory containing the relevant files.
    new_data_dir <- paste(c(
        get(x = "main_dir",
            envir = .env),
        .look_up$path_all),
        collapse = .Platform$file.sep)
###-------------------------------------------------------------------
    ##  Find the array-nodes of interest for the present investigation
    .array_nodes_local <- LG_default$result$array_nodes
    .array_nodes_global <- LG_default$result_orig$array_nodes
###-------------------------------------------------------------------
    ##  Find the arguments to be used by 'LG_plot'.  Reminder: I would
    ##  have liked these details to be performed later on, but as they
    ##  depend on the part above (that might be revised later on),
    ##  it's nevertheless placed here.  There's also an issue with
    ##  regard to whether there should be an update of the interface
    ##  for the block-case, in order to select between "median",
    ##  "block_median" and "block_mean".  For the later alternatives I
    ##  need to take into account that the global case need an
    ##  adjusted version of '.aes_xy' in order to work properly.

#####  This should now rather be taken care of in 'LG_lookup'
    .aes_xy <- if (any(.look_up$details$block,
                      .look_up$details$boot)) {
                   aes(x = lag,
                       y = value)
               } else
                   eval(bquote(
                       aes(x = lag,
                           y = .(as.symbol(LG_default$sample.prefix)))))
    ##  Reminder: 'eval' is needed in order for 'LG_plot' to work.
    if (.selected %in% c("Spectra", "Boot_Spectra")) {
        .aes_xy <-
            if (.look_up$.block) {
                aes(x = omega,
                    y = median)
            } else
                eval(bquote(
                    aes(x = omega,
                        y = .(as.symbol(LG_default$sample.prefix)))))
        ##  Reminder: 'eval' is needed in order for 'LG_plot' to work.
        .aes_min_max <-
            if (.selected == "Boot_Spectra")
                local({
                    .content <- .look_up$confidence_interval_local
                    ##  Identify the limits of the confidence interval.
                    .aes_ymin <- as.symbol(
                        .content[
                            str_detect(string = .content,
                                       pattern = "low|min")])
                    .aes_ymax <- as.symbol(
                        .content[
                            str_detect(string = .content,
                                       pattern = "high|max")])
                    ##  Create the desired result.
                    eval(bquote(
                        aes(ymin = .(.aes_ymin),
                            ymax = .(.aes_ymax))))
                })
    }
###-------------------------------------------------------------------
    ##  Extract the pieces from '.look_up' that will be used to decide
    ##  which parts that must be restricted in the different steps,
    ##  this collects the part of '.look_up' that will be used for the
    ##  restriction.  A comparison will be made against previously
    ##  stored versions, and then a logical vector will be created to
    ##  decide which parts that has to be done.

#    capture_env() 
    
    if  (.selected %in% c("Approx", "Boot_Approx")) {
        ##  Create restriction-lists that describes how the global and
        ##  local data should be treated.  Note: The solution is a bit
        ##  mezzy due to ad hoc solutions inherited from the initial
        ##  part of the code.  Reminder: These restriction lists are
        ##  adjusted later on when information about the lag is
        ##  available.
        .LGC_restrict_global <- list(
            second = c(TS = ifelse(
                           test = .selected == "Approx",
                           yes  = "TS_for_analysis",
                           no   = "TS_original")),
                       #####  Reminder: The code above is the result
                       #####  of ad hoc solutions earlier on...
            third = list(
                pairs = unique(c(.look_up$pairs_ViVj, .look_up$pairs_VjVi))))
        .LGC_restrict_local <- list(
            first = c(
                point_type = .look_up$point_type),
            second = c(
                bw_points = .look_up$bw_points),
            third = list(
                pairs = unique(c(.look_up$pairs_ViVj, .look_up$pairs_VjVi))),
            fourth = list(
                levels = unique(c(.look_up$levels_point, .look_up$levels_point_reflected))),
            fifth = c(
                global_local = .look_up$global_local))
###-------------------------------------------------------------------
        ##  Define the names to be used for the local and global arrays.
        .global_name <- ".LGC_list_array_global"
        .local_name <- ".LGC_list_array_local"
        ##  And define derived names for the intermediate parts.
        .derived_global <- structure(
            .Data = paste(.global_name,
                          c(seq_along(.LGC_restrict_local),
                            "dimnames"),
                          sep = "_"),
            .Names = c(names(.LGC_restrict_local),
                       "dimnames"))
        .derived_local <- structure(
            .Data = paste(.local_name,
                          c(seq_along(.LGC_restrict_local),
                            "dimnames"),  
                          sep = "_"),
            .Names = c(names(.LGC_restrict_local),
                       "dimnames"))
###-------------------------------------------------------------------
        ##  To load or not to load?  Check if existing data has been
        ##  loaded already, if necessary load the date from files.  Check
        ##  which of the restrictions that are necessary.  Strategy:
        ##  Create a new storage environment (inside of '.env'), with an
        ##  attribute that reveals if new data must be loaded.
        .env_name <- "plot_data_lag"
        .env_exists <- identical(
            x = attributes(.env[[.env_name]])$.source,
            y = c(.look_up$.bookmark, .look_up$type))
        ##  Create the environment if it doesn't already exist, and add
        ##  the required ingredients to it, otherwise compare the old and
        ##  new '.restriction_sequence' and compute the logical vector to
        ##  be used when deciding which parts that should be redone.
        if (! .env_exists) {
            .env[[.env_name]] <- structure(
                .Data = new.env(),
                .source = c(.look_up$.bookmark, .look_up$type))
            ##  Load the global approx-file into the environment.
            .global_file <- file.path(
                new_data_dir,
                .look_up$.global_file,
                fsep = .Platform$file.sep)
            ##  Load the required list-array into the environment.
            LG_load( 
                .file = .global_file,
                .env = .env[[.env_name]],
                .name = .global_name)
            kill(.global_file)
###-----------------------------------------------
            ##  Load the local file(s) into the environment, i.e. find the
            ##  values needed for the 'file_list'-argument of
            ##  'array_from_files'
#####  Reminder: This setup might crash if the information of interest
#####  has been split into several files...
            ##  Find the path to the file having the data of interest.
            .approx_path <- file.path(
                new_data_dir,
                .look_up$approx_file,
                fsep = .Platform$file.sep)
            ##  Load the object into the desired environment under the
            ##  selected name.
            LG_load(.file = .approx_path,
                    .env = .env[[.env_name]],
                    .name = .local_name)
            kill(.approx_path)
###-----------------------------------------------
            ##  Adjust global and local values to list-array format.
            .env[[.env_name]][[.global_name]] <- array_to_list_array(
                .arr = .env[[.env_name]][[.global_name]],
                .array_nodes = c("TS"))
            .env[[.env_name]][[.local_name]] <- array_to_list_array(
                .arr = .env[[.env_name]][[.local_name]],
                .array_nodes = c("bw_points"))
###-------------------------------------------------------------------
            ##  Add the restriction information to the environment.
            .env[[.env_name]]$.LGC_restrict_global <- .LGC_restrict_global
            .env[[.env_name]]$.LGC_restrict_local <- .LGC_restrict_local
            ##  Initiate "do them all" indicator
            .done_before <- structure(
                .Data = rep(x = FALSE,
                            length.out = length(.LGC_restrict_local)),
                .Names = names(.LGC_restrict_local))
        } else {
            ##  Compute which parts that must be redone.
            .done_before <- structure(
                .Data = vapply(
                    X = seq_along(.env[[.env_name]]$.LGC_restrict_local),
                    FUN = function(i) {
                        identical(
                            x = .env[[.env_name]]$.LGC_restrict_local[i],
                            y = .LGC_restrict_local[i])
                    },
                    FUN.VALUE = logical(1)),
                .Names = names(.LGC_restrict_local))
            ##  To ensure that all updates are properly performed,
            ##  everything after the first case of 'FALSE' in
            ##  '.done_before' must also be set to 'FALSE'.
            .done_before[] <- as.logical(cumprod(.done_before))
            ##  Update the value stored in the environment.
            .env[[.env_name]]$.LGC_restrict_global <- .LGC_restrict_global
            .env[[.env_name]]$.LGC_restrict_local <- .LGC_restrict_local
        }
        kill(.env_exists)
        ##  To make the code later on more compact, create a pointer to
        ##  the environment that we want to update.
        ..env <- .env[[.env_name]]
        kill(.env, .env_name)
###-------------------------------------------------------------------
        ##  Add '.aes_xy'  to the environment.
        ..env$.aes_xy <- .aes_xy
        kill(.aes_xy)
        ##  Add the value of '.selected_percentile' to '..env'
        ..env$.selected_percentile <-
            .look_up$.selected_percentile
        ##  Add the title, with to the '..env'
        ..env$.LGC_title <- paste(
            toupper(substr(x = .look_up$details$text$plot_type,
                           start = 1,
                           stop = 1)),
            substr(x = .look_up$details$text$plot_type,
                   start = 2,
                   stop = nchar(.look_up$details$text$plot_type)),
            .look_up$details$text$plot_type_YiYj,
            sep = "")
###-------------------------------------------------------------------
        ##  Perform stepwise extractions/restrictions based on
        ##  '.done_before', starting with extractiong details from the
        ##  main-branch of the loaded list-array.
        if (! .done_before["first"]) {
            ##  Create pointers to the previous step (nothing copied yet).
            ..env[[.derived_global["first"]]] <- ..env[[.global_name]]
            ..env[[.derived_local["first"]]] <- ..env[[.local_name]]
            ##  Create/update the stored dimension-names for the
            ##  present main-branch (based on selected values).
            ..env[[.derived_global["dimnames"]]] <- local({
                .original_dimnames <- attributes(
                    ..env[[.derived_global["first"]]])$original_dimnames
                .sub_dim <- attributes(
                    ..env[[.derived_global["first"]]])$.sub_dim
                .original_dimnames[.sub_dim]
            })
            ..env[[.derived_local["dimnames"]]] <- local({
                .bm <- .LGC_restrict_local$first
                .original_dimnames <- attributes(
                    ..env[[.derived_local["first"]]][[.bm]])$original_dimnames
                .sub_dim <- attributes(
                    ..env[[.derived_local["first"]]][[.bm]])$.sub_dim
                .original_dimnames[.sub_dim]
            })
            ##  Register all the lags and those that are non-negative,
            ##  this is needed for the unfolding later on.
            ..env$non_negative_lags <- 
                list(lag = ..env[[.derived_global["dimnames"]]]$lag)
            ..env$pos_lags <- 
                list(lag = setdiff(x = ..env[[.derived_global["dimnames"]]]$lag,
                                   y = "0"))
        }
        kill(.global_name, .local_name)
###-------------------------------------------------------------------
        ##  Extract the local and global arrays of interest for the
        ##  present configuration.
        if (! .done_before["second"]) {
            ..env[[.derived_global["second"]]] <- local({
                .bm <- .LGC_restrict_global$second
                .tmp <- ..env[[.derived_global["first"]]][[.bm]]
                ##  Add dimension-names and return.
                dimnames(.tmp) <- ..env[[.derived_global["dimnames"]]]
                .tmp
            })
            ..env[[.derived_local["second"]]] <- local({
                .bm <- unlist(c(.LGC_restrict_local$first,
                                .LGC_restrict_local$second))
                .tmp <- ..env[[.derived_local["first"]]][[.bm]]
                ##  Add dimension-names.
                dimnames(.tmp) <- ..env[[.derived_local["dimnames"]]]
                .tmp
            })
        }
###-------------------------------------------------------------------
        ##  Restrict the attention to the pairs of interest.  NOTE:
        ##  This must take into account that an "unfolding" of the
        ##  stored data might be necessary.  Reminder: Do not drop any
        ##  dimensions at this stage, need the full set for the
        ##  unfolding in the next step.
        if (! .done_before["third"]) {
            ##  Adjust the restrictions if the lag zero component also
            ##  can be dropped (for those cases where it is constant
            ##  and equal to one.)
            if (! .look_up$lag_zero_needed) {
                .LGC_restrict_global$third <- c(
                    .LGC_restrict_global$third,
                    ..env$pos_lags)
                .LGC_restrict_local$third <- c(
                    .LGC_restrict_local$third,
                    ..env$pos_lags)
            }
            ..env[[.derived_global["third"]]] <- restrict_array(
                .arr = ..env[[.derived_global["second"]]],
                .restrict = .LGC_restrict_global$third)
            ..env[[.derived_local["third"]]] <- restrict_array(
                .arr = ..env[[.derived_local["second"]]],
                .restrict = .LGC_restrict_local$third)
            ##  Compute the ylimit to be used for these values (at the
            ##  moment restricted to the local case, since the global
            ##  part has not yet been included in the plot).
            ..env$.LGC_ylim <- range(0, ..env[[.derived_local["third"]]])
        }
###-------------------------------------------------------------------
        ##  Restrict the attention to the point(s) of interest (for
        ##  the local Gaussian correlations), perform the required
        ##  unfolding, and convert to a data-frame.
        if (! .done_before["fourth"]) {
            ..env[[.derived_global["fourth"]]] <- ..env[[.derived_global["third"]]]
            ##  Create the restriction lists to be used in the
            ##  unfolding part.  Note: All combinations of the pairs
            ##  are included, which simplifies the situation since it
            ##  now will be sufficient to focus upon the levels.
            .restrict_pos_lags <- c(
                list(pairs = .look_up$pairs_ViVj),
                if (.look_up$lag_zero_needed) {
                    ..env$non_negative_lags
                } else
                    ..env$pos_lags)
            .restrict_neg_lags <-
                if (.look_up$negative_lags_needed)
                    c(list(pairs = .look_up$pairs_VjVi),
                      ..env$pos_lags)
            ##  Perform the restrictions, and convert the results into
            ##  the desired data-frames.  Note that the negative lags
            ##  will be ignored in the plots when symmetry ensures
            ##  that they are equal to the positive lags.
            pos_lags <- restrict_array(
                .arr = ..env[[.derived_global["fourth"]]],
                .restrict = .restrict_pos_lags,
                .drop = TRUE,
                .never_drop = c("lag", "content"),
                .keep_attributes = FALSE)
            ##  Extract the negative lags too, when necessary (the
            ##  result becomes 'NULL' when requirements not
            ##  satisfied).
            negative_lags <- 
                if  (.look_up$negative_lags_needed) 
                    local({
                        ##  Need to modify the lag-values.
                        .tmp <- restrict_array(
                            .arr = ..env[[.derived_global["fourth"]]],
                            .restrict = .restrict_neg_lags,
                            .drop = TRUE,
                            .never_drop = c("lag", "content", "variable"),
                            .keep_attributes = FALSE)
                        dimnames(.tmp)$lag <- paste(
                                          "-",
                                          dimnames(.tmp)$lag,
                                          sep = "")
                        .tmp
                    })
            ##  Collect these pieces into '..env'
            ..env[[.derived_global["fourth"]]] <- my_abind(
                pos_lags,
                negative_lags)

            #####  Similar code for the local case
            ..env[[.derived_local["fourth"]]] <- restrict_array(
                .arr = ..env[[.derived_local["third"]]],
                .restrict = .LGC_restrict_local$fourth)
            ##  Extend the restriction lists to include 'levels'.
            .restrict_pos_lags <- c(
                .restrict_pos_lags,
                list(levels = .look_up$levels_point_reflected))
            .restrict_neg_lags <-
                if (.look_up$negative_lags_needed)
                    c(.restrict_neg_lags,
                      list(levels = .look_up$levels_point))
            ##  Perform the restrictions, and convert the results into
            ##  the desired data-frames.  Note that the negative lags
            ##  will be ignored in the plots when symmetry ensures
            ##  that they are equal to the positive lags.
            pos_lags <- restrict_array(
                .arr = ..env[[.derived_local["fourth"]]],
                .restrict = .restrict_pos_lags,
                .drop = TRUE,
                .never_drop = c("lag", "content", "variable"),
                .keep_attributes = FALSE)
            ##  Extract the negative lags too, when necessary (the
            ##  result becomes 'NULL' when requirements not
            ##  satisfied).
            negative_lags <- 
                if  (.look_up$negative_lags_needed) 
                    local({
                        ##  Need to modify the lag-values.
                        .tmp <- restrict_array(
                            .arr = ..env[[.derived_local["fourth"]]],
                            .restrict = .restrict_neg_lags,
                            .drop = TRUE,
                            .never_drop = c("lag", "content", "variable"),
                            .keep_attributes = FALSE)
                        dimnames(.tmp)$lag <- paste(
                                          "-",
                                          dimnames(.tmp)$lag,
                                          sep = "")
                        .tmp
                    })
            ##  Collect these pieces into '..env'
            ..env[[.derived_local["fourth"]]] <- my_abind(
                pos_lags,
                negative_lags)
            ##  Reminder: The object above contains 'eflag' (when it
            ##  comes from a proper five-parameter local Gaussian
            ##  approximation), and this reveals if the numerical
            ##  convergence was successful during the computation.
            ##  This should in some way be used in the plot to
            ##  indicate that the estimates hopefully might not be
            ##  completely of the mark.  However, for the time being
            ##  this is not implemented...
        }

###-------------------------------------------------------------------
        ##  Select the part to show, in particular if it should be the
        ##  global part or the local part.
        if (! .done_before["fifth"]) {
            ##  Create a selector.
            .use_global <- any({.look_up$global_local == "global"})
            .use_part <- ifelse(
                test = .use_global,
                yes  = .derived_global["fourth"],
                no   = .derived_local["fourth"])
            ##  Identify the relevant part.
            .lag_values_only <-
                if (.use_global) {
                    ..env[[.derived_global["fourth"]]]
                } else
                    restrict_array(
                        .arr =  ..env[[.derived_local["fourth"]]],
                        .restrict = list(variable = "rho"),
                        .drop = TRUE,
                        .never_drop = c("content", "lag"))
            ..env$.lag_data_frame <- "lag_data_frame"
            ..env[[..env$.lag_data_frame]] <- local({
                .tmp <- reshape2::melt(data = .lag_values_only)
                ##  Identify if a box-plot is desired.
                .boxplot <- any(.look_up$details$block,
                                .look_up$details$boot )
                if (! .boxplot) {
                    .formula <- quote(lag ~ content)
                    .tmp <- reshape2::dcast(
                                          data = .tmp,
                                          formula = eval(.formula))
                }
                attributes(.tmp)$lag_data <- TRUE
                attributes(.tmp)$boxplot <- .boxplot
                .tmp
            })
            ##  Add the value of '.selected_percentile'
            ..env$.selected_percentile <-
                .look_up$.selected_percentile
            ##  Find the x-limit to be used in the plot
            ..env$.LGC_xlim <- range(c(0, ..env[[..env$.lag_data_frame]]$lag))
        }
###-------------------------------------------------------------------
        ##  Create and return the plot
        ..env$.lag_plot <- LG_plot(
            .data = ..env[[..env$.lag_data_frame]],
            .lag = NULL,
            .percentile = ..env$.selected_percentile,
            .xlim = ..env$.LGC_xlim,
            .ylim = ..env$.LGC_ylim,
            .aes_xy = ..env$.aes_xy,
            .sanity_checks = FALSE)
        ##  Add title and 'trustworthiness'.
        ..env$.lag_plot <- ..env$.lag_plot +
            ggplot2::ggtitle(label = ..env$.LGC_title) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
            annotate(geom = "text",
                     x = -Inf,
                     y = -Inf,
                     size = 4,
                     label = .look_up$details$text$trust_the_result,
                     col = "darkgreen",
                     ## col = "darkred",
                     vjust = -.5,
                     hjust = -0.1)
        names(..env$.lag_plot$layers) <- c(
            head(x = names(..env$.lag_plot$layers), n = -1),
            ".annotate_convergence")
        ##  Add the details as an attribute.
        attributes(..env$.lag_plot)$details <- .look_up$details
        ##   Return the plot to the workflow.
        return(..env$.lag_plot)
    }
###-------------------------------------------------------------------
######################################################################
    ##  A helper function to nudge the ranges
    .range_adjust <- function(.range) {
        .range + c(-1, 1) * diff(.range) /250
    }
######################################################################
###-------------------------------------------------------------------
    if (.selected %in% c("Spectra", "Boot_Spectra")) {
        ##  Create restriction-lists that describes how the global
        ##  and local data should be treated.  NOTE: There are
        ##  some cases where no natural matching global concept
        ##  exists for the local Gaussian concept, i.e. for the
        ##  compination "auto" + "off_diag".  The logical value
        ##  '.look_up$matching_global' will be used to enable the
        ##  code to deal with those cases.  The present strategy
        ##  is to load the ordinary global autospectra in this
        ##  case (even though it might not be used later on), and
        ##  then tweak the arguments of the plot-function for
        ##  these "corner"-cases.
        .LGS_restrict_global <- list(
            first = c(
                auto_cross = ifelse(
                    test = .look_up$.auto_pair,
                    yes  = "auto",
                    no   = "cross"),
                spectrum_variant = ifelse(
                    test = .look_up$matching_global,
                    yes  = switch(
                        EXPR = .look_up$spectrum_variant,
                        auto = "Re",
                        co   = "Re",
                        quad = "negIm",
                        .look_up$spectrum_variant),
                    no   = "Re")),
            second = c(list(
                window = .look_up$window,
                pairs = ifelse(
                    test = .look_up$.auto_pair,
                    yes  = .look_up$pairs_ViVj,
                    no   = .look_up$pairs$cross),
                spec = "spec"),
                if (.selected == "Boot_Spectra")
                    list(content = .look_up$confidence_interval_global)),
            third = list(
                omega = do.call(
                    what = ":",
                    args = as.list(.look_up$omega_range))),
            fourth = c(cut = .look_up$cut),
            fifth = list(Y_range = .look_up$Y_range))
        ##  Redefine '.look_up$matching_global' to enable global
        ##  data to be included for some of the plots
        if (! .look_up$matching_global)
            if (.look_up$spectrum_variant %in% c("co", "amplitude"))
                .look_up$matching_global <- TRUE
        ##  Or redefine it to turn plot of in some particular cases.
        if (! is.null(.look_up$spectrum_variant))
            if (.look_up$spectrum_variant == "squared_coherence")
                if (! is.null(.look_up$od_c_a_sqc))
                    if (! .look_up$od_c_a_sqc %in% c("Re", "amplitude"))
                        .look_up$matching_global <- FALSE
###-------------------------------------------------------------------
        ##  Give the restrictions for the local case.
        .LGS_restrict_local <- list(
            first = c(
                point_type = .look_up$point_type,
                auto_cross = ifelse(
                    test = .look_up$.auto_pair,
                    yes  = "auto",
                    no   = "cross"),
                spectrum_variant = switch(
                    EXPR = .look_up$spectrum_variant,
                    auto = "Re",
                    co   = "Re",
                    quad = "negIm",
                    .look_up$spectrum_variant)),
            second = c(list(
                type = .look_up$type,
                window = .look_up$window,
                pairs = ifelse(
                    test = .look_up$.auto_pair,
                    yes  = .look_up$pairs_ViVj,
                    no   = .look_up$pairs$cross),
                bw_points = .look_up$bw_points,
                spec = if (.look_up$.od_c_a_sqc) {
                           .look_up$od_c_a_sqc
                       } else
                           "spec"),
                if (.selected == "Boot_Spectra")
                    list(content = .look_up$confidence_interval_local)),
            third = list(
                omega = do.call(
                    what = ":",
                    args = as.list(.look_up$omega_range))),
            third_to_fourth  = list(
                levels = ifelse(
                    test = .look_up$adjust_result,
                    yes  = .look_up$levels_point_reflected,
                    no   = .look_up$levels_point)),
            fourth = c(cut = .look_up$cut),
            fifth = list(Y_range = .look_up$Y_range))
###-------------------------------------------------------------------
        ##  Define the names to be used for the local and global arrays.
        .global_name <- ".LGS_list_array_global"
        .local_name <- ".LGS_list_array_local"
        ##  And the derived names to be used for the intermediate
        ##  parts, one fore each restriction-step and one for the
        ##  dimension-names that is present at the nodes of the
        ##  list-array.
        .derived_global <- structure(
            .Data = paste(.global_name,
                          c(seq_along(.LGS_restrict_local),
                            "dimnames",
                            "df"),
                          sep = "_"),
            .Names = c(names(.LGS_restrict_local),
                       "dimnames",
                       "df"))
        .derived_local <- structure(
            .Data = paste(.local_name,
                          c(seq_along(.LGS_restrict_local),
                            "dimnames",
                            "df"),
                          sep = "_"),
            .Names = c(names(.LGS_restrict_local),
                       "dimnames",
                       "df"))
###-------------------------------------------------------------------
        ##  To load or not to load?  Check if existing data has
        ##  been loaded already, if necessary load the date from
        ##  files.  Check which of the restrictions that are
        ##  necessary.  Strategy: Create a new storage environment
        ##  (inside of '.env'), with an attribute that reveals if
        ##  new data must be loaded.
        .env_name <- "plot_data_spectra"
        .env_exists <- identical(
            x = attributes(.env[[.env_name]])$.source,
            y = c(.look_up$.bookmark, .look_up$type))
###-------------------------------------------------------------------
        ##  Create the environment if it doesn't already exist,
        ##  and add the required ingredients to it, otherwise
        ##  compare the old and new '.LGS_restrict_local' and compute
        ##  the logical vector to be used when deciding which
        ##  parts that should be redone.
        if (! .env_exists) {
            .env[[.env_name]] <- structure(
                .Data = new.env(),
                .source = c(.look_up$.bookmark, .look_up$type))
###-----------------------------------------------
            ##  Load the global file into the environment.
            .global_file <- file.path(
                new_data_dir,
                .look_up$.global_file,
                fsep = .Platform$file.sep)
            ##  Load the required list-array into the environment.
            LG_load( 
                .file = .global_file,
                .env = .env[[.env_name]],
                .name = .global_name)
            ## ## ## kill(.global_file)
###-----------------------------------------------
            ##  Load the local file(s) into the environment, i.e. find the
            ##  values needed for the 'file_list'-argument of
            ##  'array_from_files'
            ##  TASK: The following might be simplified, but I don't have
            ##  time to investigative that now.
            ## ## ## ## .env[[.env_name]][[.local_name]] <- local({
            ## ## ## ##     .restrict_list = list()
            ## ## ## ##     threshold <- 500
            ## ## ## ##     array_from_files(
            ## ## ## ##         data_dir = new_data_dir,
            ## ## ## ##         file_list = .look_up$.file_list,
            ## ## ## ##         .restrict_list = .restrict_list,
            ## ## ## ##         .class = LG_default$class$array,
            ## ## ## ##         threshold = threshold)
            ## ## ## ## })
#####  Simplified code assuming only one file...  God enough for the
#####  testing in this part, I hope.
            LG_load( 
                .file = file.path(
                    new_data_dir,
                    .look_up$.file_list$file_names),
                .env = .env[[.env_name]],
                .name = .local_name)
################################################################################
#####  2017-04-19: REMINDER
            ##  Code to be used in an intermediate phase, as
            ##  previously encountered files might not be stored in
            ##  the desired format yet.  Later on this should be
            ##  possible to simplify a bit.  The idea is that an
            ##  additional class-attribute should be added to the
            ##  joined data, and that the conversion only should be
            ##  needed when that class is missing.  If it is missing,
            ##  the conversion must be done here, and the revised
            ##  result should be saved back to file.


            ## capture_env() 

            ## ## ## ##  Convert back to the original setup, and adjust the
            ## ## ## ##  creation of the subsetting of the arrays.

            

            ## ## ## ## ## .env[[.env_name]][[.local_name]] <- 
            ## ## ## ## ##     list_array_to_array(.list_arr = .env[[.env_name]][[.local_name]])

            ## ## ## .env[[.env_name]][[.global_name]] <- 
            ## ## ##     list_array_to_array(.list_arr = .env[[.env_name]][[.global_name]])

            
            
            
            if (! "LG_joined" %in% class(.env[[.env_name]][[.local_name]])) {
                .env[[.env_name]][[.local_name]] <- array_to_list_array(
                    .arr = .env[[.env_name]][[.local_name]],
                    .array_nodes = c("type", "window", "pairs", "bw_points",
                                     "spec", "content"))
                class(.env[[.env_name]][[.local_name]]) <- c(
                    "LG_joined",
                    class(.env[[.env_name]][[.local_name]]))
                temp_local_joined <- .env[[.env_name]][[.local_name]]
                save(temp_local_joined,
                     file = file.path(
                         new_data_dir,
                         .look_up$.file_list$file_names))
                kill(temp_local_joined)
            }
##################################################            
            if (! "LG_joined" %in% class(.env[[.env_name]][[.global_name]])) {
                .env[[.env_name]][[.global_name]] <- array_to_list_array(
                    .arr = .env[[.env_name]][[.global_name]],
                    .array_nodes = c("window", "pairs", "spec", "content"))
                class(.env[[.env_name]][[.global_name]]) <- c(
                    "LG_joined",
                    class(.env[[.env_name]][[.global_name]]))
                temp_global_joined <- .env[[.env_name]][[.global_name]]
                save(temp_global_joined,
                     file = .global_file)
                kill(temp_global_joined)
            }

###-------------------------------------------------------------------
            ##  Add '.LGS_restrict_local' to the environment.
            .env[[.env_name]]$.LGS_restrict_local <- .LGS_restrict_local
            ##  Initiate "do them all" indicator
            .done_before <- structure(
                .Data = rep(x = FALSE,
                            length.out = length(.LGS_restrict_local)),
                .Names = names(.LGS_restrict_local))
        } else {
            ##  Compute which parts that must be redone.
            .done_before <- structure(
                .Data = vapply(
                    X = seq_along(.env[[.env_name]]$.LGS_restrict_local),
                    FUN = function(i) {
                        identical(
                            x = .env[[.env_name]]$.LGS_restrict_local[i],
                            y = .LGS_restrict_local[i])
                    },
                    FUN.VALUE = logical(1)),
                .Names = names(.LGS_restrict_local))
            ##  To ensure that all updates are properly performed,
            ##  everything after the first case of 'FALSE' in
            ##  '.done_before' must also be set to 'FALSE'.
            .done_before[] <- as.logical(cumprod(.done_before))
            ##  Update the value stored in the environment.
            .env[[.env_name]]$.LGS_restrict_local <- .LGS_restrict_local
        }
        kill(.env_exists, new_data_dir)
        ##  To make the code later on more compact, create a pointer to
        ##  the environment that we want to update.
        ..env <- .env[[.env_name]]
        kill(.env, .env_name)
###-------------------------------------------------------------------
        ##  Add '.aes_xy' and '.aes_min_max' to the environment.
        ..env$.aes_xy <- .aes_xy
        ..env$.aes_min_max <- .aes_min_max
        ..env$.selected_lag <- .look_up$.selected_lag
        ..env$.selected_percentile <- .look_up$.selected_percentile
        kill(.aes_xy, .aes_min_max)
        ##  Add the title, with to the '..env'
        ..env$.LGS_title <- paste(
            toupper(substr(x = .look_up$details$text$plot_type,
                           start = 1,
                           stop = 1)),
            substr(x = .look_up$details$text$plot_type,
                   start = 2,
                   stop = nchar(.look_up$details$text$plot_type)),
            .look_up$details$text$plot_type_YiYj,
            sep = "")
###-------------------------------------------------------------------
        ##  Perform stepwise extractions/restrictions based on
        ##  '.done_before', starting with extractiong details from
        ##  the main-branch of the loaded list-array.
        if (! .done_before["first"]) {
            ##  Create pointers to the previous step (nothing copied yet).
            ..env[[.derived_global["first"]]] <- ..env[[.global_name]]
            ..env[[.derived_local["first"]]] <- ..env[[.local_name]]
            ##  Create/update the stored dimension-names for the
            ##  present main-branch (based on selected values).
            ..env[[.derived_global["dimnames"]]] <- local({
                .bm <- .LGS_restrict_global$first
                .original_dimnames <- attributes(
                    ..env[[.derived_global["first"]]][[.bm]])$original_dimnames
                .sub_dim <- attributes(
                    ..env[[.derived_global["first"]]][[.bm]])$.sub_dim
                .original_dimnames[.sub_dim]
            })
            ..env[[.derived_local["dimnames"]]] <- local({
                .bm <- .LGS_restrict_local$first
                .original_dimnames <- attributes(
                    ..env[[.derived_local["first"]]][[.bm]])$original_dimnames
                .sub_dim <- attributes(
                    ..env[[.derived_local["first"]]][[.bm]])$.sub_dim
                .original_dimnames[.sub_dim]
            })
        }
        kill(.global_name, .local_name)
###-------------------------------------------------------------------
        ##  Extract the local and global arrays of interest for
        ##  the present configuration.  Reminder: Several
        ##  components can be present in the 'content'-part, these
        ##  must be extracted individually and merged together.
        if (! .done_before["second"]) {
            ..env[[.derived_global["second"]]] <-
                if (.selected == "Boot_Spectra") {
                    local({
                        .bm <- c(.LGS_restrict_global$first,
                                 unlist(head(x = .LGS_restrict_global$second, n = -1)))
                        .content <- .LGS_restrict_global$second$content
                        .tmp <- ..env[[.derived_global["first"]]][[.bm]][.content]
                        .class <- class(.tmp[[1]])
                        ##  Add dimensions for the array-nodes.
                        for (.node in seq_along(.tmp))
                            .tmp[[.node]] <- structure(
                                .Data = .tmp[[.node]],
                                .Dim  = c(1, dim(.tmp[[.node]])),
                                .Dimnames = c(
                                    list(content = names(.tmp)[.node]),
                                    ..env[[.derived_global["dimnames"]]]),
                                class = .class)
                        ##  Restrict the attention of the global case to
                        ##  the relevant 'TS'-part (if the user interface
                        ##  is extended to include a select 'original' or
                        ##  'normalised' version, then an extra step must
                        ##  be added.)
                        .restrict_TS <- list(
                            TS = ifelse(
                                test = length(dimnames(.tmp[[1]])$TS) > 1,
                                yes  = "TS_for_analysis",
                                no   = dimnames(.tmp[[1]])$TS))
                        for (.node in seq_along(.tmp))
                            .tmp[[.node]] <- restrict_array(
                                .arr = .tmp[[.node]],
                                .restrict = .restrict_TS,
                                .drop = TRUE,
                                .never_drop = c("content", "cut", "omega"))
                        ##  Collect into one array, addust and return result.
                        .tmp <- do.call(what = "my_abind", args = .tmp)
                        class(.tmp) <- .class
                        .tmp
                    })
                } else
                    local({
                        .bm <- c(.LGS_restrict_global$first,
                                 unlist(head(x = .LGS_restrict_global$second, n = -1)),
                                 "spec",
                                 "orig")
                        .tmp <- ..env[[.derived_global["first"]]][[.bm]]
                        ##  Adjust so the desired dimension-names are present.
                        .tmp <- structure(
                            .Data = .tmp,
                            .Dim = c(1, dim(.tmp)),
                            .Dimnames = c(
                                list(content = "orig"),
                                ..env[[.derived_global["dimnames"]]]),
                            class = class(.tmp))
                        ##  Restrict the attention of the global case to
                        ##  the relevant 'TS'-part (if the user interface
                        ##  is extended to include a select 'original' or
                        ##  'normalised' version, then an extra step must
                        ##  be added.)
                        .restrict_TS <- list(
                            TS = ifelse(
                                test = length(dimnames(.tmp)$TS) > 1,
                                yes  = "TS_for_analysis",
                                no   = dimnames(.tmp)$TS))
                        ##  Return the restricted array.
                        restrict_array(
                            .arr = .tmp,
                            .restrict = .restrict_TS,
                            .drop = TRUE,
                            .never_drop = c("content", "cut", "omega"))
                    })
            ..env[[.derived_local["second"]]] <-
                if (.selected == "Boot_Spectra"){
                    local({
                        .bm <- c(.LGS_restrict_local$first,
                                 unlist(head(x = .LGS_restrict_local$second, n = -1)))
                        .content <- .LGS_restrict_local$second$content
                        .tmp <- ..env[[.derived_local["first"]]][[.bm]][.content]
                        .class <- class(.tmp[[1]])
                        ##  Add dimensions for the array-nodes.
                        ##  Add dimensions for the array-nodes.
                        for (.node in seq_along(.tmp))
                            .tmp[[.node]] <- structure(
                                .Data = .tmp[[.node]],
                                .Dim  = c(1, dim(.tmp[[.node]])),
                                .Dimnames = c(
                                    list(content = names(.tmp)[.node]),
                                    ..env[[.derived_local["dimnames"]]]),
                                class = .class)
                        ##  Collect into one array and return result.
                        .tmp <- do.call(what = "my_abind", args = .tmp)
                        class(.tmp) <- .class
                        .tmp
                    })
                } else
                    local({
                        .bm <- c(.LGS_restrict_local$first,
                                 unlist(head(x = .LGS_restrict_local$second, n = -1)),
                                 "spec",
                                 "orig")
                        .tmp <- ..env[[.derived_local["first"]]][[.bm]]
                        ##  Adjust so the desired dimension-names are present.
                        structure(
                            .Data = .tmp,
                            .Dim = c(1, dim(.tmp)),
                            .Dimnames = c(
                                list(content = "orig"),
                                ..env[[.derived_local["dimnames"]]]),
                            class = class(.tmp))
                    })
        }
###-------------------------------------------------------------------
        ##  Restrict to the specified omega-range (no restriction for
        ##  the default which is the full range), adjust the sign when
        ##  required, find the value to be used for '.xlim', and
        ##  '.ylim', and find the data-frame for the global data.
        if (! .done_before["third"]) {
            ##  Create pointers to the previous step (nothing
            ##  copied yet).
            ..env[[.derived_local["third"]]] <- ..env[[.derived_local["second"]]]
            ..env[[.derived_global["third"]]] <- ..env[[.derived_global["second"]]]
            ##  Restrict the omega-dimension when required.
            if (length(dimnames(..env[[.derived_local["second"]]])$omega) >
                length(.LGS_restrict_local$third$omega)) {
                ..env[[.derived_global["third"]]] <-
                    restrict_array(
                        .arr = ..env[[.derived_global["second"]]],
                        .restrict = .LGS_restrict_local$third)
                ..env[[.derived_local["third"]]] <-
                    restrict_array(
                        .arr = ..env[[.derived_local["second"]]],
                        .restrict = .LGS_restrict_local$third)
            }
            ##  Adjust the sign when required.
            if (.look_up$adjust_result)
                if (.look_up$spectrum_variant %in% c("quad", "phase")) {
                    ..env[[.derived_global["third"]]] <-
                        -1 * ..env[[.derived_global["third"]]]
                    ..env[[.derived_local["third"]]] <-
                        -1 * ..env[[.derived_local["third"]]]
                }
            ##  The limit to be used on the x-axis:
            ..env$.LGS_xlim <- range(
                as.numeric(dimnames(..env[[.derived_local["third"]]])$omega))
            ##  The limit to be used on the y-axis, this will give the
            ##  same limit for all truncation levels and all points at
            ##  which the local Gaussian entity has been computed.

            ## ## ## ..env$.ylim <- range(
            ## ## ##     if (.look_up$matching_global)
            ## ## ##         ..env[[.derived_global["third"]]],
            ## ## ##     ..env[[.derived_local["third"]]])
#####   Not an optimal solution, but in order to have the same
#####   functionality as before, it seems necessary to use an ad hoc
#####   solution here.  A revised solution based on 'env_arrays' would
#####   simplify this mess

            ##  Create the required data-frame (for the global data).
            .components <- names(dimnames(..env[[.derived_global["third"]]]))
            ..env$.formula <-
                if (length(.components) == 2) {
                    quote(omega ~ content)
                } else {
                    parse(text = paste(
                              paste(
                                  c(setdiff(x = .components,
                                            y = c("omega", "content")),
                                    "omega"),
                                  collapse = " + "),
                              "content",
                              sep  = " ~ "))
                }
            kill(.components)
            ##  The data-frame for the global data.
            ..env[[.derived_global["df"]]] <-
                reshape2::dcast(data = reshape2::melt(data = ..env[[.derived_global["third"]]]),
                                formula = eval(..env$.formula))
        }
###-------------------------------------------------------------------
        ##  Restrict to the desired point of interest, and create the
        ##  data-frame for the local data.
        if (! .done_before["third_to_fourth"]) {
            ##  Restrict the local 
            ..env[[.derived_local["third_to_fourth"]]] <-
                restrict_array(
                    .arr = ..env[[.derived_local["third"]]],
                    .restrict = .LGS_restrict_local$third_to_fourth,
                    .drop = TRUE,
                    .never_drop = c("content", "cut", "omega"))

            ##  Create the data-frame for the local data.
            ..env[[.derived_local["df"]]] <-
                reshape2::dcast(data = reshape2::melt(data = ..env[[.derived_local["third_to_fourth"]]]),
                                formula = eval(..env$.formula))
        }
###-------------------------------------------------------------------
        ##  Restrict to the selected truncation level, find the
        ##  full '.ylim'-range, and create the desired plot.
        if (! .done_before["fourth"]) {
#####   Not an optimal solution, but in order to have the same
#####   functionality as before, it seems necessary to use an ad hoc
#####   solution here.  A revised solution based on 'env_arrays' would
#####   simplify this mess
            ..env$.LGS_ylim <- range(
                if (.look_up$matching_global)
                    restrict_array(
                        .arr = ..env[[.derived_global["third"]]],
                        .restrict = list(
                            cut = as.character(.LGS_restrict_global[["fourth"]]))),
                restrict_array(
                    .arr = ..env[[.derived_local["third"]]],
                    .restrict = list(
                        cut = as.character(.LGS_restrict_local[["fourth"]]))))
            ..env[[.derived_global["fourth"]]] <- local({
                .select <- ..env[[.derived_global["df"]]]$cut == 
                    .LGS_restrict_local$fourth
                ..env[[.derived_global["df"]]][.select, ]
            })
            ..env[[.derived_local["fourth"]]] <- local({
                .select <- ..env[[.derived_local["df"]]]$cut == 
                    .LGS_restrict_local$fourth
                ..env[[.derived_local["df"]]][.select, ]
            })
            ##  Check if the local Gaussian case of interest has
            ##  some matching global data to compare against.
            .global_data <-
                if (.look_up$matching_global) 
                    list(.block = .look_up$.block,
                         .data = ..env[[.derived_global["fourth"]]])
            ##  Create the desired plot.
            ..env$.spectra_plot <- LG_plot(
                .data = ..env[[.derived_local["fourth"]]],
                .global = .global_data,
                .lag = ..env$.selected_lag,
                .percentile = ..env$.selected_percentile,
                .select = "all",
                .xlim = ..env$.LGS_xlim,
                .ylim = ..env$.LGS_ylim,
                .aes_xy = ..env$.aes_xy,
                .aes_min_max = ..env$.aes_min_max,
                .sanity_checks = FALSE)
            ##  Add title and 'trustworthiness'.
            ..env$.spectra_plot <- ..env$.spectra_plot +
                ggplot2::ggtitle(label = ..env$.LGS_title) +
                ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
                annotate(geom = "text",
                         x = -Inf,
                         y = -Inf,
                         size = 4,
                         label = .look_up$details$text$trust_the_result,
                         col = "darkgreen",
                         ## col = "darkred",
                         vjust = -.5,
                         hjust = -0.1)
            names(..env$.spectra_plot$layers) <- c(
                head(x = names(..env$.spectra_plot$layers), n = -1),
                ".annotate_convergence")
            ##  Add the details as an attribute.
            attributes(..env$.spectra_plot)$details <- .look_up$details
        }
###-------------------------------------------------------------------
        ##  Adjust the range of the y-axis (when relevant), and if
        ##  necessary update the saved plot.
        if (! .done_before["fifth"]) {
            ##  Find the specified Y-range (as integers that gives
            ##  the lower and uper range of the available values).
            .Y_range <- .LGS_restrict_local$fifth$Y_range
            ##  Find the 'lengths of Y and omega', to compare.
            .Y_length <- diff(.Y_range) + 1
            .omega_length <- length(.LGS_restrict_local$third$omega)
            ##  Reminder: Only necessary to adjust when the length
            ##  of the available omega-vector is larger than the
            ##  length of the desired (zoomed) Y-vector.
            if (.omega_length > .Y_length) {
                ##  Update '.Y_range' when necessary.
                if (max(.Y_range) > max(.LGS_restrict_local$third$omega))
                    .Y_range <- .Y_range - {
                        max(.Y_range) - max(.LGS_restrict_local$third$omega)
                    }
                ##  Find the subset of values to be investigated.
                .local_copy <- apply(
                    X = ..env[[.derived_local["third"]]],
                    MARGIN = c("cut", "levels", "content"),
                    FUN = function(vec) {
                        sort(vec)[.Y_range]
                    })
                .global_copy <- apply(
                    X = ..env[[.derived_global["third"]]],
                    MARGIN = c("cut", "content"),
                    FUN = function(vec) {
                        sort(vec)[.Y_range]
                    })
                ##  Find the range of these subsets, and update
                ##  the saved plot with the new range.
                .zoomed_ylim <- range(
                    .local_copy,
                    if (.look_up$matching_global)
                        .global_copy)
                ..env$.spectra_plot <-
                    ..env$.spectra_plot +
                    coord_cartesian(ylim = .zoomed_ylim)
                kill(.local_copy, .global_copy)
            }
            kill(.Y_range, .Y_length, .omega_length)
        }
        ##   Return the plot to the workflow.
        return(..env$.spectra_plot)
    }
}
