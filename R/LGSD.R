################################################################################
##########  Internal defaults
#'
#' Internal default values.
#'
#' This list contains internal default-values, that other functions in
#' the package need in order to "communicate" together in a simple
#' manner.  That is to say, this list governs the naming conventions
#' used upon dimension-names for arrays; it specifies the
#' directory-hierarchy and the prefixes to be used for the the files
#' stored in them; and it also specifies the information that the
#' sanity-check function need to have access to.
#'
#' THIS DOCUMENTATION HAS TO BE UPDATED!  See the use of @format and
#' "describe" for the documentation of \code{TS_families}.
#' 
#' The present stuff contained in \code{LG_default} are:
#' 
#' \code{boot.prefix}, that are used to name the arrays that contain
#' bootstrap-related information, and in the functions that later on
#' extract information from them.
#'
#' \code{boot.dir}, that will be used as prefix for the
#' bootstrap-directories, such that different settings for the
#' bootstrap-procedure can thus be compared against each other.
#'
#' \code{approx.save}, three file-names to be used when storing
#' estimated parameters/function values from the local Gaussian
#' approximations for the bootstrap-replicates. Two files are used for
#' the data, due to the difference in structure between the zero-lag
#' case and the rest.  The third file is used to store information to
#' be used for the extraction of the data needed for the construction
#' of the local Gaussian spectra, i.e. the paths to the two first
#' files, and quotes that can be evaluated in order to extract the
#' information needed for the computation of the local Gaussian
#' spectra. In addition to this there is a numeric summarising the
#' e-flag status for the values computed by \code{localgauss}, this
#' should be zero if no problems occurred.
#' 
#' \code{boot.approx.save}, that gives the three file-names to be used
#' when storing estimated parameters/function values from the local
#' Gaussian approximations for the bootstrap-replicates. Two files are
#' used for the data, due to the difference in structure between the
#' zero-lag case and the rest.  The third file is used to store
#' information to be used for the extraction of the data needed for
#' the construction of the local Gaussian spectra, i.e. the paths to
#' the two first files, specification of the subsetting to be used and
#' a vector with the e-flag status for the values computed by
#' \code{localgauss}.
#' 
#' \code{boot.spectrals.save}, that gives the file-name to be used when
#' storing the derived data based on the data from
#' \code{LG_boot_approx}.
#'
## #' @usage This list is used as a reference for other functions.
#' @name LG_default
#' @keywords internal

LG_default <-
    list(###-------------------------------------------------------------------
        ##  Class to be added by 'TS_sample' or 'LG_select_points'
        class = list(
            points = "LG_points",
            TS = "TS_simulated",
            array = c("LG_array", "array"),
            block = "block_investigation"),
###-------------------------------------------------------------------
        ##  Attribute to be added by 'TS_LG_object', which  will be tested for
        ##  in 'LG_sanity_checks' by evaluating the quote in 'TS_test'
        ##  the functions that does the heavy work, and the programs will
        ##  terminate if the 'TS'-argument does not have this attribute.
        OK_attribute = "TS adjusted",
        TS_test = quote(local(
            expr = {
                .OK <- attr(x = arg_env$TS, which = "OK_attribute")
                if (is.null(.OK)) {
                    FALSE
                } else
                    .OK == LG_default$OK_attribute
            })),
###-------------------------------------------------------------------
        ##  Reminder of some default arguments that are used.
        LG_type = c("par_five", "par_one"),
        window = c("Tukey", "Parzen", "Bartlett"),
        boot_type = "block",
        .bws_mixture =  c("mixture", "local", "global"),
        content_details = c("rho_only", "rho_log.fun", "rho_all"),
        .method = c("all", "mean", "median"),
###-------------------------------------------------------------------
        ##  The dimnames on the local Gaussian spectra
        spectra_dimnames = list(
            par_five =
                c("spec", "cut", "window", "omega", "levels",
                  "bw_points", "type"),
            par_one =
                c("spec", "cut", "window", "omega", "levels",
                  "bw_points", "type")),
#####  REMINDER: Above: Two identical versions due to a residue from
#####  an earlier version.  Could update the code to simplify this
#####  part, but not a priority at the moment.
###-------------------------------------------------------------------
        ##  Name on main dimensions
        dimnames = list(main = "content"),
###-------------------------------------------------------------------
        ##  Name on files storing `TS` and `acr` (global values)
        global = c(TS = "TS.Rda",
                   TS_boot = "TS_boot.Rda",
                   acr = "acr.Rda",
                   acr_boot = "acr_boot.Rda",
                   spectral = "spectral.Rda",
                   spectral_boot = "spectral_boot.Rda",
                   spectral_CI = "spectral_CI.Rda",
                   spectral_collected = "spectral_collected.Rda",
                   adjustment = "adjustment.Rda"),
###-------------------------------------------------------------------
        ##  Name to be used when saving autocorrelations.
        acr_file = "acr.Rda",
#####  TASK: Obsolete, get rid of this one.
###-------------------------------------------------------------------
        ##  Name to be used when saving the "global bandwidths".
        bws_file = "bws_global.Rda",
###-------------------------------------------------------------------
        ##  Name to be used when saving the "local bandwidths".
        bws_local_file = "bws_local.Rda",
###-------------------------------------------------------------------
        ##  Prefix to be used in the sample-context.
        sample.prefix = "orig",
###-------------------------------------------------------------------
        ##  Prefix to be used in the bootstrap-context.
        boot.prefix = "boot",
###-------------------------------------------------------------------
        ##  Prefix to be used in the contiguous-context.
        contiguous.prefix = "block",
###-------------------------------------------------------------------
        ##  Internal name to be used in info-object to show files that
        ##  contain collected data from original computations and data
        ##  based on bootstrap-replicates.
        collected_data = "collected_data",
###-------------------------------------------------------------------
        ##  Check-list to be used when sanity-checking arguments.
        check = list(
            original = list(
                subset = c("LG_type", "window", ".bws_mixture",
                           "content_details", ".method"),
                logic = c(".bws_fixed_only"),
                integer_length_one = c("lag_max", "omega_length_out"),
                integer_vec = c("bw_points", "cut_vec"),
                numeric_length_one = "", ##  "marg"),  # obsolete.
                numeric_vec = c("omega_vec"),
                numeric_vec_positive_or_NULL = ".bws_fixed"),
            ##---
            bootstrap = list(
                subset =
                    c("boot_type", "bw_points", "cut_vec", "LG_type",
                      "omega_vec", "window", ".bws_mixture",
                      ".bws_fixed", "content_details"),
                logic = c("all_statistics", "log_", ".bws_fixed_only"),
                integer_length_one =
                    c("block_length", "boot_seed", "lag_max",
                      "nb", "threshold"),
                dir = c("data_dir", "spectra_dir"))),
        ##  Reminder: 'TS' will be checked with regard to having been
        ##  adjusted by 'TS_LG_object'.
###-------------------------------------------------------------------
        ##  Main files for storing of data:
###  At the highest level, overview of the time series that has been
###  generated, i.e. type, paramaters, length, seed.
        main_dir = "~/LG_DATA",
        main_dir_contiguous = "~/LG_DATA_CONTIGUOUS",
        other_TS_dir_prefix = "other",
        content_file_name = "TS_content.Rda",
###  At the second highest level, i.e. within each folder
###  corresponding to a simulated (or observed) time series, this will
###  be used to store information about the sub-folders corresponding
###  to different parameters for the local Gaussian approximations
###  (bandwidths, levels, lags) and local Gaussian spectra
###  (frequencies, window, truncation point), and the further
###  sub-directories due to boosrmtrapping (where we might want to
###  compare the effect of using different bootstrap-strategies).
        info_file_name = "info.Rda",
###-------------------------------------------------------------------
        ##  Folder-defaults for the saving of different steps.
        folder_defaults =
            c(ts.dir= "TS",
              approx.dir = "Approx",
              spectra.dir = "Spectra",
              boot.approx.dir = "Boot_Approx",
              boot.spectra.dir = "Boot_Spectra"),
###-------------------------------------------------------------------
        ##  Defaults for 'ignore_these' in comparison of spy-reports.

        ignore_these = list(
            approx.dir = c("main_dir", "save_dir"),
            spectra.dir = c("main_dir", "save_dir", "omega_length_out"),
            boot.approx.dir = c("main_dir", "save_dir", "threshold"),
            boot.spectra.dir = c("main_dir", "save_dir", "threshold")),
#####  TASK: Recheck this when the code has settled, and create a tiny
#####  sanity-check to see if the argument of the functions are like
#####  we want them to be.
###-------------------------------------------------------------------
        ##  Stems to be used when saving objects
        approx_save =
            c(par_one  = "par_one_approx",
              par_five = "par_five_approx"),
        spectra_save =
            c(par_one  = "par_one_spectra",
              par_five = "par_five_spectra"),
#############---------------------------------------------------------
###  Note that the stems above will be prefixed with 'type' (and when
###  necessary with the 'boot.prefix' too) in the saving process, and
###  that the file-names that will be used will be created by
###  suffixing with ".Rda" (eventually also with an added
###  "part_a_of_b" if memory-issues require the computation to be
###  partitioned into several chunks.
###-------------------------------------------------------------------
        ##  Create function to initial data-frame for saving of
        ##  file-related information.  'object_size' is included since
        ##  some latter computations might have to be partitioned due
        ##  to memory issues.
        file_info_df =  function(L){
            place_holder_char <- paste(rep(".", 160), collapse = "") 
            ##  Note: Use "large" placeholder to avoid a need for
            ##  increased memory when the information is updated.
            data.frame(
                part_a_of_b = rep(place_holder_char, ),
                content = rep(place_holder_char, ), 
                data_files = rep(place_holder_char, ), 
                object_size_MB= rep(NA_real_, L),
                stringsAsFactors = FALSE)
        },
###-------------------------------------------------------------------
        ##  Initiate stuff used for storing the results in lists, an
        ##  update below will add additional components.  Reminder
        ##  "negIm" (negative of "Im") is used to simplify formulas
        ##  later on, in accordance with the way the cross spectrum is
        ##  written as "f = q - i*q".
        result = list(
            .azero = as.array(0), ## For "degenerate arrays".
            auto_node = c("on_diag", "auto", "Re"),
            hierarchy = list(
                point = c("on_diag", "off_diag"),
                type = c("auto", "cross"),
                part = c("Re", "negIm", "amplitude", "phase", "squared_coherence"))
        )
    )


##  Add the names to be used on the the 'actionButtons' that selects
##  the different types of spectra that can be investigated.
##  Reminder: "G" and "L" refers respectively to "Global" and "Local",
##  the "S" represents "Spectrum", "a" and "c" refers to "auto" and
##  "cross" versions, whereas the last part refers to the different
##  alternatives.  For the local auto-case, we will actually need (for
##  off-diagonal points) the versions that investigate the
##  complex-valued results.

LG_default$spectrum_type_ID <- local({
    ##  Create the combinations of interest (includes some
    ##  alternatives that must be pruned away later on). Reminder: The
    ##  somewhat cranky reordering in the 'paste'-part is obviously
    ##  not necessary as such, but it makes the manual inspection of
    ##  things easier whem updating the code.
    .grid <- expand.grid(
        GL = paste(c("G", "L"),
                   "S_",
                   sep = ""),
        type = c("", "_amplitude", "_phase", "_Co",  "_Quad"),
        ac = c("a", "c"),
        stringsAsFactors = FALSE)
    ##  Reminder: Some rows are not needed for the inspection of the
    ##  ordinary global auto-spectrum.
    unlist(lapply(X = 1:dim(.grid)[1],
           FUN = function(x) {
               .row <- .grid[x, ]
               if (all(.row$GL == "GS_", .row$type != "", .row$ac == "a")) {
                   NULL
               } else
                   paste(.row[c("GL", "ac", "type")], collapse = "")
           }))
})

##  Add the 'zero'-value to be used in the initiation phase.
LG_default$spectrum_type_zero <- structure(
    .Data = 0L,
    class = c("integer","shinyActionButtonValue"))


##  Add stuff that use the already defined content as arguments,
##  including a simpler structure that doesn't require the 'point'
##  since it works upon the original (global) level instead of the
##  local level.
LG_default$result <- c(
    LG_default$result,
    list(skeleton = leanRcoding::skeleton_list(
             names_list = LG_default$result$hierarchy,
             default_content = LG_default$result$.azero,
             add_names_list_as_attribute = FALSE),
         skeleton_dimnames = leanRcoding::skeleton_list(
             names_list = LG_default$result$hierarchy,
             default_content = NULL,
             add_names_list_as_attribute = FALSE),
         array_nodes = plyr::alply(
             .data = expand.grid(
                 LG_default$result$hierarchy,
                 stringsAsFactors = FALSE),
             .margins = 1,
             .fun = unlist)))
LG_default$result_orig <- list(
    auto_node = LG_default$result$auto_node[2:3],
    hierarchy = LG_default$result$hierarchy[c("type", "part")]
)
LG_default$result_orig <- c(
    LG_default$result_orig,
    list(skeleton = leanRcoding::skeleton_list(
             names_list = LG_default$result_orig$hierarchy,
             default_content = LG_default$result$.azero,
             add_names_list_as_attribute = FALSE),
         skeleton_dimnames = leanRcoding::skeleton_list(
             names_list = LG_default$result_orig$hierarchy,
             default_content = NULL,
             add_names_list_as_attribute = FALSE),
         array_nodes = plyr::alply(
             .data = expand.grid(
                 LG_default$result_orig$hierarchy,
                 stringsAsFactors = FALSE),
             .margins = 1,
             .fun = unlist)))


