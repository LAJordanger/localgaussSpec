#' Internal default values for the local Gaussian machinery
#'
#' @description This list contains internal default-values that other
#'     functions in the package need in order to "communicate"
#'     together.  This list governs the naming conventions used for
#'     the dimension-names for arrays; it specifies the
#'     directory-hierarchy and the prefixes to be used for the files
#'     stored in them; and it also specifies assorted information that
#'     the sanity-check function need to have access to.
#'
#' @format list
#'
#' @name LG_default
#'
#' @keywords internal

LG_default <- list()

##  Class to be added by 'TS_sample' or 'LG_select_points'.

LG_default$class  <-  list(
    points = "LG_points",
    TS = "TS_simulated",
    array = c("LG_array", "array"),
    block = "block_investigation")

##  Attribute to be added by 'TS_LG_object', which will be tested for
##  in 'LG_sanity_checks' by evaluating the quote in 'TS_test'.  The
##  program will terminate if the 'TS'-argument does not have this
##  attribute.

LG_default$OK_attribute  <-  "TS adjusted"
LG_default$TS_test  <-  quote(local(
    expr = {
        .OK <- attr(x = arg_env$TS, which = "OK_attribute")
        if (is.null(.OK)) {
            FALSE
        } else
            .OK == LG_default$OK_attribute
    }))

##  Some default arguments that are used.

LG_default$LG_type <- c("par_five", "par_one")
LG_default$window <- c("Tukey", "Parzen", "Bartlett")
LG_default$boot_type <- "block"
LG_default$.bws_mixture <-  c("mixture", "local", "global")
LG_default$content_details <- c("rho_only", "rho_log.fun", "rho_all")
LG_default$.method <- c("all", "mean", "median")

##  Name to be used on the main dimension of the stored arrays.

LG_default$dimnames <- list(main = "content")

##  Name on files storing `TS` and `acr` (global values)g

LG_default$global <- c(TS = "TS.Rda",
                      TS_boot = "TS_boot.Rda",
                      acr = "acr.Rda",
                      acr_boot = "acr_boot.Rda")

##  Name to be used when saving the "global bandwidths".

LG_default$bws_file <- "bws_global.Rda"

##  Name to be used when saving the "local bandwidths".

LG_default$bws_local_file <- "bws_local.Rda"

##  Prefix to be used in the sample-context.

LG_default$sample.prefix <- "orig"

##  Prefix to be used in the bootstrap-context.

LG_default$boot.prefix <- "boot"

##  Prefix to be used in the contiguous-context.

LG_default$contiguous.prefix <- "block"

##  Check-list to be used when sanity-checking arguments.

LG_default$check <- list(
    original = list(
        subset = c("LG_type", ".bws_mixture",
                   "content_details", ".method"),
        logic = c(".bws_fixed_only"),
        integer_length_one = c("lag_max"),
        integer_vec = c("bw_points", "cut_vec"),
        numeric_vec_positive_or_NULL = ".bws_fixed"),
    bootstrap = list(
        subset =
            c("boot_type", "bw_points", "LG_type",
              ".bws_mixture", ".bws_fixed", "content_details"),
        logic = c("all_statistics", "log_", ".bws_fixed_only"),
        integer_length_one =
            c("block_length", "boot_seed", "lag_max",
              "nb", "threshold"),
        dir = "data_dir"))

##  Reminder: 'TS' will be checked with regard to having been
##  adjusted by 'TS_LG_object'.

###------------------------------------------------------###

##  Main files for storing of data:
###  At the highest level, overview of the time series that has been
###  generated, i.e. type, paramaters, length, seed.
## Reminder: Important to avoid OS-dependent symbols in this part!

LG_default$main_dir <- c("~", "LG_DATA")
LG_default$other_TS_dir_prefix <- "other"
LG_default$content_file_name <- "TS_content.Rda"

###  At the second highest level, i.e. within each folder
###  corresponding to a simulated (or observed) time series, this will
###  be used to store information about the sub-folders corresponding
###  to different parameters for the local Gaussian approximations
###  (bandwidths, levels, lags), and the further sub-directories due
###  to bootstrapping (where we might want to compare the effect of
###  using different bootstrap-strategies).

LG_default$info_file_name <- "info.Rda"

##  Folder-defaults for the saving of different steps.

LG_default$folder_defaults <-
    c(ts.dir= "TS",
      approx.dir = "Approx",
      boot.approx.dir = "Boot_Approx")

##  Defaults for 'ignore_these' in comparison of spy-reports.

LG_default$ignore_these <- list(
    approx.dir = c("main_dir", "save_dir"),
    boot.approx.dir = c("main_dir", "save_dir", "threshold"))

##  Create function to initial data-frame for saving of file-related
##  information.  'object_size' is included since some latter
##  computations might have to be partitioned due to memory issues.

LG_default$file_info_df <-  function(L) {
            place_holder_char <- paste(rep(".", 160), collapse = "") 
            ##  Note: Use "large" placeholder to avoid a need for
            ##  increased memory when the information is updated.
            data.frame(
                part_a_of_b = rep(place_holder_char, ),
                content = rep(place_holder_char, ), 
                data_files = rep(place_holder_char, ), 
                object_size_MB= rep(NA_real_, L),
                stringsAsFactors = FALSE)
        }

##  Initiate stuff used for storing the results in lists, an update
##  below will add additional components.

LG_default$result <- list(
    .azero = as.array(0), ## For "degenerate arrays".
    auto_node = c("on_diag", "auto", "Re"),
    hierarchy = list(
        point = c("on_diag", "off_diag"),
        type = c("auto", "cross"),
        part = c("Re", "negIm", "amplitude", "phase", "squared_coherence")))

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
    ##  things easier when updating the code.
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

## Add the defaults for the curlicues for the different
## plots. Reminder: Some values, i.e. 'include' and 'label' will
## primarily be updated from the internal code.  When the plots are
## called outside of the shiny-application, then it is possible to
## override all the information — which in particular is of interest
## if the plots are to be shrinked into some grid-based presentation.

## Reminder: 'ggplot2::theme_get()' gives a list with the default
## values in the theme, and that is where the 'rel(1.2)'-default
## values comes from.

## Reminder: Some of the 'NULL'-values in the list below will later on
## be filled out based on the plot under investigation.

LG_default$curlicues <- list(
    x.label_low_high = NULL,
    title = list(
        include = TRUE,
        label = NULL,
        element_text = list(
            family = NULL,
            face = NULL,
            color = "black",
            size = rel(1.2),
            hjust = 0.5,
            vjust = NULL,
            angle = NULL,
            lineheight = NULL,
            margin = NULL,
            debug = NULL,
            inherit.blank = FALSE)),
    plot_stamp = list(
        include = TRUE,
        annotate = list(
            geom = "text",
            x = NULL,
            y = Inf,
            size = 9,
            label = NULL,
            col = "black",
            alpha = 0.6,
            vjust = 1.5,
            hjust = "center")),
    m_value = list(
        include = TRUE,
        annotate = list(
            geom = "text",
            x = NULL,
            y = Inf,
            size = 4,
            label = NULL,
            col = "brown",
            alpha = 1,
            vjust = 1.3,
            hjust = "inward")),
    v_value = list(
        include = TRUE,
        annotate = list(
            geom = "text",
            x = NULL,
            y = Inf,
            size = 4,
            label = NULL,
            col = "brown",
            alpha = 1,
            vjust = 1.3,
            hjust = "inward")),
    b_value = list(
        include = TRUE,
        annotate = list(
            geom = "text",
            x = NULL,
            y = Inf,
            size = 4,
            label = NULL,
            col = "brown",
            alpha = 1,
            vjust = 2 * 1.3,
            hjust = "inward")),
    NC_value = list(
        include = TRUE,
        short_or_long_label = "long",
        annotate = list(
            geom = "text",
            x = NULL,
            y = -Inf,
            size = 4,
            label = NULL,
            col = "darkgreen",
            alpha = 1,
            vjust = -.5,
            hjust = "inward")),
    n_R_L_values = list(
        include = TRUE,
        annotate = list(
            geom = "text",
            x = NULL,
            y = -Inf,
            size = 4,
            label = NULL,
            col = "brown",
            alpha = 1,
            vjust = -.5,
            hjust = "inward")),
    TS_plot = list(
        description = list(
            x = 0,
            y = Inf,
            size = 4,
            label = NULL,
            col = "brown",
            alpha = 1,
            vjust = "inward",
            hjust = "inward"),
        line = list(
            alpha = 0.8,
            lwd = 0.03),
        hline  = list(
            linetype = 2,
            col = "brown",
            alpha = 0.8,
            lwd = .1)),
    spectra_plot = list(
        WN_line  = list(
            include = TRUE,
            size = .5,
            alpha = .8,
            linetype = 3),
        global = list(
            line.include = TRUE,
            ribbon.include = TRUE,
            line.size = .5,
            line.alpha = .8,
            ribbon.alpha = .3),
        local = list(
            line.include = TRUE,
            ribbon.include = TRUE,
            line.size = .5,
            line.alpha = .8,
            ribbon.alpha = .3)),
    correlation_plot = list(
        hline  = list(
            lwd = .5,
            alpha = .8),
        vline  = list(
            lwd = .5,
            alpha = .25),
        boxplot = list(
            width = .01,
            boundary = .1,
            size = .5,
            colour = "black",
            fill = "white",
            alpha = .5,
            outlier.size = .5,
            outlier.alpha = .3),
        segment = list(
            size = .25,
            alpha = .8)),
    data_extraction = list(
        ##  local spectrum for real data.
        .geom_line_local = FALSE,
        ##  local spectrum for 'block'-case.
        .geom_line_global_me = FALSE,
        ##  global spectrum.
        .geom_line_global = FALSE,
        ##  global pointwise confidence intervals.
        .geom_ribbon_global = FALSE,
        ##  local pointwise confidence intervals.
        .geom_ribbon_local = FALSE))
