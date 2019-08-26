#' Find the parameters to be used in the \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .approx A vector that specifies one of the nodes of
#'     \code{.info}.
#'
#' @param .iterated A logical value, default \code{FALSE}, that is
#'     included since this function will call itself when an
#'     approx-node contains bootstrap-information.  (There are some
#'     minor differences with regard to how the result should be
#'     returned in these to cases.)
#'
#' @return This helper function finds the parameters to use when
#'     \code{LG_shiny} shall visualise the content of interest.
#'
#' @keywords internal

LG_shiny_interface_1_helper <- function(.env, .approx, .iterated=FALSE) {
    ##  Initiate the environment to be added at the approx-nodes.  The
    ##  idea is to have a bunch of objects that stores different
    ##  components of the required information.  A list 'details' will
    ##  be used to collect some relevant information needed for the
    ##  other workers later on.  Then there will be a collection of
    ##  lists named in accordance with the actionButtons, radioButtons
    ##  and sliderInputs that they contain data for.  These lists will
    ##  be used to create the interface, and they will also be used as
    ##  a memory to ensure that we get back to the last visited
    ##  parameter-configuration when we returns to a previously
    ##  visited branch.  The stored environment will in addition also
    ##  contain some lists that helps keep track of the status of some
    ##  of the actionButtons, in order to keep track of which one that
    ##  were used the last time around.
###-------------------------------------------------------------------    
    ##  Note that this function also must deal with the case that an
    ##  approx node contains additional bootstrap-nodes, and this is
    ##  done by the function being called iteratively when it is
    ##  discovered from '.env$info' that there are such
    ##  bootstrap-nodes.
###-------------------------------------------------------------------    
    ##  Create a few shortcuts in order to compactify the code.
    .info_part <- .env$info[[.approx]]
    ##  Note that '.info_part' in fact might be 'NULL' if no
    ##  approximations have been computed.  If so, terminate.
    if (is.null(.info_part))
        return(NULL)
    ##  If no problem, get hold of the desired environment.
    ..env <- .info_part$spy_report$envir
    ##  Initiate the 'details'-lists by extracting parts from the
    ##  '.env$info'-object. Reminder: I think that it actually only
    ##  will be a few of these that will be used by the other workers
    ##  later on, but they can stay as they are.
    .from_TS_info <- c("TS_key", "TS", "block", "details", "N",
                       ".variables", ".nr_variables",
                       ".original_variable_names", ".variable_pairs",
                       ".bivariate_pairs", ".bivariate_pairs_II",
                       ".univariate_pairs")
    ##  Reminder: It should not be necessary to include this for both
    ##  the approx-node and its bootstrap-nodes, but I am not going to
    ##  worry about that for the time being.
    details <- .env$info$TS_info[.from_TS_info]
    kill(.from_TS_info)
    ##  Compute a few additional nodes from this information.
    details$is_multivariate <- details$.nr_variables > 1
    ##  The number of samples (when simulated data)
    if (details$block) {
        details$nr_simulated_samples <- .env$info$TS_info$spy_report$envir$nr_samples
    }
    ##  Add information about the LG_types that are present.
    details$LG_type <- ..env$LG_type
    ##  Add information that only occurs for the bootstrap-cases.
    if (.iterated) {
        for (.arg in c("boot_type", "block_length", "nb"))
            details[[.arg]] <- ..env[[.arg]]
        kill(.arg)
    }
###-------------------------------------------------------------------
    ##  Add information about where data are stored.  Reminder: At
    ##  this step paths relative to the actual OS is added.
    details$data_dir <- paste(c(.env$main_dir,
                                .env$input$TS,
                                .approx),
                             collapse = .Platform$file.sep)
    details$.global_file <- paste(
        c(.env$main_dir,
          if (.iterated) {
              .info_part$acr_boot
          } else
              .info_part$acr),
        collapse = .Platform$file.sep)
    details$.local_files <- structure(
        .Data = file.path(details$data_dir,
                          .info_part$data_files_df$data_files),
        .Names = .info_part$data_files_df$content)
###-------------------------------------------------------------------    
    ##  Add information about the convergence status for the
    ##  'par_five'-case, i.e. a summary of the 'eflag'-status from the
    ##  computations by the 'localgauss'-package.
    details$convergence <- .info_part$convergence
    ##  Extract information about 'type'
    details$type <- ..env$LG_type
###-------------------------------------------------------------------
    ##  Identify details related to the points under investigation,
    ##  i.e. inspect the attributes of the 'LG_points'-argument.
    details$.Horizontal <- attributes(..env$LG_points)$Horizontal
    details$.Vertical   <- attributes(..env$LG_points)$Vertical
    details$.Shape      <- attributes(..env$LG_points)$Shape
    ##  Create a logical value that reveals if only on-diagonal points
    ##  will be encountered later on.
    details$is_only_diagonal <- nested_if(
        if_list = list(
            details$.Shape != "rectangle",
            identical(x = details$.Horizontal,
                      y = details$.Vertical)),
        expr_not_all_TRUE = FALSE)
    ##  Add information about the dimension-names too.  Reminder: If
    ##  both of the possible approximations are present (.i.e. both
    ##  the five-parameter that should be used and the one-parameter
    ##  that should be shunned), then there will be two different
    ##  dimension-nodes, that only differs by means of an 'e-flag'
    ##  value for the fiver-parameter case. Some additional tweaking
    ##  is thus required in order to get things properly done.
    ##  Reminder: Start out by identifying the names of the nodes
    ##  containing the dimension names, and then select the one belong
    ##  to the five-parameter one (if present).
    .dn <- names(.info_part)[stringr::str_detect(string = names(.info_part),
                                          pattern = "dimnames")]
    if (length(.dn) == 2)
        .dn <- .dn[stringr::str_detect(string = .dn,
                                       pattern = "par_five")]
    if (.iterated) {
        details$.dimnames <- .info_part[[.dn]]$boot_par
    } else
        details$.dimnames <- .info_part[[.dn]]$par
    kill(.dn)
###-------------------------------------------------------------------
    ##  Create a list with logical values that can be used when the
    ##  interface is to be created, i.e. the purpose is to simplify
    ##  the interface by removing selections where only one value is
    ##  present.  Note that the names on this list must match those to
    ##  be used as ID in the interface of interest.
    .simplify_logical <- list(
        Vi = ! details$is_multivariate,
        Vj = ! details$is_multivariate,
        type = {length(details$LG_type) == 1},
        point_type = details$is_only_diagonal,
        bw_points = {length(details$.dimnames$bw_points) == 1})
    ##  Create a list with information that should be used instead of
    ##  a selector for the trivial cases.  This list will only contain
    ##  information for the cases were a simplification is present.
    .simplify_text <- list()
    if (.simplify_logical$Vi) {
        .simplify_text$Vi <- "Univariate time series"
        .simplify_text$Vi <- "Univariate time series"
    }
    if (.simplify_logical$type)
        .simplify_text$type <-
            sprintf("Estimates from %s-parametric local Gaussian correlations<br>",
                    ifelse(test = details$LG_type == "par_five",
                           yes  = "five",
                           no   = "one"))
    if (.simplify_logical$point_type)
        .simplify_text$point_type  <-  "Only diagonal points<br>"
    if (.simplify_logical$bw_points)
        .simplify_text$bw_points <-
            sprintf("Bandwidth: %s<br>",
                    details$.dimnames$bw_points)
###-------------------------------------------------------------------
    ##  Find the values for the radioButtons
    .radioButtons <- list(
        ##  The value for the first variable:
        Vi = list(
            label = "Var 1",
            choices = .env$info$TS_info$.variables,
            selected = head(.env$info$TS_info$.variables, 1)),
        ##  The value for the second variable:
        Vj = list(
            label = "Var 2",
            choices = .env$info$TS_info$.variables,
            selected = tail(.env$info$TS_info$.variables, 1)),
        ##  The value that decides if we are on or off the diagonal.
        ##  WARNING: The selection below should probably not create a
        ##  problem for any of the cases investigated up to now, but
        ##  it could be an issue in general (if a region completely
        ##  outside of the diagonal is investigated).
        point_type = list(
            label = "Select branch for points",
            choices = LG_default$result$hierarchy$point,
            selected = LG_default$result$hierarchy$point[1]),
        ##  Specification of the type of local Gaussian approximation,
        ##  i.e. 'par_five' or 'par_one'. (Hopefully no one will ever
        ##  waste computational resources on the crappy
        ##  'par_one'-alternative that I never wanted to include in
        ##  the first place.  After all, from a geometrical point of
        ##  view it is obvious that the 'par_one'-alternative in
        ##  general will be completely useless.)
        type = list(
            label = "Local Gaussian approximation",
            choices = ..env$LG_type,
            selected = ifelse(
                test = isTRUE(.env$default_type %in% ..env$LG_type),
                yes  = .env$default_type,
                no   = ..env$LG_type[1])),
        ##  The bandwidths used for the estimation of the local
        ##  Gaussian auto- and cross-correlations.
        bw_points = list(
            label = "Local bandwidth solution",
            choices = details$.dimnames$bw_points,
            selected = details$.dimnames$bw_points[1]),
        ##  An option that allows an inspection of the ordinary
        ##  (global) correlations and spectra.
        global_local = list(
            label = "Local or global data",
            choices =  c("global", "local"),
            selected = "local"),
        ##  Selection of the confidence intervals,
        confidence_interval = list(
            label = "Select estimated pointwise confidence interval",
            choices = c("min_max", "99", "95", "90"),
            selected = "95"))
###-------------------------------------------------------------------
    ##  Find the values for the sliderInputs.  This contains the
    ##  information related to the selection of the points/levels, the
    ##  selection of truncation point 'cut' and a specification of the
    ##  frequency range.
    .center <- function(vec)
        ceiling(length(vec)/2)
    .sliderInputs <- list(
        levels_Diagonal = list(
            ##  on-diagonal case
            label = "Index for points on diagonal",
            min = 1L,
            max = length(details$.dimnames$levels),
            value = .center(details$.dimnames$levels)), 
        levels_Line = list(
            ##  off-diagonal, line.
            label = "Index for points on line",
            min = 1L,
            max = length(details$.Horizontal),
            value = .center(details$.Horizontal)),
        levels_Horizontal = list(
            ##  off-diagonal, rectangle.
            label = "Index for horizontal points",
            min = 1L,
            max = length(details$.Horizontal),
            value = .center(details$.Horizontal)),
        levels_Vertical = list(
            ##  off-diagonal, rectangle.
            label = "Index for vertical points",
            min = 1L,
            max = length(details$.Vertical),
            value = .center(details$.Vertical)),
        cut = list(
           ##  Selection of truncation level.  Reminder: The
           ##  truncation level in the weight-function specifies the
           ##  first part that will have the weight zero, i.e. the
           ##  smallest value 'cut' can have is 1 - in which case only
           ##  the lag 0 component will be included.
            label = "Select m-truncation",
            min = 0L,
            max = length(details$.dimnames$lag) - 1,
            value = min(10, .center(details$.dimnames$lag))),
        frequency_range = list(
            ##  Reminder: The range is based on a rescaling, thus from
            ##  '0' to '0.5', and the value is given as a vector of
            ##  length two in order for this
            label = "Specify the frequency-range",
            min = 0,
            max = 0.5,
            value = c(0, 0.5)))
    kill(.center, ..env)
    ##  Add details related to lag-window-function, needed for the
    ##  smoothing of the estimated spectra.
    .selectInputs <- list(
        window = list(
            label = "Specify the lag-window-function",
            choices = LG_default$window,
            selected = "Tukey"))
    ##  Add details related to the 'actionButtons'.  These are used To
    ##  select between different branches of the investigation.  They
    ##  are a bit pesky since they can be pushed more than once, and
    ##  as such it is necessary to include some derived graphical
    ##  values to register whether a new branch should be selected or
    ##  if nothing should be done.  The '.zero' defined below is in
    ##  order to have the initial format correct.
    .zero <- LG_default$spectrum_type_zero
    .actionButtons <- c(list(
        ##  Buttons for the main graphical selection
        TS_graphic = .zero,
        Approx_graphic = .zero,
        Spectra_graphic = .zero,
        ## Buttons specific for the time series themselves.
        TS_plot = .zero,
        TS_acf = .zero,
        TS_pacf = .zero,
        TS_spec.pgram = .zero,
        TS_lags = .zero),
        ##  Buttons for the spectrum-part, created by the help of the
        ##  information stored in 'LG_default
        structure(
            .Data = lapply(
                X = LG_default$spectrum_type_ID,
                FUN = function(x) LG_default$spectrum_type_zero),
            .Names = LG_default$spectrum_type_ID))
    kill(.zero)
    ##  Reminder: The setup for the actionButtons differs from the
    ##  others since they always are reset to zero if the panel they
    ##  are living on are turned off and then on again.  Thus, this
    ##  part does not contain the code to create the buttons, but
    ##  instead it stores the values derived from them by the other
    ##  workers.  These derived values will be added directly to the
    ##  'input' (without a corresponding interface) in order for the
    ##  workers that creates the plot and the explanations to have
    ##  access to them. Note that the vectors stored here have length
    ##  two in order to keep track of the cases where an
    ##  action-button has been pushed twice (or more), since it is
    ##  desirable to not trigger any updates of the interface or plots
    ##  for that case.
    .derived_graphical  <- list(
        TCS_type = c(NA_character_, NA_character_),
        sub_graph_type = c(NA_character_, NA_character_),
        S_type = c(NA_character_, NA_character_))
    ##  The information about the spectrum type that is selected is
    ##  based on a similar, but somewhat more messy setup.  The
    ##  approach below also contains (as the first component of the
    ##  vectors) the values to be used as default when the node is
    ##  visited for the first time.  Note that parts of this will be
    ##  irrelevant for univariate time series when all the points lies
    ##  on the diagonal.
    .Spectrum_type  <- local({
        list(global =
                 list(auto  = c("GS_a", "GS_a"),
                      cross = c("GS_c_Co", NA_character_)),
             local =
                 list(auto =
                          list(on_diag = c("LS_a", "LS_a"),
                               off_diag = c("LS_a_Co", NA_character_)),
                      cross =
                          list(on_diag = c("LS_c_Co", "LS_c_Co"),
                               off_diag = c("LS_c_Co", NA_character_))
                      ))
    })
    .res <- as.environment(list(
        is_bootstrap = .iterated,
        .actionButtons = .actionButtons,
        .radioButtons = .radioButtons,
        .sliderInputs = .sliderInputs,
        .selectInputs = .selectInputs,
        .derived_graphical = .derived_graphical,
        .Spectrum_type = .Spectrum_type,
        details = details,
        .simplify_logical = .simplify_logical,
        .simplify_text = .simplify_text))
    ##  Add a 'last'-object to identify the most recently visited
    ##  bootstrap-node , and add a 'names'-object to list the
    ##  available bootstrap-nodes.  For the cases where there is no
    ##  bootstrap-nodes, these will permanently be 'NA', and that will
    ##  be used when updating the 'TS_logging'-object.
    .res$last <- NA_character_    
    .res$names <- NA_character_    
    ##  When we do not have simulated data, we need to check if there
    ##  might be bootstrap-nodes to consider.  In this context the
    ##  logical value 'block' is the one of interest.
    if (!.res$details$block) {
        ##  Find the bootstrapped nodes, if any are present.
        .res$names <- names(.info_part)[stringr::str_detect(
                               string = names(.info_part),
                               pattern =  LG_default$folder_defaults["boot.approx.dir"])]
        ##  Add information for 'label', 'header' and 'last'.
        ##  Reminder: The present setup differs from those at the
        ##  higher nodes since we do not want to auto select any
        ##  bootstrap nodes (if we did, we would in many cases not be
        ##  able to  investigate the approx-level).
        .res$label <- sprintf("%s bootstrap approximation%s available",
                              length(.res$names),
                              ifelse(test = length(.res$names) > 1,
                                     yes  = "s",
                                     no   = ""))
        .res$header <- "Select a bootstrap approximation"
        if (length(.res$names) == 1) {
            .res$last <- .res$names
        } else {
            .res$last <- .res$header
        }
        ##  Override the previous values when no data are available.
        if (length(.res$names) == 0) {
            .res$label <- "No bootstrap approximations detected"
            .res$last <- "Nothing here to select"
            .res$header <- "Nothing here to select"
        }
###-------------------------------------------------------------------
        ##  Use this function iteratively to add information about
        ##  the bootstrap-nodes (when they are present).
        for (.node in .res$names)
            assign(x = .node,
                   value = LG_shiny_interface_1_helper(
                       .env = .env,
                       .approx = c(.approx, .node),
                       .iterated = TRUE),
                   envir = .res)
    }        
    ##  Return the result to the workflow, either as a list with the
    ##  name '.approx' (in the case when '.iterated' is 'FALSE') or
    ##  simply return the object '.res' as it is since the iterative
    ##  use of this function then will place it at the desired place
    ##  in the hierarchical structure.
    if (.iterated) {
        return(.res)
    } else
        return(structure(.Data = list(.res),
                         .Names = tail(.approx, 1)))
}
