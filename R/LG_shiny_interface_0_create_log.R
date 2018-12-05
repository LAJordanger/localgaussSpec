#' Create \code{TS_logging}-object for the dynamic
#' \code{LG_shiny}-interface.
#'
#' This function initiates the \code{TS_logging}-object needed in
#' order for the dynamic structure of the \code{LG_shiny}-application
#' to work without any glitches.  Moreover, it does also adds a bunch
#' of values to the reactive \code{input}-object in the
#' \code{shiny}-application started by \code{LG_shiny}.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.  The logging-object will be added to this
#'     environment.
#' 
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function adds the \code{TS_logging}-object into the
#'     environment \code{.env}.  This object will log input-values in
#'     order to avoid problems when switching between different
#'     branches, i.e. it prevents residual input-values to trigger
#'     subsetting problems.  Moreover, the last selected input-values
#'     for an old branch will be stored when there is a switch to a
#'     new branch, and these will then be loaded the next time the old
#'     branch is visited.  The function does also add the initial
#'     \code{NA}-values to the reactive \code{input}-object in the
#'     \code{shiny}-application started by \code{LG_shiny}, and this
#'     is done in order for some of the logical tests to become
#'     simpler during the initiation phase.
#'
#' @keywords internal

LG_shiny_interface_0_create_log <- function(.env, .env2) {
    ##  Create 'TS_logging', an object whose purpose is to keep track
    ##  of available values at different levels.  This is done in
    ##  order to avoid updating issues when switching between
    ##  different branches of the available data.
###-------------------------------------------------------------------
    ##  Use a 'with'-construction for the updating, in order to avoid
    ##  all the '.env$'-references that otherwise would be needed.
    ##  Reminder: The new objects defined in this setup will be added
    ##  to '.env', so remove intermediate stuff to keep that
    ##  environment as clean as possible.
    with(data = .env,
         expr = {
             ##  The core skeleton based on 'TS_content'.
            TS_logging <- structure(
                 .Data = lapply(
                     X = seq_along(TS_content),
                     FUN = function(i) {
                         structure(
                             .Data = lapply(
                                 X = names(TS_content[[i]]),
                                 FUN = function(x)
                                     list(last = NA_character_,
                                          names = NA_character_,
                                          header = character(0),
                                          label = NA_character_,
                                          visited = FALSE)),
                             .Names = names(TS_content[[i]]))
                     }),
                 .Names = names(TS_content))
###-------------------------------------------------------------------
             ##  Add the nodes to the first level too, in order to
             ##  simplify the code later on.
             TS_logging$names <- sort(names(TS_logging))
             TS_logging$label <- ifelse(
                 test = length(TS_logging$names) == 1,
                 yes = "Auto-selected the only available group",
                 no = sprintf("%s groups available",
                              length(TS_logging$names)))
             if (length(TS_logging$names) > 1) {
                 TS_logging$header <- "Select a group"
             } else {
                 TS_logging$header <- character(0)
             }
                 
            if (length(TS_logging$names) == 1) {
                TS_logging$last <- TS_logging$names
            } else {
                TS_logging$last <- TS_logging$header
            }
             ##  Override the previous values when no data are available.
             if (length(TS_logging$names) == 0) {
                 TS_logging$last <- "Nothing here to select"
                 TS_logging$label <- "No groups detected"
             }
             ##  Update the information on the second level too.
             for (.name in TS_logging$names) {
                 TS_logging[[.name]]$names <- sort(names(TS_logging[[.name]]))
                 TS_logging[[.name]]$label <- ifelse(
                     test = length(TS_logging[[.name]]$names) == 1,
                     yes = "Auto-selected the only available time series",
                     no = sprintf("%s time series available",
                                  length(TS_logging[[.name]]$names)))
                 if (length(TS_logging[[.name]]$names) > 1) {
                     TS_logging[[.name]]$header <- "Select a time series"
                 }
                 TS_logging[[.name]]$last <- ifelse(
                     test = {length(TS_logging[[.name]]$names) == 1},
                     yes  = TS_logging[[.name]]$names,
                     no   = TS_logging[[.name]]$header)
                 ##  Override the previous values when no data are available.
                 if (length(TS_logging[[.name]]$names) == 0) {
                     TS_logging[[.name]]$last <- "Nothing here to select"
                     TS_logging[[.name]]$label <- "No time series detected"
                 }
             }
             kill(.name)
###-------------------------------------------------------------------
             ##  Add a top level 'update'-node, with the information
             ##  required in order for the master function to know
             ##  when different workers are required.  Reminder: The
             ##  basic idea in this setup is that 'update' will be a
             ##  list with three nodes, all having names in accordance
             ##  with the interface-workers.  Reminder: The
             ##  'input_triggers'-part of 'update' is a list that will
             ##  collect all the last set values from the time the
             ##  interface was last created, and these are structured
             ##  in such a manner that it is easy to decide where/if a
             ##  new value have been given.  The second component is a
             ##  logical list that will be used to tell the master a
             ##  'TRUE'/'FALSE' with regard to calling the different
             ##  workers.  The third component is a logical list that
             ##  simply states whether or not the desired update is to
             ##  hide something, and this is included to avoid
             ##  duplicating tests in successive functions.  Depending
             ##  on the available data, the different workers will
             ##  decide the status of the next worker(s) in line.
             .zero <- LG_default$spectrum_type_ID
             TS_logging$update <- list(
                 ##  Reminder: The 'input_triggers' is converted to an
                 ##  environment.  It is thus necessary with some
                 ##  additional information in order to sort the input
                 ##  list in a way that makes it easier to work with
                 ##  during development and updates.  The first node
                 ##  is included in order to deal with this.
                 sort_input = c("TS_info", "var_local", "p_diag_bw",
                                "derived_graphical", "plots",
                                "TCS_type", "spectrum_type",
                                "second_graphical", "explanations"),
                 input_triggers = as.environment(list(
                     TS_info = list(TS_key = NA_character_,
                                    TS = NA_character_,
                                    Approx = NA_character_,
                                    Boot_Approx = NA_character_),
                     TCS_type = list(TS_graphic = .zero,
                                     Approx_graphic = .zero,
                                     Spectra_graphic = .zero),
                     var_local = list(Vi = NA_character_,
                                      Vj = NA_character_,
                                      global_local = NA_character_),
                     p_diag_bw = list(type = NA_character_,
                                      point_type = NA_character_,
                                      bw_points = NA_character_),
                     ##  Selectors for spectrum inspection.
                     spectrum_type = structure(
                         .Data = lapply(
                             X = LG_default$spectrum_type_ID,
                             FUN = function(x) LG_default$spectrum_type_zero),
                         .Names = LG_default$spectrum_type_ID),
                     ##  Selectors for time series inspection
                     second_graphical = list(TS_plot = .zero,
                                             TS_acf = .zero,
                                             TS_pacf = .zero,
                                             TS_spec.pgram = .zero,
                                             TS_lags = .zero),
                     plots = list(get_code = FALSE,
                                  window = NA_character_,
                                  cut = NA_character_,
                                  confidence_interval = NA_character_,
                                  frequency_range = NA_character_,
                                  levels_Diagonal = NA_character_,
                                  levels_Line = NA_character_,
                                  levels_Horizontal = NA_character_,
                                  levels_Vertical = NA_character_),
                     explanations = list(explain_interface = FALSE,
                                         explain_plot = TRUE,
                                         show_shiny = TRUE),
                     ## Reminder: This last part is not a part of the
                     ## interface as such. This is used in order to
                     ## simplify some parts of the code that deals
                     ## with values derived from the 'TCS_type'
                     ## and 'second_graphical' parts of this list.
                     derived_graphical = list(TCS_type = NA_character_,
                                              sub_graph_type = NA_character_,
                                              S_type = NA_character_))),
                 worker = list(TS_info = FALSE,
                               TCS_type = FALSE,
                               var_local = FALSE,
                               p_diag_bw = FALSE,
                               second_graphical = FALSE,
                               spectrum_type = FALSE,
                               plots = FALSE,
                               explanations = FALSE))
             ##  Add details to simplify some of the code later on,
             ##  i.e. only look up stuff once.
             TS_logging$update$select_names  <-  list(
                 TS_key = TS_logging$names,
                 TS = NA_character_,
                 Approx = NA_character_,
                 Boot_Approx = NA_character_)
             #####  ## The stuff above will hopefully soon be
             #####  ## removed...
             TS_logging$update$select_Input  <-  structure(
                 .Data = lapply(
                     X = names(TS_logging$update$input_triggers$TS_info),
                     FUN = function(x)
                         list(label = NA_character_,
                              choices = NA_character_,
                              selected = NA_character_)),
                 .Names = names(TS_logging$update$input_triggers$TS_info))
             TS_logging$update$is_bootstrap = FALSE
###-------------------------------------------------------------------
             ##  Figure out if any pre-selected values are present
             ##  in 'data_dir'
             pre_selected <- structure(
                 .Data = as.list(data_dir),
                 .Names = LG_default$folder_defaults[names(data_dir)])
             ##  Add 'TS_key' (when possible).
             if (! is.null(data_dir)) {
                 pre_selected$TS_key <- local(expr = {
                     .match <- vapply(
                         X = TS_logging,
                         FUN = function(x) 
                             pre_selected$TS %in% names(x),
                         FUN.VALUE = logical(1))
                     names(.match)[.match]
                 })
             }
             ##  If not present, add 'Boot_Approx'-node too.
             if (is.null(pre_selected$Boot_Approx))
                 pre_selected$Boot_Approx <- NA_character_
             ##  If possible, update the 'TS_key' and 'TS' part of
             ##  TS_logging' based on 'pre_selected', and update the
             ##  'input_triggers' too.  'pre_selected' can of course
             ##  contain more information that this, but those updates
             ##  can not be performed before the info-object has been
             ##  loaded.
             if (! is.null(pre_selected$TS_key)) {
                 TS_logging$last <- pre_selected$TS_key
                 ## ## ## TS_logging$update$input_triggers$TS_info["TS_key"] <-
                 TS_logging$update$input_triggers$TS_info$TS_key <-
                     TS_logging$last
                 ##  Update the values to be used in 'select_Input'
                 TS_logging$update$select_Input$TS_key$label <- TS_logging$label
                 TS_logging$update$select_Input$TS_key$selected <- TS_logging$last
                 TS_logging$update$select_Input$TS_key$choices <- c(TS_logging$header,
                                                                    TS_logging$names)
             }
             if (!is.null(pre_selected$TS)) {
                 TS_logging[[pre_selected$TS_key]]$last <- pre_selected$TS
                 ## ## ## TS_logging$update$input_triggers$TS_info["TS"] <-
                 TS_logging$update$input_triggers$TS_info$TS <-
                     TS_logging[[pre_selected$TS_key]]$last
                 TS_logging$update$select_names[["TS"]] <-
                     TS_logging[[pre_selected$TS_key]]$names
                 ## ## I think the select_names/select_Input stuff
                 ## ## mentioned here in fact should be superfluous,
                 ## ## given the updates encountered later on.
                 TS_logging$update$select_Input$TS$label <-
                     TS_logging[[pre_selected$TS_key]]$label
                 TS_logging$update$select_Input$TS$selected <-
                     TS_logging[[pre_selected$TS_key]]$last
                 TS_logging$update$select_Input$TS$choices <-
                     c(TS_logging[[pre_selected$TS_key]]$header,
                       TS_logging[[pre_selected$TS_key]]$names)
             }
             ##  Create a logical value '.first_time' to be used later
             ##  on in the code in order for pre-selected values to be
             ##  added the first time an info object is loaded.
             .first_time <- TRUE
         })
###-------------------------------------------------------------------
    ## Add the initial values to the 'input'-object in order for the
    ## logical tests to become simpler during the initiation phase.
    LG_shiny_set_values(.env, .env2)
###-------------------------------------------------------------------    
    ##  Instruct the first worker-function that it is time to act.
    .env$TS_logging$update$worker$TS_info <- TRUE
}
