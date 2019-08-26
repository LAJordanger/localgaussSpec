#' Link, read, or write, to the deep levels of \code{TS_logging}
#'
#' @param .env The environment where \code{TS_logging} lives.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.  This argument is only required when we want
#'     to update the \code{input}-list.
#'
#' @param .RW Either \code{L} (Link), \code{R} (Read) or \code{W}
#'     (Write).  This argument specifies if we should provide a link
#'     to the environment of interest, should read from it or write to
#'     it.  If left unspecified, the default will be \code{L}.
#'
#' @return When the \code{.RW}-argument is \code{L}, the function will
#'     return a link to the environment-node containing the values to
#'     be used when creating the deeper parts of the
#'     \code{shiny}-interface.  The value \code{R} implies that the
#'     stored data will be used to update the internal
#'     \code{input_triggers} and \code{input}-lists, whereas the value
#'     \code{W} instead writes from these objects to the environment. 
#' @keywords internal



LG_shiny_interface_0_RW_log <- function(.env, .env2,
                                        .RW = c("L", "R", "W")) {
###-------------------------------------------------------------------
    ##  Restrict '.RW' to length one (default "L") if not specified.
    .RW <- .RW[1]
    ##  Create a shortcut to the environment of interest, by first
    ##  using the 'TS_info' to create a bookmark.  Reminder: The last
    ##  value could be 'NA' (no bootstrap), and a minor test for that
    ##  is thus required.
    .input_triggers <- .env$TS_logging$update$input_triggers
    .bm <- unlist(.input_triggers$TS_info)
    ##  Identify the environment for the approx-node, and then adjust
    ##  it if a proper "Bootstrap_node" also is present.
    ..env <- .env$TS_logging[[unlist(head(.bm,-1))]]
    if (! any(isTRUE(stringr::str_detect(
                           string = .bm["Boot_Approx"],
                           pattern = "[Ss]elect")),
              is.na(.bm["Boot_Approx"]))) {
        ..env <- ..env[[.bm["Boot_Approx"]]]
    }
    ##  Return the environment if '.RW' is "L"
    if (.RW == "L")
        return(..env)
    ##  Read from the environment if '.RW' is "R", and use this to
    ##  update the 'input_triggers' and 'input'-lists.
    if (.RW == "R") {
        ##  Update the 'input_triggers' and the 'input'_lists based on
        ##  the values stored in '..env' (at the nodes
        ##  '.derived_graphical', '.actionButtons', '.radioButtons'
        ##  and '.sliderInputs').  Reminder: Only some parts of the
        ##  'input_triggers' are updated here, i.e. 'TS_info' is taken
        ##  care of at a higher level, and the 'explanations' are
        ##  something that I want to keep out of this updating (at
        ##  least for the time being).
        .input_triggers$derived_graphical <- 
            lapply(X = ..env$.derived_graphical,
                   FUN = function(x) x[1])
        ##  Specify the parts related to the 'actionButtons'
        .action_Buttons_part <- c("TCS_type",
                                  "second_graphical", "spectrum_type")
        ##  Specify the remaining targets.
        .targets <- setdiff(
            x = names(.input_triggers),
            y = c("TS_info",
                  "derived_graphical",
                  .action_Buttons_part,
                  "explanations"))
        for (.t in .action_Buttons_part) {
            for (.n in names(.input_triggers[[.t]])) 
                .input_triggers[[.t]][[.n]] <-
                    ..env$.actionButtons[[.n]]
        }
        kill(.action_Buttons_part, .t, .n)
        ##  Extract the desired content from '..env'.
        .from_radioButtons <- structure(
            .Data = lapply(
                X = names(..env$.radioButtons),
                FUN = function(x)
                    ..env$.radioButtons[[x]]$selected),
            .Names = names(..env$.radioButtons))
        .from_selectInputs <- structure(
            .Data = lapply(
                X = names(..env$.selectInputs),
                FUN = function(x)
                    ..env$.selectInputs[[x]]$selected),
            .Names = names(..env$.selectInputs))
        .from_sliderInputs <- structure(
            .Data = lapply(
                X = names(..env$.sliderInputs),
                FUN = function(x)
                    ..env$.sliderInputs[[x]]$value),
            .Names = names(..env$.sliderInputs))
        ##  Loop over '.targets' and update the content in the
        ##  'input_triggers'.
    
        for (.t in .targets) {
            for (.n in names(.input_triggers[[.t]])) {
                if (.n == "get_code")
                    next
                .input_triggers[[.t]][[.n]] <-
                    if (.n %in% names(.from_radioButtons)) {
                        .from_radioButtons[[.n]]
                    } else {
                        if (.n %in% names(.from_selectInputs)) {
                            .from_selectInputs[[.n]]
                        } else
                            .from_sliderInputs[[.n]]
                    }
            }
        }
        kill(.targets, .from_radioButtons, .from_sliderInputs, .t, .n)
        ##  Update the 'input'-lists too.
        LG_shiny_set_values(.env, .env2)
        return(NULL)
    }
###-------------------------------------------------------------------
    ##  Still running: Then we need to write to the environment based
    ##  on the values from 'input' that are different from 'NA'.
    ##  (Reminder: This 'NA'-part is included in order to avoid
    ##  problems during the initiation phase, where 'NA'-values are
    ##  used as placeholders.)
    .new_values <- .env$input[!is.na(.env$input)]
    ##  Update the 'input_triggers' in the 'TS_logging'-object.
    for (.name in names(.input_triggers))
        for (.var in names(.input_triggers[[.name]]))
            .input_triggers[[.name]][[.var]] <-
                .env$input[[.var]]
    ##  Update the 'radioButtons' and the 'sliderInputs' (the values
    ##  for the 'actionButtons' are not relevant).  Start out by
    ##  identifying the parts of input that contains relevant
    ##  information to use in the updating
    .nodes <- c(".radioButtons", ".sliderInputs")
    .update_these <- structure(
        .Data = lapply(
            X = .nodes,
            FUN = function(x) {
                intersect(x = names(.new_values),
                          y = names(..env[[x]]))
            }),
        .Names = .nodes)
    ##  Reminder: The values from the input sliders turned out to have
    ##  been stored as characters instead of numeric values, and a
    ##  modification is thus necessary in order to rectify this.
    for (.i in names(..env$.sliderInputs))
        .new_values[[.i]] <- as.numeric(.new_values[[.i]])
    ##  Reminder: The two nodes have different structures with regard
    ##  to having 'selected' or 'value' as the node containing the
    ##  information of interest.  Need a solution that takes that into
    ##  account when the updates are to be performed.
    for (.i in .nodes) {
        .update <- ifelse(
            test = {.i == ".radioButtons"},
            yes  = "selected",
            no   = "value")
        for (.key in .update_these[[.i]])
            ..env[[.i]][[c(.key, .update)]] <- .new_values[[.key]]
    }
}
