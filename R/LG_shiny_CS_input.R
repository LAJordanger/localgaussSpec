#' Create \code{radioButtons} and \code{sliderInputs} for the \code{LG_shiny}-interface.
#'
#' @description This internal function creates the
#'     \code{LG_shiny}-interface.  It does also deal with details
#'     related to the internal logs, which enables switching between
#'     different branch to work better.
#' 
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @param .part One of the values \code{c("var_local", "p_diag_bw",
#'     "levels")}.  This decides which parts of the
#'     interface that should be updated.
#'
#' @return This function creates the \code{radioButtons} and
#'     \code{sliderInputs} for the \code{LG_shiny}-interface.
#'
#' @keywords internal

LG_shiny_CS_input <- function(.env,
                              .env2,
                              .part = c("var_local", "p_diag_bw",
                                        "levels")) {
    on.exit({.env$TS_logging$update$worker$plots <- TRUE})
    ##  The internal workings of this function requires a comparison
    ##  of old and new input-parameters.  Note that all of these are
    ##  equal in the initiation phase.
    .initial <- !any(unlist(.env$xyz[c("var_local", "p_diag_bw")]))
    ##  Log the information from 'input' when not '.initial'.
    if (!.initial)
        LG_shiny_interface_0_RW_log(.env, .env2, .RW = "W")
    ##  Create a link to the environment of interest, where the
    ##  information needed for the fine tuning of the interface can be
    ##  found.  (Needed in order to simplify the interface when
    ##  e.g. only one value is present.)
    ..env <- LG_shiny_interface_0_RW_log(.env, .env2, .RW = "L")
    .input_triggers <- .env$TS_logging$update$input_triggers
    ##  Check if some parts of the interface should be removed, and
    ##  use the 'eval' + 'bquote' + '.()' construction to update the
    ##  'output'-list in '.env2' when required.
    if (.env$input$global_local == "global") {
        for (.node in c("p_diag_bw", "levels")) 
            eval(expr = bquote(output[[.(.node)]] <- NULL),
                 envir = .env2)
        kill(.node)
    }
    ##  Initiate the structures to contain information about the
    ##  desired 'radioButtons' and 'sliderInputs', and include a
    ##  structure to specify if some parts of the interface can be
    ##  simplified.
    .new_radioButtons <- list()
    .new_sliderInputs <- list()
    ##  Work through the different cases to figure out what parts that
    ##  should be updated.  Modifications should only be done when
    ##  required.
    if (.part == "var_local") {
        ##  Add the required buttons.
        .new_radioButtons$var_local  <- c("global_local", "Vi", "Vj")
        ##  Check if the other parts also should be included.
        if (.env$input$global_local == "local") 
            .part <- "p_diag_bw"
    }
    if (.part == "p_diag_bw") {
        ##  Decide if the detected changes in 'var_local' requires an
        ##  actual update of the interface.
        .update <- if (.initial) {
                       TRUE
                   } else 
                       all(.env$input$global_local == "local",
                           .env$xyz$var_local["global_local"])
        ##  Add new 'radioButtons' when required.
        if (.update)
            .new_radioButtons$p_diag_bw  <- c("type", "point_type", "bw_points")
        ## Update '.part' so also "levels" can be checked.
        .part <- "levels"
    }
    if (.part == "levels") {
        ##  Decide if the detected changes in 'var_local' and
        ##  'p_diag_bw' requires an actual update of the interface.
        .update <- if (.initial) {
                       TRUE
                   } else {
                       any(all(.env$input$global_local == "local",
                               .env$xyz$var_local["global_local"]),
                           .env$xyz$p_diag_bw["point_type"])
                   }
        if (.update) {
            ##  Investigate the type of slider that should be used.
            .new_sliderInputs <-
                if (.env$input$point_type == "on_diag") {
                    "levels_Diagonal"
                } else
                    c("levels_Horizontal", "levels_Vertical")
            ##  Reminder: I should think about the case 'levels_Line'
            ##  to be used when points lies on a line outside of the
            ##  diagonal.
        }
    }
    kill(.part, .initial, .update)
    ## For the '.radioButtons': Start out by first creating all the
    ## interfaces, then check to see if simplifications are possible.
    for (.name in names(.new_radioButtons)) {
        .stored_values <- ..env$.radioButtons[.new_radioButtons[[.name]]]
        ##  Update the information stored in the input_triggers and in
        ##  the input-lists.
        for (.value in .new_radioButtons[[.name]])  {
            .input_triggers[[.name]][[.value]] <-
                .stored_values[[.value]]$selected
            LG_shiny_set_values(.env, .env2, .part = .name)
        }
        ##  Create the list with the desired radioButtons-content.
        .result <- structure(
            .Data = lapply(
                X = .new_radioButtons[[.name]],
                FUN = function(x) {
                    radioButtons(
                        inputId = x,
                        label = .stored_values[[x]]$label,
                        choices = .stored_values[[x]]$choices,
                        selected = .stored_values[[x]]$selected,
                        inline = TRUE)
                }),
            .Names = .new_radioButtons[[.name]])
        ##  Check if simplifications can be used.
        if (all(.name == "var_local", ..env$.simplify_logical$Vi)) {
            .result$Vi <- ..env$.simplify_text$Vi
            .result$Vj <- NULL
        }
        if (.name == "p_diag_bw") {
            ##  Identify if any parts can be simplified
            .simpl <- intersect(
                x = names(..env$.simplify_logical)[unlist(..env$.simplify_logical)],
                y = names(.result))
            for (.i in .simpl)
                .result[[.i]] <- list(HTML(..env$.simplify_text[[.i]]))
        }
        ##  Use the 'bquote' + '.()' construction to update the
        ##  'output'-list that lives in '.env2'
        eval(expr = bquote(output[[.(.name)]] <-
                               renderUI(.(.result))),
         envir = .env2)
    }
    kill(.i, .name, .simpl, .value, .stored_values, .new_radioButtons,
         .result)
    ## Add the '.sliderInputs' (when required)
    if (length(.new_sliderInputs)>0) {
        .stored_values <- ..env$.sliderInputs[.new_sliderInputs]
        ##  Create the list with the desired sliderInput-content.
        .result <- structure(
            .Data = lapply(
                X = .new_sliderInputs,
                FUN = function(x) {
                    sliderInput(
                        inputId = x,
                        label   = .stored_values[[x]]$label,
                        min     = .stored_values[[x]]$min,
                        max     = .stored_values[[x]]$max,
                        value   = .stored_values[[x]]$value,
                        step    = 1,
                        animate = 
                            animationOptions(interval = 500, loop = TRUE))
                }),
            .Names = .new_sliderInputs)
        ##  Use the 'bquote' + '.()' construction to update the
        ##  'output'-list that lives in '.env2'
        eval(expr = bquote(output$levels <-
                               renderUI(.(.result))),
             envir = .env2)
    }
    kill(.stored_values, .result, .new_sliderInputs)
    ##  Call the next function depending on the value of the graphical
    ##  trigger.  Reminder: This function is only called if the
    ##  trigger is 'C' or S', so we do not need to worry about the
    ##  'T'-case here.
    if (..env$.derived_graphical$TCS_type[1] == "C") {
        LG_shiny_C_input(.env, .env2)
    } else
        LG_shiny_S_input(.env, .env2)
}
