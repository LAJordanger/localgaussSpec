#' Update logged values related to the spectrum-type that is
#' investigated in the interactive \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function is a bookkeeping function that deals with
#'     issues related to the actionButtons that selects the type of
#'     spectrum to investigate.
#'
#' @keywords internal

LG_shiny_S_update <- function(
                              .env,
                              .env2) {
    on.exit(expr = {
        .env$TS_logging$update$worker$spectrum_type <- FALSE})
    ###-------------------------------------------------------------------
    ##  Create links to the environments of interest.
    ..env <- LG_shiny_interface_0_RW_log(.env, .env2, .RW = "L")
    .input_triggers <- .env$TS_logging$update$input_triggers
    ##  Create a bookmark to the relevant spectrum type.
    .bm <- c(.env$input$global_local,
             ifelse(
                 test = {.env$input$Vi == .env$input$Vj},
                 yes  = "auto",
                 no   = "cross"),
             if (.env$input$global_local == "local")
                 .env$input$point_type)
    ##  Check if we have a cross-spectrum investigation and a switch
    ##  between a global and local inspection.  In this case case we
    ##  want the "initiation" of the selected spectrum to carry over.
    .GL_change <- all(.env$input$Vi != .env$input$Vj,
                      .env$xyz$var_local["global_local"])
    ###-------------------------------------------------------------------
    ##  Identify old and new values for the triggers.
    .old_values <- .input_triggers$spectrum_type
    .new_values <- .env$input[names(.old_values)]
    ##  Update the stored values.
    .input_triggers$spectrum_type <- .new_values
    ###-------------------------------------------------------------------    
    ##  In the initiation-phase everything will be '0' in
    ##  '.new_values', and we do not need to update the logged values
    ##  in that case.
    if (sum(unlist(.new_values)) > 0) {
        ##  Detect the position with the difference, and use the name
        ##  of that position to update the logged values.  
        .newest <- names(.new_values)[as.logical(unlist(.new_values) -
                                                 unlist(.old_values))]
        ..env$.Spectrum_type[[.bm]] <- c(
            .newest,
            ..env$.Spectrum_type[[.bm]][1])
    }
    ###-------------------------------------------------------------------
    ##  Update the 'S_type'-part of '.derived_graphical', with the
    ##  twist that we for the cross-spectra stays at the same
    ##  selection if we change between global and local
    .new <- if (.GL_change) {
                ifelse(
                    test = .env$input$global_local == "global",
                    yes  = gsub(
                        pattern = "LS_",
                        replacement = "GS_",
                        x = ..env$.derived_graphical$S_type[1]),
                    no   = gsub(
                        pattern = "GS_",
                        replacement = "LS_",
                        x = ..env$.derived_graphical$S_type[1]))
            } else {
                ..env$.Spectrum_type[[.bm]][1]
            }
    ..env$.derived_graphical$S_type <- c(
        .new,
        ..env$.derived_graphical$S_type[1])
    ##  If the 'S_type' contains two different values, then the
    ##  'plots'-worker should be called to duty.
    if (length(unique(..env$.derived_graphical$S_type)) == 2)
        .env$TS_logging$update$worker$plots <- TRUE
    ###-------------------------------------------------------------------    
    ##  Update the information in the input_trigger.
    .input_triggers$derived_graphical$S_type <- ..env$
        .derived_graphical$S_type[1]
    ##  Update the information the 'input'-lists.
    LG_shiny_set_values(.env, .env2, .part = "derived_graphical")
}
 
