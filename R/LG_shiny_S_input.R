#' Create the spectrum-specific part of the \code{LG_shiny}-interface.
#'
#' @description This internal function is used to update the interface
#'     created by \code{LG_shiny}, in order for the investigation to
#'     focus on the local Gaussian (auto- and cross-) spectra.
#' 
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#' 
#' @note This function creates the two output-nodes, "spectrum_type"
#'     and "spectrum_arguments".  Both nodes will be created in the
#'     initial phase, whereas only the "spectrum_type" needs to be
#'     updated later on (since different spectra will occur depending
#'     on auto- and cross-versions of the spectra, and points on or
#'     off the diagonal).  The internal workings of this function thus
#'     requires a comparison of old and new input-parameters.  Note
#'     that all of these are equal in the initiation phase.
#'
#' @return This function creates the \code{actionButtons} needed for
#'     the spectrum-specific part of the \code{LG_shiny}-interface.
#'     This includes the selection of the type of spectrum to
#'     investigate (auto- or cross-spectrum, with possible
#'     complex-valued results), and the arguments to be used when
#'     computing the different spectra (frequency-range, truncation
#'     point and lag-window-function).
#'
#' @keywords internal

LG_shiny_S_input <- function(.env,.env2) {
    ##  Create links to the environments of interest, where the
    ##  information needed for the interfaces can be found.
    ..env <- LG_shiny_interface_0_RW_log(.env, .env2, .RW = "L")
    .input_triggers <- .env$TS_logging$update$input_triggers
    ##  The internal workings of this function requires a comparison
    ##  of some old and new input-parameters.  Note that all of these
    ##  are equal in the initiation phase.  Reminder: The basic idea
    ##  is that both of the interfaces should be created during the
    ##  initiation phase, and that only the 'spectrum_type' interface
    ##  should be updated later on.  Reminder: This test is also used
    ##  in the 'CS_input'-function, and the saving of the input values
    ##  has thus been taken care of there
    .initial <- !any(unlist(.env$xyz[c("var_local", "p_diag_bw")]))
    if (.initial) {
        ##  Create the "spectrum_arguments"-part of the interface, by
        ##  defining the ingredients in the order they should be
        ##  listed in the output.
        .spectrum_interface  <-  list(
            .sliderInputs = c("cut", "frequency_range"),
            .radioButtons = "confidence_interval",
            .selectInputs = "window")
        ##  For the update on the input_triggers and input-lists, it
        ##  is necessary to know where the pivotal information is
        ##  stored as 'value' and where it is stored as 'selected'.
        .value_part <- .spectrum_interface$.sliderInputs
        .selected_part <- c(
            .spectrum_interface$.radioButtons,
            .spectrum_interface$.selectInputs)
        ##  Extract all the information from '..env'
        .stored_values <- new.env()
        lapply(X = names(.spectrum_interface),
               FUN = function(x) 
                   lapply(X = .spectrum_interface[[x]],
                          FUN = function(y)
                              .stored_values[[y]] <- ..env[[x]][[y]]))
        ##  Update the information in the input_triggers and in the
        ##  'input'-lists.  Reminder: Only the 'plots'-part of the
        ##  input_triggers needs to be updated.
        for (.v in .value_part) {
            .input_triggers$plots[[.v]] <- .stored_values[[.v]]$value
        }
        for (.s in .selected_part) {
            .input_triggers$plots[[.s]] <- .stored_values[[.s]]$selected
        }
        ##  Update the 'input'-lists too.
        LG_shiny_set_values(.env, .env2, .part = "plots")
        kill(.v, .value_part, .s, .selected_part)
        ##  Create the 'frequency_arguments'-interface.
        .result <- list()
        for (x in .spectrum_interface$.sliderInputs) {
            .result[[x]] <- sliderInput(
                inputId = x,
                label   = .stored_values[[x]]$label,
                min     = .stored_values[[x]]$min,
                max     = .stored_values[[x]]$max,
                value   = .stored_values[[x]]$value,
                step    = if (x == "cut") {
                              1
                          } else
                              .5/500,
                animate = if (x == "cut") {
                              animationOptions(interval = 500, loop = TRUE)
                          } else
                              FALSE)
        }
        for (x in .spectrum_interface$.radioButtons) {
            .result[[x]] <- radioButtons(
                inputId = x,
                label = .stored_values[[x]]$label,
                choices = .stored_values[[x]]$choices,
                selected = .stored_values[[x]]$selected,
                inline = TRUE)
        }
        for (x in .spectrum_interface$.selectInputs) {
            .result[[x]] <- selectInput(
                inputId = x,
                label = .stored_values[[x]]$label,
                choices = .stored_values[[x]]$choices,
                selected = .stored_values[[x]]$selected,
                width = 333)
        }
        ##  Use the 'bquote' + '.()' construction to update the
        ##  'output'-list that lives in '.env2'
        eval(expr = bquote(output$spectrum_arguments  <-
                               renderUI({.(.result)})),
             envir = .env2)
        kill(.result, .spectrum_interface, .stored_values, x)
    }
    ##  The next part will be performed regardless of the value of
    ##  '.initial', i.e. it is the creation of the 'actionButtons'
    ##  needed for the selection of the "spectrum_type".  We need to
    ##  figure out if it for the non-initial case has been a change
    ##  between a global or a local investigation, or if we have moved
    ##  between univariate or bivariate.  For the local case it is
    ##  also of necessary to see if we are on or off the diagonal.
    ###------------------------------------------------------###
    ##  Find the names of the IDs to be used for the actionButtons of
    ##  interest for the present configuration.
    .ID  <- paste(
        GL = ifelse(
            test = {.env$input$global_local == "local"},
            yes  = "LS_",
            no   = "GS_"),
        ac = ifelse(
            test = {.env$input$Vi == .env$input$Vj},
            yes  = "a",
            no   = "c"),
        ##  Reminder: Shorter list only of interest for the two
        ##  situations "global and autospectrum" or "local and
        ##  autospectrum and on diagonal".
        type = if (any(
            all(.env$input$global_local == "global",
                .env$input$Vi == .env$input$Vj),
            all(.env$input$global_local == "local",
                .env$input$Vi == .env$input$Vj,
                .env$input$point_type == "on_diag"))){
                   ""   
               } else 
                   c("", "_Co",  "_Quad", "_amplitude", "_phase"),
        ## c("", "_amplitude", "_phase", "_Co",  "_Quad"),
        sep = "")
    ##  An ad hoc solution for a quick fix of adjust the interface,
    ##  done in order to get rid of some redundant buttons.
    .ID <- setdiff(x = .ID,
                   y = c("LS_a", "LS_c", "GS_a", "GS_c"))
    ##  Give the collection of all the possible labels, using
    ##  subsetting to restrict when only the first is needed.
    .Labels <- c("Co", "Quad", "alpha", "phi")[seq_along(.ID)]
    ###-------------------------------------------------------------------    
    ##  Create/update the 'frequency_type'-interface.
    .result <- list()
    for (.id in seq_along(.ID))
        .result[[.id]] <- actionButton(
            inputId = .ID[.id],
            label = .Labels[.id])
    ##  Adjustment for the case where no buttons are present:
    if  (length(.result)==0)
        .result <- NULL
    ##  Use the 'eval'+ 'bquote' + '.()' construction to update the
    ##  'output'-list that lives in '.env2'
    eval(expr = bquote(output$spectrum_type <-
                           renderUI({.(.result)})),
         envir = .env2)
    kill(.result, .ID, .Labels)
    ###-------------------------------------------------------------------
    ##  Reminder: The creation of actionButtons resets the previously
    ##  stored values, which implies that the stored values also must
    ##  be reset for the comparisons later on to work properly
    for (.s in names(.input_triggers$spectrum_type))
        .input_triggers$spectrum_type[[.s]] <- LG_default$spectrum_type_zero
    LG_shiny_set_values(.env, .env2, .part="spectrum_type")
    ##  Set the 'spectrum_type'-worker to 'TRUE', so the updating of
    ##  the logging-structure can be performed in the next function.
    .env$TS_logging$update$worker$spectrum_type <- TRUE
}
