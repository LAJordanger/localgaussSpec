#' Master for the creation of the dynamic \code{LG_shiny}-interface.
#'
#' @description This internal function is the master-function that
#'     calls different helper-functions required for the dynamic
#'     \code{LG_shiny}-interface and the associated bookkeeping.
#'     Depending on the status of the application, it will either
#'     initiate the log, or it will update the log and allow different
#'     workers to do the tasks of loading files, fine-tuning different
#'     parts of the interface, and calling the functions that presents
#'     the plots and the explanations of the plots.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function governs the workers that provides the dynamic
#'     interface for the \code{LG_shiny}-function.  (The resulting
#'     updates are stored in the environments \code{.env} and
#'     \code{.env2}.)
#'
#' @keywords internal

LG_shiny_interface_Master <- function(.env, .env2) {
    ##  Create an updated nonreactive version of the 'input'-values.
    .env$input <- reactiveValuesToList(x = .env2$input,
                                       all.names = TRUE)
    ##  Create a list containing the names of 'output'-nodes that at
    ##  some times should be hidden away from the interface.  This is
    ##  mainly for the use of the 'TS_info'-worker, since the
    ##  remaining functions themselves will activate or hide the
    ##  relevant parts.
    .env$output_nodes <- list(
        TS_info = c("TS_key", "TS", "Approx", "Boot_Approx"),
        TCS = "TCS_type",
        CS = c("var_local", "p_diag_bw",  "levels"),
        S = c("spectrum_type", "spectrum_arguments"),
        ass = c("graphs", "graphs_call", "Explain_Plot"))
    ##  The first time this function is called, it workers will create
    ##  the object 'TS_logging' (in '.env').  This object is central
    ##  for the dynamic interface to work properly, since residual
    ##  'input'-values can wreak havoc if there is no bookkeeping to
    ##  help things stay on track when different branches are
    ##  investigated.  Note that this initial test also contains an
    ##  escape-test in order to avoid problems if there is nothing
    ##  there to investigate.
    if (!exists(x="TS_logging", envir = .env, inherits = FALSE)) {
        ##  Check if something is available for inspection.
        if (length(.env$TS_content) == 0) {
            ##  Return a message if there is nothing to see.
            .Hoodwinked_text <- renderText(sprintf(
                fmt="You have been hoodwinked! %sAn empty list was found in the file '%s'.%s",
                "<br>",
                paste(c(.env$main_dir,
                        LG_default$content_file_name),
                      collapse = .Platform$file.sep),
                "<br>"))
            eval(expr = bquote(output$Hoodwinked <- .(.Hoodwinked_text)),
                 envir = .env2)
            return(NULL)
            ##  REMINDER: Need to tweak this Hoodwinked-text later on
            ##  if it is to look decent when printed.
        } else {
            ##  Create the 'TS_logging' object, and add the initial
            ##  values to the 'input'-object in order for the logical
            ##  tests to become simpler during the initiation phase.
            LG_shiny_interface_0_create_log(.env, .env2)
            .env$counter <- 1
        }
    } else {
        .env$counter <- .env$counter + 1
        ##  Create a list that detects the differences between stored
        ##  and logged input-parameters.  Reminder: The 'xyz'-list is
        ##  used in some of the functions in order to keep track of
        ##  switching between sub-branches.
        .env$xyz <- list()
        for (.name in names(.env$TS_logging$update$input_triggers)) {
            .triggers <- .env$TS_logging$update$input_triggers[[.name]]
            .env$xyz[[.name]] <- vapply(
                X = names(.triggers),
                FUN = function(x)  {
                    ##  Reminder: The test for the numerical values
                    ##  was selected due getting a quick fix to the
                    ##  issue that the numerical values might be
                    ##  stored as numeric and integer at different
                    ##  places in the code.  The 'any'-part is
                    ##  included since some of the stored numerical
                    ##  values have length larger than 1.
                    if (is.numeric(.triggers[[x]])) {
                        any(.triggers[[x]] != .env$input[[x]])
                    } else
                        ! identical(x = unclass(.triggers[[x]]),
                                    y = unclass(.env$input[[x]]))
                },
                FUN.VALUE = logical(1))
            .env$TS_logging$update$worker[[.name]] <- any(.env$xyz[[.name]])
            ##  Another ad hoc solution, to ensure that the updating
            ##  of the triggers for the "plots"-case is taken care.
            if (.env$TS_logging$update$worker$plots) {
                ##  Identify the part to update, and update it.
                .update_this <- names(.env$xyz$plots)[.env$xyz$plots]
                .env$TS_logging$update$input_triggers$plots[[.update_this]] <- .env$input[[.update_this]]
                kill(.update_this)
            }
        }
        kill(.name, .triggers)
        ##  Reminder: The iterative nature of the shiny-application,
        ##  perhaps triggered by the 'input'-update performed by some
        ##  of the worker-functions, does alas trigger this function
        ##  to be called immediately after an update.  In these cases
        ##  none of the workers should be called, and the following
        ##  termination procedure is thus included.
        if (!any(unlist(.env$TS_logging$update$worker))) {
            return(NULL)
        }
        ##  Reminder: If the test above did not terminate this
        ##  function, then the relevant worker-functions will now be
        ##  called.  Each function will update the 'worker'-node
        ##  status according to the 'input'-parameters and the
        ##  available information in 'TS_logging'.  This implies that
        ##  the workers will hand the task of creating the
        ##  interface/plots onwards as far as possible.
    }
    ##  Reorder the content of the input-copy in order to make it
    ##  easier to inspect during development/updates.
    .names_from_input_triggers <- 
        unlist(lapply(X = .env$TS_logging$update$sort_input,
                      FUN = function(x)
                          names(.env$TS_logging$update$input_triggers[[x]])))
    .other_names <- setdiff(x = names(.env$input),
                            y = .names_from_input_triggers)
    .env$input <- c(.env$input[.names_from_input_triggers],
                    .env$input[.other_names])
    kill(.names_from_input_triggers, .other_names) 
    ##  When required call the 'TS_info'-worker.  This function
    ##  creates the interface for 'TS_key', 'TS', 'Approx', and
    ##  'Boot_Approx'.  If sufficient information is available, then
    ##  it will also create the 'TCS_type'-output and trigger the
    ##  'TCS_type'-worker to perform further updates of the interface.
    if (.env$TS_logging$update$worker$TS_info) {
        LG_shiny_interface_1_TS_info(.env, .env2)
    }
    ##  When required call the 'TCS_type'-worker.  This function
    ##  creates the buttons and sliders that describes the plot to be
    ##  investigated.
    if (.env$TS_logging$update$worker$TCS_type) {
        LG_shiny_TCS_input(.env, .env2)
    }
    if (.env$TS_logging$update$worker$var_local) {
        LG_shiny_CS_input(.env, .env2, .part = "p_diag_bw")
    }
    if (.env$TS_logging$update$worker$p_diag_bw) {
        LG_shiny_CS_input(.env, .env2, .part = "levels")
    }
    ##  When required call the 'S_update'-worker, which takes care of
    ##  the effects due to the actionButtons that selects the type of
    ##  spectrum to investigate.
    if (.env$TS_logging$update$worker$spectrum_type) {
        LG_shiny_S_update(.env, .env2)
    }
    ##  When required call the 'plots'-worker.  This function creates
    ##  the plot (or the code needed to re-create the )
    if (.env$TS_logging$update$worker$plots) {
        LG_shiny_interface_plots(.env, .env2)
    }
    ##  When required call the 'explanation'-worker.
    if (.env$input$explain_interface) {
        LG_shiny_interface_explanations(.env, .env2, .explain="interface")
    }
    if (.env$input$explain_plot) {
        LG_shiny_interface_explanations(.env, .env2, .explain="plot")
    }
    if (isTRUE(.env$input$show_shiny)) 
        LG_shiny_interface_explanations(.env, .env2, .explain="show_shiny")
}
