#' Create the plot-related part of the \code{LG_shiny}-interface.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This function creates the plot (or the code to create it
#'     outside of the \code{LG_shiny}-application).
#'
#' @keywords internal

LG_shiny_interface_plots <- function(
    .env,
    .env2) {
    on.exit(expr = {.env$TS_logging$update$worker$plots <- FALSE})
    ##  Use the plot-helper to create the result, which either will be
    ##  a plot or the code to create the plot outside of shiny.
    .result  <- LG_plot_helper(
        input = .env$input,
        .env = .env)
    ##  Use the 'eval'+ 'bquote' + '.()' construction to update the
    ##  relevant part of the 'output'-list in '.env2'.  Reminder:
    ##  There are two cases, either we want the plot itself, or we
    ##  want the code needed in order to recreate the plot (e.g. if it
    ##  is to be used in a paper).
    if (.env$input$get_code) {
        eval(bquote(output$graphs_call  <- renderPrint(cat(.(.result),
                                                           sep = "\n"))),
             envir = .env2)
    } else {
        eval(bquote(output$graphs <- renderPlot(.(.result))),
             envir = .env2)
    }
}

