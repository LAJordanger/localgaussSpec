#' Create the part of the \code{LG_shiny}-interface that explains it.
#'
#' @description This internal function calls the functions that print
#'     the explanations of the interface and the plots.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @param .explain Either \code{interface}, \code{plot} or
#'     \code{show_shiny}, decides which part to be updated.
#'
#' @return This interface-function adds some explanations to the
#'     \code{LG_shiny}-interface.
#'
#' @keywords internal

LG_shiny_interface_explanations <- function(
    .env,
    .env2,
    .explain) {
    ##  Use the 'eval'+ 'bquote' + '.()' construction to update the
    ##  relevant part of the 'output'-list in '.env2'.
    if (.explain == "interface") 
        eval(bquote(
            output$Explain_Interface  <- renderPrint({
                LG_shiny_explain_interface(
                    input = .(.env$input),
                    .env = .(.env),
                    .env2 = .(.env2))
            })),
            envir = .env2)
    if (.explain == "plot") 
        eval(bquote(
            output$Explain_Plot  <- renderPrint({
                LG_shiny_explain_plots(
                    input = .(.env$input),
                    .env = .(.env))
            })),
            envir = .env2)
    if (.explain == "show_shiny") {
        ##  This is only intended to be used during development,
        ##  i.e. the button that can be used to select this will not
        ##  be a part of the final package.
        .result <- paste("\n\t",
                         paste(names(.env$input), " = ", .env$input, collapse = "\n\t"),
                         sep = "")
        eval(bquote(
            output$internal_status <- renderPrint(
                cat("Values from input:",
                    .(.result),
                    "\n\n"))),
            envir = .env2)
    }
}
