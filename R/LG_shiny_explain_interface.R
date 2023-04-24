#' Create the instructions for \code{LG_shiny}.
#'
#' @description This internal function is a helper-function for
#'     \code{LG_shiny}, that creates some explanations that can be
#'     added to the interface.  This version is still in its infancy,
#'     i.e. it is mostly a placeholder.
#'
#' @param input A list containing the arguments needed in order to
#'     perform the extraction.
#'
#' @param .env The environment where the original arguments given to
#'     \code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
#'     \code{data_dir}.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return An explanation will be returned.
#'
#' @keywords internal

LG_shiny_explain_interface <- function(
    input,
    .env,
    .env2) {
    return("The explanation of the interface is not there yet...")
    
    ### As an intermezzo between old and new solution, some stuff must
    ### be extracted from '.env' and 'env2'.

    ##  Create assorted information about the interface.  Strategy:
    ##  Create a vector and convert it to html.

    ##  Create helper functions for the markdown-syntax.
    .italics <- function(.text) paste("_", .text, "_", sep = "")
    .bold <- function(.text) paste("__", .text, "__", sep = "")
    .item <- function(n = 1, .text) {
        .indentation <- paste(
            paste(rep(x = " ", times = 4*(n-1)),
                  collapse = ""),
            ifelse(test = {n == 1},
                   yes  = "* ",
                   no   = "+ "),
            collapse = "")
        paste(.indentation,
              .text,
              sep = "")
    }
    .header <- function(n, .text = "") 
        paste(
            paste(rep("#", n), collapse = ""),
            " ",
            .text,
            sep = "")
    ##  Create information about the interface.
    .main_header <- .header(
        n = 3,
        .text = "Explanation of interface:")
    TS_item <- .item(
        n = 1,
        .text = paste(
            .bold("TS:"),
            "Properties of the (pseudo-normalised) time series"))
    TS_sub_item <- .item(n=2,
                        .text = "This is a test...  ")
    ##  Create  the desired info-vector.
    .info_vector <- c(
        .main_header,
        TS_item,
        TS_sub_item,
        "further testing...")
    ##  Convert it to html-code
    .result <- HTML(mark(
        text = .info_vector))
    
    return(.result)
    
    ##  Information about the buttons.
    .TS_button <- paste(
        "The ",
        sQuote("TS"),
        "-button gives information about the (pseudo-normalised) version ",
        "of time series under investigation.",
        "()")

    .result <- HTML(mark(
        text = .info_vector))

    return(cat("Select a component to inspect!"))

}
