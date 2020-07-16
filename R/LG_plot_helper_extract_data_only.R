#' Extract only data for the plots
#'
#' @description This internal function is used to extract the
#'     underlying data for a plot created by \code{LG_plot_helper}.
#'
#' @details This function is primarily a helper function used during
#'     the development of new plots.  Some of the scripts might also
#'     use this function, but that is intended as a temporary
#'     solution.
#'
#' @inheritParams LG_plot_helper
#' 
#' @return This internal function will extract the underlying data
#'     from the plot created by the \code{LG_plot_helper}-function.
#'     The result is given as a list containing the following two
#'     nodes.
#'
#' \describe{
#'
#' \item{..env}{An environment containing the underlying data.}
#'
#' \item{look_up}{A list containing the chunks of information that the
#'     internal functions need in order to extract the correct data
#'     for the specified plot.  This list does also include details
#'     needed in order for the explanation of the content of the
#'     plots.}
#'
#' }
#' 
#' @keywords internal

LG_plot_helper_extract_data_only <- function(
    main_dir,
    input,
    input_curlicues = NULL,
    .env) {
    ##  Recipe: Make a copy of the ordinary 'LG_plot_helper'-function,
    ##  and extract a copy of its body.  Modify the
    ##  '.extract_LG_data_only'-argument in the final call of the
    ##  copied body, and update the 'LG_plot_helper'-copy with the
    ##  revised body.  Evaluate this function with the given
    ##  arguments.
    .fun_copy <- LG_plot_helper
    .fun_body <- body(.fun_copy)
    ##  Identify the positions of interest, and update the relevant
    ##  argument value to 'TRUE'
    .pos1 <- length(.fun_body)
    .return <- .fun_body[[.pos1]]
    .pos2 <- which(names(.return) == ".extract_LG_data_only")
    .return[[.pos2]] <- TRUE
    .fun_body[[.pos1]] <- .return
    body(.fun_copy) <- .fun_body
    ##   Run the copy with the given values.
    .fun_copy(
        main_dir = main_dir,
        input = input,
        input_curlicues = input_curlicues,
        .env = .env)
}
