#' Writes the code needed for the calling of \code{LG_shiny}
#'
#' @template main_dir_arg
#'
#' @param data_dir The directory-component of the result from
#'     \code{LG_approx}.
#'
#' @param con A connection object or a character string, default value
#'     is \code{stdout()}.  This argument is used when this function
#'     calls \code{writeLines}.
#'
#' @return This function writes a few lines of code that contains the
#'     details needed in order to start \code{LG_shiny} with the
#'     arguments \code{main_dir} and \code{data_dir}.  Depending on
#'     the value of \code{con}, the result will either be sent to the
#'     terminal or it will be saved to file.
#'
#' @export

LG_shiny_writeLines <- function(main_dir, data_dir, con=stdout()) {
    ##  Split 'main_dir' into a vector, to avoid the presence of
    ##  OS-dependent file separators.
    .main_dir <- unlist(strsplit(x = main_dir,
                                split = .Platform$file.sep))
    ##  Create a suitable character string
    .main_dir <- sprintf(
        "main_dir <- c(%s)\n",
        paste(
            paste("\"",
                  .main_dir,
                  "\"",
                  sep = ""),
            collapse = ", "))
    ##  The 'data_dir' argument must be treated slightly different
    ##  since the names also must be preserved.
    .data_dir <- vapply(
        X = seq_along(data_dir),
        FUN = function(x) {
            sprintf("%s = \"%s\"",
                    names(data_dir)[x],
                    data_dir[x])
        },
        FUN.VALUE = character(1))
    ##  Create a suitable character string, with indentation.
    .indentation <- 4
    .data_dir <- sprintf(
        "data_dir <-\n%sc(%s)\n\n",
        strrep(" ",
               .indentation),
        paste(.data_dir,
              collapse = paste(
                  ",\n",
                  strrep(" ",
                         .indentation + 1))))
    ##  Create a vector for the content, with extra line-breaks at the
    ##  beginning and end if the result is to be sent to the terminal.
    .content <- c(
        if (!is.character(con))
            "\n",
        "library(localgaussSpec)",
        "\n\n",
        .main_dir,
        .data_dir,
        "LG_shiny(\n",
        "    main_dir = main_dir,\n",
        "    data_dir = data_dir)",
        if (!is.character(con))
            "\n")
    ##  Return the result.
    writeLines(
        text = paste(.content,
                     collapse=""),
        con = con)
}
