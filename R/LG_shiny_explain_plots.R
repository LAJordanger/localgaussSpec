################################################################################
#'
#' Explain details about the plots created by \code{LG_shiny}.
#'
#' This is a helper function for \code{LG_shiny}, which explains the
#' content of the plots.  This version is still in its infancy, and is
#' at the moment mostly intended as a placeholder.
#'
#' @param main_dir The part of the \code{data_dir}-argument from the
#'     calling function \code{LG_shiny}, that specifies the main path
#'     to where the information is stored.
#'
#' @param input A list containing the arguments needed in order to
#'     perform the extraction.
#'
#' @param show_graphical A logical vector that specifies the kind of
#'     data that should be investigated.  Required in order to
#'     identify the correct array to work upon (since more than one
#'     can be available).
#'
#' @param path_selection_list A list containing the components needed
#'     in order to identify the correct array to work upon (since more
#'     than one can be available).
#' 
#' @param .env An environment in which \code{.arr} will be updated.
#'     This argument can be skipped when called in a non-interactive
#'     setting.
#'
#' @return An explanation will be returned.
#' 
#' @export


LG_shiny_explain_plots <- function(
    main_dir,
    input,
    show_graphical,
    path_selection_list,
    .env) {
###-------------------------------------------------------------------
    ##  Create assorted information about the interface.  Strategy:
    ##  Create a vector and convert it to html.
###-------------------------------------------------------------------
    ##  Create helper functions for the markdown-syntax.
    .italics <- function(.text) paste("_", .text, "_", sep = "")
    .bold <- function(.text) paste("__", .text, "__", sep = "")
    .item <- function(.text, n = 1) {
        if (is.null(.text))
            return(NULL)
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
###-------------------------------------------------------------------
    ##  Create information about the interface.
    .main_header <- .header(
        n = 3,
        .text = "Explanation of plot:")
###-------------------------------------------------------------------
    ##  Procedure when no graphical component has been selected.
    if (! any(show_graphical)) {
        .info_vector <- c(
            .main_header,
            "No plot detected.")
        .result <- HTML(renderMarkdown(
            text = .info_vector,
            encoding = "UTF-8"))
        return(.result)
    }
###-------------------------------------------------------------------
    if (input$get_code) {
        ##  Extend '.main_header' with an explanation about the code
        ##  that now should be displayed.
        .main_header <- c(
            .main_header,
            paste("The code for the plot is now displayed.",
                  "  Copy this code if you want to use the plot in",
                  " a non-interactive setting.  Note that the resulting",
                  " plot can be saved to an object and that it then is",
                  " possible to extract an attribute named ",
                  sQuote("details"),
                  " from it.  The intention of this attribute is that it",
                  " should be used with ",
                  sQuote("knitr"),
                  " to enable inline explanations of the details in the plot.  ",
                  sep = ""))
    }
###-------------------------------------------------------------------
    ##  Avoid the requirement that the `shiny`-package must be loaded
    ##  when we only want a non-reactive plot (e.g. in a paper).
    if (! "reactivevalues" %in% is(input)) {
        isolate <- function(x) x
        is.reactive <- function(x) FALSE
        input$get_code <- FALSE
        if (missing(.env))
            .env <- new.env()
    }
###-------------------------------------------------------------------
    ##  Identify the unique graphical component to investigate.
    .selected <- names(show_graphical)[show_graphical]
###-------------------------------------------------------------------
    ##  An adjustment to ensure that this function can accept both
    ##  reactive and non-reactive arguments, with the added benefit
    ##  that it's easier to work with it in the development phase.
    .input <- vector(mode = "list", length = length(input))
    for (.name in isolate(names(input)))
        .input[[.name]] <- isolate(input[[.name]])
    kill(input)
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##     ##  The following cases are still not implemented.
## ## ## ##     if (.selected %in% c("Approx", "Spectra", "Boot_Approx"))
## ## ## ##         return()
###-------------------------------------------------------------------
    ##  Get hold of `info`.  Ensure that we use a non-reactive version
    ##  (when this function is called from `LG_shiny`) or load it from
    ##  file in the non-reactive case.  Note: The present construction
    ##  extracts `NULL` from `.env` in the non-reactive case.
    .info <- .env[["info"]]  
    if (is.reactive(.info)) {
        .info <- isolate(.info())
    } else {
        ##  Load from file in the non-interactive case, using the name
        ##  `.info` to match the interactive case.
        LG_load(.file = file.path(
                    paste(main_dir,
                          collapse = .Platform$file.sep),
                    path_selection_list$new$TS,
                    LG_default$info_file_name),
                .name = ".info")
    }
###-------------------------------------------------------------------
    ##  Use 'LG_lookup' to create an updated version of '.input' from
    ##  which the relevant information can be extracted.

    ##  Use 'LG_lookup' to find the three values '.block',
    ##  '.only_diagonal' and '.multivariate', three values related to
    ##  points under investigation '.Horizontal', '.Vertical' and
    ##  '.Shape', and finally '.bookmark', 'path_all' and 'info_part'
    ##  that contains details about the whereabouts of the files
    ##  needed for the computations later on.

    ## LG_lookup_call <- create_call(
    ##     .cc_fun= LG_lookup,
    ##     .info = .info,
    ##     .selected = .selected,
    ##     .path_selection_list = path_selection_list,
    ##     .input = .input)

    ## capture_env() 
    
    ## if (.selected == "Approx")
    ##     capture_env() 
    
    LG_lookup(.info = .info,
              .selected = .selected,
              .path_selection_list = path_selection_list,
              .input = .input)
    ##  Get rid of superfluous stuff.
    kill(.info, .path_selection_list)
###-------------------------------------------------------------------
    ##  Add information that only depends on the time series under
    ##  investigation, i.e. the length, how many variables that are
    ##  present, and if it is based on simulated or real data.
    
    ## capture_env() 

    ## LG_explain_plot_call = create_call(
    ##     .cc_fun = LG_explain_plot,
    ##     .plot_details = .input$details)
    ## capture_env() 
    
    .result <- HTML(renderMarkdown(
        text = LG_explain_plot(.input$look_up$details),
        encoding = "UTF-8"))
    return(.result)

    
    
################################################################################
    ##  Below: The original solution
    
###-------------------------------------------------------------------
    if (.selected == "TS") {
        .info_vector <- c(.main_header,
                          "Sorry: This has  not been implemented yet")
    }
    if (.selected == "Approx") {
        .info_vector <- c(.main_header,
                          .item(.details$text$content),
                          .item(.details$text$computations),
                          .item(NULL))
    }
    if (.selected == "Boot_Approx") {
        .info_vector <- c(.main_header,
                          "Sorry: This has  not been implemented yet")
    }
    if (.selected == "Spectra") {
        .info_vector <- c(.main_header,
                          "Sorry: This has  not been implemented yet")
    }
    if (.selected == "Boot_Spectra") {
        .info_vector <- c(.main_header,
                          .item(.details$text$content),
                          .item(.details$text$computations),
                          NULL)
        ## capture_env() 
    }
###-------------------------------------------------------------------
    ##  Return the desired information as html-code.
    
    ##  Reminder for the final selection.
    .result <- HTML(renderMarkdown(
        text = .info_vector,
        encoding = "UTF-8"))
    return(.result)

}



## > str(.details)
## List of 16
##  $ TS_key                   : chr "rugarch"
##  $ TS                       : Named chr [1:2] "1251eefebdd1e934f6960bfd9e4a561b" "TS.Rda"
##   ..- attr(*, "names")= chr [1:2] "" "TS"
##  $ block                    : logi TRUE
##  $ details                  : NULL
##  $ N                        : int 1974
##  $ .variables               : chr "Y"
##  $ .nr_variables            : int 1
##  $ save_dir                 : Named chr "1251eefebdd1e934f6960bfd9e4a561b"
##   ..- attr(*, "names")= chr "ts.dir"
##  $ nr_simulated_samples     : num 100
##  $ N_text                   : chr "simulated data of length 1974, univariate observations."
##  $ boot                     : logi FALSE
##  $ confidence_interval      : logi TRUE
##  $ confidence_interval_value: num 90
##  $ confidence_interval_text : chr "90\\% confidence interval based on 100 independendt samples."
##  $ bandwidth                : chr "1"
##  $ truncation_level         : num 10

