#' Bookkeeping for the local Gaussian estimates
#'
#' @description This internal function perform the bookkeeping for the
#'     arguments gathered by the \code{spy}-report from the targeted
#'     function, which should be one of the functions
#'     \code{LG_approx_scribe}, \code{LG_boot_approx_scribe}, and
#'     \code{LG_shiny}.
#'
#' @details This function records the given arguments and sends them
#'     to \code{LG_sanity_checks} in order to see if those arguments
#'     are sensible.  Then it takes care of the bookkeeping of the
#'     information stored in the info-file.
#'
#' @template spy_report_arg
#' 
#' @return This function returns a list to the internal workflow, with
#'     some key details related to the computations to be performed.
#'     The pivotal detail of this list is the logical value
#'     \code{done_before}, since this can be used to prevent that
#'     computational time is used to reproduce local Gaussian
#'     estimates that already are available in the file-hierarchy.
#'
#' @keywords internal 

LG_bookkeeping <- function(spy_report) {
    ##  Sanity-check the arguments, update 'spy_report$envir', and add
    ##  'data_dir', 'dir_bookmark', 'folder_defaults', 'info',
    ##  'info_path' and 'save_dir' to the present environment.
    LG_sanity_checks(spy_report = spy_report)
    ##  For the 'LG_shiny'-case, all the work has been done by
    ##  'LG_sanity_checks', so quit this program.
    if (spy_report$fun == "LG_shiny")
        return(invisible(NULL))
    ##  Identify what kind of new stuff that will be created.
    new_stuff <- 
         folder_defaults[setdiff(
            x = names(folder_defaults),
            y = names(data_dir))][1]
    ##  Find a quote for the corresponding level in the info-object.
    ##  Reminder: Direct subsetting did not work well, use quotes!
    level_quote <- bquote(info)
    for (i in seq_along(dir_bookmark))
        level_quote <- bquote(.(level_quote)[[.(dir_bookmark[i])]])
    ##  Restrict attention to the relevant part of 'info'.
    level_data <- eval(level_quote)
    ##  Restrict again to the information about earlier computations.
    level_data <-
        level_data[str_detect(string = names(level_data),
                              pattern = new_stuff)]
    ##  Compare the present spy_report with any previous ones.
    done_before_vec <- seq_along(level_data)
    for (i in seq_along(level_data)) 
        done_before_vec[i] <- 
            compare_spy_reports(
                first_report = spy_report,
                second_report = level_data[[i]]$spy_report,
                ignore_these = LG_default$ignore_these[[names(new_stuff)]])
    kill(i)
    ##  Compute a logic value that is 'TRUE' when the computation has
    ##  been done before.
    done_before <- (sum(done_before_vec) != 0)
    ##  Find/create 'new_dir', where stuff is stored/to be stored.
    if (done_before) {
        ##  Where is the result stored, when already computed.  (We
        ##  assume that no nitwit manually has copied any directories
        ##  at the desired level, i.e. there can at most be one
        ##  earlier recorded occurrence of the present computation.)
        new_dir <- 
            names(level_data)[which(done_before_vec==1)]
    } else {
        ##  Create a "running number" name for new content.
        new_dir <-
            paste(new_stuff,
                  str_sub(
                      paste("__",
                            length(level_data) + 1,
                            sep = ""),
                      start = - 3),
                  sep = "")
        kill(done_before_vec)
        ##  Use 'bquote' and '.()' to update 'info' with 'spy_report'.
        add_spy_report_quote <-
            bquote(
                .(level_quote)[[.(new_dir)]][["spy_report"]] <-
                    spy_report)
        ##  Evaluate 'add_spy_report_quote' to update 'info'.
        eval(add_spy_report_quote)
        kill(add_spy_report_quote, level_quote)
    }
    ##  Add 'new_dir' to data_dir, and update names, but keep a copy
    ##  of the old version.
    old_data_dir <- data_dir
    data_dir <- c(data_dir, new_dir)
    names(data_dir)[length(data_dir)] <- names(new_stuff)
    kill(new_stuff)
    ##  Update 'save_dir' too, and convert to a path.
    ## save_dir <- c(save_dir, new_dir)
    save_dir <- paste(c(save_dir, new_dir),
                      collapse = .Platform$file.sep)
    ##  Create a bookmark for updating later on of the info-object.
    bookmark <- 
        data_dir[names(data_dir) %in%
                 names(folder_defaults)]
    kill(folder_defaults)
    ##  Extract 'data_files_df' based upon the value of 'done_before'.
    ##  Return the files containing the results when 'done_before' is
    ##  'TRUE', otherwise return those containing the data needed for
    ##  the computation.
    data_files_df <- info[[c(
        dir_bookmark,
        if (done_before) {
            new_dir
        } else {
            c()
        },
        "data_files_df")]]
    kill(new_dir)
    ##  Update the column 'data_files' in 'data_files_df' to include
    ##  the full path in the _present_ operative system.
    data_files_df$data_files <- vapply(
        X = data_files_df$data_files,
        FUN = function(x) {
            paste(c(spy_report$envir$main_dir,
                    if (done_before) {
                        data_dir
                    } else 
                        old_data_dir,
                    x),
                  collapse = .Platform$file.sep)
        },
        FUN.VALUE = character(1))
    kill(old_data_dir)
    ##  NOTE: 'data_files_df' will be empty when 'LG_approx_scribe'
    ##  calls this function, but that does not matter since it is not
    ##  used by that function.
    ###------------------------------------------------------###
    ##  Add additional arguments to be used when working upon the
    ##  ordinary (global) autocorrelations.
    additional_args <- list(
        .TS_info = list(
            main_dir = spy_report$envir$main_dir,
            TS = info$TS_info$TS,
            save_dir = data_dir))
    ##  Return the answer.  The format depends on the value of
    ##  'done_before'.  The first time a computation is encountered,
    ##  include 'bookmark', 'info' and 'info_path'.  NOTE: The reason
    ##  'info' and 'info_path' is returned instead of the relevant
    ##  part of 'info', is that the saving of the updated info-file
    ##  will be taken care of by the parent function _after_ the
    ##  computation has been successfully performed and saved.
    return(c(
        list(done_before = done_before,
             main_dir = spy_report$envir$main_dir,
             data_dir = data_dir,
             save_dir = save_dir,
             data_files_df = data_files_df),
        if (! done_before) {
            c(list(info = info,
                   bookmark = bookmark,
                   info_path = info_path),
              additional_args)
        } else {
            list()
        }))
}
