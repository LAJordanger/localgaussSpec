#' Local Gaussian Approximations, scribe-function
#'
#' @details This function takes care of the bookkeeping when the local
#'     Gaussian correlations is computed for the lag-h pairs for a
#'     time series.  It calls the function \code{LG_approx} when
#'     required, and will then feed that function the relevant
#'     arguments, the result is saved to file and the
#'     information-object will be updated.  A list with key-details is
#'     always returned to the work-flow.
#' 
#' @template main_dir_arg
#' @template data_dir_arg
#' @template TS_arg
#' @template lag_max_arg
#' @template LG_points_arg
#' @template bws_mixture_arg
#' @template bw_points_arg
#' @template bws_fixed_arg
#' @template bws_fixed_only_arg
#' @template content_details_arg
#' @template LG_type_arg
#' 
#' @return This function is a scribe that reads and records
#'     information to info-files.  When a new computation is required,
#'     the task is sent to \code{LG_approx}, and the result is then
#'     saved to file.  A list containing the following key-information
#'     is returned to the work.flow.
#'
#' \describe{
#'
#' \item{done_before}{Logical value that reveals if the computation
#' has been performed before.}
#'
#' \item{main_dir}{The \code{main_dir}-argument is included here.}
#'
#' \item{data_dir}{The \code{data_dir}-argument is included here.}
#'
#' }
#' 
#' @export

LG_approx_scribe <- function(
    main_dir,
    data_dir,
    TS,
    lag_max = ceiling(3*sqrt(length(TS))),
    LG_points,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = c(25, 35),
    .bws_fixed = NULL,
    .bws_fixed_only = FALSE,
    content_details = c("rho_only", "rho_log.fun", "rho_all"),
    LG_type = c("par_five", "par_one")) {
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(lag_max, bw_points, content_details)
    ##  Sanity-check and update the arguments, and check them against
    ##  previous computations.  Note: This also adjusts
    ##  'spy_report$envir' by adding 'save_dir' based on 'main_dir'
    ##  and 'data_dir'.
    books <- LG_bookkeeping(spy_report = spy_report)
    ##  If the same computation already has been performed, we can
    ##  just return the relevant parts of 'books'.
    if (books$done_before)
        return(books[c("done_before", "main_dir", "data_dir")])
    ##  Compute the (ordinary global) auto-correlations.
    .TS_acr <- TS_acr(
        .TS_info = books$.TS_info,
        lag_max = spy_report$envir$lag_max)
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_acr$.acr_type]] <-
        .TS_acr$.acr_content
    kill(.TS_acr)
    ##  Reminder: The setup below is a bit superfluous for the present
    ##  solution, but it could be of interest to build upon this later
    ##  on if memory-related partitions of the task should be included
    ##  at this part of the computation.
    loop_list <-
        list(list(
            data_files = NA_character_, #  In this case no file to load.
            arg_list = as.list(spy_report$envir,
                               all.names = TRUE)))
    names(loop_list) <- " "  #  Since its length is one.
    ##  Create a template for the data-frame needed for information
    ##  about file-names and memory-size (in MB) for the objects.
    L <- length(loop_list) * length(LG_type)
    data_files_df <- LG_default$file_info_df(L = L)
    ##  Initiate counter for updating of 'data_files_df'.
       row_nr <- 1
    ##  Create list-templates for storing of dimension-names and
    ##  'eflag' (the latter comes from 'localgauss', and it tells us
    ##  if the computation was successful, i.e. whether or not the
    ##  values can be trusted).
    dimnames.par_one <-list()
    dimnames.par_five <-list()
    convergence <-list()
    ##  Work through 'loop_list'
    for (part in names(loop_list)) {
        ##  Evaluate a call for 'LG_approx' based on 'part'.
        result <- eval(create_call(
            .cc_fun = LG_approx,
            loop_list[[part]]$arg_list,
            .cc_list = TRUE))
        ##  When required, do the 'par_one' case.
        if ("par_one" %in% result$LG_type) {
            ##  Save data for 'par_one', register the file-name.
            save_files.par_one <- LG_save(
                data = result$par_one_data,
                data_type = "approx_save",
                LG_type = "par_one",
                part = part,
                save_dir = books$save_dir)
            ##  Update 'dimnames.par_one', such that it contains the
            ##  information about the part stored in the file.
            dimnames.par_one <-
                c(dimnames.par_one, list(dimnames(result$par_one_data$on_diag)))
            names(dimnames.par_one)[length(dimnames.par_one)] <-
                save_files.par_one
            ##  Add information about 'par_one' to 'data_files_df'.
            data_files_df[row_nr, ] <- 
                list(part_a_of_b = part,
                     content = "par_one",
                     data_files = save_files.par_one,
                        object_size_MB=
                            as.numeric(object.size(result$data.0) / 1024^2))
        } 
        ##  When required, do the 'par_five' case.
        if ("par_five" %in% result$LG_type) {
            ##  Save data for 'par_five', register the file-name.
            save_files.par_five <- LG_save(
                data = result$par_five_data,
                data_type = "approx_save",
                LG_type = "par_five",
                part = part,
                save_dir = books$save_dir)
            ##  Update 'dimnames.par_five', such that it contains the
            ##  information about the part stored in the file.
            dimnames.par_five <-
                c(dimnames.par_five, list(dimnames(result$par_five_data$on_diag)))
            names(dimnames.par_five)[length(dimnames.par_five)] <-
                save_files.par_five
            ##  Add information about 'par_five' to 'data_files_df'.
            data_files_df[row_nr + (L - 1), ] <- 
                list(part_a_of_b = part,
                     content = "par_five",
                     data_files = save_files.par_five,
                     object_size_MB=
                         as.numeric(object.size(result$data.0) / 1024^2))
            ##  Add information about the convergence (based on 'eflag').
            convergence <- c(
                convergence,
                list(vapply(X = result$par_five_data,
                   FUN = function(x) {
                       if (! is.null(attributes(x)$convergence)) {
                           attributes(x)$convergence
                       } else
                           TRUE
                   },
                   FUN.VALUE = logical(1))))
            names(convergence)[length(convergence)] <- save_files.par_five
        } 
        ##  Update 'row_nr' for next loop.
        row_nr <- row_nr + L ##  length(result$LG_type)
    } ##  The end of the loop over 'loop_list'
    kill(row_nr, L, part, loop_list, result,
         save_files.par_one, save_files.par_five)
    ##  Add the data-frame and the relevant lists to the info-object.
    books$info[[books$bookmark]][["data_files_df"]] <- data_files_df
    if ("par_one" %in% LG_type)
        books$info[[books$bookmark]][["dimnames.par_one"]] <- dimnames.par_one
    if ("par_five" %in% LG_type)
        books$info[[books$bookmark]][["dimnames.par_five"]] <- dimnames.par_five
    books$info[[books$bookmark]][["convergence"]] <- convergence
    kill(LG_type, dimnames.par_one, dimnames.par_five)
    ##  Save the info-object to the info-file.  Remember to extract it
    ##  from 'books' in order to save 'info' instead of 'books$info'.
    info <- books$info
    save(info, file = books$info_path)
    ##  Modify 'data_files_df$data_files' to include the full path in
    ##  the _present_ operative system.
    data_files_df$data_files <- file.path(
        books$save_dir, 
        data_files_df$data_files)
    ##  Return information about 'done_before', 'main_dir' and
    ##  'data_dir' to the workflow.
    return(list(
        done_before = books$done_before,
        main_dir = books$main_dir,
        data_dir = books$data_dir))
}
