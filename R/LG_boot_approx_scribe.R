#' Local Gaussian Approximations for bootstrap replicates, scribe-function.
#'
#' @description This function takes care of the bookkeeping when the
#'     local Gaussian (auto- and cross-) correlations are computed for
#'     the lag-h pairs for bootstrapped replicates of a given time
#'     series.
#' 
#' @details This function records its arguments and compares them to a
#'     previously stored information-object for the time series under
#'     investigation, in order to avoid redoing previously performed
#'     computations.  The function then calls \code{LG_boot_approx}
#'     when a new computation is required, the result is then saved to
#'     file, and the information-object is updated with the key
#'     details.
#'
#' @details Note that default values are not given for any of the
#'     tuning parameters of the local Gaussian estimation algorithm.
#'     The basic idea is that such arguments only should be specified
#'     for the bootstrap-part if it is of interest to restrict the
#'     attention to a subset of the tuning parameters that were used
#'     for the local Gaussian investigation that was done on the
#'     original sample.  When an argument is left unspecified, the
#'     bookkeeping-system will look up the value that was used during
#'     the investigation of the original sample, and that value will
#'     then be inherited to the present investigation.
#'
#' @template main_dir_arg
#' @template data_dir_arg
#' @template lag_max_boot
#' @template LG_points_boot
#'
#' @template content_details_boot
#'
#' @template LG_type_arg
#' @template bws_mixture_arg
#' @template bw_points_arg
#' @template bws_fixed_arg
#' @template bws_fixed_only_arg
#' 
#' @template nb_boot
#' @template boot_type_boot
#' @template block_length_boot
#' @template boot_seed_boot
#'
#' @template threshold_arg  
#'
#' @return This function is a scribe that reads and records
#'     information, whereas another function performs the actual
#'     computation, see details for further information.  A list
#'     containing the following key-information is always returned to
#'     the workflow.
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

LG_boot_approx_scribe <- function(
    main_dir,
    data_dir,
    lag_max = NULL,
    LG_points = NULL,
    content_details = NULL,
    LG_type = NULL,
    .bws_mixture = NULL,
    bw_points = NULL,
    .bws_fixed = NULL,
    .bws_fixed_only = NULL,
    nb = NULL,
    boot_type = NULL,
    block_length = NULL,
    boot_seed = NULL,
    threshold = 500) {
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(data_dir, lag_max, bw_points, nb, boot_type, block_length,
         boot_seed, threshold)
    ##  Sanity-check and update the arguments (inside of 'spy_envir'),
    ##  and check them against previous computations.
    books <- LG_bookkeeping(spy_report = spy_report)
    ##  If the same computation already has been performed, we can
    ##  just return the relevant parts of 'books'.
    if (books$done_before)
        return(books[c("done_before", "main_dir", "data_dir")])
    ##  Create and evaluate a call for the reproducible computation of
    ##  the bootstrapped sample.  To simplify the feeding of arguments
    ##  into the function, a minor adjustment of `spy_report$envir`
    ##  will be done first, i.e. it must be reassigned in order to
    ##  point one level deeper into the file-hierarchy,
    spy_report$envir$save_dir <- books$save_dir
    ##  Compute the (ordinary global) auto- or cross-correlations.
    TS_boot <- eval(create_call(
        .cc_fun = TS_boot_sample,
        spy_report$envir,
        .cc_list = TRUE))
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][["TS_boot"]] <- TS_boot$TS
    ##  Compute the (ordinary global) auto-correlations.
    .TS_acr <- TS_acr(
        .TS_info = TS_boot,
        lag_max = spy_report$envir$lag_max)
    ##  Add information to the 'info'-object.
    books$info[[books$bookmark]][[.TS_acr$.acr_type]] <-
        .TS_acr$.acr_content
    kill(.TS_acr)
    ##  Create a 'loop_list' to govern the computations, this will
    ##  decrease the chance for memory-related problems.
    loop_list <- LG_splitting(books)
    ##  Create a template for the data-frame needed for information
    ##  about file-names and memory-size (in MB) for the objects.
    LG_type <- spy_report$envir$LG_type
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
            .cc_fun = LG_boot_approx,
            c(list(TS_boot = TS_boot),
              loop_list[[part]]$arg_list$compute[[1]]),
            .cc_list = TRUE))
        ##  When required, do the 'par_one' case.
        if ("par_one" %in% result$LG_type) {
            ##  Save data for 'par_one', register the file-name.
            save_files.par_one <- LG_save(
                data = result$par_one_data,
                data_type = "approx_save",
                LG_type = "par_one",
                bootstrap = TRUE,
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
                bootstrap = TRUE,
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
        row_nr <- row_nr + L
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
###-------------------------------------------------------------------
    ##  Return information about 'done_before' and 'save_file'.
    return(list(
        done_before = books$done_before,
        main_dir = books$main_dir,
        data_dir = books$data_dir))
}
