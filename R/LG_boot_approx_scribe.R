################################################################################
#####  2014-10-20

#' Local Gaussian Approximations for bootstrap replicates,
#' scribe-function.
#'
#' This function takes care of the bookkeeping when we want to
#' investigate a time series by means of local Gaussian Approximations
#' used on bootstrapped replicates of the time series.
#' 
#' @details This function records its arguments and compares them to a
#'     previously stored information-object for the time series under
#'     investigation, in order to avoid redoing previously performed
#'     computations.  For new computations, relevant data will be
#'     extracted from \code{data_dir}, which then will be analysed in
#'     order to see if memory issues requires that the computation
#'     should be performed in smaller chunks.  The function then calls
#'     \code{LG_boot_approx} that does the computations, and the
#'     result is then added to the file-structure.  Finally the
#'     information-object will be updated and a two component value is
#'     returned to the work-flow.
#'
#' @details Note that no default values are given for any of the
#'     arguments, and that solution is made in order to dissuade users
#'     from calling this (often quite time consuming) function
#'     directly from the work-space.  The intention is that the
#'     bootstrap-wrapper should call this function, and then with
#'     arguments inherited (restrictions are allowed) from those used
#'     in a previous analysis of a time series.  This is done since
#'     the main motivation for working with bootstrapped replicates of
#'     our original time series is to obtain bootstrap-based
#'     confidence intervals for the Local Gaussian Spectral Densities.
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
#####  Arguments that goes to 'TS_boot_sample'.
#' @template nb_boot
#' @template boot_type_boot
#' @template block_length_boot
#' @template boot_seed_boot
#'
#' @template threshold_arg  
#'
#' @return This function will return a two component list to the
#'     work-flow.  The first component is the logic value
#'     \code{done_before} that reveals whether or not the result
#'     already existed (in the specified file-structure), whereas the
#'     second component \code{data_dir} gives the location of the
#'     saved data.
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
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(data_dir, lag_max, bw_points, nb, boot_type, block_length,
         boot_seed, threshold)
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments (inside of 'spy_envir'),
    ##  and check them against previous computations.
    books <- LG_bookkeeping(spy_report = spy_report)
    
    #####  HERE: Need to make adjustments for three new arguments, and
    #####  perhaps clean out a few of the old ones that now are
    #####  obsolete.
    
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  If the same computations already has been performed, we can
    ##  just return 'books' and nothing more is needed.
    if (books$done_before)
        return(books)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
#####   TASK: If this function should work in general, i.e. also when
#####   not called from the wrapper, then some adjustments must be
#####   made here (and perhaps also in the sanity-checking).  I think
#####   the defaults in those cases, for the arguments 'LG_type' and
#####   'lag_max' might be necessary to either compute from scratch,
#####   or perhaps it might be possible to extract them from some
#####   previously stored 'TS'-object, but not necessarily the one
#####   picked out by the code below.
    ##   print(str(books$info$TS_info$TS))
#####  It is in fact possible to extract the information, but it would
#####  be just as well to simply compute them over again, since the
#####  ones used are the defaults - and it would be clearer to compute
#####  from scratch in order to make the code more transparent.
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------

#####  Rewrite the function based on the revised solution used for
#####  'LG_approx_scribe', and see if that simplifies some this mess.
################################################################################
    ##  Create and evaluate a call for the reproducible computation of
    ##  the bootstrapped sample.  To simplify the feeding of arguments
    ##  into the function, a minor adjustment of `spy_report$envir`
    ##  will be done first.
    spy_report$envir$save_dir <- books$save_dir


    ## TS_boot_sample_call <- create_call(
    ##     .cc_fun = TS_boot_sample,
    ##     spy_report$envir,
    ##     .cc_list = TRUE)

    ## capture_env() 
    
    TS_boot <- eval(create_call(
        .cc_fun = TS_boot_sample,
        spy_report$envir,
        .cc_list = TRUE))
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][["TS_boot"]] <- TS_boot$TS
###-------------------------------------------------------------------
    ##  Compute the auto-correlations.

    

    TS_acr_call <- create_call(
        .cc_fun = TS_acr,
        .TS_info = TS_boot,
        lag_max = spy_report$envir$lag_max)

    
    ## capture_env() 


    .TS_acr <- TS_acr(
        .TS_info = TS_boot,
        lag_max = spy_report$envir$lag_max)
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_acr$.acr_type]] <-
        .TS_acr$.acr_content
    kill(.TS_acr, spy_report)
###-------------------------------------------------------------------
    ##  Create a 'loop_list' to govern the computations, this will
    ##  decrease the chance for memory-related problems.

    ## LG_splitting_call <- create_call(
    ##     .cc_fun = LG_splitting,
    ##     books = books)
    ## capture_env() 
    

    loop_list <- LG_splitting(books)
###-------------------------------------------------------------------
    ##  Create a template for the data-frame needed for information
    ##  about file-names and memory-size (in MB) for the objects.
    L <- length(loop_list) * length(LG_type)
    data_files_df <- LG_default$file_info_df(L = L)
    ##  Reminder: 'L' is also needed later on in the code.
###-------------------------------------------------------------------
    ##  Initiate counter for updating of 'data_files_df'.
    row_nr <- 1
###-------------------------------------------------------------------
    ##  Create list-templates for storing of dimension-names and
    ##  'eflag' (the latter comes from 'localgauss', and it tells us
    ##  if the computation was successful, i.e. whether or not the
    ##  values can be trusted).
    dimnames.par_one <-list()
    dimnames.par_five <-list()
    convergence <-list()
###-------------------------------------------------------------------
    ##  Work through 'loop_list'
    for (part in names(loop_list)) {
        ##  Evaluate a call for 'LG_approx' based on 'part'.
        LG_boot_approx_call <- create_call(
            .cc_fun = LG_boot_approx,
            c(list(TS_boot = TS_boot),
              loop_list[[part]]$arg_list$compute[[1]]),
            .cc_list = TRUE)
        ## capture_env() 
        result <- eval(LG_boot_approx_call)
        kill(LG_boot_approx_call)
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
#####  2017-01-07: The stuff stored here must be updated later on, in
#####  order to keep track of the part that might be present in
#####  'off_diag'.  This is important with regard to the computation
#####  of the splitting later on.  But that can be postponed until the
#####  main part of the code has been updated.
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
#####  2017-01-07: The stuff stored here must be updated later on, in
#####  order to keep track of the part that might be present in
#####  'off_diag'.  This is important with regard to the computation
#####  of the splitting later on.  But that can be postponed until the
#####  main part of the code has been updated.
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
###-------------------------------------------------------------------
        ##  Save 'adjustment', in the correct folder.
        adjustment <- result$adjustment
        save(adjustment,
             file = file.path(
                 books$save_dir,
                 LG_default$global["adjustment"]))
        kill(adjustment)
#####  TASK: Not optimal to save 'adjustment' inside of this loop, but
#####  it hardly matters with regard to the total computational time.
###-------------------------------------------------------------------
        ##  Update 'row_nr' for next loop.
        row_nr <- row_nr + L ##  length(result$LG_type)
    } ##  The end of the loop over 'loop_list'
    kill(row_nr, L, part, loop_list, result,
         save_files.par_one, save_files.par_five)
###-------------------------------------------------------------------
    ##  Add the data-frame and the relevant lists to the info-object.
    books$info[[books$bookmark]][["data_files_df"]] <- data_files_df
    if ("par_one" %in% LG_type)
        books$info[[books$bookmark]][["dimnames.par_one"]] <- dimnames.par_one
    if ("par_five" %in% LG_type)
        books$info[[books$bookmark]][["dimnames.par_five"]] <- dimnames.par_five
    books$info[[books$bookmark]][["convergence"]] <- convergence
    kill(LG_type, dimnames.par_one, dimnames.par_five)
###-------------------------------------------------------------------
    ##  Save the info-object to the info-file.  Remember to extract it
    ##  from 'books' in order to save 'info' instead of 'books$info'.
    info <- books$info
    save(info, file = books$info_path)
###-------------------------------------------------------------------
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
        data_dir = books$data_dir,
        data_files_df = data_files_df))
}
