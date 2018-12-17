################################################################################
#' Local Gaussian Approximations, scribe-function.
#'
#' This function takes care of the bookkeeping when we investigate a
#' time series by means of Local Gaussian Approximations.
#' 
#' @details This function records its arguments and compares them to a
#'     previously stored information-object for the time series, in
#'     order to avoid redoing previously performed computations.  For
#'     new computations, relevant data will be extracted from
#'     \code{data_dir}, which then will be analysed in order to see if
#'     memory issues requires that the computation should be performed
#'     in smaller chunks.  The function calls \code{LG_approx} in
#'     order to obtain the result, before it ads it to the
#'     file-structure.  Finally the information-object will be updated
#'     and a two component value is returned to the work-flow.
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
##### TASK: Update the description of the result, this is wrong.
#' @return The result of this function will depend upon the value of
#'     \code{bootstrap}.  The default case, \code{bootstrap=FALSE},
#'     will create two files with respectively the zero-lag and
#'     positive-lag estimated local Gaussian approximations, where we
#'     later on in particular will use the parameters for the local
#'     Gaussian auto-correlations and auto-covariances.  Furthermore,
#'     the info-object corresponding to the time series will be
#'     updated, and information about the new save-directory will be
#'     returned to the work-flow.  Note that the info-object will
#'     store information about all the previous computations, and a
#'     check will be performed at the beginning in order to ensure
#'     that we avoid identical computations.
#'
#'     When \code{bootstrap=FALSE}, i.e. when this function is called
#'     by \code{LG_boot_approx}, then the specified data will be
#'     delivered directly to that function -- which then bundles
#'     together all the parts to one larger object.  In this case
#'     \code{LG_boot_approx} will take care of all the file-handling.
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
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(lag_max, bw_points, content_details)
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments, and check them against
    ##  previous computations.  Note: This also adjusts
    ##  'spy_report$envir' by adding 'save_dir' based on 'main_dir'
    ##  and 'data_dir'.
    books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  If the same computations already has been performed, we can
    ##  just return 'books' and nothing more is needed.
    if (books$done_before)
        return(books)
###-------------------------------------------------------------------
    
    ## ## TS_acr_call <- create_call(
    ## ##     .cc_fun = TS_acr,
    ## ##     .TS_info = books$.TS_info,
    ## ##     lag_max = spy_report$envir$lag_max)

    ## ## capture_env() 
    
    ##  Compute the (ordinary global) auto-correlations.
    .TS_acr <- TS_acr(
        .TS_info = books$.TS_info,
        lag_max = spy_report$envir$lag_max)
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_acr$.acr_type]] <-
        .TS_acr$.acr_content
    kill(.TS_acr)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
##
#####  TASK: Include test in order to see if the computation should be
#####  partitioned into several pieces.  Perhaps not necessary in this
#####  first function?
###  Let's think about this for a moment, in order to initiate the new
###  regime with regard to storing of information about saved files, I
###  will need to start here with a possible 'split into chunks' code.
###  The main thing of interest must be to create argument array to
###  loop over.  The first column should be "part_a_of_b" (or only ""
###  when no splitting is required), then I need the files to
###  investigate, and finally I need a list with the restricted
###  arguments, to be delivered to the formals of the function of
###  interest.
##
##  At the moment, just create a suitable list to loop over, no
##  need to use names on this list.  Later on this should be the
##  created by a 'do we need to split this due to memory-issues'
##  function.
#####  To keep this in line with the other scribe-versions, I think I
#####  would like to use a list instead of an environment here - even
#####  though it seems far fetched that a splitting should be necessary
#####  to do at this case.
    loop_list <-
        list(list(
            data_files = NA_character_, #  In this case no file to load.
            arg_list = as.list(spy_report$envir,
                               all.names = TRUE)))   ####  arg_list))
    names(loop_list) <- " "  #  Since its length is one.
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

    ## capture_env() 
    
    for (part in names(loop_list)) {
        ##  Evaluate a call for 'LG_approx' based on 'part'.
        
        ## LG_approx_call <- create_call(
        ##     .cc_fun = LG_approx,
        ##     loop_list[[part]]$arg_list,
        ##     .cc_list = TRUE)
        ## capture_env() 

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
