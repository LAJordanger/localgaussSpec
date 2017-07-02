#' Bootstrap-estimated statistics for local Gaussian spectral
#' densities, scribe-function
#'
#' This function takes care of the bookkeeping when we use
#' bootstrap-techniques to investigate the local Gaussian spectra of a
#' time series.
#'
#' @details This function records its arguments and compares them to a
#'     previously stored information-object, in order to avoid redoing
#'     previously performed computations.  For new computations,
#'     relevant data will be extracted from \code{data_dir}, which
#'     then will be analysed in order to see if memory issues requires
#'     that the computations should be performed in smaller chunks.
#'     The function then calls \code{LG_approx_extract} and
#'     \code{LG_boot_spectra} in order to obtain the result -- and
#'     finally the result is added to the file-structure while the
#'     information-object is updated.
#'
#' @template main_dir_arg
#' @template data_dir_arg
#' @template spectra_dir_arg
#' @template LG_type_boot
#' @template omega_vec_boot
#' @template window_boot
#' @template cut_vec_boot
#' @template all_statistics_arg
#' @template log__arg
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


LG_boot_spectra_scribe <- function(
    main_dir,
    data_dir,
    spectra_dir,
    LG_type = NULL,
    omega_vec = NULL,
    window = NULL,
    cut_vec = NULL,
    all_statistics = FALSE,
#####  I wonder, should I add a list of arguments here instead for the
#####  kind of statistics that we want to compute, in order for this
#####  to become more fine-tuned?
    log_ = FALSE,
#####  TASK: Think more about how this threshold should work.
    threshold = 500) {
#############---------------------------------------------------------
###  This computation can, depending on the number of
###  bootstrap-replicates, require some intermediate objects that
###  requires much more memory than the result.  The size of the
###  largest intermediate object will be the size of the original
###  object (scaled down by the extraction, which collapses 'variable'
###  down to one vale) scaled up with the product of
###  'omega_length_out' (or the length of 'omega_vec'), and the
###  lengths of 'window' and 'cut_vec'.  It will then decrease as we
###  estimate the local Gaussian spectra by summing along 'lag', and
###  finally the 'bootstrap' dimension will be replaced by the
###  statistics based upon it.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create the spy-report.
    spy_report <- spy()
    kill(main_dir, data_dir, omega_vec, window, cut_vec,
         all_statistics, log_, threshold)
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments, and check them against
    ##  previous computations.
    books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  If the same computation already has been performed, we can
    ##  just return 'books' and nothing more is needed.
    if (books$done_before)
        return(books)
###-------------------------------------------------------------------
    ##  Compute (and save) the ordinay (global) spectral densities.
    .TS_spectral <- TS_spectral(
        acr_data = books$.acr_data,
        omega_vec = spy_report$envir$omega_vec,
        window = spy_report$envir$window,
        cut_vec = spy_report$envir$cut,
        .method = "all")
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_spectral$.spectral_type]] <-
        .TS_spectral$.spectral_content
    ##  Compute the desired statistics needed for the confidence
    ##  intervals to be produced.

    ## TS_CI_call <- create_call(
    ##     .cc_fun = TS_CI,
    ##     main_dir = spy_report$envir$main_dir,
    ##     spectral_content = .TS_spectral$.spectral_content,
    ##     all_statistics = spy_report$envir$all_statistics,
    ##     log_ = spy_report$envir$log_)
    ## capture_env() 
    
    .TS_CI <- TS_CI(
        main_dir = spy_report$envir$main_dir,
        spectral_content = .TS_spectral$.spectral_content,
        all_statistics = spy_report$envir$all_statistics,
        log_ = spy_report$envir$log_)
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_CI$.spectral_type]] <-
        .TS_CI$.spectral_content
    kill(.TS_spectral, .TS_CI)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Load 'adjustment' into the work-flow.
    load(file = file.path(spy_report$envir$save_dir,
                          LG_default$global["adjustment"]))
    kill(spy_report)
###-------------------------------------------------------------------
    ##  Update the info-object by inserting cross-referring bookmarks.
    ##  Reminder: We must skip the first component of `spectra_dir`,
    ##  since that refers to the "level above" the `info`-object.
    spectra_bookmark <- 
        spectra_dir[names(spectra_dir) %in%
                    names(LG_default$folder_defaults)][-1]
    ##---
    books$info[[spectra_bookmark]][["boot_spectra_bookmark"]] <-
        books$bookmark
    ##---
    books$info[[books$bookmark]][["spectra_bookmark"]] <-
        spectra_bookmark
    kill(spectra_dir, spectra_bookmark)
    ##  NOTE: The present incarnation of the code only allows one
    ##  connection between 'spectra' and 'bootstrap_spectra', which I
    ##  think should be sufficient for my purposes.
###-------------------------------------------------------------------
    ##  Create a 'loop_list' to govern the computations, this will
    ##  decrease the chance for memory-related problems.

    ## LG_splitting_call <- create_call(
    ##     .cc_fun = LG_splitting,
    ##     books = books)
    ## capture_env() 
    
    loop_list <- LG_splitting(books)
###-------------------------------------------------------------------
    ##  Create a template for the data-frame that will store
    ##  file-names and memory-size in MB for the saved objects.
    data_files_df <- LG_default$file_info_df(
        L = length(loop_list) * length(LG_type))
###-------------------------------------------------------------------
    ##  Initiate counter for updating of 'data_files_df'.
    row_nr <- 1
###-------------------------------------------------------------------
    ##  Create list-template for storing of dimension-names. The main
    ##  list will have one or two sub-lists depending on 'LG_type'.
    dimnames_spectra <- vector(mode = "list", length = length(LG_type))
    names(dimnames_spectra) <- LG_type
    for (type in LG_type)
        dimnames_spectra[[type]] <- list()
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Reminder: The Local Gaussian Information about 'par_one' and
###  'par_five' will be stored under different names in different
###  files, so an intermediate environment storage technique will be
###  used to get data from the files into 'approx_data'.  This frees
###  us from worrying about the names used to save the data - as long
###  as only one object is stored in each file.  The trick is to
###  create environments and then load the content of the files into
###  these environments.  The correct subsetting needed to extract the
###  content can then be obtained by using 'ls' on our intermediate
###  environment, and we're done.
#############---------------------------------------------------------
###-------------------------------------------------------------------

    ## ## capture_env() 
    
    for (part in seq_along(loop_list)) {
        ##  Loop over the types given in 'LG_type'.
        for (type in LG_type) {
            ##  Use intermediate environment storage, restrict loading
            ##  of relevant data to the 'type' under investigation.
            par_one.env <- new.env()
            par_five.env <- new.env()
            ##  Record if additional restrictions are required.
            restrict_more <- loop_list[[part]]$restrict_ingoing_necessary

##### 2017-04-04: Horrible ad-hoc solution that might not work in
##### general.  Point, there is a need for a revision of the way the
##### approx-information is saved into the info-object (today only the
##### on-diagonal part seems to be present), and that is no good.
            restrict_more <- FALSE
            
            ##  Compose data to be given to 'LG_approx_extract',
            ##  restrict to the case of interest, i.e. 'type', in
            ##  order to save memory.
            LG_boot_approx_data  <- list(
                LG_type = type,
                par_one_data =
                    if ("par_one" %in% type) {
                        load(loop_list[[part]]$data_files["par_one"],
                             envir = par_one.env)
                        if (restrict_more) {
                            .copy <- par_one.env[[ls(par_one.env)]]
                            par_one.env[[ls(par_one.env)]] <- 
                                restrict_array(
                                    .arr = .copy,
                                    .restrict = loop_list[[part]]$restrict_ingoing)
                            kill(.copy)
                        }
                        ##
                        par_one.env[[ls(par_one.env)]]
                    },
                par_five_data =
                    if ("par_five" %in% type) {
                        load(loop_list[[part]]$data_files["par_five"],
                             envir = par_five.env)
                        if (restrict_more) {
                            .copy <- par_five.env[[ls(par_five.env)]]
                            par_five.env[[ls(par_five.env)]] <- 
                                restrict_array(
                                    .arr = .copy,
                                    .restrict = loop_list[[part]]$restrict_ingoing)
                            kill(.copy)
                        }
                        ##
                        par_five.env[[ls(par_five.env)]]
                    },
                adjustment = adjustment)
            kill(par_one.env, par_five.env, restrict_more)
            ##  Extract the data needed by 'LG_spectra'.
            LG_approx_extract_data <- LG_approx_extract(
                approx_data = LG_boot_approx_data,
                LG_type = type,
                .method = "all")
            kill(LG_boot_approx_data)
###-------------------------------------------------------------------
            ##  Extract pieces from 'loop_list' to simplify the code
            ##  in the call to 'foreach'.
            pieces <- loop_list[[part]]$arg_list$pieces
            arg_list <- loop_list[[part]]$arg_list$compute
            ## ## ## ## subset_list <- loop_list[[part]]$arg_list$subset
###-------------------------------------------------------------------
            ##  Create a local copy of 'list_array_join' (with the required
            ##  arguments) for 'array_nodes' and '.class' to be used
            ##  with 'foreach' when combining the pieces.
            update_formals(.fun = list_array_join,
                           array_nodes = localgaussSpec:::LG_default$result$array_nodes,
                           .class =  localgaussSpec:::LG_default$class$array)
#####  REMINDER: 'localgaussSpec:::LG_default' is needed here since the
#####  formals are updated in a function from another package.
#####  However, this solution triggers a note from devtools::check,
#####  and as such an alternative approach must be applied, I guess it
#####  should be sufficient to store local versions of the desired
#####  values in the present environment?
            ##  Perform the computation

            ## capture_env() 
            
            spectra <- foreach(
                sub_piece = pieces,
                .combine = list_array_join,
                .multicombine = TRUE) %do% {
                    ##  Use 'eval' and 'create_call' to perform the computation.
                    eval(create_call(
                        .cc_fun = LG_boot_spectra,
                        c(arg_list[[sub_piece]],
                          list(LG_approx_extract_data =
                                   LG_approx_extract_data)),
                        .cc_list = TRUE))
                }

            ## ## ## ## LG_boot_spectra_call <- create_call(
            ## ## ## ##     .cc_fun = LG_boot_spectra,
            ## ## ## ##     c(arg_list[[sub_piece]],
            ## ## ## ##       list(LG_approx_extract_data =
            ## ## ## ##                LG_approx_extract_data)),
            ## ## ## ##     .cc_list = TRUE)
            
            kill(list_array_join, sub_piece, pieces, arg_list, LG_approx_extract_data)
###-------------------------------------------------------------------
            ##  Save the result to file, with updated attributes
            ##  describing the array nodes.  Record the file-name.
            save_file <- LG_save(
                data = list_array_dims(spectra),
                data_type = "spectra_save",
                LG_type = type,
                bootstrap = TRUE,
                part = part,
                save_dir = books$save_dir)
###-------------------------------------------------------------------
            ##  Update 'dimnames_spectra'.
            dimnames_spectra[[type]] <- c(
                dimnames_spectra[[type]],
                list(attributes(spectra)$list_array_dimnames))
                ## ## ## ## c(dimnames_spectra[[type]],
                ## ## ## ##   list(dimnames(spectra)))
            names(dimnames_spectra[[type]])[length(dimnames_spectra[[type]])] <-
                save_file
###-------------------------------------------------------------------
            ##  Update 'data_files_df'
            data_files_df[row_nr, ] <- 
                list(part_a_of_b = part,
                     content = type,
                     data_files = save_file,
                     object_size_MB=
                         as.numeric(object.size(spectra) / 1024^2))
###-------------------------------------------------------------------
            ##  Update 'row_nr' for next loop.
            row_nr <- row_nr + 1
        }
        kill(type)
        ##  This ends the loop over 'LG_type'
    }
    kill(part, loop_list, spectra, row_nr, LG_type, adjustment)
    ##  This ends the loop over 'loop_list'
###-------------------------------------------------------------------
    ##  Add 'data_files_df' and 'dimnames_spectra' to the info-object.
    books$info[[books$bookmark]][["data_files_df"]] <- data_files_df
    books$info[[books$bookmark]][["dimnames_spectra"]] <- dimnames_spectra
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
