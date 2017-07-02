################################################################################
#'
#' Local Gaussian spectra, scribe-function.
#'
#' This function takes care of the bookkeeping when the investigation
#' of a time series by means of local Gaussian spectra is performed.
#'
#' @details This function records its arguments and compares them to a
#'     previously stored information-object, in order to avoid redoing
#'     previously performed computations.  For new computations,
#'     relevant data will be extracted from \code{data_dir}, which
#'     then will be analysed in order to see if memory issues requires
#'     that the computations should be performed in smaller chunks.
#'     The function then calls \code{LG_approx_extract} and
#'     \code{LG_spectra} in order to obtain the result -- and finally
#'     the result is added to the file-structure while the
#'     information-object is updated.
#'
#' @template main_dir_arg
#' @template data_dir_arg
#' @template LG_type_arg
#' @template omega_vec_arg
#' @template omega_length_out_arg
#' @template window_arg
#' @template cut_vec_arg
#' @template method_arg
#'
#' @return This function will return a two component list to the
#'     work-flow.  The first component is the logical value
#'     \code{done_before} that reveals whether or not the result
#'     already existed (in the specified file-structure), whereas the
#'     second component \code{data_dir} gives the location of the
#'     saved data.
#'
#' @export



LG_spectra_scribe <- function(
    main_dir,
    data_dir,
    LG_type = c("par_five", "par_one"),
    omega_vec = NULL,
    omega_length_out = 2^6,
    window =
        c("Tukey", "Parzen", "Bartlett"),
    cut_vec = NULL,
    .method = c("all", "mean", "median")) {
###-------------------------------------------------------------------
    ##  We need a spy_report to capture the arguments.
    spy_report <- spy()
    kill(main_dir, data_dir, omega_vec, omega_length_out, window,
         cut_vec)
###-------------------------------------------------------------------
    ##  Sanity-check and update the arguments, and check them against
    ##  previous computations.
    books <- LG_bookkeeping(spy_report = spy_report)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  If the same computations already has been performed, we can
    ##  just return 'books' and nothing more is needed.
    if (books$done_before)
        return(books)

    ## TS_spectral_call <- create_call(
    ##     .cc_fun = TS_spectral,
    ##     acr_data = books$.acr_data,
    ##     omega_vec = spy_report$envir$omega_vec,
    ##     window = spy_report$envir$window,
    ##     cut_vec = spy_report$envir$cut,
    ##     .method = spy_report$envir$.method)

    ## capture_env() 
###-------------------------------------------------------------------
    ##  Compute (and save) the ordinary (global) spectral densities.
    .TS_spectral <- TS_spectral(
        acr_data = books$.acr_data,
        omega_vec = spy_report$envir$omega_vec,
        window = spy_report$envir$window,
        cut_vec = spy_report$envir$cut,
        .method = spy_report$envir$.method)
    ##  Add information to the `info`-object.
    books$info[[books$bookmark]][[.TS_spectral$.spectral_type]] <-
        .TS_spectral$.spectral_content
    kill(.TS_spectral)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Load 'adjustment' into the work-flow.
    load(file = file.path(spy_report$envir$save_dir,
                          LG_default$global["adjustment"]))

###-------------------------------------------------------------------
    ##
#####  TASK: Include test in order to see if the computation should be
#####  partitioned into several pieces.  Perhaps not necessary in this
#####  first function.
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
###-------------------------------------------------------------------
    ##  Start out by using 'split' on 'data_files_df' in order to get
    ##  a list that shows the number of parts in the previous step.
    tmp_list <- split(x = books$data_files_df,
                      f = books$data_files_df$part_a_of_b)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Create a new 'loop_list' by looping over 'tmp_list'.  Check if we
###  need to split the computation into smaller pieces in order to
###  avoid "out of memory" issues.  (Not implemented at all yet...)
#############---------------------------------------------------------
###-------------------------------------------------------------------

    ## capture_env() 
    ## part <- 1

    ##  Initiate 'loop_list'
    loop_list <- list()
    ##  Fill loop list with content.
    for (part in seq_along(tmp_list)) {
        ##  Extract the data-files as a vector of paths.
        data_files <- structure(
            .Data = tmp_list[[part]]$data_files,
            .Names = tmp_list[[part]]$content)
##### TASK:
        ##  Include a function to investigate if the computation
        ##  should be divided into smaller chunks.  The idea I've got
        ##  in mind at the moment is to make copies of
        ##  'spy_report$envir and modify that.  Care must be taken
        ##  here, since environments behave in a different way than
        ##  lists.  I guess I perhaps might just as well create a
        ##  list-copy instead, to avoid any risk of such errors.  Add
        ##  new stuff to loop_list.
        loop_list[[part]] <-
            list(data_files = data_files,
                 arg_list = as.list(spy_report$envir,
                                    all.names = TRUE))  ## arg_list)
    }
    kill(part, tmp_list, data_files, spy_report)
    ##  Add names to loop list.
    if (length(loop_list) == 1) {
        names(loop_list) <- " "
    } else {
        names(loop_list) <- paste("part_",
                                  seq_along(loop_list),
                                  "_of_",
                                  length(loop_list),
                                  sep = "")
    }
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

    
    ## capture_env() 
    
    for (part in names(loop_list)) {
        ##  Loop over the types given in 'LG_type'.
        for (type in LG_type) {
            ##  Use intermediate environment storage, restrict loading
            ##  of relevant data to the 'type' under investigation.
            par_one.env <- new.env()
            par_five.env <- new.env()
            ##  Compose data to be given to 'LG_approx_extract' and
            ##  'LG_spectra'.  Restrict to the case of interest,
            ##  i.e. 'type', in order to save memory.

            ## capture_env() 
            approx_data <- list(
                LG_type = type,
                par_one_data =
                    if ("par_one" %in% type) {
                        load(loop_list[[part]]$data_files["par_one"],
                             envir = par_one.env)
                        par_one.env[[ls(par_one.env)]]
                    },
                par_five_data =
                    if ("par_five" %in% type) {
                        load(loop_list[[part]]$data_files["par_five"],
                             envir = par_five.env)
                        par_five.env[[ls(par_five.env)]]
                    },
                adjustment = adjustment)
            kill(par_one.env, par_five.env)


            ## capture_env() 
            ##  Use call-construction to find the estimates of the
            ##  local Gaussian spectral density of type 'type'.

            ## LG_approx_extract_call <- create_call(
            ##     .cc_fun = LG_approx_extract,
            ##     approx_data = approx_data,
            ##     LG_type = type,
            ##     .method = .method)

            ## capture_env() 

            
            ## LG_spectra_call <- create_call(
            ##     .cc_fun = LG_spectra,
            ##     c(loop_list[[part]]$arg_list,
            ##       list(LG_approx_extract_data = LG_approx_extract(
            ##                approx_data = approx_data,
            ##                LG_type = type,
            ##                .method = .method))),
            ##     .cc_list = TRUE)
            ## capture_env() 

            
            spectra <- eval(create_call(
                .cc_fun = LG_spectra,
                c(loop_list[[part]]$arg_list,
                  list(LG_approx_extract_data = LG_approx_extract(
                           approx_data = approx_data,
                           LG_type = type,
                           .method = .method))),
                .cc_list = TRUE))
            ## ## ## ## ##  Add attributes describing the dimensions.

            ## ## ## ## ## list_array_dims_call <- create_call(
            ## ## ## ## ##     .cc_fun = list_array_dims,
            ## ## ## ## ##     .list = spectra)
            
            ## ## ## ## ## capture_env() 
            
            ## ## ## ## spectra <- list_array_dims(.list = spectra)
            ##  Save the result to file, record the file-name.
            save_file <- LG_save(
                data = spectra,
                data_type = "spectra_save",
                LG_type = type,
                part = part,
                save_dir = books$save_dir)
            ##  Update 'dimnames_spectra'.
            dimnames_spectra[[type]] <-
                c(dimnames_spectra[[type]],
                  list(attributes(spectra)$list_array_dimnames))
                  ## list(dimnames(spectra)))
            names(dimnames_spectra[[type]])[length(dimnames_spectra[[type]])] <-
                save_file
            ##  Update 'data_files_df'
            data_files_df[row_nr, ] <- 
                list(part_a_of_b = part,
                     content = type,
                     data_files = save_file,
                     object_size_MB=
                         as.numeric(object.size(spectra) / 1024^2))
            ##  Update 'row_nr' for next loop.
            row_nr <- row_nr + 1
        }  ##  This ends the loop over 'LG_type'
    }   ##  This ends the loop over 'loop_list'
    kill(part, loop_list, type, LG_type, approx_data, adjustment,
         spectra, save_file, row_nr)
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
    ## data_files_df$data_files <- file.path(
    ##     paste(books$data_dir,
    ##           collapse = .Platform$file.sep),
    ##     data_files_df$data_files)
###-------------------------------------------------------------------
    ##  Return information about 'done_before' and 'save_file'.
    return(list(
        done_before = books$done_before,
        main_dir = books$main_dir,
        data_dir = books$data_dir,
        data_files_df = data_files_df))
}
