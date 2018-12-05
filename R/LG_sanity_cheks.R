################################################################################
#####  2016-02-11

##  After an awful lot of ad hoc solutions, I think it is time to
##  make a solution to this.  The point now is to create a function
##  that can be called from 'LG_bookkeeping' in order to test the
##  sanity of the arguments, and to do a set of basic computations and
##  updates.  This initial stuff here is to be a reminder while
##  creating this crap.  First of all, I want a function that can take
##  four arguments 'spy_report', 'info', 'new_stuff' and 'wrapper'.
##  The non-NULL arguments in 'spy_record' should be sanity-checked
##  against stuff stored in 'LG_defaults' or previously computed stuff,
##  depending on the value of 'new_stuff'.

#' Check the sanity of arguments to the scribes and the wrappers
#'
#' This function checks that the arguments are valid, and it might
#' also add values to the environment of the function under
#' investigation.
#'
#' @template spy_report_arg
#' 
#' @return If any arguments are erroneous, the program will be stopped
#'     and information about the problems returned to the workspace.
#'     If no problems are found, the programs returns nothing.  Note
#'     that the program will work upon a shortcut to the environment
#'     part of \code{spy_report}, and that implies that any updates
#'     will have been registered in the \code{spy_report} in the
#'     calling function.
#'
#' @export


LG_sanity_checks <- function(
    spy_report) {
###-------------------------------------------------------------------
    ##  Identify the target function we are checking.
    target_fun <- spy_report$fun
    ##  Register the name of the present function.
    .this_function <- this_function()
###-------------------------------------------------------------------
    ##  Check that 'spy_report' comes from a valid function.
    valid_fun <- c(
        "LG_approx_scribe", "LG_spectra_scribe",
        "LG_boot_approx_scribe", "LG_boot_spectra_scribe",
        "LG_Wrapper_Original", "LG_Wrapper_Bootstrap",
        "LG_Wrapper_Blocks", "LG_collect_orig_and_boot",
        "LG_merge_files_scribe",
        "LG_collect_blocks", "LG_shiny")
    ##---
    if (! target_fun %in% valid_fun)
        error(.argument = "spy_report",
              paste(sQuote(.this_function),
                    " attempted used with ",
                    sQuote("spy_report"),
                    " from ",
                    sQuote(target_fun),
                    ".",
                    sep = ""),
              c("Only the following functions have been implemented: ",
                paste(sQuote(valid_fun),
                      collapse = ", ")))
###-------------------------------------------------------------------
    ##  Create a shortcut to 'spy_report$envir'.  Reminder: This is
    ##  not a local copy; changes to 'arg_env' will also occur in the
    ##  original environment.
    arg_env <- spy_report$envir
###-------------------------------------------------------------------
    ##  Check the validity of the 'main_dir'-argument.
    if (! dir.exists(arg_env$main_dir))
        error(.argument = "main_dir",
              n = 3,
              c("Could not find the directory: ",
                sQuote(arg_env$main_dir)))
#####  TASK: Test this to see that the correct value is returned.
######  TASK: Return something else than desired when testing with
######  under construction, but I think the `capture_env()`-function
######  might become adjusted to remove the need for this...
###-------------------------------------------------------------------
    ##  Check the existence of the `TS_content`-file.
    TS_content_file <- file.path(arg_env$main_dir,
                                 LG_default$content_file_name)
    if (! file.exists(TS_content_file))
        error(.argument = "main_dir",
              n = 3,
              c("Could not find the file",
                sQuote(LG_default$content_file_name),
                "in the directory",
                sQuote(arg_env$main_dir)))
    ##  If `target_fun` is "LG_shiny", then load `TS_content` into its
    ##  environment, and split `main_dir` into a vector.  Return at
    ##  this stage if `data_dir` is `NULL`.
    if (target_fun == "LG_shiny") {
        gramps <- sys.frame(which = -2)
        load(file = TS_content_file,
             envir = gramps)
        gramps$main_dir <- unlist(strsplit(
            x = spy_report$envir$main_dir,
            split = .Platform$file.sep))
        if (is.null(spy_report$envir$data_dir))
            return(invisible(NULL))
    }
###-------------------------------------------------------------------
    ##  Check the validity of the directory-argument, i.e. extract
    ##  either `data_dir` or `spectra_dir`.
    if (target_fun == "LG_Wrapper_Bootstrap") {
        data_dir <- arg_env$spectra_dir
    } else
        data_dir <- arg_env$data_dir 
    ##
    data_dir_path <- paste(c(arg_env$main_dir,
                             data_dir),
                           collapse = .Platform$file.sep)
    if (! dir.exists(data_dir_path))
        error(.argument = "data_dir",
              n = 3,
              c("Could not find a directory named ",
                sQuote(arg_env$data_dir),
                "inside the main directory",
                sQuote(arg_env$main_dir)))
    ##  Add 'data_dir_path'' as 'save_dir' to 'arg_env'
    arg_env$save_dir <- data_dir_path
###-------------------------------------------------------------------
    ##  Find the path to the info-file.  Note: Only use the first part
    ##  of 'arg_env$data_dir'
    info_path <-
        file.path(paste(c(arg_env$main_dir,
                          head(x = data_dir, n = 1)),
                        collapse = .Platform$file.sep),
                  LG_default$info_file_name)
###-------------------------------------------------------------------
    ##  Check that the main directory contains the desired info-file.
    if (! file.exists(info_path))
        error(.argument = "data_dir",
              n = 3,
              c("No file named ",
                sQuote(LG_default$info_file_name),
                "in the data directory",
                sQuote(data_dir),
                "- that was found inside the main directory",
                sQuote(arg_env$main_dir)))
###-------------------------------------------------------------------
    ##  Return here if `target_fun` is "LG_shiny".
    if (target_fun == "LG_shiny") 
        return(invisible(NULL))
###-------------------------------------------------------------------
    ##  Load the info-file to get access to 'info'.
    load(file = info_path)
###-------------------------------------------------------------------
    ##  Add `TS` to 'spy_report$envir', so the bootstrap procedure
    ##  will work later on.
    if (target_fun == "LG_boot_approx_scribe") 
        spy_report$envir$TS <- info$TS_info$TS
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Some functions need to look stuff up in the 'info'-object, and
###  for these we need to check the validity of the bookmarks
###  corresponding to the directory argument
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create an adjusted copy of the defaults for the directories,
    ##  without the first component that refers to the time series.
    folder_defaults <- tail(LG_default$folder_defaults, n = -1)
###-------------------------------------------------------------------
    ##  For the initial bootstrap-scribes, remove the "spectra.dir"
    ##  part from 'defaults' and 'data_dir'.
    if (target_fun %in% c("LG_boot_approx_scribe",
                          "LG_boot_spectra_scribe"))  {
        folder_defaults <-
            folder_defaults[names(folder_defaults) != "spectra.dir"]
        ##---
        data_dir <- data_dir[names(data_dir) != "spectra.dir"]
        arg_env$data_dir <- data_dir
    }
###-------------------------------------------------------------------
    ##  Create the bookmark corresponding to 'data_dir'.
    dir_bookmark <- data_dir[names(data_dir) %in%
                             names(folder_defaults)]
###-------------------------------------------------------------------
    ##  Check the validity of the bookmark for the relevant functions.
    if (! target_fun %in% c("LG_approx_scribe",
                            "LG_Wrapper_Original",
                            "LG_Wrapper_Blocks")) {
        ##---
        tmp <- try(expr = info[[dir_bookmark]],
                   silent = TRUE)
        ##---
        if (class(tmp) == "try-error")
            error(.argument = "data_dir",
                  n = 3,
                  c(sQuote(paste("info",
                                 paste(dir_bookmark,
                                     collapse = "$"),
                                 sep = "$")),
                    "was not found."))
#####  TASK: Test this error.
        kill(tmp)
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  If the test above didn't terminate this function, then it's time
###  to sanity check the rest of the arguments and perform updates.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  The 'TS'-argument, can refer to a file, and it might also be
    ##  that there's an attribute 'TS_for_analysis' that should be
    ##  used.  Some tweaking is thus needed.
    if (! is.null(arg_env$TS)) {
        if (is.character(arg_env$TS)) {
            ##  If `target_fun` is `LG_Wrapper_Bootstrap`, create a
            ##  quote to revert `TS` back to path after testing.
            if (target_fun %in% c("LG_Wrapper_Bootstrap",
                                   "LG_boot_approx_scribe"))
                revert_TS_to_path_quote <- bquote(
                    arg_env$TS <- .(arg_env$TS))
            ##  Update `arg_env` with `TS`-data from file.
            load(file = paste(c(arg_env$main_dir,
                                arg_env$TS),
                              collapse = .Platform$file.sep),
                 envir = arg_env)
        }
        ##  Adjust 'TS' to be tested, when required.
        if (! identical(x = attributes(arg_env$TS)$TS_for_analysis,
                        y = NULL)) 
            arg_env$TS <- attributes(arg_env$TS)$TS_for_analysis
    }
###-------------------------------------------------------------------

################################################################################
#####  2016-12-30: Need to think a bit more about the multivariate
#####  case, in order to figure out how to deal with this.

    ## ## ## capture_env() 
    
    ## ## ## ##  Stop the program if a wrapper has been used with the wrong
    ## ## ## ##  kind of argument
    ## ## ## if (spy_report$fun == "LG_Wrapper_Original")
    ## ## ##     if (! is.null(attributes(arg_env$TS)$.multivariate_TS))
    ## ## ##         if (attributes(arg_env$TS)$.multivariate_TS)
    ## ## ##             error(.argument = "TS",
    ## ## ##                   c("The argument 'TS' gives a multivariate",
    ## ## ##                     "time-series, while the function",
    ## ## ##                     sQuote(spy_report$fun),
    ## ## ##                     "requires a univariate time-series."),
    ## ## ##                   sep = "")
###-------------------------------------------------------------------
    ##  The case 'LG_collect_orig_and_boot' differs from the rest, and
    ##  will thus be treated first, i.e. check that 'data_dir' points
    ##  to bootstrap-data, and add some objects to the environment of
    ##  'LG_collect_orig_and_boot', i.e. the grandparent-environment.
#####  TASK: Revise this solution later on, so only one function is
#####  needed when collecting the files.  That should be feasible to
#####  do, I think - but I don't have time available to investigate
#####  this now.
    if (target_fun %in% c("LG_collect_orig_and_boot",
                          "LG_merge_files_scribe",
                          "LG_collect_blocks")) {
###-------------------------------------------------------------------
        ##  Identify the type of bootstrap we are considering.
        boot_type <- gsub(pattern = "\\.dir",
                          replacement = "",
                          x = tail(names(dir_bookmark), 1))
#####  2017-01-11: REMINDER: The above will always be returned, even
#####  when not needed.  This is a consequence of the ad hoc approach
#####  used when developing this code...
###-------------------------------------------------------------------
        ##  Check that 'boot_type' is valid.
        if (! boot_type %in% c("boot.approx", "boot.spectra"))
            error(.argument = "data_dir",
                  n = 3,
                  c(sQuote("data_dir"),
                    "does not point point to bootstrap-data."))
#####  TASK: The present approach ignores the testing of the
#####  threshold-value, but I think I will ignore that for the time
#####  being.  I suppose the structure of this function might benefit
#####  from a little bit more planning with regard to when different
#####  stuff is performed, and it would be nice to have everything
#####  going in the flow without any such ad hoc parts like this...
###-------------------------------------------------------------------
        ##  Some ad hoc adjustments:
        if (target_fun == "LG_collect_blocks") {
            ##  Adjust hierarchy, so 'LG_shiny' finds the data in the same
            ##  configuration as the one used for the ordinary case.
            .new_hierarchy <- LG_default$folder_defaults["boot.spectra.dir"]
            ##  Prepare `info` for storing of new level, or register
            ##  that stuff has been done before.
            if (is.null(info[[c(dir_bookmark, .new_hierarchy)]])) {
                info[[c(dir_bookmark, .new_hierarchy)]] <- list()
                done_before <- FALSE
            } else
                done_before <- TRUE
            ##  Create a collected bookmark for the adding of results.
            collected_bookmark <- c(dir_bookmark,
                                    .new_hierarchy,
                                    LG_default$collected_data)
            ##  Extend data dir for saving to `info`-object later on.
            data_dir <- c(data_dir, .new_hierarchy)
        } else
            done_before <- ! is.null(
            info[[dir_bookmark]][[LG_default$collected_data]])
        ##  Add extra objects to the grandparent-environment, i.e. to
        ##  'LG_collect_orig_and_boot' or `LG_collect_blocks`.
        gramps <- sys.frame(which = -2)
        ##---
        gramps$boot_type <- boot_type
        gramps$data_dir <- data_dir
        gramps$dir_bookmark <- dir_bookmark
        gramps$done_before <- done_before
        gramps$info <- info
        gramps$info_path <- info_path
        gramps$save_dir <- paste(c(arg_env$main_dir,
                                   data_dir),
                                 collapse = .Platform$file.sep)
        if (target_fun == "LG_collect_blocks")
            gramps$collected_bookmark <- collected_bookmark
        ##  Quit this program
        return(invisible(NULL))
    }
    ##  This concludes the treatment of 'LG_collect_orig_and_boot' and
    ##  `LG_collect_blocks`
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  The updates for the remaining functions will mostly be on
###  'arg_env', the shortcut to the environment 'spy_report$envir'
###  from the main function, and these are thus automatically updated
###  at the correct level.  We need an extra step to give
###  'LG_bookkeeping' access to other objects.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Add extra objects to the parent, i.e. to 'LG_bookkeeping'.
    calling_env <- sys.frame(which = -1)
    ##---
    calling_env$data_dir <- data_dir
    calling_env$dir_bookmark <- dir_bookmark 
    calling_env$folder_defaults <- folder_defaults
    calling_env$info <- info
    calling_env$info_path <- info_path
    calling_env$save_dir <- str_split(
        string = data_dir_path,
        pattern = .Platform$file.sep)[[1]]
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  It's now time to check the rest of the arguments, and perform
###  relevant updates based on 'target_fun'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Create a list to record if the arguments are valid.
    arg_names <- ls(arg_env, all.names = TRUE)
    valid_args <- vector(
        mode = "list",
        length = length(arg_names))
    names(valid_args) <- arg_names
###-------------------------------------------------------------------
    ##  The 'data_dir' (or 'spectra_dir' in the bootstrap-wrapper) has
    ##  already been tested in the code above.
    if (target_fun == "LG_Wrapper_Bootstrap") {
        valid_args$spectra_dir <- TRUE
        ##  Add 'data_dir' for the sake of 'LG_boot_approx'.
        arg_env$data_dir <- head(arg_env$spectra_dir, -1)
    } else
        valid_args$data_dir <- TRUE
###-------------------------------------------------------------------
    ##  Select 'check-list' and 'default_list' based on 'target_fun'.
    if (target_fun %in% c("LG_approx_scribe",
                          "LG_spectra_scribe",
                          "LG_Wrapper_Original",
                          "LG_Wrapper_Blocks")) {
        ##  Select check-list.
        check_list <- LG_default$check$original
        ##  Pick out default values to compare against.
        default_list <-
            LG_default[which(names(LG_default) %in% check_list$subset)]
    }
    ##---
    if (target_fun %in% c("LG_boot_approx_scribe",
                          "LG_boot_spectra_scribe",
                          "LG_Wrapper_Bootstrap")) {
        ##  Select check-list.
        check_list <- LG_default$check$bootstrap
        ##  Extract default values from previous computations.
        approx_env <- {
            if (! target_fun == "LG_Wrapper_Bootstrap") {
                info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir
            } else {
                info[[arg_env$spectra_dir["approx.dir"]]]$spy_report$envir
            }
        }
        if (! is.null(arg_env$spectra_dir)) {
            spectra_env <-
                info[[arg_env$spectra_dir[c("approx.dir", "spectra.dir")]]]$spy_report$envir
        } else
            spectra_env <- NULL
        ##  Note: 'spectra_env' will be 'NULL' unless the calling
        ##  function is 'LG_boot_spectra_scribe'.
###-------------------------------------------------------------------
        ##  Create the default list.  This list contains more than we
        ##  need, but that doesn't matter for the code later on.
        ##  Reminder: The order stuff ensures that arguments souped up
        ##  from the computation of the original spectra "dominates"
        ##  those from the original approximation and the formals in
        ##  'TS_boot_sample'.
        default_list <- c(
            { if (! is.null(spectra_env))
                  as.list(spectra_env,
                          all.names = TRUE) },
            as.list(approx_env,
                    all.names = TRUE),
            formals(TS_boot_sample))
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Use different tests to update 'valid_args' and 'arg_env'.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Check the validity of 'TS', when relevant.
    if ("TS" %in% names(valid_args))
        valid_args$TS <- eval(LG_default$TS_test)
###-------------------------------------------------------------------
    ##  Check the validity of 'LG_points' when relevant.
    if (target_fun %in% c("LG_approx_scribe",
                          "LG_Wrapper_Original",
                          "LG_Wrapper_Blocks")) {
        ##  Check for correct class (created by 'LG_select_points')
        valid_args[["LG_points"]] <-
            LG_default$class$points %in% class(arg_env[["LG_points"]])
    }
    if (target_fun %in% c("LG_boot_approx_scribe",
                          "LG_Wrapper_Bootstrap")) {
        if (! is.null(arg_env[["LG_points"]])) {
            ##  Check for class first, then for subset of original.
            valid_args[["LG_points"]] <- local({
                .class <-
                    LG_default$class$points %in% class(arg_env[["LG_points"]])
                if (.class) {
                    .old_names <- rownames(default_list[["LG_points"]])
                    .new_names <- rownames(arg_env[["LG_points"]])
                    all(.new_names %in% .old_names)
                } else
                    FALSE
            })
        } else {
            ##  Update with old values.
            arg_env[["LG_points"]] <- default_list[["LG_points"]]
            valid_args[["LG_points"]] <- TRUE
        }
            
    } 
###-------------------------------------------------------------------
    ##  Check validity of arguments that should be a subset of (or
    ##  equal to) the default values.  Insert defaults if necessary.
    for (arg in intersect(arg_names, check_list$subset))
        if (! is.null(arg_env[[arg]])) {
            valid_args[[arg]] <- as.logical(
                prod(as.character(arg_env[[arg]]) %in%
                     as.character(default_list[[arg]])))
        } else {
            arg_env[[arg]] <- default_list[[arg]]
            valid_args[[arg]] <- TRUE
        }
###-------------------------------------------------------------------
    ## Check validity of 'logic' arguments.
    for (arg in intersect(arg_names, check_list$logic))
        if (! is.null(arg_env[[arg]])) {
            valid_args[[arg]] <- arg_env[[arg]] %in% c(FALSE, TRUE)
         } else {
             arg_env[[arg]] <- default_list[[arg]]
             valid_args[[arg]] <- TRUE
         }
###-------------------------------------------------------------------
    ##  Check validity of positive 'integer' length one arguments.
    for (arg in intersect(arg_names, check_list$integer_length_one))
        valid_args[[arg]] <- all(
            length(arg_env[[arg]]) == 1,
            is.numeric(arg_env[[arg]]),
            if (is.numeric(arg_env[[arg]])) {
                all.equal(arg_env[[arg]],
                          round(arg_env[[arg]]))
            } else
                FALSE,
            arg_env[[arg]] > 0)
###-------------------------------------------------------------------
    ##  Check validity of positive 'integer' vectors.
    for (arg in intersect(arg_names, check_list$integer_vec))
        valid_args[[arg]] <- all(
            is.numeric(arg_env[[arg]]),
            if (is.numeric(arg_env[[arg]])) {
                all.equal(arg_env[[arg]],
                          round(arg_env[[arg]]))
            } else
                FALSE,
            all(arg_env[[arg]] > 0))
###-------------------------------------------------------------------
    ##  Check validity of 'numeric' length one arguments in (0, 1).
    for (arg in intersect(arg_names, check_list$numeric_length_one))
        valid_args[[arg]] <- all(
            length(arg_env[[arg]]) == 1, 
            is.numeric(arg_env[[arg]]),
            arg_env[[arg]] < 1,
            arg_env[[arg]] > 0)
###-------------------------------------------------------------------
    ##  Check validity of 'numeric' length two arguments in [0, 1].
    for (arg in intersect(arg_names, check_list$numeric_length_two))
        valid_args[[arg]] <- as.logical(
            prod(length(arg_env[[arg]]) == 2, 
                 is.numeric(arg_env[[arg]]),
                 arg_env[[arg]] <= 1,
                 arg_env[[arg]] >= 0))
###-------------------------------------------------------------------
    ##  Check validity of 'numeric' "no ties" vectors,  
    for (arg in intersect(arg_names, check_list$numeric_vec)) 
        valid_args[[arg]] <- as.logical(
            prod(is.numeric(arg_env[[arg]]),
                 identical(
                     x = length(unique(arg_env[[arg]])),
                     y = length(arg_env[[arg]]))))
###-------------------------------------------------------------------
    ##  Check validity of 'positive' 'numeric' "no ties"  vectors,
    for (arg in intersect(arg_names, check_list$numeric_vec_positive_or_NULL))
        if (is.null(arg_env[[arg]])) {
            valid_args[[arg]] <- TRUE
        } else
            valid_args[[arg]] <- as.logical(
            prod(is.numeric(arg_env[[arg]]),
                 prod(arg_env[[arg]] > 0),
                 identical(
                     x = length(unique(arg_env[[arg]])),
                     y = length(arg_env[[arg]]))))
###-------------------------------------------------------------------
#############---------------------------------------------------------
    ##  Additional adjustments based on the calling functions.  The
    ##  goal is to ensure that any arguments given with 'NULL' is
    ##  replaced with the correct default values.
#############---------------------------------------------------------
    ##  Compatibility of arguments '.bws_fixed' and '.bws_fixed_only',
    ##  relevant for the four functions 'LG_approx_scribe',
    ##  'LG_Wrapper_Original', 'LG_Wrapper_Blocks',
    ##  'LG_boot_approx_scribe' and 'LG_Wrapper_Bootstrap'.
    if (all(target_fun %in% c("LG_approx_scribe",
                              "LG_Wrapper_Original",
                              "LG_Wrapper_Blocks",
                              "LG_boot_approx_scribe",
                              "LG_Wrapper_Bootstrap"),
            is.null(arg_env$.bws_fixed),
            arg_env$.bws_fixed_only))
        error(.argument = c(".bws_fixed_only", ".bws_fixed"),
              n = 3,
              c("The combination",
                sQuote(".bws_fixed_only"),
                "equal to",
                sQuote("TRUE"),
                "and",
                sQuote(".bws_fixed"),
                "equal to",
                sQuote("NULL"),
                "is not allowed!"))
###-------------------------------------------------------------------
    ##  'LG_approx_scribe', 'LG_Wrapper_Original' and
    ##  'LG_Wrapper_Blocks': 'lag_max' (truncate if valid, but too
    ##  high).
    if (all(target_fun %in% c("LG_approx_scribe",
                               "LG_Wrapper_Original",
                              "LG_Wrapper_Blocks"),
            valid_args$lag_max,
            valid_args$TS))
        arg_env$lag_max <- min(
            arg_env$lag_max,
            length(dimnames(arg_env$TS)$observations) -1)
###-------------------------------------------------------------------
    ##  'LG_spectra_scribe': 'cut_vec' (truncate if valid but too high).
    if (all(target_fun == "LG_spectra_scribe",
            valid_args$cut_vec)) {
        cut_max <- 1 + 
            info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir$lag_max
        arg_env$cut_vec <-
            arg_env$cut_vec[arg_env$cut_vec <= cut_max]
    }
###-------------------------------------------------------------------
    ##  "LG_Wrapper_Original" and "LG_Wrapper_Blocks": 'cut_vec'
    ##  (truncate if valid but too high).
    if (all(target_fun %in% c("LG_Wrapper_Original",
                              "LG_Wrapper_Blocks"),
            valid_args$cut_vec,
            valid_args$lag_max))
        arg_env$cut_vec <-
            arg_env$cut_vec[arg_env$cut_vec <= arg_env$lag_max + 1]
###-------------------------------------------------------------------
    ##  'LG_spectra_scribe': 'cut_vec' (if unspecified: compute if
    ##  possible)
    if (all(target_fun == "LG_spectra_scribe",
            is.null(arg_env$cut_vec))) {
        cut_max <- 1 + 
            info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir$lag_max
        arg_env$cut_vec <- 1:cut_max
        valid_args$cut_vec <- TRUE
    }
###-------------------------------------------------------------------
    ##  'LG_Wrapper_Original' and "LG_Wrapper_Blocks": 'cut_vec' (if
    ##  unspecified: compute if possible)
    if (all(target_fun %in% c("LG_Wrapper_Original",
                              "LG_Wrapper_Blocks"),
             is.null(arg_env$cut_vec),
             valid_args$lag_max)) {
        cut_max <- 1 + arg_env$lag_max
        arg_env$cut_vec <- 1:cut_max
        valid_args$cut_vec <- TRUE
    }
#####  TASK: Should I include a truncation of lag_max too (for the
#####  case 'LG_Wrapper_Original')?  It should be done for the
#####  bootstrap-case, but it might not be that important for the
#####  original case?
###-------------------------------------------------------------------
    ##  'LG_spectra_scribe', "LG_Wrapper_Original" and
    ##  "LG_Wrapper_Blocks": 'omega_vec' (compute if possible)
    if (all(target_fun %in% c("LG_spectra_scribe",
                               "LG_Wrapper_Original",
                              "LG_Wrapper_Blocks"),
            is.null(arg_env$omega_vec),
            valid_args$omega_length_out)) {
        arg_env$omega_vec <- seq(
            from = 0, to = 1/2,
            length.out = arg_env$omega_length_out)
        valid_args$omega_vec <- TRUE
    }
###-------------------------------------------------------------------
    ##  'LG_spectra_scribe', "LG_Wrapper_Original" and
    ##  "LG_Wrapper_Blocks": 'omega_length_out' (ignore value if
    ##  'omega_vec' is well defined).
    if (target_fun %in% c("LG_spectra_scribe",
                          "LG_Wrapper_Original",
                          "LG_Wrapper_Blocks")) {
        if (all(! valid_args$omega_length_out,
                valid_args$omega_vec))
            valid_args$omega_length_out <- TRUE
    }
###-------------------------------------------------------------------
    ##  For 'LG_boot_approx_scribe' and 'LG_Wrapper_Bootstrap':
    ##  'boot_seed', 'block_length', 'nb' and 'lag_max'
    if (target_fun %in% c("LG_boot_approx_scribe",
                          "LG_Wrapper_Bootstrap")) {
###-------------------------------------------------------------------
        ##  Pick a 'boot_seed' if necessary:
        if (is.null(arg_env$boot_seed)) {
            arg_env$boot_seed <- as.numeric(
                paste(sample(0:9, 9, replace = TRUE),
                      collapse = ""))
            valid_args$boot_seed <- TRUE
        }
###-------------------------------------------------------------------
        ##  Insert default values for arguments that still are 'NULL'.
        is_NULL <- unlist(
            lapply(X = arg_env,
                   FUN = is.null))
        null_names <-
            names(is_NULL[is_NULL == TRUE])
        for (arg in null_names) {
            arg_env[[arg]] <- default_list[[arg]]
            valid_args[[arg]] <- TRUE
        }
###-------------------------------------------------------------------
        ##  Add '.adjustment_rule' to the argument-environment
        arg_env$.adjustment_rule <- attributes(attributes(
            info$TS_info$TS)$TS_for_analysis)$.adjustment_rule
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
    ##  Extra for 'LG_boot_spectra_scribe': 'spectra_dir'.
    if (target_fun == "LG_boot_spectra_scribe") {
        ##  Test if we get a valid bookmark from 'spectra_dir'.
        spectra_bookmark <- 
            arg_env$spectra_dir[names(arg_env$spectra_dir) %in%
                                names(folder_defaults)]
        ##  Check the validity of the bookmark
        tmp <- try(expr = info[[spectra_bookmark]],
                   silent = TRUE)
        ##---
        valid_args$spectra_dir <-
            class(tmp) != "try-error"
#####  TASK: Create error-message too?
        ## stop("\n\t",
        ##      "Can't find previous data for 'spectra_dir'",
        ##      "\n\t",
        ##      paste(arg_env$spectra_dir,
        ##            collapse = .Platform$file.sep))
    }
###-------------------------------------------------------------------
    ##  Extra for 'LG_Wrapper_Original' and 'LG_Wrapper_Blocks', add
    ##  '.method'-argument to be used by 'LG_approx_extract'.
    if (target_fun  %in% c("LG_Wrapper_Original",
                           "LG_Wrapper_Blocks")) {
        arg_env$.method <- 
            if (target_fun == "LG_Wrapper_Original") {
                "all"
            } else
                c("median", "mean")
    }
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Testing (and updating) of arguments are now finished.  Inform the
###  user about arguments that failed the sanity-checks.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  Convert 'valid_args' to vector for easier analysis.
    valid_args <- unlist(valid_args)
###-------------------------------------------------------------------
    ##  Stop if any arguments are erroneous.
    if (! all(valid_args)) {
        invalid_args <- names(valid_args)[valid_args == FALSE]
        stop("\t",
             "Erroneous argument",
             ifelse(test = length(invalid_args) == 1,
                    yes = "",
                    no = "s"),
             " in '",
             target_fun,
             "'.\n\t",
             "The following argument",
             ifelse(test = length(invalid_args) == 1,
                    yes = " is ",
                    no = "s are "),
             "invalid: ",
             paste(
                 invalid_args,
                 collapse = ", "),
             call. = FALSE)
#####  TASK: Implement more detailed feedback-routine?  That can wait
#####  until latter on.
    }
###-------------------------------------------------------------------
    ##  Revert the `TS` back to path when relevant.
    if (all(target_fun %in% c("LG_Wrapper_Bootstrap",
                              "LG_boot_approx_scribe"),
            exists(x = "revert_TS_to_path_quote",
                   inherits = FALSE)))
        eval(revert_TS_to_path_quote)
###-------------------------------------------------------------------
    ##  Return nothing to the workflow.
    return(invisible(NULL))
}



################################################################################
#####  2016-12-30: Anticipate some major modifications might be
#####  necessary, and it would be preferable to have backups of the
#####  details here too (in addition to the git-repository).

## ## LG_sanity_checks <- function(
## ##     spy_report) {
## ## ###-------------------------------------------------------------------
## ##     ##  Identify the target function we are checking.
## ##     target_fun <- spy_report$fun
## ##     ##  Register the name of the present function.
## ##     .this_function <- this_function()
## ## ###-------------------------------------------------------------------
## ##     ##  Check that 'spy_report' comes from a valid function.
## ##     valid_fun <- c(
## ##         "LG_approx_scribe", "LG_spectra_scribe",
## ##         "LG_boot_approx_scribe", "LG_boot_spectra_scribe", 
## ##         "LG_Wrapper_Original", "LG_Wrapper_Bootstrap",
## ##         "LG_Wrapper_Blocks", "LG_collect_orig_and_boot",
## ##         "LG_collect_blocks", "LG_shiny")
## ##     ##---
## ##     if (! target_fun %in% valid_fun)
## ##         error(.argument = "spy_report",
## ##               paste(sQuote(.this_function),
## ##                     " attempted used with ",
## ##                     sQuote("spy_report"),
## ##                     " from ",
## ##                     sQuote(target_fun),
## ##                     ".",
## ##                     sep = ""),
## ##               c("Only the following functions have been implemented: ",
## ##                 paste(sQuote(valid_fun),
## ##                       collapse = ", ")))
## ## ###-------------------------------------------------------------------
## ##     ##  Create a shortcut to 'spy_report$envir'.  Reminder: This is
## ##     ##  not a local copy; changes to 'arg_env' will also occur in the
## ##     ##  original environment.
## ##     arg_env <- spy_report$envir
## ## ###-------------------------------------------------------------------
## ##     ##  Check the validity of the 'main_dir'-argument.
## ##     if (! dir.exists(arg_env$main_dir))
## ##         error(.argument = "main_dir",
## ##               n = 3,
## ##               c("Could not find the directory: ",
## ##                 sQuote(arg_env$main_dir)))
## ## #####  TASK: Test this to see that the correct value is returned.
## ## ######  TASK: Return something else than desired when testing with
## ## ######  under construction, but I think the `capture_env()`-function
## ## ######  might become adjusted to remove the need for this...
## ## ###-------------------------------------------------------------------
## ##     ##  Check the existence of the `TS_content`-file.
## ##     TS_content_file <- file.path(arg_env$main_dir,
## ##                                  LG_default$content_file_name)
## ##     if (! file.exists(TS_content_file))
## ##         error(.argument = "main_dir",
## ##               n = 3,
## ##               c("Could not find the file",
## ##                 sQuote(LG_default$content_file_name),
## ##                 "in the directory",
## ##                 sQuote(arg_env$main_dir)))
## ##     ##  If `target_fun` is "LG_shiny", then load `TS_content` into its
## ##     ##  environment, and split `main_dir` into a vector.  Return at
## ##     ##  this stage if `data_dir` is `NULL`.
## ##     if (target_fun == "LG_shiny") {
## ##         gramps <- sys.frame(which = -2)
## ##         load(file = TS_content_file,
## ##              envir = gramps)
## ##         gramps$main_dir <- unlist(strsplit(
## ##             x = spy_report$envir$main_dir,
## ##             split = .Platform$file.sep))
## ##         if (is.null(spy_report$envir$data_dir))
## ##             return(invisible(NULL))
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  Check the validity of the directory-argument, i.e. extract
## ##     ##  either `data_dir` or `spectra_dir`.
## ##     if (target_fun == "LG_Wrapper_Bootstrap") {
## ##         data_dir <- arg_env$spectra_dir
## ##     } else
## ##         data_dir <- arg_env$data_dir 
## ##     ##
## ##     data_dir_path <- paste(c(arg_env$main_dir,
## ##                              data_dir),
## ##                            collapse = .Platform$file.sep)
## ##     if (! dir.exists(data_dir_path))
## ##         error(.argument = "data_dir",
## ##               n = 3,
## ##               c("Could not find a directory named ",
## ##                 sQuote(arg_env$data_dir),
## ##                 "inside the main directory",
## ##                 sQuote(arg_env$main_dir)))
## ##     ##  Add 'data_dir_path'' as 'save_dir' to 'arg_env'
## ##     arg_env$save_dir <- data_dir_path
## ## ###-------------------------------------------------------------------
## ##     ##  Find the path to the info-file.  Note: Only use the first part
## ##     ##  of 'arg_env$data_dir'
## ##     info_path <-
## ##         file.path(paste(c(arg_env$main_dir,
## ##                           head(x = data_dir, n = 1)),
## ##                         collapse = .Platform$file.sep),
## ##                   LG_default$info_file_name)
## ## ###-------------------------------------------------------------------
## ##     ##  Check that the main directory contains the desired info-file.
## ##     if (! file.exists(info_path))
## ##         error(.argument = "data_dir",
## ##               n = 3,
## ##               c("No file named ",
## ##                 sQuote(LG_default$info_file_name),
## ##                 "in the data directory",
## ##                 sQuote(data_dir),
## ##                 "- that was found inside the main directory",
## ##                 sQuote(arg_env$main_dir)))
## ## ###-------------------------------------------------------------------
## ##     ##  Return here if `target_fun` is "LG_shiny".
## ##     if (target_fun == "LG_shiny") 
## ##         return(invisible(NULL))
## ## ###-------------------------------------------------------------------
## ##     ##  Load the info-file to get access to 'info'.
## ##     load(file = info_path)
## ## ###-------------------------------------------------------------------
## ##     ##  Add `TS` to 'spy_report$envir', so the bootstrap procedure
## ##     ##  will work later on.
## ##     if (target_fun == "LG_boot_approx_scribe") 
## ##         spy_report$envir$TS <- info$TS_info$TS
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  Some functions need to look stuff up in the 'info'-object, and
## ## ###  for these we need to check the validity of the bookmarks
## ## ###  corresponding to the directory argument
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  Create an adjusted copy of the defaults for the directories,
## ##     ##  without the first component that refers to the time series.
## ##     folder_defaults <- tail(LG_default$folder_defaults, n = -1)
## ## ###-------------------------------------------------------------------
## ##     ##  For the initial bootstrap-scribes, remove the "spectra.dir"
## ##     ##  part from 'defaults' and 'data_dir'.
## ##     if (target_fun %in% c("LG_boot_approx_scribe",
## ##                           "LG_boot_spectra_scribe"))  {
## ##         folder_defaults <-
## ##             folder_defaults[names(folder_defaults) != "spectra.dir"]
## ##         ##---
## ##         data_dir <- data_dir[names(data_dir) != "spectra.dir"]
## ##         arg_env$data_dir <- data_dir
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  Create the bookmark corresponding to 'data_dir'.
## ##     dir_bookmark <- data_dir[names(data_dir) %in%
## ##                              names(folder_defaults)]
## ## ###-------------------------------------------------------------------
## ##     ##  Check the validity of the bookmark for the relevant functions.
## ##     if (! target_fun %in% c("LG_approx_scribe",
## ##                             "LG_Wrapper_Original",
## ##                             "LG_Wrapper_Blocks")) {
## ##         ##---
## ##         tmp <- try(expr = info[[dir_bookmark]],
## ##                    silent = TRUE)
## ##         ##---
## ##         if (class(tmp) == "try-error")
## ##             error(.argument = "data_dir",
## ##                   n = 3,
## ##                   c(sQuote(paste("info",
## ##                                  paste(dir_bookmark,
## ##                                      collapse = "$"),
## ##                                  sep = "$")),
## ##                     "was not found."))
## ## #####  TASK: Test this error.
## ##         kill(tmp)
## ##     }
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  If the test above didn't terminate this function, then it's time
## ## ###  to sanity check the rest of the arguments and perform updates.
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  The 'TS'-argument, can refer to a file, and it might also be
## ##     ##  that there's an attribute 'TS_for_analysis' that should be
## ##     ##  used.  Some tweaking is thus needed.
## ##     if (! is.null(arg_env$TS)) {
## ##         if (is.character(arg_env$TS)) {
## ##             ##  If `target_fun` is `LG_Wrapper_Bootstrap`, create a
## ##             ##  quote to revert `TS` back to path after testing.
## ##             if (target_fun %in% c("LG_Wrapper_Bootstrap",
## ##                                    "LG_boot_approx_scribe"))
## ##                 revert_TS_to_path_quote <- bquote(
## ##                     arg_env$TS <- .(arg_env$TS))
## ##             ##  Update `arg_env` with `TS`-data from file.
## ##             load(file = paste(c(arg_env$main_dir,
## ##                                 arg_env$TS),
## ##                               collapse = .Platform$file.sep),
## ##                  envir = arg_env)
## ##         }
## ##         ##  Adjust 'TS' to be tested, when required.
## ##         if (! identical(x = attributes(arg_env$TS)$TS_for_analysis,
## ##                         y = NULL)) 
## ##             arg_env$TS <- attributes(arg_env$TS)$TS_for_analysis
## ##     }
## ## ###-------------------------------------------------------------------

## ## ################################################################################
## ## #####  2016-12-30: Need to think a bit more about the multivariate
## ## #####  case, in order to figure out how to deal with this.
    
## ##     ##  Stop the program if a wrapper has been used with the wrong
## ##     ##  kind of argument
## ##     if (spy_report$fun == "LG_Wrapper_Original")
## ##         if (! is.null(attributes(arg_env$TS)$.multivariate_TS))
## ##             if (attributes(arg_env$TS)$.multivariate_TS)
## ##                 error(.argument = "TS",
## ##                       c("The argument 'TS' gives a multivariate",
## ##                         "time-series, while the function",
## ##                         sQuote(spy_report$fun),
## ##                         "requires a univariate time-series."),
## ##                       sep = "")
## ## ###-------------------------------------------------------------------
## ##     ##  The case 'LG_collect_orig_and_boot' differs from the rest, and
## ##     ##  will thus be treated first, i.e. check that 'data_dir' points
## ##     ##  to bootstrap-data, and add some objects to the environment of
## ##     ##  'LG_collect_orig_and_boot', i.e. the grandparent-environment.
## ## #####  TASK: Revise this solution later on, so only one function is
## ## #####  needed when collecting the files.  That should be feasible to
## ## #####  do, I think - but I don't have time available to investigate
## ## #####  this now.
## ##     if (target_fun %in% c("LG_collect_orig_and_boot",
## ##                           "LG_collect_blocks")) {
## ## ###-------------------------------------------------------------------
## ##         ##  Identify the type of bootstrap we are considering.
## ##         boot_type <- gsub(pattern = "\\.dir",
## ##                           replacement = "",
## ##                           x = tail(names(dir_bookmark), 1))
## ## ###-------------------------------------------------------------------
## ##         ##  Check that 'boot_type' is valid.
## ##         if (! boot_type %in% c("boot.approx", "boot.spectra"))
## ##             stop("\t",
## ##                  "In '",
## ##                  spy_report$fun,
## ##                  "',\n\t",
## ##                  "the argument 'data_dir', does not point to bootstrap-data.",
## ##                  call. = FALSE)
## ## #####  TASK: The present approach ignores the testing of the
## ## #####  threshold-value, but I think I will ignore that for the time
## ## #####  being.  I suppose the structure of this function might benefit
## ## #####  from a little bit more planning with regard to when different
## ## #####  stuff is performed, and it would be nice to have everything
## ## #####  going in the flow without any such ad hoc parts like this...
## ## ###-------------------------------------------------------------------
## ##         ##  Some ad hoc adjustments:
## ##         if (target_fun == "LG_collect_blocks") {
## ##             ##  Adjust hierarchy, so 'LG_shiny' finds the data in the same
## ##             ##  configuration as the one used for the ordinary case.
## ##             .new_hierarchy <- LG_default$folder_defaults["boot.spectra.dir"]
## ##             ##  Prepare `info` for storing of new level, or register
## ##             ##  that stuff has been done before.
## ##             if (is.null(info[[c(dir_bookmark, .new_hierarchy)]])) {
## ##                 info[[c(dir_bookmark, .new_hierarchy)]] <- list()
## ##                 done_before <- FALSE
## ##             } else
## ##                 done_before <- TRUE
## ##             ##  Create a collected bookmark for the adding of results.
## ##             collected_bookmark <- c(dir_bookmark,
## ##                                     .new_hierarchy,
## ##                                     LG_default$collected_data)
## ##             ##  Extend data dir for saving to `info`-object later on.
## ##             data_dir <- c(data_dir, .new_hierarchy)
## ##         } else
## ##             done_before <- ! is.null(
## ##             info[[dir_bookmark]][[LG_default$collected_data]])
## ##         ##  Add extra objects to the grandparent-environment, i.e. to
## ##         ##  'LG_collect_orig_and_boot' or `LG_collect_blocks`.
## ##         gramps <- sys.frame(which = -2)
## ##         ##---
## ##         gramps$boot_type <- boot_type
## ##         gramps$data_dir <- data_dir
## ##         gramps$dir_bookmark <- dir_bookmark
## ##         gramps$done_before <- done_before
## ##         gramps$info <- info
## ##         gramps$info_path <- info_path
## ##         gramps$save_dir <- paste(c(arg_env$main_dir,
## ##                                    data_dir),
## ##                                  collapse = .Platform$file.sep)
## ##         if (target_fun == "LG_collect_blocks")
## ##             gramps$collected_bookmark <- collected_bookmark
## ##         ##  Quit this program
## ##         return(invisible(NULL))
## ##     }
## ##     ##  This concludes the treatment of 'LG_collect_orig_and_boot' and
## ##     ##  `LG_collect_blocks`
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  The updates for the remaining functions will mostly be on
## ## ###  'arg_env', the shortcut to the environment 'spy_report$envir'
## ## ###  from the main function, and these are thus automatically updated
## ## ###  at the correct level.  We need an extra step to give
## ## ###  'LG_bookkeeping' access to other objects.
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  Add extra objects to the parent, i.e. to 'LG_bookkeeping'.
## ##     calling_env <- sys.frame(which = -1)
## ##     ##---
## ##     calling_env$data_dir <- data_dir
## ##     calling_env$dir_bookmark <- dir_bookmark 
## ##     calling_env$folder_defaults <- folder_defaults
## ##     calling_env$info <- info
## ##     calling_env$info_path <- info_path
## ##     calling_env$save_dir <- str_split(
## ##         string = data_dir_path,
## ##         pattern = .Platform$file.sep)[[1]]
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  It's now time to check the rest of the arguments, and perform
## ## ###  relevant updates based on 'target_fun'.
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  Create a list to record if the arguments are valid.
## ##     arg_names <- ls(arg_env, all.names = TRUE)
## ##     valid_args <- vector(
## ##         mode = "list",
## ##         length = length(arg_names))
## ##     names(valid_args) <- arg_names
## ## ###-------------------------------------------------------------------
## ##     ##  The 'data_dir' (or 'spectra_dir' in the bootstrap-wrapper) has
## ##     ##  already been tested in the code above.
## ##     if (target_fun == "LG_Wrapper_Bootstrap") {
## ##         valid_args$spectra_dir <- TRUE
## ##         ##  Add 'data_dir' for the sake of 'LG_boot_approx'.
## ##         arg_env$data_dir <- head(arg_env$spectra_dir, -1)
## ##     } else
## ##         valid_args$data_dir <- TRUE
## ## ###-------------------------------------------------------------------
## ##     ##  Select 'check-list' and 'default_list' based on 'target_fun'.
## ##     if (target_fun %in% c("LG_approx_scribe",
## ##                           "LG_spectra_scribe",
## ##                           "LG_Wrapper_Original",
## ##                           "LG_Wrapper_Blocks")) {
## ##         ##  Select check-list.
## ##         check_list <- LG_default$check$original
## ##         ##  Pick out default values to compare against.
## ##         default_list <-
## ##             LG_default[which(names(LG_default) %in% check_list$subset)]
## ##     }
## ##     ##---
## ##     if (target_fun %in% c("LG_boot_approx_scribe",
## ##                           "LG_boot_spectra_scribe",
## ##                           "LG_Wrapper_Bootstrap")) {
## ##         ##  Select check-list.
## ##         check_list <- LG_default$check$bootstrap
## ##         ##  Extract default values from previous computations.
## ##         approx_env <- {
## ##             if (! target_fun == "LG_Wrapper_Bootstrap") {
## ##                 info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir
## ##             } else {
## ##                 info[[arg_env$spectra_dir["approx.dir"]]]$spy_report$envir
## ##             }
## ##         }
## ##         if (! is.null(arg_env$spectra_dir)) {
## ##             spectra_env <-
## ##                 info[[arg_env$spectra_dir[c("approx.dir", "spectra.dir")]]]$spy_report$envir
## ##         } else
## ##             spectra_env <- NULL
## ##         ##  Note: 'spectra_env' will be 'NULL' unless the calling
## ##         ##  function is 'LG_boot_spectra_scribe'.
## ## ###-------------------------------------------------------------------
## ##         ##  Create the default list.  This list contains more than we
## ##         ##  need, but that doesn't matter for the code later on.
## ##         ##  Reminder: The order stuff ensures that arguments souped up
## ##         ##  from the computation of the original spectra "dominates"
## ##         ##  those from the original approximation and the formals in
## ##         ##  'TS_boot_sample'.
## ##         default_list <- c(
## ##             { if (! is.null(spectra_env))
## ##                   as.list(spectra_env,
## ##                           all.names = TRUE) },
## ##             as.list(approx_env,
## ##                     all.names = TRUE),
## ##             formals(TS_boot_sample))
## ##     }
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  Use different tests to update 'valid_args' and 'arg_env'.
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  Check the validity of 'TS', when relevant.
## ##     if ("TS" %in% names(valid_args))
## ##         valid_args$TS <- eval(LG_default$TS_test)
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of arguments that should be a subset of (or
## ##     ##  equal to) the default values.  Insert defaults if necessary.
## ##     for (arg in intersect(arg_names, check_list$subset))
## ##         if (! is.null(arg_env[[arg]])) {
## ##             valid_args[[arg]] <- as.logical(
## ##                 prod(as.character(arg_env[[arg]]) %in%
## ##                      as.character(default_list[[arg]])))
## ##         } else {
## ##             arg_env[[arg]] <- default_list[[arg]]
## ##             valid_args[[arg]] <- TRUE
## ##         }
## ## ###-------------------------------------------------------------------
## ##     ## Check validity of 'logic' arguments.
## ##     for (arg in intersect(arg_names, check_list$logic))
## ##         if (! is.null(arg_env[[arg]])) {
## ##             valid_args[[arg]] <- arg_env[[arg]] %in% c(FALSE, TRUE)
## ##          } else {
## ##              arg_env[[arg]] <- default_list[[arg]]
## ##              valid_args[[arg]] <- TRUE
## ##          }
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of positive 'integer' length one arguments.
## ##     for (arg in intersect(arg_names, check_list$integer_length_one))
## ##         valid_args[[arg]] <- all(
## ##             length(arg_env[[arg]]) == 1,
## ##             is.numeric(arg_env[[arg]]),
## ##             if (is.numeric(arg_env[[arg]])) {
## ##                 all.equal(arg_env[[arg]],
## ##                           round(arg_env[[arg]]))
## ##             } else
## ##                 FALSE,
## ##             arg_env[[arg]] > 0)
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of positive 'integer' vectors.
## ##     for (arg in intersect(arg_names, check_list$integer_vec))
## ##         valid_args[[arg]] <- all(
## ##             is.numeric(arg_env[[arg]]),
## ##             if (is.numeric(arg_env[[arg]])) {
## ##                 all.equal(arg_env[[arg]],
## ##                           round(arg_env[[arg]]))
## ##             } else
## ##                 FALSE,
## ##             all(arg_env[[arg]] > 0))
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of 'numeric' length one arguments in (0, 1).
## ##     for (arg in intersect(arg_names, check_list$numeric_length_one))
## ##         valid_args[[arg]] <- all(
## ##             length(arg_env[[arg]]) == 1, 
## ##             is.numeric(arg_env[[arg]]),
## ##             arg_env[[arg]] < 1,
## ##             arg_env[[arg]] > 0)
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of 'numeric' length two arguments in [0, 1].
## ##     for (arg in intersect(arg_names, check_list$numeric_length_two))
## ##         valid_args[[arg]] <- as.logical(
## ##             prod(length(arg_env[[arg]]) == 2, 
## ##                  is.numeric(arg_env[[arg]]),
## ##                  arg_env[[arg]] <= 1,
## ##                  arg_env[[arg]] >= 0))
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of 'numeric' "no ties" vectors,  
## ##     for (arg in intersect(arg_names, check_list$numeric_vec)) 
## ##         valid_args[[arg]] <- as.logical(
## ##             prod(is.numeric(arg_env[[arg]]),
## ##                  identical(
## ##                      x = length(unique(arg_env[[arg]])),
## ##                      y = length(arg_env[[arg]]))))
## ## ###-------------------------------------------------------------------
## ##     ##  Check validity of 'positive' 'numeric' "no ties"  vectors,
## ##     for (arg in intersect(arg_names, check_list$numeric_vec_positive_or_NULL))
## ##         if (is.null(arg_env[[arg]])) {
## ##             valid_args[[arg]] <- TRUE
## ##         } else
## ##             valid_args[[arg]] <- as.logical(
## ##             prod(is.numeric(arg_env[[arg]]),
## ##                  prod(arg_env[[arg]] > 0),
## ##                  identical(
## ##                      x = length(unique(arg_env[[arg]])),
## ##                      y = length(arg_env[[arg]]))))
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ##     ##  Additional adjustments based on the calling functions.  The
## ##     ##  goal is to ensure that any arguments given with 'NULL' is
## ##     ##  replaced with the correct default values.
## ## #############---------------------------------------------------------
## ##     ##  Compatibility of arguments '.bws_fixed' and '.bws_fixed_only',
## ##     ##  relevant for the four functions 'LG_approx_scribe',
## ##     ##  'LG_Wrapper_Original', 'LG_Wrapper_Blocks',
## ##     ##  'LG_boot_approx_scribe' and 'LG_Wrapper_Bootstrap'.
## ##     if (all(target_fun %in% c("LG_approx_scribe",
## ##                               "LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks",
## ##                               "LG_boot_approx_scribe",
## ##                               "LG_Wrapper_Bootstrap"),
## ##             is.null(arg_env$.bws_fixed),
## ##             arg_env$.bws_fixed_only))
## ##         stop("\t",
## ##              "The combination '.bws_fixed_only' equal to 'TRUE' and",
## ##              "\n\t",
## ##              "'.bws_fixed' equal to 'NULL' is not allowed!",
## ##              call. = FALSE)
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_approx_scribe', 'LG_Wrapper_Original' and
## ##     ##  'LG_Wrapper_Blocks': 'lag_max' (truncate if valid, but too
## ##     ##  high).
## ##     if (all(target_fun %in% c("LG_approx_scribe",
## ##                                "LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks"),
## ##             valid_args$lag_max,
## ##             valid_args$TS))
## ##         arg_env$lag_max <- min(
## ##             arg_env$lag_max,
## ##             length(dimnames(arg_env$TS)$observations) -1)
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_approx_scribe', 'LG_Wrapper_Original' and
## ##     ##  'LG_Wrapper_Blocks': 'dl_vec' (if unspecified: compute if
## ##     ##  possible)
## ##     if (all(target_fun %in% c("LG_approx_scribe",
## ##                               "LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks"),
## ##             is.null(arg_env$dl_vec),
## ##             valid_args$TS,
## ##             valid_args$quantiles,
## ##             valid_args$range_q,
## ##             valid_args$dl_length_out)) {
## ##         if (arg_env$quantiles) {
## ##             arg_env$dl_vec <-
## ##                 quantile(arg_env$TS,
## ##                          seq(from = arg_env$range_q[1],
## ##                              to = arg_env$range_q[2], 
## ##                              length.out = arg_env$dl_length_out))
## ##         } else {
## ##             dl_vec <-
## ##                 seq(from = min(arg_env$TS),
## ##                     to = max(arg_env$TS),
## ##                     length.out = arg_env$dl_length_out)
## ##         }
## ##         valid_args[["dl_vec"]] <- TRUE
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_spectra_scribe': 'cut_vec' (truncate if valid but too high).
## ##     if (all(target_fun == "LG_spectra_scribe",
## ##             valid_args$cut_vec)) {
## ##         cut_max <- 1 + 
## ##             info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir$lag_max
## ##         arg_env$cut_vec <-
## ##             arg_env$cut_vec[arg_env$cut_vec <= cut_max]
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  "LG_Wrapper_Original" and "LG_Wrapper_Blocks": 'cut_vec'
## ##     ##  (truncate if valid but too high).
## ##     if (all(target_fun %in% c("LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks"),
## ##             valid_args$cut_vec,
## ##             valid_args$lag_max))
## ##         arg_env$cut_vec <-
## ##             arg_env$cut_vec[arg_env$cut_vec <= arg_env$lag_max + 1]
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_spectra_scribe': 'cut_vec' (if unspecified: compute if
## ##     ##  possible)
## ##     if (all(target_fun == "LG_spectra_scribe",
## ##             is.null(arg_env$cut_vec))) {
## ##         cut_max <- 1 + 
## ##             info[[arg_env$data_dir["approx.dir"]]]$spy_report$envir$lag_max
## ##         arg_env$cut_vec <- 1:cut_max
## ##         valid_args$cut_vec <- TRUE
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_Wrapper_Original' and "LG_Wrapper_Blocks": 'cut_vec' (if
## ##     ##  unspecified: compute if possible)
## ##     if (all(target_fun %in% c("LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks"),
## ##              is.null(arg_env$cut_vec),
## ##              valid_args$lag_max)) {
## ##         cut_max <- 1 + arg_env$lag_max
## ##         arg_env$cut_vec <- 1:cut_max
## ##         valid_args$cut_vec <- TRUE
## ##     }
## ## #####  TASK: Should I include a truncation of lag_max too (for the
## ## #####  case 'LG_Wrapper_Original')?  It should be done for the
## ## #####  bootstrap-case, but it might not be that important for the
## ## #####  original case?
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_spectra_scribe', "LG_Wrapper_Original" and
## ##     ##  "LG_Wrapper_Blocks": 'omega_vec' (compute if possible)
## ##     if (all(target_fun %in% c("LG_spectra_scribe",
## ##                                "LG_Wrapper_Original",
## ##                               "LG_Wrapper_Blocks"),
## ##             is.null(arg_env$omega_vec),
## ##             valid_args$omega_length_out)) {
## ##         arg_env$omega_vec <- seq(
## ##             from = 0, to = 1/2,
## ##             length.out = arg_env$omega_length_out)
## ##         valid_args$omega_vec <- TRUE
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  'LG_spectra_scribe', "LG_Wrapper_Original" and
## ##     ##  "LG_Wrapper_Blocks": 'omega_length_out' (ignore value if
## ##     ##  'omega_vec' is well defined).
## ##     if (target_fun %in% c("LG_spectra_scribe",
## ##                           "LG_Wrapper_Original",
## ##                           "LG_Wrapper_Blocks")) {
## ##         if (all(! valid_args$omega_length_out,
## ##                 valid_args$omega_vec))
## ##             valid_args$omega_length_out <- TRUE
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  For 'LG_boot_approx_scribe' and 'LG_Wrapper_Bootstrap':
## ##     ##  'boot_seed', 'block_length', 'nb' and 'lag_max'
## ##     if (target_fun %in% c("LG_boot_approx_scribe",
## ##                           "LG_Wrapper_Bootstrap")) {
## ## ###-------------------------------------------------------------------
## ##         ##  Pick a 'boot_seed' if necessary:
## ##         if (is.null(arg_env$boot_seed)) {
## ##             arg_env$boot_seed <- as.numeric(
## ##                 paste(sample(0:9, 9, replace = TRUE),
## ##                       collapse = ""))
## ##             valid_args$boot_seed <- TRUE
## ##         }
## ## ###-------------------------------------------------------------------
## ##         ##  Insert default values for arguments that still are 'NULL'.
## ##         is_NULL <- unlist(
## ##             lapply(X = arg_env,
## ##                    FUN = is.null))
## ##         null_names <-
## ##             names(is_NULL[is_NULL == TRUE])
## ##         for (arg in null_names) {
## ##             arg_env[[arg]] <- default_list[[arg]]
## ##             valid_args[[arg]] <- TRUE
## ##         }
## ## ###-------------------------------------------------------------------
## ##         ##  Add '.adjustment_rule' to the argument-environment
## ##         arg_env$.adjustment_rule <- attributes(attributes(
## ##             info$TS_info$TS)$TS_for_analysis)$.adjustment_rule
## ##     }
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ##     ##  Extra for 'LG_boot_spectra_scribe': 'spectra_dir'.
## ##     if (target_fun == "LG_boot_spectra_scribe") {
## ##         ##  Test if we get a valid bookmark from 'spectra_dir'.
## ##         spectra_bookmark <- 
## ##             arg_env$spectra_dir[names(arg_env$spectra_dir) %in%
## ##                                 names(folder_defaults)]
## ##         ##  Check the validity of the bookmark
## ##         tmp <- try(expr = info[[spectra_bookmark]],
## ##                    silent = TRUE)
## ##         ##---
## ##         valid_args$spectra_dir <-
## ##             class(tmp) != "try-error"
## ## #####  TASK: Create error-message too?
## ##         ## stop("\n\t",
## ##         ##      "Can't find previous data for 'spectra_dir'",
## ##         ##      "\n\t",
## ##         ##      paste(arg_env$spectra_dir,
## ##         ##            collapse = .Platform$file.sep))
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  Extra for 'LG_Wrapper_Original' and 'LG_Wrapper_Blocks', add
## ##     ##  '.method'-argument to be used by 'LG_approx_extract'.
## ##     if (target_fun  %in% c("LG_Wrapper_Original",
## ##                            "LG_Wrapper_Blocks")) {
## ##         arg_env$.method <- 
## ##             if (target_fun == "LG_Wrapper_Original") {
## ##                 "all"
## ##             } else
## ##                 c("median", "mean")
## ##     }
## ## ###-------------------------------------------------------------------
## ## #############---------------------------------------------------------
## ## ###  Testing (and updating) of arguments are now finished.  Inform the
## ## ###  user about arguments that failed the sanity-checks.
## ## #############---------------------------------------------------------
## ## ###-------------------------------------------------------------------
## ##     ##  Convert 'valid_args' to vector for easier analysis.
## ##     valid_args <- unlist(valid_args)
## ## ###-------------------------------------------------------------------
## ##     ##  Stop if any arguments are erroneous.
## ##     if (! all(valid_args)) {
## ##         invalid_args <- names(valid_args)[valid_args == FALSE]
## ##         stop("\t",
## ##              "Erroneous argument",
## ##              ifelse(test = length(invalid_args) == 1,
## ##                     yes = "",
## ##                     no = "s"),
## ##              " in '",
## ##              target_fun,
## ##              "'.\n\t",
## ##              "The following argument",
## ##              ifelse(test = length(invalid_args) == 1,
## ##                     yes = " is ",
## ##                     no = "s are "),
## ##              "invalid: ",
## ##              paste(
## ##                  invalid_args,
## ##                  collapse = ", "),
## ##              call. = FALSE)
## ## #####  TASK: Implement more detailed feedback-routine?  That can wait
## ## #####  until latter on.
## ##     }
## ## ###-------------------------------------------------------------------
## ##     ##  Revert the `TS` back to path when relevant.
## ##     if (all(target_fun %in% c("LG_Wrapper_Bootstrap",
## ##                                    "LG_boot_approx_scribe"),
## ##             exists(x = "revert_TS_to_path_quote",
## ##                         inherits = FALSE)))
## ##         eval(revert_TS_to_path_quote)
## ## ###-------------------------------------------------------------------
## ##     ##  Return nothing to the workflow.
## ##     return(invisible(NULL))
## ## }
