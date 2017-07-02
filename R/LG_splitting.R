################################################################################
#'
#' Split local Gaussian related computations into smaller pieces
#'
#' To reduce the chance for out of memory problems (see
#' \code{help("Memory-limits")} for more information), some
#' computations will be partitioned into smaller chunks.  This
#' function checks if a splitting is required, and it gives a
#' partition of the arguments that should (hopefully) ensure that the
#' available amount of memory is sufficient for the task at hand.
#' Warning: This is a rather crude approach, that does not measure how
#' much memory that is available, i.e. it might fail if the work-space
#' is cluttered with large objects.
#'
#' @details The argument and value of this function reflects that it
#'     is an internal function of the \code{LG_scribe}-functions.
#'     There are two aspects this function takes care of.  First of
#'     all it aims at ensuring that the final objects that are saved
#'     to file(s) stay below some threshold.  Moreover, as some
#'     intermediate objects can grow very large (in particular when
#'     dealing with bootstrap-replicates), it will also check if a
#'     computation should be divided into smaller chunks in order to
#'     avoid memory problems.  The strategy used in order to obtain
#'     this is to "split" the array by chopping up some of the
#'     dimensions.
#'
#' @param books The (internal) result we get from
#'     \code{LG_bookkeeping}, that contains all the information needed
#'     for this function.  This includes paths to data-files, the size
#'     and dimension-names of the objects found in these -- and the
#'     arguments that will be used in the desired computation based
#'     upon these data.
#'
#' @return This function will return a "loop-list" to be used
#'     internally in the \code{LG_scribe}-function.  This is a list
#'     with the following components: \code{data_files} that gives the
#'     paths to the files that the main-function must read to get
#'     access to the data. \code{arg_list} that contains the arguments
#'     needed for the computation -- and this part will also decide if
#'     a computation must be split into smaller chunks due to large
#'     intermediate objects.
#'
#' @export

LG_splitting <- function(books) {
###-------------------------------------------------------------------
    ##  Identify the calling function
    target_fun <- books$info[[books$bookmark]]$spy_report$fun
###-------------------------------------------------------------------
    ##  Check that 'books' commes from a function that 'LG_splitting'
    ##  knows how to handle.
    valid_fun <- c(
        "LG_approx_scribe", "LG_spectra_scribe",
        "LG_boot_approx_scribe", "LG_boot_spectra_scribe")
    ##---
    if (! target_fun %in% valid_fun)
        error(.argument = "books",
              c("Function attempted used with",
                sQuote("books"),
                "created in the function",
                sQuote(target_fun)),
              "Don't know what to do...")
###-------------------------------------------------------------------    
    ##  Extract the arguments as a list (environments must be cloned
    ##  if we want to use them instead).
    arg_list <- as.list(
        books$info[[books$bookmark]]$spy_report$envir,
        all.names = TRUE)
###-------------------------------------------------------------------
    ##  For the boot_approx and boot_spectra cases, we need some of
    ##  the arguments from the original computation.
    if (target_fun %in% c("LG_boot_approx_scribe",
                          "LG_boot_spectra_scribe")) {
        orig_arg_list <- as.list(
            books$info[[head(books$bookmark, -1)]]$spy_report$envir,
            all.names = TRUE)
        ##---
        if (target_fun == "LG_boot_approx_scribe")
            orig_arg_list <-
                orig_arg_list[setdiff(
                    x = intersect(
                        x = names(orig_arg_list),
                        y = names(arg_list)),
                    y = "data_dir")]
    }    
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Use 'target_fun' to figure out the type of, and the size of,
###  ingoing files, i.e. for 'LG_approx_scribe' the size will depend
###  on the original time series 'TS', whereas 'LG_boot_approx_scribe'
###  also need to take into account the number 'nb' of
###  bootstrap-replicates that we want to investigate.  For
###  'LG_spectra_scribe' and 'LG_boot_spectra_scribe' the data comes
###  from files, and since the previous code (depending on the value
###  of 'threshold') could have divided the result into several files,
###  each file must be investigated separately.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    if (target_fun %in% c("LG_approx_scribe", "LG_boot_approx_scribe")) {
        ##  Extract the relevant information from the extended version
        ##  of 'LG_points', i.e. the number of rows (to be splitt),
        ##  and attributes that should be kept in the result.
        orig_LG_points <- LG_extend_points(LG_points = orig_arg_list$LG_points)
        orig.vec_LG_points <- seq_len(nrow(orig_LG_points))
        LG_points <- LG_extend_points(LG_points = arg_list$LG_points)
        .vec_LG_points <- seq_len(nrow(LG_points))
        .attributes_LG_points <- local({
            .keep <- ! names(attributes(LG_points)) %in% c("dim", "dimnames")
            attributes(LG_points)[.keep]
        })
    }
    ##  Initiate 'loop_list'
    loop_list <- list()
###-------------------------------------------------------------------
#####   TASK: Write better code for the 'approx'-case.
    if (target_fun == "LG_approx_scribe") {
        loop_list <- list(
            data_files = NA,
            restrict_list = NA,
            ##  HERE
            arg_list = split_vector(
                vec = .vec_LG_points,
                pieces = 1, #  No splitting at all!
                compute_name = "LG_points",
                subset_name = "LG_points",
                add_to_compute = arg_list[names(arg_list) != "LG_points"]))
####  End of case 'target_fun == "LG_approx_scribe'.
    } else {
        ##  The three other cases.
###-------------------------------------------------------------------
        ##  Use 'split' on 'data_files_df' in order to get a list of
        ##  the ingoing data from the previous step.
        ingoing_files <- split(
            x = books$data_files_df,
            f = books$data_files_df$part_a_of_b)
###-------------------------------------------------------------------
        ##  Fill loop list with content.
        for (part in seq_along(ingoing_files)) {
###-------------------------------------------------------------------
            ##  Pick out the relevant data-files as a vector.
            data_files <- structure(
                .Data = ingoing_files[[part]]$data_files,
                .Names = ingoing_files[[part]]$content)
###-------------------------------------------------------------------
            ##  Based on 'target_fun' investigate the size of the
            ##  ingoing object in this 'part' of 'ingoing_files'.
            ingoing_dimnames <-
                books$info[[head(books$bookmark, -1)]]$dimnames.par_five[[part]]
###-------------------------------------------------------------------
            ##  Get the size of the ingoing object (in MB), but never
            ##  use less than 1 MB.
            ingoing_size <-
                max(ingoing_files[[part]]$object_size_MB, 1)
###-------------------------------------------------------------------
            ##  Adjustment-factor based on 'target_fun'
            if (target_fun == "LG_boot_approx_scribe") {
                ##  Take into account possible restrictions on the
                ##  arguments 'lag_max', "LG_points' and 'bw_points'
                adjust_restricted_args <- prod(
                    divide_by(e1 = arg_list$lag_max,
                              e2 = orig_arg_list$lag_max),
                    divide_by(e1 = length(arg_list$bw_points),
                              e2 = length(orig_arg_list$bw_points)),
                    divide_by(e1 = length(.vec_LG_points),
                              e2 = length(orig.vec_LG_points)))
                ##---
                ##  Multiply with 'nb' and adjust for the dropping of
                ##  'eflag' from the 'variable'-dimension.
##### TASK 2015-12-15: This detail with regard to 
                adjust_common <-
                    adjust_final <- prod(
                        adjust_restricted_args,
                        arg_list$nb,
                        divide_by(
                            e1 = length(ingoing_dimnames$variable) - 1,
                            e2 = length(ingoing_dimnames$variable)))
            } else {
                ##  The two spectra-cases (ordinary and boot).
                adjust_common <- divide_by(
                    prod(length(arg_list$omega_vec),
                         length(arg_list$cut_vec),
                         length(arg_list$window)),
                    length(ingoing_dimnames$variable))
###  Note: For "LG_spectra_scribe" this will be the adjustment factor
###  to the final object, whereas it will be the adjustment to the
###  intermediate object in "LG_boot_spectra_scribe"
###  -------------------------------------------------------------------
                ##  Adjustment-factor for final result.
                adjust_final <- {
                    if (target_fun == "LG_spectra_scribe") {
                        adjust_common
                    } else {
                        divide_by(
                            prod(adjust_common,
                                 ifelse(test = identical(arg_list$all_statistics, FALSE),
#####  TASK: Make this more robust, but how?
                                        yes = 9,
                                        no = 14)),
                            prod(length(ingoing_dimnames$content),
                                 length(ingoing_dimnames$lag)))
                    }
                }
            }
####  End of the computation of adjustment-factors.
###-------------------------------------------------------------------
            ## Estimate the size of the final object?
            outgoing_size <- prod(
                ingoing_size,
                adjust_final)
###-------------------------------------------------------------------
            ## Compute the number of final pieces, that never should
            ## be smaller than one (rounding might imply that
            ## 'outgoing_size' is 0 when measured in MB).
            final_pieces <-
                max(1, ceiling(outgoing_size/arg_list$threshold))
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Note: The number 'final_pieces' is to be used with regard to
###  restricting the size of the ingoing data, which thus will be
###  treated as one chunk and saved accordingly.  Note however that
###  this number likely will be smaller than the actuall number we end
###  out with, due to the need for 'allow_extra_pieces=TRUE' in
###  'split_vector' -- which ensures that no files are to big.
###  Moreover, in the boot_spectra-case, we might (depending on the
###  number of bootstrap-replicates) need to chop the ingoing data
###  into much smaller chunks in order to avoid memory-problems when
###  we deal with the intermediate objects.
#############---------------------------------------------------------
###-------------------------------------------------------------------
            ##  Adjusted number for 'final_pieces' for the
            ##  boot_spectra case.

#####  2017-04-30: I have not checked it properly, but I think the
#####  revised solution for the treatment of 'window' and 'cut' might
#####  make the next part mostly obsolete.  I think some solution will
#####  be needed here, but for the moment I think the base code might
#####  be sufficient...
            
## ## ## ##             if (target_fun == "LG_boot_spectra_scribe") {
## ## ## ##                 ##  Register the limits one how much we can divide.
## ## ## ##                 divide_limit_out <- prod(
## ## ## ##                     unlist(
## ## ## ##                         lapply(X = arg_list[c("omega_vec", "window")],
## ## ## ##                                FUN = length)))
## ## ## ##                 ##---
## ## ## ##                 divide_limit_in <- prod(local({
## ## ## ##                     .bws <- if (orig_arg_list$.bws_fixed_only) {
## ## ## ##                                 ".bws_fixed"
## ## ## ##                             } else
## ## ## ##                                 c("bw_points",
## ## ## ##                                   ".bws_fixed")
## ## ## ##                     #####  REMINDER: The above solution not complete
## ## ## ##                     #####  with regard to the threatment of
## ## ## ##                     #####  '.bws_mixture' and the possibility that
## ## ## ##                     #####  '.bws_fixed' might be 'NULL'
## ## ## ##                     unlist(
## ## ## ##                         lapply(X = orig_arg_list[c("LG_points", .bws)],
## ## ## ##                                FUN = length))
## ## ## ##                 }))
## ## ## ##                 ##---
## ## ## ##                 max_divide <- prod(divide_limit_in,
## ## ## ##                                    divide_limit_out)
## ## ## ## #####  TASK: These three divide values seems to be common for the
## ## ## ## #####  spectra cases, and as such I guess they might just as well be
## ## ## ## #####  defined outside of this if-statement.
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##                 ##  Investigate the effect of cutting along 'cut_vec'.
## ## ## ##                 cut_vec_split <- LG_cut_vec_splitter(
## ## ## ##                     cut_vec = arg_list$cut_vec,
## ## ## ##                     ingoing_size = prod(
## ## ## ##                         ingoing_size,
## ## ## ##                         adjust_common),
## ## ## ##                     lag_max = orig_arg_list$lag_max,
## ## ## ##                     threshold = arg_list$threshold)
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##                 ##  The highest 'threshold_ratio' from 'cut_vec_split'
## ## ## ##                 ##  will inform us what we need to do.
## ## ## ##                 worst_case <- max(cut_vec_split$threshold_ratio)
## ## ## ## #####  Reminder, 2017-04-30: Deactivated the test below for the time
## ## ## ## #####  being.  Most likely necessary to find another approach for the
## ## ## ## #####  general case in order to avoid these problems.
## ## ## ## ## ## ## ## ## #############---------------------------------------------------------
## ## ## ## ## ## ## ## ## ####  If 'worst_case' exceeds 'max_divide', then this approach will
## ## ## ## ## ## ## ## ## ####  not work (will require a combination of a high value for the
## ## ## ## ## ## ## ## ## ####  truncation point, a huge value for the number of
## ## ## ## ## ## ## ## ## ####  bootstrap-replicates and a "low" memory threshold).
## ## ## ## ## ## ## ## ## #############---------------------------------------------------------
## ## ## ## ## ## ## ## ## ###-------------------------------------------------------------------
## ## ## ## ## ## ## ## ##                 if (worst_case > max_divide)
## ## ## ## ## ## ## ## ##                     stop("\t",
## ## ## ## ## ## ## ## ##                          "The present combination of highest truncation point, ",
## ## ## ## ## ## ## ## ##                          max(arg_list$cut_vec),
## ## ## ## ## ## ## ## ##                          ",\n\t",
## ## ## ## ## ## ## ## ##                          "and number of bootstrap-replicates, ",
## ## ## ## ## ## ## ## ##                          orig_arg_list$nb,
## ## ## ## ## ## ## ## ##                          ",\n\t",
## ## ## ## ## ## ## ## ##                          "requires more memory than the present threshold of ",
## ## ## ## ## ## ## ## ##                          arg_list$threshold,
## ## ## ## ## ## ## ## ##                          " MB.",
## ## ## ## ## ## ## ## ##                          call. = FALSE)
## ## ## ## #############---------------------------------------------------------
## ## ## ## ####  If 'worst_case' is between 'max_divide' and 'divide_limit_out',
## ## ## ## ####  then the ingoing data must be divided.
## ## ## ## #############---------------------------------------------------------
## ## ## ## ###-------------------------------------------------------------------
## ## ## ##                 if (worst_case > divide_limit_out) {
## ## ## ##                     ##  Compute the required division of ingoing data.
## ## ## ##                     final_pieces <- ceiling(
## ## ## ##                         divide_by(e1 = worst_case,
## ## ## ##                                   e2 = divide_limit_out))
## ## ## ##                 }
## ## ## ##             }
###-------------------------------------------------------------------
#############---------------------------------------------------------
####  With the possible modification of 'ingoing_pieces' due to the
####  above 'if'-statement for the boot_spectra case, the stage is set
####  for a computation of our 'restrict_list'
#############---------------------------------------------------------
###-------------------------------------------------------------------
            ## Initiate a restrict list for 'aa_restrict'.
            restrict_list <-
                vector("list", length = final_pieces)
###-------------------------------------------------------------------            
            ##  Split ingoing-data based upon 'levels'.
            ingoing_split <- split_vector(
                vec = ingoing_dimnames$levels,
                pieces = final_pieces,
                allow_extra_pieces = TRUE,
                subset_name = "levels")
################################################################################
            if (final_pieces > 1 & target_fun == "LG_boot_approx_scribe")
                error(c("REMINDER: Need to update the algorithm for",
                        "the splitting.",
                        "Naive method will not work well for all cases,",
                        "due to the way some computations will need",
                        "to use information about diagonally reflected points..."))
################################################################################
####  TASK: This will only work if 'final_pieces' is smaller than the
####  length of 'levels', and some more tweaking might be necessary to
####  include here.  But as the 'bw_points' dimension is thought to be
####  temporary, this can't be prioritized now.
###---
####  Note: The split is with regard to the dimnames related to the
####  positive lags, but as we only will use the 'subset' part of the
####  result from 'split_vector' that doesn't matter.
###-------------------------------------------------------------------
            ##  Initiate preliminary 'add_to_loop'
            add_to_loop <- vector("list", length(ingoing_split$subset))
            for (i in seq_along(add_to_loop)) 
                add_to_loop[[i]] <- list(
                    data_files = data_files,
                    restrict_ingoing_necessary = final_pieces != 1,
                    restrict_ingoing =
                        ingoing_split$subset[[i]],
#############---------------------------
                    new_intermediate_size = prod(
                        ingoing_size,
                        adjust_common,
                        divide_by(
                            length(ingoing_split$subset[[i]]$levels),
                            length(ingoing_dimnames$levels))),
#############---------------------------
                    intermediate_pieces = max(
                        1,
                        ceiling(
                            divide_by(
                                prod(ingoing_size,
                                     adjust_common,
                                     divide_by(
                                         length(ingoing_split$subset[[i]]$levels),
                                         length(ingoing_dimnames$levels))),
                                arg_list$threshold))) )
####  Note: It's the sizes after 'ingoing_split' that must be taken
####  into account with regard to the need for a splitting of the
####  intermediate computation in "LG_boot_spectra_scribe".  The part
####  'intermediate_pieces' should be 1 for the other cases.
###-------------------------------------------------------------------
            ##  Based on 'intermediate_pieces', investigate if a
            ##  further splitting is necessary.  (This should only be
            ##  the case for "LG_boot_spectra_scribe".)
#######################-----------------------------------------------
#####  TASK: The idea of splitting along cut_vec_once more might not
#####  reduce the size as much as I anticipated - or perhaps I have
#####  messed up something with regard to the selected size of the
#####  intermediate object?  However, in as far as it seems to be
#####  enough to split along 'omega_vec', its more tempting to let it
#####  stay at that.
            ## capture_env()
            ## 
            ##     ##  Investigate the effect of cutting along 'cut_vec'.
            ## cut_vec_split <- LG_cut_vec_splitter(
            ##     cut_vec = arg_list$cut_vec,
            ##     ingoing_size = add_to_loop[[i]]$new_intermediate_size,
            ##     lag_max = orig_arg_list$lag_max,
            ##     threshold = arg_list$threshold)
#######################-----------------------------------------------
            for (i in seq_along(add_to_loop)) {
                add_to_loop[[i]]$arg_list <- {
                    ##  Must discern based on 'target_fun'.
                    if (target_fun == "LG_boot_approx_scribe") {
                        ##  No splitting will happen at this stage,
                        ##  only 'compute' part will be used later on.
                        split_vector(
                            vec = .vec_LG_points,
                            pieces = add_to_loop[[i]]$intermediate_pieces,
                            compute_name = "LG_points",
                            subset_name = "LG_points",
                            add_to_compute = arg_list[names(arg_list) != "LG_points"])
                    } else {
###-------------------------------------------------------------------
                        ##  The two spectra cases: Take into account
                        ##  the number of available cores in order to
                        ##  do stuff in parallel.  Round up
                        ##  'intermediate_pieces' to the closest
                        ##  multiplum of available cores in the
                        ##  backend, minus one (due to the use of
                        ##  'allow_extra_pieces' in 'split_vector').
                        ##  More is not always better, should a
                        ##  maximum number of cores be used?
                        p <- add_to_loop[[i]]$intermediate_pieces 
                        c <- getDoParWorkers()
                        ##---
                        add_to_loop[[i]]$intermediate_pieces <-
                            c * {p %/% c + 1} - 1
                        ##---
                        ##  Use split_vector to find 'arg_list'.
                        split_vector(
                            vec = arg_list$omega_vec,
                            pieces = add_to_loop[[i]]$intermediate_pieces,
                            allow_extra_pieces = TRUE,
                            compute_name = "omega_vec",
                            subset_name = "omega",
                            add_to_compute = arg_list[names(arg_list) != "omega_vec"])
                    }
                    ##  End of computation of 'arg_list'.
                }
                ##---
                add_to_loop[[i]]$split_intermediate <-
                    add_to_loop[[i]]$intermediate_pieces != 1
                ##---
                add_to_loop[[i]]$intermediate_pieces <- NULL
            }
###-------------------------------------------------------------------
            ##  Add 'add_to_loop' to 'loop_list'
            loop_list <- c(
                loop_list,
                add_to_loop)
        }
#####  End of loop: 'part in seq_along(ingoing_files)'
    }
#####  End of 'if'-statement 'target_fun %in% c("LG_spectra_scribe",
#####  "LG_boot_spectra_scribe")'
###-------------------------------------------------------------------
    ##  Add names to loop list.
    if (length(loop_list) == 1) {
        names(loop_list) <- " "
    } else {
        names(loop_list) <- paste(
            "part_",
            seq_along(loop_list),
            "_of_",
            length(loop_list),
            sep = "")
    }
###-------------------------------------------------------------------
    if (target_fun %in% c("LG_approx_scribe", "LG_boot_approx_scribe")) {
        ##  Replace indices for the rows of 'LG_point' with the
        ##  required array.
        for (i in seq_along(loop_list)) {
            .tmp_c <- loop_list[[i]]$arg_list$compute
            .tmp_s <- loop_list[[i]]$arg_list$subset
            for (j in seq_along(.tmp_c)) {
                .vec <- .tmp_c[[j]]$LG_points
                .tmp_c[[j]]$LG_points <- local({
                    .tmp <- LG_points[.vec, , drop = FALSE]
                    attributes(.tmp) <- c(
                        attributes(.tmp),
                        .attributes_LG_points)
                    .tmp
                })
                .tmp_s[[j]]$LG_points <- 
                    rownames(LG_points)[.vec]
            }
            loop_list[[i]]$arg_list$compute <- .tmp_c
            loop_list[[i]]$arg_list$subset <- .tmp_c
        }
        loop_list[[1]]$arg_list$compute[[1]]$LG_points
    }    
###-------------------------------------------------------------------
    ##  Return 'loop_list' to the work-flow.
    return(loop_list)
}
