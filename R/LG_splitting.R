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
    valid_fun <- c("LG_approx_scribe", "LG_boot_approx_scribe")
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
    ##  For the boot_approx case, we need some of the arguments from
    ##  the original computation.
    if (target_fun == "LG_boot_approx_scribe") {
        orig_arg_list <- as.list(
            books$info[[head(books$bookmark, -1)]]$spy_report$envir,
            all.names = TRUE)
        ##---
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
###  bootstrap-replicates that we want to investigate.
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
                adjust_common <-
                    adjust_final <- prod(
                        adjust_restricted_args,
                        arg_list$nb,
                        divide_by(
                            e1 = length(ingoing_dimnames$variable) - 1,
                            e2 = length(ingoing_dimnames$variable)))
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
    ##  Replace indices for the rows of 'LG_point' with the required
    ##  array.
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
###-------------------------------------------------------------------
    ##  Return 'loop_list' to the work-flow.
    return(loop_list)
}
