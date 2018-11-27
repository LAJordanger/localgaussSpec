################################################################################
#'
#' Merge files containing data created by different steps.
#'
#' @details Some plots depends on content from different parts of the
#'     overall algorithm, for these cases it's of interest to merge
#'     the relevant data into new files.  This process is not straight
#'     forward, since the data from the different steps might be
#'     distributed over several files.  (This function will most likely
#'     be an internal function in the package, i.e. that it will not
#'     be exported.)
#'
#' @param components A list containing information that identifies the
#'     names of the files (in the directories specified by
#'     \code{source_dir}) that contains the data to be loaded, and
#'     also names to be used on the files where the merged that will
#'     be saved (in the directory specified by \code{save_dir})
#'
#' @param source_dir A list with information related to the paths
#'     needed to find the directories where the data of interest are
#'     stored.
#'
#' @param save_dir The information needed to find the directory where
#'     the merged files will be saved (with names as given in
#'     \code{components}).
#'
#' @return The primary goal of this function is to merge the files,
#'     and save them at the assigned positions in the file-hierarchy.
#'     When this is done, the \code{components} object will be updated
#'     with information about the sizes (in MB) of the saved objects,
#'     some excess information will be pruned away, and the revised
#'     object will then be returned to the workflow (in order for the
#'     calling function to update the \code{info}-object with it).
#'
#' @export
        

LG_merge_files <- function(components,
                           source_dir,
                           save_dir) {
###-------------------------------------------------------------------
#############---------------------------------------------------------
###-------------------------------------------------------------------
###  This function assumes that the function that called it checked
###  the sanity of the arguments.  The extraction and pasting together
###  of the desired information will be done by using a
###  parallel-divide on the top-level, since the updating of the final
###  array by 'update_array' must be done sequentially.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ## Create an argument-list to use with 'foreach'.
    loop_arg_list <- list()
    for (content in names(components)) {
        pieces <- length(components[[content]]$combined$file_names)
        for (i in 1:pieces) 
            loop_arg_list <- c(
                loop_arg_list,
                list(list(content = content, part = i)))
    }
    kill(content, i, pieces)
    ##  Extract information about the part of the second level in
    ##  'components', that gives information about the sources.
    ##  to know which source to loop from.
    source_loop <- setdiff(
        x = names(components[[1]]),
        y = "combined")
###-------------------------------------------------------------------
    ##  Let foreach take care of the loading and saving of files, and
    ##  only return the value needed for an update of the
    ##  'object_size_MB'-part of 'components'

    saved_sizes_MB <- foreach(i = seq_along(loop_arg_list)) %dopar% {
#####
    ##  Drop parallel when testing
    ## saved_sizes_MB <- foreach(i = seq_along(loop_arg_list)) %do% {
#####
        ##  Extract the relevant bookmarks.
        content <- loop_arg_list[[i]]$content
        part <- loop_arg_list[[i]]$part
        kill(i)

        
        
###-------------------------------------------------------------------
        ##  We need the array to fill out with data, and the files
        ##  from which those data should be extracted.
###-------------------------------------------------------------------
        ##  Find the dimnames of the result
        dimnames_result <-
            components[[content]]$combined$dimnames[[part]]
        ##  Create a template for the desired array.
        list_array_result <- LG_default$result$skeleton
        for (.node in LG_default$result$array_nodes) {
            ##  Skip any empty parts.
            if (identical(x = dimnames_result[[.node]],
                          y = NULL))
                next
            ##  Initiate an array to be updated later on.
            list_array_result[[.node]] <- structure(
                .Data = array(
                    data = NA_real_,
                    dim = vapply(
                        X = dimnames_result[[.node]],
                        FUN = length,
                        FUN.VALUE = numeric(1)),
                    dimnames = dimnames_result[[.node]]),
                class = LG_default$class$array)
        }
        kill(.node)
        ##  Double loop: 'source_loop' + 'file_loop'.
        for (.source in source_loop) {
            ##  Find the file-names that contains the desired objects.
            file_loop <- 
                components[[c(content, .source, "file_names")]]
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  To avoid loading files with no contribution to the desired
###  result, figure out by the help of 'dimnames_intersect' if any of
###  the files can be ignored. (Use 'vapply' and a helper function.)
#############---------------------------------------------------------
###-------------------------------------------------------------------
            .help_fun <- function(x) {
                .tmp <- vapply(
                    X = LG_default$result$array_nodes,
                    FUN = function(.node) {
                        ##  Empty parts are by default not needed.
                        if (identical(x = dimnames_result[[.node]],
                                      y = NULL)) {
                            FALSE
                        } else {
                            ##  Check if non-emtpy intersection.
                            .tmp <- dimnames_intersect(
                                dimnames1 = x[[.node]],
                                dimnames2 = dimnames_result[[.node]])
                            length(.tmp) > 0
                        }
                    },
                    FUN.VALUE = logical(1))
                ##  The component is needed if at least one is 'TRUE'.
                any(.tmp)
            }
            ##  The list of intersection.
            .keepers <- vapply(
                X = components[[c(content, .source, "dimnames")]],
                FUN = .help_fun,
                FUN.VALUE = logical(1))
            ##  Revised version of 'file_loop'.
            file_loop <- file_loop[.keepers]
            kill(.help_fun, .keepers)
###-------------------------------------------------------------------
            ##  Load the files and extract the relevant data.
            for (.file in file_loop) {
                ##  Load the desired object into this
                ##  function-environment.
                LG_load(.file = file.path(source_dir[[.source]],
                                          .file),
                        .name = "ingoing_list_array")
                ##  Read the dimension-names.
                .dn_la <- attributes(ingoing_list_array)$list_array_dimnames
                ##  Loop over the array nodes and extract the desired
                ##  components into 'list_array_result'.

                ## ## ## ## capture_env() 
                for (.node in LG_default$result$array_nodes) {
                    ##  Do nothing if no array is present.
                    if (identical(x = .dn_la[[.node]],
                                  y = NULL))
                        next
                    ##  Identify the part of the array-node that we
                    ##  want to extract.  Remember: Use
                    ##  'dimnames_result' as the first argument, and,
                    ##  the restriction of 'file_loop' ensures proper
                    ##  intersections.
                    intersected_dn <- dimnames_intersect(
                        dimnames1 = dimnames_result[[.node]],
                        dimnames2 = .dn_la[[.node]])
                    ##  Use 'update_array' and 'restrict_array' to
                    ##  update 'list_array_result'.

                    ## capture_env() 
#####  2017-01-12: Something goes wrong when the code is used with no
#####  parallel backend... WTF.
                    update_array(.arr = list_array_result[[.node]],
                                 .sub_arr = restrict_array(
                                     .arr = ingoing_list_array[[.node]],
                                     .restrict = intersected_dn,
                                     .permute = TRUE))
                }
            }  #  'file_loop' is now completed
        }  #  'source_loop' is now completed.
        kill(dimnames_result, file_loop, ingoing_list_array,
             intersected_dn)
###-------------------------------------------------------------------
        ##  Use 'bquote' and '.()' to create a save_quote for
        ##  'list_array_result' (with updated attributes for the
        ##  array-nodes), to store it with a unique name.
        save_file <- components[[content]]$combined$file_names[part]
        save_name <- gsub(pattern = ".Rda",
                          replacement = "",
                          x = save_file)
        ##  Update 'save_file' to give the full path.
        save_file <- file.path(save_dir,
                               save_file)
        ##---
        save_quote <- bquote({
            .(save_name) <- list_array_dims(list_array_result)
            save(.(save_name),
                 file = save_file)
            ##  Inform the user that the file was created.
            file_created(save_file)
            kill(.(save_name))
        })
        ##  Evaluate 'save_quote' to save the object to disk.
        eval(save_quote)
        ##  For the present combination of 'content' and 'part',
        ##  the only thing that now remains is to add
        ##  'object_size_MB' to 'components'.  When done in
        ##  'foreach' this number must be returned from the
        ##  worker-node before any updates can be made.
        as.numeric(object.size(list_array_result) / 1024^2)
    } #  This concludes 'foreach' used on 'loop_arg_list'
###-------------------------------------------------------------------
    ##  Update the 'object_size_MB'-part of content.
    for (i in seq_along(loop_arg_list)) {
        content <- loop_arg_list[[i]]$content
        part <- loop_arg_list[[i]]$part
        ##---
        components[[content]]$combined$object_size_MB[part] <- 
            saved_sizes_MB[i]
    }
    kill(i, content, loop_arg_list, part, saved_sizes_MB)
    ##  Prune away the source-part from 'components'
    for (.source in source_loop) 
        for (type in names(components))
            components[[c(type, .source)]] <- NULL
###-------------------------------------------------------------------
    ##  Return the revised version of 'components' to the workflow.
    components
}



## LG_merge_files <- function(components,
##                            source_dir,
##                            save_dir) {
## ###-------------------------------------------------------------------
## #############---------------------------------------------------------
## ###-------------------------------------------------------------------
## ###  This function assumes that the function that called it checked
## ###  the sanity of the arguments.  The extraction and pasting together
## ###  of the desired information will be done by using a
## ###  parallel-divide on the top-level, since the updating of the final
## ###  array by 'update_array' must be done sequentially.
## #############---------------------------------------------------------
## ###-------------------------------------------------------------------
##     ##  Create an argument-list to use with 'foreach'.
##     loop_arg_list <- list()
##     for (content in names(components)) {
##         pieces <- length(components[[content]]$combined$file_names)
##         for (i in 1:pieces) 
##             loop_arg_list <- c(
##                 loop_arg_list,
##                 list(list(content = content, part = i)))
##     }
##     kill(content, i, pieces)
##     ##  Extract information about the part of the second level in
##     ##  'components', that gives information about the sources.
##     ##  to know which source to loop from.
##     source_loop <- setdiff(
##         x = names(components[[1]]),
##         y = "combined")
## ###-------------------------------------------------------------------
##     ##  Let foreach take care of the loading and saving of files, and
##     ##  only return the value needed for an update of the
##     ##  'object_size_MB'-part of 'components'


##     ## capture_env() 
##     ## i <- 1
    
##     ## saved_sizes_MB <- foreach(i = seq_along(loop_arg_list)) %dopar% {

##     ##  Drop parallel when testing
##     saved_sizes_MB <- foreach(i = seq_along(loop_arg_list)) %do% {

        
##     ## ## ## saved_sizes_MB <- foreach(i = seq_along(loop_arg_list)) %do% {
##         content <- loop_arg_list[[i]]$content
##         part <- loop_arg_list[[i]]$part
## ###-------------------------------------------------------------------
##         ##  We need the array to fill out with data, and the files
##         ##  from which those data should be extracted.
## ###-------------------------------------------------------------------
##         ##  Find the dimnames of the result
##         dimnames_result <-
##             components[[content]]$combined$dimnames[[part]]

##         capture_env()
 

        
##         ##  Create a template for the desired array.
##         array_result <- structure(
##             .Data = array(
##                 data = NA_real_,
##                 dim = vapply(
##                     X = dimnames_result,
##                     FUN = length,
##                     FUN.VALUE = numeric(1)),
##                 dimnames = dimnames_result),
##             class = LG_default$class$array)
## #####  TASK: This pre-allocation might slow stuff down, and it might
## #####  be faster to use a solution where the saved files have been
## #####  forced into a format that can be joined by the help of abind
## #####  instead.  That would require a redesign of other parts of the
## #####  code, and that's not something I can waste time on now.
##         ##  Double loop: 'source_loop' + 'file_loop'.
##         for (.source in source_loop) {
##             ##  Find the file-names that contains the desired objects.
##             file_loop <- 
##                 components[[c(content, .source, "file_names")]]
## ###-------------------------------------------------------------------
## #############---------------------------------------------------------
## ###  To avoid loading files with no contribution to the desired
## ###  result, use 'dimnames_intersect' to figure out if any files can
## ###  be ignored.
## #############---------------------------------------------------------
## ###-------------------------------------------------------------------
##             ##  The list of intersection.
##             tmp_intersected_dimnames <- lapply(
##                 X = components[[c(content, .source, "dimnames")]],
##                 FUN = dimnames_intersect,
##                 dimnames2 = dimnames_result)
##             ##  A Boolean value telling us which to include.
##             tmp_boolean <- vapply(
##                 X = tmp_intersected_dimnames,
##                 FUN = length,
##                 FUN.VALUE = numeric(1)) != 0
##             ##  Revised version of 'file_loop'.
##             file_loop <- file_loop[tmp_boolean]
##             kill(tmp_intersected_dimnames, tmp_boolean)
## ###-------------------------------------------------------------------
##             ##  Load the files and extract the relevant data.
##             for (.file in file_loop) {
##                 ##  Create an environment to load the object into. 
##                 ingoing_array <- new.env()
##                 ##  Load the object into its environment.
##                 load(file = file.path(source_dir[[.source]],
##                                       .file),
##                      envir = ingoing_array)
##                 ##  Read the dimension-names from `ingoing_array`.
##                 .dn <- dimnames(
##                     ingoing_array[[ls(envir = ingoing_array,
##                                       all.names = TRUE)]])
##                 ##  Find the part of the object that we want to
##                 ##  extract.  Remember: Use 'dimnames_result' as the
##                 ##  first argument, and, the restriction of
##                 ##  'file_loop' ensures proper intersections.
##                 intersected_dn <- dimnames_intersect(
##                     dimnames1 = dimnames_result,
##                     dimnames2 = .dn)
##                 ## ## identical(dimnames_result,
##                 ## ##           dimnames(ingoing_array[[ls(ingoing_array, all.names = TRUE)]]))
##                 ##  Use 'update_array' and 'restrict_array' to update
##                 ##  'array_result'.
##                 update_array(.arr = array_result,
##                              .sub_arr = restrict_array(
##                                  .arr = ingoing_array[[ls(ingoing_array)]],
##                                  .restrict = intersected_dn,
##                                  .permute = TRUE))
##             }  #  'file_loop' is now completed
##         }  #  'source_loop' is now completed.
##         kill(dimnames_result, file_loop, ingoing_array, intersected_dn)
## ###-------------------------------------------------------------------
##         ##  Use 'bquote' and '.()' to create a save_quote for
##         ##  'array_result', to store it with a unique name.
##         save_file <- components[[content]]$combined$file_names[part]
##         save_name <- gsub(pattern = ".Rda",
##                           replacement = "",
##                           x = save_file)
##         ##  Update 'save_file' to give the full path.
##         save_file <- file.path(save_dir,
##                                save_file)
##         ##---
##         save_quote <- bquote({
##             .(save_name) <- array_result
##             save(.(save_name),
##                  file = save_file)
##             ##  Inform the user that the file was created.
##             file_created(save_file)
##             kill(.(save_name))
##         })
##         ##  Evaluate 'save_quote' to save the object to disk.
##         eval(save_quote)
##         ##  For the present combination of 'content' and 'part',
##         ##  the only thing that now remains is to add
##         ##  'object_size_MB' to 'components'.  When done in
##         ##  'foreach' this number must be returned from the
##         ##  worker-node before any updates can be made.
##         as.numeric(object.size(array_result) / 1024^2)
##     } #  This concludes 'foreach' used on 'loop_arg_list'
## ###-------------------------------------------------------------------
##     ##  Update the 'object_size_MB'-part of content.
##     for (i in seq_along(loop_arg_list)) {
##         content <- loop_arg_list[[i]]$content
##         part <- loop_arg_list[[i]]$part
##         ##---
##         components[[content]]$combined$object_size_MB[part] <- 
##             saved_sizes_MB[i]
##     }
##     kill(i, content, loop_arg_list, part, saved_sizes_MB)
##     ##  Prune away the source-part from 'components'
##     for (.source in source_loop) 
##         for (type in names(components))
##             components[[c(type, .source)]] <- NULL
## ###-------------------------------------------------------------------
##     ##  Return the revised version of 'components' to the workflow.
##     components
## }
