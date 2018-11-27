################################################################################
#'
#' Collect data from original and bootstrap into one object
#' 
#' The computation of the local Gaussian spectra can create an
#' abundance of files in a plethora of directories. This function
#' takes care of the pesky task of collecting relevant pieces into one
#' object.
#'
#' @details This function will, after a sanity-check of the arguments
#'     \code{main_dir} and \code{data_dir}, figure out the
#'     dimension-names that should be included in the collected data,
#'     and an estimate of the size of it.  If the size exceeds the
#'     value given to \code{threshold}, then a splitting will be
#'     performed.  File-names will be created based on the combination
#'     of values from the directory names used on the spectra-part and
#'     the boots_spectra-part.  For each file we will start out by
#'     creating an array with the desired dimension-properties, and
#'     then the saved data will be read and included into these.
#'     After saving (which might require a minor update of
#'     \code{LG_save}), the info-object should also be updated and
#'     saved.
#'
#'
#' @template main_dir_arg
#' 
#' @param data_dir The directory containing the result from the
#'     bootstrap.
#'
#' @param threshold An \code{integer}, default value 500 (measured in
#'     MB), that gives the upper in-memory size of the objects stored
#'     in the resulting files.
#' 
#' @return  A data-table with the stuff of interest.
#'
#' @export

LG_collect_orig_and_boot <- function(
    main_dir,
    data_dir,
    threshold = 500) {
###-------------------------------------------------------------------
    ##  Create a spy_report
    spy_report <- spy()
    ##  Sanity-check the arguments, and add new objects to the present
    ##  environment: 'boot_type', 'info', 'info_path', 'dir_bookmark',
    ##  'done_before' and 'save_dir'.
    LG_bookkeeping(spy_report)
    ## Quit if the computation already has been performed.
    if (done_before) {
        print("The collection of these data has already been done.")
        ##  Return the path to be used by 'LG_shiny'
        return(data_dir)
    }
    kill(done_before, spy_report)
    ##  Create a collected bookmark for the adding of results.
    collected_bookmark <- c(dir_bookmark,
                            LG_default$collected_data)
###-------------------------------------------------------------------
    ##  Collect the ordinary (global) spectra first, start by creating
    ##  specific bookmarks for the extraction of the information.
    .file_ref <- "spectral"
    .file_CI_ref <- "spectral_CI"
    .collected_file <- LG_default$global["spectral_collected"]
    ##
    .spectra_bookmark <- c(info[[c(dir_bookmark, "spectra_bookmark")]],
                            .file_ref)
    .spectra_CI_bookmark <- c(dir_bookmark, .file_CI_ref)
    ##
    .file_path <- paste(c(main_dir,
                           info[[.spectra_bookmark]]),
                         collapse = .Platform$file.sep)
    .file_CI_path <- paste(c(main_dir,
                             info[[.spectra_CI_bookmark]]),
                           collapse = .Platform$file.sep)
    kill(.file_ref, .file_CI_ref, .spectra_bookmark,
         .spectra_CI_bookmark)
    ##  Load the desired objects into the present environment under
    ##  the default names `spectra_original` and `spectra_CI`.
    LG_load(.file = .file_path,
            .name = "spectra_original")
    LG_load(.file = .file_CI_path,
            .name = "spectra_CI")
    kill(.file_path, .file_CI_path)
###-------------------------------------------------------------------
    ##  Reminder: The content dimension will have no common elements,
    ##  whereas the dimensions of `spectra_CI` should be included in
    ##  (or equal to) those of `spectra_original`. It might thus be
    ##  necessary to restrict `spectra_original` before joining these
    ##  arrays to one collected version.
    .dn_original <- attributes(spectra_original)$list_array_dimnames
    .dn_CI <- attributes(spectra_CI)$list_array_dimnames
    ##  Loop over the array-nodes.
    for (.node in LG_default$result_orig$array_nodes) {
        ##  Skip any empty parts.
        if (identical(x = .dn_original[[.node]],
                      y = NULL))
            next
        ##  Identify the "content-"positions.
        .content_pos_original <-
            which(names(.dn_original[[.node]]) == "content")
        .content_pos_CI <-
            which(names(.dn_CI[[.node]]) == "content")
        ##  Identify the common parts.
        .common_parts <- dimnames_intersect(
            dimnames1 = .dn_original[[.node]][-.content_pos_original],
            dimnames2 = .dn_CI[[.node]][-.content_pos_CI])
        ##  Restrict `spectra_original` when necessary.
        if (! identical(x = .common_parts,
                        y = .dn_original[[.node]][-.content_pos_original])) {
            ##  Update restriction list.
            for (.name in names(.common_parts))
                .dn_original[[.node]][[.name]] <-
                    .common_parts[[.name]]
            ##  Perform restriction.
            spectra_original[[.node]] <- restrict_array(
                .arr = spectra_original[[.node]],
                .restrict = .dn_original[[.node]])
        }
        kill(.content_pos_original, .content_pos_CI, .common_parts)
    }
    kill (.dn_original, .dn_CI)
###-------------------------------------------------------------------
    ##  Collect the components into a new list-array-object.
    spectra_collected <- list_array_join(
        spectra_CI,
        spectra_original,
        array_nodes = LG_default$result_orig$array_nodes,
        .class = LG_default$class$array)
    ## Save the result to file.
    LG_save(data = spectra_collected,
            save_file.Rda = .collected_file,
            save_dir = save_dir)
    ##  Update the `info`-object
    info[[collected_bookmark]] <- list(
        spectral_collected = c(tail(x = data_dir, n = -1),
                               .collected_file))
    kill(spectra_original, spectra_CI, spectra_collected, .collected_file)
    ##  Identify the 'content' we are working upon.
    content_types <-
        unique(info[[dir_bookmark]]$data_files_df$content)
#####  TASK: In general I would like ingoing data to be stored in a
#####  list whose naming always should be something like "data_info",
#####  in order to capture the result by a simple command like
### content_types <- names(info[[c(some_bookmark, "data_info")]])
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  TASK: The idea of this code is to simulate the stuff that I hope
###  later on (with a modification of earlier code) will turn out to
###  be a simple "pick up the correct piece".
#############---------------------------------------------------------
###-------------------------------------------------------------------
    ##  A bookmark needed in order to find the original data.
    orig_bookmark <-
        if (boot_type == "boot.spectra") {
            info[[dir_bookmark]]$spectra_bookmark
        } else
            head(dir_bookmark, -1)
###-------------------------------------------------------------------
    ##  Create a list to store the data needed for our computation
    components <- vector(mode = "list",
                         length = length(content_types))
    names(components) <- content_types
    ##---
    for (content in content_types) {
        ##  Extract information from the info-object, and initiate
        ##  stuff to be filed out in the next step.
        boot_subset <- info[[dir_bookmark]]$data_files_df$content == content
        orig_subset <- info[[orig_bookmark]]$data_files_df$content == content
        components[[content]] <- list(
            boot = list(
                dimnames =
                    if (boot_type == "boot.spectra") {
                        info[[dir_bookmark]]$dimnames_spectra[[content]]
                    } else {
                        info[[c(dir_bookmark,
                                gsub(pattern = "data.",
                                     replacement = "dimnames_",
                                     x = content))]]
                    },
                file_names =     info[[dir_bookmark]]$data_files_df$data_files[boot_subset],
                object_size_MB = info[[dir_bookmark]]$data_files_df$object_size_MB[boot_subset]),
            ##---
            orig = list(
                dimnames =
                    if (boot_type == "boot.spectra") {
                        info[[orig_bookmark]]$dimnames_spectra[[content]]
                    } else {
                        info[[c(orig_bookmark,
                                gsub(pattern = "data.",
                                     replacement = "dimnames_",
                                     x = content))]]
                    },
                file_names =     info[[orig_bookmark]]$data_files_df$data_files[orig_subset],
                object_size_MB = info[[orig_bookmark]]$data_files_df$object_size_MB[orig_subset]),
            ##---
            combined = list(
                dimnames = list(),  ##  To be added later on.
                file_names = paste(
                    paste(LG_default$collected_data,
                          content,
                          tail(orig_bookmark, 1),
                          tail(dir_bookmark, 1),
                          sep = ".."),
                    ".Rda",
                    sep = ""),
                object_size_MB = NA_real_,
                array_nodes = LG_default$result$array_nodes))
    }
    kill(boot_subset, content, content_types, orig_subset)
###-------------------------------------------------------------------
    ##  Create 'source_dir' to simplify the code for reading files.
    base_dir <- c(main_dir, data_dir["ts.dir"])
    source_dir <- list(
        orig = paste(c(base_dir, orig_bookmark),
                     collapse = .Platform$file.sep),
        boot = paste(c(base_dir, dir_bookmark),
                     collapse = .Platform$file.sep))
    kill(base_dir, main_dir, orig_bookmark)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Create a template for the combined dimnames by updating one from
###  the 'boot'-case with values from 'boot_arg_list' and adding an
###  extra layer to include the original values.  Use the stored
###  object-sizes from the 'boot'-case to get an estimate of the total
###  size, compare with 'threshold' and divide if necessary.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    for (content in names(components)) {
        ##  Tweak the bootstrap-result into a template for the
        ##  collected objects.
        dimnames_template <- components[[content]]$boot$dimnames[[1]]
        ##  In order to include the original data, extend a suitable
        ##  dimension based on 'boot_type'.
        add_original <- ifelse(
            test = boot_type == "boot.spectra",
            yes = LG_default$dimnames$main,
            no = LG_default$boot.prefix)
        ##  Perform the loop to update 'dimnames_template'.
        for (.node in LG_default$result$array_nodes) {
            if (identical(x = dimnames_template[[.node]],
                          y = NULL))
                next
            ##  A minor helper function to take the sorted unique
            ##  collection of the dimensional attributes.
            .helper <- function(...) 
                sort(unique(c(...)))
            ##  Use do.call' to update 'dimnames_template' so all the
            ##  dimension-information is stored in one place.
            for (.name in names(dimnames_template[[.node]]))
                dimnames_template[[c(.node, .name)]] <- do.call(
                    what = ".helper",
                    args = lapply(
                        X = seq_along(components[[content]]$boot$dimnames),
                        FUN = function(x) 
                            components[[content]]$boot$dimnames[[x]][[c(.node, .name)]]))
            ##  Add detail for original data.
            dimnames_template[[c(.node, add_original)]] <- 
                c(LG_default$sample.prefix,
                  dimnames_template[[c(.node, add_original)]])
        }
        kill(.name, .node, .helper)
###-------------------------------------------------------------------
        ##  Compute an estimate of the size of the new object, based
        ##  on the size of the ingoing data. NB: This setup is based
        ##  on the assumption that there always will be a computation
        ##  of the autospectra on the diagonal.
        .auto_node <- c(LG_default$result$auto_node, add_original)
        if (identical(x = dimnames_template[[.auto_node]], y = NULL))
            error(c("Breakdown of assumptions!",
                    "No auto-spectra part detected, program terminated."))
        outgoing_size <- prod(
            divide_by(
                e1 = length(dimnames_template[[.auto_node]]),
                e2 = length(dimnames_template[[.auto_node]]) - 1),
            sum(components[[content]]$boot$object_size_MB))
        kill(.auto_node, add_original)
        ##  Check if it is necessary to split the new object in order
        ##  to keep it below the selected threshold.
        final_pieces <- ceiling(
            divide_by(
                e1 = outgoing_size,
                e2 = threshold))
        kill(outgoing_size) 
###-------------------------------------------------------------------
        ##  Update the non-NULL content of 'dimnames_template' by the
        ##  help of the 'split_vector', then switch the level of new
        ##  and old 'nodes' to better suit the need of the
        ##  'LG_merge'-function.  Note that the number of pieces can
        ##  increase in this step, so the code must deal with that
        ##  possibility.
        updated_final_pieces <- final_pieces
        for (.node in LG_default$result$array_nodes) {
            if (identical(x = dimnames_template[[.node]],
                          y = NULL))
                next
            ##  Replace the node with a splitted version.
            dimnames_template[[.node]] <- split_vector(
                vec = dimnames_template[[.node]]$levels,
                pieces = final_pieces,
                allow_extra_pieces = TRUE,
                compute_name = "levels",
                add_to_compute =
                    dimnames_template[[.node]][names(dimnames_template[[.node]]) != "levels"],
                position = which(
                    names(dimnames_template[[.node]]) ==  "levels"))$compute
            ##  This spliting of dimension-names can increase the
            ##  number of pieces, so an update might be needed.
            updated_final_pieces <- max(
                updated_final_pieces,
                length(dimnames_template[[.node]]))
        }
        kill(.node, final_pieces)
###-------------------------------------------------------------------
        ##  Add a "flipped version" of the above dimnames into the
        ##  'combined' part of 'components'.
        components[[content]]$combined$dimnames <- lapply(
                                 X = seq_len(updated_final_pieces),
                                 FUN = function(x)
                                     LG_default$result$skeleton_dimnames)
        ##  Add content.
        for (i in seq_len(updated_final_pieces)) 
            for (.node in LG_default$result$array_nodes) {
                if (identical(x = dimnames_template[[.node]],
                              y = NULL))
                    next
                components[[content]]$combined$dimnames[[i]][[.node]] <-
                                        dimnames_template[[.node]][[i]]
            }
        kill(i, .node, dimnames_template)
        ##  Update the names of the save files (when necessary).
        if (updated_final_pieces > 1)
            components[[content]]$combined$file_names <-
                                    paste(
                                        gsub(pattern = ".Rda",
                                             replacement = "",
                                             x = components[[content]]$combined$file_names),
                                        paste("part_",
                                              1:updated_final_pieces,
                                              "_of_",
                                              updated_final_pieces,
                                              ".Rda",
                                              sep = ""),
                                        sep = "..")
        kill(updated_final_pieces)
    }  ##  End of loop over content.
    kill(content, boot_type, threshold)
###-------------------------------------------------------------------
    ##  Let 'LG_merge_files' read and save data, and replace
    ##  'components' with the revised version from that function.
    ## LG_merge_files_call <- create_call(
    ##     .cc_fun = LG_merge_files,
    ##     components = components,
    ##     source_dir = source_dir,
    ##     save_dir = save_dir)
    ## capture_env()
    
    components <- LG_merge_files(
        components = components,
        source_dir = source_dir,
        save_dir = save_dir)
    kill(source_dir)
###-------------------------------------------------------------------
    ##  Update 'info' with the information from 'components'
    info[[collected_bookmark]] <-
        c(info[[collected_bookmark]],
          components)
    ##  Save 'info'
    save(info, file = info_path)
    ##  Return the path to be used by 'LG_shiny'
    data_dir
}
