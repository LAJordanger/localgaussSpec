###############################################################################
#'
#' Collect data from block of independent series into one object.
#'
#' The computation of the local Gaussian spectra for the blocks of
#' independent series creates a file-hierarchy that differs a bit from
#' the setup of the resulting code.  In order to allow the present
#' incarnation of \code{LG_shiny} to get access to the results, some
#' tweaking of the related objects must be done.
#'
#' @details Based on the \code{data_dir} argument, there will be
#'     performed an adjustment of the corresponding file-hierarchy --
#'     in order for \code{LG_shiny} to find the data (without a major
#'     rewriting of the code in that function.)
#' 
#' @template main_dir_arg
#'
#' @param data_dir The directory containing the confidence intervals
#'     part from the function \code{LG_Wrapper_Blocks}.
#'
#' @param threshold An \code{integer}, default value 500 (measured in
#'     MB), that gives the upper in-memory size of the objects stored
#'     in the resulting files.
#' 
#' @return  A path to be used by \code{LG_shiny}.
#'
#' @export

LG_collect_blocks <- function(
    main_dir,
    data_dir,
    threshold = 500) {
###-------------------------------------------------------------------
    ##  Create a spy_report
    spy_report <- spy()
    ##  Sanity-check the arguments, and add new objects to the present
    ##  environment: 'boot_type', 'info', 'info_path', 'dir_bookmark',
    ##  'collected_bookmark', 'done_before' and 'save_dir'.
    LG_bookkeeping(spy_report)
    ## Quit if the computation already has been performed.
    if (done_before) {
        print("The collection of these data has already been done.")
        ##  Return the path to be used by 'LG_shiny'
        return(data_dir)
    }
    kill(done_before, spy_report)
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
    ##  Load the desired objects into temporary environments, then
    ##  insert them into the present environment under default names
    ##  `spectra_original` and `spectra_CI`.
    LG_load(.file = .file_path,
            .name = "spectra_original")
    LG_load(.file = .file_CI_path,
            .name = "spectra_CI")
    kill(.file_path, .file_CI_path)
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
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Identify the two parts to compose, i.e. the data that belongs to
###  the analysis based on mean/median of the local Gaussian
###  approximations, and those containing information about the
###  confidence intervals.  NB: This is at the time of writing an
###  awful ad-hoc solution, as the names of the directories doesn't
###  match the content.
#############---------------------------------------------------------
###---------------------------------------------------------
    ##  Extract the bookmark that gives the path to the data based on
    ##  the mean/median of the local Gaussian correlations.
    mean_bookmark <- info[[dir_bookmark]]$spectra_bookmark
###-------------------------------------------------------------------
    ##  Create a list to store the data needed for our computation
    components <- structure(
        .Data = vector(mode = "list",
                       length = length(content_types)),
        .Names = content_types)
    ##---
    for (content in content_types) {
        ##  Extract information from the info-object, and initiate
        ##  stuff to be filed out in the next step.
        CI_subset <- info[[dir_bookmark]]$data_files_df$content == content
        mean_subset <- info[[mean_bookmark]]$data_files_df$content == content
        components[[content]] <- list(
            CI = list(
                dimnames = info[[dir_bookmark]]$dimnames_spectra[[content]],
                file_names = info[[dir_bookmark]]$data_files_df$data_files[CI_subset],
                object_size_MB = info[[dir_bookmark]]$data_files_df$object_size_MB[CI_subset]),
            ##---
            mean = list(
                dimnames = info[[mean_bookmark]]$dimnames_spectra[[content]],
                file_names = info[[mean_bookmark]]$data_files_df$data_files[mean_subset],
                object_size_MB = info[[mean_bookmark]]$data_files_df$object_size_MB[mean_subset]),
            ##---
            combined = list(
                dimnames = list(),  ##  To be added later on.
                file_names = paste(
                    paste(LG_default$collected_data,
                          content,
                          tail(mean_bookmark, 1),
                          tail(dir_bookmark, 1),
                          sep = ".."),
                    ".Rda",
                    sep = ""),
                object_size_MB = NA_real_,
                array_nodes = LG_default$result$array_nodes))
    }
    kill(CI_subset, content, content_types, mean_subset)
###-------------------------------------------------------------------
    ##  Create 'source_dir' to simplify the code for reading files.
    base_dir <- c(main_dir, data_dir["ts.dir"])
    source_dir <- list(
        mean = paste(c(base_dir, mean_bookmark),
                     collapse = .Platform$file.sep),
        CI = paste(c(base_dir, dir_bookmark),
                     collapse = .Platform$file.sep))
    kill(base_dir, mean_bookmark)
###-------------------------------------------------------------------
#############---------------------------------------------------------
###  Create a template for the combined dimnames by updating one from
###  the 'CI'-case with values from 'CI_arg_list' and adding an
###  extra layer to include the mean values.  Use the stored
###  object-sizes from the 'CI'-case to get an estimate of the total
###  size, compare with 'threshold' and divide if necessary.
#############---------------------------------------------------------
###-------------------------------------------------------------------
    for (content in names(components)) {
        ##  Find a template for the complete set of merged dimnames,
        ##  by investigating those present for the 'CI'-part and the
        ##  'mean'-part, using the 'CI'-part as template.
        dimnames_template <- components[[content]]$CI$dimnames[[1]]
        ##  Find the dimnames for the 'mean'-part.
        dimnames_mean <- components[[content]]$mean$dimnames[[1]]

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
                        X = seq_along(components[[content]]$CI$dimnames),
                        FUN = function(x) 
                            components[[content]]$CI$dimnames[[x]][[c(.node, .name)]]))
            ##  Merge the main-dimension of the 'mean'-part into the
            ##  main-dimension of the 'CI'-part.
            .main <- LG_default$dimnames$main
            dimnames_template[[c(.node, .main)]] <- 
                c(dimnames_mean[[c(.node, .main)]],
                  dimnames_template[[c(.node, .main)]])
        }
        kill(.name, .node, .helper)
###-------------------------------------------------------------------
        ##  Compute an estimate of the corresponding size, based on
        ##  the adjustment that comes from adding a layer for the mean
        ##  data.  NB: This setup is based on the assumption that
        ##  there always will be a computation of the autospectra on
        ##  the diagonal.
        .auto_node <- c(LG_default$result$auto_node, .main)
        if (identical(x = dimnames_template[[.auto_node]], y = NULL))
            error(c("Breakdown of assumptions!",
                    "No auto-spectra part detected, program terminated."))
        outgoing_size <- prod(
            divide_by(
                e1 = length(dimnames_template[[.auto_node]]),
                e2 = length(dimnames_template[[.auto_node]]) -
                    length(dimnames_mean[[.auto_node]])),
            sum(components[[content]]$CI$object_size_MB))
        kill(.auto_node, .main, dimnames_mean)
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

    ## ## LG_merge_files_call <- create_call(
    ## ##     .cc_fun = LG_merge_files,
    ## ##     components = components,
    ## ##     source_dir = source_dir,
    ## ##     save_dir = save_dir)
    ## ## capture_env()
    
    components <- LG_merge_files(
        components = components,
        source_dir = source_dir,
        save_dir = save_dir)
    kill(source_dir, save_dir)
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
