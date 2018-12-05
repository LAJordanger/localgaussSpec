#' A scribe for the "old-branch"-parameters from the
#' \code{LG_shiny}-interface.
#'
#' @param .env The environment where the \code{TS_logging}-object
#'     lives.
#'
#' @param .env2 The environment containing the two lists \code{input}
#'     and \code{output}.
#'
#' @return This helper-function will check if any changes in the top
#'     level of the interface for the \code{LG_shiny}-function implies
#'     that an update of stored values should be undertaken.  In
#'     addition it will perform the initial adjustments of the
#'     \code{TS_info} part of the \code{input_triggers}, in order for
#'     the possibility of auto-select to work as intended in the main
#'     function.
#'
#' @keywords internal

LG_shiny_interface_1_TS_info_scribe <- function(.env, .env2) {
###-------------------------------------------------------------------
    ##  The first time this function is called, it might be some
    ##  'pre-selected values that still ought to be added to the
    ##  selected values.  This should only happen once.
    if (.env$.first_time)
        on.exit(.env$.first_time <- FALSE)
    ##  Create a helper-function to perform a pesky repeated test, to
    ##  see if the value of interest is of the problematic type.
    .not_na_select <- function(.pos) {
        .x <- .bm[.pos]
        if (is.na(.x)) {
            FALSE
        } else
            ! stringr::str_detect(string = .x, pattern = "[Ss]elect")
    }
###-------------------------------------------------------------------
    ##  Check the information in the 'TS_info'-part of
    ##  'input_triggers', and figure out if a valid
    ##  environment-bookmark can be constructed.
    .bm <- unlist(.env$TS_logging$update$input_triggers$TS_info)
    ##  Reminder: The test for validity is that neither 'NA'-values
    ##  nor "Select..." occurs in any of the three first positions
    ##  ("TS_key", "TS", and "Approx").  The "Boot_Approx" is a
    ##  subnode of "Approx", and can thus be excluded.  
    .valid_bm <- all(vapply(X = seq_along(head(.bm, -1)),
                            FUN = .not_na_select,
                            FUN.VALUE = logical(1)))
    if (.valid_bm) {
        ##  Write the 'input'-values to the logging-environment
        LG_shiny_interface_0_RW_log(.env, .env2, .RW = "W")
        ##  Reset the values logged for the actionButtons.
        .env$TS_logging$update$input_triggers$TCS_type[]  <- 0L
        .env$TS_logging$update$input_triggers$second_graphical[]  <- 0L
        LG_shiny_set_values(.env, .env2,
                            .part =c("TCS_type", "second_graphical"))
    }
    kill(.valid_bm)
###-------------------------------------------------------------------    
    ##  Identify the position where the 'input'-value differs from the
    ##  value registered in the 'input_trigger'.
    .new_values <- unlist(.env$input[names(.bm)])
    ##  Reminder: Normally, this function will not be called unless
    ##  there is a difference between the values in '.bm' and
    ##  '.new_values', but in the initiation phase they will by
    ##  construction be identical, and that must be taken into account
    ##  when we look for the position of the relevant starting point.
    if (identical(.bm, .new_values)) {
        .first_time_not_NA <- !is.na(.bm)
        ##  Terminate if nothing has been recorded.
        if (!any(.first_time_not_NA)) {
            return(NULL)
        } else {
            ## Identify the position of the last non-NA-value, and use
            ## that as a starting point.
            .pos <- max(which(.first_time_not_NA))
        }
    } else {
        ##  Find the position of the difference.
        for (.pos in seq_along(.bm)) {
            if (! identical(x = .new_values[.pos],
                            y = .bm[.pos]))
                break
        }
    }
###-------------------------------------------------------------------                 
    ## Update the 'TS_info'-part of '.env$TS_logging' (using the '.bm'
    ##  as intermediate to get more compact code).  Start by setting
    ##  all the parts of '.bm' that are going to be updated to 'NA'.
    ##  In case there are insufficient data available to perform a
    ##  full update, this will avoid errors concerning invalid paths
    ##  later on in the code.
    .bm[.pos:length(.bm)] <- NA
    .bm[.pos] <- .new_values[.pos]
    ##  Reset the stored slider-names for the parts after position
    ##  number '.pos' to 'NA'.  (Not meaningful to do when 'pos' is
    ##  equal to '4'.)
    if (.pos < 4) {
        .env$TS_logging$update$select_names[(.pos+1):4]  <- NA_character_
        ## ## ##  Hopefully the above part will soon be obsolete.
        for (.p in (.pos+1):4) 
            .env$TS_logging$update$select_Input[[.p]]$names <- NA_character_
        kill(.p)
    }
    ##  Update the relevant 'last'-value in 'TS_logging'.
     if (.pos == 1) {
        .env$TS_logging$last <- .new_values[.pos]
    } else
        .env$TS_logging[[.bm[1:(.pos-1)]]]$last <- .new_values[.pos]
    kill(.new_values)
    ##  If the replaced position refers to a "Select..."-case, then it
    ##  only remains to update the 'input_triggers' and terminate.
    if (!.not_na_select(.pos)) {
        .env$TS_logging$update$input_triggers$TS_info[] <- .bm
        return(NULL)
    }
###-------------------------------------------------------------------    
    ##  Still running?  If so we need to see how much of the interface
    ##  that can be extracted from the available data.  First of all:
    ##  If '.pos' is '1', then use the 'last'-node to fill in the
    ##  second position of the 'input_triggers'.  This could be an
    ##  'NA'-value or a "Select..."-case, and adjustments due to that
    ##  is necessary in the auto-selection procedure.
    if (.pos == 1) {
        ##  Update 'input_triggers' and 'select_names' for 'TS'
        .env$TS_logging$update$select_names$TS <-
            .env$TS_logging[[.bm[1:.pos]]]$names
################################################################################
        ## ## ##  Hopefully the above part will soon be obsolete.
        .env$TS_logging$update$select_Input$TS$label <- 
            .env$TS_logging[[.bm[1:.pos]]]$label
        .env$TS_logging$update$select_Input$TS$selected <- 
            .env$TS_logging[[.bm[1:.pos]]]$last
        .env$TS_logging$update$select_Input$TS$choices <- 
            c(.env$TS_logging[[.bm[1:.pos]]]$header,
              .env$TS_logging[[.bm[1:.pos]]]$names)
################################################################################
        .bm[.pos+1] <- .env$TS_logging[[.bm[1:.pos]]]$last
        if (!.not_na_select(.pos+1)) {
            .env$TS_logging$update$input_triggers$TS_info[] <- .bm
            return(NULL)
        } else {
            ##  A minor update of the non-reactive 'input'-list is
            ##  needed for the next part to work properly.
            .env$input$TS <- .env$TS_logging$update$select_Input$TS$selected
            .pos <- .pos + 1
        }
    }
###-------------------------------------------------------------------    
    ##  The case where '.pos' is equal to '2' implies a new value for
    ##  "TS".  In this case we might need to load the relevant
    ##  info-object (if it is the first time that TS-node is being
    ##  visited), and extract the desired information about the
    ##  available "Approx"- and "Boot_Approx"-nodes.
    if (.pos == 2) {
        .TS_node <- .bm[c("TS_key", "TS")]
        ##  The first time this node is visited, it is necessary to
        ##  load the info-object to get hold of the information.
        if (!.env$TS_logging[[.TS_node]]$visited) {
            .env$TS_logging[[.TS_node]]$visited <- TRUE
            ##  Load the 'info'-file.
            load(file = file.path(
                     paste(c(.env$main_dir,
                             .bm["TS"]),
                           collapse = .Platform$file.sep),
                     LG_default$info_file_name),
                 envir = .env)
            ##  Detect the relevant names of the "Approx"-nodes.
            .env$TS_logging[[.TS_node]]$names <- 
                names(.env$info)[str_detect(
                         string = names(.env$info),
                         pattern = LG_default$folder_defaults["approx.dir"])]
            ##  Add information for 'label', 'header' and 'last'
            .env$TS_logging[[.TS_node]]$label <- ifelse(
                test = length(.env$TS_logging[[.TS_node]]$names) == 1,
                yes  = "Auto-selected the only available approximation",
                no   = sprintf("%s approximations available",
                             length(.env$TS_logging[[.TS_node]]$names)))
            if (length(.env$TS_logging[[.TS_node]]$names) > 1) {
                .env$TS_logging[[.TS_node]]$header <- "Select an approximation"
            } else {
                .env$TS_logging[[.TS_node]]$header <- character(0)
            }
            if (length(.env$TS_logging[[.TS_node]]$names) == 1) {
                .env$TS_logging[[.TS_node]]$last <- .env$TS_logging[[.TS_node]]$names
            } else {
                .env$TS_logging[[.TS_node]]$last <- .env$TS_logging[[.TS_node]]$header
            }
             ##  Override the previous values when no data are available.
             if (length(.env$TS_logging[[.TS_node]]$names) == 0) {
                 .env$TS_logging[[.TS_node]]$last <- "Nothing here to select"
                 .env$TS_logging[[.TS_node]]$label <- "No approximations detected"
                 .env$TS_logging[[.TS_node]]$header <- "Nothing here to select"
             }
            ##  For the approx-parts: Add a bookkeeping-environment to
            ##  keep track of all the relevant values.
            for (.part in .env$TS_logging[[.TS_node]]$names) {
                .env$TS_logging[[.TS_node]] <- c(
                    .env$TS_logging[[.TS_node]],
                    LG_shiny_interface_1_helper(
                        .env = .env,
                        .approx = .part))
            }
            kill(.part)
        }
        ##  Update the 'select_names' for "Approx".
        .env$TS_logging$update$select_names$Approx <-
            .env$TS_logging[[.TS_node]]$names
################################################################################
        ## ## ##  Hopefully the above part will soon be obsolete.
        .env$TS_logging$update$select_Input$Approx$label <- 
            .env$TS_logging[[.TS_node]]$label
        .env$TS_logging$update$select_Input$Approx$selected <- 
            .env$TS_logging[[.TS_node]]$last
        .env$TS_logging$update$select_Input$Approx$choices <- 
            c(.env$TS_logging[[.TS_node]]$header,
              .env$TS_logging[[.TS_node]]$names)
################################################################################
        ##  The first time this function is used, it might be some
        ##  'pre-selected values hanging around.
        if (all(.env$.first_time,
                !is.na(.env$pre_selected$Approx))) {
            .env$TS_logging$update$select_Input$Approx$selected <- .env$pre_selected$Approx
            .env$TS_logging[[.TS_node]]$last <- .env$pre_selected$Approx
            ## ## .env$TS_logging$update$input_triggers$TS_info["Approx"] <- .env$pre_selected$Approx
            .env$TS_logging$update$input_triggers$TS_info$Approx <- .env$pre_selected$Approx
        }
        ##  Add information to the next position of the
        ##  'input_triggers', and check if it is possible to continue,
        ##  or if it is necessary to wait for further input from the
        ##  interactive user-interface.
        .bm[.pos+1] <- .env$TS_logging[[.TS_node]]$last
        .env$TS_logging$update$input_triggers$TS_info[] <- .bm
        if (.not_na_select(.pos+1)) {
            .pos <- .pos + 1
        } else 
            return(NULL)
        kill(.TS_node)
    }
###-------------------------------------------------------------------    
    ##  The case where '.pos' is equal to '3' indicates that it might
    ##  or might not be bootstrap-values available.  For simulated
    ##  data there never will be any bootstrap-nodes, whereas it for
    ##  real data might not have been computed yet. Nevertheless, this
    ##  part of the code does not need to worry about that, since the
    ##  stored default 'NA'-values will trigger the desired behaviour
    ##  in the workers arriving later on.
    if (.pos == 3) {
        .TS_node <- .bm[1:.pos]
        ##  Update the 'select_names' for "Boot_Approx".
        .env$TS_logging$update$select_names$Boot_Approx <-
            .env$TS_logging[[.TS_node]]$names
################################################################################
        ## ## ##  Hopefully the above part will soon be obsolete.
        .env$TS_logging$update$select_Input$Boot_Approx$label <- 
            .env$TS_logging[[.TS_node]]$label
        ###  REMINDER: The value above is 'NULL'. Why? Did I leave a
        ###  test for that case somewhere.  I doubt so...
        .env$TS_logging$update$select_Input$Boot_Approx$selected <- 
            .env$TS_logging[[.TS_node]]$last
        .env$TS_logging$update$select_Input$Boot_Approx$choices <- 
            c(.env$TS_logging[[.TS_node]]$header,
              .env$TS_logging[[.TS_node]]$names)
################################################################################
        ##  Register if we do have an actual bootstrap-node
        .env$TS_logging$update$is_bootstrap <-
            .env$TS_logging[[.TS_node]]$is_bootstrap
        ##  Update the 'input_triggers' and return.
        .bm[.pos+1] <- .env$TS_logging[[.TS_node]]$last
        .env$TS_logging$update$input_triggers$TS_info[] <- .bm
        ##  Read information from 'TS_logging' into the 'input'-lists
        LG_shiny_interface_0_RW_log(.env, .env2, .RW="R")
        return(NULL)
    }
###-------------------------------------------------------------------    
    ##  The case where '.pos' is equal to '4' indicates that we have
    ##  an actual bootstrap-node.  The only thing that is required now
    ##  is to update the 'input_triggers' and return.
    if (.pos == 4) {
        ##  Update the 'input_triggers'
        .env$TS_logging$update$input_triggers$TS_info[] <- .bm
        ##  Find the correct environment and update the required parts
        ##  of the logging-structure
        ..env <- LG_shiny_interface_0_RW_log(.env, .env2)
        ..env$last <- .bm[.pos]
        .env$TS_logging$update$select_Input$Boot_Approx$selected <- .bm[.pos]
        ##  Read information from 'TS_logging' into the 'input'-lists
        LG_shiny_interface_0_RW_log(.env, .env2, .RW="R")
        return(NULL)
    }
}

##### REMINDER FROM THE INITIATION PHASE OF THIS FUNCTION
## In this case the scribe should ensure that the input-settings is
## stored before there is a change to a new branch.  The point of
## interest is that the 'TS_info' part of the 'input_triggers' in
## 'TS_logging' should work as a bookmark to the node that should be
## updated.  However, there are some situations where nothing should
## be done.  In particular, during the initiation phase where
## 'input'-contains 'NULL'-values.  The idea is to compare the
## relevant parts of input with the information stored in 'TS_info',
## and if there is a difference then the relevant old input-values are
## stored at the node specified by 'TS_info'.  There are however some
## cases where an attempt at using 'TS_node' as a bookmark would fail,
## since there can be "Select..." and "NA"-values stored there (occurs
## when a curious user for some stupid reason selects the option
## "Select..." from the interface).
###-------------------------------------------------------------------
## The initiation-phase, figure out if there are some 'NA'-values
## present in the 'input', and if so there should be no saving.  Only
## update the relevant part of 'TS_info' and return.  Reminder: This
## updating is based on the principle that a comparison is made from
## the start of 'TS_logging' in order to figure out the position where
## the difference is located.  All the parts before this position will
## be kept as they are, the part at the position is updated from
## 'input' and then everything after that position will be updated
## based on the earlier part and the information that can be found in
## 'TS_logging'.  This is done in order (if possible) to switch back
## to the last input-configuration that was used if the new branch was
## visited at some earlier time.  If the new branch is visited for the
## first time it will instead be an attempt at detecting if something
## can be auto-selected (relevant when e.g. only one approx is
## present).
