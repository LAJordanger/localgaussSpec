#' Helper-function to create the \code{curlicues}-list of \code{look_up}
#'
#' @description This internal helper function prepares the information
#'     that is needed in order to annotate assorted information
#'     (curlicues) to the final plots.  A plot-stamp that reveals the
#'     content of the plot will always be present together with
#'     information about the length of the sample and the number of
#'     replicates.  Additional information will be added depending on
#'     the type of plot, this can e.g. be details related to the
#'     truncation level, the point of investigation, the bandwidth,
#'     the status for the numerical convergence of the estimates, or
#'     the block length used for the bootstrapping algorithm.  See the
#'     scripts for examples related to how the annotated text can
#'     adjusted when the plots are created in a nonreactive setting.
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @return A \code{curlicues}-list will be returned to the calling
#'     function (i.e. \code{LG_lookup}), and this list contains e.g.
#'     details related to the size of lines, and the position, colour
#'     and size of the annotated text.
#'
#' @keywords internal

LG_lookup_curlicues <- function(look_up) {
    ##  Read in the most basic 'curlicues'-values.
    curlicues <- LG_default$curlicues
    ##  Define case-specific-curlicues (CSC).
    CSC <- list()
    CSC$title  <- list(label = look_up$details$.plot_label)
    ##  Compute the 'include'-nodes of the different labels.
    CSC$plot_stamp$include <- {look_up$TCS_type %in% c("C", "S")}
    CSC$NC_value$include <- {look_up$global_local == "local"}
    CSC$complex_Cartesian$include <- all(look_up$complex,
                                         look_up$complex_c_or_p_or_z == "c")
    CSC$complex_polar$include <- all(look_up$complex,
                                     look_up$complex_c_or_p_or_z == "p")
    CSC$n_R_L_values$include <- TRUE
    CSC$m_value$include  <- {look_up$TCS_type == "S"}
    CSC$v_value$include  <- look_up$is_local
    CSC$b_value$include <- look_up$is_local
    ##  Adjust details for the 'L2-distance'-investigations.
    if (any(look_up$L2_distance_normal,
            look_up$L2_distance_percentages)) { 
        if (look_up$L2_inspection_vbmL == "v") {
            CSC$v_value$include <- FALSE
            ## Adjust position of 'b'-label so it moves to the empty
            ## position from the 'v'-label.
            curlicues$b_value$annotate$vjust <-
                curlicues$v_value$annotate$vjust
        }
        if (look_up$L2_inspection_vbmL == "b") 
            CSC$b_value$include <- FALSE
        if (look_up$L2_inspection_vbmL == "m") 
            CSC$m_value$include <- FALSE
        ##  Reminder: The "L"-case is adjusted later on.
    }
    ##  Adjust details for heatmap-investigations of 'v', 'm' and 'b'.
    if (look_up$heatmap) {
        if (look_up$heatmap_b_or_v == "b") {
            CSC$b_value$include <- FALSE
        }
        if (look_up$heatmap_b_or_v == "m") {
            CSC$m_value$include <- FALSE
        }
        if (look_up$heatmap_b_or_v == "v") {
            CSC$v_value$include <- FALSE
            ## Adjust position of 'b'-label so it moves to the empty
            ## position from the 'v'-label.
            curlicues$b_value$annotate$vjust <-
                curlicues$v_value$annotate$vjust
        }
    }
    ##  Adjust the colour and the size of the numerical convergence
    ##  information if things did not converge numerically.
    if (isFALSE(look_up$details$convergence)) {
        curlicues$NC_value$annotate$size <- 4 + 2 *
            curlicues$NC_value$annotate$size
        curlicues$NC_value$annotate$col  <- "red"
    }
    ##  Add the relevant labels, and use information from 'xlim' to
    ##  set the x-component of the position (it is here assumed that a
    ##  scaling of the plots later on only will be related to a
    ##  scaling of the second axis).  The user can adjust the
    ##  'xlim'-value by the help of the 'x.label_low_high'-node of
    ##  'input_curlicues'-argument of 'LG_plot_helper'.
    xlim <- 
        if (is.null(look_up$input_curlicues$x.label_low_high)) {
            look_up$xlim
        } else {
            look_up$input_curlicues$x.label_low_high
        }
    ##  The 'x_low_hig'-node as done its task, and as it deviates in
    ##  format from the rest of the nodes it should now be removed.
    curlicues$x.label_low_high <- NULL
    look_up$input_curlicues$x.label_low_high <- NULL
    ##  Reminder: The labels of interest will later on be added to the
    ##  plot based on a single data-frame and the 'annotate' function
    ##  (with 'geom="text"').  It is then necessary to use
    ##  'parse=TRUE' in order for the 'plot-stamp' to be properly
    ##  rendered, and this implies that all the labels below must be
    ##  constructed in such a manner that they work with 'parse=TRUE'.
    if (CSC$plot_stamp$include) {
        CSC$plot_stamp$annotate <- list(
            x = xlim[1] + 0.5*diff(xlim),
            label = LG_lookup_curlicues_plot_stamp(
                look_up = look_up,
                CSC = CSC))
    }
    if (CSC$n_R_L_values$include) {
        CSC$n_R_L_values$annotate <- list(
            x = xlim[2],
            label = local({
                ##  Identify the length 'n', the number of blocks 'R'
                ##  and the block length for the case where a
                ##  resampling has been performed.
                .n <- sprintf("n == '%s'",
                              look_up$details$N)
                .R <- sprintf("R == '%s'",
                              if (look_up$is_block) {
                                  look_up$details$nr_simulated_samples
                              } else {
                                  if (look_up$is_bootstrap) {
                                      look_up$nb
                                  } else
                                      1
                              })
                .l <- sprintf("L == '%s'", look_up$block_length)
                ##  Collect the pieces into the list-format that
                ##  'plotmath' requires, taking into account that the
                ##  block length only should occur in some cases.
                .include_L <- all(
                    look_up$is_bootstrap,
                    ! all(any(look_up$L2_distance_normal,
                              look_up$L2_distance_percentages),
                          look_up$look_up$L2_inspection_vbmL == "L"))
                sprintf("list(%s,%s%s)",
                        .n,
                        .R,
                        ifelse(test = .include_L,
                               yes  = sprintf(",%s", .l),
                               no   = ""))
            }))
    }
    if (CSC$m_value$include) {
        CSC$m_value$annotate  <-  list(
            x = xlim[1],
            label = sprintf("m == '%s'",
                            look_up$m_selected))
        ##  The frequency of inspection should be included too when a
        ##  complex-valued inspection is performed.  The inclusion of
        ##  the user defined number of digits requires some tweaking.
        if (look_up$complex) {
            CSC$m_value$annotate$label <- local({
                .tmp <- sprintf(
                    "list(m == '%%s', omega == '%%.%sf')",
                    look_up$complex_frequency_print_digits)
                sprintf(.tmp,
                        look_up$m_selected,
                        look_up$complex_frequency)
            })
        }
    }
    if (CSC$complex_Cartesian$include) {
        CSC$complex_Cartesian$annotate  <-  list(
            x = xlim[1],
            label = "'Cartesian, '*z == x + i*y")
    }
    if (CSC$complex_polar$include) {
        CSC$complex_polar$annotate  <-  list(
            x = xlim[1],
            label = "'Polar, '*z == re^{i*theta}")
    }
    if (CSC$v_value$include) {
        CSC$v_value$annotate = list(
            x = xlim[2],
            label = sprintf("%s*'%% :: '*%s*'%%'",
                            round(x = pnorm(look_up$.point_coord[1]),
                                  digits = 3) * 100,
                            round(x = pnorm(look_up$.point_coord[2]),
                                  digits = 3) * 100))
    }
    if (CSC$b_value$include) {
        CSC$b_value$annotate = list(
            x = xlim[2],
            label = sprintf("b == '%s'",
                            look_up$bw_points))
    }
    if (CSC$NC_value$include) {
        CSC$NC_value$annotate = list(
            x = xlim[1],
            label = if (look_up$type == "par_one") {
                        structure(
                            .Data = "'Computations based on the heinous 1-parameter approach.  Use 5-parameter instead!'",
                            short = "'Warning: 1-parameter approach!'")
                    } else {
                        structure(
                            .Data = ifelse(test = isTRUE(look_up$details$convergence),
                                           yes  = "NC == OK~'(numerical convergence verified)'",
                                           no   = "NC == FAIL~'(numerical convergence failed)'"),
                            convergence = isTRUE(look_up$details$convergence),
                            short = ifelse(test = isTRUE(look_up$details$convergence),
                                           yes  = "NC == OK",
                                           no   = "NC == FAIL"))
                    })
    }
    ##  Add details relevant for the spectra-case.
    if (look_up$TCS_type == "S") {
        .lty = ifelse(test = look_up$is_block,
                      yes  = 2,
                      no   = 1)
        CSC$spectra_plot <- list(
            global = list(linetype = .lty),
            local = list(linetype = .lty),
            WN_line = list(yintercept = switch(
                               EXPR = look_up$spectra_type,
                               Co = ifelse(
                                   test = look_up$is_auto_pair,
                                   yes  = 1,
                                   no   = 0),
                               Quad = 0,
                               amplitude = ifelse(
                                   test = look_up$is_auto_pair,
                                   yes  = 1,
                                   no   = 0),
                               phase = 0)))
        kill(.lty)
    }
    ##  Update the 'curlicues'-values.
    curlicues <- LG_update_list(
        .new = CSC,
        .old = curlicues)
    ##  When required, update any user-specified values (relevant when
    ##  fine tuning is desired outside of the 'shiny'-application.)
    if (!is.null(look_up$input_curlicues)) {
        curlicues <- LG_update_list(
            .new = look_up$input_curlicues,
            .old = curlicues)
    }
    ##  A special adjustment is needed if the 'NC_value'-node
    ##  specifies a short label.
    if (identical(x = curlicues$NC_value$short_or_long_label,
                  y = "short")) {
        curlicues$NC_value$annotate$label =
            attributes(curlicues$NC_value$annotate$label)$short
    }
    ##  Ensure that the proper adjustment is made if a numerical
    ##  convergence problem should be highlighted for the boxplot of
    ##  the local Gaussian autocorrelations.
    if (all(look_up$TCS_type == "C",
            isFALSE(look_up$details$convergence))) {
        ##  Update the values used for the boxplot, so it is easy to
        ##  detect the problematic lags.
        .subsetting <- look_up$details$NC_fail_details$C_status
        .colour <- rep(
            x = curlicues$correlation_plot$boxplot$colour,
            times = length(.subsetting))
        .fill <- rep(
            x = curlicues$correlation_plot$boxplot$fill,
            times = length(.subsetting))
        ##  Adjust '.colour' and '.fill' for the problematic cases.
        .colour[.subsetting] <- "brown"
        .fill[.subsetting] <- "red"
        ##  Update the values to be used.
        curlicues$correlation_plot$boxplot$colour <- .colour
        curlicues$correlation_plot$boxplot$fill <- .fill
        kill(.subsetting, .colour, .fill)
    }
    ##  Add the data-frame needed in order for the text-annotations.
    curlicues$text$annotated_df <- local({
        ##  Identify relevant nodes.
        .tmp <- vapply(
            X = curlicues,
            FUN = function(.node) {
                all(.node$include,
                    .node$annotate$geom == "text",
                    ! is.null(.node$annotate$label))
            },
            FUN.VALUE = logical(1))
        text_nodes <- names(.tmp)[.tmp]
        ##  Extract information, and make a data-frame out of it.
        .tmp <- lapply(
            X = text_nodes,
            FUN = function(.node) {
                curlicues[[.node]]$annotate
            })
        .tmp <- as.data.frame(
            x = do.call(
                what = "rbind",
                args = .tmp),
            row.names = text_nodes)
        ##  Convert the columns into the desired format, so the
        ##  functions later on can manage to digest them.
        for (.r in colnames(.tmp)) {
            .tmp[[.r]] <- unlist(.tmp[[.r]])
        }
        ##  Return 'NULL' if an empty data-frame has been created,
        ##  otherwise return the desired data-frame.
        if (length(.tmp) == 0) {
            NULL
        } else {
            .tmp
        }
    })
    ##  Add a quote for the code that adds the text to the plot.
    curlicues$text$annotated <- local({
        .df <- curlicues$text$annotated_df
        ##  Return 'NULL' if nothing should be added.
        if (is.null(.df))
            return(NULL)
        ##  The list to be used with 'create_call'
        .tmp <- list(geom = "text",
                     x = .df$x,
                     y = .df$y,
                     label = .df$label,
                     size = .df$size,
                     col = .df$col,
                     alpha = .df$alpha,
                     vjust = .df$vjust,
                     hjust = .df$hjust,
                     parse = TRUE)
        create_call(
            .cc_fun = "annotate",
            .tmp,
            .cc_list = TRUE)
    })
    ##  Return the result to 'LG_lookup', which will add this as a new
    ##  node to its 'look_up'-list.
    curlicues
}
