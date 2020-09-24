#' Helper-function to create the plot-stamp-label for the the \code{curlicues}-list
#'
#' @description This internal function is called by
#'     \code{LG_lookup_curlicues} in order to produce the plot-stamp,
#'     i.e. the annotated text that reveals the content of the plot
#'     under investigation.
#'
#' @param look_up A list from the internal workings of the function
#'     \code{LG_lookup}.
#'
#' @param CSC The case-specific-curlicues (CSC) from the internal
#'     workings of the \code{LG_lookup_curlicues}-function.
#'
#' @return This function returns the plot-stamp-label to the calling
#'     function, i.e. \code{LG_lookup_curlicues}, which will add it to
#'     the \code{CSC}-list.
#'
#' @keywords internal

LG_lookup_curlicues_plot_stamp <- function(look_up, CSC) {
    ##  Create the key ingredients of the plot-stamp-label.  Note that
    ##  some of the distance based plots (those that consider
    ##  percent-wise changes) will require more than one instance of
    ##  the basic spectrum-component, and as such it is necessary to
    ##  take that into account while constructing the labels.  The
    ##  present setup starts with the details related to 'm'
    ##  (truncation level) and 'L' (block length for the bootstrap).
    .m_sup <-
        if (look_up$TCS_type == "S") {
            if (all(any(look_up$L2_distance_plot,
                        look_up$L2_distance_percentages),
                    look_up$L2_distance_vbmL == "m")) {
                if (look_up$L2_distance_plot) {
                    "^m"
                } else {
                    c("^{m+1}", "^m")
                }
            } else {
                sprintf("^'%s'",
                        ifelse(test = look_up$heatmap_b_or_v == "m",
                               yes  = "m",
                               no   = look_up$m_selected))
            }
        } else {
            ""
        }
    .L_sub <-
        if (look_up$TCS_type == "S") {
            if (all(any(look_up$L2_distance_plot,
                        look_up$L2_distance_percentages),
                    look_up$L2_distance_vbmL == "L")) {
                if (look_up$L2_distance_plot) {
                    "['[L]']"
                } else {
                    c("['[L+1]']", "['[L]']")
                }
            } else {
                ""
            }
        } else {
            ""
        }
    ##  Compute the remaining details
    .f_or_F <- ifelse(
        test = {look_up$TCS_type == "C"},
        yes  = "rho",
        no   = look_up$spectra_f_or_F)
    .global_or_local <- ifelse(
        test = look_up$is_local,
        yes  = "[v]",
        no   = "")
    ##  Reminder: The 'ifelse' only picks the first element from a
    ##  vector, and it is thus required to use 'if'-'else' for the
    ##  case below.
    .exponent_or_not <-
        if(look_up$TCS_type == "C") {
            ""
        } else {
            .m_sup
        }
    .h_or_omega <- ifelse(
        test = {look_up$TCS_type == "C"},
        yes  = "h",
        no   = "omega")
    ##  Create the basic plot-stamp-label.
    .label = sprintf(
        "%s%s%s*(%s)%s",
        .f_or_F,
        .global_or_local,
        .exponent_or_not,
        .h_or_omega,
        .L_sub)
    ##  We are done if the inspection is of the correlations, or if it
    ##  is an investigation of the complex-valued presentation.
    if (any(look_up$TCS_type == "C",
            look_up$complex))
        return(.label)
    ##  Still running? Then check if it is necessary to add some
    ##  additional stuff, in which case the distance investigation
    ##  takes precedence over the other cases.
    if (any(look_up$L2_distance_plot,
            look_up$L2_distance_percentages)) {
        .label <- sprintf("D*(%s)",
                          .label)
        ##  Two components will be present in the "percentages"-case,
        ##  and for those the label need to be readjusted.
        if (length(.label) == 2) {
            ## sprintf("100*%.%*(%s/%s - 1)",
            .label <- sprintf(
                ## "100*(%s/%s - 1)",
                "100%%.%%(%s/%s - 1)",
                .label[1],
                .label[2])
        }
        return(.label)
    }
    ##  Finally, check if it is necessary to specify if the target of
    ##  interest is a 'Co'-, 'Quad'-, 'amplitude'- or 'phase'-plot.
    if (any(look_up$is_cross_pair,
            all(look_up$is_auto_pair,
                look_up$is_local,
                look_up$is_off_diagonal))) {
        ## Extend the stamp-label with information that
        ## reveals the spectrum variant.
        .label <- sprintf(
            "%s(%s)",
            switch(
                EXPR = look_up$details$spectrum_variant,
                Co = "Co",
                Quad = "Quad",
                amplitude = "alpha",
                phase = "phi"),
            .label)
    }
    ###-------------------------------------------------------------------
    ##  Return the label to 'LG_lookup_curlicues', which will add this
    ##  as the 'label'-node of 'CSC$plot_stamp$annotate'.
    .label
}
