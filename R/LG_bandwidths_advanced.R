#' Find bandwidths to use for the local Gaussian estimates
#'
#' @description The original bandwidth-algorithm gives a global result
#'     that does not work so good when used in the periphery of the
#'     data.  This internal function enables us to combine the
#'     bandwidths from the original algorithm and those found by a
#'     nearest-neighbour strategy where the selection is made with
#'     regard to the requirement that a certain percentage of the
#'     total number of lagged points should be contained inside the
#'     region specified by a central point and a bandwidth-square.
#'
#' @template save_dir_arg
#' 
#' @param TS The time series we want to investigate by means of a
#'     Local Gaussian Approximation (and later on with Local Gaussian
#'     Spectra).
#'
#' @param lag_max The number of lags to include in the analysis.
#'  
#' @param .bws_mixture An argument that specifies how the global
#'     bandwidths and those obtained by the nearest-neighbour strategy
#'     should be combined.  The three available options are
#'     \code{c("mixture", "local", "global")}, which have the
#'     following effects.  The alternatives \code{local} and
#'     \code{global} will respectively only select the nearest
#'     neighbour or global.  These alternatives seems however to not
#'     work well when used on some parts of the lagged pairs of
#'     interest, i.e. the nearest neighbour values might be to "small"
#'     in the center of the distribution, whereas the global
#'     bandwidths seems to fail in the periphery of the distribution.
#'     The alternative \code{mixture} attempt to resolve this by (for
#'     each grid point) selecting the largest of the two alternative
#'     bandwidths.  Note that the value of \code{.bws_mixture} decides
#'     how much information that is computed, i.e. the alternative
#'     \code{local} will turn off the computation of global
#'     bandwidths.  However, the computations of the nearest neighbour
#'     bandwidths will also be computed when the alternative
#'     \code{global} is used, since it does not take long to compute
#'     and it is that function that creates the array we need as a
#'     mould for the result.  If the user does not make a selection,
#'     then all three alternatives will be computed.
#'
#' @param bw_points A vector, default \code{c(25, 35)}, that specifies
#'     the percentage of the observations that we want inside the
#'     "bandwidth-square".  If \code{.bws_mixture} is selected to be
#'     \code{global}, then this argument will be ignored. and no
#'     nearest neighbours will be computed.
#' 
#' @param levels The points at which we (for different lags) want
#'     to center the "bandwidth-squares".  The format of
#'     \code{levels} must be a matrix with one row for each point
#'     of interest, and with columns named \code{c("v1", "v2")}.
#'
#' @return A list with the two arrays \code{h} and \code{convergence}.
#'     The array \code{h} contains the bandwidths to be used when
#'     finding local Gaussian estimates for the lagged pairs (of the
#'     time series \code{TS}) from 1 to \code{lag_max}, whereas the
#'     array \code{convergence} contains information related to the
#'     numerical convergence of the bandwidths returned from the
#'     global algorithm.
#'
#' @keywords internal

LG_bandwidths_advanced <- function(
    save_dir = NULL,
    TS,
    lag_max,
    .bws_mixture = c("mixture", "local", "global"),
    bw_points = c(25, 35),
    levels) {
    ##  Create a spy-report.
    spy_report <- spy()
    kill(save_dir, lag_max, bw_points, levels)
    ##  If 'TS' originates from a 'TS_LG_object', it should have an
    ##  attribute 'TS_for_analysis' that should be used instead of TS.
    if (! identical(x = attributes(TS)$TS_for_analysis,
                    y = NULL)) {
        TS <- attributes(TS)$TS_for_analysis
        spy_report$envir$TS <- TS
    }
    ##  Find the nearest neighbour bandwidths.  Always do this as we
    ##  need it to mould the remaining results upon.
    .bws_nearest_neighbour <- eval(create_call(
        .cc_fun = bws_nearest_neighbour,
        spy_report$envir,
        .cc_list = TRUE))
    ##  Add an extra dimension for the values of "bi" and "bj".  This
    ##  is necessary in order to have compatibility with the result
    ##  from the global case.  Note that we for compatibility with
    ##  those results want the dimension 'bw_points' as the last one.
    .bws_nearest_neighbour  <- append_dimensions(
        orig_arr = .bws_nearest_neighbour,
        added_dimnames = list(find_bws = c("bi", "bj")),
        positions = length(dim(.bws_nearest_neighbour)))
    ##  Find the global bandwidths, when required.
    if (any(c("mixture", "global") %in% .bws_mixture)) {
        ##  Find the values for the positive lags.
        .bws_global <- eval(create_call(
            .cc_fun = bws_global,
            spy_report$envir,
            .cc_list = TRUE))
        ##  Extract information about convergence.
        .convergence <- restrict_array(
            .arr = .bws_global,
            .restrict = list(find_bws = "convergence"))
        ##  Add dimensions to match the structure from the nearest
        ##  neighbour case.
        .new_dims <- setdiff(
            x = names(dimnames(.bws_nearest_neighbour)),
            y = names(dimnames(.bws_global)))
        .new_pos <- which(TRUE == names(dimnames(
                              .bws_nearest_neighbour)) %in% .new_dims)
        .bws_global <- append_dimensions(
            orig_arr = .bws_global,
            added_dimnames =
                dimnames(.bws_nearest_neighbour)[.new_dims],
            positions = .new_pos)
        kill(.new_dims, .new_pos)
        ##  Restrict '.bws_global' to the values for "bi" and "bj",
        ##  and only one occurrence of 'bw_points'.
        .bws_global <- restrict_array(
            .arr = .bws_global,
            .restrict = list(
                bw_points = 1,
                find_bws = c("bi", "bj")))
        ##  Adjust dimension name to match content.
        dimnames(.bws_global)$bw_points <- "global"
    }
    kill(TS, spy_report)
    ###------------------------------------------------------###
    ##  The stuff above can now be combined in order to find the
    ##  mixture (when required) where the largest value of the
    ##  bandwidths will be used.  The key to doing this without to
    ##  much fuzz is 'pmax', using the recycling principle with regard
    ##  to the global bandwidths.  Reminder: In order for the result
    ##  to have the desired format, the arrays from nearest neighbour
    ##  must be given as the first argument.
    ###------------------------------------------------------###
    if (any("mixture" %in% .bws_mixture)) {
        ##  Use 'pmax' on the positive lags, recycling automatic.
        .bws_mixture_h <- pmax(
            .bws_nearest_neighbour,
            .bws_global)
        ##  Adjust the dimension names.
        dimnames(.bws_mixture_h)$bw_points <- paste(
            "mix",
            dimnames(.bws_mixture_h)$bw_points,
            sep = "_")
    }
    ##  Use abind to produce the final result, by pasting stuff
    ##  together along the dimension named "bw_points".
    .result <- abind(
        if ("local" %in% .bws_mixture)
            .bws_nearest_neighbour,
        if ("global" %in% .bws_mixture)
            .bws_global,
        if ("mixture" %in% .bws_mixture)
            .bws_mixture_h,
        along = which(names(dimnames(
            .bws_nearest_neighbour)) ==  "bw_points"))
    class(.result) <- LG_default$class$array
    ##  Update the names of the dimensions.
    names(dimnames(.result)) <-
        names(dimnames(.bws_nearest_neighbour))
    ##  Add attributes to reveal if computations later on can be
    ##  recycled, i.e. to see if we perhaps can avoid redoing
    ##  computations for the "mixture"-case.
    recycling <- all(
        c("mixture", "local", "global") %in% .bws_mixture)
    ##  Create look-up attribute for the positive lags.
    look_up <-
        if (recycling) {
            ##  Identify equalities between mixture and local,
            ##  matching shapes of arrays ensures a simple test.
            p_part <- 
                .bws_mixture_h == .bws_nearest_neighbour
            ##  Identify equality between mixture and global.  The
            ##  arrays have different shapes, and must be converted to
            ##  vectors in order for recycling to be performed.
            g_part <- p_part  ##  Create storage for global part.
            g_part[] <-
                as.vector(.bws_mixture_h) == as.vector(.bws_global)
            ##  The two objects 'p_part' and 'g_part' need to be
            ##  reduced one dimension, since we want to know if all
            ##  the bandwidths occur from the same source.
            p_part <- aaply(
                .data = p_part,
                .margins = which(names(dimnames(p_part)) != "find_bws"),
                .fun = all,
                .drop = FALSE)
            g_part <- aaply(
                .data = g_part,
                .margins = which(names(dimnames(g_part)) != "find_bws"),
                .fun = all,
                .drop = FALSE)
            ##   Create the array to use for 'look_up'.
            .tmp <- p_part #  Create a storage for the look-up.
            .tmp[] <- "m"  #  Default 'm' (mixture) means compute new.
            ##  Insert 'p' (percentage) or 'g' (global) to avoid
            ##  recomputing stuff when the bandwidths coincide
            ##  completely with those from respectively the nearest
            ##  neighbour algorithm or global algorithm.
            .tmp[p_part] <- "p"
            .tmp[g_part] <- "g"
            kill(p_part, g_part)
            ##  Return the result to the object 'look_up'.
            .tmp
        } else
            NA
    ##  Add the attributes to '.result'.
    attributes(.result) <- c(
        attributes(.result),
        list(recycling = recycling,
             look_up = look_up))
    ##  Collect the answer and return it to the work-flow.  Reminder:
    ##  The part '.convergence' might become rather large when used in
    ##  the bootstrap-setting, so it's not added as attribute.
    list(h = .result,
         convergence =
             if (exists(".convergence")) {
                 .convergence
             } else
                 0)
}
