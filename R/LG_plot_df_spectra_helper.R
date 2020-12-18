#' Helper-function for \code{LG_plot_df_spectra}
#'
#' @description This internal function is called by
#'     \code{LG_plot_df_spectra} in order to compute the (local
#'     Gaussian) spectra.
#'
#' @param ..env The environment that contains the correlations, and
#'     also the environment in which the result will be stored.
#'
#' @param look_up An environment created by \code{LG_lookup} (in the
#'     function \code{LG_plot_helper}), where the key details have
#'     been extracted (or constructed) from a (non-reactive copy) of
#'     the values defined by the \code{shiny}-interface.
#'
#' @param .gl Either "local" or "global", default "local"
#'
#' @return The environment \code{..env} will be updated with lists
#'     containing arrays with information about the different spectral
#'     densities that can be investigated.
#'
#' @keywords internal

LG_plot_df_spectra_helper <- function(..env, look_up, .gl = "local") {
    ##  Two shortcuts to get the code more compact.
    cache <- look_up$cache
    restrict <- look_up$restrict
    ##  A function that return a logical value that will be true for
    ##  those cases where the resulting spectrum is real valued.  This
    ##  is the case for univariate time series, alleyways for the global
    ##  spectral density and when on the diagonal for the local
    ##  spectral densities.
    .real <- function(.node) {
        if (look_up$is_cross_pair)
            return(FALSE)
        if (.gl == "global") {
            return(TRUE)
        } else
            identical(x = .node,
                      y = "on_diag")
    }
    ##  Identify the relevant cache-details, that is needed in order
    ##  to get hold of the correct data.
    weights_cache <- ifelse(
        test = {look_up$spectra_f_or_F == "f"},
        yes  = cache$weights_f,
        no   = cache$weights_F)
    summands_cache <- ifelse(
        test = {.gl == "local"},
        yes  = cache$spectra_summands_local,
        no   = cache$spectra_summands_global)
    spectra_cache <- ifelse(
        test = {.gl == "local"},
        yes  = cache$spectra_local,
        no   = cache$spectra_global)
    ###-------------------------------------------------------------------
    ##  Add a couple of helper-functions that will be used at the end
    ##  of this function.  Reminder: The (potentially) complex-valued
    ##  spectra requires an investigation by means of the Co-, Quad-,
    ##  amplitude- and phase-spectra.  This will be done by adding a
    ##  new dimension "S_type" to the arrays.  To get a unified
    ##  treatment later on, the dimensions for the real valued cases
    ##  will be modified by adding a length one "S_type"-dimension
    ##  having the name "Co".  Moreover, the need for a common ylim
    ##  for the different plots will also be taken care of here - in
    ##  particular, the ylims computed here will be the one to be used
    ##  if we want to keep the given truncation level and see how
    ##  things changes between the points (given the specified
    ##  selection of Vi and Vj).
    S_type_fun <- function(x, .cut) {
        ##  A minor helper function to take care of the extraction
        ##  of the relevant parts.
        .extract <- function(.type) {
            ##  Return 'NULL' when relevant
            if (all(mode(x) != "complex",
                    .type != "Co"))
                return(NULL)
            ##  Find the function to be used.
            .fun <- switch(
                EXPR = .type,
                Co = function(x) Re(x),
                Quad = function(x) -Im(x),
                amplitude = function(x) Mod(x),
                phase = function(x) Arg(x))
            ##  Find the desired array
            .arr <- structure(
                .Data = .fun(x),
                .Dim = c(1, 1, dim(x)),
                .Dimnames = c(list(S_type = .type,
                                   cut = .cut),
                              dimnames(x)))
            ##  Return the array together with information about
            ##  the range of it (to be used when creating the
            ##  ylim-value for the plot function).
            list(.arr = .arr,
                 .range = structure(
                     .Data = range(.arr),
                     .Dim = c(1, 1, 2),
                     .Dimnames = list(S_type = .type,
                                      cut = .cut,
                                      range = c("min", "max"))))
        }
        ##  Compute the desired components (which can be NULL)
        .Co <- .extract(.type = "Co")
        .Quad <- .extract(.type = "Quad")
        .amplitude <- .extract(.type = "amplitude")
        .phase <- .extract(.type = "phase")
        ##  Return the desired result as a list, using the wrapper
        ##  'my_abind' for the merging of the components.
        list(spec = my_abind(.Co$.arr,
                             .Quad$.arr,
                             .amplitude$.arr,
                             .phase$.arr),
             range = my_abind(.Co$.range,
                              .Quad$.range,
                              .amplitude$.range,
                              .phase$.range))
    }
    ##  A function that computes the ylim-value to be used when all
    ##  the cuts are taken into account.  Reminder: The procedure
    ##  applied for this is to collect the information from the nodes
    ##  first, and then merge that into a final result, with the help
    ##  of the 'eval'+'bquote+'.()'-construction.  Note that this
    ##  function also adds the result as a node to the list specified
    ##  by '.spec'.
    .find_ylim <- function(.spec) {
        .ylim <- list()
        for (.node in names(..env[[.spec]])) {
            my_abind_quote <- quote(my_abind())
            for (.cut in names(..env[[.spec]][[.node]])) {
                my_abind_quote[[as.numeric(.cut) + 1]] <-
                    bquote(..env[[.spec]][[.node]][[.(.cut)]]$range)
            }
            .ylim[[.node]] <- eval(my_abind_quote)
        }
        kill(.node, .cut, my_abind_quote)
        ##  Another quote-construction in order to get the minimum of
        ##  the min and the maxima of the max.
        for (.node in names(.ylim)) {
            .range <- list()
            for (.mm in dimnames(.ylim[[.node]])$range) {
                ##  This part is the result of some toying around with
                ##  the quote-construction...
                .range[[.mm]] <- eval(bquote(local({
                    .tmp <- restrict_array(
                        .arr = .ylim[[.node]],
                        .restrict = list(range = .(.mm)))
                    .fun <- bquote(do.call(what = .(.mm),
                                           args = list(x)))
                    .res <- aaply(
                        .data = .tmp,
                        .margins = which(names(dimnames(.tmp))=="S_type"),
                        .fun = function(x)
                            eval(.fun),
                        .drop = FALSE)
                    dimnames(.res) <- c(
                        dimnames(.res)["S_type"],
                        list(range = .(.mm)))
                    ##  Return the result
                    .res
                })))
            }
            ##  Update the stored values
            .ylim[[.node]] <- my_abind(
                .range$min,
                .range$max)
        }
        kill(.node, .range, .mm)
        ##  Finally, add an ylim-node to the spectra information.
        ..env[[.spec]]$ylim <- .ylim
        ## ## ## attributes(..env[[.spec]])$ylim <- .ylim
    }
    ##  Initiate the list-structure.
    ..env[[spectra_cache]] <- list()
    ##  Compute a partial list with the sums based on the
    ##  contributions from the positive lags, and then use the
    ##  diagonal-reflection property to get hold of the values for
    ##  the negative lags too.  Take care to add the correct
    ##  values for the lag-zero contributions when required.
    .intermediate <- list()
    for (.cut in names(..env[[weights_cache]])) {
        for (.node in names(..env[[summands_cache]])) {
            if (is.null(..env[[summands_cache]][[.node]]))
                next
            ##  Extract the relevant part of the stored
            ##  correlation-values.
            .tmp <- restrict_array(
                .arr =  ..env[[summands_cache]][[.node]]$pos,
                .restrict = look_up$restrict$S$spectra_local(.cut))
            ##  Multiply correlations with weights.
            .tmp <- multiply_arrays(
                .arr1 = .tmp,
                ..env[[weights_cache]][[.cut]])
            ##  The summation, that gives the (partial)
            ##  estimate of the spectral density.
            .dims <- which(names(dimnames(.tmp)) == "lag")
            .intermediate[[.node]][[.cut]] <- structure(
                .Data = colSums(x = .tmp,
                                dims = .dims),
                .Dim = dim(.tmp)[-.dims],
                .Dimnames = dimnames(.tmp)[-.dims],
                class = class(.tmp))
        }
    }
    kill(.cut, .node, .tmp, .dims)
    ##  REMINDER: Update the part above so it can be done in parallel.
    ###------------------------------------------------------###
    ##  Use the diagonal reflection to compute the estimated local
    ##  spectra.  Reminder: With (p1,p2) the point of investigation
    ##  and (p2,p1) its diagonally reflection, the local Gaussian
    ##  correlations satisfies '$\rho_{kl|(p1,p2)}(-h) =
    ##  \rho_{lk|(p2,p1)}(h)$'.  The available information about the
    ##  positive lags, together with the lag-zero contribution thus
    ##  enables us to compute the local Gaussian spectral densities at
    ##  this stage.  Moreover, it follows from the diagonal reflection
    ##  property that the local Gaussian spectrum
    ##  '$f_{kl|(p1,p2)}(\omega)$' equals the complex conjugate of
    ##  '$f_{lk|(p2,p1)}(\omega)$'.  It is thus sufficient to compute
    ##  the desired values for the case where the index $k$ is less
    ##  than (or equal to $l$).  Finally, for the auto-spectra case
    ##  (i.e. $k=l$) it is also sufficient to only consider the case
    ##  where the point $p1$ is less than or equal to $p2$.  However,
    ##  to avoid an additional layer of complexity when subsetting the
    ##  result later on, I will not implement this particular detail
    ##  for the auto-spectra at the moment.  Still, I will use the
    ##  simplification that the local Gaussian auto-spectra for a
    ##  point on the diagonal is real-valued.
    ###------------------------------------------------------###
    ##  Identify the 'pairs'-values to be used in the
    ##  construction, common for all cases.
    .sorted_ViVj <- look_up$pairs_ViVj
    .rev_sorted_ViVj <- look_up$pairs_VjVi
    for (.node in names(.intermediate)) {
        if (is.null(.intermediate[[.node]]))
            next
        ##  Unfold the information from '.intermedatiate', by
        ##  constructing the "reversed" dimnames.  These are needed in
        ##  order for the addition of the arrays to work properly.
        ##  NB: The reversed version for the local sepctra need to
        ##  identify the reversed-levels, where level is the name used
        ##  for the storing of the points in the format "p1_p2".
        .dn <- dimnames(.intermediate[[.node]][[1]])
        .rev_dn <- .dn
        .rev_dn$pairs <- .sorted_ViVj
        if (.gl == "local") {
            .rev_dn$levels <- local({
                ##  Read the present levels of interest
                .levels <- .dn$levels
                ## Split the components and store in an array
                .levels <- stringr::str_split(string = .levels,
                                              pattern = "_",
                                              simplify = TRUE)
                ## Paste together in the reversed order.
                paste(.levels[, 2],
                      .levels[, 1],
                      sep = "_")
            })
        }
        ##  Find the contribution that must be added for the lag-zero
        ##  case.  This will always be one in the auto-spectrum case
        ##  (taken care of below), whereas it must be extracted from
        ##  the previously stored values in the cross-spectrum case.
        .lag_zero <-
            if (look_up$spectra_f_or_F == "f") {
                ##  The spectral density.
                if (look_up$is_auto_pair) {
                    1
                } else {
                    restrict_array(
                        .arr = ..env[[summands_cache]][[.node]]$lag_zero,
                        .restrict = list(pairs = .sorted_ViVj))
                }
            } else {
                ##  The cumulative spectral density.
                local({
                    ..omega <- matrix(
                        data = look_up$omega_vec,
                        ncol = 1,
                        dimnames = list(omega = look_up$omega_vec,
                                        pairs = .sorted_ViVj))
                    if (look_up$is_auto_pair) {
                        ..omega + .5
                    } else {
                        .tmp <- restrict_array(
                            .arr = ..env[[summands_cache]][[.node]]$lag_zero,
                            .restrict = list(pairs = .sorted_ViVj))
                        .tmp <- append_dimensions(
                            orig_arr = .tmp,
                            added_dimnames = list(omega = look_up$omega_vec))
                        ##  Multiply the lag-zero-values with omega to
                        ##  arrive at the integrated values of interest.
                        .res <- multiply_arrays(
                            .arr1 = .tmp,
                            .arr2 = ..omega)
                        ##  Add half of the lag-zero-values to get the
                        ##  correct starting value at omega = 0.
                        add_arrays(
                            .arr1 = 0.5 * .tmp,
                            .arr2 = .res)
                    }
                })
            }
        ##  Add the corner-case with "cut" equal to "1".  This must be
        ##  present in order to avoid glitches in the interactive
        ##  investigation.  The auto-spectra case requires some more
        ##  work in this case.  Reminder: Since we do have a constant
        ##  in this case, it is sufficient to use only the end-points
        ##  of the frequency range in the omega-dimension.
        ..env[[spectra_cache]][[.node]][["1"]] <- 
            if (look_up$is_auto_pair) {
                ##  Adjust the dimension-names to minimise the size of
                ##  the resulting object.
                .dn$omega  <- look_up$frequency_range
                .dn$pairs <- .sorted_ViVj
                ##  Create the array.  The value '1' gives a
                ##  horizontal line at height 1 for the 0-truncated
                ##  spectral density , whereas the other alternative
                ##  is the line corresponding to the 0-truncated
                ##  cumulative spectral density.
                .values <- if (look_up$spectra_f_or_F == "f") {
                               1
                           } else {
                               .dn$omega + .5
                           }
                ##  The result must be stored as complex valued when
                ##  we are off the diagonal, since the mode of the
                ##  node later on will decide the desired outcome.
                if (.gl == "local") {
                    if (.node == "off_diag")
                        .values <- .values + 0i
                }
                ##  Return the desired array
                array(
                    data = .values,
                    dim = vapply(X = .dn,
                                 FUN = length,
                                 FUN.VALUE = numeric(1)),
                    dimnames = .dn)
            } else {
                ##  Reminder: This case must be saved as
                ##  complex-valued in order for the tests later on
                ##  (which uses the modes of the nodes) to treat it
                ##  correctly.  NOTE: The 'as.complex'-function killed
                ##  of the dimension-names when used on the case where
                ##  the array only contained one value.  The solution
                ##  below circumvents that problem.
                .lag_zero[] <- as.complex(.lag_zero)
                if (is.null(dimnames(.lag_zero)$omega)) {
                    append_dimensions(
                        orig_arr = .lag_zero,
                        added_dimnames = list(omega = look_up$frequency_range))
                }
            }
        kill(.dn)
        ##  Time to find the spectral densities, which can be complex
        ##  valued in quite a few of the cases of interest.
        for (.cut in names(.intermediate[[.node]])) {
            ##  Create the arrays to be added.
            if (look_up$is_auto_pair) {
                ##  Reminder: For the auto-case, only one array is
                ##  needed for the global spectral density.  That is
                ##  also sufficient for the local spectral density on
                ##  the diagonal, but off-diagonal it is necessary to
                ##  include another array too.
                .arr_ViVj <- restrict_array(
                    .arr = .intermediate[[.node]][[.cut]],
                    .restrict = list(pairs = .sorted_ViVj))
                if (.gl == "local") {
                    if (.node == "off_diag") {
                        ##  We need a copy of the first array, but
                        ##  with the revised naming on the levels.
                        .rev_arr_ViVj <- .arr_ViVj
                        dimnames(.rev_arr_ViVj) <- .rev_dn
                    }
                }
            } else {
                ##  The cross-case.
                .arr_ViVj <- restrict_array(
                    .arr = .intermediate[[.node]][[.cut]],
                    .restrict = list(pairs = .sorted_ViVj))
                .rev_arr_ViVj <- restrict_array(
                    .arr = .intermediate[[.node]][[.cut]],
                    .restrict = list(pairs = .rev_sorted_ViVj))
                dimnames(.rev_arr_ViVj) <- .rev_dn
            }
            ##  Take the sum of the non-zero lags, keep in mind that a
            ##  simplification can be made for the local spectrum in
            ##  the auto-case when on the diagonal, since symmetry
            ##  then implies that we have a real-valued result.
            .res_ViVj <-
                if  (.real(.node)) {
                    ## Symmetry simplifies to.
                    2 * Re(.arr_ViVj)
                } else {
                    ##  Take the sum of the arrays, and recall that a
                    ##  complex conjugation is needed for the part
                    ##  corresponding to the negative lags (obtained
                    ##  by the help of the diagonal reflection
                    ##  property).
                    add_arrays(
                        .arr1 = .arr_ViVj,
                        .arr2 = Conj(.rev_arr_ViVj))
                }
            ##  Add lag-zero component, and store the final result.
            ..env[[spectra_cache]][[.node]][[.cut]] <- add_arrays(
                .arr1 = .res_ViVj,
                .arr2 = .lag_zero,
                keep_shape = TRUE)
        }
        kill(.cut, .arr_ViVj, .rev_arr_ViVj, .res_ViVj, .lag_zero)
    }
    kill(.node, .rev_dn, .sorted_ViVj, .rev_sorted_ViVj, .intermediate)
    ##  Update the stored content, so complex-valued entities can
    ##  be investigated by means of their real parts.
    for (.node in names(..env[[spectra_cache]]))
        for (.cut in names(..env[[spectra_cache]][[.node]])) {
            ..env[[spectra_cache]][[.node]][[.cut]] <-
                S_type_fun(x=..env[[spectra_cache]][[.node]][[.cut]],
                           .cut = .cut)
        }
    kill(.node, .cut)
    ##  Find the common range for all the available cuts, for both
    ##  the on- and off-diagonal cases.  Reminder: The procedure
    ##  applied for this is to collect the information from the
    ##  nodes first, and then merge that into a final result, with
    ##  the help of the 'eval'+'bquote+'.()'-construction.
    .ylim <- list()
    for (.node in names(..env[[spectra_cache]])) {
        my_abind_quote <- quote(my_abind())
        for (.cut in names(..env[[spectra_cache]][[.node]])) {
            my_abind_quote[[as.numeric(.cut) + 1]] <-
                bquote(..env[[spectra_cache]][[.node]][[.(.cut)]]$range)
        }
        .ylim[[.node]] <- eval(my_abind_quote)
    }
    kill(.node, .cut, my_abind_quote)
    ##  Finally, compute the common 'ylim'-value and add it to the
    ##  environment as an attribute.
    .find_ylim(spectra_cache)
    ##  The environment has been updated, so there is nothing that has
    ## to be returned.
    invisible(NULL)
}
