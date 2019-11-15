#' Computation of the spectra based on correlations.
#'
#' This helper function computes the spectra for a collection of
#' (local Gaussian) auto- and cross-correlations.  This function is an
#' internal function which is called from the functions that creates
#' the plots.
#'
#' @param .look_up A list created by \code{LG_lookup} (in the function
#'     \code{LG_plot_helper}), where the key details have been
#'     extracted (or constructed) from a (non-reactive copy) of the
#'     values defined by the \code{shiny}-interface.
#'
#' @param ..env The environment that contains the correlations, and
#'     also the environment in which the result will be stored.
#'
#' @return An update of \code{..env} will be performed with values
#'     needed for the plotting of the spectra.
#'
#' @keywords internal

LG_shiny_spectra  <- function(.look_up,
                              ..env)  {
    ##  Some shortcuts to get the code more compact.
    cache <- .look_up$cache
    .global_name <- .look_up$.global_name
    .local_name <- .look_up$.local_name
    ###-------------------------------------------------------------------
    ##  First: Some helper-functions, that is needed Later on.
    ##  Reminder: The (potentially) complex-valued spectra requires an
    ##  investigation by means of the Co-, Quad-, amplitude- and
    ##  phase-spectra.  This will be done by adding a new dimension
    ##  "S_type" to the arrays.  To get a unified treatment later on,
    ##  the dimensions for the real valued cases will be modified by
    ##  adding a length one "S_type"-dimension having the name "Co".
    ##  Moreover, the need for a common ylim for the different plots
    ##  will also be taken care of here - in particular, the ylims
    ##  computed here will be the one to be used if we want to keep
    ##  the given truncation level and see how things changes between
    ##  the points (given the specified selection of Vi and Vj).
    S_type_fun <- function(x) {
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
    ###-------------------------------------------------------------------
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
    ###-------------------------------------------------------------------    
    ##  The investigation of the spectra requires that an array
    ##  with the 'exp^{-2*pi*i*omega*h}'-values must be multiplied
    ##  with the available correlations (depends on bandwidth).
    ##  The '.exp'-array is computed first, and then in the next
    ##  part they are multiplied with the correlations.
    if (!exists(x = cache$.exp, envir = ..env)) {
        ..env[[cache$.exp]] <- structure(
            .Data = exp(outer(X = .look_up$lag_vec,
                              Y = -2i*pi*.look_up$omega_vec)),
            .Dimnames = list(lag = .look_up$lag_vec,
                             omega = .look_up$omega_vec),
            class = LG_default$class$array)
    }
    ###-------------------------------------------------------------------
    ##  Find the desired '.weights' to be used for the specified
    ##  windows functions.  Reminder: These depend on the "cut"-value,
    ##  which specifies the first term having a weight of zero.  The
    ##  adjustment below ensures that all the available correlations
    ##  can contribute to the estimates.  A "cut"-value of "1" implies
    ##  that we only have the lag-zero contribution left.  This is not
    ##  really interesting, but the corner case must still be treated
    ##  later on in order to avoid glitches during the interactive
    ##  investigation.
    if (!exists(x = cache$.weights, envir = ..env)) {
        ..env[[cache$.weights]] <- list()
        for (.cut in .look_up$lag_vec)
            ..env[[cache$.weights]][[as.character(.cut+1)]] <-
                myWindows[[.look_up$window]](.cut = .cut + 1) 
    }
    ##  Add the weight-version needed when we want to integrate the
    ##  spectral density function.
    if (!exists(x = cache$.weights_integral, envir = ..env)) {
        ..env[[cache$.weights_integral]] <- list()
        for (.cut in names(..env[[cache$.weights]])) {
            ..env[[cache$.weights_integral]][[.cut]] <-
                1i/(2*pi) * ..env[[cache$.weights]][[.cut]] /
                seq_along(..env[[cache$.weights]][[.cut]])
        }
    }
    ###-------------------------------------------------------------------
    ##  Compute the unweighted product of the '.exp'-array and the
    ##  estimated ordinary (global) correlations.
    if (!exists(x = cache$.spectra_summands_global, envir = ..env)) {
        .result <- list()
        for (.node in dimnames(..env[[.global_name]])$TS) {
            .result[[.node]] <- list()
            .corr <- restrict_array(
                .arr = ..env[[.global_name]],
                .restrict = list(TS = .node))
            .dn <- dimnames(.corr)
            ##  Extract the lag-zero component when required.
            ##  Reminder: In order for the addition of the arrays
            ##  to work properly later on, the "lag"-dimension
            ##  must be dropped.
            if ("0" %in% .dn$lag)  {
                .result[[.node]][["lag_zero"]] <-
                    restrict_array(
                        .arr = .corr,
                        .restrict = list(lag = "0"))
                .dn <- dimnames(.result[[.node]][["lag_zero"]])
                .lag_pos <- which(names(.dn) == "lag")
                dim(.result[[.node]][["lag_zero"]]) <-
                    dim(.result[[.node]][["lag_zero"]])[-.lag_pos]
                dimnames(.result[[.node]][["lag_zero"]]) <- .dn[-.lag_pos]
                kill(.lag_pos)
            }
            ##  Compute the desired product of the positive
            ##  lag-correlations and the '.exp'-values.
            .result[[.node]][["pos"]] <-
                multiply_arrays(
                    .arr1 = append_dimensions(
                        orig_arr = restrict_array(
                            .arr = .corr,
                            .restrict =
                                list(lag = as.character(.look_up$lag_vec))),
                        added_dimnames =
                            list(omega = as.character(.look_up$omega_vec))),
                    .arr2 = ..env[[cache$.exp]])
        }
        ..env[[cache$.spectra_summands_global]]  <- .result
        kill(.result, .node, .dn, .corr)
    }
    kill(.global_name)
    ###-------------------------------------------------------------------
    ##  Compute the global spectra,
    if (!exists(x = cache$.spectra_global, envir = ..env)) { 
        ##  Initiate the list-structure.
        ..env[[cache$.spectra_global]] <- list()
        ##  Compute a partial list with the sums based on the
        ##  contributions from the positive lags.  For the ordinary
        ##  auto-spectrum the symmetry ensures that this is all we
        ##  need, whereas the equality of '$\rho_{kl}(-h)$' and
        ##  '$\rho_{lk}(h)$' is used to access the negative lags
        ##  needed for the ordinary cross-spectrum. Remember to add
        ##  the values for the lag-zero contributions when required.
        .intermediate <- list()
        for (.cut in names(..env[[cache$.weights]])) {
            for (.node in names(..env[[cache$.spectra_summands_global]])) {
                if (is.null(..env[[cache$.spectra_summands_global]][[.node]]))
                    next
                ##  Extract the required part of the stored
                ##  correlation-values.
                .restrict <- list(
                    lag = as.character(1:(as.numeric(.cut)-1)),
                    pairs = unique(c(.look_up$pairs_ViVj,
                                     .look_up$pairs_VjVi)))
                .tmp <- restrict_array(
                    .arr =  ..env[[cache$.spectra_summands_global]][[.node]]$pos,
                    .restrict = .restrict)
                ##  Multiply correlations with weights.
                .tmp <- multiply_arrays(
                    .arr1 = .tmp,
                    ..env[[cache$.weights]][[.cut]])
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
        kill(.cut, .node, .restrict, .tmp, .dims)
        ###-------------------------------------------------------------------
        ##  Use symmetry-properties to compute the estimated spectra.
        ##  Reminder: The extracted part corresponding to the negative
        ##  lags must be complex conjugated when the sum for the
        ##  cross-spectrum is to be computed.
        ###-------------------------------------------------------------------
        ##  Identify the 'pairs'-values to be used in the
        ##  construction.  
        .sorted_ViVj <- sort(unlist(.look_up[c("Vi", "Vj")]))
        .rev_sorted_ViVj <- rev(.sorted_ViVj)
        .sorted_ViVj <- paste(.sorted_ViVj,
                              collapse = "_")
        .rev_sorted_ViVj <- paste(.rev_sorted_ViVj,
                                  collapse = "_")
        for (.node in names(.intermediate)) {
            if (is.null(.intermediate[[.node]]))
                next
            ##  Identify the available dimension-names, and prepare
            ##  the revised version needed later on when the sum of
            ##  positive and negative lags is to be taken.
            .dn <- dimnames(.intermediate[[.node]][[1]])
            .rev_dn <- .dn
            .rev_dn$pairs <- .sorted_ViVj
            ##  Find the contribution that must be added for the
            ##  lag-zero case.  This will always be one in the
            ##  auto-spectrum case (taken care of below), whereas it
            ##  must be extracted from the previously stored values in
            ##  the cross-spectrum case.
            .lag_zero <-
                if (.look_up$is_auto_pair) {
                    1
                } else
                    restrict_array(
                        .arr = ..env[[cache$.spectra_summands_global]][[.node]]$lag_zero,
                        .restrict = list(pairs = .sorted_ViVj))
            ##  Add the corner-case with "cut" equal to "1".  This must be
            ##  present in order to avoid glitches in the interactive
            ##  investigation.  The auto-spectra case requires some more
            ##  work in this case.  Reminder: Since we do have a constant
            ##  in this case, it is sufficient to use only the end-points
            ##  of the frequency range in the omega-dimension.
            ..env[[cache$.spectra_global]][[.node]][["1"]] <- 
                if (.look_up$is_auto_pair) {
                    ##  Adjust the dimension-names
                    .dn$omega  <- .look_up$frequency_range
                    .dn$pairs <- .sorted_ViVj
                    array(
                        data = 1,
                        dim = vapply(X = .dn,
                                     FUN = length,
                                     FUN.VALUE = numeric(1)),
                        dimnames = .dn)
                } else {
                    ##  Reminder: This case must be saved as
                    ##  complex-valued in order for tests later on
                    ##  (which uses the modes of the nodes) to treat
                    ##  it correctly.  NOTE: The 'as.complex'-function
                    ##  killed of the dimension-names when used on the
                    ##  case where the array only contained one value.
                    ##  The solution below circumvents that problem.
                    .lag_zero[] <- as.complex(.lag_zero)
                    append_dimensions(
                        orig_arr = .lag_zero,
                        added_dimnames = list(omega = .look_up$frequency_range))
                }
            kill(.dn)
            ##  Time to find the ordinary spectra, which will be real
            ##  valued for the auto-spectrum an complex valued for the
            ##  cross-spectrum
            for (.cut in names(.intermediate[[.node]])) {
                ##  Create the arrays to be added.
                if (.look_up$Vi == .look_up$Vj) {
                    ##  Only one array need for the auto-case.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                } else {
                    ##  The cross-case.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                    .rev_arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .rev_sorted_ViVj))
                    ##  Adjust dimension names to prepare for the sum
                    dimnames(.rev_arr_ViVj) <- .rev_dn
                }
                ##  Take the sum of the non-zero lags, keep in mind that a
                ##  simplification can be made for the auto-case on the
                ##  diagonal, since symmetry in that case implies that we
                ##  have a real-valued result.
                .res_ViVj <-
                    if  (.look_up$Vi == .look_up$Vj){
                        ## Symmetry simplifies this part
                        2 * Re(.arr_ViVj)
                    } else {
                        ##  Take the sum of the arrays. Recall that a
                        ##  complex conjugation is needed for the part
                        ##  corresponding to the negative lags.
                        add_arrays(
                            .arr1 = .arr_ViVj,
                            .arr2 = Conj(.rev_arr_ViVj))
                    }
                ##  Add the lag-zero component, store the result.
                ..env[[cache$.spectra_global]][[.node]][[.cut]] <- 
                    if  (.look_up$Vi == .look_up$Vj) {
                        ##  The auto case.
                        1 + .res_ViVj 
                    } else {
                        ##  The cross-case.
                        add_arrays(
                            .arr1 = .res_ViVj,
                            .arr2 = .lag_zero,
                            keep_shape = TRUE)
                    }
            }
            kill(.cut, .arr_ViVj, .rev_arr_ViVj, .res_ViVj,
                 .lag_zero)
        }
        kill(.node, .rev_dn, .sorted_ViVj, .rev_sorted_ViVj, .intermediate)
        ##  Update the stored content, so complex-valued entities can
        ##  be investigated by means of their real parts.
        for (.node in names(..env[[cache$.spectra_global]]))
            for (.cut in names(..env[[cache$.spectra_global]][[.node]])) {
                ..env[[cache$.spectra_global]][[.node]][[.cut]] <-
                    S_type_fun(x=..env[[cache$.spectra_global]][[.node]][[.cut]])
        }
        kill(.node, .cut)
        ##  Finally, compute the common 'ylim'-value and add it to the
        ##  environment as an attribute.
        .find_ylim(cache$.spectra_global)
    }


    ## capture_env()

    ###-------------------------------------------------------------------
    ##  Compute the collections of estimated integrated spectra (for
    ##  different lags) given the selected lag-window function.
    cache$.integrated_spectra_global <- "TESTING_INTEGRATION_GLOBAL"


    ###-------------------------------------------------------------------
    ##  Compute the global spectra,
    if (!exists(x = cache$.integrated_spectra_global, envir = ..env)) { 
        ##  Initiate the list-structure.
        ..env[[cache$.integrated_spectra_global]] <- list()
        ##  Compute a partial list with the sums based on the
        ##  contributions from the positive lags.  For the ordinary
        ##  auto-spectrum the symmetry ensures that this is all we
        ##  need, whereas the equality of '$\rho_{kl}(-h)$' and
        ##  '$\rho_{lk}(h)$' is used to access the negative lags
        ##  needed for the ordinary cross-spectrum. Remember to add
        ##  the values for the lag-zero contributions when required.
        .intermediate <- list()
        for (.cut in names(..env[[cache$.weights_integral]])) {
            for (.node in names(..env[[cache$.spectra_summands_global]])) {
                if (is.null(..env[[cache$.spectra_summands_global]][[.node]]))
                    next
                ##  Extract the required part of the stored
                ##  correlation-values.
                .restrict <- list(
                    lag = as.character(1:(as.numeric(.cut)-1)),
                    pairs = unique(c(.look_up$pairs_ViVj,
                                     .look_up$pairs_VjVi)))
                .tmp <- restrict_array(
                    .arr =  ..env[[cache$.spectra_summands_global]][[.node]]$pos,
                    .restrict = .restrict)
                ##  Multiply correlations with weights.
                .tmp <- multiply_arrays(
                    .arr1 = .tmp,
                    ..env[[cache$.weights_integral]][[.cut]])
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
        kill(.cut, .node, .restrict, .tmp, .dims)
        ###-------------------------------------------------------------------
        ##  Use symmetry-properties to compute the estimated spectra.
        ##  Reminder: The extracted part corresponding to the negative
        ##  lags must be complex conjugated when the sum for the
        ##  cross-spectrum is to be computed.
        ###-------------------------------------------------------------------
        ##  Identify the 'pairs'-values to be used in the
        ##  construction.  
        .sorted_ViVj <- sort(unlist(.look_up[c("Vi", "Vj")]))
        .rev_sorted_ViVj <- rev(.sorted_ViVj)
        .sorted_ViVj <- paste(.sorted_ViVj,
                              collapse = "_")
        .rev_sorted_ViVj <- paste(.rev_sorted_ViVj,
                                  collapse = "_")
        for (.node in names(.intermediate)) {
            if (is.null(.intermediate[[.node]]))
                next
            ##  Identify the available dimension-names, and prepare
            ##  the revised version needed later on when the sum of
            ##  positive and negative lags is to be taken.
            .dn <- dimnames(.intermediate[[.node]][[1]])
            .rev_dn <- .dn
            .rev_dn$pairs <- .sorted_ViVj
            ##  Find the contribution that must be added for the
            ##  lag-zero case.  This will always be one in the
            ##  auto-spectrum case (taken care of below), whereas it
            ##  must be extracted from the previously stored values in
            ##  the cross-spectrum case.

            .lag_zero <- local({
                ..omega <- matrix(
                    data = .look_up$omega_vec,
                    ncol = 1,
                    dimnames = list(omega = .look_up$omega_vec,
                                    pairs = .sorted_ViVj))
                if (.look_up$is_auto_pair) {
                    ..omega + .5
                } else {
                    .tmp <- restrict_array(
                        .arr = ..env[[cache$.spectra_summands_global]][[.node]]$lag_zero,
                        .restrict = list(pairs = .sorted_ViVj))
                    .tmp <- append_dimensions(
                        orig_arr = .tmp,
                        added_dimnames = list(omega = .look_up$omega_vec))
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
            



            
            ##  Add the corner-case with "cut" equal to "1".  This must be
            ##  present in order to avoid glitches in the interactive
            ##  investigation.  The auto-spectra case requires some more
            ##  work in this case.  Reminder: Since we do have a constant
            ##  in this case, it is sufficient to use only the end-points
            ##  of the frequency range in the omega-dimension.
            ..env[[cache$.integrated_spectra_global]][[.node]][["1"]] <- 
                if (.look_up$is_auto_pair) {
                    ##  Adjust the dimension-names
                    .dn$omega  <- .look_up$frequency_range
                    .dn$pairs <- .sorted_ViVj
                    array(
                        data = .dn$omega + .5,
                        dim = vapply(X = .dn,
                                     FUN = length,
                                     FUN.VALUE = numeric(1)),
                        dimnames = .dn)
                } else {
                    ##  Reminder: This case must be saved as
                    ##  complex-valued in order for tests later on
                    ##  (which uses the modes of the nodes) to treat
                    ##  it correctly.  NOTE: The 'as.complex'-function
                    ##  killed of the dimension-names when used on the
                    ##  case where the array only contained one value.
                    ##  The solution below circumvents that problem.
                    .lag_zero[] <- as.complex(.lag_zero)
                    ## Add the updated '.lag_zero' to this node.
                    .lag_zero
                }

            
            kill(.dn)
            ##  Time to find the ordinary spectra, which will be real
            ##  valued for the auto-spectrum an complex valued for the
            ##  cross-spectrum
            for (.cut in names(.intermediate[[.node]])) {
                ##  Create the arrays to be added.
                if (.look_up$Vi == .look_up$Vj) {
                    ##  Only one array need for the auto-case.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                } else {
                    ##  The cross-case.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                    .rev_arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .rev_sorted_ViVj))
                    ##  Adjust dimension names to prepare for the sum
                    dimnames(.rev_arr_ViVj) <- .rev_dn
                }
                ##  Take the sum of the non-zero lags, keep in mind that a
                ##  simplification can be made for the auto-case on the
                ##  diagonal, since symmetry in that case implies that we
                ##  have a real-valued result.
                .res_ViVj <-
                    if  (.look_up$Vi == .look_up$Vj){
                        ## Symmetry simplifies this part
                        2 * Re(.arr_ViVj)
                    } else {
                        ##  Take the sum of the arrays. Recall that a
                        ##  complex conjugation is needed for the part
                        ##  corresponding to the negative lags.
                        add_arrays(
                            .arr1 = .arr_ViVj,
                            .arr2 = Conj(.rev_arr_ViVj))
                    }
                ##  Add the lag-zero component, store the result.
                ..env[[cache$.integrated_spectra_global]][[.node]][[.cut]] <- {
                    add_arrays(
                        .arr1 = .res_ViVj,
                        .arr2 = .lag_zero)

                }
            }
            kill(.cut, .arr_ViVj, .rev_arr_ViVj, .res_ViVj,
                 .lag_zero)
        }
        kill(.node, .rev_dn, .sorted_ViVj, .rev_sorted_ViVj, .intermediate)
        ##  Update the stored content, so complex-valued entities can
        ##  be investigated by means of their real parts.
        for (.node in names(..env[[cache$.integrated_spectra_global]]))
            for (.cut in names(..env[[cache$.integrated_spectra_global]][[.node]])) {
                ..env[[cache$.integrated_spectra_global]][[.node]][[.cut]] <-
                    S_type_fun(x=..env[[cache$.integrated_spectra_global]][[.node]][[.cut]])
        }
        kill(.node, .cut)
        ##  Finally, compute the common 'ylim'-value and add it to the
        ##  environment as an attribute.
        .find_ylim(cache$.integrated_spectra_global)
    }

    



    
    ###-------------------------------------------------------------------
    ##  Compute the product of the '.exp'-array and the estimated
    ##  local Gaussian correlations for the given bandwidth.
    if (!exists(x = cache$.spectra_summands_local, envir = ..env)) {
        .result <- list()
        for (.node in names(..env[[.local_name]])) {
            if (is.null(..env[[.local_name]][[.node]]))
                next
            .result[[.node]] <- list()
            .corr <- restrict_array(
                .arr = ..env[[.local_name]][[.node]],
                .restrict = list(bw_points = .look_up$bw_points,
                                 variable = "rho"))
            ##  Extract the lag-zero component when required.
            ##  Reminder: In order for the addition of the arrays
            ##  to work properly later on, the "lag"-dimension
            ##  must be dropped.
            if (.look_up$is_lag_zero_included)  {
                .result[[.node]][["lag_zero"]] <-
                    restrict_array(
                        .arr = .corr,
                        .restrict = list(lag = "0"))
                .dn <- dimnames(.result[[.node]][["lag_zero"]])
                .lag_pos <- which(names(.dn) == "lag")
                dim(.result[[.node]][["lag_zero"]]) <-
                    dim(.result[[.node]][["lag_zero"]])[-.lag_pos]
                dimnames(.result[[.node]][["lag_zero"]]) <- .dn[-.lag_pos]
                kill(.lag_pos)
            }
            ##  Compute the desired product of the positive
            ##  lag-correlations and the '.exp'-values.
            .result[[.node]][["pos"]] <- 
                multiply_arrays(
                    .arr1 = append_dimensions(
                        orig_arr = restrict_array(
                            .arr = .corr,
                            .restrict =
                                list(lag = as.character(.look_up$lag_vec))),
                        added_dimnames =
                            list(omega = as.character(.look_up$omega_vec))),
                    .arr2 = ..env[[cache$.exp]])
        }
        ..env[[cache$.spectra_summands_local]]  <- .result
        kill(.result, .node, .dn, .corr)
    }
    kill(.local_name)
    ###-------------------------------------------------------------------
    ##  Compute the collections of estimated spectra (for different
    ##  lags) given the selected lag-window function.
    if (!exists(x = cache$.spectra_local, envir = ..env)) { 
        ##  Initiate the list-structure.
        ..env[[cache$.spectra_local]] <- list()
        ##  Compute a partial list with the sums based on the
        ##  contributions from the positive lags, and then use the
        ##  diagonal-reflection property to get hold of the values for
        ##  the negative lags too.  Take care to add the correct
        ##  values for the lag-zero contributions when required.
        .intermediate <- list()
        for (.cut in names(..env[[cache$.weights]])) {
            for (.node in names(..env[[cache$.spectra_summands_local]])) {
                if (is.null(..env[[cache$.spectra_summands_local]][[.node]]))
                    next
                ##  Extract the required part of the stored
                ##  correlation-values.
                .restrict <- list(
                    lag = as.character(1:(as.numeric(.cut)-1)),
                    pairs = unique(c(.look_up$pairs_ViVj,
                                     .look_up$pairs_VjVi)))
                .tmp <- restrict_array(
                    .arr =  ..env[[cache$.spectra_summands_local]][[.node]]$pos,
                    .restrict = .restrict)
                ##  Multiply correlations with weights.
                .tmp <- multiply_arrays(
                    .arr1 = .tmp,
                    ..env[[cache$.weights]][[.cut]])
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
        kill(.cut, .node, .restrict, .tmp, .dims)
        ##  REMINDER: Should update the part above so it can be done in
        ##  parallel.
        ###-------------------------------------------------------------------
        ##  Use the diagonal reflection symmetry to compute the estimated
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
        ###-------------------------------------------------------------------
        ##  Identify the 'pairs'-values to be used in the
        ##  construction.  
        .sorted_ViVj <- sort(unlist(.look_up[c("Vi", "Vj")]))
        .rev_sorted_ViVj <- rev(.sorted_ViVj)
        .sorted_ViVj <- paste(.sorted_ViVj,
                              collapse = "_")
        .rev_sorted_ViVj <- paste(.rev_sorted_ViVj,
                                  collapse = "_")
        for (.node in names(.intermediate)) {
            if (is.null(.intermediate[[.node]]))
                next
            ##  Identify the "reversed"-levels that is needed before the
            ##  sum can be taken.  Reminder "level" is the name used for
            ##  the storing of the points, in the format "p1_p2".
            .dn <- dimnames(.intermediate[[.node]][[1]])
            .rev_levels <- local({
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
            ##  Create the "reversed"-dimnames to be used on the negative
            ##  lags after the splitting into the different components.
            ##  (These are needed in order for the addition of the arrays
            ##  to work properly.)
            .rev_dn <- .dn
            .rev_dn$pairs <- .sorted_ViVj
            .rev_dn$levels <- .rev_levels
            kill(.rev_levels)
            ##  Find the contribution that must be added for the lag-zero
            ##  case.  This will always be one in the auto-spectrum case
            ##  (taken care of below), whereas it must be extracted from
            ##  the previously stored values in the cross-spectrum case.
            .lag_zero <-
                if (.look_up$is_auto_pair) {
                    1
                } else
                    restrict_array(
                        .arr = ..env[[cache$.spectra_summands_local]][[.node]]$lag_zero,
                        .restrict = list(pairs = .sorted_ViVj))
            ##  Add the corner-case with "cut" equal to "1".  This must be
            ##  present in order to avoid glitches in the interactive
            ##  investigation.  The auto-spectra case requires some more
            ##  work in this case.  Reminder: Since we do have a constant
            ##  in this case, it is sufficient to use only the end-points
            ##  of the frequency range in the omega-dimension.
            ..env[[cache$.spectra_local]][[.node]][["1"]] <- 
                if (.look_up$is_auto_pair) {
                    ##  Adjust the dimenison-names
                    .dn$omega  <- .look_up$frequency_range
                    .dn$pairs <- .sorted_ViVj
                    ##  Create the array, with the result stored as
                    ##  complex valued when we are off the diagonal,
                    ##  since the mode of the node later on will
                    ##  decide the desired outcome.
                    array(
                        data = ifelse(
                            test = .node == "off_diag",
                            yes  = 1 + 0i,
                            no   = 1),
                        dim = vapply(X = .dn,
                                     FUN = length,
                                     FUN.VALUE = numeric(1)),
                        dimnames = .dn)
                } else {
                    ##  Reminder: This case must be saved as
                    ##  complex-valued in order for tests later on
                    ##  (which uses the modes of the nodes) to treat
                    ##  it correctly.  NOTE: The 'as.complex'-function
                    ##  killed of the dimension-names when used on the
                    ##  case where the array only contained one value.
                    ##  The solution below circumvents that problem.
                    .lag_zero[] <- as.complex(.lag_zero)
                    append_dimensions(
                        orig_arr = .lag_zero,
                        added_dimnames = list(omega = .look_up$frequency_range))
                }
            kill(.dn)
            ##  Time to find the local Gaussian spectra, which will be
            ##  complex valued except for the auto-spectrum for points on
            ##  the diagonal.
            for (.cut in names(.intermediate[[.node]])) {
                ##  Create the arrays to be added.
                if (.look_up$Vi == .look_up$Vj) {
                    ##  Reminder: For the auto-case we only need two
                    ##  arrays when we are on the off-diagonal-node.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                    if (.node == "off_diag") {
                        ##  We need a copy of the first array, but
                        ##  with the revised naming on the levels.
                        .rev_arr_ViVj <- .arr_ViVj
                        dimnames(.rev_arr_ViVj) <- .rev_dn
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
                ##  simplification can be made for the auto-case on the
                ##  diagonal, since symmetry in that case implies that we
                ##  have a real-valued result.
                .res_ViVj <-
                    if  (all(.look_up$Vi == .look_up$Vj,
                             .node == "on_diag")) {
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
                ##  Add the lag-zero component, store the result.
                ..env[[cache$.spectra_local]][[.node]][[.cut]] <- 
                    if  (.look_up$Vi == .look_up$Vj) {
                        ##  The auto case.
                        1 + .res_ViVj 
                    } else {
                        ##  The cross-case
                        add_arrays(
                            .arr1 = .res_ViVj,
                            .arr2 = .lag_zero)
                    }
            }
            kill(.cut, .arr_ViVj, .rev_arr_ViVj, .res_ViVj,
                 .lag_zero)
        }
        kill(.node, .rev_dn, .sorted_ViVj, .rev_sorted_ViVj, .intermediate)
        ##  Update the stored content, so complex-valued entities can
        ##  be investigated by means of their real parts.
        for (.node in names(..env[[cache$.spectra_local]]))
            for (.cut in names(..env[[cache$.spectra_local]][[.node]])) {
                ..env[[cache$.spectra_local]][[.node]][[.cut]] <-
                    S_type_fun(..env[[cache$.spectra_local]][[.node]][[.cut]])
            }
        kill(.node, .cut)
        ##  Find the common range for all the available cuts, for both
        ##  the on- and off-diagonal cases.  Reminder: The procedure
        ##  applied for this is to collect the information from the
        ##  nodes first, and then merge that into a final result, with
        ##  the help of the 'eval'+'bquote+'.()'-construction.
        .ylim <- list()
        for (.node in names(..env[[cache$.spectra_local]])) {
            my_abind_quote <- quote(my_abind())
            for (.cut in names(..env[[cache$.spectra_local]][[.node]])) {
                my_abind_quote[[as.numeric(.cut) + 1]] <-
                    bquote(..env[[cache$.spectra_local]][[.node]][[.(.cut)]]$range)
            }
            .ylim[[.node]] <- eval(my_abind_quote)
        }
        kill(.node, .cut, my_abind_quote)
        ##  Finally, compute the common 'ylim'-value and add it to the
        ##  environment as an attribute.
        .find_ylim(cache$.spectra_local)
    }


    ################################################################################
    ## The code for the spectral density (above), and the code for the
    ## integrated spectral (below) might be possible to unify by
    ## introducing a new helper function — but it seems premature to
    ## aim for such an adjustment at the moment.

    ###-------------------------------------------------------------------
    ##  Compute the collections of estimated integrated spectra (for
    ##  different lags) given the selected lag-window function.
    cache$.integrated_spectra_local <- "TESTING_INTEGRATION_LOCAL"
    
    
    if (!exists(x = cache$.integrated_spectra_local, envir = ..env)) { 
        ##  Initiate the list-structure.
        ..env[[cache$.integrated_spectra_local]] <- list()


        
        ##  Compute a partial list with the sums based on the
        ##  contributions from the positive lags, and then use the
        ##  diagonal-reflection property to get hold of the values for
        ##  the negative lags too.  Take care to add the correct
        ##  values for the lag-zero contributions when required.
        .intermediate <- list()
        for (.cut in names(..env[[cache$.weights_integral]])) {
            for (.node in names(..env[[cache$.spectra_summands_local]])) {
                if (is.null(..env[[cache$.spectra_summands_local]][[.node]]))
                    next
                ##  Extract the required part of the stored
                ##  correlation-values.
                .restrict <- list(
                    lag = as.character(1:(as.numeric(.cut)-1)),
                    pairs = unique(c(.look_up$pairs_ViVj,
                                     .look_up$pairs_VjVi)))
                .tmp <- restrict_array(
                    .arr =  ..env[[cache$.spectra_summands_local]][[.node]]$pos,
                    .restrict = .restrict)
                ##  Multiply correlations with weights.
                .tmp <- multiply_arrays(
                    .arr1 = .tmp,
                    ..env[[cache$.weights_integral]][[.cut]])
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
        kill(.cut, .node, .restrict, .tmp, .dims)
        ##  REMINDER: Should update the part above so it can be done in
        ##  parallel.
        ###-------------------------------------------------------------------
        ##  Use the diagonal reflection symmetry to compute the estimated
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
        ###-------------------------------------------------------------------
        ##  Identify the 'pairs'-values to be used in the
        ##  construction.  
        .sorted_ViVj <- sort(unlist(.look_up[c("Vi", "Vj")]))
        .rev_sorted_ViVj <- rev(.sorted_ViVj)
        .sorted_ViVj <- paste(.sorted_ViVj,
                              collapse = "_")
        .rev_sorted_ViVj <- paste(.rev_sorted_ViVj,
                                  collapse = "_")
        for (.node in names(.intermediate)) {
            if (is.null(.intermediate[[.node]]))
                next
            ##  Identify the "reversed"-levels that is needed before the
            ##  sum can be taken.  Reminder "level" is the name used for
            ##  the storing of the points, in the format "p1_p2".
            .dn <- dimnames(.intermediate[[.node]][[1]])
            .rev_levels <- local({
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
            ##  Create the "reversed"-dimnames to be used on the negative
            ##  lags after the splitting into the different components.
            ##  (These are needed in order for the addition of the arrays
            ##  to work properly.)
            .rev_dn <- .dn
            .rev_dn$pairs <- .sorted_ViVj
            .rev_dn$levels <- .rev_levels
            kill(.rev_levels)
            ##  Find the contribution that must be added for the
            ##  lag-zero case.  This will always be one in the
            ##  auto-spectrum case (taken care of below), whereas it
            ##  must be extracted from the previously stored values in
            ##  the cross-spectrum case.  For the investigation of the
            ##  integral of the spectral density, we also need to
            ##  include the frequency-dimension.
            .lag_zero <- local({
                ..omega <- matrix(
                    data = .look_up$omega_vec,
                    ncol = 1,
                    dimnames = list(omega = .look_up$omega_vec,
                                    pairs = .sorted_ViVj))
                if (.look_up$is_auto_pair) {
                    ..omega + .5
                } else {
                    .tmp <- restrict_array(
                        .arr = ..env[[cache$.spectra_summands_local]][[.node]]$lag_zero,
                        .restrict = list(pairs = .sorted_ViVj))
                    .tmp <- append_dimensions(
                        orig_arr = .tmp,
                        added_dimnames = list(omega = .look_up$omega_vec))
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
            
            
#####!!!!!!!!!!!!!!!
            ##  For the integrated version of the real-valued case, I
            ##  think it should be sufficient to multiply with the
            ##  values found in the frequency-vector
            ##  '.look_up$omega_vec', but I will need to check the
            ##  details later on first.
            
#####!!!!!!!!!!!!!!!
            
            ##  Add the corner-case with "cut" equal to "1".  This
            ##  must be present in order to avoid glitches in the
            ##  interactive investigation.  The auto-spectra case
            ##  requires some more work in this case.  Reminder: Since
            ##  the graph in this case will be a straight line, it is
            ##  sufficient to use only the end-points of the frequency
            ##  range in the omega-dimension.
            ..env[[cache$.integrated_spectra_local]][[.node]][["1"]] <- 
                if (.look_up$is_auto_pair) {
                    ##  Adjust the dimension-names
                    .dn$omega  <- .look_up$frequency_range
                    .dn$pairs <- .sorted_ViVj
                    ##  Create the array, with the result stored as
                    ##  complex valued when we are off the diagonal,
                    ##  since the mode of the node later on will
                    ##  decide the desired outcome.
                    array(
                        data = if (.node == "off_diag") {
                                   0.5 + .dn$omega + 0i
                               } else
                                   0.5 + .dn$omega,
                        dim = vapply(X = .dn,
                                     FUN = length,
                                     FUN.VALUE = numeric(1)),
                        dimnames = .dn)
                } else {
                    ##  Reminder: This case must be saved as
                    ##  complex-valued in order for tests later on
                    ##  (which uses the modes of the nodes) to treat
                    ##  it correctly.  NOTE: The 'as.complex'-function
                    ##  killed of the dimension-names when used on the
                    ##  case where the array only contained one value.
                    ##  The solution below circumvents that problem.
                    ###  SOME ISSUE HERE: Dimensionnames lost?
                    .lag_zero[] <- as.complex(.lag_zero)

                    ## Add the updated '.lag_zero' to this node.
                    .lag_zero
                    
                    ## ## ## append_dimensions(
                    ## ## ##     orig_arr = .lag_zero,
                    ## ## ##     added_dimnames = list(omega = .look_up$frequency_range))
                }
            kill(.dn)
            ##  Time to find the local Gaussian spectra, which will be
            ##  complex valued except for the auto-spectrum for points on
            ##  the diagonal.
            for (.cut in names(.intermediate[[.node]])) {
                ##  Create the arrays to be added.
                if (.look_up$Vi == .look_up$Vj) {
                    ##  Reminder: For the auto-case we only need two
                    ##  arrays when we are on the off-diagonal-node.
                    .arr_ViVj <- restrict_array(
                        .arr = .intermediate[[.node]][[.cut]],
                        .restrict = list(pairs = .sorted_ViVj))
                    if (.node == "off_diag") {
                        ##  We need a copy of the first array, but
                        ##  with the revised naming on the levels.
                        .rev_arr_ViVj <- .arr_ViVj
                        dimnames(.rev_arr_ViVj) <- .rev_dn
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
                ##  simplification can be made for the auto-case on the
                ##  diagonal, since symmetry in that case implies that we
                ##  have a real-valued result.
                .res_ViVj <-
                    if  (all(.look_up$Vi == .look_up$Vj,
                             .node == "on_diag")) {
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
                
                ##  Add the lag-zero component, store the result.
                ##  Note that we for the investigation of the integral
                ##  of the spectral density no longer need to
                ##  distinguish between auto- and cross-cases.
                ..env[[cache$.integrated_spectra_local]][[.node]][[.cut]] <- {
                    add_arrays(
                        .arr1 = .res_ViVj,
                        .arr2 = .lag_zero)
                }
            }
            kill(.cut, .arr_ViVj, .rev_arr_ViVj, .res_ViVj,
                 .lag_zero)
        }
        kill(.node, .rev_dn, .sorted_ViVj, .rev_sorted_ViVj, .intermediate)
        ##  Update the stored content, so complex-valued entities can
        ##  be investigated by means of their real parts.
        for (.node in names(..env[[cache$.integrated_spectra_local]]))
            for (.cut in names(..env[[cache$.integrated_spectra_local]][[.node]])) {
                ..env[[cache$.integrated_spectra_local]][[.node]][[.cut]] <-
                    S_type_fun(..env[[cache$.integrated_spectra_local]][[.node]][[.cut]])
            }
        kill(.node, .cut)
        ##  Find the common range for all the available cuts, for both
        ##  the on- and off-diagonal cases.  Reminder: The procedure
        ##  applied for this is to collect the information from the
        ##  nodes first, and then merge that into a final result, with
        ##  the help of the 'eval'+'bquote+'.()'-construction.
        .ylim <- list()
        for (.node in names(..env[[cache$.integrated_spectra_local]])) {
            my_abind_quote <- quote(my_abind())
            for (.cut in names(..env[[cache$.integrated_spectra_local]][[.node]])) {
                my_abind_quote[[as.numeric(.cut) + 1]] <-
                    bquote(..env[[cache$.integrated_spectra_local]][[.node]][[.(.cut)]]$range)
            }
            .ylim[[.node]] <- eval(my_abind_quote)
        }
        kill(.node, .cut, my_abind_quote)
        ##  Finally, compute the common 'ylim'-value and add it to the
        ##  environment as an attribute.
        .find_ylim(cache$.integrated_spectra_local)
    }


    ## ##  TEMP: Do some tweaking to see if any decent plots might be
    ## ##  made from the integrated data...

    ## ..env[[cache$.spectra_local]] <- ..env[[cache$.integrated_spectra_local]]

    ## ..env[[cache$.spectra_global]] <- ..env[[cache$.integrated_spectra_global]]
    


################################################################################    

    ## ##  Reminder: The heatmap-based plot is based on the structure
    ## ##  above, so I guess it might be at this position it would be
    ## ##  natural to include the desired 
    ## if (.look_up$m_selected == 10)
    ##     capture_env()

    
    ###-------------------------------------------------------------------
    ##  Extract data from 'cache$.spectra_global' and
    ##  'cache$.spectra_local'.  The idea is to use subsetting to
    ##  get hold of a two dimensional array with the "omega" and
    ##  "content" dimensions.  A minor help-function is needed in
    ##  order to deal with the actual computations.
    .CI_helper <- function(.arr) {
        ##  Check if any confidence intervals should be computed,
        ##  if not simply return '.arr' back to the workflow.
        if  (!.look_up$is_CI_needed)
            return(.arr)
        ##  Still running? If so, keep in mind the difference
        ##  between the bootstrap case and the simulated blocks.
        ##  In the former, the content-component "orig" must be
        ##  treated separately.
        if (.look_up$is_bootstrap) {
            .arr_original <- restrict_array(
                .arr = .arr,
                .restrict = list(content = "orig"))
            .arr <- restrict_array(
                .arr = .arr,
                .restrict = list(content = setdiff(
                                     x = dimnames(.arr)$content,
                                     y = "orig")))
        } else 
            .arr_original <- NULL
        ##  Return an array with the desired confidence
        ##  intervals, the min-max values and the median (and
        ##  "orig" for the bootstrap case).  The new values are
        ##  computed by the help of 'LG_boot_statistics' (which I
        ##  guess better be renamed in the revised approach).
        ##  Reminder: More statistics could be computed from this,
        ##  but the present interface does not deal with that yet
        ##  (and most likely never will).
        newnames <- LG_boot_statistics(names_only = TRUE)$content
        my_abind(
            .arr_original,
            plyr::aaply(
                      .data = .arr,
                      .margins = which(names(dimnames(.arr))=="omega"),
                      .fun = function(x)
                          array(data = LG_boot_statistics(x = x),
                                dim = c(length(newnames)),
                                dimnames = list(content = newnames)),
                      .drop = FALSE))
    }
    ###-------------------------------------------------------------------
    ##  The next helper extracts the relevant part of the range, with
    ##  some minor tweaks in order to get out the desired stuff.
    .ylim <- function(global_local, spectra_type) {
        ##  Identify the relevant source.
        .source <- switch(
            EXPR = global_local,
            global = cache$.spectra_global,
            local  = cache$.spectra_local)
        ##  Find the list with the range-values
        .ylim_node <- ..env[[.source]]$ylim
        ##  Extract into one vector the relevant 'min'- and
        ##  'max'-values from the different nodes, and then use the
        ##  range of that vector to find the overall desired limits.
        .min_max <- c()
        for (.node in names(.ylim_node))
            for (.mm in c("min", "max")) {
                if (spectra_type %in% dimnames(.ylim_node[[.node]])$S_type)
                    .min_max <- c(.min_max,
                                  restrict_array(
                                      .arr = .ylim_node[[.node]],
                                      .restrict = list(S_type = spectra_type,
                                                       range = .mm)))
            }
        ##  The range is not always equal to the desired 'ylim'-value
        ##  and some tweaks are thus included to ensure that we get
        ##  the desired output.
        switch(
            EXPR = spectra_type,
            Co = range(0, .min_max),
            Quad = range(0, .min_max),
            amplitude = range(0, .min_max),
            phase = c(-pi, pi))
    }
    ##  The content of the resulting lists (based on the setup below)
    ##  will in some cases require a change of sign.  The next helper
    ##  takes care of that.
    .change_sign <- function(.cache_code) {
        if (all(.look_up$is_adjust_sign,
                .look_up$spectra_type %in% c("Quad", "phase"))) {
            ..env[[.cache_code]]$.data <- 
                -1 * ..env[[.cache_code]]$.data
            ..env[[.cache_code]]$.ylim <-
                -1 * rev(..env[[.cache_code]]$.ylim)
        }   
    }
    ###-------------------------------------------------------------------
    ##  Read the relevant data for the two cases and perform the
    ##  computations.  Reminder: The idea is to construct a bookmark
    ##  '.bm' that extracts the desired node from the main list, and
    ##  then use 'restrict_array' to extract the desired part for the
    ##  present investigation.  From this stage on it should no longer
    ##  be necessary to keep track of the length one dimensions, and
    ##  these will henceforth be dropped in order to extract a two
    ##  dimensional array with the dimensions "omega" and "content".
    if  (!exists(x = cache$.CI_global, where = ..env)) {
        .bm <- c("TS_original", .look_up$cut, "spec")
        .restrict <- list(S_type = .look_up$spectra_type)
        ##  When the target of interest is to consider a local
        ##  Gaussian auto-spectrum for a point off the diagonal, then
        ##  it is necessary to tweak the stored data a bit here.
        .arr <- ..env[[cache$.spectra_global]][[.bm]]
        if (all(.look_up$is_auto_pair,
                .look_up$spectra_type != "Co")) {
            if (.look_up$spectra_type == "amplitude")
                .arr <- Mod(.arr) 
            if (.look_up$spectra_type == "Quad")
                .arr[] <- 0
            if (.look_up$spectra_type == "phase")
                .arr <-  Arg(.arr)
            dimnames(.arr)$S_type <- .look_up$spectra_type
            ##  Adjustments for the 'ylim'-case.
            ..ylim <-
                if (.look_up$spectra_type == "amplitude") {
                    range(0, abs(.ylim(global_local = "global",
                                       spectra_type = "Co")))
                } else {
                    0
                }
        } else {
            ..ylim <- .ylim(global_local = "global",
                           spectra_type = .look_up$spectra_type)
        }
        ##  Find the desired array.
        .tmp <- restrict_array(
            .arr = .arr,
            .restrict = .restrict,
            .drop = TRUE,
            .never_drop = c("omega", "content"))
        ..env[[cache$.CI_global]] <- list(
            .data = .CI_helper(.arr = .tmp),
            .ylim = ..ylim)
        kill(.bm, .restrict, .tmp, ..ylim)
        ##  Adjust the sign when required.
        .change_sign(cache$.CI_global)
    }
    if (!exists(x = cache$.CI_local, where = ..env)) {
        .bm <- .look_up$.bm_CI_local_spectra
        ## .restrict <- list(S_type = .look_up$spectra_type,
        ##                   levels = .look_up$levels_point)
        .restrict <- list(S_type = .look_up$spectra_type,
                          levels = ifelse(
                              test = .look_up$is_adjust_sign,
                              yes  = .look_up$levels_point_reflected,
                              no   = .look_up$levels_point))
        ##  An adjustment is needed for the univariate case when an
        ##  on-diagonal point is investigated by the framework where
        ##  the horizontal and vertical components can be adjusted
        ##  separately.  The reason for this adjustment is that the
        ##  computations earlier on then only have computed the
        ##  "Co"-part, since the result will be real-valued.
        .arr <- ..env[[cache$.spectra_local]][[.bm]]
        if (all(.look_up$is_auto_pair,
                length(dimnames(.arr)$S_type) == 1,
                .look_up$spectra_type != "Co")) {
            if (.look_up$spectra_type == "amplitude")
                .arr <- Mod(.arr) 
            if (.look_up$spectra_type == "Quad")
                .arr[] <- 0
            if (.look_up$spectra_type == "phase")
                .arr <-  Arg(.arr)
            dimnames(.arr)$S_type <- .look_up$spectra_type
            ##  The adjustments of 'ylim' for the three cases
            ##  encountered here.
            ..ylim <- switch(
                EXPR = .look_up$spectra_type,
                amplitude = range(0, abs(.ylim(global_local = "local",
                                               spectra_type = "Co"))),
                phase = c(-pi, pi),
                Quad = .ylim(global_local = "local",
                             spectra_type = "Quad"))
        } else {
            ..ylim <- .ylim(global_local = "local",
                            spectra_type = .look_up$spectra_type)
        }
        .tmp <- restrict_array(
            .arr = .arr,
            .restrict = .restrict,
            .drop = TRUE,
            .never_drop = c("omega", "content"))
        ..env[[cache$.CI_local]] <- list(
            .data = .CI_helper(.arr = .tmp),
            .ylim = ..ylim)
        kill(.bm, .restrict, .tmp)
        ##  Adjust the sign when required.
        .change_sign(cache$.CI_local)
    }
    ##  Nothing to return from this function, since the environment
    ##  '..env' now contains all the desired content.
    invisible(NULL)
}
