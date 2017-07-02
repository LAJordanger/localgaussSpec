################################################################################
#####  2016-02-11

#' Compute the local Gaussian spectral densities
#'
#' @template LG_approx_extract_data_arg
#' @template omega_vec_arg
#' @template omega_length_out_arg
#' @template window_arg
#' @template cut_vec_arg
#'
#' @return A list containing the information needed for
#'     \code{LG_shiny} to create plots of the local Gaussian auto- and
#'     cross-spectra.  The list will contain the components \code{Re}
#'     and \code{Im} that in essences gives the real and imaginary
#'     parts of the result.  The component \code{ccr_zero} contains
#'     the lag-zero contribution for the real part of the cross-
#'     spectra (the value will always be identical to \code{1} for the
#'     lag-zero contribution to the auto-spectra, and this is thus not
#'     stored to reduce the memory load.)
#'
#' @export

LG_spectra <- function(
    LG_approx_extract_data,
    omega_vec = NULL,
    omega_length_out = 2^6,
    window =
        c("Tukey", "Parzen", "Bartlett"),
    cut_vec = NULL) {
###-------------------------------------------------------------------
    ##  Enable an iterative approach to this computation, with the
    ##  twist that the top-level must be updated too.  The values
    ##  should be those in "LG_default$result$hierarchy[[1]]"
    .iterative <- TRUE
    ##  Create a shorthand for the default to be added when a node
    ##  should be left "empty".
    az <- LG_default$result$.azero
    ##  Use the iterative approach on the "top level".
    if (.iterative) {
        ##  Create a local copy of this function and revise its body
        ##  to avoid an infinite loop when calling it.
        fun_copy <- LG_spectra
        body(fun_copy)[[2]] <- quote(.iterative <- FALSE)
        ##  Compute the result when the local copy 'fun_copy' is used
        ##  on "extracted nodes" of the list 'LG_approx_extract_data',
        ##  with a setup that ensures that the required list strucutre
        ##  is present also when no computations are needed.  NB: Note
        ##  that attributes are added for the iterated function to
        ##  know which branch it is working upon.
        .tmp <- list(
            on_diag =
                if (! is.null(LG_approx_extract_data[["on_diag"]])) {
                    new_data <- LG_approx_extract_data[["on_diag"]]
                    attr(x = new_data, which = "on_diag") <- TRUE
                    LG_approx_extract_data[["on_diag"]] <- NULL
                    fun_copy(
                        LG_approx_extract_data = new_data,
                        omega_vec = omega_vec,
                        omega_length_out = omega_length_out,
                        window = window,
                        cut_vec = cut_vec)  
                } else
                    list(auto  = list(Re = az, negIm = az),
                         cross = list(Re = az, negIm = az)),
            off_diag =
                if (! is.null(LG_approx_extract_data[["off_diag"]])) {
                    new_data <- LG_approx_extract_data[["off_diag"]]
                    attr(x = new_data, which = "on_diag") <- FALSE
                    LG_approx_extract_data[["off_diag"]] <- NULL
                    fun_copy(
                        LG_approx_extract_data = new_data,
                        omega_vec = omega_vec,
                        omega_length_out = omega_length_out,
                        window = window,
                        cut_vec = cut_vec)
                } else
                    list(auto  = list(Re = az, negIm = az),
                         cross = list(Re = az, negIm = az)))
        kill(LG_approx_extract_data, omega_vec, omega_length_out,
             window, cut_vec, new_data, fun_copy, .iterative, az)
        ##  Add extra nodes with the computed cross-related spectra.
        ##  (NB: Only those arrays that requires computations will
        ##  contain anything else than placeholders, the others will
        ##  be computed on the fly later on.)
        for (.branch in LG_default$result$hierarchy$point)
            for (.type in LG_default$result$hierarchy$type) {
                .node <- c(.branch, .type)
                .tmp[[.node]] <- c(
                    .tmp[[.node]],
                    LG_auto_cross_computations(
                        .list_array = .tmp,
                        .branch = .branch,
                        .type = .type,
                        .spectrum = c("phase", "amplitude", "squared_coherence")))
            }
        kill(.branch, .type, .node)
        ##  Return the object with the added dimensional attributes
        ##  that describes the properties of the arrays.
        return(list_array_dims(.tmp))
    }
    kill(.iterative)
    ##  Below: The code that will work upon each of the nodes.
###-------------------------------------------------------------------
    ##  Extract information from 'LG_approx_extract_data' to make the
    ##  code later on easier to read.
    pos_lag <- LG_approx_extract_data$pos_lag
    lag_max <- LG_approx_extract_data$info$lag_max
    .on_diag <- attributes(LG_approx_extract_data)$on_diag
    kill(LG_approx_extract_data)
###-------------------------------------------------------------------
    ##  Extract relevant attributes from 'pos_lag'.
    .ignore <- c("dim", "dimnames", "class")
    .target <- ! names(attributes(pos_lag)) %in% .ignore
    .attr_pos_lag <- attributes(pos_lag)[.target]
    attributes(pos_lag) <- attributes(pos_lag)[! .target]
    kill(.ignore, .target)
###-------------------------------------------------------------------
    ##  Create `cut_vec' if none is given.
    if (is.null(cut_vec))
        cut_vec <- 1:(lag_max + 1)
###-------------------------------------------------------------------
    ##  Compare 'lag_max' with the highest value in 'cut_vec'.  If
    ##  'lag_max' is higher than needed (i.e. higher than
    ##  'max(cut_vec)-1'), then do a truncation in order to avoid
    ##  superfluous computations.
    if (lag_max > (max(cut_vec) - 1)) {
        lag_max <- max(cut_vec) - 1
        pos_lag <-
            restrict_array(
                .arr = pos_lag,
                .restrict = list(lag = 0:lag_max))
    }
###-------------------------------------------------------------------
    ##  Create a lag vector to be used later on.
    lag_vec <- 0:lag_max
    kill(lag_max)
###-------------------------------------------------------------------
    ##  Compute a list of windown-weight-vectors (stored as three
    ##  dimensional arrays).  Copies of this list will later on be
    ##  updated with the estimated values, before the nodes are
    ##  collected back into an array.
    .window_cut_list_array <- skeleton_list(
        names_list = list(window = window,
                          cut = cut_vec),
        add_names_list_as_attribute = FALSE)
    for (.window in window)
        for (.cut in cut_vec) {
            .window_cut_list_array[[c(.window, .cut)]] <-
                    structure(
                        .Data = if (.cut == 1) {
                                    1
                                } else
                                    myWindows[[.window]](
                                        .length = .cut - 1,
                                        cut = .cut),
                        .Dim = c(1, 1, .cut),
                        .Dimnames = list(window = window,
                                         cut = .cut,
                                         lag = lag_vec[1:.cut]),
                        class = "LG_array") 
        }
    kill(.window, .cut)
###-------------------------------------------------------------------
    ##  The frequency vector.  Use 'omega_vec' if specified, otherwise
    ##  create vector from 0 to 1/2 based on `omega_length_out'.
    if (! is.null(omega_vec)) {
        omega <- omega_vec
    } else {
        omega <- seq(from = 0, to = 1/2,
                     length.out = omega_length_out)
    }
    kill(omega_vec, omega_length_out)
###-------------------------------------------------------------------
    ##  Make a character-version of omega to be used as
    ##  dimension-names, and an updated version scaled with 2pi to be
    ##  used in the computations.
    omega_char <- as.character(omega)
    omega <- 2 * pi * omega
###-------------------------------------------------------------------
    ##  Combine 'omega' with `lag_vec' and create the matrices
    ##  `cos(omega*lag)', and `sin(omega*lag)' (the latter only when
    ##  cross-correlations are needed).
    cos_omega_h <- structure(
        .Data = cos(outer(X = lag_vec, Y = omega)),
        .Dimnames = list(lag = lag_vec,
                         omega = omega_char),
        class = LG_default$class$array)
    ##  The array with the sine-values is always needed for
    ## multivariate time series, and it is also needed for univariate
    ## time series when points are off the diagonal.
    if (any(! .on_diag, .attr_pos_lag$.multivariate_TS)) 
        sin_omega_h <- structure(
            .Data = sin(outer(X = lag_vec, Y = omega)),
            .Dimnames = list(lag = lag_vec,
                         omega = omega_char),
            class = LG_default$class$array)
    kill(omega)
###-------------------------------------------------------------------
    ##  Initiate the result to store the local Gaussian auto- and
    ##  cross-spectra.  For the cross-spectra it will be necessary to
    ##  store both a real and an imaginary part.  NB: The algorithm
    ##  used for the computation (using folding) counts the lag zero
    ##  cases twice, wich for the real part of the complex number
    ##  implies that an adjustment is needed at the end.  Thus, it is
    ##  required to store this for a bit.  Note that the auto-case can
    ##  be made a bit more compact since the value to be used always
    ##  will be '1' (per definition).
    .zero_lags <- list(
        acr = 1, ##  Always one (per definiton).
        ccr = if (.attr_pos_lag$.multivariate_TS) 
            local({
                .tmp <- restrict_array(
                    .arr = pos_lag,
                    .restrict = list(lag = "0",
                                     pairs = .attr_pos_lag$.bivariate_pairs))
                ##  Drop the 'lag' dimmension to enable the use of
                ##  'add_arrays' later on.
                adrop(x = .tmp,
                      drop = which(names(dimnames(.tmp)) == "lag"))
            }) ) 
    ##  Create a helper function to take care of the diagonal
    ##  reflection needed when dealing with the 'off-diagonal'
    ##  case. (No effect in the diagonal case).
    .diag_reflect <- function(.arr) {
        .levels <- dimnames(.arr)$levels
        .levels_splitted_reflected <-
            vapply(X = .levels,
                   FUN = function(x)
                       strsplit(x = x, split = "_")[[1]][c(2,1)],
                   FUN.VALUE = character(2))
        .reflected_levels <- apply(
            X = .levels_splitted_reflected,
            MARGIN = 2,
            FUN = paste,
            collapse = "_")
        ##  Sanity check that all the diagonal points are present.
        if (! all(.reflected_levels %in% .levels))
            error(.argument = "LG_approx_extract_data",
                  n = 2,
                  c("Missing diagonally reflected points!"))
        ##  Do "nothing" more if only diagonal points.
        if (all(.reflected_levels ==  .levels))
            return(.arr)
        ##  Reflect the levels in '.arr'.
        .arr <- restrict_array(
            .arr = .arr,
            .restrict = list(levels = .reflected_levels))
        ##  Rename the values to enable the array versions of
        ##  multiplication and addition.
        dimnames(.arr)$levels <- .levels
        ##  Return the result to the workflow.
        .arr        
    }
    .result <- list(
        ##  The auto-part: The computations can be simplified on the
        ##  diagonal, i.e. the imaginary part is always zero.
        auto =
            if (.on_diag) {
                list(
                    Re = 2 * restrict_array(
                                 .arr = pos_lag,
                                 .restrict = list(
                                     lag = as.character(lag_vec),
                                     pairs = .attr_pos_lag$.univariate_pairs)),
                    negIm = az)
            } else {
                local({
                    ##  Extract (and reflect) the components needed
                    ##  for the "folding" in the off-diagonal case of
                    ##  the univariate settting.
                    .levels_orig <- restrict_array(
                        .arr = pos_lag,
                        .restrict = list(
                            lag = as.character(lag_vec),
                            pairs = .attr_pos_lag$.univariate_pairs))
                    .levels_reflected <- .diag_reflect(
                        .arr = .levels_orig)
                    ##  Return the list to be the basis for the
                    ##  computations later on.
                    list(
                        Re = local({
                            ##  Add (since cosine an even function)
                            .tmp <- add_arrays(
                                .arr1 = .levels_reflected,
                                .arr2 = .levels_orig)
                            class(.tmp) <- LG_default$class$array
                            .tmp }),
                        negIm = local({
                            ##  Subtract (since sine an odd function).
                            .tmp <- add_arrays(
                                .arr1 = .levels_orig,
                                .arr2 = - .levels_reflected)
                            class(.tmp) <- LG_default$class$array
                            .tmp}))
                })
            },
        ##  The cross-part, when relevant - both real and imaginary
        ##  parts.  NOTE: This is stored as the real and imaginary
        ##  parts of the local Gaussian cross-spectrum.  The local
        ##  Gaussian cospectrum will thus be the real part, whereas
        ##  the local Gaussian quadrature spectrum will be minus the
        ##  imaginary part.  Reminder: For this case, the use of
        ##  'diag_reflect' on '.part_II' takes care of both the
        ##  on-diagonal and off-diagonal cases.
        cross =
            if (.attr_pos_lag$.multivariate_TS) {
                local({
                    ##  Extract the parts needed for the "folding".
                    .part_I <- restrict_array(
                        .arr = pos_lag,
                        .restrict = list(
                            lag = as.character(lag_vec),
                            pairs = .attr_pos_lag$.bivariate_pairs))
                    .part_II <- restrict_array(
                        .arr = pos_lag,
                        .restrict = list(
                            lag = as.character(lag_vec),
                            pairs = .attr_pos_lag$.bivariate_pairs_II))
                    ##  Diagonally reflect '.part_II'
                    .part_II <- .diag_reflect(.arr = .part_II)
                    ##  Adjust dimnames to enable the use of 'add_arrays'.
                    dimnames(.part_II)$pairs <- .attr_pos_lag$.bivariate_pairs
                    ##  Return the list to be the basis for the
                    ##  computations later on.
                    list(
                        Re = local({
                            ##  Add (since cosine an even function)
                            .tmp <- add_arrays(.arr1 = .part_II,
                                               .arr2 = .part_I)
                            class(.tmp) <- LG_default$class$array
                            .tmp }),
                        negIm = local({
                            ##  Subtract (since sine an odd function)
                            .tmp <- add_arrays(.arr1 = .part_I,
                                               .arr2 = - .part_II)
                            class(.tmp) <- LG_default$class$array
                            .tmp}))
                })
            } else
                list(Re = az,
                     negIm = az)
    )
    kill(pos_lag, .diag_reflect, lag_vec, az, .on_diag)
###-------------------------------------------------------------------
    ##  In order to use the recycle-the-shortest-vector principle, we
    ##  need to first extend the storage arrays in '.result' by adding
    ##  the frequencies 'omega' along the second dimension.  Then
    ##  compute the summands that the window-function(s) will work
    ##  upon.  Reminder: Use 'cos' for 'Re'-parts and 'sin' for
    ##  'Im'-parts.
#####  REMINDER: Somewhat outdated description...
    ##  Then extend the array to allow for a new
    ##  recycle-the-shortest-vector when multiplying with the
    ##  window-weights, for our purposes this means that we add the
    ##  cut-dimension and window-dimension at respectively position
    ##  two and three.  (UPDATE: The use of 'multiply_arrays' removes
    ##  the need for this requirement, but the code later on would
    ##  probably run slower if this is is ignored.) Then compute the
    ##  product with 'cut_window_lag_weights', and sum over the first
    ##  dimension (.e. 'lag') to get the desired result.  Then, add
    ##  the lag zero terms, which only is needed for the cases built
    ##  upon cosine (no point adding '0' for the sine).  Note that the
    ##  contribution for the 'auto'-part always (per definition) will
    ##  be '1' from the lag zero part.  Finally, modify dimensions and
    ##  dimension-names to reflect that we have computed different
    ##  estimates of the local Gaussian spectra.
    for (.type in LG_default$result$hierarchy$type)
        for (.part in c("Re", "negIm")) {
            .node <- c(.type, .part)
            if (! identical(x = .result[[.node]],
                            y = LG_default$result$.azero)) {
                .class <- class(.result[[.node]])
                ##  Prepare for recycling.
                .result[[.node]] <-
                    append_dimensions(
                        orig_arr = .result[[.node]],
                        added_dimnames  = list(omega = omega_char),
                        positions = 2)
                ## Compute the summands to be worked upon by the
                ## window-functions.
                .result[[.node]] <- multiply_arrays(
                    .arr1 = .result[[.node]],
                    .arr2 = if (.part == "Re") {
                                cos_omega_h
                            } else
                                sin_omega_h,
                    keep_shape = TRUE)
                ##  Add window and cut dimensions, in order for the
                ##  next 'multiply_arrays' command to work properly
                .orig_dimnames <- dimnames(.result[[.node]])
                dim(.result[[.node]]) <- c(1, 1, dim(.result[[.node]]))
                dimnames(.result[[.node]]) <- c(
                    list(window = "WINDOW", cut = "CUT"),
                    .orig_dimnames)
                ##  Perform the multiplications with the desired
                ##  window-weights for the different cut-values based
                ##  on '.window_cut_list_array'.  Strategy: First
                ##  update a copy of the list with the desired
                ##  estimates of the spectral densities, then collect
                ##  those results into arrays to be stored in
                ##  '.result[[.node]]'.  Create a copy of
                ##  '.window_cut_list_array'
                .window_cut_list_copy <- .window_cut_list_array
                for (.window in window) {
                    for (.cut in cut_vec) {
                        ##  Bookmark for window+cut
                        .window_cut <- c(.window, .cut)
                        ##  Update the dimnames of '.result[[.node]]'
                        ##  to enable product to work properly.
                        dimnames(.result[[.node]])[c("window", "cut")] <- 
                            dimnames(.window_cut_list_copy[[.window_cut]])[c("window", "cut")]
                        ##  The product step, temporary array.
                        .tmp <- multiply_arrays(
                            .arr1 = restrict_array(
                                .arr = .result[[.node]],
                                .restrict = dimnames(.window_cut_list_copy[[.window_cut]])["lag"]),
                            .arr2 = .window_cut_list_copy[[.window_cut]],
                            keep_shape = TRUE)
                        ##  The summation, that gives the estimate of
                        ##  the spectral density.  NB: Note the
                        ##  procedure that ensures that any dimensions
                        ##  of length one is keept.
                        .dims <- which(names(dimnames(.tmp)) == "lag")
                        .window_cut_list_copy[[.window_cut]] <- structure(
                            .Data = colSums(x = .tmp,
                                            dims = .dims),
                            .Dim = dim(.tmp)[-.dims],
                            .Dimnames = dimnames(.tmp)[-.dims],
                            class = class(.tmp))
                        kill(.tmp, .dims)
                    }
                    ##  Collect the cut-nodes at window-node level.
                    .window_cut_list_copy[[.window]] <- local({
                        .tmp <- do.call(
                            what = my_abind,
                            args = .window_cut_list_copy[[.window]])
                        class(.tmp) <- class(.window_cut_list_copy[[.window_cut]])
                        .tmp})
                }
                ##  Collect the window-nodes into the result node.
                .result[[.node]] <- local({
                    .tmp <- do.call(
                        what = my_abind,
                        args = .window_cut_list_copy)
                    class(.tmp) <- class(.window_cut_list_copy[[.window]])
                    .tmp})
                kill(.window_cut_list_copy)
                ##  Subtract the lag zero part for 'Re', since the
                ##  setup above included that part twice.
                if (all(.node == c("auto", "Re")))
                    .result[[.node]] <-
                        .result[[.node]] - .zero_lags$acr
                ##  Adjust the dimension and dimension-names.
                if (all(.node == c("cross", "Re"))) {
                    .result[[.node]] <- add_arrays(
                        .arr1 = .result[[.node]],
                        .arr2 = - .zero_lags$ccr)
                }
                .result[[.node]] <- structure(
                    .Data = .result[[.node]],
                    .Dim = c(1, dim(.result[[.node]])),
                    .Dimnames = c(list(spec = "spec"),
                                  dimnames(.result[[.node]])),
                    class = .class)
            }
        }
    kill(.type, .part, .node, .class, cut_vec, omega_char, window,
         cos_omega_h, sin_omega_h, .zero_lags)
###-------------------------------------------------------------------
    ##  Return the result to the work-flow together with the stuff
    ##  stored in 'attr_pos_lag'.
    c(.result,
      .attr_pos_lag)
}
