#' Compute norms, distances and angles
#' 
#' @param C1_env The environment containing the coefficients used for
#'     the first collection of spectra.  These coefficients will be
#'     scaled with the weights from \code{W1}.  Reminder: A copy of the
#'     \code{look_up}-argument must be added to this environment
#'     before this function is called!
#'
#' @param W1 A vector with the weights to be used for the scaling of
#'     the coefficients given in \code{C1_env}, i.e. the scaling that
#'     truncates the estimates at a given lag. It will always be
#'     assumed that \code{W1} is symmetric around the lag zero
#'     component, so only the positive lags are specified.  It is
#'     furthermore only necessary to specify the nonzero weights, so
#'     this vector can thus be shorter than the one given in
#'     \code{C1_env}.  If this argument is left unspecified, i.e. if
#'     the default value \code{NULL} is given, then it will be assumed
#'     that all the weights should be \code{1}.
#'
#' @param C2_env An optional environment that can be used when it is
#'     of interest to compute the distance between two families of
#'     spectra (i.e. the spectra based on coefficients and weights in
#'     \code{C1_env} and \code{W1}, and the spectra based on
#'     \code{C2_env} and \code{W2}).  The default value for
#'     \code{C2_env} is \code{NULL}.  Note that the structure of the
#'     content of this environment does not need to be identical to
#'     the one in \code{C1_env}, since the comparison will be based on
#'     the common components.  Reminder: If the target of interest is
#'     the distance of the spectra from white noise, then it is
#'     sufficient to 'take the square root of norm minus one' in the
#'     auto-correlation case.  For the cross-spectra it is sufficient
#'     to simply take the square root of the norm.  Reminder: A copy
#'     of the \code{look_up}-argument must be added to this
#'     environment before this function is called!
#'
#' @param W2 An optional vector with the weights to be used for the
#'     scaling of the coefficients given in \code{C2_env}.  The same
#'     assumptions occur for \code{W2} as those specified for
#'     \code{W1}.
#' 
#' @return The result of this function is a collection of norms,
#'     distances and angles related to the investigated spectra.  If
#'     both \code{C1_env} and \code{C2_env} are given, then the
#'     following four variations will be returned: C1-local
#'     vs. C1-global, C2-local vs. C2-global, C1-global vs. C2-global,
#'     and C1-local vs. C2-local.
#'
#' @keywords internal

LG_spectrum_norm <- function(C1_env=NULL,
                             W1=NULL,
                             C2_env=NULL,
                             W2=NULL) {
    ##  Check that 'C1_env' contains 'look_up'.
    if (is.null(C1_env$look_up)) {
        error(sprintf("The argument %s must contain a copy of %s.", 
                      sQuote("C1_env"),
                      sQuote("look_up")))
    } else {
        ##  Get hold of the desired content when needed.
        if (C1_env$look_up$TCS_type != "C")
            LG_shiny_correlation(look_up = C1_env$look_up,
                                 ..env = C1_env)
    }
    ##  Check that 'C2_env', when present, contains 'look_up'.
    if (!is.null(C2_env)) {
        if (is.null(C2_env$look_up)) {
            error(sprintf("The argument %s must contain a copy of %s.", 
                          sQuote("C2_env"),
                          sQuote("look_up")))
        } else  {
        ##  Get hold of the desired content when needed.
        if (C2_env$look_up$TCS_type != "C")
            LG_shiny_correlation(look_up = C2_env$look_up,
                                 ..env = C2_env)
        }
    }
    ###-------------------------------------------------------------------
    ##  Define several helper-functions, and then compute the desired
    ##  norms, distances and angles at the end.  The code is at times
    ##  somewhat messy since it is necessary to unfold the information
    ##  that was loaded from files.  Wrapping is used when positive
    ##  and negative lags coincide, and moreover the lag zero terms
    ##  are dropped for the univariate case (always equal to 1).  The
    ##  first helper function extracts the weighted coefficients.
    extract_coefficients <- function(.env, .W) {
        ## If '.W' has been left as 'NULL', then define it to be '1'
        ## for all the available auto/cross-correlations.  To match
        ## the structure in the ordinary defined setup, it is
        ## necessary to only define it for the positive lags.
        if (is.null(.W)) {
            .W <- array(
                data = rep(1, times = length(.env$look_up$lag_vec)),
                dim = length(.env$look_up$lag_vec),
                dimnames = list(lag = .env$look_up$lag_vec))
        }
        ##  Throw an error if '.W' goes beyond the content in '.env'.
        if (!all(as.numeric(names(.W)) %in% .env$look_up$lag_vec)) {
            error(sprintf(
                "Mismatch between weights and correlations. Weigths given up to lag %s, but the highest available lag is %s.",
                max(as.numeric(names(.W))),
                max(.env$look_up$lag_vec)),
                .argument = c(".W", ".env"))
        }
        .res <- list(
            is_bootstrap = .env$look_up$is_bootstrap,
            is_block = .env$look_up$is_block,
            local = list(is_local =  TRUE),
            global = list(is_local = FALSE))
        ##  Create a result-list with information from '.env$look_up'
        ##  that will be needed later on in the helper functions.
        .logical_values <- c(
            "is_negative_lags_needed",
            "is_lag_zero_needed",
            "is_cross_pair",
            "is_off_diagonal")
        for (i in .logical_values) {
            .res$local[[i]] <- .env$look_up[[i]]
        }
        ##  Create a copy of '.W' that can be adjusted to deal with
        ##  the different structures that occurs in the data that has
        ##  been stored for the global and local cases.  Reminder:
        ##  Keep the lag "0" case (always weight 1) as the first
        ##  entry, to simplify the code needed later on.
        .W_extended <- structure(
            .Data = as.array(c(1, .W, .W)),
            .Dimnames = list(lag = c("0",
                                     names(.W),
                                     paste("-",
                                           names(.W),
                                           sep = ""))))
        ##  Find suitable weights for the different cases.  For the
        ##  global case we do not need to wory about the issues
        ##  related to being on or off the diagonal.  Some logical
        ##  values will be stored for the global case, since they are
        ##  needed later on when the inner products between global and
        ##  local spectra are to be computed.
        if (.res$local$is_cross_pair) {
            ##  Cross-case, need lag zero and negative lags.
            .W_global <- .W_extended
            .res$global$is_negative_lags_needed <- TRUE
            .res$global$is_lag_zero_needed <- TRUE
        } else {
            ##  Auto-case, only positive lags needed.
            .W_global <- .W
            .res$global$is_negative_lags_needed <- FALSE
            .res$global$is_lag_zero_needed <- FALSE
        }
        ##  For the local case we need to take into account three
        ##  different cases.
        .W_local  <- 
            if (.res$local$is_cross_pair) {
                ##  Cross-case, need lag zero and negative lags.
                .W_extended
            } else if (.res$local$is_off_diagonal) {
                ##  Auto-case, but off-diagonal.  Need the negative
                ##  lags, but not lag zero.
                .W_extended[-1] 
            } else {
                ##  Auto-case and on-diagonal, only positive lags
                ##  needed.
                .W
            }
        ##  Extract the relevant parts of the local and global data.
        .res$global$data <- restrict_array(
            .arr = .env[[.env$look_up$cache$G_pairs]]$.data,
            .restrict= dimnames(.W_global))
        .res$local$data <- restrict_array(

            ##  This part does not play well for all the cases of
            ##  interest.  I guess what I actually would like to use
            ##  is the 'L_pairs' case, where it is simple to get outh
            ##  the desired wrapped version of the correlations in the
            ##  local Gaussian case.  The case with cross-correlation
            ##  for the global case must be dealt with later on...
            
            .arr = .env[[.env$look_up$cache$L_levels]]$.data,
            .restrict= c(dimnames(.W_local),
                         variable = "rho"),
            .drop = TRUE,
            .never_drop = c("lag", "content"))
        ##  Multiply with the weights in order to get hold of the
        ##  desired coefficients.
        .res$global$data <- multiply_arrays(
            .arr1 = .res$global$data,
            .arr2 = .W_global)
        .res$local$data <- multiply_arrays(
            .arr1 = .res$local$data,
            .arr2 = .W_local)
        ##  Return the result to the workflow.
        .res
    }
    ##################################################
    ##  A minor helper function for the computation of squared norms
    ##  and inner products.  This function expects other functions to
    ##  have taken care of the required restrictions of the arrays (in
    ##  order for the multiplication part to work), and it will
    ##  moreover sum over the content-dimension of the result.  Keep
    ##  in mind that further adjustments might be needed to adjust for
    ##  the potential folding and the presence of lag zero terms.  Note
    multiply_and_sum <- function(.arr1, .arr2=NULL) {
        if (is.null(.arr2)) {
            my_apply(
                X  = .arr1^2,
                MARGIN = "content",
                FUN = sum)
        } else {
            my_apply(
                X  = multiply_arrays(
                    .arr1 = .arr1,
                    .arr2 = .arr2),
                MARGIN = "content",
                FUN = sum)
        }
    }
    ##################################################
    ##  A helper function that computes the inner-product of the
    ##  coefficients from 'f1' and 'f2'.  The 'f1' and 'f2' can
    ##  e.g. be the global and local components extracted from
    ##  'C1_env', or one could be from 'C1_env' and one from 'C2_env'.
    inner_prod <- function(f1, f2) {
        ##  To cover the case when 'C2_env' is 'NULL', allow the
        ##  argument 'f2' to be '0'.
        if (identical(x= f2, y = 0))
            return(list(data = 0))
        ##  Identify parts that are compatible, and restrict the
        ##  attention to those.  Note that some additional care might
        ##  be required since the coefficients stored in 'f1' and 'f2'
        ##  might be wrapped in different ways.
        .dn_common <- dimnames_intersect(
            dimnames1 = dimnames(f1$data),
            dimnames2 = dimnames(f2$data))
        ##  Check if the arrays are compatible, i.e. that it makes
        ##  sense to compute the inner product.  Return an error if
        ##  'f1' and 'f2' fails to be compatible.
        if (length(.dn_common) != 2) {
            error(sprintf("The content of %s and %s fails to be compatible.",
                          sQuote("f1"),
                          sQuote("f2")),
                  .argument = c("f1", "f2"))
        }
        ##  Some compatibility has been detected, and we want to
        ##  perform the cross-multiplications followed by a sum along
        ##  the content-dimension.  It is here necessary to take into
        ##  account the potential differences that might be present
        ##  with regard to folding, and moreover also adjust for the
        ##  possibility that the lag zero terms might not be present
        ##  both places.  The simplest case is when neither 'f1' nor
        ##  'f2' contains the negative lags explicitly, since that
        ##  implies that the lag zero term in both cases are 1.
        if (all(isFALSE(f1$is_negative_lags_needed),
                isFALSE(f2$is_negative_lags_needed))) {
            ##  In this case: Multiply and sum the positive lags, then
            ##  multiply with 2 to account for the wrapping, and
            ##  finally add 1 for the product of the lag zero terms.
            res <- multiply_and_sum(
                .arr1 = restrict_array(
                    .arr = f1$data,
                    .restrict = .dn_common),
                .arr2 = restrict_array(
                    .arr = f2$data,
                    .restrict = .dn_common)) * 2 + 1
            ##  Return the result to the workflow.
            return(res)
        }
        ##  Next up is the case when the negative lags are needed for
        ##  both 'f1' and 'f2'.
        if (all(isTRUE(f1$is_negative_lags_needed),
                isTRUE(f2$is_negative_lags_needed))) {
            ##  In this case we need to check if the lag zero
            ##  component must be added separately, or if it is
            ##  already taken care of.  This requires a two step
            ##  process for this case.  Three cases can occur: Both
            ##  'f1' and 'f2' contain lag zero, nothing needs to be
            ##  added.  Neither 'f1' nor 'f2' contains lag zero, and
            ##  we need to add 1.  One of them contains lag zero, and
            ##  we need to extract that part in order to add it
            ##  separately.  Strategy: First compute the trivial part,
            ##  and then adjust as required.
            res <- multiply_and_sum(
                .arr1 = restrict_array(
                    .arr = f1$data,
                    .restrict = .dn_common),
                .arr2 = restrict_array(
                    .arr = f2$data,
                    .restrict = .dn_common))
            ##  Check if we need to adjust for missing lag zero terms.
            if (!all(isTRUE(f1$is_lag_zero_needed),
                     isTRUE(f2$is_lag_zero_needed))) {
                ##  We need to identify a lag zero component.  This
                ##  will be one if neither 'f1' nor 'f2' contains it,
                ##  otherwise it will be the lag zero components from
                ##  the one having it (since it will be multiplied
                ##  with a '1' from the part missing it).
                if (!any(isTRUE(f1$is_lag_zero_needed),
                         isTRUE(f2$is_lag_zero_needed))) {
                    zero_part <- 1
                } else {
                    ##  Extract the lag zero component, and perform a
                    ##  trivial sum in order to match the other
                    ##  component.
                    zero_part <- my_apply(
                        X = restrict_array(
                            .arr = if (f1$is_lag_zero_needed) {
                                       f1$data
                                   } else
                                       f2$data,
                            .restrict = list(lag = "0")),
                        MARGIN = "content",
                        FUN = sum)
                }
                ##  Update the result with the lag zero part.
                res <- res + zero_part
            }
            ##  Return the result to the workflow.
            return(res)
        }
        ##  If the program is still running at this point, then we
        ##  have a situation with one component that contain negative
        ##  lags and one that only contains the positive lags.  It
        ##  might here also be necessary to adjust for the
        ##  absence/presence of lag zero components.
        if (f2$is_negative_lags_needed) {
            ##  To simplify the treatment, ensure that 'f1' is the
            ##  component having the negative lags.
            temp <- f1
            f1 <- f2
            f2 <- temp
            kill(temp)
        }
        ##  We want to separately extract the positive and negative
        ##  coefficients from 'f1', and multiply these with
        ##  coefficients from 'f2'. After this we need to sum over the
        ##  content-dimension and add the lag-zero components.  For
        ##  this to work we also need to create an adjusted copy of
        ##  '.dn_common' suited for the extraction of the negative
        ##  lags.
        .dn_common_neg <- .dn_common
        .dn_common_neg$lag <- paste("-",
                                    .dn_common_neg$lag,
                                    sep = "")
        f2_restricted <- restrict_array(
            .arr = f2$data,
            .restrict = .dn_common)
        f1_pos_restricted <- restrict_array(
            .arr = f1$data,
            .restrict = .dn_common)
        f1_neg_restricted <- restrict_array(
            .arr = f1$data,
            .restrict = .dn_common_neg)
        ##  For the multiplication to work, we need to adjust the
        ##  names of the lags for the negative part.
        dimnames(f1_neg_restricted)$lag <- gsub(
                                       pattern  = "-",
                                       replacement="",
                                       x = dimnames(f1_neg_restricted)$lag)
        ##  Find the contribution from the positive and negative lags.
        pos_part <- multiply_and_sum(
            .arr1 = f1_pos_restricted,
            .arr2 = f2_restricted)
        neg_part <- multiply_and_sum(
            .arr1 = f1_neg_restricted,
            .arr2 = f2_restricted)
        ##  Find the contribution from the lag zero part.
        if (f1$is_lag_zero_needed) {
            ##  Extract the lag zero component, and perform a trivial
            ##  sum in order to match the other components.
            zero_part <- my_apply(
                X = restrict_array(
                    .arr = f1$data,
                    .restrict = list(lag = "0")),
                MARGIN = "content",
                FUN = sum)
        } else {
            zero_part <- 1
        }
        ## Return the final result to the workflow.
        neg_part + zero_part + pos_part
    }
    ##################################################
    ##  A helper function that computes the squared norm of the
    ##  coefficients from 'f1' (either the global or local part of the
    ##  coefficients extracted from 'C1_env' and 'C2_env').  This
    ##  function is superfluous since it could have been computed by
    ##  the help of the 'inner_prod'-helper defined above, but it is a
    ##  bit faster to compute it directly.
    L2_norm <- function(f) {
        ##  To cover the case when 'C2_env' is 'NULL', allow the
        ##  argument 'f' to be '0'.
        if (identical(x= f, y = 0))
            return(0)
        ##  Reminder: If only positive lags are present, then we need
        ##  to multiply the sum of squares with '2' and add '1' for
        ##  the lag zero component.  Note that we might need to adjust
        ##  for the lag zero component even if the negative lags are
        ##  present, e.g. the case for an investigation of an
        ##  auto-spectrum off the diagonal.
        res2 <- my_apply(
            X = f$data^2,
            MARGIN = "content",
            FUN = sum)
        if (f$is_negative_lags_needed) {
            ##  We might need to adjust for a missing lag zero term.
            if (! f$is_lag_zero_needed) 
                res2 <- res2 + 1
        } else {
            ##  We need to multiply by '2' due to the folding, and
            ##  furthermore add '1' for the lag zero term.
            res2 <- 2 * res2 + 1
        }
        ##  Return the result to the workflow.  Keep in mind that the
        ##  norm is the square root of the previously defined result.
        sqrt(res2)
    }
    ##################################################
    ##  Use the helper-functions defined above, and extract the
    ##  desired coefficients needed for the computation of the norms
    ##  and angles of interest.
    C1 <- extract_coefficients(.env=C1_env, .W=W1)
    if (!is.null(C2_env))
        C2 <- extract_coefficients(.env=C2_env, .W=W2)
    ##################################################
    ##  Some code that might better be taken care of some other place,
    ##  i.e. the need for the inclusion of a mean in the case where we
    ##  have blocks of simulated or bootstrapped data.  How to treat
    ##  this? I guess I might better create a copy containing only the
    ##  desired mean, since that is the solution that might be
    ##  required in a more general setting?
    ##### 
    ##  Create a helper function to take care of the mean-extraction.
    mean_of_extract <- function(.coeff) {
        ##  Update the '.coeff'-argument with the desired mean
        ##  component.  Reminder: Need to adjust the dimension-names
        ##  so the other helper functions can digest the result.
        for (.part in c("global", "local")) {
            ##  Adjustment needed if the coefficients originates from
            ##  a bootstrap investigation, since the "orig"-component
            ##  then must be dropped before computing the mean.
            .X <-
                if (.coeff$is_bootstrap) {
                    .not_orig <- which(dimnames(.coeff[[c(.part, "data")]])$content != "orig")
                    restrict_array(
                        .arr = .coeff[[c(.part, "data")]],
                        .restrict = list(content = .not_orig))
                } else{
                    .coeff[[c(.part, "data")]]
                }
            .tmp <- my_apply(
                X = .X,
                MARGIN = "lag",
                FUN = mean,
                .front = FALSE)
            .i <- which(names(dimnames(.tmp)) %in% "mean")
            names(dimnames(.tmp))[.i] <- "content"
            .coeff[[c(.part, "data")]] <- .tmp
        }
        ##  Return the result to the workflow.
        .coeff
    }
    mean1 <- mean_of_extract(C1)
    if (!is.null(C2_env))
        mean2 <- mean_of_extract(C2)
    ####-------------------------------------------------------------------
    ##  Create the node-result function, that in the end will be used
    ##  to create the content for the final list.
    .node_result <- function(f1, f2) {
        ##  Compute the desired inner product and norms.
        f1_inner_prod_f2 <- inner_prod(f1, f2)
        L2_norm_f1 <- L2_norm(f = f1)
        L2_norm_f2 <- L2_norm(f = f2)
        ##  Compute the distances and angles of interest. Reminder: It
        ##  can happen that the same spectral density is present both
        ##  places, in which case rounding errors might trigger tiny
        ##  negative value which again triggers 'NaN'-warnings from
        ##  'sqrt'.  A simple check corrects for this for tiny
        ##  negative numbers to 0, but lets larger negative number
        ##  pass since they could indicate an issue that should be
        ##  properly looked into.
        .tmp <- L2_norm_f1^2 - 2 * f1_inner_prod_f2 + L2_norm_f2^2
        if (any(.tmp < 0)) {
            .problems <- which(.tmp < 0)
            if (.tmp[.problems] > - 10 * .Machine$double.eps)
                .tmp[.problems] <- 0
        }
        f1_distance_f2 <- sqrt(.tmp)
        ##  The potential problem above can also trigger problems with
        ##  'acos', and a similar adjustment is thus included here.
        .tmp <- f1_inner_prod_f2/(L2_norm_f1 * L2_norm_f2)
        if (any(.tmp > 1)) {
            .problems <- which(.tmp > 1)
            if (.tmp[.problems] < 1 + 10 * .Machine$double.eps)
                .tmp[.problems] <- 1
        }
        f1_angle_f2 <- acos(.tmp)
        ##  Rteurn a matrix with the desired content as columns
        ##  vectors (to enable faster extraction).
        matrix(
            data = c(L2_norm_f1,
                     L2_norm_f2,
                     f1_inner_prod_f2,
                     f1_distance_f2,
                     f1_angle_f2),
            nrow = length(L2_norm_f1),
            ncol  = 5,
            dimnames = c(dimnames(f1$data)["content"],
                         list(value = c("L2_norm_f1",
                                        "L2_norm_f2",
                                        "f1_inner_prod_f2",
                                        "f1_distance_f2",
                                        "f1_angle_f2"))))
    }
    ###-------------------------------------------------------------------
    ##  Another helper function to deal with the mean-case.
    .the_diff_mean_case <- function(.full, .mean) {
        ##  Extend the 'content'-dimension of '.mean' to match that of
        ##  '.full', do this by some tweaking based on the
        ##  dimension-names.
        names(dimnames(.mean$data))[2] <- "tmp"
        .mean$data <- append_dimensions(
            orig_arr = .mean$data,
            added_dimnames = dimnames(.full$data)["content"])
        .node_result(f1 = .full, f2=.mean)
    }
    ##  Create an environment with all the combinations of the
    ##  results. Some redundancy in this, but so be it.
    .res <- new.env()
    ##  Stuff related to 'C1_env':
    .res$C1l_vs_C1g <- .node_result(f1 = C1$local, f2 = C1$global)
    ##  Add mean-based stuff when the input is based on more than one
    ##  single sample.
    if (any(unlist(C1[c("is_bootstrap", "is_block")]))) {
        .res$C1lm_vs_C1gm <- .node_result(f1 = mean1$local, f2 = mean1$global)
        .res$C1l_vs_C1lm <- .the_diff_mean_case(.full = C1$local, .mean = mean1$local)
        .res$C1g_vs_C1gm <- .the_diff_mean_case(.full = C1$global, .mean = mean1$global)
    }
    ##  Add more content when 'C2_env' is present.
    if (!is.null(C2_env)) {
        ##  Stuff related to 'C1_env':
        .res$C2l_vs_C2g <- .node_result(f1 = C2$local, f2 = C2$global)
        ##  Add mean-based stuff when the input is based on more than
        ##  one single sample.
        if (any(unlist(C2[c("is_bootstrap", "is_block")]))) {
            .res$C2lm_vs_C2gm <- .node_result(f1 = mean2$local, f2 = mean2$global)
            .res$C2l_vs_C2lm <- .the_diff_mean_case(.full = C2$local, .mean = mean2$local)
            .res$C2g_vs_C2gm <- .the_diff_mean_case(.full = C2$global, .mean = mean2$global)
        }
        ##  Add stuff that compares across 'C1_env' and 'C2_env'.
        .res$C1l_vs_C2l <- .node_result(f1 = C1$local, f2 = C2$local)
        .res$C1g_vs_C2g <- .node_result(f1 = C1$global, f2 = C2$global)
        if (any(unlist(C1[c("is_bootstrap", "is_block")]))) {
            .res$C2l_vs_C1lm <- .the_diff_mean_case(.full = C2$local, .mean = mean1$local)
            .res$C2g_vs_C1gm <- .the_diff_mean_case(.full = C2$global, .mean = mean1$global)
        }
        if (any(unlist(C2[c("is_bootstrap", "is_block")]))) {
            .res$C1l_vs_C2lm <- .the_diff_mean_case(.full = C1$local, .mean = mean2$local)
            .res$C1g_vs_C2gm <- .the_diff_mean_case(.full = C1$global, .mean = mean2$global)
        }
        if (all(any(unlist(C1[c("is_bootstrap", "is_block")])),
                any(unlist(C2[c("is_bootstrap", "is_block")])))) {
            .res$C1lm_vs_C2lm <- .node_result(f1 = mean1$local, f2 = mean2$local)
            .res$C1gm_vs_C2gm <- .node_result(f1 = mean1$global, f2 = mean2$global)
        }
    }
    ##  Return the result to the workflow.
    .res
}
