#' A list with the lag-window functions that is used to smooth the
#' estimated spectral densities.
#'
#' At the moment only three alternatives are present: "Tukey",
#' "Parzen" and "Bartlett".
#'
#' The \code{.cut}-argument that is common for all of the lag-window
#' functions is the first lag that is given the weight zero.  Note
#' that the truncation level \code{m} that is used for the truncated
#' spectra refers to the last non-zero lag, which implies that the
#' code later on must adjust for this difference.
#'
#' @keywords internal

myWindows <- list(
    ##  Due to symmetry, only necessary to create non-negative part,
    ##  and since the first value (the weight for lag zero) always are
    ##  1 that one will also be dropped from this investigation.
    ##  Moreover, the case 'cut' equal to '1' will always be treated
    ##  in a separate manner later on in the code, so I do not worry
    ##  about the fact that that value creates an error.
    Tukey = function(.cut) {
        ##  Use the 'Tukey'-window for the computation.
        .all <- pi/.cut * 1:(.cut-1)
        structure(
            .Data = as.array(0.5 * (1 + cos(.all))),
            .Dimnames = list(lag = 1:(.cut-1)))        
    },
    Parzen = function(.cut) {
        ##  Use the 'Parzen'-window for the computation.
        .all <- 1/.cut * 1:(.cut-1)
        .first <- .all[seq_len(floor(.cut/2))]
        .second <- setdiff(x = .all,
                           y = .first)
        structure(
            .Data = as.array(c(1 - 6 * (.first)^2 + 6 * (.first)^3,
                               2 * (1-.second)^2)),
            .Dimnames = list(lag = 1:(.cut-1)))
    },
    Bartlett = function(.cut) {
        ##  Use the 'Bartlett'-window for the computation.
        .all <- 1/.cut * 1:(.cut-1)
        structure(
            .Data = as.array(1 - .all),
            .Dimnames = list(lag = 1:(.cut-1)))
    }
)
