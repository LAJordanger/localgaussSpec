% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TS_families.R
\name{TS_key_rugarch}
\alias{TS_key_rugarch}
\title{The univariate GARCH case for \code{TS_key}}
\description{
The \code{TS_key_rugarch}-case part of
    \code{TS_families} is simply an implementation of
    \code{ugarchpath} from the \code{rugarch}-package.
}
\note{
The arguments \code{spec}, \code{n.start}, \code{m.start},
    \code{presigma}, \code{preresiduals}, \code{rseed},
    \code{custom.dist}, \code{mexsimdata} and \code{vexsimdata}
    should be given to \code{TS_sample}, which will send them to
    this function by the internal helper-function
    \code{TS_sample_helper}.
}
