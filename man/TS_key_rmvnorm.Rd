% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TS_families.R
\name{TS_key_rmvnorm}
\alias{TS_key_rmvnorm}
\title{The bivariate Gaussian case for \code{TS_key}}
\description{
The \code{TS_key_rmvnorm}-case part of
    \code{TS_families} is simply an implementation of
    \code{rmvnorm} from the \code{mvtnorm}-package.
}
\note{
The arguments \code{mean}, \code{sigma}, \code{method} and
    \code{pre0.9_9994} should be given to \code{TS_sample}, which
    will send them to this function by the internal helper-function
    \code{TS_sample_helper}.
}
