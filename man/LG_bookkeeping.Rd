% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_bookkeeping.R
\name{LG_bookkeeping}
\alias{LG_bookkeeping}
\title{Bookkeeping for the local Gaussian estimates}
\usage{
LG_bookkeeping(spy_report)
}
\arguments{
\item{spy_report}{The result returned by \code{spy} when used on
the targeted function.}
}
\value{
This function returns a list to the internal workflow, with
    some key details related to the computations to be performed.
    The pivotal detail of this list is the logical value
    \code{done_before}, since this can be used to prevent that
    computational time is used to reproduce local Gaussian
    estimates that already are available in the file-hierarchy.
}
\description{
This internal function perform the bookkeeping for the
    arguments gathered by the \code{spy}-report from the targeted
    function, which should be one of the functions
    \code{LG_approx_scribe}, \code{LG_boot_approx_scribe}, and
    \code{LG_shiny}.
}
\details{
This function records the given arguments and sends them
    to \code{LG_sanity_checks} in order to see if those arguments
    are sensible.  Then it takes care of the bookkeeping of the
    information stored in the info-file.
}
\keyword{internal}
