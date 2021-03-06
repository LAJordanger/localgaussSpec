% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_lookup_curlicues.R
\name{LG_lookup_curlicues}
\alias{LG_lookup_curlicues}
\title{Helper-function to create the \code{curlicues}-list of \code{look_up}}
\usage{
LG_lookup_curlicues(look_up)
}
\arguments{
\item{look_up}{A list from the internal workings of the function
\code{LG_lookup}.}
}
\value{
A \code{curlicues}-list will be returned to the calling
    function (i.e. \code{LG_lookup}), and this list contains e.g.
    details related to the size of lines, and the position, colour
    and size of the annotated text.
}
\description{
This internal helper function prepares the information
    that is needed in order to annotate assorted information
    (curlicues) to the final plots.  A plot-stamp that reveals the
    content of the plot will always be present together with
    information about the length of the sample and the number of
    replicates.  Additional information will be added depending on
    the type of plot, this can e.g. be details related to the
    truncation level, the point of investigation, the bandwidth,
    the status for the numerical convergence of the estimates, or
    the block length used for the bootstrapping algorithm.  See the
    scripts for examples related to how the annotated text can
    adjusted when the plots are created in a nonreactive setting.
}
\keyword{internal}
