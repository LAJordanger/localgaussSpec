% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_lookup_curlicues_plot_stamp.R
\name{LG_lookup_curlicues_plot_stamp}
\alias{LG_lookup_curlicues_plot_stamp}
\title{Helper-function to create the plot-stamp-label for the the \code{curlicues}-list}
\usage{
LG_lookup_curlicues_plot_stamp(look_up, CSC)
}
\arguments{
\item{look_up}{A list from the internal workings of the function
\code{LG_lookup}.}

\item{CSC}{The case-specific-curlicues (CSC) from the internal
workings of the \code{LG_lookup_curlicues}-function.}
}
\value{
This function returns the plot-stamp-label to the calling
    function, i.e. \code{LG_lookup_curlicues}, which will add it to
    the \code{CSC}-list.
}
\description{
This internal function is called by
    \code{LG_lookup_curlicues} in order to produce the plot-stamp,
    i.e. the annotated text that reveals the content of the plot
    under investigation.
}
\keyword{internal}
