% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_shiny_interface_1_helper.R
\name{LG_shiny_interface_1_helper}
\alias{LG_shiny_interface_1_helper}
\title{Find the parameters to be used in the \code{LG_shiny}-interface.}
\usage{
LG_shiny_interface_1_helper(.env, .approx, .iterated = FALSE)
}
\arguments{
\item{.env}{The environment where the original arguments given to
\code{LG_shiny} lives, i.e. arguments like \code{main_dir} and
\code{data_dir}.}

\item{.approx}{A vector that specifies one of the nodes of
\code{.info}.}

\item{.iterated}{A logical value, default \code{FALSE}, that is
included since this function will call itself when an
approx-node contains bootstrap-information.  (There are some
minor differences with regard to how the result should be
returned in these to cases.)}
}
\value{
This helper function finds the parameters to use when
    \code{LG_shiny} shall visualise the content of interest.
}
\description{
Find the parameters to be used in the \code{LG_shiny}-interface.
}
\keyword{internal}