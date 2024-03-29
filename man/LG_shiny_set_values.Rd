% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_shiny_set_values.R
\name{LG_shiny_set_values}
\alias{LG_shiny_set_values}
\title{Set some values in the \code{LG_shiny}-application}
\usage{
LG_shiny_set_values(
  .env,
  .env2,
  .part = c("TS_info", "TCS_type", "var_local", "p_diag_bw", "spectrum_type",
    "second_graphical", "plots", "explanations", "derived_graphical")
)
}
\arguments{
\item{.env}{The environment where the \code{TS_logging}-object
lives.}

\item{.env2}{The environment containing the two lists \code{input}
and \code{output}.}

\item{.part}{A reference to the part(s) of the
\code{input_triggers} that we want to use as basis for the
updating.  The possible values are \code{c("TS_info",
"TCS_type", "var_local", "second_graphical",
"plots", "explanations", "derived_graphical")}.  The default is
that all these will be updated.}
}
\value{
This helper-function will take the values found in the
    \code{TS_logging}-object and use them to update both the
    reactive and non-reactive copies of the \code{input}-list that
    lives in the \code{LG_shiny}-application.
}
\description{
This internal function is used to update the values of
    the \code{LG_shiny}-application based on the content of the
    \code{TS_logging}-object.
}
\keyword{internal}
