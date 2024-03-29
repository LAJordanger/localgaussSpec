% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_approx_scribe.R
\name{LG_approx_scribe}
\alias{LG_approx_scribe}
\title{Local Gaussian Approximations, scribe-function}
\usage{
LG_approx_scribe(
  main_dir,
  data_dir,
  TS,
  lag_max = ceiling(3 * sqrt(length(TS))),
  LG_points,
  .bws_mixture = c("mixture", "local", "global"),
  bw_points = c(25, 35),
  .bws_fixed = NULL,
  .bws_fixed_only = FALSE,
  content_details = c("rho_only", "rho_log.fun", "rho_all"),
  LG_type = c("par_five", "par_one")
)
}
\arguments{
\item{main_dir}{The path to the main directory, that contains the
file-hierarchy created when using the local Gassian approach
for the investigation of time series.}

\item{data_dir}{A specification of the directory to be used when
loading and saving data.}

\item{TS}{The time series we want to investigate by means of
techniques based on local Gaussian approximations.  Note that
this argument in the "wrapper"- and "scribe"-functions can be
given as a character vector that specifies the file where
\code{TS} from \code{TS_LG_object} was stored.}

\item{lag_max}{The number of lags to include in the analysis.  The
default value \code{ceiling(3*sqrt(length(TS)))} will probably
in most cases include more lags than actually required.  It
might thus be worthwhile to run some test and see if a smaller
value can be used instead.}

\item{LG_points}{An array that specifies the point at which it is
desired to compute the local Gaussian estimates.  Use the
function \code{LG_select_points} to create this argument.}

\item{.bws_mixture}{An argument that specifies how the global
bandwidths and those obtained by the nearest-neighbour strategy
should be combined.  The three available options are
\code{c("mixture", "local", "global")}, which have the
following effects.  The alternatives \code{local} and
\code{global} will respectively only select the nearest
neighbour or global.  These alternatives seems however to not
work well when used on some parts of the lagged pairs of
interest, i.e. the nearest neighbour values might be to "small"
in the center of the distribution, whereas the global
bandwidths seems to fail in the periphery of the distribution.
The alternative \code{mixture} attempt to resolve this by (for
each grid point) selecting the largest of the two alternative
bandwidths.  Note that the value of \code{.bws_mixture} decides
how much information that is computed, i.e. the alternative
\code{local} will turn off the computation of global
bandwidths.  However, the computations of the nearest neighbour
bandwidths will also be computed when the alternative
\code{global} is used, since it does not take long to compute
and it is that function that creates the array we need as a
mould for the result.  If the user does not make a selection,
then all three alternatives will be computed.}

\item{bw_points}{A vector, default \code{c(25, 35)}, that specifies
the percentage of the observations that we want inside the
"bandwidth-square".  If \code{.bws_mixture} is selected to be
\code{global}, then this argument will be ignored. and no
nearest neighbours will be computed.}

\item{.bws_fixed}{A vector of non-negative real values, that can be
used to specify fixed values for the bandwidths (which might be
of interest to do in a preliminary analysis).  The default
value \code{NULL} will prevent the computation of Local
Gaussian Estimates based on fixed bandwidths.}

\item{.bws_fixed_only}{A logic value, default \code{FALSE}, that
can be used to drop the rather time-consuming data-driven
estimation of bandwidths in favour of the simplified approach
where fixed bandwidths are used instead.  Note that
\code{.bws_fixed} must be specified when \code{.bws_fixed_only}
are set to \code{TRUE}.}

\item{content_details}{A character string from \code{c("rho_only",
"rho_log.fun", "rho_all")}, of which the first will be selected
if the user doesn't specify something else.  The purpose of
this argument is to decide the amount of details that should be
stored from the estimates governed by the argument
\code{LG_type}.  The default \code{rho_only} is the one needed
for the computation of the Local Gaussian Spectral Densities,
whereas \code{rho_log.fun} in addition returns the estimated
values of the logarithm of the density.  The option
\code{rho_all} will in addition return all the parameters when
\code{LG_type} is given as \code{par_five}.}

\item{LG_type}{One of \code{c("par_five", "par_one")}, i.e. should the
Local Gaussian Autocorrelations be based on the approach using
five parameters or the simplified approach using only one
parameter.  The default choice is \code{"par_five"}.}
}
\value{
This function is a scribe that reads and records
    information, whereas another function performs the actual
    computation, see details for further information.  A list
    containing the following key-information is always returned to
    the workflow.

\describe{

\item{done_before}{Logical value that reveals if the computation
has been performed before.}

\item{main_dir}{The \code{main_dir}-argument is included here.}

\item{data_dir}{The \code{data_dir}-argument is included here.}

}
}
\description{
This function takes care of the bookkeeping when the
    local Gaussian (auto- and cross-) correlations are computed for
    the lag-h pairs for a single time series.  It can also digest a
    collection of samples from a parametric time series model.
}
\details{
This function records its arguments and compares them to a
    previously stored information-object for the time series under
    investigation, in order to avoid redoing previously performed
    computations.  This function calls \code{LG_approx} when a new
    computation is required, the result is then saved to file, and
    the information-object is updated with the key details.
}
\note{
Regarding the case where the \code{LG_type}-argument is equal
    to "par_one": The author of this package has always considered
    the "par_one"-approach to be reasonable when the aim of the
    investigation is to estimate a density at a given point.
    However, the extraction of the correlation value from the
    resulting density-estimate will in general not capture the
    local geometrical properties of the targeted distribution at
    the point of investigation.  The "par_one"-approach is as such
    (in general) a complete waste of computation resources.
}
