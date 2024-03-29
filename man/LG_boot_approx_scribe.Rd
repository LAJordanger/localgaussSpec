% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LG_boot_approx_scribe.R
\name{LG_boot_approx_scribe}
\alias{LG_boot_approx_scribe}
\title{Local Gaussian Approximations for bootstrap replicates, scribe-function.}
\usage{
LG_boot_approx_scribe(
  main_dir,
  data_dir,
  lag_max = NULL,
  LG_points = NULL,
  content_details = NULL,
  LG_type = NULL,
  .bws_mixture = NULL,
  bw_points = NULL,
  .bws_fixed = NULL,
  .bws_fixed_only = NULL,
  nb = NULL,
  boot_type = NULL,
  block_length = NULL,
  boot_seed = NULL,
  threshold = 500
)
}
\arguments{
\item{main_dir}{The path to the main directory, that contains the
file-hierarchy created when using the local Gassian approach
for the investigation of time series.}

\item{data_dir}{A specification of the directory to be used when
loading and saving data.}

\item{lag_max}{The number of lags to include in the analysis.}

\item{LG_points}{An array that specifies the point at which it is
desired to compute the local Gaussian estimates.  The default
value \code{NULL} will imply that the values used in the
computations upon the original data are recycled.  Values can
alternatively be computed by the help of the function
\code{LG_select_points}, but these must then be a subset of the
original values.}

\item{content_details}{A value from \code{c("rho_only",
"rho_log.fun", "rho_all")}, which decides the amount of details
stored from the local Gaussian approximations.}

\item{LG_type}{One of \code{c("par_five", "par_one")}, i.e. should the
Local Gaussian Autocorrelations be based on the approach using
five parameters or the simplified approach using only one
parameter.  The default choice is \code{"par_five"}.}

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

\item{nb}{An integer that specifies how many bootstrap-replicates
we want to use in our analysis.  Default value 5 (at least in
the development phase).}

\item{boot_type}{This argument should be either
\code{"cibbb_tuples"} or \code{"block"}, the former gives an
implementation of the circular index-based block bootstrap for
tuples adjustment, whereas the latter gives the ordinary block
bootstrap.  The default option is \code{"cibbb_tuples"}.}

\item{block_length}{The length of the blocks to be used when
\code{boot_type="block"} is used.  Default value 20 in the
development phase, but I suppose in general it should be some
formula based on the time series under investigation.}

\item{boot_seed}{Use this to enable reproducible results.  The
default value \code{NULL} will trigger a random seed to be
selected for this value (that then will be recorded in case a
reproduction of the result is desired later on).}

\item{threshold}{An integer, default value 500 (measured in MB),
that tells the program when a computation should be divided
into smaller chunks.  This reduces the chance of memory-related
problems, but as the present incarnation of \code{LG_splitter}
are rather stupid such problems might still occur for long
time-series where a huge number of lags are included.}
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
    the lag-h pairs for bootstrapped replicates of a given time
    series.
}
\details{
This function records its arguments and compares them to a
    previously stored information-object for the time series under
    investigation, in order to avoid redoing previously performed
    computations.  The function then calls \code{LG_boot_approx}
    when a new computation is required, the result is then saved to
    file, and the information-object is updated with the key
    details.

Note that default values are not given for any of the
    tuning parameters of the local Gaussian estimation algorithm.
    The basic idea is that such arguments only should be specified
    for the bootstrap-part if it is of interest to restrict the
    attention to a subset of the tuning parameters that were used
    for the local Gaussian investigation that was done on the
    original sample.  When an argument is left unspecified, the
    bookkeeping-system will look up the value that was used during
    the investigation of the original sample, and that value will
    then be inherited to the present investigation.
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
