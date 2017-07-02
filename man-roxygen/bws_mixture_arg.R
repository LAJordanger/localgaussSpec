#' @param .bws_mixture An argument that specifies how the global
#'     bandwidths and those obtained by the nearest-neighbour strategy
#'     should be combined.  The three available options are
#'     \code{c("mixture", "local", "global")}, which have the
#'     following effects.  The alternatives \code{local} and
#'     \code{global} will respectively only select the nearest
#'     neighbour or global.  These alternatives seems however to not
#'     work well when used on some parts of the lagged pairs of
#'     interest, i.e. the nearest neighbour values might be to "small"
#'     in the center of the distribution, whereas the global
#'     bandwidths seems to fail in the periphery of the distribution.
#'     The alternative \code{mixture} attempt to resolve this by (for
#'     each grid point) selecting the largest of the two alternative
#'     bandwidths.  Note that the value of \code{.bws_mixture} decides
#'     how much information that is computed, i.e. the alternative
#'     \code{local} will turn off the computation of global
#'     bandwidths.  However, the computations of the nearest neighbour
#'     bandwidths will also be computed when the alternative
#'     \code{global} is used, since it does not take long to compute
#'     and it is that function that creates the array we need as a
#'     mould for the result.  If the user does not make a selection,
#'     then all three alternatives will be computed.
