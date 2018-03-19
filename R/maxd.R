#' Detect the position of the maximum
#'
#' Find the index or depth of the maximum in a (possibly smoothed) variable along a cast. Useful to find the depth of the Deep Chlorophyll Maximum (DCM) from a fluorescence profile for example.
#'
#' @inheritParams check_input
#' @inheritParams clined
#' @param n.smooth integer, number of times to smooth the data before detecting the maximum (1 by default, because we are rarely interested in the absolute maximum, which often corresponds to a spike, but rather in the region of the maximum values).
#' @param k order of the moving average used for smoothing; the window size is 2k+1. NB: when data is smoothed, it should have been recorded at approximately regular intervals for the moving average to make sense.
#'
#' @return When `depth` is `NULL`, return the index of `x` corresponding to the maximum, `i`. When `depth` is provided, return `depth[i]`, the value of the depth of the maximum.
#' @export
#'
#' @examples
#' plot(-depth ~ fluo, data=d, type="l")
#' # find the absolute maximum
#' dcm <- maxd(d$fluo, d$depth, n.smooth=0)
#' abline(h=-dcm, col="chartreuse4", lty="dashed")
#' # find the region of the maximum, along a smoothed cast
#' dcm <- maxd(d$fluo, d$depth, n.smooth=3, k=3)
#' abline(h=-dcm, col="chartreuse4")
maxd <- function(x, depth=NULL, n.smooth=1, k=2, ...) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }

  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)

  # get the maximum
  i <- which.max(x)

  # if the depth is provided, extract the corresponding depth
  i <- get_depth(i, depth)

  return(i)
}
