#' Detect the position of a cline
#'
#' The cline of a variable is the depth at which it varies most sharply along the cast; here it is computed as the maximum of the moving standard deviation over the (possibly smoothed) cast.
#'
#' @inheritParams smooth
#' @param depth vector of depths at which `x` is measured; optional.
#' @param n.smooth integer, number of times to smooth the data before computing the moving standard deviation.
#'
#' @return When `depth` is `NULL`, return the index of `x` corresponding to the cline, `i`. When `depth` is provided, return `depth[i]`, the value of the depth of the cline.
#' @export
#'
#' @family functions computing remarkable depths
#' @seealso [slide()] for the underlying implementation of the moving standard deviation, [smooth()] for smoothing.
#'
#' @examples
#' plot(-depth ~ temp, data=d, type="l")
#' thermocline <- clined(d$temp, d$depth)
#' abline(h=-thermocline, col="red")
#'
#' plot(-depth ~ sal, data=d, type="l")
#' halocline <- clined(d$sal, d$depth)
#' abline(h=-halocline, col="red")
clined <- function(x, depth=NULL, n.smooth=0, k=2) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }

  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)

  # compute the standard deviation
  s <- slide(x, k=k, stats::sd, na.rm=TRUE)
  # get its maximum
  i <- which.max(s)

  # if the depth is provided, extract the corresponding depth
  i <- get_depth(i, depth)

  return(i)
}
