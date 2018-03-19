#' Detect the position of a cline in a CTD profile
#'
#' The cline of a variable is the depth at which it varies the most along the cast; here it is computed as the maximum of the moving standard deviation over the (possibly smoothed) cast.
#'
#' @inheritParams rollapply
#' @param depth vector of depths at which `x` is measured; optional.
#' @param n.smooth integer, number of times to smooth the data before computing the moving standard deviation.
#' @param ... not used.
#'
#' @return When `depth` is `NULL`, return the index of the cline, `i`; when `depth` is provided, return `depth[i]`, the value of the depth of the cline.
#' @export
#'
#' @examples
#' plot(-depth ~ temp, data=d, type="l")
#' thermocline <- clined(d$temp, d$depth)
#' abline(h=-thermocline, col="red")
#'
#' plot(-depth ~ sal, data=d, type="l")
#' halocline <- clined(d$sal, d$depth)
#' abline(h=-halocline, col="red")
clined <- function(x, depth=NULL, k=2, n.smooth=0, ...) {
  # check input
  ok <- check_input(x, depth)
  if (!ok) { return(NA) }

  # smooth the profile (if requested)
  x <- smooth(x, k=k, n=n.smooth)

  # compute the standard deviation
  s <- rollapply(x, k=k, sd, na.rm=TRUE)
  # get its maximum
  i <- which.max(s)

  # if the depth is provided, extract the corresponding depth
  i <- get_depth(i, depth)

  return(i)
}