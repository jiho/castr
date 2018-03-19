#' Smooth a variable profile
#'
#' Smooth a variable profile using a weighted moving average.
#'
#' @inheritParams rollapply
#' @param n number of times to smooth the data.
#'
#' @return A vector containing the smoothed variable, containing as many elements as the original variable.
#' @export
#'
#' @examples
#' plot(-depth ~ fluo, data=d, type="l")
#' lines(-depth ~ smooth(fluo), data=d, type="l", col="red")
#' lines(-depth ~ smooth(fluo, k=2, n=5), data=d, type="l", col="blue")
smooth <- function(x, k=1, n=1) {
  # compute centered weights
  w <- c(1:k,k+1,k:1)
  w <- w / sum(w)
  # compute the (running) weighted moving average
  rollapply(x, k=k, weighted.mean, na.rm=TRUE, w=w, n=n)
}
