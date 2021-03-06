#' Smooth a variable
#'
#' Smooth a variable in a cast using a weighted moving average.
#'
#' @inheritParams slide
#' @param x vector of the variable of interest. It must have been recorded at approximately regular intervals (see [slide()] as to why).
#' @param n number of times to smooth the data.
#'
#' @return A vector containing the smoothed variable, containing as many elements as the original variable.
#' @export
#'
#' @seealso [slide()] for the underlying implementation.
#'
#' @examples
#' plot(-depth ~ chla, data=d)
#' lines(-depth ~ smooth(chla), data=d, type="l", col="red")
#' lines(-depth ~ smooth(chla, k=2, n=5), data=d, type="l", col="blue")
#' legend(1, 0, legend=c("k=1, n=1", "k=2, n=5"), col=c("red", "blue"), lty="solid")
smooth <- function(x, k=1, n=1) {
  # compute centered weights
  w <- c(1:k,k+1,k:1)
  w <- w / sum(w)
  # compute the (running) weighted moving average
  slide(x, k=k, stats::weighted.mean, na.rm=TRUE, w=w, n=n)
}
