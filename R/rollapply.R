#' Apply a function over a rolling window along a vector
#'
#' Allows to compute a moving average, median, sd, mad, etc. in a generic way.
#'
#' @param x vector of data (assumed to be approximately evenly spaced for moving window statistics to make sense).
#' @param k order of the window; the window size is 2k+1.
#' @param fun function to apply in the moving window.
#' @param n number of times to pass the function over the data.
#' @param ... arguments passed to `fun`. A usual one is `na.rm=TRUE`.
#'
#' @return The data passed through `fun`, `n` times.
#' @export
#'
#' @examples
#' # create some data and add random noise
#' xs <- sin(seq(0, 4*pi, length=100))
#' x <- xs + rnorm(length(xs), sd=0.25)
#' plot(x)
#' lines(xs)
#' # filter the data in various ways
#' # moving average
#' mav   <- rollapply(x, 3, mean, na.rm=T)
#' # running moving average
#' rmav  <- rollapply(x, 3, mean, na.rm=T, n=4)
#' # weighted moving average
#' wmav  <- rollapply(x, 3, weighted.mean, na.rm=T, w=c(1,2,3,4,3,2,1)/16)
#' # weighted running moving average
#' wrmav <- rollapply(x, 3, weighted.mean, na.rm=T, w=c(1,2,3,4,3,2,1)/16, n=4)
#' # moving median
#' mmed  <- rollapply(x, 3, median, na.rm=T)
#' lines(mav, col="red")
#' lines(rmav, col="red", lty="dotted")
#' lines(wmav, col="orange")
#' lines(wrmav, col="orange", lty="dotted")
#' lines(mmed, col="blue")
#' # inspect variability around filtered data
#' plot(rollapply(x-rmav, 7, sd))
#' plot(rollapply(x-mmed, 7, mad))
rollapply <- function(x, k, fun, n=1, ...) {
  if (n>=1) {
    # repeat n times
    for (t in 1:n) {
      # pad the extremities of data to be able to compute over the whole vector
      x <- c(rep(NA, times=k), x, rep(NA, times=k))

      # apply the rolling function (and and remove padding at the extremities)
      x <- sapply((k+1):(length(x)-k), function(i) {
        fun(x[(i-k):(i+k)], ...)
      })
    }
  }

  return(x)
}
