#' Apply a function in a sliding window along a vector
#'
#' Allows to compute a moving average, moving median, or even moving standard deviation, etc. in a generic way.
#'
#' @param x input numeric vector.
#' @param k order of the window; the window size is 2k+1.
#' @param fun function to apply in the moving window.
#' @param n number of times to pass the function over the data.
#' @param ... arguments passed to `fun`. A usual one is `na.rm=TRUE` to avoid getting `NA`s at the extremities of `x`.
#'
#' @details A window of size `2k+1` is centred on element `i` of `x`. All elements from index `i-k` to index `i+k` are sent to function `fun`. The returned value is associated with index `i` in the result. The window is moved to element `i+1` and so on.
#'
#' For such sliding window computation to make sense, the data must be recorded on a regular coordinate (i.e. at regular intervals). Otherwise, data points that are far from each other may end up in the same window.
#'
#' The extremeties of the input vector are padded with `NA` to be able to center the sliding window from the first to the last elements. This means that, to avoid getting `k` missing values at the beginning and at the end of the result, `na.rm=TRUE` should be passed to `fun`.
#'
#' @return The data passed through `fun`, `n` times.
#' @export
#'
#' @seealso [cweights()] to compute weights centered in the middle of the window.
#'
#' @examples
#' # create some data and add random noise
#' xs <- sin(seq(0, 4*pi, length=100))
#' x <- xs + rnorm(length(xs), sd=0.25)
#' plot(x)
#' lines(xs)
#' # filter the data in various ways
#' # moving average
#' mav   <- slide(x, 3, mean, na.rm=TRUE)
#' # running moving average
#' rmav  <- slide(x, 3, mean, na.rm=TRUE, n=4)
#' # weighted moving average
#' wmav  <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3))
#' # weighted running moving average
#' wrmav <- slide(x, 3, weighted.mean, na.rm=TRUE, w=cweights(3), n=4)
#' # moving median
#' mmed  <- slide(x, 3, median, na.rm=TRUE)
#' lines(mav, col="red")
#' lines(rmav, col="red", lty="dashed")
#' lines(wmav, col="orange")
#' lines(wrmav, col="orange", lty="dashed")
#' lines(mmed, col="blue")
#' # inspect variability around filtered data
#' plot(slide(x-rmav, 7, sd))
#' plot(slide(x-mmed, 7, mad))
slide <- function(x, k, fun, n=1, ...) {
  # make sure to get a function as the `fun` argument (i.e. avoid name masking)
  if (!is.function(fun)) {
    fun <- get(as.character(substitute(fun)), mode="function")
  }

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
