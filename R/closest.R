#' Find closest matches
#'
#' For each element of `from`, find the the closest element of `within`.
#'
#' @param from a numeric vector (or coercible as such); the values to be matched.
#' @param within a vector of the same nature as `from`; the values to be matched against.
#' @param outside logicial. When `TRUE` (the default) return the extremities for elements outside of the range `[min(within),max(within)]`. When `FALSE` return `NA` for such elements
#'
#' @return A vector of integers of the same length as `from`: for `closest()`, the value of the closest elements of `within`; for `which.closest()` the indexes of those values.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' to_match  <- round(runif(5, min=0, max=100), 1)
#' reference <- round(runif(10, min=20, max=80), 1)
#'
#' # compare the vectors
#' to_match
#' reference
#'
#' # get closest mactches, or their index
#' closest(to_match, within=reference)
#' which.closest(to_match, within=reference)
#'
#' # verify graphically
#' plot(
#'   x=c(reference, to_match),
#'   y=c(rep(2, 10), rep(1, 5)), ylim=c(0, 3), yaxt="n",
#'   xlab="", ylab=""
#' )
#' axis(2, at=1:2, labels=c("to match", "reference"))
#' # join closest points
#' segments(
#'   x0=to_match, y0=1,
#'   x1=closest(from=to_match, within=reference), y1=2
#' )
#' # join closest points inside "within"
#' abline(v=range(reference), lty="22")
#' segments(
#'   x0=to_match, y0=1,
#'   x1=closest(from=to_match, within=reference, outside=FALSE), y1=2,
#' col="red")
closest <- function(from, within, outside=TRUE) {
  within[which.closest(from=from, within=within, outside=outside)]
}

#' @export
#' @rdname closest
which.closest <- function(from, within, outside=TRUE) {
  # record order/rank of inputs
  order_from <- order(from)
  rank_from <- rank(from)
  order_within <- order(within)

  # find closest matches on sorted versions of the inputs
  i <- round(approx(x=within[order_within], y=1:length(within), xout=from[order_from], rule=ifelse(outside, 2, 1))$y)

  # "unsort" back to the original order
  order_within[i][rank_from]
}
