#' Round values to a given precision
#'
#' @param x vector of values, can be numeric, POSIXct, Date.
#' @param precision precision at which to round, in the unit of `x` (for POSIXct, seconds; for Date, days).
#' @param f rouding function: [base::floor()], [base::ceiling()], and , [base::round()]
#'
#' @return A vector of the same length as x, rounded to precision.
#' @export
#'
#' @examples
#' roundp(1:10, 1)
#' roundp(1:10, 4)
#' roundp(1:10, 4, floor)
#' roundp(1:10, 4, ceiling)
#'
#' roundp(Sys.time() + 1:5, 3)
#' roundp(Sys.time() + 1:5, 5, floor)
#' # round on the hour
#' roundp(Sys.time(), 3600)
#'
#' roundp(Sys.Date() + 1:5, 2)
#' roundp(Sys.Date() + 1:5, 2, floor)
roundp <- function (x, precision, f=round) {
  UseMethod("roundp")
}

#' @export
roundp.numeric <- function(x, precision, f=round) {
  f(x/precision) * precision
}

#' @export
roundp.POSIXct <- function(x, precision, f=round) {
  tz <- format(x[1], "%Z")
  xr <- roundp(as.numeric(x), precision, f)
  as.POSIXct(xr, origin="1970-01-01 00:00.00 UTC", tz=tz)
}

#' @export
roundp.Date <- function(x, precision, f=round) {
  xr <- roundp(as.numeric(x), precision, f)
  as.Date(xr, origin="1970-01-01")
}
