#' Define an (optimised) regular coordinate
#'
#' Based on an original coordinate, define a new one that is regular and as close as possible to the original one, overall.
#'
#' @param x vector containing the original coordinate; there are methods for `numeric`, `POSIXt` (date and times), and `Date` objects.
#' @param start initial guess for the starting point of the regular coordinate. If `NULL`, set to the current starting point (i.e. the minimum value of x).
#' @param step initial guess for the step of the regular coordinate. If `NULL`, set so that the new coordinate spans the entire range of `x`.
#' @param control passed as the `control` argument of [stats::optim()]. `parscale` is already set.
#'
#' @export
#'
#' @examples
#' # Plot original and regularised coordinates
#' pc <- function(x, xr, ...) {
#'   plot(x, rep(1, length(x)), type="n",
#'        xlim=range(c(x, xr)), ylim=c(0.8, 1.2),
#'        ylab="", yaxt="n")
#'   abline(h=1, lty="22")
#'   segments(x0=x, y0=1, x1=x, y1=1.1)
#'   segments(x0=xr, y0=1, x1=xr, y1=0.9)
#'   axis(2, at=c(1.05, 0.95), labels=c("original", "regularised"))
#' }
#'
#' # Numeric coordinate
#' # Already regular coordinate
#' x <- 1:10
#' pc(x, regularise_coord(x))
#'
#' # Random coordinate
#' set.seed(1)
#' x <- sort(runif(10))
#' pc(x, regularise_coord(x))
#' pc(x, regularise_coord(x, step=0.05))
#'
#' # Almost regular coordinate
#' set.seed(1)
#' x <- jitter(1:10, factor=0.5)
#' pc(x, regularise_coord(x))
#'
#' # Regular coordinate with additions
#' x <- sort(c(1:10, 5.2))
#' pc(x, regularise_coord(x))
#'
#' # Regular coordinate with deletions
#' x <- (1:10)[-c(3)]
#' pc(x, regularise_coord(x))
#'
#' # Regular coordinate with outliers
#' x <- c(1:10, 12.9)
#' pc(x, regularise_coord(x))
#' x <- c(1:10, 19.9)
#' pc(x, regularise_coord(x))
#' # -> not ideal...
#'
#' # Times
#' set.seed(1)
#' x <- as.POSIXct("2010-01-01 00:00:00", tz="UTC") + jitter(1:10) * 10
#' pc(x, regularise_coord(x))
#' x <- as.POSIXct("2010-01-01 00:00:00", tz="UTC") + jitter(1:10) * 3600 * 24
#' pc(x, regularise_coord(x))
#'
#' # Dates
#' set.seed(1)
#' x <- as.Date("2010-01-01") + round(jitter(1:10)*20)
#' pc(x, regularise_coord(x))
regularise_coord <- function(x, start=NULL, step=NULL, control=NULL, ...) {
  UseMethod("regularise_coord", x)
}

regularise_coord.Date <- function(x, start=NULL, step=NULL, control=NULL, ...) {
  xn <- as.numeric(x - (x[1] - 1))
  xr <- regularise_coord(xn, start=NULL, step=NULL, control=NULL, ...)
  as.Date(xr, origin=x[1] - 1)
}

regularise_coord.POSIXt <- function(x, start=NULL, step=NULL, control=NULL, ...) {
  xn <- as.numeric(x - (x[1] - 1))
  xr <- regularise_coord(xn, start=NULL, step=NULL, control=NULL, ...)
  as.POSIXct(xr, tz=attr(x,"tzone"), origin=x[1] - 1)
}

regularise_coord.numeric <- function(x, start=NULL, step=NULL, control=NULL, ...) {
  # guess initial values for the coordinate definition
  if (is.null(start)) {
    start <- min(x, na.rm=TRUE)
  }
  if (is.null(step)) {
    step <- diff(range(x, na.rm=TRUE)) / (length(x)-1)
  }
  # TODO allow to set n rather than step
  stop <- max(x, na.rm=TRUE)
  init_par <- c(start=start, step=step)

  # Generate a regular coordinate
  gen_coord <- function(par, stop) {
    seq(
      from=par[1],      # start
      to=stop+par[2]/2, # allow to go a bit after the end of the initial data
      # TODO does not work with Date, POSIXct
      by=par[2]         # step
    )
  }

  # Score a regular coordinate compared to the original one
  score <- function(par, x) {
    # compute new coordinate based on par
    xr <- gen_coord(par, stop)
    # score it
    # sum((x - closest(from=x, within=xr))^2) + sum((xr - closest(from=xr, within=x))^2)
    sum((x - closest(from=x, within=xr))^2)
  }

  # browser()
  # optimise the starting point and step
  o <- tryCatch(
    optim(par=init_par, fn=score, x=x, control=c(list(parscale=init_par), control), ...),
    error=function(e) {
      warning("Cannot find an optimal coordinate.\nUsing the un-optimised `start` and `step` values.", call.=FALSE)
      return(list(par=c(start, step)))
    }
  )

  # return the optimised coordinate
  gen_coord(o$par, stop)
}
