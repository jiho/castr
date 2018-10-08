#' Define an (optimised) regular coordinate
#'
#' Based on an original coordinate, define a new one that is regular and as close as possible to the original one, overall.
#'
#' @param x vector containing the original coordinate.
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
#' # 0. Already regular coordinate
#' x <- 1:10
#' pc(x, regularise_coord(x))
#'
#' # 1. Random coordinate
#' set.seed(1)
#' x <- sort(runif(10))
#' pc(x, regularise_coord(x))
#' pc(x, regularise_coord(x, step=0.05))
#'
#' # 2. Almost regular coordinate
#' set.seed(1)
#' x <- jitter(1:10, factor=0.5)
#' pc(x, regularise_coord(x))
#'
#' # 3. Regular coordinate with additions
#' x <- sort(c(1:10, 5.2))
#' pc(x, regularise_coord(x))
#'
#' # 4. Regular coordinate with deletions
#' x <- (1:10)[-c(3)]
#' pc(x, regularise_coord(x))
#'
#' # 5. Regular coordinate with outliers
#' x <- c(1:10, 12.9)
#' pc(x, regularise_coord(x))
#' x <- c(1:10, 19.9)
#' pc(x, regularise_coord(x))
#' # -> not ideal...
regular_coord <- function(x, start=NULL, step=NULL, control=NULL, ...) {
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
