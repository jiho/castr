#' Detect the lower limit of the euphotic zone from PAR
#'
#' @param par vector of Photosynthetic Active Radiation.
#' @param depth vector of depths at which `par` is measured.
#'
#' @details The lower limit of the euphotic zone (`Ze`) is the depth at which the downwelling PAR irradiance is reduced to 1% or its surface value. Values are accepted as "surface" until a maximum depth of 2 m.
#'
#' @return The depth of the lower limit of the euphotic zone.
#' @export
#'
#' @family functions computing remarkable depths
#' @seealso [smooth()] for smoothing.
#'
ze_from_par <- function(par, depth) {
  ok <- check_input(par, depth)
  if (!ok) { return(NA) }

  df <- stats::na.omit(data.frame(par, depth))

  if (min(df$depth) > 2) {
    warning("No value of PAR above 2 m depth. Cannot compute Ze.")
    ze <- NA
  } else {
    ze <- df$depth[min(which(df$par < df$par[1] * 0.01))]
  }

  return(ze)
}


