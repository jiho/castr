#' Compute the lower limit of the euphotic zone from surface chlorophyll content
#'
#' @param chla value(s) of chlorophyll concentration at the surface, in mg m-3, typically measured from satellite.
#'
#' @details The lower limit of the euphotic zone (`Ze`) is normally computed from a profile of downwelling PAR irradiance (see [ze_from_par()]). Here, it is estimated using the procedure of Lavigne et al (2012): (1) use \[Chl a\] to estimate Kd490, the attenuation coefficient at 490 nm (Morel and Maritorena, 2001), (2) use Kd490 to estimate KdPAR (Rochford et al, 2001), (3) use KdPAR to estimate Ze assuming an exponential decrease of light with depth.
#'
#' @return The estimated depth of the lower limit of the euphotic zone.
#' @export
#'
#' @family functions computing remarkable depths
#'
#' @references
#' Morel and Maritorena (2001). Bio‐optical properties of oceanic waters: A reappraisal. Journal of Geophysical Research: Oceans, 106(C4), 7163-7180.
#'
#' Rochford et al (2001). Importance of solar subsurface heating in ocean general circulation models. Journal of Geophysical Research: Oceans, 106(C12), 30923-30938.
#'
#' Lavigne et al (2012). Towards a merged satellite and in situ fluorescence ocean chlorophyll product. Biogeosciences, 9, 2111–2125.
#'
#' @examples
#' ze_from_surface_chla(0.01)
#' ze_from_surface_chla(0.1)
#' ze_from_surface_chla(1)
ze_from_surface_chla <- function(chla) {

  # from Morel and Maritorena 2001, for a wavelength of 490 nm
  Kw <- 0.01660
  e <- 0.68955
  X <- 0.07242
  Kd490 <- Kw + X*chla^e

  # from Rochford et al. 2001
  KdPAR <- 0.0085 + 1.6243 * Kd490

  ze <- -log(0.01)/KdPAR

  return(ze)
}
