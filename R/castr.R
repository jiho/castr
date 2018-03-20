#' castr: An R package to process CTD casts.
#'
#' This package contains functions to process data collected during CTD casts:
#' \itemize{
#' \item [smooth()], [despike()], or [integrate()] a variable;
#' \item compute the depth of clines with [clined()], of the deep chlorophyll maximum with [maxd()], of the bottom of the mixed layer with [mld()];
#' \item compute a stratification index with [stratif()]
#' }
#' It also provides utilities to apply a function in a moving window along the cast ([rollapply()]), hence enabling the computation of moving averages (to [smooth()] the data), medians (to detect [spikes()]), etc.
#' Finally, two small datasets are provided to test the functions: one with a single cast ([d]) and another one with 41 casts ([ctd]).
#'
#' @section Usage:
#' All functions in `castr` work on vectors and make few assumptions regarding your data. Therefore, they fit in the `\link[tidyverse]{tidyverse}` philosophy and work well with `\link[dplyr]{dplyr}` verbs, as shown in the example below.
#'
#' @examples
#' \dontrun{
#' # We have a data.frame containing a time series of 41 CTD casts collected
#' # at the same station over two years, identified by the date of sampling
#' head(ctd)
#'
#' # Display the data
#' library("ggplot2")
#' ggplot(ctd) + geom_path(aes(x=temp, y=-depth, group=date), alpha=0.6)
#' # there are a few spikes in temperature
#'
#' # Despike each profile
#' library("dplyr")
#' ctd_clean <- ctd %>% group_by(date) %>%
#'   mutate(temp=despike(temp, mult=3))
#' ggplot(ctd_clean) + geom_path(aes(x=temp, y=-depth, group=date), alpha=0.6)
#' # the spikes are gone! Do the same for all variables
#' # NB: the despiking parameters should really be tested and adjusted
#' ctd_clean <- ctd %>% group_by(date) %>%
#'   mutate(
#'     temp=despike(temp, mult=3),
#'     sal=despike(sal, mult=3),
#'     fluo=despike(fluo, k=2, mult=4),
#'     # NB: fluorescence is more spiky so be less stringent
#'     sigma=despike(sigma, mult=3)
#'   )
#' ggplot(ctd_clean) + geom_path(aes(x=sal, y=-depth, group=date), alpha=0.6)
#' ggplot(ctd_clean) + geom_path(aes(x=fluo, y=-depth, group=date), alpha=0.6)
#'
#' # Now compute summary statistics on each despiked profile
#' stats <- ctd_clean %>% group_by(date) %>%
#'   summarise(
#'     thermocline = clined(temp, depth, n.smooth=2, k=2),
#'     pycnocline = clined(sigma, depth),
#'     strat_index = stratif(sigma, depth, min.depths=0:5, max.depth=60:65),
#'     DCM = maxd(fluo, depth, n.smooth=2, k=3),
#'     MLD = mld(sigma, depth, ref.depths=0:5, default.depth=80),
#'     # it is even possible to use variables computed above to make the
#'     # following computations adapted to each cast:
#'     # average tempeature in the mixed layer only
#'     temp_avg = integrate(temp, depth, from=0, to=MLD, fun=mean),
#'     # stock of Chl a within 10 m of the DCM
#'     chla_dcm_stock = integrate(fluo, depth, from=DCM-10, to=DCM+10)
#'   )
#' # Inspect the results
#' ggplot(stats) + geom_path(aes(x=date, y=-thermocline))
#' # thermocline depth is still messy despite the smoothing, probably
#' # related to sharp changes in this coastal region.
#'
#' ggplot(stats) + geom_path(aes(x=date, y=strat_index))
#' ggplot(stats) + geom_path(aes(x=date, y=-MLD))
#' # the stratification index and the MLD show a nice seasonality in mixing,
#' # with shallow MLDs and strong stratification in summer and deep mixing
#' # in winter.
#'
#' ggplot(stats) + geom_path(aes(x=date, y=temp_avg))
#' # average temperature in the mixed layer shows the expected seasonality.
#'
#' ggplot(stats) + geom_path(aes(x=date, y=chla_dcm_stock))
#' # the stock of chlorophyll a (estimated by fluorescence) within 10 m on
#' # either side of the DCM suggests that 2016 had a more productive DCM.
#' }
#'
#' @docType package
#' @name castr
NULL


#' A CTD cast
#'
#' A dataset representing a single CTD cast.
#'
#' @format A data frame with 77 rows and 5 variables:
#' \describe{
#'   \item{depth}{depth, in m, positive towards the bottom}
#'   \item{temp}{temperature, in degrees Celsius}
#'   \item{sal}{salinity, in PSU}
#'   \item{fluo}{fluorescence of Chlorophyll a, in mg/m^3}
#'   \item{sigma}{potential density, in kg/m^3 - 1000}
#' }
#' @examples
#' head(d)
#' plot(-depth ~ temp,  data=d, type="b")
#' plot(-depth ~ sal,   data=d, type="b")
#' plot(-depth ~ fluo,  data=d, type="b")
#' plot(-depth ~ sigma, data=d, type="b")
"d"

#' Several CTD casts
#'
#' A time series of 41 CTD casts collected at the same station over two years, identified by the date of sampling.
#'
#' @format A data frame with 3134 rows and 6 variables:
#' \describe{
#'   \item{date}{date of sampling}
#'   \item{depth}{depth, in m, positive towards the bottom}
#'   \item{temp}{temperature, in degrees Celsius}
#'   \item{sal}{salinity, in PSU}
#'   \item{fluo}{fluorescence of Chlorophyll a, in mg/m^3}
#'   \item{sigma}{potential density, in kg/m^3 - 1000}
#' }
#' @examples
#' head(ctd)
#' plot_casts <- function(formula, x) {
#'   plot(formula, data=x, type="n")
#'   for (day in unique(x$date)) {
#'     lines(formula, data=subset(x, date==day))
#'   }
#' }
#' plot_casts(-depth ~ temp, ctd)
#' plot_casts(-depth ~ sal, ctd)
#' plot_casts(-depth ~ fluo, ctd)
#' plot_casts(-depth ~ sigma, ctd)
"ctd"
