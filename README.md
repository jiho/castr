# castr

*An R package to process CTD casts.*

This package contains functions to process data collected along CTD casts:

- `smooth()`, `despike()`, or `integrate()` a variable;
- compute the depth of clines with `clined()`, of the deep chlorophyll maximum with `maxd()`, of the bottom of the mixed layer with `mld()`;
- compute a stratification index with `stratif()`.

It also provides utilities to apply a function in a moving window along the cast (`rollapply()`), hence enabling the computation of moving averages (to `smooth()` the data), medians (to detect `spikes()`), etc.

Finally, two small datasets are provided to test the functions: one with a single cast (`d`) and another one with 41 casts (`ctd`).

## Installation

The package is not on CRAN yet. Install it using

```R
# install.packages("devtools")
devtools::install_github("jiho/castr")
```

## Usage

All functions in `castr` work on vectors and make few assumptions regarding your data. Therefore, they fit in the [`tidyverse`](https://www.tidyverse.org) philosophy and work well with [`dplyr`](http://dplyr.tidyverse.org) verbs, as shown in the example below (which is repeated in `?castr`).

```R
# We have a data.frame containing a time series of 41 CTD casts collected
# at the same station over two years, identified by the date of sampling
head(ctd)

# Display the data
library("ggplot2")
ggplot(ctd) + geom_path(aes(x=temp, y=-depth, group=date), alpha=0.6)
# there are a few spikes in temperature

# Despike each profile
library("dplyr")
ctd_clean <- ctd %>% group_by(date) %>%
  mutate(temp=despike(temp, mult=3))
ggplot(ctd_clean) + geom_path(aes(x=temp, y=-depth, group=date), alpha=0.6)
# the spikes are gone! Do the same for all variables
# NB: the despiking parameters should really be tested and adjusted
ctd_clean <- ctd %>% group_by(date) %>%
  mutate(
    temp=despike(temp, mult=3),
    sal=despike(sal, mult=3),
    chla=despike(chla, k=2, mult=4),
    # NB: chlarescence is more spiky so be less stringent
    sigma=despike(sigma, mult=3)
  )
ggplot(ctd_clean) + geom_path(aes(x=sal, y=-depth, group=date), alpha=0.6)
ggplot(ctd_clean) + geom_path(aes(x=chla, y=-depth, group=date), alpha=0.6)

# Now compute summary statistics on each despiked profile
stats <- ctd_clean %>% group_by(date) %>%
  summarise(
    thermocline = clined(temp, depth, n.smooth=2, k=2),
    pycnocline = clined(sigma, depth),
    strat_index = stratif(sigma, depth, min.depths=0:5, max.depth=60:65),
    DCM = maxd(chla, depth, n.smooth=2, k=3),
    MLD = mld(sigma, depth, ref.depths=0:5, default.depth=80),
    # it is even possible to use variables computed above to make the
    # following computations adapted to each cast:
    # average tempeature in the mixed layer only
    temp_avg = integrate(temp, depth, from=0, to=MLD, fun=mean),
    # stock of Chl a within 10 m of the DCM
    chla_dcm_stock = integrate(chla, depth, from=DCM-10, to=DCM+10)
  )
# Inspect the results
ggplot(stats) + geom_path(aes(x=date, y=-thermocline))
# thermocline depth is still messy despite the smoothing, probably
# related to sharp changes in this coastal region.

ggplot(stats) + geom_path(aes(x=date, y=strat_index))
ggplot(stats) + geom_path(aes(x=date, y=-MLD))
# the stratification index and the MLD show a nice seasonality in mixing,
# with shallow MLDs and strong stratification in summer and deep mixing
# in winter.

ggplot(stats) + geom_path(aes(x=date, y=temp_avg))
# average temperature in the mixed layer shows the expected seasonality.

ggplot(stats) + geom_path(aes(x=date, y=chla_dcm_stock))
# the stock of chlorophyll a (estimated by fluorescence) within 10 m on
# either side of the DCM suggests that 2016 had a more productive DCM.
```
