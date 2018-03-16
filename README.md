# castr

*Deal with CTD casts in R.*

This package contains functions to process data collected during CTD casts: `smooth()`, `despike()`, or `integrate()` a variable, compute the depth of clines (`clined()`), of the deep chlorophyll maximum (`maxd()`), of the bottom of the mixed layer (`mld()`), the stratification index (`stratif()`). It also provides utilities to apply a function over a moving window along the cast (`rollapply()`), hence enabling the computation of moving averages, medians, etc.

## Installation

The package is not on CRAN yet. Install it using

```{r}
# install.packages("devtools")
devtools::install_github("jiho/castr")
```
