
<!-- README.md is generated from README.Rmd. Please edit that file -->

# waveaggreg

<!-- badges: start -->
<!-- badges: end -->

The goal of waveaggreg is to estimate component curves from aggregated
curves by applying wavelet shrinkage methods.

## Installation

You can install the released version of waveaggreg from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("waveaggreg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(waveaggreg)
#> Loading required package: wavethresh
#> Loading required package: MASS
#> WaveThresh: R wavelet software, release 4.6.8, installed
#> Copyright Guy Nason and others 1993-2016
#> Note: nlevels has been renamed to nlevelsWT
# A = matrix(rnorm(80),16,5)
# y1 = runif(5)
# y = t(matrix(c(y1,1-y1),5,2))
# compfun(A,y,0.9,1,5)
```
# waveaggreg
