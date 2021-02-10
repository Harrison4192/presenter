
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presenteR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/presenteR)](https://CRAN.R-project.org/package=presenteR)
[![R-CMD-check](https://github.com/Harrison4192/presenteR/workflows/R-CMD-check/badge.svg)](https://github.com/Harrison4192/presenteR/actions)

[![](http://cranlogs.r-pkg.org/badges/grand-total/presenteR?color=blue)](https://cran.r-project.org/package=presenteR)
[![](https://img.shields.io/github/languages/code-size/Harrison4192/presenteR.svg)](https://github.com/Harrison4192/presenteR)
[![](https://img.shields.io/github/last-commit/Harrison4192/presenteR.svg)](https://github.com/Harrison4192/presenteR/commits/master)
<!-- badges: end -->

This package consists of custom wrapper functions using packages
openxlsx, flextable, and officer to create highly formatted MS office
friendly output of your data frames. These viewer friendly outputs are
intended to match expectations of professional looking presentations in
business and consulting scenarios. The functions are opinionated in the
sense that they expect the input data frame to have certain properties
in order to take advantage of the automated formatting.

## Installation

You can install the released version of presenteR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("presenteR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Harrison4192/presenteR")
```
