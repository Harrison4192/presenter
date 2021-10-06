
<!-- README.md is generated from README.Rmd. Please edit that file -->

# presenter

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/presenter)](https://CRAN.R-project.org/package=presenter)
[![R-CMD-check](https://github.com/Harrison4192/presenter/workflows/R-CMD-check/badge.svg)](https://github.com/Harrison4192/presenter/actions)

[![](http://cranlogs.r-pkg.org/badges/grand-total/presenter?color=blue)](https://cran.r-project.org/package=presenter)
[![](https://img.shields.io/github/languages/code-size/Harrison4192/presenter.svg)](https://github.com/Harrison4192/presenter)
[![](https://img.shields.io/github/last-commit/Harrison4192/presenter.svg)](https://github.com/Harrison4192/presenter/commits/main)
<!-- badges: end -->

## Installation

You can Install the released version of presenter from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("presenter")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Harrison4192/presenter")
```

## Package Description

This package consists of custom wrapper functions using packages
[openxlsx](https://ycphs.github.io/openxlsx/index.html),
[flextable](https://davidgohel.github.io/flextable/), and
[officer](https://davidgohel.github.io/officer/) to create highly
formatted MS office friendly output of your data frames. This should be
useful to anyone who relies on sharing information in excel or
powerpoint in business and consulting scenarios. The functions are
opinionated in the sense that they expect the input data frame to have
certain properties in order to take advantage of the automated
formatting. Certain novel features of this package include:

-   **Supports Lists:** `make_simple_excel` and `make_powerpoint` can
    accept single tables or lists of tables, which will be given
    separate sheets

-   **Automatic Naming:** You can pipe your data frame, list, or
    flextable directly into the function. An excel or powerpoint will be
    generated with no further arguments necessary, named after your
    table. In order to avoid name clashing, a random string is appended
    to the name so files will not be overwritten. In the case of a
    multi-pipe operation piped into your function, a generic name will
    be randomly generated.

-   **Automatic Opening:** Files are automatically opened upon
    generation so you can inspect your output.

-   **header words:** By supplying a character vector to the
    “header\_word” argument, you can automatically merge or color
    adjacent column headers based off a common key word. This
    functionality relies on word separation with “.” or "\_" and will
    not work properly in other cases like snakeCase. Can be left blank
    for no special formatting.

-   **last id col:** This package is optimized for tables that begin
    with their “id” or “group” columns and then followed by “value”
    columns. By supplying an integer position of the last id column, you
    can apply special formatting for the id cols. Can be left blank for
    no special formatting.
