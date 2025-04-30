
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gg1d <a href="https://selkamand.github.io/gg1d/"><img src="man/figures/logo.png" align="right" height="138" alt="gg1d website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gg1d)](https://CRAN.R-project.org/package=gg1d)
[![R-CMD-check](https://github.com/selkamand/gg1d/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/gg1d/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/gg1d/branch/main/graph/badge.svg)](https://app.codecov.io/gh/selkamand/gg1d?branch=main)
![GitHub Issues or Pull
Requests](https://img.shields.io/github/issues-closed/selkamand/gg1d)
[![code
size](https://img.shields.io/github/languages/code-size/selkamand/gg1d.svg)](https://github.com/selkamand/gg1d)
![GitHub last
commit](https://img.shields.io/github/last-commit/selkamand/gg1d)
![r-universe](https://selkamand.r-universe.dev/badges/gg1d)

[![Codecov test
coverage](https://codecov.io/gh/selkamand/gg1d/graph/badge.svg)](https://app.codecov.io/gh/selkamand/gg1d)
<!-- badges: end -->

> \[!WARNING\]  
> We strongly recommend that you migrate to the
> [ggEDA](https://github.com/CCICB/ggEDA) package, which provides a
> richer feature set, ongoing maintenance, and enhanced performance. The
> gg1d package is no longer under active development and will only
> receive essential bug-fixes.

Effortlessly visualize all columns in a data frame with vertically
aligned plots and automatic plot selection based on variable type. Plots
are fully interactive, and custom tooltips can be added.

A graphical user interface to all gg1d visualisations is available at
<https://CCICB.github.io/featurepeeker/>

**Why 1 dimensional plots?**

To understand trends in your data, especially correlative relationships
between 2 or more features, it can be useful to densely stack visual
representations of each feature vertically, regardless of data type. By
unifying the $x$-axis across each plot, **gg1d** turns a series of 1D
plots into an $n\text{-dimensional}$ visualization where
$n = \text{number of columns in dataset}$. Note the key idea of gg1d is
to ‘preserve the individual.’ **gg1d** does **NOT** plot distributions
of properties, but rather each value of a feature for each
subject/observation in the dataset.

gg1d can be used for exploratory data analysis (EDA) or to produce
publication quality graphics summarizing a dataset.

## Installation

``` r
install.packages("gg1d")
```

### Development Version

You can install the development version of gg1d from
[GitHub](https://github.com/) with:

``` r
if (!require("remotes"))
    install.packages("remotes")

remotes::install_github("selkamand/gg1d")
```

Or from R-universe with:

``` r
install.packages("gg1d", repos = "https://ropensci.r-universe.dev")
```

## Quick Start

For examples of interactive gg1d plots see the [gg1d
gallery](https://selkamand.github.io/gg1d/articles/gallery.html)

``` r
# Load library
library(gg1d)

# Read data
path_gg1d <- system.file("example.csv", package = "gg1d")
df <- read.csv(path_gg1d, header = TRUE, na.strings = "")

# Plot data, sort by Glasses
gg1d(
  df,
  col_id = "ID",
  col_sort = "Glasses",
  interactive = FALSE,
  verbose = FALSE,
  options = gg1d_options(legend_nrow = 2)
)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Customise Colours

Customise colours by supplying a named list to the `palettes` argument

``` r
gg1d(
  df,
  col_id = "ID",
  col_sort = "Glasses",
  palettes = list("EyeColour" = c(
    Brown = "rosybrown4",
    Blue = "steelblue",
    Green = "seagreen"
  )),
  interactive = FALSE,
  verbose = FALSE,
  options = gg1d_options(legend_nrow = 2)
)
```

<img src="man/figures/README-customise_colours-1.png" width="100%" />

## Parallel Coordinate Plots

For datasets with many observations and mostly numeric features,
parallel coordinate plots may be more appropriate.

``` r
ggparallel(
 data = minibeans,
 col_colour = "Class",
 order_columns_by = "auto",
 interactive = FALSE
)
#> ℹ Ordering columns based on mutual information with [Class]
```

<img src="man/figures/README-minibeans_class-1.png" width="100%" />

``` r
 ggparallel(
   data = minibeans,
   col_colour = "Class",
   highlight = "DERMASON",
   order_columns_by = "auto",
   interactive = FALSE
 )
#> ℹ Ordering columns based on how well they differentiate 1 group from the rest [DERMASON] (based on mutual information)
```

<img src="man/figures/README-minibeans_highlight-1.png" width="100%" />

``` r
 ggparallel(
   data = minibeans,
   order_columns_by = "auto",
   interactive = FALSE
 )
#> ℹ To add colour to plot set `col_colour` to one of: Class
#> ℹ Ordering columns to minimise crossings
#> ℹ Choosing axis order via repetitive nearest neighbour with two-opt refinement
```

<img src="man/figures/README-minibeans_noclass-1.png" width="100%" />

## Community Contributions

All types of contributions are encouraged and valued. See our [guide to
community
contributions](https://selkamand.github.io/gg1d/CONTRIBUTING.html) for
different ways to help.
