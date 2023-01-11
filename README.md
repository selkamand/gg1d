
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gg1d

<!-- badges: start -->

[![R-CMD-check](https://github.com/selkamand/gg1d/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/selkamand/gg1d/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/gg1d)](https://CRAN.R-project.org/package=gg1d)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/selkamand/gg1d/branch/master/graph/badge.svg)](https://app.codecov.io/gh/selkamand/gg1d?branch=master)
<!-- badges: end -->

Effortlessly visualize all columns in a data frame with gg1d’s
vertically aligned plots and automatic plot selection based on variable
type. Plots are fully interactive, and custom tooltips can be added.

**Why 1 dimensional plots?**

When trying to understand trends in your data, it’s often helpful to
plot multiple 2D plots. However, there are many applications it’s
extremely useful to densely stack visual representations of each
property in your dataset on top of one another, regardless of data type.
By unifying the x axis across each plot, gg1d allows you to turn a
series of 1D plots into an n-dimensional visualization where n = number
of columns in your data frame. This can be a very useful tool for
various applications, and in my case was developed to annotate oncoplots
with clinical metadata.

Note the key to utility in this endeavour is to ‘preserve the
individual.’ We don’t plot distributions of properties, we plot each
value of a feature for each subject in the dataset.

## Installation

You can install the development version of gg1d from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
devtools::install_github("selkamand/gg1d")
```

## Quick Start

``` r
# Load library
library(gg1d)

# Read data
path_gg1d <- system.file("testdata/testinput.csv", package = "gg1d")
df <- read.csv(path_gg1d, header = TRUE)

# Plot data, sort by Glasses
gg1d_plot(df, col_id = "ID", col_sort = "Glasses")
#> ! Categorical columns must have <= 6 unique values to be visualised. Columns with too many unique values: Date (19)
#> 
#> ── Sorting ─────────────────────────────────────────────────────────────────────
#> • Sorting X axis by: Glasses
#> • Order type: frequency
#> • Sort order: descending
#> 
#> ── Generating Plot ─────────────────────────────────────────────────────────────
#> ℹ Found 7 plottable columns in .data
#> ! Skipping column ID
#> ✔ Plotting column Age
#> ✔ Plotting column Gender
#> ✔ Plotting column EyeColour
#> ✔ Plotting column Height
#> ✔ Plotting column HairColour
#> ✔ Plotting column Glasses
#> ✔ Plotting column WearingHat
#> ! Skipping column WearingHat_tooltip
#> ! Skipping column Date
#> ℹ Stacking plots vertically
```

<img src="man/figures/README-example-1.png" width="100%" />
