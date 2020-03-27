
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A `ggplot2` and `gganimate` Version of Pac-Man

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/ggpacman.svg?label=latest%20tag)](https://github.com/mcanouil/ggpacman)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-ago/ggpacman)](https://cran.r-project.org/package=ggpacman)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/ggpacman)](https://cran.r-project.org/web/checks/check_results_ggpacman.html)
[![CRAN\_Download\_total](https://cranlogs.r-pkg.org/badges/ggpacman)](https://cran.r-project.org/package=ggpacman)
[![R build
status](https://github.com/mcanouil/ggpacman/workflows/R-CMD-check/badge.svg)](https://github.com/mcanouil/ggpacman/actions)
<!-- badges: end -->

The goal of pacman is to …  
Build a GIF of Pac-Man …

## Installation

``` r
# Install ggpacman from CRAN:
install.packages("ggpacman")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("mcanouil/ggpacman")
```

## Pac-Man in Action

``` r
library(ggpacman)
animate_pacman(
  pacman = pacman,
  ghosts = list(blinky, pinky, inky, clyde),
  font_family = "xkcd"
)
```

![](man/figures/README-pacman-1.gif)<!-- -->

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/mcanouil/ggpacman/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/mcanouil/ggpacman/blob/master/.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
