
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skater

<!-- badges: start -->

[![R-CMD-check-stable](https://github.com/signaturescience/skater/workflows/R-CMD-check-stable/badge.svg)](https://github.com/signaturescience/skater/actions)

[![R-CMD-check-dev](https://github.com/signaturescience/skater/workflows/R-CMD-check-dev/badge.svg)](https://github.com/signaturescience/skater/actions)
<!-- badges: end -->

**S**NP-based **K**inship **A**nalysis **T**esting and **E**valuation:
miscellaneous **R** data analysis utilties.

## Installation

Install from GitHub:

``` r
remotes::install_github("signaturescience/skater", build_vignettes=TRUE)
```

A GitHub [personal access token](https://github.com/settings/tokens)
must be in the environment variable `GITHUB_PAT` or supplied as a string
to `auth_token`. See `?remotes::install_github`.

## Usage

The “Basic Usage” vignette steps through the primary functionality of
the `skater` package:

``` r
vignette("basic_usage", package = "skater")
```
