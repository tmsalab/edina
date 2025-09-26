

<!-- README.md is generated from README.qmd. Please edit that file -->

# edina

<!-- badges: start -->

[![R-CMD-check](https://github.com/tmsalab/edina/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tmsalab/edina/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Perform a Bayesian estimation of the Exploratory Deterministic Input,
Noisy “And” Gate (EDINA) cognitive diagnostic model described by Chen et
al. (2018).

## Installation

You can install `edina` from CRAN using:

``` r
install.packages("edina")
```

Or, you can be on the cutting-edge development version on GitHub using:

``` r
if(!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("tmsalab/edina")
```

## Usage

To use the `edina` package, load it into *R* using:

``` r
library("edina")
```

From there, the EDINA model can be estimated using:

``` r
edina_model = edina(<data>, chain_length = 10000)
```

To compute a model underneath different *K* attribute configured *Q*
matrices, use:

``` r
edina_model = auto_edina(<data>, k = 2:4, chain_length = 10000)
```

**Note:** Higher *K* configured *Q* matrices take longer to estimate.

## Authors

James Joseph Balamuta, Steven Andrew Culpepper, and Jeffrey A. Douglas

## Citing the `edina` package

To ensure future development of the package, please cite `edina` package
if used during an analysis or simulation studies. Citation information
for the package may be acquired by using in *R*:

``` r
citation("edina")
```

## License

GPL (\>= 2)
