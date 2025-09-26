# edina 0.1.2

## Changes

- Added a pkgdown website for the R package.
- Added explicit dependencies on R (>= 4.3.0), Rcpp (>= 1.1.0), and RcppArmadillo (>= 15.0.2-2)
- Removed CXX11 from `src/Makevars` and `src/Makevars.win` to avoid potential compilation issues
  with newer versions of Armadillo through RcppArmadillo.
- Switched README.Rmd to README.qmd to use Quarto for rendering.
- Fixed CITATION file to use `c()` instead of `personList()` and `bibentry()` to
  avoid CRAN check notes.
- Updated GitHub Action workflows.

# edina 0.1.1

## Bug Fix

- Fix ambiguous overloaded of `pow` on Solaris.

## Documentation

- Added CRAN badges to README.

# edina 0.1.0

## Features

- Provides a high-performing modeling routine for the Exploratory 
  Deterministic Input, Noisy "And" Gate model.
- Estimate Q matrices with varying _k_ values and compare.
