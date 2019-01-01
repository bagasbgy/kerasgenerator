
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kerasgenerator <img src="man/figures/logo.png" align="right" height=140/>

`kerasgenerator` contain some data generator functions which designed to
make the data preparation process for
[`keras`](https://keras.rstudio.com)’
[`fit_generator()`](https://keras.rstudio.com/reference/fit_generator.html)
easier.

## Installation

The package is in early development stage and currently only available
via GitHub. You can install the development version using this way:

``` r
# Install development version from GitHub
devtools::install_github("bagasbgy/kerasgenerator")
```

## Further Development

  - Support for generating data from **file in specified path**
  - Support for incorporating
    [`recipes`](https://tidymodels.github.io/recipes/) package for
    **data preprocessing**
  - Data generator for **k-shot learning** model fitting
