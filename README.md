
# supreme

[![Travis build status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)

Structure robust Shiny applications with modules 

## Installation

```r
devtools::install_github("strboul/supreme")
```

## Usage

``` r
files <- list.files(system.file("examples", package = "supreme"), pattern = "\\.R$")
supreme::tree_app(files)
```

## Contribution

This package is still on progress. PRs and issues are much appreciated.

## License

GPL-2.0

