
# supreme

[![Travis build status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)

Structure robust Shiny applications with modules

## Installation

``` r
# install.packages("supreme")
devtools::install_github("strboul/supreme")
```

## Usage

``` r
file <- system.file(file.path("examples", "sample-module-app.R"), package = "supreme")
supreme::module_tree(file)
```

## License

GPL-2.0
