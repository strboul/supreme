
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supreme

[![Travis build
status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)

Structure robust Shiny applications with modules

## Installation

``` r
devtools::install_github("strboul/supreme")
```

## Usage

Create a supreme object:

``` r
library(supreme)
fs <- example_app_path()
s <- supreme(fs)
s
```

You can visualize the module structure:

``` r
graph(s)
```

Possible to focus on a particular module to make particular modules’
parent and child modules explicit:

``` r
graph(s, focus = "module1")
```

You can also use `%>%` pipes from the
[magrittr](https://cran.r-project.org/package=magrittr) package:

``` r
fs <- example_app_path()
supreme(fs) %>%
  graph(focus = "module1")
```

## Contribution

This package is still on progress. PRs and issues are much appreciated.

## License

GPL-2.0
