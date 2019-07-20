
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supreme

[![Travis build
status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/strboul/supreme?branch=master&svg=true)](https://ci.appveyor.com/project/strboul/supreme)
[![Coverage
status](https://codecov.io/gh/strboul/supreme/branch/master/graph/badge.svg)](https://codecov.io/github/strboul/supreme?branch=master)

*supreme* helps you better structure your Shiny application developed
with modules

Therefore, you should be able to:

  - Track your modules, which are either the different parts of your
    application, or reused in multiple places, for your existing
    application;

  - Create a modeling, and visualize your results either in to design a
    new application from scratch.

## Installation

``` r
devtools::install_github("strboul/supreme")
```

## Usage

Create a supreme object from a file:

``` r
library(supreme)
path <- example_app_path()
obj <- supreme(src_file(path))
obj
```

You can visualize the module structure:

``` r
graph(obj)
```

Possible to focus on particular modules by changing the style,
especially to make the modules’ parent and child explicit:

``` r
graph(obj, style = list(name = "module1", color = "#fff"))
```

You can also use `%>%` pipes from the
[magrittr](https://cran.r-project.org/package=magrittr) package:

``` r
supreme(src_file(path)) %>%
  graph()
```

It’s possible to see model object in tabular way:

``` r
as.data.frame(obj)
```

## Contribution

This package is still on progress. PRs and issues are much appreciated.

### How does static analysis part work?

*supreme* does a static analysis on the code thanks to *R* allowing to
manipulate *abstract syntax trees* (AST) easily.

## Acknowledgements

  - [**datamodelr**](https://github.com/bergant/datamodelr)

## License

GPL-2.0
