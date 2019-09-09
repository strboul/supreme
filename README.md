
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supreme

[![Repo
status](https://www.repostatus.org/badges/latest/wip.svg)](https://github.com/strboul/supreme)
[![Travis build
status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/strboul/supreme?branch=master&svg=true)](https://ci.appveyor.com/project/strboul/supreme)
[![Coverage
status](https://codecov.io/gh/strboul/supreme/branch/master/graph/badge.svg)](https://codecov.io/github/strboul/supreme?branch=master)

*supreme* helps you structure Shiny applications developed with modules

Therefore, you are able to:

  - **Visualize** relationship of modules in any existing application

  - **Design** new applications from scratch

## Usage

First of all, the application has to be read as a model object by
*supreme*.

For your existing application, you may read it from a file by using
`src_file()`. Please see the vignette for the other options to read your
application as model objects.

After reading, create a *supreme* object from the read model object:

``` r
library(supreme)
path <- example_app_path()
obj <- supreme(src_file(path))
obj
#> A supreme model object
#> 6 entities: server, customers_tab_module_server, items_tab_module_server, items_plot_server, ...
```

Visualize the module structure:

``` r
graph(obj)
```

<img src="inst/media/supreme-graph-example-1.png" width="100%" />

<br>

See the generated *supreme* object in tabular form (as `data.frame` or
`tibble`):

``` r
df <- as.data.frame(obj)
tbbl <- tibble::as_tibble(df)
tbbl
#> # A tibble: 6 x 6
#>   type   name                 input  output  calling_modules src           
#>   <chr>  <chr>                <I(ch> <I(chr> <I(list)>       <chr>         
#> 1 module server               <NA>   <NA>    <chr [3]>       app.R         
#> 2 module customers_tab_modul… <NA>   <NA>    <chr [1]>       module-custom…
#> 3 module items_tab_module_se… <NA>   <NA>    <chr [2]>       module-items.R
#> 4 module items_plot_server    <NA>   <NA>    <chr [1]>       module-items.R
#> 5 module transactions_tab_mo… <NA>   <NA>    <chr [1]>       module-transa…
#> 6 module main_table_server    <NA>   <NA>    <chr [1]>       module-utils.R
```

For designing a new application, or designing a new part in your
existing application, use YAML:

## Read more

The following vignettes contain more detailed information about how to
use *supreme* in different ways.

*(they’re under development)*

  - [**Getting started with
    supreme**](https://strboul.github.io/supreme/articles/getting-started.html)

  - [**Visualize ‘supreme’
    objects**](https://strboul.github.io/supreme/articles/visualize-supreme-objects.html)

## Installation

``` r
devtools::install_github("strboul/supreme")
```

## Contribution

This package is still on progress. **API is subject to change.**

PRs and issues are much appreciated.

## Acknowledgements

  - R Core Team: *supreme* came to life thanks to *R* allowing to
    manipulate *abstract syntax trees* (AST) easily in the code that is
    a huge benefit of practising static analyses.

  - [datamodelr](https://github.com/bergant/datamodelr): Inspiring work
    for creating modelling language

  - [shinypod](https://github.com/ijlyttle/shinypod): Interesting
    thoughts regarding the implementation of Shiny modules

## License

MIT © Metin Yazici
