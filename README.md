
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supreme <a href='https://github.com/strboul/supreme/'><img src='https://raw.githubusercontent.com/strboul/supreme/master/inst/media/logo.png' align="right" height="135"></a>

[![Repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://github.com/strboul/supreme)
[![Travis build
status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/strboul/supreme?branch=master&svg=true)](https://ci.appveyor.com/project/strboul/supreme)
[![Coverage
status](https://codecov.io/gh/strboul/supreme/branch/master/graph/badge.svg)](https://codecov.io/github/strboul/supreme?branch=master)

*supreme* is a modeling tool helping you better structure Shiny
applications developed with modules

Therefore, you are able to:

1.  **Visualize** relationship of modules in *existing applications*

2.  **Design** *new applications* from scratch

## Installation

``` r
# install.packages("devtools")
devtools::install_github("strboul/supreme")
```

## Usage

### Existing applications

For your existing application, you can use `src_file()` call that reads
your application from files.

*supreme* package comes with an example path containing a dummy Shiny
application created with modules for testing issues.

After the application has been read, create a *supreme* object from the
read model object:

``` r
library(supreme)
path <- example_app_path()
obj <- supreme(src_file(path))
obj
#> A supreme model object
#> 6 entities: server, customers_tab_module_server, items_tab_module_server, items_plot_server, ...
```

If you want to run *supreme* on your application that is stored in a
different format (as a package, environment etc.), please see the
`?src_file` documentation page that has information about the other
options to read your application as *supreme* source model objects.

See the generated *supreme* object in tabular form (as `data.frame` or
`tibble`):

``` r
df <- as.data.frame(obj)
tbbl <- tibble::as_tibble(df)
tbbl
#> # A tibble: 6 x 7
#>   type   name             input   output return calling_modules src        
#>   <chr>  <chr>            <I<chr> <I<ch> <chr>  <I<list>>       <chr>      
#> 1 module server           <NA>    <NA>   <NA>   <chr [3]>       app.R      
#> 2 module customers_tab_m… <NA>    <NA>   <NA>   <chr [1]>       module-cus…
#> 3 module items_tab_modul… <NA>    <NA>   <NA>   <chr [2]>       module-ite…
#> 4 module items_plot_serv… <NA>    <NA>   <NA>   <chr [1]>       module-ite…
#> 5 module transactions_ta… <NA>    <NA>   <NA>   <chr [1]>       module-tra…
#> 6 module main_table_serv… <NA>    <NA>   <NA>   <chr [1]>       module-uti…
```

Visualize the module structure:

``` r
graph(obj)
```

<img src="man/figures/README-supreme-graph-example-1.png" width="100%" />

<br>

### Creating YAML model objects

Model definition with *YAML* is a handy to design a new application from
scratch or just design the specific parts of applications. Planning
ahead with model tool can really be beneficial before going wild on
implementation.

``` yaml
- type: module
  name: server
  calling_modules: 
    - items_tab_module_server
    - customers_tab_module_server 
    - transactions_tab_module_server
    
- type: module
  name: items_tab_module_server
  input: 
    - items_list
    - is_fired
  src: inventory
  
- type: module
  name: customers_tab_module_server
  input: customers_list
  output: 
    - paid_customers_table
    - free_customers_table
  src: sales
    
- type: module
  name: transactions_tab_module_server
  output: transactions_table
  return: transactions_keys
  src: sales
```

``` r
library(supreme)
model_yaml <- src_yaml(text = model)
obj <- supreme(model_yaml)
tibble::as_tibble(as.data.frame(obj))
#> # A tibble: 4 x 7
#>   type   name             input   output  return     calling_modules src   
#>   <chr>  <chr>            <list>  <list>  <chr>      <I<list>>       <chr> 
#> 1 module server           <chr [… <chr [… <NA>       <chr [3]>       <NA>  
#> 2 module items_tab_modul… <chr [… <chr [… <NA>       <chr [1]>       inven…
#> 3 module customers_tab_m… <chr [… <chr [… <NA>       <chr [1]>       sales 
#> 4 module transactions_ta… <chr [… <chr [… transacti… <chr [1]>       sales
```

There are some special rules when creating model objects with *YAML*:

  - Each entity in the model must have *type* and *name* fields.

  - The entities can have optional fields, which are *input*, *output*,
    *return*, *calling\_modules* and *src*.

  - The fields *input*, *output* and *calling\_modules* can have
    mutliple elements. The others can only have a single element.

Any other field, which is not known by the supreme modal object (like a
`shape: star`), added to the model definition is not allowed and it will
throw an error.

Finally, we can visualize the supreme mode object we created above from
*YAML* model:

``` r
graph(obj)
```

<img src="man/figures/README-yaml-graph-1.png" width="100%" />

See the next session to understand how this [model
language](#model%20language) works.

### The model language

A *supreme* object is consisted by *entities*. An *“entity”* denotes
here that a Shiny server component is allowed to either be a server side
of a module or the main `server` function of a Shiny application.

<img src="inst/media/supreme-diagram.png" width="90%" style="display: block; margin: auto;" />

<br/>

A graph entity consists of five main fields:

1.  Module name

2.  Module inputs (except the defaults *input*, *output*, *session*)

3.  Module outputs

4.  Return from the module

5.  Modules called by the module

## Current limitations

  - Although it’s possible to create a Shiny application by only
    providing `input` and `output` arguments in the server side,
    *supreme* will not read any Shiny server side component missing a
    `session` argument. That’s reasonable decision because modules
    cannot work without `session` argument and supreme is a package
    designed to work with Shiny modules.

  - *supreme* will not properly parse the source code of your
    application if server side component is created with
    `shinyServer()`, which is kind of soft-deprecated after a very early
    Shiny version `0.10`.

  - Some idiosyncratic Shiny application code may not be parsed as
    intended. For such cases, it would be great if you open an issue
    describing the situation with a reproducible example.

## Acknowledgements

  - [R Core Team](https://www.r-project.org/): *supreme* package is
    brought to life thanks to *R* allowing *abstract syntax trees* (AST)
    that is used to practice static analysis on the code.

  - [datamodelr](https://github.com/bergant/datamodelr): Inspiring work
    for creating modelling language

  - [shinypod](https://github.com/ijlyttle/shinypod): Interesting
    thoughts regarding the implementation of Shiny modules

## License

MIT © Metin Yazici
