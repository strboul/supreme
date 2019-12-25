
<!-- README.md is generated from README.Rmd. Please edit that file -->

# supreme <a href='https://github.com/strboul/supreme/'><img src='https://raw.githubusercontent.com/strboul/supreme/master/inst/media/logo.png' align="right" height="135"></a>

[![Travis build
status](https://travis-ci.org/strboul/supreme.svg?branch=master)](https://travis-ci.org/strboul/supreme)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/strboul/supreme?branch=master&svg=true)](https://ci.appveyor.com/project/strboul/supreme)
[![CRAN status
badge](https://www.r-pkg.org/badges/version/supreme)](https://cran.r-project.org/package=supreme)
[![CRAN mirror
downloads](https://cranlogs.r-pkg.org/badges/supreme)](https://www.r-pkg.org/pkg/supreme)
[![Coverage
status](https://codecov.io/gh/strboul/supreme/branch/master/graph/badge.svg)](https://codecov.io/github/strboul/supreme?branch=master)

*supreme* is a modeling tool helping you better structure Shiny
applications developed with modules.

Therefore, you are able to:

1.  **Visualize** relationship of modules in *existing applications*

2.  **Design** *new applications* from scratch

## Installation

You can install the released version from
[CRAN](https://cran.r-project.org/package=supreme):

``` r
install.packages("supreme")
```

Or get the development version from
[GitHub](https://github.com/strboul/supreme):

``` r
# install.packages("devtools")
devtools::install_github("strboul/supreme")
```

## Usage

### Existing applications

For your existing application, you can use `src_file()` call that reads
your application from files.

*supreme* package comes with an example path containing a dummy Shiny
application created with modules for testing issues (in
`example_app_path()`).

After the application has been read, create a *supreme* object from the
model object:

``` r
library(supreme)
path <- example_app_path()
obj <- supreme(src_file(path))
obj
#> A supreme model object
#> 5 entities: server, customers_tab_module_server, items_tab_module_server, transactions_tab_module_server, ...
```

See the generated *supreme* object in tabular form a.k.a. `data.frame`:

``` r
as.data.frame(obj)
#>                             name                 input
#> 1                         server                    NA
#> 2    customers_tab_module_server        customers_list
#> 3        items_tab_module_server  items_list, is_fired
#> 4 transactions_tab_module_server table, button_clicked
#> 5            module_modal_dialog                  text
#>                                       output            return calling_modules
#> 1                                         NA              <NA>    list(ite....
#> 2 paid_customers_table, free_customers_table              <NA>              NA
#> 3                                         NA              <NA>    list(mod....
#> 4                         transactions_table transactions_keys              NA
#> 5                                         NA              <NA>              NA
#>                     src
#> 1                 app.R
#> 2    module-customers.R
#> 3        module-items.R
#> 4 module-transactions.R
#> 5        module-utils.R
```

Finally, visualize the module structure:

``` r
graph(obj)
```

<img src="man/figures/README-supreme-graph-example-1.png" width="100%" />

<br>

### Creating YAML model objects

Model definition with *YAML* is a handy to design a **new application**
from scratch or just design the specific parts of applications. Planning
ahead with model tool can really be beneficial before going wild on
implementation.

``` yaml
- name: server
  calling_modules:
    - items_tab_module_server: ItemsTab
    - customers_tab_module_server: CustomersTab
    - transactions_tab_module_server: TransactionsTab
  src: app.R

- name: customers_tab_module_server
  input: customers_list
  output:
    - paid_customers_table
    - free_customers_table
  src: module-customers.R

- name: items_tab_module_server
  input:
    - items_list
    - is_fired
  calling_modules:
    - module_modal_dialog: ~
  src: module-items.R

- name: transactions_tab_module_server
  input:
    - table
    - button_clicked
  output: transactions_table
  return: transactions_keys
  src: module-transactions.R

- name: module_modal_dialog
  input:
    - text
  src: module-utils.R
```

There are some special rules when creating model objects with *YAML*:

  - Each entity in the model must have a *name* field.

  - The entities can have optional fields, which are *input*, *output*,
    *return*, *calling\_modules* and *src*.

  - The fields *input*, *output*, *return* and *calling\_modules* can
    have multiple elements means that these fields can contain an array
    in the YAML. The other fields can only have a single element.

  - Any other field, which is not known by the *supreme* modal object,
    is not allowed and it will throw an error.

After all, use `src_yaml()` to read modeling from the YAML file:

``` r
model_yaml <- src_yaml(text = model)
obj <- supreme(model_yaml)
```

See the next section to understand how the [model
language](#model%20language) works.

### The model language

A *supreme* object is consisted by *entities*. An *“entity”* denotes
here that a Shiny server component is allowed to either be a server side
of a module or the main `server` function of a Shiny application.

<br/>

<img src="https://raw.githubusercontent.com/strboul/supreme/master/inst/media/supreme-diagram.png" width="90%" style="display: block; margin: auto;" />

<br/>

A graph entity consists of five main fields:

1.  Module name

2.  Module inputs (except the defaults *input*, *output*, *session*)

3.  Module outputs

4.  Module returns

5.  Calling modules, which are modules called by the module

## Known limitations

  - Although it’s possible to create a Shiny application by only
    providing `input` and `output` arguments in the server side,
    *supreme* will not read any Shiny server side component missing a
    `session` argument. That’s reasonable decision because modules
    cannot work without `session` argument and supreme is a package
    designed to work with Shiny modules.

  - For the module returns, all return values in a module should
    explicitly be wrapped in `return()` call.

  - *supreme* will not properly parse the source code of your
    application if server side component is created with
    `shinyServer()`, which is kind of soft-deprecated after a very early
    Shiny version `0.10`.

  - Some idiosyncratic Shiny application code may not be parsed as
    intended. For such cases, it would be great if you open an issue
    describing the situation with a reproducible example.

## Acknowledgment

  - [R Core Team](https://www.r-project.org/): *supreme* package is
    brought to life thanks to *R* allowing *abstract syntax trees* (AST)
    that is used to practice static analysis on the code.

  - [datamodelr](https://github.com/bergant/datamodelr): Inspiring work
    for creating modeling language

  - [shinypod](https://github.com/ijlyttle/shinypod): Interesting
    thoughts regarding the implementation of Shiny modules

## License

MIT © Metin Yazici
