
# supreme 1.1.0 (2020-07-05)

+ Preparation for [CRAN](https://cran.r-project.org/package=supreme).


# supreme 1.0.0 (2019-12-25)

+ Refactored the `find_*` calls

+ Improved graph facility: introduced shapes for different fields e.g. input, output,
  return

+ Made graphing more customizable: allowed to choose fields, styles etc.

+ Removed some `src_*` calls (kept only `src_yaml()` and `src_file()`) because they
  were too expensive to maintain given the scope of the project

+ Added new unit & integration tests; increased the overall code coverage

+ Improved the modeling language


# supreme 0.2.1

+ Make the exported variables in `R/globals.R` options placed in `R/zzz.R`.


# supreme 0.2.0 (2019-08-07)

+ Create example functions in the API for demonstration purposes, e.g.
`example_app_path()`.

+ Use [nomnoml](https://cran.r-project.org/package=nomnoml) package for the `graph()`
facility.

+ Implement reading functions for files (`src_file()`), packages (`src_pkg()`),
environments (`src_env()`), expressions (`src_expr()`) and YAML objects
(`src_yaml()`).


# supreme 0.1.0 (2019-02-06)

+ Added a `NEWS.md` file to track changes to the package.

+ Package creation and initial work.

