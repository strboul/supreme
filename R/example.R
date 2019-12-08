
#' Example description
#'
#' @details
#' Please note that all examples give the same output.
#'
#' @name example_description
NULL

#' Get paths to `supreme` example
#'
#' Contains the paths of the Shiny application that is a fully-fledged example
#' endeavors to demonstrate all the features `supreme` has.
#'
#' @param file file names. If no file names are put (which `path` is `NULL`), then
#'   all the example file paths will be listed.
#'
#' @inherit example_description details
#' @examples
#' files <- example_app_path(c("app", "module-customers"))
#' supreme(src_file(files))
#' @family source examples
#' @export
example_app_path <- function(file = NULL) {
  pat <- file.path("extdata", "file")
  pkg <- system.file(pat, package = "supreme", mustWork = TRUE)
  files <- list.files(pkg, pattern = "\\.R$", full.names = TRUE)
  if (is.null(file)) {
    files
  } else {
    files[grep(file, files)]
  }
}


#' Get YAML to `supreme` example
#'
#' @inherit example_description details
#' @examples
#' yaml <- example_yaml()
#' supreme(src_yaml(env))
#' @family source examples
#' @export
example_yaml <- function() {
  pat_file <- file.path("extdata", "yaml", "example-model.yaml")
  yaml <- system.file(pat_file, package = "supreme", mustWork = TRUE)
  yaml
}


#' Get environment to `supreme` example
#'
#' @inherit example_description details
#' @examples
#' env <- example_environment()
#' supreme(src_env(env))
#' @family source examples
#' @export
example_environment <- function() {
  env <- new.env()
  sapply(rev(example_app_path()), function(x) source(x, env))
  env
}


#' Get expression to `supreme` example
#'
#' @inherit example_description details
#' @examples
#' expr <- example_expression()
#' supreme(src_expr(expr))
#' @family source examples
#' @export
example_expression <- function() {
  expr <- .read_srcfile(example_app_path())
  expr
}

