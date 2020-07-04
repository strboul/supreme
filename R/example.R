#' Get paths to supreme example
#'
#' The example Shiny application to demonstrate all the capabilities of what
#' `supreme` offers.
#'
#' @param file file names. If no file names are put (which `path` is `NULL`), then
#'   all the example file paths will be listed.
#'
#' @return a character vector containing the R file path for the example.
#' @examples
#' files <- example_app_path(c("app", "module-customers"))
#' supreme(src_file(files))
#' @family source examples
#' @export
example_app_path <- function(file = NULL) {
  pat <- file.path("extdata", "file")
  pkg <- system_file(pat)
  files <- list.files(pkg, pattern = "\\.R$", full.names = TRUE)
  if (is.null(file)) {
    files
  } else {
    files[grep(file, files)]
  }
}


#' Get YAML to supreme example
#'
#' @return a character vector containing the YAML file path for the example.
#' @examples
#' yaml <- example_yaml()
#' supreme(src_yaml(yaml))
#' @family source examples
#' @export
example_yaml <- function() {
  pat_file <- file.path("extdata", "yaml", "example-model.yaml")
  yaml <- system_file(pat_file)
  yaml
}

system_file <- function(pat) {
  system.file(pat, package = "supreme", mustWork = TRUE)
}

