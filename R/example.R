
#' Get paths to `supreme` example
#'
#' Contains the paths of the Shiny application that is a fully-fledged example
#' endeavors to demonstrate all the features `supreme` has.
#'
#' @param file file names. If no file names are put (which `path` is `NULL`), then
#'   all the example file paths will be listed.
#'
#' @examples
#' example_app_path()
#' example_app_path(c("app.R", "module-customers.R"))
#' @export
example_app_path <- function(file = NULL) {
  pkg <- system.file("extdata", package = "supreme", mustWork = TRUE)
  files <- list.files(pkg, full.names = TRUE)
  if (is.null(file)) {
    files
  } else {
    files[basename(files) %in% file]
  }
}

