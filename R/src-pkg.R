
#' Read an \R package
#'
#' The package should contain some Shiny application.
#'
#' @param x a package name as character (must be installed in the system).
#' @family source functions
#' @export
src_pkg <- function(x) {
  ## enforce input to be a character.
  if (!is.character(x)) x <- as.character(x)
  if (!is_package_exist(x)) {
    ncstopf("package '%s' not found.", x)
  }
  obj <- make_src_pkg(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_pkg"))
}

#' @export
print.src_pkg <- function(x, ...) {
  cat("Model package object", "\n")
}

