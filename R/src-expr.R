
#' Read \R expressions
#'
#' @param x an \R expression.
#' @family source functions
#' @export
src_expr <- function(x) {
  if (!length(x) == 1L) {
    ncstopf("expression length must be one, instead of: %s", length(x))
  }
  obj <- make_module_entities_from_expression(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_expr"))
}

#' @export
print.src_expr <- function(x, ...) {
  cat("Model expression object", "\n")
}

#' Make module entities from file paths
#'
#' @param x file paths.
#' @noRd
make_module_entities_from_expression <- function(x) {
  out <- list(list(body = x, src = NULL))
  structure(out, class = "module_entities")
}
