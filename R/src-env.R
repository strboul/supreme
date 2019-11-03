
#' Read \R environment
#'
#' @param x an \R environment.
#' @family source functions
#' @export
src_env <- function(x) {
  if (!is.environment(x)) {
    ncstopf("input must be environment, not:", typeof(x))
  }
  obj <- make_src_env(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_env"))
}

#' @export
print.src_env <- function(x, ...) {
  cat("Model environment object", "\n")
}

