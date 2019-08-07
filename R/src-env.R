
#' @export
src_env <- function(x) {
  if (!is.environment(x)) {
    ncstopf("input must be environment, not:", typeof(x))
  }
  obj <- make_module_entities_from_environment(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_env"))
}

#' @export
print.src_env <- function(x, ...) {
  cat("Model environment object", "\n")
}

make_module_entities_from_environment <- function(x) {
  all_calls <- ls(x, all.names = TRUE)
  exprs <- list()
  for (i in seq_along(all_calls)) {
    c.name <- all_calls[i]
    fun <- get(c.name, envir = x)
    if (!is.function(fun)) {
      next
    }
    form <- formals(fun)
    bod <- body(fun)
    substd <- substitute({name <- fun}, list(
      name = as.name(c.name),
      fun = as.function(x = c(form, bod))
    ))
    ## subset body:
    substd <- substd[[2L]]
    # TODO maybe tlist?
    exprs[[length(exprs) + 1L]] <- substd
  }

  exprs <- as.call(c(as.name("{"), exprs))
  exprs <- as.expression(exprs)
  out <- list(list(body = exprs, src = "expression"))
  structure(out, class = "module_entities")
}

