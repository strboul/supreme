
#' Make module entities from an environment
#'
#' @param x an environment.
#' @details
#' This call does not take all objects from an environment. Objects from a given
#' environment will be eliminated:
#' 1. if it is not a function,
#' 2. (it is a function), but if it is not a Shiny component.
#' @noRd
make_module_entities_from_environment <- function(x) {
  stopifnot(is.environment(x))
  objs <- mget(ls(x, all.names = TRUE), envir = x)
  funs <- Filter(is.function, objs)
  server_components <- Filter(is_shiny_server_component, funs)
  exprs <- vector("list", length(server_components))
  for (i in seq_along(server_components)) {
    elem <- server_components[i]
    elem.name <- names(elem)
    elem.sub <- elem[[1L]]
    Arguments <- formals(elem.sub)
    Body <- body(elem.sub)
    Function <- substitute({name <- fun}, list(
      name = as.name(elem.name),
      fun = as.function(x = c(Arguments, Body))
    ))[[2L]]
    # TODO maybe tlist?
    exprs[[length(exprs) + 1L]] <- Function
  }
  exprs <- as.call(c(as.name("{"), exprs))
  exprs <- as.expression(exprs)
  exprs
}

#' Make module entities from packages
#'
#' (Packages are essentially environments)
#'
#' @importFrom utils packageName
#' @noRd
make_src_pkg <- function(pkg.name) {
  stopifnot(is.character(pkg.name))
  ns <- asNamespace(pkg.name)
  ## verify package name
  stopifnot(length(utils::packageName(asNamespace(pkg.name))) > 0L)
  entities <- make_module_entities_from_environment(ns)
  out <- list(list(body = entities, src = paste("package", pkg.name, sep = ":")))
  structure(out, class = "module_entities")
}

#' Make module entities from environment objects
#'
#' @noRd
make_src_env <- function(envir) {
  stopifnot(is.environment(envir))
  entities <- make_module_entities_from_environment(envir)
  out <- list(list(body = entities, src = "expression"))
  structure(out, class = "module_entities")
}

