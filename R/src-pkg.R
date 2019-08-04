
#' Read an \R package
#'
#' The package should contain some Shiny application.
#'
#' @param x package name installed in the system.
#' @export
src_pkg <- function(x) {
  ## enforce input to be a character.
  if (!is.character(x)) x <- as.character(x)
  if (!is_package_exist(x)) {
    ncstopf("package '%s' not found.", x)
  }
  obj <- make_module_entities_from_package(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_pkg"))
}

#' @export
print.src_pkg <- function(x, ...) {
  cat("Model package object", "\n")
}

#' @param x package name.
#' @noRd
is_package_exist <- function(x) {
  stopifnot(is.character(x))
  pkg <- find.package(x, quiet = TRUE)
  if (length(pkg) >= 1L) {
    TRUE
  } else {
    FALSE
  }
}

#' Make module entities from file paths
#'
#' This call does not take all objects from a package, instead it eliminates in some
#' way. An object from the specified package will be eliminated;
#' 1. if it is not a function,
## 2. (it is a function), but if it is not a shiny component (either server or
## module).
#'
#' @param x character. package name.
#' @importFrom utils packageName
#' @noRd
make_module_entities_from_package <- function(x) {
  stopifnot(is.character(x))
  ns <- asNamespace(x)
  ## verify package name:
  pkg.name <- utils::packageName(ns)
  all_calls <- ls(ns, all.names = TRUE)
  exprs <- list()
  for (i in seq_along(all_calls)) {
    c.name <- all_calls[i]
    fun <- get(c.name, envir = ns)
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
  out <- list(list(body = exprs, src = paste("package", pkg.name, sep = ":")))
  structure(out, class = "module_entities")
}

