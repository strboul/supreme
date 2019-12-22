
#' The main constructor call for all `module entities`
#'
#' @description
#' Parses language objects from `module entities`.
#'
#' @param x a list storing module entities.
#'
#' @noRd
entity_constructor <- function(x) {
  stopifnot(is_module_entities(x))
  res <- list()
  for (i in seq_along(x)) {
    entity <- x[[i]]
    src <- entity[["src"]]
    entity_body <- entity[["body"]][[1]]
    which_components <- which(sapply(entity_body, is_shiny_server_component))

    for (c in which_components) {

      fun_block <- entity_body[[c]]

      name <- find_binding_name(fun_block)
      # TODO input, output, return
      calling_modules <- find_calling_modules(fun_block)

      ## FIXME: workaround until adding ui to calling_modules:
      wa_calling_modules <- lapply(calling_modules, function(x) {
        elem <- list(NULL)
        names(elem) <- x
        elem
      })

      ## add fields:
      out <- list(name = name)
      if (length(calling_modules) > 0) {
        out <- c(out, list(calling_modules = wa_calling_modules))
      }
      if (length(src) > 0) {
        out <- c(out, list(src = src))
      }

      ## assign to result:
      res[[length(res) + 1L]] <- out
    }
  }
  res
}

is_module_entities <- function(x) {
  is_list(x) && inherits(x, "supreme_module_entities")
}

