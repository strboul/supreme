
#' The main constructor call for all `module entities`
#'
#' @description
#' Parses language objects from `module entities`.
#'
#' @param x a list storing module entities.
#'
#' @noRd
entity_constructor <- function(x) {

  stopifnot(is_supreme_module_entities(x))

  res <- list()
  for (i in seq_along(x)) {

    entity <- x[[i]]
    src <- entity[["src"]]
    entity_body <- entity[["body"]][[1]]
    which_components <- which(vapply(entity_body, is_shiny_server_component, logical(1)))

    for (c in which_components) {

      fun_block <- entity_body[[c]]

      name <- find_binding_name(fun_block)

      inputs <- find_inputs(fun_block)
      ## exclude the compulsory Shiny input fields:
      inputs <- setdiff(inputs, c("input", "output", "session"))

      outputs <- find_outputs(fun_block)
      returns <- find_returns(fun_block)
      calling_modules <- find_calling_modules(fun_block)

      ## Add fields:
      out <- list(name = name)
      if (length(inputs) > 0L) {
        out <- c(out, list(input = inputs))
      }
      if (length(outputs) > 0L) {
        out <- c(out, list(output = outputs))
      }
      if (length(returns) > 0L) {
        out <- c(out, list(return = returns))
      }
      if (length(calling_modules) > 0L) {
        out <- c(out, list(calling_modules = calling_modules))
      }
      if (length(src) > 0L) {
        out <- c(out, list(src = src))
      }

      ## assign to result:
      res[[length(res) + 1L]] <- out
    }
  }
  res <- structure(res, class = "supreme_entity_constructor")
  check_duplicate_module_names(res)
  res
}


check_duplicate_module_names <- function(x) {
  stopifnot(is_supreme_entity_constructor(x))
  ## sapply->vapply failed because sometimes names are NULL
  mod_names <- sapply(x, `[[`, "name")
  if (anyDuplicated(mod_names) > 0) {
    ncstopf(
      "duplicated module names in the source: %s",
      paste(
        paste0("'",
               unique(mod_names[duplicated(mod_names)]),
               "'"),
        collapse = ", ")
    )
  }
}


is_supreme_entity_constructor <- function(x) {
  is_list(x) && inherits(x, "supreme_entity_constructor")
}


is_supreme_module_entities <- function(x) {
  is_list(x) && inherits(x, "supreme_module_entities")
}

