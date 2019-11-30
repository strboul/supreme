
#' Constructor call for entities
#'
#' Parse language objects from `module entities`.
#'
#' @param x a list keeping module entities.
#'
#' @noRd
entity_constructor <- function(x) {
  if (!is_module_entities(x)) {
    ncstopf("input not module entities, instead: '%s'", class(x), internal = TRUE)
  }
  res <- list()
  for (i in seq_along(x)) {
    entity <- x[[i]]
    src <- entity[["src"]]
    entity_body <- entity[["body"]][[1]]
    which_components <- which(sapply(entity_body, is_shiny_server_component))
    for (c in which_components) {
      f_body <- entity_body[[c]]
      name <- find_block_assignment_name(f_body)
      calling_modules <- find_block_calling_modules(f_body)
      out <- list(
        name = name,
        calling_modules = calling_modules,
        src = src
      )
      res[[length(res) + 1L]] <- out
    }
  }
  res
}

is_module_entities <- function(x) {
  if (is_list(x) && inherits(x, "module_entities")) {
    TRUE
  } else {
    FALSE
  }
}

