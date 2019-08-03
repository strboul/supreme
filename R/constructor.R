
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
  fields <- c(SUPREME_REQUIRED_FIELDS, SUPREME_OPTIONAL_FIELDS)
  res <- list()
  for (i in seq_along(x)) {
    entity <- x[[i]]
    src <- entity[["src"]]
    entity.body <- entity[["body"]][[1]]
    which.components <- which(sapply(entity.body, is_shiny_server_component))
    for (c in which.components) {
      f.body <- entity.body[[c]]
      name <- find_block_assignment_name(f.body)
      calling_modules <- find_block_calling_modules(f.body)
      out <- list(
        type = "module",
        name = name,
        calling_modules = calling_modules,
        src = src
      )
      res[[length(res)+1L]] <- out
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

