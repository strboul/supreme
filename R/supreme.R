
#' Wrapper around cat for tree printing
#' @noRd
tree_cat <- function(...) {
  cat(paste(..., sep = ""), "\n")
}

#' Checks if an object is a list
#' @noRd
is_list <- function(x) {
  inherits(x, "list")
}

#' Get server block
#'
#' @param app.body parsed body separated into lists.
#' @return returns the index of the server from the body.
#' @noRd
get_server_block <- function(lbody) {
  find_block(lbody, "server")
}

#' Find a function block with its assignment name
#'
#' @param name variable name used in assignment.
#' @noRd
find_block <- function(lbody, name) {
  for (i in seq_along(lbody)) {
    lbody.sub <- lbody[[i]]
    lbody.sub.name <- lbody.sub[[2L]]
    if (lbody.sub.name == name) {
      result <- lbody.sub[[3L]]
      return(result)
    }
  }
  stop(paste("cannot find block:", name), call. = FALSE)
}

#' Get modules from a function block
#' @noRd
get_modules <- function(lbody) {
  lbody.sub <- lbody[[3L]]
  res <- list()
  for (i in seq_along(lbody.sub)) {
    lbody.sub.elem <- lbody.sub[[i]]
    if (any(grepl("callModule", as.character(lbody.sub.elem)))) {
      res[[length(res) + 1L]] <- lbody.sub.elem
    }
  }
  res
}

#' Extract module names from callModule functions
#'
#' @param modules callModule call block.
#' @return a character vector.
#' @noRd
extract_module_names <- function(modules) {
  stopifnot(is_list(modules))
  modnames <- list()
  for (i in seq_along(modules)) {
    modules.list <- as.list(modules[[i]])
    modnames[[i]] <- as.character(modules.list[["module"]])
  }
  unlist(modnames)
}


#' Create a module tree
#'
#' @param x an R file name containing a Shiny application.
#' @examples \dontrun{
#' }
#' @export
module_tree <- function(x) {

  file <- read_srcfile(x)
  src <- parse(text = file)
  body <- as.list(src)

  cat("server", "\n")

  server <- get_server_block(body)
  server.modules <- get_modules(server)

  .tree_recur(server, body, level = 1, module = 0)
}

module_tree.print <- function(x) {
  # TODO
  sym <- supreme.symbols()
  tree_cat(paste(rep(" ", level-1L), collapse = ""), sym$arrow$Vup.Hright, sym$arrow$H, name)
}

.tree_recur <- function(x, body, level, module) {

  module.names <- vector()

  module <- get_modules(x)

  if (length(module) == 0) {
    return(invisible())
  }

  module.names <- c(module.names, get_module_names(module))

  name <- character()
  for (i in seq_along(module.names)) {
    name <- module.names[i]
  }

  x <- find_block(body, name)

  Recall(x, body, level + 1L, module)
}
