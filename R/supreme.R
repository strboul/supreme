
#' Get server block
#'
#' @param app.body parsed body separated into lists.
#' @return returns the index of the server from the body.
#' @noRd
get_server_block <- function(lbody) {
  get_block(lbody, "server")
}

#' Find a function block with its assignment name
#'
#' @param name variable name used in assignment.
#' @noRd
get_block <- function(lbody, name) {
  for (i in seq_along(lbody)) {
    lbody.sub <- lbody[[i]]
    lbody.sub.symbol <- lbody.sub[[1L]]
    if (lbody.sub.symbol == "<-") {
      lbody.sub.name <- lbody.sub[[2L]]
      if (lbody.sub.name == name) {
        result <- lbody.sub[[3L]]
        return(result)
      }
    }
  }
  ncstopf("cannot find block: %s", name)
}

get_object <- function(lbody, name) {
  for (i in seq_along(lbody)) {
    lbody.sub <- lbody[[i]]
    lbody.sub.name <- lbody.sub[[1L]]
    if (lbody.sub.name == name) {
      return(lbody.sub)
    }
  }
  ncstopf("cannot find object: %s", name)
}

#' Get modules from a function block
#' @noRd
get_modules <- function(lbody) {
  lbody.sub <- lbody[[3L]]
  res <- list()
  for (i in seq_along(lbody.sub)) {
    lbody.sub.elem <- lbody.sub[[i]]
  # TODO get sub modules
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
#' @param file an R file name containing a Shiny application.
#' @rdname module_tree
#' @export
module_tree <- function(file) {

  browser()
  src <- read_srcfile(file)
  body <- as.list(src)

  ## do not proceed if that Shiny app object not found:
  tryCatch(get_object(body, "shinyApp"),
           error = function(e)
             ncstopf("A shinyApp object not found: `shinyApp(ui, server)`"))

  ## create an empty list to keep module tree:
  L <- list()

  ## the root of the tree is server:
  L[[1]] <- "server"

  server.block <- get_server_block(body)
  server.modules <- get_modules(server.block)

  server.module.names <- extract_module_names(server.modules)

  sub.module.names <- vector("list", length(server.module.names))
  for (i in seq_along(server.module.names)) {
    sub.module.block <- get_block(body, server.module.names[i])
    sub.module <- get_modules(sub.module.block)
    sub.module.names <- extract_module_names(sub.module)
  }
}


.recur <- function(body, names, level) {

  block <- vector("list", length(names))
  for (n in seq_along(names)) {
    block[[n]] <- get_block(body, names[n])
  }

}

#' TODO
#' @rdname module_tree
module_tree.print <- function(x) {
  sym <- supreme.shapes()
  tree_cat(paste(rep(" ", level-1L), collapse = ""), sym$arrow$Vup.Hright, sym$arrow$H, name)
}

