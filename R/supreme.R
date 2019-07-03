


### ----------------------------------------------------------------- ###
### OBJECT AND SYMBOL CHECKS ----
### ----------------------------------------------------------------- ###

#' Check objects
#'
#' @param x a valid \R expression.
#'
#' @name objcheck
#' @noRd
NULL

#' Checks if an object is a list (but not a data.frame)
#' @rdname objcheck
is_list <- function(x) {
  is.list(x) && !is.data.frame(x)
}

#' @rdname objcheck
is_expression <- function(x) {
  is.expression(x) && is.language(x)
}

#' Checks the symbol of a call (the first element)
#'
#' @param x a valid \R expression.
#'
#' @name objsymcheck
#' @noRd
NULL

#' @rdname objsymcheck
is_left_assign_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`<-`))
}

#' @rdname objsymcheck
is_expr_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`{`))
}

#' @rdname objsymcheck
is_func_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`function`))
}

#' @rdname objsymcheck
is_callModule_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`callModule`))
}

### ----------------------------------------------------------------- ###
### PARSE CALLS ----
### ----------------------------------------------------------------- ###

#' Segment an expression into sub-divisions
#'
#' @param x an \R expression, or a list holding an \R expression.
#' @noRd
divide_expr <- function(x) {
  if (is.language(x)) {
    as.list(x)
  } else {
    ncstopf("cannot handle input: `%s`", typeof(x))
  }
}

#' Find code a block
#'
#' @param x expression.
#' @param bname block name to look for.
#' @noRd
find_block <- function(x, bname) {
  res <- list()
  if (is_expr(x[[1]])) {
    for (i in seq(2L, length(x))) {
      if (is_left_assign(x[[i]][[1]])) {
        if (is.symbol(x[[i]][[2]])) {
          if (x[[i]][[2]] == bname) {
            res[[length(res)+1]] <- x[[i]][[3]]
          }
        }
      }
    }
  } else {
    res <- unlist(lapply(x, function(b) find_block(b, bname)))
  }
  res
}

get_server_block <- function(x) {
  server <- find_block(x, "server")
  if (length(server) > 1) {
    ncstopf("supreme cannot proceed because 'server' is defined multiple times")
  }
  server
}

get_modules_from_block <- function(block) {

  .get_modules_from_block <- function(x) {

    if (is.call(x)) {
      if (is.symbol(x[[1]])) {
        if (is_func(x[[1]])) {
          Recall(x[[3]])
        } else if (is_left_assign(x[[1]])) {
          Recall(x[[3]])
        } else if (is_expr(x[[1]])) {
          for (i in seq(2L, length(x))) {
            if (is_callModule(x[[i]][[1]])) {
              mod.names <- names(x[[i]])
              mod.names.inds <- if (!is.null(mod.names)) {
                module <- which(mod.names == "module")
                if (!length(module) == 0L) {
                  module
                } else {
                  2L
                }
              } else {
                2L
              }
              res[[length(res)+1]] <<- as.character(x[[i]][[mod.names.inds]])
            } else {
              Recall(x[[i]])
            }
          }
        } else if (is_callModule(x[[1]])) {
          mod.names <- names(x)
          mod.names.inds <- if (!is.null(mod.names)) {
            module <- which(mod.names == "module")
            if (!length(module) == 0L) {
              module
            } else {
              2L
            }
          } else {
            2L
          }
          res[[length(res)+1]] <<- as.character(x[[mod.names.inds]])
        } else {
          if (length(x) >= 2) {
            if (is.call(x[[2]])) {
              Recall(x[[2]])
            }
          }
        }
      }
    }
  }

  res <- list()
  .get_modules_from_block(block)
  res
}

### ----------------------------------------------------------------- ###
### API ----
### ----------------------------------------------------------------- ###

#' Create a module tree
#'
#' @param x an \R expression or a file name that contain a (valid) Shiny application.
#' @export
module_tree <- function(x) {

  src <- if (file.exists(as.character(x))) {
    read_srcfile(x)
  } else if (is.language(x)) {
    x
  } else {
    ncstopf("cannot handle input: `%s`", typeof(x))
  }

  body <- divide_expr(src)

  server.block <- get_server_block(body[[1]])
  server.block.modules <- get_modules_from_block(server.block[[1]])

  list(server = unlist(server.block.modules))
}


#' @export
module_tree.print <- function(x) {
  sym <- supreme.shapes()
  tree_cat(paste(rep(" ", level-1L), collapse = ""), sym$arrow$Vup.Hright, sym$arrow$H, name)
}

