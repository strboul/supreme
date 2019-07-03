


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
subset_expr <- function(x) {
  if (is_expression(x) || is_list(x)) {
    if (length(x) == 1L) {
      x[[1L]]
    } else {
      ncstopf("length greater than one: %s",
              length(x),
              internal = TRUE)
    }
  } else {
    ncstopf("input not an expression, instead: `%s`",
            typeof(x),
            internal = TRUE)
  }
}

### ----------------------------------------------------------------- ###
### FIND CALLS ----
### ----------------------------------------------------------------- ###

#' Find code a block
#'
#' @param x an expression.
#' @param bname the block name to look for.
#' @noRd
find_block <- function(x, bname) {
  res <- list()
  if (is.call(x)) {
    if (is_expr_sym(x[[1]])) {
      for (i in seq(2L, length(x))) {
        if (is_left_assign_sym(x[[i]][[1]])) {
          if (is.symbol(x[[i]][[2]])) {
            if (x[[i]][[2]] == bname) {
              res[[length(res)+1]] <- x[[i]][[3]]
            }
          }
        }
      }
    }
  } else {
    res <- unlist(lapply(x, function(b) find_block(b, bname)))
  }
  res
}

### ----------------------------------------------------------------- ###
### GET CALLS ----
### ----------------------------------------------------------------- ###

get_server_block <- function(x) {
  x <- subset_expr(x)
  server <- find_block(x, "server")
  if (is.null(server)) {
    ncstopf("cannot find server")
  }
  if (length(server) > 1) {
    ncstopf("cannot proceed because 'server' is defined multiple times")
  }
  server
}

#' @param x an expression.
#' @param modname the module name to look form.
# TODO get_module_block <- function(x, modname)

get_modules_from_block <- function(block) {

  .find_modules_from_block <- function(x) {

    if (is.call(x)) {
      if (is.symbol(x[[1]])) {
        if (is_func_sym(x[[1]])) {
          Recall(x[[3]])
        } else if (is_left_assign_sym(x[[1]])) {
          Recall(x[[3]])
        } else if (is_expr_sym(x[[1]])) {
          for (i in seq(2L, length(x))) {
            if (is_callModule_sym(x[[i]][[1]])) {
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
        } else if (is_callModule_sym(x[[1]])) {
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

  block <- subset_expr(block)
  res <- list()
  .find_modules_from_block(block)
  if (length(res) > 0) {
    res
  } else {
    NULL
  }
}

### ----------------------------------------------------------------- ###
### API ----
### ----------------------------------------------------------------- ###

#' Create a module tree
#'
#' @param x an \R expression or a file name that contain a (valid) Shiny application.
#' @importFrom tools file_path_as_absolute
#' @export
tree_app <- function(x) {

  body <- if (file.exists(as.character(x))) {
    fullp <- tools::file_path_as_absolute(x)
    read_srcfile(fullp)
  } else if (is_expression(x)) {
    x
  } else {
    ncstopf("cannot handle input: `%s`", typeof(x))
  }

  server.block <- get_server_block(body)
  server.block.modules <- get_modules_from_block(server.block)

  list(server = unlist(server.block.modules))
}


### ----------------------------------------------------------------- ###
### S3 METHODS ----
### ----------------------------------------------------------------- ###

tree_app.plot <- function(x) {

}

tree_app.print <- function(x) {
  sym <- supreme.shapes()
  tree_cat(paste(rep(" ", level-1L), collapse = ""), sym$arrow$Vup.Hright, sym$arrow$H, name)
}

