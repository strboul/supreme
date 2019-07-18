
#' Find code a block
#'
#' @param x an \R expression.
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

#' Find modules from a code block
#'
#' @param x an \R expression.
#'
#' @details
#' What a `callModule` call can get:
#'
#' + name: the name for the Shiny module server function,
#'
#' + id: corresponding id with the module UI's function,
#'
#'  and various arguments to be passed onto module function.
#'
#' What a Shiny module (*the server part*) can get:
#'
#' + symbol.name: the function name for the server-side of a module
#'
#' + arguments: arguments are passed into the module function (`input`, `output`,
#' `session` are always the default)
#'
#' @noRd
find_block_modules <- function(x) {

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

  res <- list()
  .find_modules_from_block(x)
  if (length(res) > 0) {
    res
  } else {
    NULL
  }
}

#' Find arguments of a function body
#'
#' @param x an \R expression.
#' @noRd
find_arguments <- function(x) {
  if (is.call(x)) {
    if (is_func_sym(x[[1]])) {
      if (is.pairlist(x[[2]])) {
        return(names(x[[2]]))
      }
    }
  } else {
    unlist(lapply(x, find_arguments))
  }
}

