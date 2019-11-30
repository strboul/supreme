
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
              res[[length(res) + 1]] <- x[[i]][[3]]
            }
          }
        }
      }
    }
  } else if (is_list(x)) {
    invisible(NULL)
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
find_block_calling_modules <- function(x) {

  .find_modules_from_block <- function(x) {
    if (is.call(x)) {
      if (is.symbol(x[[1]])) {
        if (is_func_sym(x[[1]])) {
          Recall(x[[3]])
        } else if (is_left_assign_sym(x[[1]])) {
          Recall(x[[3]])
        } else if (is_expr_sym(x[[1]])) {
          if (!length(x) > 1L) {
            return(NULL)
          }
          for (i in seq(2L, length(x))) {
            if (is.call(x[[i]])) {
              if (is_callModule_sym(x[[i]][[1]])) {
                mod_names <- names(x[[i]])
                mod_names_inds <- if (!is.null(mod_names)) {
                  module <- which(mod_names == "module")
                  if (!length(module) == 0L) {
                    module
                  } else {
                    2L
                  }
                } else {
                  2L
                }
                res[[length(res) + 1L]] <<- as.character(x[[i]][[mod_names_inds]])
              } else {
                if (is.call(x[[i]])) {
                  Recall(x[[i]])
                }
              }
            } else {
              return(NULL)
            }
          }
        } else if (is_callModule_sym(x[[1]])) {
          mod_names <- names(x)
          mod_names_inds <- if (!is.null(mod_names)) {
            module <- which(mod_names == "module")
            if (!length(module) == 0L) {
              module
            } else {
              2L
            }
          } else {
            2L
          }
          res[[length(res) + 1L]] <<- as.character(x[[mod_names_inds]])
        } else {
          if (length(x) >= 2) {
            if (is.call(x[[2]])) {
              Recall(x[[2]])
            } else {
              return(NULL)
            }
          }
        }
      }
    } else if (is.function(x)) {
      bod <- as.list(x)
      bod.len <- length(bod)
      Recall(bod[[bod.len]])
    }
  }

  res <- list()
  .find_modules_from_block(x)
  if (length(res) > 0) {
    unlist(res)
  } else {
    NULL
  }
}

#' Find formals of a function
#'
#' @param x an \R expression.
#' @return returns `NULL` if the given expression is not a function body.
#' @noRd
find_formals <- function(x) {
  if (is.call(x)) {
      if (is_func_sym(x[[1]])) {
        if (is.pairlist(x[[2]])) {
          return(names(x[[2]]))
        }
      } else if (is_left_assign_sym(x[[1]])) {
        Recall(x[[3]])
      }
  } else if (is.function(x)) {
    return(names(formals(x)))
  } else if (is_expr_sym(x)) {
    invisible(NULL)
  } else {
    unlist(lapply(x, find_formals))
  }
}

#' Find assignment name of a function
#'
#' @param x an \R expression.
#' @return returns the assigned name of the function body as character vector.
#' @noRd
find_block_assignment_name <- function(x) {
  if (is.call(x)) {
    if (is_left_assign_sym(x[[1]])) {
      return(as.character(x[[2]]))
    } else {
      unlist(lapply(x, find_block_assignment_name))
    }
  }
}

