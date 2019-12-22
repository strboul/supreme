
#' Find binding name of a function
#'
#' @param x an \R language object.
#' @return
#' The binding name of the function as a character vector.
#' Returns `NULL` if the input is not a `call`.
#' @noRd
find_binding_name <- function(x) {
  if (is.call(x)) {
    if (is_left_assign_sym(x[[1]])) {
      return(as.character(x[[2]]))
    } else {
      unlist(lapply(x, find_binding_name))
    }
  } else {
    NULL
  }
}


#' Find inputs of a `call`
#'
#' @param x an \R language object.
#' @return
#' Returns `NULL` if the given expression is not a function body.
#' @noRd
find_inputs <- function(x) {
  if (is.call(x)) {
    if (is_func_sym(x[[1L]])) {
      if (is.pairlist(x[[2L]])) {
        return(names(x[[2L]]))
      }
    } else if (is.function(x)) {
      return(names(formals(x)))
    } else {
      unlist(lapply(x, find_inputs))
    }
  } else {
    NULL
  }
}


#' Find outputs of a `call`, which is a server side Shiny module
#'
#' @param x an \R language object.
#' @return
#' The name of the `output`s as a character vector.
#' @noRd
find_outputs <- function(x) {

  .find_outputs_from_block <- function(x) {
    if (is.call(x)) {
      if (is_dollar_sym(x[[1L]]) || is_double_bracket_sym(x[[1L]])) {
        if (is_output_sym(x[[2L]])) {
          value <- as.character(x[[3L]])
          res[[length(res) + 1L]] <<- value
        }
      } else if (is_left_assign_sym(x[[1L]])) {
        for (i in seq(2L, length(x))) {
          if (is.call(x[[i]])) {
            Recall(x[[i]])
          }
        }
      } else if (is_func_sym(x[[1L]])) {
        Recall(x[[3L]])
      } else if (is_expr_sym(x[[1L]])) {
        for (i in seq(2L, length(x))) {
          if (is.call(x[[i]])) {
            Recall(x[[i]])
          }
        }
      } else {
        NULL
      }
    } else {
      NULL
    }
  }

  res <- list()
  .find_outputs_from_block(x)
  if (length(res) > 0) {
    unlist(res)
  } else {
    NULL
  }

}


#' Find return values of a `call`
#'
#' @param x an \R language object.
#'
#' @details
#' Finds the return value of a call. Normally, functions can have
#' early returns; therefore there can always be multiple return
#' values. The common return syntax in \R as follows:
#'
#' + The object wrapped inside `return()`
#' + The last object of a call
#' + The last "binded object" of a `call`
#'
#' However, this finder is especially looking for the return values
#' of the Shiny modules, we always look for the return calls that are
#' wrapped inside the `return()`.
#'
#' @noRd
find_returns <- function(x) {

  .find_returns_from_block <- function(x) {
    if (is.call(x)) {
      if (is_return_sym(x[[1L]])) {
        if (is.call(x[[2L]])) {
          if (is_expr_sym(x[[2L]][[1L]])) {
            value <- as.character(x[[2L]][[2L]])
            res[[length(res) + 1L]] <<- value
          }
        } else {
          value <- if (is.null(x[[2L]])) "NULL" else as.character(x[[2L]])
          res[[length(res) + 1L]] <<- value
        }
      } else if (is_expr_sym(x[[1L]]) || is_if_sym(x[[1L]])) {
        for (i in seq(2L, length(x))) {
          if (is.call(x[[i]])) {
            Recall(x[[i]])
          }
        }
      } else if (is_left_assign_sym(x[[1]])) {
        for (i in seq(2L, length(x))) {
          if (is.call(x[[i]])) {
            Recall(x[[i]])
          }
        }
      } else if (is_func_sym(x[[1L]])) {
        Recall(x[[3L]])
      } else {
        NULL
      }
    } else {
      NULL
    }
  }

  res <- list()
  .find_returns_from_block(x)
  if (length(res) > 0) {
    unlist(res)
  } else {
    NULL
  }
}


#' Find Shiny modules from a function block
#'
#' @param x an \R language object.
#'
#' @details
#' What a `callModule` call can get:
#'
#' + name: the name for the Shiny module server function,
#' + id: corresponding id with the module UI's function,
#'  and various arguments to be passed onto module function.
#'
#' What a Shiny module (*the server part*) can get:
#'
#' + symbol.name: the function name for the server-side of a module
#' + arguments: arguments are passed into the module function
#'  (`input`, `output`, `session` arguments
#'  are "always" the default ones)
#' @noRd
find_calling_modules <- function(x) {

  .find_modules_from_block <- function(x) {
    if (is.call(x)) {
      if (is.symbol(x[[1L]])) {
        if (is_func_sym(x[[1L]])) {
          Recall(x[[3L]])
        } else if (is_left_assign_sym(x[[1]])) {
          Recall(x[[3L]])
        } else if (is_expr_sym(x[[1L]])) {
          if (!length(x) > 1L) {
            return(NULL)
          }
          for (i in seq(2L, length(x))) {
            if (is.call(x[[i]])) {
              if (is_callModule_sym(x[[i]][[1L]])) {
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
                value <- as.character(x[[i]][[mod_names_inds]])
                res[[length(res) + 1L]] <<- value
              } else {
                if (is.call(x[[i]])) {
                  Recall(x[[i]])
                }
              }
            } else {
              return(NULL)
            }
          }
        } else if (is_callModule_sym(x[[1L]])) {
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
          value <- as.character(x[[mod_names_inds]])
          res[[length(res) + 1L]] <<- value
        } else {
          if (length(x) >= 2L) {
            if (is.call(x[[2L]])) {
              Recall(x[[2L]])
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
  if (length(res) > 0L) {
    unlist(res)
  } else {
    NULL
  }
}


#' Find code a block
#'
#' @param x an \R language object.
#' @param bname the block name to look for.
#' @noRd
.find_block <- function(x, bname) {
  res <- list()
  if (is.call(x)) {
    if (is_expr_sym(x[[1L]])) {
      for (i in seq(2L, length(x))) {
        if (is_left_assign_sym(x[[i]][[1L]])) {
          if (is.symbol(x[[i]][[2L]])) {
            if (x[[i]][[2L]] == bname) {
              res[[length(res) + 1L]] <- x[[i]][[3L]]
            }
          }
        }
      }
    }
  } else if (is_list(x)) {
    invisible(NULL)
  } else {
    res <- unlist(lapply(x, function(b) .find_block(b, bname)))
  }
  res
}

