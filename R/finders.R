
#' Notes about the 'computing on the language'
#'
#' @details
#' + If the class of a list element equals to "\{", that means the element is an
#' "expr". Open it up. Every "expr" contains multiple number of list elements
#' but the number of these list elements is unknown. The number of elements is
#' depended upon the number of statements between curly braces in an expression.
#'
#' + If an object is a "symbol" (or "name"), it's a leaf which doesn't have any
#' child elements.
#'
#' + In parsing \R code, recursive case is the type of "call" which it has
#' always some more elements under. Base cases are more than one, such as symbol
#' and constants (character, integer, numeric etc).
#'
#' + Recursive functions have two cases:
#'   - Base case: the part terminating the recursion (can still have more than
#'     one elements inside but they are not a `call`).
#'   - Recursive case: the part keep recursion occurring.
#'
#' + If an object is a `call`, that's never a 'leaf' (terminal node); therefore,
#' a recursion (either with `Recall` or `lapply(x, <fun_name>)`) should be
#' placed in order to go deep down the list.
#'
#' + If an object is a `pairlist`, it's technically a `list`, but not a
#' `call`. Pairlist contains argument names of functions and their default
#' values. Use `names()` to extract argument names. Get inside to each list
#' element in order to extract individual argument values.
#'
#' @noRd
NULL


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
      } else {
        unlist(lapply(x, .find_outputs_from_block))
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
      } else {
        unlist(lapply(x, .find_returns_from_block))
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
#' @importFrom stats setNames
#' @noRd
find_calling_modules <- function(x) {

  .extract_callModule_arg_ind <- function(x, arg_name, default_arg_ind) {
    mod_arg_name <- names(x)
    mod_arg_name_ind <- if (!is.null(mod_arg_name)) {
      mod_arg <- which(mod_arg_name == arg_name)
      if (length(mod_arg) > 0L) {
        mod_arg
      } else {
        default_arg_ind
      }
    } else {
      default_arg_ind
    }
    mod_arg_name_ind
  }

  .find_modules_from_block <- function(x) {
    if (is.call(x)) {
      if (is_callModule_sym(x[[1L]])) {

        ## shiny::callModule 'module' arg:
        mod_module_name_ind <- .extract_callModule_arg_ind(x, "module", 2L)
        ## shiny::callModule 'id' arg:
        mod_id_name_ind <- .extract_callModule_arg_ind(x, "id", 3L)

        module_value <- deparse(x[[mod_module_name_ind]])
        id_value <- if (!is.null(x[[mod_id_name_ind]])) {
          as.character(x[[mod_id_name_ind]])
        }

        value <- stats::setNames(list(id_value), module_value)
        res[[length(res) + 1L]] <<- value
      } else {
        unlist(lapply(x, .find_modules_from_block))
      }
    } else {
      NULL
    }
  }

  res <- list()
  .find_modules_from_block(x)
  if (length(res) > 0L) {
    res
  } else {
    NULL
  }
}

