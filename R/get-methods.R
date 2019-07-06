
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

