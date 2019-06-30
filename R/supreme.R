
### ----------------------------------------------------------------- ###
### OBJECT CHECKS ----
### ----------------------------------------------------------------- ###

stop_if_not_parsable <- function(x) {
  if (!is.call(x)) {
    ncstopf("cannot parse type: '%s'", typeof(x))
  }
}

is_left_assign <- function(x) {
  is.symbol(x) && identical(x, quote(`<-`))
}

is_expr <- function(x) {
  is.symbol(x) && identical(x, quote(`{`))
}

is_func <- function(x) {
  is.symbol(x) && identical(x, quote(`function`))
}

is_callModule <- function(x) {
  is.symbol(x) && identical(x, quote(`callModule`))
}

### ----------------------------------------------------------------- ###
### PARSE CALLS ----
### ----------------------------------------------------------------- ###

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
  stop_if_not_parsable(x)
  server <- find_block(x, "server")
  if (length(server) > 1) {
    ncstopf("supreme cannot proceed because 'server' is defined multiple times")
  }
  server
}

get_modules_from_block <- function(block) {

  stop_if_not_parsable(block)

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
                which(mod.names == "module")
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
            which(mod.names == "module")
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

  # src <- read_srcfile(file)
  body <- as.list(x)

  get_modules_from_block(body)

}


#' @export
module_tree.print <- function(x) {
  sym <- supreme.shapes()
  tree_cat(paste(rep(" ", level-1L), collapse = ""), sym$arrow$Vup.Hright, sym$arrow$H, name)
}

