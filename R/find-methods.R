
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

