
src_expr <- function(x) {
  if (!length(x) == 1L) {
    ncstopf("expression length must be one, instead of: %s", length(x))
  }
  if (is_expression(x)) {
    x
  } else {
    ncstopf("cannot read expression: `%s`", x)
  }
}

