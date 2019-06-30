
#' Unicode characters
#' @noRd
supreme.shapes <- function() {

  # Symbols to draw a Unicode tree
  # https://atom.io/packages/ascii-tree
  arrow <- list(
    H = "─",
    V = "│",
    Vup.Vdown.Hright = "├",
    Vup.Hright = "└"
  )

  list(
    arrow = arrow
  )
}

#' Wrapper around cat for tree printing
#' @noRd
tree_cat <- function(...) {
  cat(paste(..., sep = ""), "\n")
}

#' Checks if an object is a list (but not a data.frame)
#' @noRd
is_list <- function(x) {
  is.list(x) && !is.data.frame(x)
}

#' A wrapper: No call stop format
#' @noRd
ncstopf <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

