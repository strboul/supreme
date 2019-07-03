
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

#' ncstopf: No call stop format
#'
#' Differentiate the errors by selecting internal:
#'
#' 1. errors raised and shown to end users are 'normal',
#' 2. and errors caused by internal errors, which are more likely to be bug, are
#' 'internal' which they can be copied and pasted to diagnose the error.
#'
#' @noRd
ncstopf <- function(..., internal = FALSE) {
  desc <- if (internal) {
    calls <- sys.calls()
    calls.len <- length(calls)
    calls.len.minus <- calls.len - 1L
    dps <- sapply(seq(calls.len.minus), function(i) deparse(calls[[i]]))
    out <- vector("character", calls.len.minus)
    for (i in seq(calls.len.minus)) {
      spc <- paste(rep(" ", (i - 1L)*2), collapse = "")
      out[i] <- paste0(spc, " - ", dps[i], "\n")
    }
    stop(paste0("[supreme (.INTERNAL)] ",
               sprintf(...),
               ":\n",
               paste(paste(out, collapse = ""))),
         call. = FALSE)
  } else {
    stop(paste("[supreme]", sprintf(...)), call. = FALSE)
  }
}

