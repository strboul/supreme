
#' Segment an expression into sub-divisions
#'
#' @param x an \R expression, or a list holding an \R expression.
#' @noRd
subset_expr <- function(x) {
  if (is_expression(x) || is_list(x)) {
    if (length(x) == 1L) {
      x[[1L]]
    } else {
      ncstopf("length greater than one: %s",
              length(x),
              internal = TRUE)
    }
  } else {
    ncstopf("input not valid: `%s`",
            typeof(x),
            internal = TRUE)
  }
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

    truncate.lim <- 4L
    out <- vector("character", calls.len.minus)
    for (i in pmin(truncate.lim, seq(calls.len.minus))) {
      spc <- paste(rep(" ", (i - 1L)*2), collapse = "")
      out[i] <- paste0(spc, " + ", dps[i], "\n")
    }
    if (length(out) > truncate.lim)
      out[truncate.lim+1L] <- paste(paste(rep(" ", (truncate.lim)*2), collapse = ""), "...")
    stop(paste0("[supreme (.INTERNAL)] ",
               sprintf(...),
               ":\n",
               paste(paste(out, collapse = ""))),
         call. = FALSE)
  } else {
    stop(paste("[supreme]", sprintf(...)), call. = FALSE)
  }
}

