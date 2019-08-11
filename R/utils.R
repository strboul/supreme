
#' ncstopf: 'N'o 'c'all stop 'f'ormat
#'
#' Differentiate the errors by selecting internal:
#'
#' 1. errors raised and shown to end users are 'normal',
#' 2. and errors caused by internal errors, which are more likely to be bug, are
#' 'internal' which they can be copied and pasted to diagnose the error.
#'
#' @param ... arguments to be passed to the `sprintf` function.
#' @param internal value selected showing this error as internal style
#' @param single.line remove new lines and multi-spaces in the message.
#' @noRd
ncstopf <- function(..., internal = FALSE, single.line = FALSE) {
  desc <- if (internal) {
    calls <- sys.calls()
    calls.len <- length(calls)
    calls.len.minus <- calls.len - 1L
    if (identical(calls.len.minus, 0L)) calls.len.minus <- 1L
    dps <- sapply(seq(calls.len.minus), function(i) deparse(calls[[i]]))
    truncate.lim <- 4L
    out <- vector("character", calls.len.minus)
    for (i in pmin(truncate.lim, seq(calls.len.minus))) {
      spc <- paste(rep(" ", (i - 1L)*2), collapse = "")
      out[i] <- paste0(spc, " + ", dps[i], "\n")
    }
    if (length(out) > truncate.lim)
      out[truncate.lim+1L] <- paste(paste(rep(" ", (truncate.lim)*2), collapse = ""), "...")
    paste0("[supreme internal error] ",
               sprintf(...),
               "\n",
               paste(paste(out, collapse = "")))
  } else {
    paste("[supreme]", sprintf(...))
  }

  if (single.line) {
    desc <- gsub("\\n+", "", desc)
    desc <- gsub("\\s+", " ", desc)
  }

  stop(desc, call. = FALSE)
}

#' Paste by separating new lines
#' @noRd
pasten <- function(...) paste(..., sep = "\n")

#' Paste by separating new lines and collapsing empty string
#' @noRd
pastenc <- function(...) paste(..., sep = "\n", collapse = "")

#' @param x package name.
#' @noRd
is_package_exist <- function(x) {
  stopifnot(is.character(x))
  pkg <- find.package(x, quiet = TRUE)
  if (length(pkg) >= 1L) {
    TRUE
  } else {
    FALSE
  }
}

