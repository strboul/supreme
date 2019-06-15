
#' Read src file
#'
#' A small subset of \code{getSrcLines} call.
#'
#' @param fname a file name.
#' @return A parsed expression.
#' @noRd
read_srcfile <- function(fname) {

  stopifnot(is.character(fname))
  stopifnot(identical(length(fname), 1L))

  srcfile <- srcfile(fname)
  if (!.isOpen(srcfile)) {
    on.exit(close(srcfile))
  }
  first <- 1L
  conn <- open(srcfile, first)
  lines <- readLines(conn, warn = FALSE)
  # encoding stuff:
  Enc <- srcfile$Enc
  if (!is.null(Enc) && !(Enc %in% c("unknown", "native.enc"))) {
    lines <- iconv(lines, "", Enc)
  }

  parse(text = lines)
}

