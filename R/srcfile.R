
#' Read src file
#'
#' @param fname a file name.
#' A small subset of \code{getSrcLines} call.
#'
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
  lines
}
