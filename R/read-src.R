
#' Read src file
#'
#' A small subset of [base::getSrcLines].
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

  ## Wrap`{`, `}` quotes in between as the system is designed around exprs"
  lines <- paste("{", paste(lines, collapse = "\n"), "}")

  parse(text = lines)
}

#' @importFrom tools file_path_as_absolute
src_file <- function(x) {
  tryCatch({ file <- file.exists(as.character(x)) }, error = function(e)
    ncstopf("cannot read file: %s", conditionMessage(e)))
  if (file) {
    fullp <- tools::file_path_as_absolute(x)
    read_srcfile(fullp)
  } else {
    ncstopf("cannot read file: `%s`", x)
  }
}

src_expr <- function(x) {
  if (is_expression(x)) {
    x
  } else {
    ncstopf("cannot read expression: `%s`", x)
  }
}

