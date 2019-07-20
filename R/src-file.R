
src_file <- function(x) {
  tryCatch({ file.exists(as.character(x)) }, error = function(e)
    ncstopf("cannot read file: %s", conditionMessage(e)))
  read_srcfile(x)
}

#' Read src file
#'
#' A small subset of [base::getSrcLines].
#'
#' @param x a file name.
#' @return A parsed expression.
#' @noRd
read_srcfile <- function(x) {

  ljnen <- lapply(seq_along(x), function(i) {
    fname <- x[i]
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
  })

  ljnen <- unlist(ljnen)

  ## Wrap`{`, `}` quotes in between as the system is designed around exprs"
  lines <- paste("{", paste(ljnen, collapse = "\n"), "}")

  parse(text = lines)
}

