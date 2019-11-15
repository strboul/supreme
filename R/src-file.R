
#' Read \R files
#'
#' The files should contain some Shiny application.
#'
#' @param x a file path.
#' @examples
#' paths <- example_app_path()
#' s <- supreme(src_file(paths))
#' @export
src_file <- function(x) {
  tryCatch({ file.exists(as.character(x)) }, error = function(e)
    ncstopf("cannot read file: %s", conditionMessage(e)))
  obj <- make_module_entities_from_paths(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_file"))
}

#' @export
print.src_file <- function(x, ...) {
  cat("Model file object", "\n")
}

#' Read src file
#'
#' A small subset of [base::getSrcLines].
#'
#' @param x a file name.
#' @return A parsed expression.
#' @details
#' Lines starting with commenting symbol # (hash) are removed from the character
#' vector before sending to parsing. Normally, `parse()` automatically removes the
#' comments;however, we do it before anyway to avoid any potential problems, which
#' can be caused by the paste collapsing.
#'
#' Before parsing, the character vector is wrapped between curly braces (`{` and `}`)
#' as the system is designed around exprs. Also put new lines before the quotes to be
#' sure that they are not commented out from # a previous commented line.
#' @noRd
read_srcfile <- function(x) {
  lijnen <- lapply(seq_along(x), function(i) {
    fname <- x[i]
    srcfile <- srcfile(fname)
    if (!.isOpen(srcfile)) {
      on.exit(close(srcfile), add = TRUE)
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
  lijnen <- unlist(lijnen)
  if (length(commented.lines <- grep("^#", lijnen)) > 0L) {
    lijnen <- lijnen[-commented.lines]
  }
  lines <- paste("{\n", paste(lijnen, collapse = "\n"), "\n}")
  parse(text = lines)
}

#' Make module entities from file paths
#'
#' @param x file paths.
#' @noRd
make_module_entities_from_paths <- function(x) {
  short.src <- shorten_src_file_path(x)
  out <- lapply(seq_along(short.src), function(i) {
    src <- short.src[i]
    path <- x[grep(src, x)]
    body <- read_srcfile(path)
    list(body = body, src = src)
  })
  structure(out, class = "module_entities")
}

