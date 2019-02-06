
#' Get parsed data (main)
#'
#' @param filename filename. Choose the main application file.
#' @param follow.source Should \code{source} calls in the files be followed?
#'
#' @noRd
get_parsed_data <- function(filename, follow.source = FALSE) {

  src <- read_srcfile(filename)
  parsed <- parse_data(src)

  parsed

}

#' Get source file paths from files
#'
#' @param src source file.
#'
#' @noRd
get_source_file <- function(src) {
  # TODO regex
}

#' Read src file
#'
#' @param name A file name.
#' A small subset of \code{getSrcLines} call.
#' @seealso getSrcLines
#' @noRd
read_srcfile <- function(name) {

  if (!is.character(name)) stop(paste(name, "not a character"))
  if (!identical(length(name), 1L)) stop("name length should be 1")

  srcfile <- srcfile(name)
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

#' Decompose source by getParseData
#'
#' @param src source as a text file.
#'
#' @importFrom utils getParseData
#' @noRd
parse_data <- function(src) {
  stopifnot(is.character(src))
  p <- utils::getParseData(parse(text = src))
  # remove comments:
  p <- p[!p[["token"]] == "COMMENT", ]
  # omit row & col data:
  p <- p[c("id", "parent", "token", "terminal", "text")]
  # order by id:
  p <- p[order(p$id), ]
  # reset rownames:
  rownames(p) <- NULL
  p
}
