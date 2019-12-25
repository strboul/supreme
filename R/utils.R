
#' ncstopf: 'N'o 'c'all stop 'f'ormat
#'
#' @param ... arguments to be passed to the `sprintf`.
#' @param single.line remove new lines and multi-spaces in the message.
#' @noRd
ncstopf <- function(..., single.line = FALSE) {
  desc <- paste("[supreme]", sprintf(...))
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


#' Checks if file paths exist and throws an (supreme) error unless otherwise
#'
#' @param x a file path.
#' @noRd
check_paths_exist <- function(x) {
  tryCatch(
    file.exists(as.character(x)),
    error = function(e) {
      ncstopf("cannot read file: %s", conditionMessage(e))
    }
  )
}

