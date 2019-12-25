
#' Shorten src file paths
#'
#' This call "creates unique relative file paths" from full (absolute) file paths.
#'
#' @param x file paths as character.
#' @details
#' The file paths specified in the `src` field in any `supreme` or `supreme` related
#' object will be shorter so that any long paths in `src` fields will not clutter the
#' tables and graphs.
#'
#' This call behaves smarter when picking the base name from the file paths. If the
#' total src file names have duplicated base names (but unique must be in essence),
#' the call will walk through the previous names in the parent, and will include the
#' parent directories until that those paths become unique.
#'
#' If they are more than one identical absolute paths, that function will throw an
#' error.
#' @noRd
shorten_src_file_path <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  if (anyDuplicated(x) > 0L) {
    ncstopf(
      "the following src path(s) not unique: %s",
      paste(
        paste0("'", x[duplicated(x)], "'"),
        collapse = ", "
      )
    )
  }
  .make_unique_relative_path <- function(x, current, stack, N) {
    if (anyDuplicated(current) > 0L) {
      prev <- sapply(x, function(s) {
        len <- length(s)
        if (len > N) s[[len - N]] else NA_character_
      })
      prev.valid <- prev[!is.na(prev)]
      compound <- file.path(prev.valid, current[seq_along(prev.valid)])
      terminal <- current[is.na(prev)]
      if (length(terminal) > 0) {
        if (any(!is.na(terminal))) {
          terminal.non.na <- terminal[!is.na(terminal)]
          stack <- c(terminal.non.na, stack)
        }
      }
      N <- N + 1L
      Recall(x, compound, stack, N)
    } else {
      stack <- c(current, stack)
      stack
    }
  }
  out <- vector("character")
  bases <- basename(x)
  bases.unique_dups <- unique(bases[duplicated(bases)])
  if (length(bases.unique_dups) > 0L) {
    traversed <- sapply(bases.unique_dups, function(uniq) {
      uniq.inds <- bases %in% uniq
      uniq.taken <- x[uniq.inds]
      splitted <- strsplit(uniq.taken, split = .Platform[["file.sep"]])
      .make_unique_relative_path(splitted, bases[uniq.inds], vector("character"), 1L)
    }, USE.NAMES = FALSE)
    out <- c(out, unlist(traversed))
  }
  uniques <- bases[!bases %in% bases.unique_dups]
  out <- c(out, uniques)
  ## just to be sure if everything is unique and same size:
  stopifnot(identical(anyDuplicated(out), 0L))
  stopifnot(identical(length(out), length(x)))
  out
}

