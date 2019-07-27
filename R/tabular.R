
#' Turn supreme data into a data frame
#'
#' @param x a `supreme` object.
#' @param ... additional arguments to be passed to or from methods.
#' @examples \dontrun{
#' paths <- supreme_example()
#' Object <- supreme(src_file(paths))
#' as.data.frame(Object)
#' }
#' @export
as.data.frame.supreme <- function(x, ...) {
  if (!is_supreme(x)) {
    ncstopf("cannot coerce a non-supreme object")
  }
  supreme_to_df(x$data[[1]])
}

#' Turn supreme data into a data frame
#'
#' @param x an object.
#' @noRd
supreme_to_df <- function(x) {
  req_fields <- SUPREME_REQUIRED_FIELDS
  opt_fields <- SUPREME_OPTIONAL_FIELDS
  all_fields <- c(req_fields, opt_fields)
  full.tbl <- do.call(rbind, lapply(seq_along(x), function(xi) {
    mod <- x[[xi]]
    entity <- do.call(cbind, lapply(seq_along(all_fields), function(u) {
      field <- all_fields[[u]]
      value <- mod[[field]]
      if (is.null(value)) {
        value <- NA_character_
      }
      if (length(value) > 1L) {
        value <- paste(value, collapse = ", ")
      }
      tbl <- if (field %in% names(mod)) {
        data.frame(value, stringsAsFactors = FALSE)
      } else {
        data.frame(NA_character_, stringsAsFactors = FALSE)
      }
      names(tbl) <- field
      tbl
    }))
    entity
  }))
  full.tbl
}

