
#' Turn supreme data into a data frame
#'
#' @param x a `supreme` object.
#' @param ... additional arguments to be passed to or from methods.
#' @examples \dontrun{
#' paths <- example_app_path()
#' Object <- supreme(src_file(paths))
#' as.data.frame(Object)
#' }
#' @export
as.data.frame.supreme <- function(x, ...) {
  if (!is_supreme(x)) {
    ncstopf("cannot coerce a non-supreme object")
  }
  supreme_to_df(x$data)
}

#' Turn supreme data into a data.frame
#'
#' @param x an object.
#' @noRd
supreme_to_df <- function(x) {
  req_fields <- getOption("SUPREME_MODEL_REQUIRED_FIELDS")
  opt_fields <- getOption("SUPREME_MODEL_OPTIONAL_FIELDS")
  multi_fields <- getOption("SUPREME_MODEL_MULTI_VAR_FIELDS")
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
        value <- list(value)
      }
      if (!field %in% names(mod)) {
        value <- NA_character_
      }
      tbl <- if (field %in% multi_fields) {
        data.frame(I(value), stringsAsFactors = FALSE)
      } else {
        data.frame(value, stringsAsFactors = FALSE)
      }
      names(tbl) <- field
      tbl
    }))
    entity
  }))
  full.tbl
}

