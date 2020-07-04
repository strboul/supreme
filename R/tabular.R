
#' Turn supreme data into a `data.frame`
#'
#' @param x a `supreme` object.
#' @param ... methods to be passed onto.
#' @return a `data.frame`.
#' @examples
#' paths <- example_app_path()
#' sp <- supreme(src_file(paths))
#' as.data.frame(sp)
#' @export
as.data.frame.supreme <- function(x, ...) {
  supreme_to_df(x[["data"]])
}


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

      tbl <- if (length(value) > 0L && field %in% multi_fields) {
        if (identical(field, "calling_modules") && is.null(unlist(value))) {
          data.frame(I(list(value)), stringsAsFactors = FALSE)
        } else {
          data.frame(I(value), stringsAsFactors = FALSE)
        }
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

