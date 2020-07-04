
#' Create a supreme object
#'
#' @param x a valid source input.
#'
#' @return a `supreme` object.
#' @examples
#' path <- example_app_path()
#' supreme(src_file(path))
#' @export
supreme <- function(x) {
  if (!is_source_object(x)) {
    ncstopf("the provided input cannot be turned into a supreme object")
  }
  ret <- list(
    data = unclass(x),
    source_input = class(x)
  )
  structure(ret, class = "supreme")
}


#' @export
print.supreme <- function(x, ...) {
  dta <- x[["data"]]
  len.dta <- length(dta)
  nms <- vapply(seq_along(dta), function(i) dta[[i]][["name"]], character(1))
  nms.disp <- if (length(nms) > 4L) {
    c(nms[seq(4L)], "...")
  } else {
    nms
  }
  cat(
    paste(
      "A supreme model object",
      paste0(
        len.dta,
        if (len.dta > 1) " entities" else " entity",
        ": ",
        paste(nms.disp, sep = "", collapse = ", ")
      ),
      sep = "\n"
    ),
    "\n"
  )
  invisible(NULL)
}


is_supreme <- function(x) {
  inherits(x, "supreme")
}


is_source_object <- function(x) {
  inherits(x, "supreme_src_obj")
}

