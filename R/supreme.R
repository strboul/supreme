
#' Create a supreme object
#'
#' @param x a valid source input.
#' @export
supreme <- function(x) {
  if(!supreme_is_valid_input(x)) {
    ncstopf("the provided input cannot be turned into a supreme object")
  }
  ret <- list(
    components = list(
      server_side = NULL,
      ui_side = NULL
    ),
    data = list(
      x
    ),
    source_input = class(x)
  )
  if (!length(ret$data[[1]]) > 0) {
    warning("0 (zero) entries found in the source.", call. = FALSE)
  }
  structure(ret, class = "supreme")
}

# TODO finish the other srcs
supreme_is_valid_input <- function(x) {
  switch (class(x),
          "src_yaml" = TRUE,
          "src_env" = TRUE,
          "src_pkg" = TRUE,
          "src_file" = TRUE,
          "src_expr" = TRUE,
          FALSE
  )
}

#' @export
print.supreme <- function(x, ...) {
  dta <- x$data[[1]]
  len.dta <- length(dta)
  nms <- vapply(seq_along(dta), function(i) dta[[i]][["name"]], character(1))
  len.nms <- length(nms)
  nms.disp <- if (len.nms > 4L) {
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
