
#' Check objects
#'
#' @param x a valid \R expression.
#'
#' @name objcheck
#' @noRd
NULL

#' Checks if an object is a list (but not a data.frame)
#'
#' @param x an R object.
#' @rdname objcheck
#' @noRd
is_list <- function(x) {
  is.list(x) && !is.data.frame(x)
}

#' @rdname objcheck
#' @noRd
is_expression <- function(x) {
  is.expression(x) && is.language(x)
}

#' Checks the symbol of a call (the first element)
#'
#' @param x a valid \R expression.
#'
#' @name objsymcheck
#' @noRd
NULL

#' @rdname objsymcheck
#' @noRd
is_left_assign_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`<-`))
}

#' @rdname objsymcheck
#' @noRd
is_expr_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`{`))
}

#' @rdname objsymcheck
#' @noRd
is_func_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`function`))
}

#' @rdname objsymcheck
#' @noRd
is_callModule_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`callModule`))
}

#' Shiny expression checkers
#'
#' @param x a valid \R expression, a Shiny function body.
#' @name shinyexprcheck
#' @noRd
NULL

#' @rdname shinyexprcheck
#' @noRd
is_shiny_server_component <- function(x) {
  fun.formals <- if (is.language(x)) {
    find_formals(x)
  } else if (is.function(x)) {
    names(formals(x))
  } else {
    return(FALSE)
  }
  shiny.compulsory.formals <- c("input", "output", "session")
  all(shiny.compulsory.formals %in% fun.formals)
}

