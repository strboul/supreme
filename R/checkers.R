
#' Check objects
#'
#' @param x a valid \R object.
#'
#' @details
#'
#' + `is_list()` checks if an object is a list (but not a data.frame).
#'
#' + `is_expression()` checks if an object is an \R expression (with an expression
#' class) that is also a `language` object.
#'
#' @name objcheck
#' @noRd
NULL

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
#' @details
#'
#' + `is_left_assign_sym` looks for `<-` symbol that is used to bind a name (a
#' symbol) to an object
#'
#' + `is_expr_sym` looks for a symbol that is often used for having compound
#' statements enclosed within `{` (left brace). That symbol is also named as `expr`.
#'
#' + `is_func_sym()` looks for `function` symbol which is used to create function
#' objects.
#'
#' + `is_callModule_sym()` looks for `callModule` symbol, which is a "prefix"
#' function name
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
  if (is.language(x) || is.function(x)) {
    fun.formals <- find_formals(x)
  } else {
    return(FALSE)
  }
  shiny.compulsory.formals <- c("input", "output", "session")
  all(shiny.compulsory.formals %in% fun.formals)
}

