
#' Object checkers
#'
#' @param x a valid \R object.
#' @name objcheck
#' @noRd
NULL


#' Check if an object is a list (but not a data.frame)
#'
#' @rdname objcheck
#' @noRd
is_list <- function(x) {
  is.list(x) && !is.data.frame(x)
}


#' Checks whether a list is named
#'
#' @param x a list object.
#' @rdname objcheck
#' @noRd
is_named_list <- function(x) {
  stopifnot(is_list(x))
  !(is.null(names(x)) || any(names(x) == ""))
}


#' Symbol checkers
#'
#' @param x a valid \R expression.
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
is_dollar_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`$`))
}


#' @rdname objsymcheck
#' @noRd
is_double_bracket_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`[[`))
}


#' @rdname objsymcheck
#' @noRd
is_func_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`function`))
}


#' @rdname objsymcheck
#' @noRd
is_callModule_sym <- function(x) {
  is_callModule_exist_in_shiny()
  is.symbol(x) && identical(x, quote(`callModule`))
}


#' @rdname objsymcheck
#' @noRd
is_output_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`output`))
}


#' @rdname objsymcheck
#' @noRd
is_return_sym <- function(x) {
  is.symbol(x) && identical(x, quote(`return`))
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
    fun_formals <- find_inputs(x)
  } else {
    return(FALSE)
  }
  shiny_compulsory_formals <- c("input", "output", "session")
  all(shiny_compulsory_formals %in% fun_formals)
}

#' Checks if `shiny::callModule` function exists (and exported) in Shiny package
#' @noRd
is_callModule_exist_in_shiny <- function() {
  has_callmodule <- exists("callModule", where = asNamespace("shiny"), mode = "function")
  if (!has_callmodule) {
    ncstopf(
      "your 'Shiny' version (%s) doesn't seem to have `callModule` function.",
      utils::packageVersion("shiny")
    )
  }
}

