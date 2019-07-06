
#' Create a supreme object
#'
#' @param x file name containing a (valid) Shiny application.
#' @param expr an \R expression.
#'
#' @export
supreme <- function(x, expr = NULL) {

  body <- if (missing(x)) {
    x <- NULL
    if (!is.null(expr)) {
      src_expr(expr)
    } else {
      ncstopf("provide an input")
    }
  } else {
    src_file(x)
  }

  ## server:
  server <- get_server_block(body)
  server.modules <- get_block_modules(server)

  ret <- list(
    data = list(
      body = body
    ),
    components = list(
      server = NULL,
      modules = list(
        server_modules,
        ui_modules
      )
    ),
    metadata = list(
      x = x,
      expr = expr
    )
  )

  structure(ret, class = "supreme")
}

#' @export
print.supreme <- function(x, ...) {
  cat(
    paste(
      "<supreme> object",
      # TODO supreme_summary
      sep = "\n"
      ),
    "\n"
  )
}

is_supreme <- function(x) {
  inherits(x, "supreme")
}
