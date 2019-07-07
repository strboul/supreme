
#' Create a supreme object
#'
#' @param x file name(s) containing valid Shiny application(s).
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
  server.module.names <- get_block_modules(server)
  server.child <-

  ret <- list(
    components = list(
      server_side = list(
        server = server.modules
      ),
      ui_side = "TODO"
    ),
    data = list(
      body = body,
      meta = list(
        x = x,
        expr = expr
      )
    )
  )

  structure(ret, class = "supreme")
}

#' @export
print.supreme <- function(x, ...) {
  cat(
    paste(
      "A supreme object",
      # TODO supreme_summary
      sep = "\n"
      ),
    "\n"
  )
}

is_supreme <- function(x) {
  inherits(x, "supreme")
}
