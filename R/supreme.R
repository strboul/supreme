
#' Create a module tree
#'
#' @param x an \R expression or a file name that contain a (valid) Shiny application.
#' @importFrom tools file_path_as_absolute
#' @export
tree_app <- function(x) {

  body <- if (file.exists(as.character(x))) {
    fullp <- tools::file_path_as_absolute(x)
    read_srcfile(fullp)
  } else if (is_expression(x)) {
    x
  } else {
    ncstopf("cannot handle input: `%s`", typeof(x))
  }

  server.block <- get_server_block(body)
  server.block.modules <- get_modules_from_block(server.block)

  list(server = unlist(server.block.modules))
}

