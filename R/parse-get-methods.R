
get_server_block <- function(x) {
  x <- subset_expr(x)
  server <- find_block(x, "server")
  server
}

get_block_modules <- function(x) {
  x <- subset_expr(x)
  modules <- find_block_modules(x)
  if (is.null(modules)) {
    ncstopf("cannot find modules")
  }
  modules
}
