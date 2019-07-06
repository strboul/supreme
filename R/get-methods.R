
get_server_block <- function(x) {
  x <- subset_expr(x)
  server <- find_block(x, "server")
  if (is.null(server)) {
    ncstopf("cannot find server")
  }
  if (length(server) > 1) {
    ncstopf("cannot proceed because 'server' is defined multiple times")
  }
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
