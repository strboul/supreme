
#' Example app path for `supreme`
#'
#' @export
example_app_path <- function() {
  pkg <- system.file("examples", package = "supreme", mustWork = TRUE)
  app <- list.files(pkg,
                    pattern = "app\\.R$",
                    full.names = TRUE)
  modules <- list.files(pkg,
                        pattern = "^module.*\\.R$",
                        full.names = TRUE)
  c(app, modules)
}

