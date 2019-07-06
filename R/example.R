
#' Example app path for `supreme`
#'
#' @export
example_app_path <- function() {
  list.files(
    system.file("examples", package = "supreme"),
    pattern = "\\.R$",
    full.names = TRUE
  )
}

