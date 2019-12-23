
#nocov start

.onLoad <- function(libname, pkgname) {
  op <- options()
  op_supreme <- list(
    SUPREME_MODEL_REQUIRED_FIELDS = "name",
    SUPREME_MODEL_OPTIONAL_FIELDS = c("input", "output", "return", "calling_modules", "src"),
    SUPREME_MODEL_MULTI_VAR_FIELDS = c("input", "output", "return", "calling_modules"),
    ## graph symbols from: https://unicode.org/charts/nameslist/n_25A0.html
    SUPREME_GRAPH_BULLET_SYMBOLS = list("circle" = "\u25CB",
                                        "triangular" = "\u25B9",
                                        "square" = "\u25FB")
  )
  toset <- !(names(op_supreme) %in% names(op))
  if(any(toset)) options(op_supreme[toset])

  invisible()
}

#nocov end

