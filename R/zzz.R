
#nocov start

.onLoad <- function(libname, pkgname) {
  op <- options()
  op_supreme <- list(
    SUPREME_MODEL_REQUIRED_FIELDS = c("type", "name"),
    SUPREME_MODEL_OPTIONAL_FIELDS = c("input", "output", "calling_modules", "src")
  )
  toset <- !(names(op_supreme) %in% names(op))
  if(any(toset)) options(op_supreme[toset])

  invisible()
}

## Variables to prevent R CMD notes:
utils::globalVariables(c("callModule", "moduleA_server"))

#nocov end

