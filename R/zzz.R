
#nocov start

.onLoad <- function(libname, pkgname) {
  op <- options()
  op_supreme <- list(
    SUPREME_MODEL_REQUIRED_FIELDS = "name",
    SUPREME_MODEL_OPTIONAL_FIELDS = c("input", "output", "return", "calling_modules", "src"),
    SUPREME_MODEL_MULTI_VAR_FIELDS = c("input", "output", "calling_modules")
  )
  toset <- !(names(op_supreme) %in% names(op))
  if(any(toset)) options(op_supreme[toset])

  invisible()
}

## Variables to prevent R CMD notes:
utils::globalVariables(c("callModule", "moduleA_server"))

#nocov end

