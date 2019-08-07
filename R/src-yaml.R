
#' Read a yaml file containing model
#'
#' @param file file path to a yaml file.
#' @param text a YAML formatted character string.
#'
#' @examples \dontrun{
#' file <- file.path("yaml-test", "example-model-1.yaml")
#' src_yaml(file)
#'
#' model <- "
#' - type: module
#'   name: childModuleA
#'   input: [input.data, reactive]
#'   calling_modules: grandChildModule1
#'
#' - type: module
#'   name: childModuleB
#'   input: selected.model
#' "
#' src_yaml(text = model)
#' }
#' @importFrom yaml yaml.load_file yaml.load
#' @importFrom mmy text_trunc
#' @export
src_yaml <- function(file = NULL, text = NULL) {
  if (is.null(file) && is.null(text)) {
    ncstopf("Provide a file or text.")
  }
  if (is.null(file)) {
    if (is.null(text)) {
      ncstopf("Provide a file or text, not both.")
    } else {
      obj <- yaml::yaml.load(text)
    }
  } else {
    if (file.exists(file)) {
      if (is.null(text)) {
        obj <- yaml::yaml.load_file(file)
      } else {
        ncstopf("Provide a file or text, not both.")
      }
    } else {
      ncstopf(
        "File not found: `%s`",
        mmy::text_trunc(file, value = 35L, sep = " "),
        single.line = TRUE
      )
    }
  }
  verify_yaml(obj)
  structure(obj, class = c("src_obj", "src_yaml"))
}

#' @export
print.src_yaml <- function(x, ...) {
  cat("Model yaml object", "\n")
}

#' Verify yaml object for supreme
#'
#' The loaded yaml model can be verified against the structure of an supreme
#' object model. The errors catched during the parsing of yaml file will be handled
#' by the *yaml* package.
#'
#' @param x a list (yaml) object.
#'
#' @details
#'
#' + Checks whether yaml object contains sub-lists
#'
#' + Checks whether yaml object does miss some or all required fields
#'
#' + Checks whether yaml object contains any other field not existing in either
#' required or optional fields
#'
#' @return returns (invisibly) true if everything is fine.
#' @noRd
verify_yaml <- function(x) {
  if (!is_list(x)) {
    ncstopf("cannot verify object with a class of: '%s'", class(x))
  }
  if (!is.null(names(x))) {
    ncstopf("malformed yaml model")
  }
  required <- SUPREME_REQUIRED_FIELDS
  optional <- SUPREME_OPTIONAL_FIELDS
  for (xi in seq_along(x)) {
    nst <- vapply(seq_along(x[[xi]]), function(zi) {
      is.list(x[[xi]][[zi]])
    }, logical(1))
    anst <- any(nst)
    if (anst) {
      ncstopf(paste(
        "model yaml cannot contain sub-list in",
        paste0("'", xi, "'"),
        "element:", paste("'", names(x[[xi]])[nst], "'", sep = "", collapse = ", ")
      ))
    }
    anst
  }
  for (i in seq_along(x)) {
    elem <- x[[i]]
    required.names <- required %in% names(elem)
    if (!all(required.names)) {
      ncstopf(
        paste(
          "required fields are missing in",
          paste0("'", i,"'"),
          "element:",
          paste("'", required[!required.names], "'", sep = "", collapse = ", ")
        )
      )
    }
    all.names <- c(required, optional)
    req.opt.names <- names(elem) %in% all.names
    if (!all(req.opt.names)) {
      ncstopf(
        paste(
          "the following names not required or optional:",
          paste("'", names(elem)[!req.opt.names], "'", sep = "", collapse = ", ")
        )
      )
    }
  }
  invisible(TRUE)
}

