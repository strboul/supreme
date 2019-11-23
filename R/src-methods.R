
### ----------------------------------------------------------------- ###
### PUBLIC ----
### ----------------------------------------------------------------- ###

#' Read \R files
#'
#' Read files contain at least one Shiny application.
#'
#' @param x a file path.
#' @examples
#' paths <- example_app_path()
#' s <- supreme(src_file(paths))
#' @family source functions
#' @export
src_file <- function(x) {
  tryCatch({ file.exists(as.character(x)) }, error = function(e)
    ncstopf("cannot read file: %s", conditionMessage(e)))
  obj <- .make_module_entities_from_paths(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_file"))
}


#' Read a YAML file containing model
#'
#' Reads an object or a file in YAML format and returns a model YAML object.
#'
#' @param file file path to a YAML file.
#' @param text a YAML formatted character string.
#'
#' @examples \dontrun{
#' ## Read from file:
#' file <- file.path("yaml-test", "example-model-1.yaml")
#' src_yaml(file)
#'
#' ## Read from text object:
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
#' @family source functions
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
        substr(file, 1, 35),
        single.line = TRUE
      )
    }
  }
  .verify_yaml(obj)
  structure(obj, class = c("src_obj", "src_yaml"))
}


#' Read \R expressions
#'
#' @param x an \R expression.
#' @family source functions
#' @export
src_expr <- function(x) {
  if (!length(x) == 1L) {
    ncstopf("expression length must be one, instead of: %s", length(x))
  }
  obj <- .make_module_entities_from_expression(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_expr"))
}


#' Read an \R package
#'
#' The package should contain some Shiny application.
#'
#' @param x a package name as character (must be installed in the system).
#' @family source functions
#' @export
src_pkg <- function(x) {
  ## enforce input to be a character.
  if (!is.character(x)) x <- as.character(x)
  if (!is_package_exist(x)) {
    ncstopf("package '%s' not found.", x)
  }
  obj <- make_src_pkg(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_pkg"))
}


#' Read \R environment
#'
#' @param x an \R environment.
#' @family source functions
#' @export
src_env <- function(x) {
  if (!is.environment(x)) {
    ncstopf("input must be environment, not:", typeof(x))
  }
  obj <- make_src_env(x)
  out <- entity_constructor(obj)
  structure(out, class = c("src_obj", "src_env"))
}


### ----------------------------------------------------------------- ###
### S3 PRINTS ----
### ----------------------------------------------------------------- ###

#' @export
print.src_file <- function(x, ...) {
  cat("Model file object", "\n")
}


#' @export
print.src_yaml <- function(x, ...) {
  cat("Model yaml object", "\n")
}


#' @export
print.src_expr <- function(x, ...) {
  cat("Model expression object", "\n")
}


#' @export
print.src_pkg <- function(x, ...) {
  cat("Model package object", "\n")
}


#' @export
print.src_env <- function(x, ...) {
  cat("Model environment object", "\n")
}


### ----------------------------------------------------------------- ###
### PRIVATE ----
### ----------------------------------------------------------------- ###

#' Read src file
#'
#' A small subset of [base::getSrcLines].
#'
#' @param x a file name.
#' @return A parsed expression.
#' @noRd
.read_srcfile <- function(x) {
  lijnen <- lapply(seq_along(x), function(i) {
    fname <- x[i]
    srcfile <- srcfile(fname)
    if (!.isOpen(srcfile)) {
      on.exit(close(srcfile), add = TRUE)
    }
    first <- 1L
    conn <- open(srcfile, first)
    lines <- readLines(conn, warn = FALSE)
    # encoding stuff:
    Enc <- srcfile$Enc
    if (!is.null(Enc) && !(Enc %in% c("unknown", "native.enc"))) {
      lines <- iconv(lines, "", Enc)
    }
    lines
  })
  lijnen <- unlist(lijnen)
  ## Wrap`{`, `}` quotes in between as the system is designed around exprs"
  lines <- paste("{", paste(lijnen, collapse = "\n"), "}")
  parse(text = lines)
}


#' Make module entities from file paths
#'
#' @param x file paths.
#' @noRd
.make_module_entities_from_paths <- function(x) {
  short.src <- shorten_src_file_path(x)
  out <- lapply(seq_along(short.src), function(i) {
    src <- short.src[i]
    path <- x[grep(src, x)]
    body <- .read_srcfile(path)
    list(body = body, src = src)
  })
  structure(out, class = "module_entities")
}


#' Verify YAML object for supreme
#'
#' The loaded YAML model can be verified against the structure of an supreme
#' object model. The errors catched during the parsing of YAML file will be handled
#' by the *yaml* package.
#'
#' @param x a list (YAML) object.
#'
#' @details
#'
#' + Checks whether YAML object contains sub-lists
#'
#' + Checks whether YAML object does miss some or all required fields
#'
#' + Checks whether YAML object contains any other field not existing in either
#' required or optional fields
#'
#' @return returns (invisibly) true if everything is fine.
#' @noRd
.verify_yaml <- function(x) {
  if (!is_list(x)) {
    ncstopf("cannot verify object with a class of: '%s'", class(x))
  }
  if (!is.null(names(x))) {
    ncstopf("malformed YAML model")
  }
  required <- getOption("SUPREME_MODEL_REQUIRED_FIELDS")
  optional <- getOption("SUPREME_MODEL_OPTIONAL_FIELDS")
  for (xi in seq_along(x)) {
    nst <- vapply(seq_along(x[[xi]]), function(zi) {
      is_list(x[[xi]][[zi]])
    }, logical(1))
    anst <- any(nst)
    if (anst) {
      ncstopf(paste(
        "model YAML cannot contain sub-list in",
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


#' Make module entities from file paths
#'
#' @param x file paths.
#' @noRd
.make_module_entities_from_expression <- function(x) {
  out <- list(list(body = x, src = NULL))
  structure(out, class = "module_entities")
}

