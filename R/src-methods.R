
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
#' @examples
#'
#' \dontrun{
#' ## Read from file:
#' file <- file.path("path", "to", "model.yaml")
#' src_yaml(file)
#' }
#'
#' ## Read from text object:
#' model <- "
#' - name: childModuleA
#'   input: [input.data, reactive]
#'   src: package
#'
#' - name: childModuleB
#'   input: selected.model
#' "
#' src_yaml(text = model)
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
#' @details
#' The function coerces input to be a character.
#' @family source functions
#' @export
src_pkg <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  if (!is_package_exist(x)) {
    ncstopf("package '%s' not found.", x)
  }
  obj <- .make_src_pkg(x)
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
  obj <- .make_src_env(x)
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


#' Make module entities from file paths
#'
#' @param x file paths.
#' @noRd
.make_module_entities_from_expression <- function(x) {
  out <- list(list(body = x, src = NULL))
  structure(out, class = "module_entities")
}


#' Make module entities from packages
#'
#' (Packages are essentially environments)
#'
#' @importFrom utils packageName
#' @noRd
.make_src_pkg <- function(pkg.name) {
  stopifnot(is.character(pkg.name))
  ns <- asNamespace(pkg.name)
  ## verify package name
  stopifnot(length(utils::packageName(asNamespace(pkg.name))) > 0L)
  entities <- .make_module_entities_from_environment(ns)
  out <- list(list(body = entities, src = paste("package", pkg.name, sep = ":")))
  structure(out, class = "module_entities")
}


#' Make module entities from environment objects
#'
#' @noRd
.make_src_env <- function(envir) {
  stopifnot(is.environment(envir))
  entities <- .make_module_entities_from_environment(envir)
  out <- list(list(body = entities, src = "expression"))
  structure(out, class = "module_entities")
}


#' Make module entities from an environment
#'
#' @param x an environment.
#' @details
#' This call does not take all objects from an environment. Objects from a given
#' environment will be eliminated:
#' 1. if it is not a function,
#' 2. (it is a function), but if it is not a Shiny component.
#' @noRd
.make_module_entities_from_environment <- function(x) {
  stopifnot(is.environment(x))
  objs <- mget(ls(x, all.names = TRUE), envir = x)
  funs <- Filter(is.function, objs)
  server_components <- Filter(is_shiny_server_component, funs)
  exprs <- vector("list", length(server_components))
  for (i in seq_along(server_components)) {
    elem <- server_components[i]
    elem.name <- names(elem)
    elem.sub <- elem[[1L]]
    Arguments <- formals(elem.sub)
    Body <- body(elem.sub)
    Function <- substitute({name <- fun}, list(
      name = as.name(elem.name),
      fun = as.function(x = c(Arguments, Body))
    ))[[2L]]
    # TODO maybe tlist?
    exprs[[length(exprs) + 1L]] <- Function
  }
  exprs <- as.call(c(as.name("{"), exprs))
  exprs <- as.expression(exprs)
  exprs
}

### ----------------------------------------------------------------- ###
### VERIFY YAML ----
### ----------------------------------------------------------------- ###

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

  for (entity in x) {
    .verify_yaml_check_names_and_missing(entity)
    .verify_yaml_check_field_depth(entity)
  }

  invisible(TRUE)
}


.verify_yaml_check_names_and_missing <- function(
  x,
  required = getOption("SUPREME_MODEL_REQUIRED_FIELDS"),
  optional = getOption("SUPREME_MODEL_OPTIONAL_FIELDS")) {

  required.names <- required %in% names(x)
  if (!all(required.names)) {
    ncstopf(
      "%s field(s) required for every element",
      paste("'", required[!required.names], "'", sep = "", collapse = ", ")
    )
  }
  all.names <- c(required, optional)
  req.opt.names <- names(x) %in% all.names
  if (!all(req.opt.names)) {
    ncstopf(
      paste(
        "following name(s) not required or optional:",
        paste("'", names(x)[!req.opt.names], "'", sep = "", collapse = ", ")
      )
    )
  }
}

#' Also checks the calling_modules field that if it is formed properly
#' A proper formation is that:
#' - a module item is put as a sublist of calling_modules field and the module ends
#' with a colon
#' @noRd
.verify_yaml_check_field_depth <- function(x) {
  for (xi in seq_along(x)) {

    current <- x[xi]
    current_key <- names(current)
    current_value <- current[[1L]]

    ## calling modules field treated differently:
    if (identical(current_key, "calling_modules")) {
      must_list <- any(vapply(current_value, is_list, logical(1)))
      if (!must_list) {
        ncstopf(
          "'%s' field must have a UI part, a proper name or NULL (~)",
          current_key
        )
      }
      too_depth <- any(vapply(current_value, function(val) {
        vapply(val, is_list, logical(1))
      }, logical(1)))
      if (too_depth) {
        ncstopf("model YAML cannot contain too depth lists in '%s'", current_key)
      }
    } else {
      ## the rest of the fields:
      res <- is_list(current_value)
      if (res) {
        ncstopf("model YAML cannot contain too depth lists in '%s'", current_key)
      }
    }

  }
}

