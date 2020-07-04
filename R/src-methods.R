### ----------------------------------------------------------------- ###
### PUBLIC ----
### ----------------------------------------------------------------- ###

#' Read \R files
#'
#' Read files contain at least one Shiny application.
#'
#' @param x a file path.
#'
#' @return A `src_file` object.
#' @examples
#' paths <- example_app_path()
#' s <- supreme(src_file(paths))
#' @family source functions
#' @export
src_file <- function(x) {
  check_paths_exist(x)
  obj <- .make_module_entities_from_paths(x)
  out <- entity_constructor(obj)
  if (!length(out) > 0L) {
    ncstopf("cannot parse the file.")
  }
  structure(out, class = c("supreme_src_obj", "supreme_src_file"))
}


#' Read a YAML file containing a model
#'
#' Reads an object or a file in YAML format and returns a model YAML object.
#'
#' @param file file path to a YAML file.
#' @param text a YAML formatted character string.
#' @return A `src_yaml` object.
#'
#' @examples
#' ## Read from a file:
#' path <- example_yaml()
#' src_yaml(path)
#'
#' ## Read from an (text) object:
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
  structure(obj, class = c("supreme_src_obj", "supreme_src_yaml"))
}


#' @export
print.supreme_src_obj <- function(x, ...) {
  cls <- setdiff(class(x), "supreme_src_obj")
  switch (cls,
    "supreme_src_file" = "file",
    "supreme_src_yaml" = "yaml",
    NULL
  ) -> type
  stopifnot(!is.null(type))
  cat("Model", type, "object", "\n")
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
#' @details
#' Lines starting with commenting symbol # (hash) are removed from the character
#' vector before sending to parsing. Normally, `parse()` automatically removes the
#' comments;however, we do it before anyway to avoid any potential problems, which
#' can be caused by the paste collapsing.
#'
#' Before parsing, the character vector is wrapped between curly braces (`{` and `}`)
#' as the system is designed around exprs. Also put new lines before the quotes to be
#' sure that they are not commented out from # a previous commented line.
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
  if (length(commented.lines <- grep("^#", lijnen)) > 0L) {
    lijnen <- lijnen[-commented.lines]
  }
  lines <- paste("{\n", paste(lijnen, collapse = "\n"), "\n}")
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
  structure(out, class = "supreme_module_entities")
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

