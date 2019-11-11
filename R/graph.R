
#' Checks whether the type field is called as "module"
#'
#' @details
#' This function throws an error if the type is not equal to a *character string*
#' called "module". If not, it returns (invisibly) `TRUE`.
#' @noRd
check_type_module <- function(type) {
  stopifnot(is.character(type))
  if (!identical(type, "module")) {
    ncstopf("type should be a \"module\", not \"%s\"", type, internal = TRUE)
  }
  invisible(TRUE)
}

#' @examples
#' (dir <- graph_create_general_directives(
#' list(direction = "down", font = "Arial", fontSize = 11, padding = 8)
#' ))
#' @noRd
graph_create_general_directives <- function(directives) {
  stopifnot(is.list(directives))
  if (is.null(names(directives)) || any(nchar(names(directives)) == 0L)) {
    ncstopf("directive names must be properly named", internal = TRUE)
  }
  out <- do.call(pastenc, lapply(seq_along(directives), function(i) {
    dr <- directives[i]
    paste0("#", names(dr), ": ", dr[[1]])
  }))
  out
}

#' @examples
#' graph_sanitize_classifier_name("my_great_MoDule123_21")
#' @noRd
graph_sanitize_classifier_name <- function(name, random.str.len = 15L) {
  stopifnot(identical(length(name), 1L))
  sanitized <- local({
    no.digits <- gsub("[[:digit:]]+", "", name)
    no.underscore <- gsub("\\_+", "", no.digits)
    no.capital <- tolower(no.underscore)
    no.dot <- gsub("\\.+", "", no.capital)
    no.dot
  })
  random <- paste(sample(c(letters), random.str.len), collapse = "")
  add.random <- paste0(sanitized, random)
  list(
    original = name,
    result = add.random
  )
}

#' @examples
#' graph_generate_custom_classifier("my_great_MoDule123_21")
#' graph_generate_custom_classifier("server", list("fill" = "#8f8", "italic", "dashed"))
#' @noRd
graph_generate_custom_classifier <- function(classifier.name, styles = NULL) {
  if (is.null(styles)) {
    ## white background node is default:
    styles <- list("fill" = "#fff")
  }
  list.styles <- vapply(seq_along(styles), function(i) {
    st <- styles[i]
    if (names(st) != "") {
      paste(names(st), st[[1L]], sep = "=")
    } else {
      st[[1L]]
    }
  }, character(1))
  sanitized.name <- graph_sanitize_classifier_name(classifier.name)
  out <- paste0("#.", sanitized.name$result, ": ", paste(list.styles, collapse = " "))
  list(
    original = classifier.name,
    classifier = sanitized.name$result,
    classifier.str = out
  )
}

### ----------------------------------------------------------------- ###
### CREATE METHODS ----
### ----------------------------------------------------------------- ###

#' @examples
#' x <- list(list(type = "module", name = "childModuleA",
#' input = c("input.data", "reactive"), output = c("output1", "output2"),
#' return = "ret", calling_modules = "grandChildModule1"))
#' (node <- graph_create_node(x[[1]]))
#'
#' ## create a node with a classifier:
#' cls <- graph_generate_custom_classifier(x[[1]][["name"]])$classifier
#' (node_cls <- graph_create_node(x[[1]], classifier = cls))
#'
#' ## with some missing fields:
#' y <- list(list(type = "module", name = "childModuleB", input = "data"))
#' graph_create_node(y[[1]])
#' @noRd
graph_create_node <- function(x, classifier = NULL) {
  check_type_module(x[["type"]])
  if (!is.null(classifier)) {
    if (!is.character(classifier)) {
      classifier <- as.character(classifier)
    }
  }
  .create_multi_vars_field <- function(e, add.bullet = TRUE, add.quote = FALSE) {
    if (!is.null(e)) {
      if (add.bullet) e <- paste("•", e)
      if (add.quote) e <- paste0("\"", e, "\"")
      paste(e, collapse= ";")
    } else {
      ""
    }
  }
  .create_calling_modules_field <- function(e) {
    # TODO
  }
  Name <- x[["name"]]
  Classifier <- if (!is.null(classifier)) paste0("<", classifier, ">") else ""
  Inputs <- .create_multi_vars_field(x[["input"]])
  Outputs <- .create_multi_vars_field(x[["output"]])
  Returns <- .create_multi_vars_field(x[["return"]],
                                      add.bullet = FALSE,
                                      add.quote = TRUE)
  CallingModules <- .create_multi_vars_field(x[["calling_modules"]])
  name_classifier <- paste0(Classifier, Name)
  paste0("[",
         paste(name_classifier,
               Inputs,
               Outputs,
               Returns,
               CallingModules,
               sep = "|"),
         "]")
}

#' @examples
#' x <- list(list(type = "module", name = "childModuleA",
#' input = c("input.data", "reactive"), output = c("tbl1", "tbl2"),
#' return = "ret", calling_modules = "grandChildModule1"),
#' list(type = "module", name = "childModuleB", input = NULL,
#' calling_modules = NULL))
#' graph_create_edge(x[[1]])
#' graph_create_edge(x[[2]])
#' @noRd
graph_create_edge <- function(x) {
  check_type_module(x[["type"]])
  if (is.null(x[["calling_modules"]])) {
    return(NULL)
  }
  out <- paste(
    paste0(
      "[", x[["name"]], "]",
      "->",
      "[", x[["calling_modules"]], "]"
    ),
    collapse = "\n"
  )
  out
}

#' @examples
#' x <- list(list(type = "module", name = "childModuleA",
#' input = c("input.data", "reactive"), output = c("output1", "output2"),
#' return = "ret", calling_modules = "grandChildModule1"))
#' (cons <- graph_construct(x))
#' cat(cons, "\n")
#'
#' y <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"), list(type = "module", name
#' = "childModuleB", input = NULL, calling_modules = NULL))
#' graph_construct(y)
#' @noRd
graph_construct <- function(x) {
  sub_body <- do.call(pasten, lapply(seq_along(x), function(i) {
    custom_classifier <- graph_generate_custom_classifier(
      x[[i]][["name"]],
      styles = list("align" = "center", "bold"))
    if (identical(custom_classifier$original, x[[i]][["name"]])) {
      custom_classifier_name <- custom_classifier$classifier
    } else {
      ncstopf("something really went wrong", internal = TRUE)
    }
    paste(
      custom_classifier$classifier.str,
      graph_create_node(x[[i]], classifier = custom_classifier_name),
      graph_create_edge(x[[i]]),
      sep = "\n"
    )
  }))
  body <- list(
    graph_create_general_directives(list(
      direction = "down",
      font = "Arial",
      fontSize = 11,
      padding = 8
    )),
    "",
    sub_body
  )
  out <- do.call(pastenc, body)
  structure(out, class = "graph_construct")
}

#' @importFrom nomnoml nomnoml
#' @noRd
graph_render <- function(construct) {
  stopifnot(inherits(construct, "graph_construct"))
  nomnoml::nomnoml(construct)
}

#' Make a graph of modules
#'
#' @description
#' The call graph creates a UML like graph of your Shiny application.
#'
#' @param x a `supreme` object.
#'
#' @examples \dontrun{
#' path <- example_app_path()
#' supr <- supreme(src_file(path))
#' graph(supr)
#' }
#' @export
graph <- function(x) {
  if (!is_supreme(x)) {
    ncstopf("cannot graph a non-supreme object. Input class is: '%s'", class(x))
  }
  constructs <- graph_construct(x$data)
  graph_render(constructs)
}

