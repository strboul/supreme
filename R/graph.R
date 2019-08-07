
#' @examples
#' graph_create_general_directives(list(direction = "down", font = "Arial", fontSize = 11, padding = 8))
#' @noRd
graph_create_general_directives <- function(directives) {
  out <- do.call(pastenc, lapply(seq_along(directives), function(i) {
    dr <- directives[i]
    paste0("#", names(dr), ": ", dr[[1]])
  }))
  out
}

#' @examples
#' graph_sanitize_classifier_name("my_great_MoDule123_21")
#' @noRd
graph_sanitize_classifier_name <- function(name) {
  stopifnot(length(name) == 1L)
  no.digits <- gsub("[[:digit:]]+", "", name)
  no.underscore <- gsub("\\_+", "", no.digits)
  no.capital <- tolower(no.underscore)
  random <- paste(sample(c(letters), 10), collapse = "")
  add.random <- paste0(no.capital, random)
  list(
    original = name,
    result = add.random
  )
}

#' @examples
#' graph_create_custom_classifier("my_great_MoDule123_21")
#' graph_create_custom_classifier("server", list("fill" = "#8f8", "italic", "dashed"))
#' @noRd
graph_create_custom_classifier <- function(classifier.name, styles = NULL) {
  if (is.null(styles)) {
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

#' @examples
#' x <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"))
#' graph_create_node(x[[1]])
#' ## create a node with a classifier:
#' cls <- graph_create_custom_classifier(x[[1]][["name"]])$classifier
#' graph_create_node(x[[1]], classifier = cls)
#'
#' ## without any calling_modules:
#' y <- list(list(type = "module", name = "childModuleB", input =
#' "data", calling_modules = NULL))
#' graph_create_node(y[[1]])
#' @noRd
graph_create_node <- function(x, classifier = NULL) {
  if (!identical(x[["type"]], "module")) {
    stop("block should be a module")
  }
  if (!is.null(classifier)) {
    if (!is.character(classifier)) {
      classifier <- as.character(classifier)
    }
  }
  Name <- x[["name"]]
  Classifier <- if (!is.null(classifier)) paste0("<", classifier, ">") else ""
  Inputs <- if (!is.null(x[["input"]])) paste(x[["input"]], collapse= ";") else ""
  CallingModules <- if (!is.null(x[["calling_modules"]])) {
    paste(paste0(x[["calling_modules"]], "()"), collapse = "; ")
  } else {
    ""
  }
  paste0("[",
         paste(paste0(Classifier, Name),
               Inputs, CallingModules, sep = "|"),
         "]")
}

#' @examples
#' x <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"), list(type = "module", name
#' = "childModuleB", input = NULL, calling_modules = NULL))
#' graph_create_edge(x[[1]])
#' graph_create_edge(x[[2]])
#' @noRd
graph_create_edge <- function(x) {
  if (!identical(x[["type"]], "module")) stop("block type not module")
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
#' x <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"))
#' graph_construct(x)
#'
#' y <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"), list(type = "module", name
#' = "childModuleB", input = NULL, calling_modules = NULL))
#' graph_construct(y)
#' @noRd
graph_construct <- function(x) {
  sub_body <- do.call(pasten, lapply(seq_along(x), function(i) {
    custom_classifier <- graph_create_custom_classifier(x[[i]][["name"]])
    if (custom_classifier$original == x[[i]][["name"]]) {
      custom_classifier_name <- custom_classifier$classifier
    } else {
      stop("internal: something went wrong")
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

#' @examples
#' x <- list(list(type = "module", name = "childModuleA", input = c("input.data",
#' "reactive"), calling_modules = "grandChildModule1"))
#' c <- graph_construct(x)
#' graph_render(c)
#' @importFrom nomnoml nomnoml
#' @noRd
graph_render <- function(construct) {
  stopifnot(inherits(construct, "graph_construct"))
  nomnoml::nomnoml(construct)
}

#' Make a graph of modules
#'
#' @param x a `supreme` object.
#'
#' @export
graph <- function(x) {
  if (!is_supreme(x)) {
    ncstopf("cannot graph a non-supreme object. Input class is: '%s'", class(x))
  }
  constructs <- graph_construct(x$data[[1L]])
  graph_render(constructs)
}

