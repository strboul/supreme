
### ----------------------------------------------------------------- ###
### GRAPH UTILS ----
### ----------------------------------------------------------------- ###

#' @examples
#' (dir <- graph_create_general_directives(
#' list(direction = "down", font = "Arial", fontSize = 11, padding = 8)
#' ))
#' @noRd
graph_create_general_directives <- function(directives) {
  stopifnot(is_list(directives))
  if (!is_named_list(directives)) {
    ncstopf("directive names must be properly named", internal = TRUE)
  }
  out <- do.call(pastenc, lapply(seq_along(directives), function(i) {
    dr <- directives[i]
    paste0("#", names(dr), ": ", dr[[1]])
  }))
  out
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
  sanitized.name <- .graph_classifier_sanitize_name(classifier.name)
  out <- paste0("#.", sanitized.name$result, ": ", paste(list.styles, collapse = " "))
  list(
    original = classifier.name,
    classifier = sanitized.name$result,
    classifier.str = out
  )
}


.graph_classifier_sanitize_name <- function(name, random_str_len = 15L) {
  stopifnot(identical(length(name), 1L))
  sanitized <- local({
    no_digits <- gsub("[[:digit:]]+", "", name)
    no_underscore <- gsub("\\_+", "", no_digits)
    no_capital <- tolower(no_underscore)
    no_dot <- gsub("\\.+", "", no_capital)
    no_dot
  })
  random <- paste(sample(letters, random_str_len), collapse = "")
  add_random <- paste0(sanitized, random)
  list(
    original = name,
    result = add_random
  )
}


#' This creates a comment in the graph body that may be helpful for debugging.
#' @noRd
create_graph_comment <- function(comment) {
  stopifnot(is.character(comment))
  paste("//", comment)
}


#' Center the field names displayed in the nodes. An invisible unicode character is
#' used as a hidden quote because nomnoml only display spaces if they are between the
#' character strings, otherwise the leading and trailing whitespaces are trimmed.
#' @noRd
centre_graph_strings <- function(x, quotes = "\u2063") {
  if (is.null(x)) return(x)
  if (!length(x) > 0) return(NULL)
  stopifnot(is.character(x))
  centred <- format(x, justify = "centre")
  paste(quotes, centred, quotes)
}


### ----------------------------------------------------------------- ###
### CREATE NODE ----
### ----------------------------------------------------------------- ###

graph_create_node <- function(x, classifier = NULL, centre = TRUE) {

  if (!(is.null(classifier) && !is.character(classifier))) {
    classifier <- as.character(classifier)
  }

  node <- list()
  node$identifier <- paste(
    if (!is.null(classifier)) paste0("<", classifier, ">") else "",
    x[["name"]]
  )

  node$input <- .node_create_multi_vars_field(x[["input"]], bullet = "triangular")
  node$output <- .node_create_multi_vars_field(x[["output"]], bullet = "circle")

  node$return <- .node_create_single_var_field(x[["return"]], bullet = "square")

  node$calling_modules <- .node_create_calling_modules_field(
    calling_modules = x[["calling_modules"]],
    centre = centre
  )

  .node_generate_string_node(node)
}


#' @param empty_to_null change empty strings to `NULL`.
#' @noRd
.node_generate_string_node <- function(node, empty_to_null = TRUE) {
  if (empty_to_null) node[node == ""] <- NULL
  nd_sep <- do.call(function(...) paste(..., sep = " | "), node)
  paste0("[", nd_sep, "]")
}


.node_create_calling_modules_field <- function(calling_modules, centre = TRUE) {
  as.vector(vapply(calling_modules, function(cm) {
    server_module <- names(cm)
    ui_module <- paste0("<", unlist(cm, use.names = FALSE), ">")
    c(server_module, ui_module)
  }, character(2))) -> out
  if (centre) {
    out <- centre_graph_strings(out)
  }
  paste(out, collapse = ";")
}


.node_create_single_var_field <- function(e, bullet, quote = TRUE) {
  bullet_sym <- getOption("SUPREME_GRAPH_BULLET_SYMBOLS")[[bullet]]
  if (!is.null(e)) {
    if (quote) e <- paste0("\"", e, "\"")
    e <- paste(bullet_sym, e)
    e
  } else {
    ""
  }
}


.node_create_multi_vars_field <- function(e, bullet, quote = FALSE) {
  bullet_sym <- getOption("SUPREME_GRAPH_BULLET_SYMBOLS")[[bullet]]
  if (!is.null(e)) {
    if (quote) e <- paste0("\"", e, "\"")
    e <- paste(bullet_sym, e)
    paste(e, collapse= ";")
  } else {
    ""
  }
}


graph_create_edge <- function(x) {
  if (is.null(x[["calling_modules"]])) return(NULL)
  edge <- list()
  edge$name <- x[["name"]]
  edge$calling_modules <- sapply(x[["calling_modules"]], names)
  .edge_generate_string_edge(edge)
}


.edge_generate_string_edge <- function(edge) {
  paste(
    paste0(
      "[", edge$name, "]",
      "->",
      "[", edge$calling_modules, "]"
    ),
    collapse = "\n"
  )
}


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

    node_comment <- paste(paste(rep("=", 8), collapse = ""),
                          custom_classifier$original,
                          paste(rep("=", 8), collapse = ""))
    paste(
      create_graph_comment(node_comment),
      custom_classifier$classifier.str,
      graph_create_node(x[[i]], classifier = custom_classifier_name),
      graph_create_edge(x[[i]]),
      "\n",
      sep = "\n"
    )
  }))
  body <- list(
    graph_create_general_directives(list(
      direction = "down",
      font = "Courier New",
      arrowSize = 0.5,
      fontSize = 11,
      padding = 8
    )),
    "",
    sub_body
  )
  out <- do.call(pastenc, body)
  structure(out, class = "supreme_graph_construct")
}


#' @importFrom nomnoml nomnoml
#' @noRd
graph_render <- function(construct) {
  stopifnot(inherits(construct, "supreme_graph_construct"))
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

