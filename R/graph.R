
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
#'
#' @param sep_lines adds separation lines to make commentted lines more visually
#' appealing.
#' @noRd
create_graph_comment <- function(comment, sep_lines = FALSE) {
  stopifnot(is.character(comment))
  out <- paste("//", comment)
  if (sep_lines) {
    sep_lines_txt <- paste(rep("=", 8), collapse = "")
    out <- paste("//", sep_lines_txt, comment, sep_lines_txt)
  }
  out
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


#' Filters the input `x` list by modifying the input list in place.
#' @noRd
graph_filter_fields <- function(x, fields) {
  req_fields <- getOption("SUPREME_MODEL_REQUIRED_FIELDS")
  excepts <- which(!names(x) %in% c(req_fields, fields))
  x[excepts] <- NULL
  x
}


#' Set graph styles for a particular `entity`
#'
#' @description
#' The default_style is always added.
#' @noRd
graph_set_styles <- function(entity_name, entity_style) {
  default_style <- list("align" = "center", "bold")
  style <- if (!is.null(entity_style)) {
    c(default_style, entity_style)
  } else {
    default_style
  }
  graph_generate_custom_classifier(entity_name, style)
}


graph_construct <- function(x, fields, styles) {

  do.call(pasten, lapply(seq_along(x), function(i) {

    entity <- x[[i]]
    entity_name <- entity[["name"]]
    entity_style <- styles[[entity_name]]

    custom_classifier <- graph_set_styles(entity_name, entity_style)
    if (identical(custom_classifier$original, entity_name)) {
      custom_classifier_name <- custom_classifier$classifier
    }

    ## create node elements:
    out <- list()
    out$comment <- create_graph_comment(custom_classifier$original, sep_lines = TRUE)
    out$classifier <- custom_classifier$classifier.str

    ## node fields filtered here, edges are still valid.
    out$node <- if (!is.null(fields)) {
      entity_filtered <- graph_filter_fields(entity, fields)
      graph_create_node(entity_filtered, custom_classifier_name)
    } else {
      graph_create_node(entity, custom_classifier_name)
    }
    out$edge <- graph_create_edge(entity)

    paste(
      out$comment,
      out$classifier,
      out$node,
      out$edge,
      "\n",
      sep = "\n"
    )
  })) -> sub_body

  directives <- graph_create_general_directives(list(
    direction = "down",
    font = "Courier New",
    arrowSize = 0.5,
    fontSize = 11,
    padding = 8
  ))

  body <- list(directives, "", sub_body)
  out <- do.call(pastenc, body)
  structure(out, class = "supreme_graph_construct")
}


#' @importFrom nomnoml nomnoml
#' @noRd
graph_render <- function(construct) {
  stopifnot(inherits(construct, "supreme_graph_construct"))
  nomnoml::nomnoml(construct)
}


### ----------------------------------------------------------------- ###
### GRAPH ----
### ----------------------------------------------------------------- ###


#' Validates the "fields" argument of the `graph` call
#'
#' @param x object returned from the `fields` argument.
#' @noRd
graph_fields_validator <- function(x) {
  if (!is.character(x)) {
    ncstopf("`fields` argument must be a character vector")
  }
  req_fields <- getOption("SUPREME_MODEL_REQUIRED_FIELDS")
  opt_fields <- getOption("SUPREME_MODEL_OPTIONAL_FIELDS")
  all_fields <- c(req_fields, opt_fields)
  check_opt_fields <- x %in% all_fields
  if (!all(check_opt_fields)) {
    ncstopf(
      "unknown `fields` supplied: %s",
      paste(paste0("\"", x[!check_opt_fields], "\""), collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Validates the "styles" argument of the `graph` call
#'
#' @param x object returned from the `styles` argument.
#' @param data the data element of the supreme object.
#' @noRd
graph_styles_validator <- function(x, data) {
  if (!is_list(x) && !is_named_list(x)) {
    ncstopf("`styles` must be a \"named list\" object")
  }
  sub_list <- vapply(x, is_list, logical(1))
  if (!all(sub_list)) {
    ncstopf(
      "objects inside the `styles` argument must be a list, see the element: %s",
      which(!sub_list)
    )
  }
  styles_names <- names(x)
  module_names <- sapply(data, `[[`, "name")
  module_names_check <- styles_names %in% module_names
  if (!all(module_names_check)) {
    ncstopf(
      "module names specified in `styles` cannot be found: %s",
      paste(paste0("\"", styles_names[!module_names_check], "\""), collapse = ", ")
    )
  }
  invisible(TRUE)
}


#' Make a graph of modules
#'
#' @description
#' The call graph creates a *UML-like graph* from your *Shiny* application.
#'
#' @param x a `supreme` object.
#' @param fields name of the fields to include in the graph. By default, the required
#'   fields such as the "name" field always visible. There are no ways to exclude
#'   the required fields. This parameter is set to `NULL` as default.
#' @param styles a named list to apply custom styles on the graph nodes. A full list
#'  of the available styles can be seen from this link:
#'  \url{https://github.com/skanaar/nomnoml#custom-classifier-styles}
#'
#' @details
#' The graph call uses the `nomnoml` tool to draw a UML diagram of the Shiny
#' application.
#'
#' @references
#' \href{https://github.com/skanaar/nomnoml}{nomnoml: The sassy UML diagram renderer}
#' @examples
#' path <- example_yaml()
#' sp <- supreme(src_yaml(path))
#' graph(sp)
#'
#' ## Filter fields, only return the certain fields in the graph entities:
#' graph(sp, fields = c("input", "return"))
#'
#' ## Style entites:
#' graph(sp, styles = list(
#'  "server" = list(fill = "#ff0", "underline", "bold"),
#'  "module_modal_dialog" = list(fill = "lightblue", "dashed", visual = "note")
#' ))
#'
#' ## Style entities having a word "tab" in it:
#' sp_df <- as.data.frame(sp) # turn supreme object to data.frame
#' tab_modules <- sp_df$name[grep("_tab_", sp_df$name)]
#' styles <- lapply(seq_along(tab_modules), function(x) list(fill = "orange"))
#' names(styles) <- tab_modules
#' graph(sp, styles = styles)
#'
#' @export
graph <- function(x, fields = NULL, styles = NULL) {
  if (!is_supreme(x)) ncstopf("cannot graph a non-supreme object")
  sp_data <- x$data
  if (!is.null(fields)) graph_fields_validator(fields)
  if (!is.null(styles)) graph_styles_validator(styles, sp_data)
  constructs <- graph_construct(sp_data, fields, styles)
  graph_render(constructs)
}

