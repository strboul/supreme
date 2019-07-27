
#' Read an R file containing Shiny application
#'
#' @param x a file path.
#' @examples
#' paths <- supreme_example()
#' s <- supreme(src_file(paths))
#' @export
src_file <- function(x) {
  tryCatch({ file.exists(as.character(x)) }, error = function(e)
    ncstopf("cannot read file: %s", conditionMessage(e)))
  obj <- make_module_entities(x)
  structure(obj, class = "src_file")
}

#' Read src file
#'
#' A small subset of [base::getSrcLines].
#'
#' @param x a file name.
#' @return A parsed expression.
#' @noRd
read_srcfile <- function(x) {
  lijnen <- lapply(seq_along(x), function(i) {
    fname <- x[i]
    srcfile <- srcfile(fname)
    if (!.isOpen(srcfile)) {
      on.exit(close(srcfile))
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

#' Workhorse for the `src_file()` call
#' @param x
#' @noRd
make_module_entities <- function(x) {
  modules <- lapply(seq_along(x), function(i) {
    path <- x[i]
    body <- read_srcfile(path)
    src <- path
    list(body = body, src = src)
  })
  fields <- c(SUPREME_REQUIRED_FIELDS, SUPREME_OPTIONAL_FIELDS)
  res <- list()
  for (i in seq_along(modules)) {
    entity <- modules[[i]]
    src <- entity[["src"]]
    entity.body <- entity[["body"]][[1]]
    which.components <- which(sapply(entity.body, is_shiny_server_component))
    for (c in which.components) {
      f.body <- entity.body[[c]]
      name <- find_block_assignment_name(f.body)
      calling_modules <- find_block_modules(f.body)
      out <- list(
        type = "module",
        name = name,
        calling_modules = calling_modules,
        src = src
      )
      res[[length(res)+1L]] <- out
    }
  }
  res
}

