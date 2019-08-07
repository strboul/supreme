
#' Get paths to `supreme` example
#'
#' Contains the paths of the Shiny application that is a fully-fledged example
#' endeavors to demonstrate all the features `supreme` has.
#'
#' @param file file names. If no file names are put (which `path` is `NULL`), then
#'   all the example file paths will be listed.
#'
#' @examples
#' example_app_path()
#' example_app_path(c("app.R", "module-customers.R"))
#' @export
example_app_path <- function(file = NULL) {
  pkg <- system.file("extdata", package = "supreme", mustWork = TRUE)
  files <- list.files(pkg, full.names = TRUE)
  if (is.null(file)) {
    files
  } else {
    files[basename(files) %in% file]
  }
}

#' Get package to `supreme` example
#'
#' This function asks to build packages in the users' system if they are not found.
#'
#' @param pkg a list contains name (*package name*) and path (*GitHub path*) fields.
#'   For the example purposes, the default value is `supreme.pkg.test`.
#' @importFrom devtools install_github
#' @export
example_package <- function(pkg = list(name = "supreme.pkg.test",
                                       path = "strboul/supreme.pkg.test")) {

  ## check 'pkg' arg. fields:
  if (!all(c("name", "path") %in% names(pkg))) {
    ncstopf("Specify all fields in the example package.")
  }

  if (!is_package_exist(pkg$name)) {
    answer <- if (interactive()) {
      menu(
        choices = c("Yes", "No"),
        title = paste(
          "\n",
          paste0(
            "The test package '",
            pkg$name,
            "' not found in the system."),
          "Do you want to install it to run the tests?",
          "",
          paste0(
            "The package placed under the repository: '",
            paste0("https://github.com/", pkg$path),
            "'"
          ),
          sep = "\n"
        )
      )
    } else {
      1L
    }
    if (identical(answer, 1L)) {
      devtools::install_github(pkg$path)
    }
  }

  if (is_package_exist(pkg$name)) {
    pkg$name
  } else {
    ncstopf("Cannot find the package '%s' in the system.", pkg$name)
  }
}

#' Get environment to `supreme` example
#' @export
example_environment <- function() {
  e <- local({
    module1_ui <- function(id) {
    }
    module1_server <- function(input, output, session, input1) {
    }
    module2_ui <- function(id) {
    }
    module2_server <- function(input, output, session, input2) {
    }
    module3_ui <- function(id) {
    }
    module3_server <- function(input, output, session, input3) {
    }
    moduleA_ui <- function(id) {
      shiny::tagList(
        module1_ui("1"),
        module2_ui("2"),
        module3_ui("3")
      )
    }
    moduleA_server <- function(input, output, session) {
      callModule(module1_server, "1")
      callModule(module2_server, "2")
      callModule(module3_server, "3")
    }
    ui <- shiny::fluidPage(
      moduleA_ui("A")
    )
    server <- function(input, output, session) {
      callModule(moduleA_server, "A")
    }
  })
  environment(e)
}

