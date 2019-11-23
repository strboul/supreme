
#' Get paths to `supreme` example
#'
#' Contains the paths of the Shiny application that is a fully-fledged example
#' endeavors to demonstrate all the features `supreme` has.
#'
#' @param file file names. If no file names are put (which `path` is `NULL`), then
#'   all the example file paths will be listed.
#'
#' @examples
#' files <- example_app_path(c("app", "module-customers"))
#' supreme(src_file(files))
#' @family source examples
#' @export
example_app_path <- function(file = NULL) {
  pkg <- system.file("extdata", package = "supreme", mustWork = TRUE)
  files <- list.files(pkg, pattern = "\\.R$", full.names = TRUE)
  if (is.null(file)) {
    files
  } else {
    files[grep(file, files)]
  }
}


#' Get environment to `supreme` example
#'
#' @examples
#' env <- example_environment()
#' supreme(src_env(env))
#' @importFrom utils menu
#' @family source examples
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


#' Get expression to `supreme` example
#'
#' @examples
#' expr <- example_expression()
#' supreme(src_expr(expr))
#' @family source examples
#' @export
example_expression <- function() {
  e <- expression({
    text_ui <- function(id) {
      ns <- NS(id)
      tagList(textInput(ns("text"), "Write here"),
              verbatimTextOutput(ns("display")))
    }
    text_server <- function(input, output, session) {
      rv.text <- reactiveValues(val = FALSE)
      observeEvent(input$text, {
        rv.text$val <- input$text
      })
      output$display <- renderText({
        rv.text$val
      })
    }
    ui <- fluidPage(fluidRow(
      column(6,
             text_ui("text"))
    ))
    server <- function(input, output, session) {
      callModule(module = text_server, id = "text")
    }
    shinyApp(ui, server)
  })
}

