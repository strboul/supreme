
#' @export
ui <- function() {
  fluidPage(
    titlePanel("Simple application inside package"),
    moduleA_ui("A"),
    moduleB_ui("B")
  )
}

#' @export
server <- function(input, output, session) {
  callModule(moduleA_server, "A")
  callModule(moduleB_server, "B")
}

