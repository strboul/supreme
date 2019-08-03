
moduleB_ui <- function(id) {
  ns <- NS(id)
  tagList(
    childModule1_ui(ns("1"))
  )
}

moduleB_server <- function(input, output, session) {
  callModule(childModule1_server, "1")
}

