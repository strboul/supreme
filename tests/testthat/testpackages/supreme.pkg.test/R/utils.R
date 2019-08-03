
a_module_for_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("tbl"))
  )
}

a_module_for_table_server <- function(input, output, session, table) {
  output$tbl <- renderTable({
    req(table())
    table()
  })
}

#' @export
an_object <- 2L

#' A normal function
#' @export
square <- function(x) {
  x ^ 2
}

