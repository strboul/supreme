
transactions_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("transactions_table"))
  )
}

transactions_tab_module_server <- function(input, output, session, table, button_clicked) {

  output$transactions_table <- renderTable({
    table
  })

  transactions_keys <- reactive({
    if (button_clicked) {
      table[["keys"]]
    }
  })

  return(transactions_keys)
}

