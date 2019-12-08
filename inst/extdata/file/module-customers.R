
customers_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("paid_customers_table")),
    tableOutput(ns("free_customers_table")),
    br()
  )
}

customers_tab_module_server <- function(input, output, session, customers_list) {

  output$paid_customers_table <- renderTable({
    subset(customers_list, category == "Paid")
  })

  output$free_customers_table <- renderTable({
    subset(customers_list, category == "Free")
  })

}

