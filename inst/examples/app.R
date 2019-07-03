
library(shiny)

ui <- fluidPage(
  customers_page_module_ui("CustomersPage"),
  stock_page_module_ui("StockPage"),
  transactions_page_module_ui("TransactionsPage")
)

server <- function(input, output, session) {

  callModule(module = customers_page_module_server, id = "CustomersPage")
  callModule(module = stock_page_module_server, id = "StockPage")
  callModule(module = transactions_page_module_server, id = "TransactionsPage")

}

shinyApp(ui, server)

