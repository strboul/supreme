
library(shiny)
library(DT)
library(ggplot2)

sapply(list.files(pattern = "^module", full.names = TRUE), source)

ui <- fluidPage(
  titlePanel("Best clothes dashboard"),
  tabsetPanel(id = "tabs",
              tabPanel(title = "Items",
                       value = "ItemsTab",
                       items_tab_module_ui("ItemsTab")
              ),
              tabPanel(title = "Customers",
                       value = "CustomersTab",
                       customers_tab_module_ui("CustomersTab")
              ),
              tabPanel(title = "Transactions",
                       value = "TransactionsTab",
                       transactions_tab_module_ui("TransactionsTab")
              ),
              tabPanel(title = "About",
                       value = "AboutTab",
                       br(),
                       paste("This is a purely fictional corporation.",
                             "All the data displayed here is totally fake.")
              )
  )
)

server <- function(input, output, session) {

  callModule(module = items_tab_module_server, id = "ItemsTab")
  callModule(module = customers_tab_module_server, id = "CustomersTab")
  callModule(module = transactions_tab_module_server, id = "TransactionsTab")

}

shinyApp(ui, server)

