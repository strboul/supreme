
library(shiny)

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

server <- function(input, output, session) { # some comment

  customers_data <- data.frame(
    id = seq(1e3),
    category = sample(c("Free", "Paid"), 1e3, replace = TRUE, prob = c(0.75, 0.25)),
    value = rnorm(1e3, 25, 20),
    stringsAsFactors = FALSE
  )

  items_data <- data.frame(no = seq(1e2))

  transactions_data <- data.frame(keys = seq(1e3), value = rnorm(1e3, 25, 20))

  callModule(module = items_tab_module_server, id = "ItemsTab",
             items_list = items_data,
             is_fired = TRUE)
  callModule(customers_tab_module_server, "CustomersTab", customers_list = customers_data)
  ## explanatory comment..
  callModule(id = "TransactionsTab", module = transactions_tab_module_server,
             table = transactions_data,
             button_clicked = TRUE)

}

shinyApp(ui, server) # comment1

# a comment at the end of the file

