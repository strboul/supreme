
customers_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    main_table_ui(ns("customers_tbl")),
    br()
  )
}

customers_tab_module_server <- function(input, output, session) {

  customers.data <- reactive({
    read.table(
      file = file.path("data", "example_customers.csv"),
      sep = ",",
      header = TRUE
    )
  })

  callModule(
    main_table_server,
    "customers_tbl",
    customers.data
  )
}

