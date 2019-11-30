
transactions_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    main_table_ui(ns("transactions_tbl")),
    br()
  )
}

transactions_tab_module_server <- function(input, output, session) {

  transactions_data <- reactive({
    read.table(
      file = file.path("data", "example_transactions.csv"),
      sep = ",",
      header = TRUE
    )
  })

  callModule(
    main_table_server,
    "transactions_tbl",
    transactions_data
  )
}

