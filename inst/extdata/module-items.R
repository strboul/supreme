
items_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    p("Please select items for the plot below:"),
    main_table_ui(ns("items_tbl")),
    br(),
    items_plot_ui(ns("items_plot")),
    br()
  )
}

items_tab_module_server <- function(input, output, session) {

  items.data <- reactive({
    read.table(
      file = file.path("data", "example_items.csv"),
      sep = ",",
      header = TRUE
    )
  })

  tbl <- callModule(
    main_table_server,
    "items_tbl",
    items.data,
    tbl.pageLength = 5L,
    tbl.selection = "multiple"
  )

  items.selected.row.inds <- reactive({ tbl$selected })

  callModule(
    items_plot_server,
    "items_plot",
    data = items.data,
    selected.items = items.selected.row.inds
  )

}

items_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

items_plot_server <- function(input, output, session, data, selected.items) {
  output$plot <- renderPlot({
    ggplot(data(), aes(list_price, quantity)) +
      geom_point(colour = "blue") +
      geom_point(data = data()[selected.items(), ], aes(list_price, quantity), colour = "red", size = 4L) +
      facet_wrap(~ category) +
      theme_minimal()
  })
}

