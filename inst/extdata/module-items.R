
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

  items_data <- reactive({
    read.table(
      file = file.path("data", "example_items.csv"),
      sep = ",",
      header = TRUE
    )
  })

  tbl <- callModule(
    main_table_server,
    "items_tbl",
    items_data,
    tbl_pageLength = 5L,
    tbl_selection = "multiple"
  )

  items_selected_row_inds <- reactive({ tbl$selected })

  callModule(
    items_plot_server,
    "items_plot",
    data = items_data,
    selected_items = items_selected_row_inds
  )

}

items_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

items_plot_server <- function(input, output, session, data, selected_items) {
  output$plot <- renderPlot({
    ggplot(data(), aes(list_price, quantity)) +
      geom_point(colour = "blue") +
      geom_point(data = data()[selected_items(), ], aes(list_price, quantity), colour = "red", size = 4L) +
      facet_wrap( ~ category) +
      theme_minimal()
  })
}

