
main_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    dataTableOutput(ns("tbl"))
  )
}

main_table_server <- function(input, output, session, data,
                              tbl_pageLength = 10,
                              tbl_selection = c("none", "single", "multiple"))
  {

  tbl_selection <- match.arg(tbl_selection)

  rv <- reactiveValues(selected = NULL)

  output$tbl <- renderDataTable({
    datatable(
      data(),
      style = "bootstrap",
      selection = list(
        mode = tbl_selection,
        selected = if (tbl_selection != "none") 1L else NULL
      ),
      options = list(
        pageLength = tbl_pageLength
      )
    )
  })


  observeEvent(input$tbl_rows_selected, {
    rv$selected <- input$tbl_rows_selected
  }, ignoreNULL = FALSE)


  return({ rv })
}

