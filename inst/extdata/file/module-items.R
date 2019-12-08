
items_tab_module_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

items_tab_module_server <- function(input, output, session, items_list, is_fired) {

  rv_items_list <- reactive({
    if (is_fired) {

    }
  })

  callModule(module_modal_dialog, NULL)

}

