
#%########################################%#
###         FOR TESTING PURPOSES         ###
#%########################################%#

grand_child_module2_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

grand_child_module2_server <- function(input, output, session) {

}

grand_child_module1_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

grand_child_module1_server <- function(input, output, session) {

}

child_module1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("parent_module2")
  )
}

child_module1_server <- function(input, output, session) {

  callModule(module = grand_child_module1_server,
             id = "ui_id")

}


# PARENT MODULES ---------------------------------------------------------------

parent_module3_ui <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

parent_module3_server <- function(input, output, session) {

}

parent_module2_ui <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

parent_module2_server <- function(input, output, session) {

}

parent_module1_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

parent_module1_server <- function(input, output, session) {

  callModule(module = child_module1_server,
             id = "ui_id")

}


# SERVER -----------------------------------------------------------------------

library(shiny)

ui <- fluidPage(
  h1("Shiny Module test"),
  checkboxInput("checkbox", "Check this")
)

server <- function(input, output, session) {

  callModule(module = parent_module1_server,
             id = "ui_id")

  callModule(
    id = "ui_id",
    module = parent_module2_server
  )

  # selected <- reactive(input$checkbox)

  # TODO Do DFS when module names placed inside braces
  # observe({
  #   req(selected())
  #   callModule(module = parent_module3_server,
  #              id = "ui_id")
  # })

}

shinyApp(ui, server)

#%########################################%#
###         FOR TESTING PURPOSES         ###
#%########################################%#
