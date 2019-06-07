
#%##############################%#
###    FOR TESTING PURPOSES    ###
#%##############################%#

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
  )
}

child_module1_server <- function(input, output, session) {

  callModule(module = grand_child_module1_server,
             id = "ui_id")

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

library(shiny)

ui <- fluidPage(
  h1("Shiny Module test")
)

server <- function(input, output, session) {

  callModule(module = parent_module1_server,
             id = "ui_id")

  callModule(
    id = "ui_id",
    module = parent_module2_server
  )

}

shinyApp(ui, server)


#%##############################%#
###    FOR TESTING PURPOSES    ###
#%##############################%#
