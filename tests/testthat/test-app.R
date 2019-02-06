
############################
### FOR TESTING PURPOSES ###
############################

source("test-long-module.R")

library(shiny)

child_module_ui <- function(ns) {
  tagList(
    actionButton("go", "Go") # an inline comment (inside curly braces)
  )
}

# a new line comment (outside curly braces)
child_module_server <- function(input, output, session, childInput) {

  rv <- reactiveValues(test = FALSE)

  # a new line comment (inside curly braces)
  observeEvent(input$go, {
    rv$test <- TRUE
  })

  return({ rv })
}

module1_ui <- function(ns) {
  p("module1")
}

module1_server <- function(input, output, session, customInput1, customInput2) {

  normalFUN <- function(a, b, c) {
    a + b + c
  }

  output$view <- renderText({
    "# Header 1 not a comment but a part of code"
    '## Header 2, same as one but wrapped in single quote'
  })

  return({ input$btn })

}

module2_ui <- function(ns) {
  p("module2")
}

# the formals after the assignment operator are in the next line:
module2_server <- function(input, output, session, customInput1, customInput2) {

  output$view <- renderText({

  })

  return({ input$btn })

}

ui <- fluidPage(
  module1_ui("module1"),
  module2_ui("module2")
)

server <- function(input, output, session) {
  callModule(module = module1_server, id = "module1") # with "module" arg.name
  callModule(module2_server, id = "module2") # without arg. name, first arg value
  callModule(module3_server, "module3") # without any arg. name
}

shinyApp(ui, server)

############################
### FOR TESTING PURPOSES ###
############################
