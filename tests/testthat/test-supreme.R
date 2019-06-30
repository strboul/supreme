
context("test-supreme")

test_that("A nested server side test", {

  nested.server.test <- expression({

    library(shiny)

    data <- mtcars

    ui <- fluidPage(
      titlePanel("Module test application 1"),
      hr(),
      ConditionalItemsUI("ConditionalItems"),
      hr(),
      ObservedPanelUI("ObservedPanel"),
      hr(),
      SomeTabUI("SomeTab"),
      hr(),
      BarPlotPanelUI("BarPlotPanel"),
      hr(),
      CustomerListPanelUI("CustomerListPanel"),
      br()
    )

    server <- function(input, output, session) {

      ## `callModule` in the server
      callModule(module = SomeTabServer, id = "SomeTab")

      ## `callModule` in the server without argument names:
      callModule(BarPlotPanelServer, "BarPlotPanel")

      ## `callModule` in the server with different ordered argument names:
      callModule(id = "CustomerListPanel", module = CustomerListPanelServer)

      ## `callModule` inside `observe()` call
      observe(callModule(module = ObservedPanelServer, id = "ObservedPanel"))

      ## `callModule` inside `observe()` call wrapped between curly braces
      observe({
        req(someImportantData())
        callModule(module = ConditionalItemsServer, id = "ConditionalItems")
      })

      ## `callModule` inside `observe()` inside `reactive()` call where all calls are
      ## wrapped between curly braces
      react <- reactive({
        req(someImportantData())
        items1 <- callModule(
          module = ConditionalConditionalItems1Server,
          id = "ConditionalConditionalItems1"
        )
        observe({
          req(otherImportantData())
          callModule(
            module = ConditionalConditionalItems2Server,
            id = "ConditionalConditionalItems2"
          )
        })
      })

      ## `callModule` assigned to a variable:
      button <- callModule(id = "DetailsButton", module = DetailsButtonServer)

      ## an assigned constant variable:
      a <- 2L

    }

    shinyApp(ui, server)

  })

  server <- get_server_block(nested.server.test[[1]])

  server.modules <- get_modules_from_block(server[[1]])

  expect_equal(
    server.modules,
    list("SomeTabServer", "BarPlotPanelServer", "CustomerListPanelServer",
         "ObservedPanelServer", "ConditionalItemsServer", "ConditionalConditionalItems1Server",
         "ConditionalConditionalItems2Server", "DetailsButtonServer")
  )

})

test_that("When there's multiple symbols of 'server'", {

  multiple.server.symbols <- expression({

    library(shiny)

    ui <- fluidPage(
      p("Multiple server side functions defined!")
    )

    server <- function(input, output, session) {
      output$table <- renderTable({ head(iris) })
    }

    server <- function(input, output, session) {
      output$plot <- renderPlot({ plot(iris) })
    }

    shinyApp(ui, server)
  })

  #TODO server.blocks <- find_block(multiple.server.symbols, "server")
  expect_error(
    get_server_block(multiple.server.symbols[[1]])
  )
})

