
context("test-find")

test_that("can find arguments", {
  file <- file.path("data", "module-output.Rtest")
  p <- read_srcfile(file)
  block <- find_block(p, "linkedScatter")

  expect_equal(
    find_formals(block),
    c("input", "output", "session", "data", "left", "right")
  )

  expect_null(
    find_formals(quote(`{`))
  )

})

test_that("test is_shiny_server_component", {

  expr <- expression({
    server <- function(input, output, session) {
      data <- reactive({ mtcars })
      # ...
    }
  })

  expect_true(
    is_shiny_server_component(expr[[1]][[2]])
  )

  expr2 <- expression({
    square <- function(x) {
      x ^ 2
    }
  })

  expect_false(
    is_shiny_server_component(expr2[[1]][[2]])
  )

})

test_that("find block modules", {

  expr1 <- expression({

    moduleA <- function(input, output, session, data) {
      observe({
        req(data())
        callModule(childModule1Server, "childModule1UI")
      })
      callModule(childModule2Server, "childModule2UI")
    }

    moduleB <- function(input, output, session) {
      callModule(someModule, "someModuleUI")
    }

    emptyModuleFunction <- function(input, output, session) {
    }

    moduleWithoutAnyCallingModules <- function(input, output, session) {
      meann <- data.frame(mean = tapply(iris$Sepal.Length, iris$Species, mean))
      output$tbl <- renderTable({
        meann
      })
    }

    normalFunction <- function(x) x + 2

  })

  expect_equal(
    find_block_calling_modules(expr1[[1]]),
    c("childModule1Server", "childModule2Server", "someModule")
  )


  file1 <- read_srcfile("data/without-any-calling-module.Rtest")
  expect_null(find_block_calling_modules(file1[[1]]))

})

