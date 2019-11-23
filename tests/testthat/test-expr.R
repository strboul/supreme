
context("test-expr")

test_that("can read expression objects", {

  e <- expression({

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

  expect_equal(src_expr(e),
               structure(list(
                 list(
                   name = "moduleA",
                   calling_modules = c("childModule1Server",
                                       "childModule2Server"),
                   src = NULL
                 ),
                 list(
                   name = "moduleB",
                   calling_modules = "someModule",
                   src = NULL
                 ),
                 list(
                   name = "emptyModuleFunction",
                   calling_modules = NULL,
                   src = NULL
                 ),
                 list(
                   name = "moduleWithoutAnyCallingModules",
                   calling_modules = NULL,
                   src = NULL
                 )
               ), class = c("src_obj", "src_expr")))

})
