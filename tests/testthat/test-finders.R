
test_that("find_binding_name", {

  example1 <- expression({
    moduleServer <- function(input, output, session) {
    }
  })
  expect_equal(find_binding_name(example1[[1]]), "moduleServer")

})


test_that("find_inputs", {

  example1 <- expression({
    someModule <- function(input, output, session, data, button) {
    }
  })
  expect_equal(
    find_inputs(example1[[1]]),
    c("input", "output", "session", "data", "button")
  )

  example2 <- expression({
    rnorm100 <- function(x) {
      val <- x * 100
      rnorm(val)
    }
  })
  expect_equal(find_inputs(example2[[1]]), "x")

  example3 <- expression({
    funWithoutInput <- function() {
      rnorm(1)
    }
  })
  expect_null(find_inputs(example3[[1]]))

})


test_that("find_outputs", {

  example1 <- expression({
    tableDisplayModule <- function(input, output, session) {
      output$tbl <- renderTable({})
      output$controllers <- renderUI({})
    }
  })
  expect_equal(find_outputs(example1[[1]]), c("tbl", "controllers"))

  example2 <- expression({
    outputWithBrackets <- function(input, output, session) {
      output[["tbl"]] <- renderTable({})
      output[["controllers"]] <- renderUI({})
    }
  })
  expect_equal(find_outputs(example2[[1]]), c("tbl", "controllers"))

  example3 <- expression({
    outputInsideExprs <- function(input, output, session) {
      if (pass) {
        if (cond) {
          output$Table <- renderTable({})
        } else {
          output[["Controllers"]] <- renderUI({})
        }
      }
    }
  })
  expect_equal(find_outputs(example3[[1]]), c("Table", "Controllers"))

})


test_that("find_returns", {

  example1 <- expression({
    sampleModule <- function(input, output, session) {
      out <- rnorm(100)
      return(out)
    }
  })
  expect_equal(find_returns(example1[[1]]), "out")

  example2 <- expression({
    doubleReturnModule <- function(input, output, session) {
      if (flag) {
        return("Even")
      } else if (!flag) {
        return("Odd")
      } else {
        return(NULL)
      }
    }
  })
  expect_equal(find_returns(example2[[1]]), c("Even", "Odd", "NULL"))

  example3 <- expression({
    noExplicitReturnModule <- function(input, output, session) {
      x ^ 2
    }
  })
  expect_null(find_returns(example3[[1]]))

  example4 <- expression({
    returnValuesInCurlyBraces <- function(input, output, session, flag_value) {
      if (flag_value) {
        table <- reactive({1})
        return({ table })
      } else if (!flag_value) {
        plot <- reactive({2})
        return({ plot })
      } else {
        return(other)
      }
    }
  })
  expect_equal(find_returns(example4[[1]]), c("table", "plot", "other"))

})


test_that("find_calling_modules", {

  example1 <- expression({
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
    find_calling_modules(example1[[1]]),
    list(
      list(childModule1Server = "childModule1UI"),
      list(childModule2Server = "childModule2UI"),
      list(someModule = "someModuleUI")
    )
  )

  example2 <- expression({
    moduleWithoutUIPart <- function(input, output, session) {
      callModule(moduleServer, NULL)
    }
  })

  expect_equal(
    find_calling_modules(example2[[1]]),
    list(
      list(moduleServer = NULL)
    )
  )

})

