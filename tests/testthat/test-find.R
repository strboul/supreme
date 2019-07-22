
context("test-find")

test_that("can find arguments", {
  file <- file.path("data", "module-output.Rtest")
  p <- read_srcfile(file)
  block <- find_block(p, "linkedScatter")

  expect_equal(
    find_arguments(block),
    c("input", "output", "session", "data", "left", "right")
  )

  expect_null(
    find_arguments(quote(`{`))
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

