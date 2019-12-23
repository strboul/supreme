
test_that("is_named_list", {

  expect_false(is_named_list(list(1, 2)))
  expect_false(is_named_list(list(A = 1, 2)))

  expect_true(is_named_list(list(A = 1, B = 2)))
  expect_true(is_named_list(list(X = 1, "Y" = 2)))
  expect_true(is_named_list(list("X" = 1, Y = 2)))
  expect_true(is_named_list(list("X" = 1, "Y" = 2)))

})


test_that("is_shiny_server_component", {

  expr <- expression({
    server <- function(input, output, session) {
    }
  })
  expect_true(is_shiny_server_component(expr[[1]][[2]]))

  expr2 <- expression({
    square <- function(x) {
    }
  })
  expect_false(is_shiny_server_component(expr2[[1]][[2]]))

})

