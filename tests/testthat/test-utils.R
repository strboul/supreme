
context("test-utils")

test_that("testing ncstopf", {
  f <- function(x) {
    g(x)
  }

  g <- function(x) {
    h(x)
  }

  h <- function(x) {
    ncstopf("something went wrong", internal = TRUE)
  }

  expect_error(f(1))
})

