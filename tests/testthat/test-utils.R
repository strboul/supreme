
context("test-utils")

test_that("testing ncstopf", {
  f <- function(x) g(x)
  g <- function(x) h(x)
  h <- function(x) j(x)
  j <- function(x) k(x)
  k <- function(x) l(x)
  l <- function(x) m(x)
  m <- function(x) n(x)
  n <- function(x) ncstopf("something went wrong", internal = TRUE)

  expect_error(f(1))
})

test_that("is_named_list", {
  expect_false(is_named_list(list(1, 2)))
  expect_false(is_named_list(list(A = 1, 2)))
  expect_true(is_named_list(list(A = 1, B = 2)))
})

