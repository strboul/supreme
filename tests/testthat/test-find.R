
context("test-find")

test_that("can find arguments", {
  file <- file.path("data", "module-output.Rtest")
  p <- read_srcfile(file)
  block <- find_block(p, "linkedScatter")

  expect_equal(
    find_arguments(block),
    c("input", "output", "session", "data", "left", "right")
  )
})

