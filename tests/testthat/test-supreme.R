
context("test-supreme")

test_that("supreme with source yaml", {

  ex_file_path <- file.path("yaml-test", "example-model-1.yaml")

  s <- supreme(src_yaml(ex_file_path))


  expect_equal(s$source_input, "src_yaml")

})

# test_that("supreme with source expr", {
#   supreme(src_expr())
# })
#
#
# test_that("supreme with source file", {
#   supreme(src_file())
# })

