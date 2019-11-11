
context("test-supreme")

test_that("supreme with source yaml", {
  ex_file_path <- file.path("yaml-test", "example-model-1.yaml")
  yaml <- src_yaml(ex_file_path)
  supr <- supreme(yaml)
  expect_equal(supr$source_input, c("src_obj", "src_yaml"))
})

test_that("supreme print methods", {
  model1 <- '
  - type: module
    name: displayImages
  '
  s1 <- supreme(src_yaml(text = model1))
  expect_equal(
    paste(utils::capture.output(s1), collapse = " "),
    "A supreme model object 1 entity: displayImages "
  )
  model2 <- '
  - type: module
    name: displayImages

  - type: module
    name: checkInbox
  '
  s2 <- supreme(src_yaml(text = model2))
  expect_equal(
    paste(utils::capture.output(s2), collapse = " "),
    "A supreme model object 2 entities: displayImages, checkInbox "
  )
})

