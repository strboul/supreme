
context("test-supreme")

test_that("supreme with source yaml", {
  yaml_path <- "example-model.yaml"
  yaml <- src_yaml(yaml_path)
  supr <- supreme(yaml)
  expect_equal(supr$source_input, c("supreme_src_obj", "supreme_src_yaml"))
})

test_that("supreme print methods", {
  model1 <- '
  - name: displayImages
  '
  s1 <- supreme(src_yaml(text = model1))
  expect_equal(
    paste(utils::capture.output(s1), collapse = " "),
    "A supreme model object 1 entity: displayImages "
  )
  model2 <- '
  - name: displayImages

  - name: checkInbox
  '
  s2 <- supreme(src_yaml(text = model2))
  expect_equal(
    paste(utils::capture.output(s2), collapse = " "),
    "A supreme model object 2 entities: displayImages, checkInbox "
  )
})

