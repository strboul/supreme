
test_that("test src_yaml print", {
  out <- trimws(utils::capture.output(src_yaml(example_yaml())))
  expect_equal(out, "Model yaml object")
})


test_that("test src_file print", {
  out <- trimws(utils::capture.output(src_file(example_app_path())))
  expect_equal(out, "Model file object")
})

