
context("test-yaml")

ex1 <- yaml::yaml.load_file(file.path("yaml-test", "example-model-1.yaml"))

test_that("can verify yaml files", {

  expect_true(verify_yaml(ex1))

  miss <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-missing-required.yaml"))

  expect_error(
    verify_yaml(miss),
    regexp = "required fields are missing in '1' element: 'name'"
  )

  foreign <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-foreign.yaml"))
  expect_error(
    verify_yaml(foreign),
    regexp = "the following names not required or optional: 'foreign_input'"
  )

  sub <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-sublist.yaml"))
  expect_error(
    verify_yaml(sub),
    regexp = "model yaml cannot contain sub-list in '1' element: 'calling_modules'"
  )

})

