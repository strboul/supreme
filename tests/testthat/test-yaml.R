
context("test-yaml")

test_that("can verify yaml files", {

  ex1 <- yaml::yaml.load_file(file.path("yaml-test", "example-model-1.yaml"))
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

test_that("can source yaml files", {

  example.file <- file.path("yaml-test", "example-model-1.yaml")

  expect_equal(src_yaml(example.file),
               structure(list(
                 list(
                   type = "module",
                   name = "server",
                   calling_modules = c("childModuleA",
                                       "childModuleB")
                 ),
                 list(
                   type = "module",
                   name = "childModuleA",
                   input = c("input.data", "reactive"),
                   calling_modules = "grandChildModule1"
                 ),
                 list(type = "module", name = "childModuleB", input = "selected.model"),
                 list(
                   type = "module",
                   name = "grandChildModule1",
                   input = c("data",
                             "trigger.btn"),
                   calling_modules = "reusableModuleLoading"
                 ),
                 list(
                   type = "module",
                   name = "grandChildModule2",
                   input = c("data",
                             "trigger.btn"),
                   calling_modules = "reusableModuleLoading"
                 ),
                 list(
                   type = "module",
                   name = "reusableModuleLoading",
                   input = "data",
                   calling_modules = NULL
                 ),
                 list(
                   type = "module",
                   name = "orphanModule",
                   input = NULL,
                   calling_modules = NULL
                 )
               ), class = "src_yaml"))

  model <- "
  - type: module
    name: childModuleA
    input: [input.data, reactive]
    calling_modules: grandChildModule1
  - type: module
    name: childModuleB
    input: selected.model
  "

  expect_equal(src_yaml(text = model),
               structure(list(
                 list(
                   type = "module",
                   name = "childModuleA",
                   input = c("input.data",
                             "reactive"),
                   calling_modules = "grandChildModule1"
                 ),
                 list(type = "module",
                      name = "childModuleB", input = "selected.model")
               ), class = "src_yaml"))


  expect_error(
    src_yaml(file = example.file, text = model),
    regexp = "Provide a file or text, not both."
  )

  expect_error(
    src_yaml(),
    regexp = "Provide a file or text."
  )

})

