
context("test-tabular")

test_that("supreme_to_df", {

  f <- file.path("yaml-test", "example-model-1.yaml")
  obj <- src_yaml(f)

  expect_equal(supreme_to_df(obj),
               structure(
                 list(
                   type = c(
                     "module",
                     "module",
                     "module",
                     "module",
                     "module",
                     "module",
                     "module"
                   ),
                   name = c(
                     "server",
                     "childModuleA",
                     "childModuleB",
                     "grandChildModule1",
                     "grandChildModule2",
                     "reusableModuleLoading",
                     "orphanModule"
                   ),
                   input = c(
                     NA,
                     "input.data, reactive",
                     "selected.model",
                     "data, trigger.btn",
                     "data, trigger.btn",
                     "data",
                     NA
                   ),
                   output = c(
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_
                   ),
                   calling_modules = c(
                     "childModuleA, childModuleB",
                     "grandChildModule1",
                     NA,
                     "reusableModuleLoading",
                     "reusableModuleLoading",
                     NA,
                     NA
                   ),
                   src = c(
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_,
                     NA_character_
                   )
                 ),
                 row.names = c(NA,-7L),
                 class = "data.frame"
               ))

})

test_that("as.data.frame.supreme S3 method", {

  model <- "
  - type: module
    name: childModuleA
    input: [input.data, reactive]
    calling_modules: grandChildModule1
  - type: module
    name: childModuleB
    input: selected.model
  "

  Object <- supreme(src_yaml(text = model))

  expect_equal(as.data.frame(Object),
               structure(
                 list(
                   type = c("module", "module"),
                   name = c("childModuleA",
                            "childModuleB"),
                   input = c("input.data, reactive", "selected.model"),
                   output = c(NA_character_, NA_character_),
                   calling_modules = c("grandChildModule1",
                                       NA),
                   src = c(NA_character_, NA_character_)
                 ),
                 row.names = c(NA,-2L),
                 class = "data.frame"
               ))

})

