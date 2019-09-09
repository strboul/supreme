
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
                   input = list(
                     NA_character_,
                     c("input.data",
                       "reactive"),
                     "selected.model",
                     c("data", "trigger.btn"),
                     c("data",
                       "trigger.btn"),
                     "data",
                     NA_character_
                   ),
                   output = structure(
                     c(
                       NA_character_,
                       NA_character_,
                       NA_character_,
                       NA_character_,
                       NA_character_,
                       NA_character_,
                       NA_character_
                     ),
                     class = "AsIs"
                   ),
                   calling_modules = structure(
                     list(
                       c("childModuleA", "childModuleB"),
                       "grandChildModule1",
                       NA_character_,
                       "reusableModuleLoading",
                       "reusableModuleLoading",
                       NA_character_,
                       NA_character_
                     ),
                     class = "AsIs"
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
                   input = structure(list(
                     c("input.data", "reactive"), "selected.model"
                   ), class = "AsIs"),
                   output = structure(c(NA_character_,
                                        NA_character_), class = "AsIs"),
                   calling_modules = structure(c("grandChildModule1",
                                                 NA), class = "AsIs"),
                   src = c(NA_character_, NA_character_)
                 ),
                 row.names = c(NA,-2L),
                 class = "data.frame"
               ))
})

