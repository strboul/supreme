
context("test-tabular")

test_that("supreme_to_df", {
  file <- file.path("yaml-test", "example-model.yaml")
  obj <- src_yaml(file)
  ## look the first two rows as it's too long..
  expect_equal(head(supreme_to_df(obj), 2L),
               structure(
                 list(
                   type = c("module", "module"),
                   name = c("server",
                            "childModuleA"),
                   input = list(NA_character_, c("input.data",
                                                 "reactive")),
                   output = structure(c(NA_character_, NA_character_), class = "AsIs"),
                   return = c(NA_character_, NA_character_),
                   calling_modules = structure(list(
                     c("childModuleA", "childModuleB"), "grandChildModule1"
                   ), class = "AsIs"),
                   src = c(NA_character_,
                           NA_character_)
                 ),
                 row.names = 1:2,
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
                   return = c(NA_character_, NA_character_),
                   calling_modules = structure(c("grandChildModule1", NA), class = "AsIs"),
                   src = c(NA_character_, NA_character_)
                 ),
                 row.names = c(NA,-2L),
                 class = "data.frame"
               ))
})

