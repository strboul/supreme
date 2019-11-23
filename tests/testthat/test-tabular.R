
context("test-tabular")

test_that("supreme_to_df", {
  file <- file.path("yaml-test", "example-model.yaml")
  obj <- src_yaml(file)
  expect_equal(supreme_to_df(obj),
               structure(
                 list(
                   name = c(
                     "server",
                     "items_tab_module_server",
                     "customers_tab_module_server",
                     "transactions_tab_module_server"
                   ),
                   input = list(
                     NA_character_,
                     c("items_list", "is_fired"),
                     "customers_list",
                     c("table", "button_clicked")
                   ),
                   output = list(
                     NA_character_,
                     NA_character_,
                     c("paid_customers_table", "free_customers_table"),
                     "transactions_table"
                   ),
                   return = c(NA, NA, NA, "transactions_keys"),
                   calling_modules = structure(
                     list(
                       list(
                         list(items_tab_module_server = "items_tab_ui"),
                         list(customers_tab_module_server = "customers_tab_ui"),
                         list(transactions_tab_module_server = "transactions_tab_ui")
                       ),
                       NA_character_,
                       NA_character_,
                       NA_character_
                     ),
                     class = "AsIs"
                   ),
                   src = c(NA, "inventory", "sales", "sales")
                 ),
                 row.names = c(NA,-4L),
                 class = "data.frame"
               ))
})

test_that("as.data.frame.supreme S3 method", {
  model <- "
  - name: childModuleA
    input: [input.data, reactive]
    calling_modules: grandChildModule1

  - name: childModuleB
    input: selected.model
  "
  obj <- supreme(src_yaml(text = model))
  expect_equal(as.data.frame(obj),
               structure(
                 list(
                   name = c("childModuleA", "childModuleB"),
                   input = structure(list(
                     c("input.data", "reactive"), "selected.model"
                   ), class = "AsIs"),
                   output = structure(c(NA_character_, NA_character_), class = "AsIs"),
                   return = c(NA_character_, NA_character_),
                   calling_modules = structure(c("grandChildModule1",
                                                 NA), class = "AsIs"),
                   src = c(NA_character_, NA_character_)
                 ),
                 row.names = c(NA,-2L),
                 class = "data.frame"
               ))
})

