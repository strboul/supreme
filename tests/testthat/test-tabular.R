
test_that("as.data.frame with src_yaml", {
  expect_equal(as.data.frame(supreme(src_yaml(example_yaml()))),
               structure(
                 list(
                   name = c(
                     "server",
                     "customers_tab_module_server",
                     "items_tab_module_server",
                     "transactions_tab_module_server",
                     "module_modal_dialog"
                   ),
                   input = list(
                     NA_character_,
                     "customers_list",
                     c("items_list", "is_fired"),
                     c("table", "button_clicked"),
                     "text"
                   ),
                   output = list(
                     NA_character_,
                     c("paid_customers_table",
                       "free_customers_table"),
                     NA_character_,
                     "transactions_table",
                     NA_character_
                   ),
                   return = structure(c(NA, NA, NA, "transactions_keys",
                                        NA), class = "AsIs"),
                   calling_modules = structure(
                     list(
                       list(
                         list(items_tab_module_server = "ItemsTab"),
                         list(customers_tab_module_server = "CustomersTab"),
                         list(transactions_tab_module_server = "TransactionsTab")
                       ),
                       NA_character_,
                       list(list(module_modal_dialog = NULL)),
                       NA_character_,
                       NA_character_
                     ),
                     class = "AsIs"
                   ),
                   src = c(
                     "app.R",
                     "module-customers.R",
                     "module-items.R",
                     "module-transactions.R",
                     "module-utils.R"
                   )
                 ),
                 row.names = c(NA,-5L),
                 class = "data.frame"
               ))

  model <- "
  - name: childModuleA
    input: [input.data, reactive]
    calling_modules:
      - grandChildModule1: ~

  - name: grandChildModule1
    input: selected.model
  "
  obj <- supreme(src_yaml(text = model))
  expect_equal(as.data.frame(obj),
               structure(
                 list(
                   name = c("childModuleA", "grandChildModule1"),
                   input = structure(list(
                     c("input.data", "reactive"), "selected.model"
                   ), class = "AsIs"),
                   output = structure(c(NA_character_, NA_character_), class = "AsIs"),
                   return = structure(c(NA_character_, NA_character_), class = "AsIs"),
                   calling_modules = structure(list(list(
                     list(grandChildModule1 = NULL)
                   ), NA_character_), class = "AsIs"),
                   src = c(NA_character_, NA_character_)
                 ),
                 row.names = c(NA,-2L),
                 class = "data.frame"
               ))

})


test_that("as.data.frame with src_file", {
  expect_equal(as.data.frame(supreme(src_file(example_app_path()))),
               structure(
                 list(
                   name = c(
                     "server",
                     "customers_tab_module_server",
                     "items_tab_module_server",
                     "transactions_tab_module_server",
                     "module_modal_dialog"
                   ),
                   input = list(
                     NA_character_,
                     "customers_list",
                     c("items_list", "is_fired"),
                     c("table", "button_clicked"),
                     "text"
                   ),
                   output = list(
                     NA_character_,
                     c("paid_customers_table",
                       "free_customers_table"),
                     NA_character_,
                     "transactions_table",
                     NA_character_
                   ),
                   return = structure(c(NA, NA, NA, "transactions_keys",
                                        NA), class = "AsIs"),
                   calling_modules = structure(
                     list(
                       list(
                         list(items_tab_module_server = "ItemsTab"),
                         list(customers_tab_module_server = "CustomersTab"),
                         list(transactions_tab_module_server = "TransactionsTab")
                       ),
                       NA_character_,
                       list(list(module_modal_dialog = NULL)),
                       NA_character_,
                       NA_character_
                     ),
                     class = "AsIs"
                   ),
                   src = c(
                     "app.R",
                     "module-customers.R",
                     "module-items.R",
                     "module-transactions.R",
                     "module-utils.R"
                   )
                 ),
                 row.names = c(NA,-5L),
                 class = "data.frame"
               ))
})

