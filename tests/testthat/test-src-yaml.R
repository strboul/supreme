
example_model <- example_yaml()

str_model <- "
  - name: childModuleA
    input: [input.data, reactive]
    calling_modules:
      - grandChildModule1:
        - grandChildModule1UI
  "

test_that("src_yaml", {

  expect_equal(src_yaml(example_model),
               structure(
                 list(
                   list(
                     name = "server",
                     calling_modules = list(
                       list(items_tab_module_server = "ItemsTab"),
                       list(customers_tab_module_server = "CustomersTab"),
                       list(transactions_tab_module_server = "TransactionsTab")
                     ),
                     src = "app.R"
                   ),
                   list(
                     name = "customers_tab_module_server",
                     input = "customers_list",
                     output = c("paid_customers_table",
                                "free_customers_table"),
                     src = "module-customers.R"
                   ),
                   list(
                     name = "items_tab_module_server",
                     input = c("items_list",
                               "is_fired"),
                     calling_modules = list(list(module_modal_dialog = NULL)),
                     src = "module-items.R"
                   ),
                   list(
                     name = "transactions_tab_module_server",
                     input = c("table", "button_clicked"),
                     output = "transactions_table",
                     return = "transactions_keys",
                     src = "module-transactions.R"
                   ),
                   list(name = "module_modal_dialog", input = "text", src = "module-utils.R")
                 ),
                 class = c("supreme_src_obj",
                           "supreme_src_yaml")
               ))

  expect_equal(src_yaml(text = str_model),
               structure(list(
                 list(
                   name = "childModuleA",
                   input = c("input.data",
                             "reactive"),
                   calling_modules = list(list(grandChildModule1 = "grandChildModule1UI"))
                 )
               ), class = c("supreme_src_obj",
                            "supreme_src_yaml")))

})

test_that("src_yaml errors", {

  expect_error(
    src_yaml(file = example_model, text = str_model),
    regexp = "[supreme] Provide a file or text, not both.",
    fixed = TRUE
  )

  expect_error(
    src_yaml(),
    regexp = "[supreme] Provide a file or text.",
    fixed = TRUE
  )

})

test_that("src_yaml (unique src paths)", {

  test_src_unique_file_paths <- "
  - name: server
    src: folder/proj/app.R
  - name: table
    src: folder/proj/sub-module/table.R
  - name: button
    src: folder/proj/sub-module/app.R
  "

  expect_equal(src_yaml(text = test_src_unique_file_paths),
               structure(
                 list(
                   list(name = "server", src = "folder/proj/app.R"),
                   list(name = "table", src = "folder/proj/sub-module/table.R"),
                   list(name = "button", src = "folder/proj/sub-module/app.R")
                 ),
                 class = c("supreme_src_obj",
                           "supreme_src_yaml")
               ))

})


test_that("verify_yaml", {

  ## first, check if example file is ok:
  ex <- yaml::yaml.load_file(example_yaml())
  expect_true(.verify_yaml(ex))

  missing <- "
  - input: [data, trigger.btn]
  "
  missing_yaml <- yaml::yaml.load(missing)
  expect_error(
    .verify_yaml(missing_yaml),
    regexp = "[supreme] 'name' field(s) required for every element",
    fixed = TRUE
  )

  alien <- "
  - name: grandChildModule
    alien_field: 1
  "
  alien_yaml <- yaml::yaml.load(alien)
  expect_error(
    .verify_yaml(alien_yaml),
    regexp = "[supreme] following name(s) not required or optional: 'alien_field'",
    fixed = TRUE
  )

  deep <- "
  - name: too deep
    calling_modules:
      - depth1:
        - depth2:
          - depth3
  "
  deep_yaml <- yaml::yaml.load(deep)
  expect_error(
    .verify_yaml(deep_yaml),
    regexp = "[supreme] model YAML cannot contain too depth lists in 'calling_modules'",
    fixed = TRUE
  )

  cm <- "
  - name: childModuleA
    calling_modules: grandChildModule1
  "
  cm_yaml <- yaml::yaml.load(cm)
  expect_error(
    .verify_yaml(cm_yaml),
    regexp = "[supreme] 'calling_modules' field must have a UI part, a proper name or NULL (~)",
    fixed = TRUE
  )

})

