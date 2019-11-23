
context("test-yaml")

test_that("can verify yaml files", {

  ex1 <- yaml::yaml.load_file(file.path("yaml-test", "example-model.yaml"))
  expect_true(.verify_yaml(ex1))

  miss <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-missing-required.yaml"))
  expect_error(
    .verify_yaml(miss),
    regexp = "required fields are missing in '1' element: 'name'"
  )

  foreign <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-foreign.yaml"))
  expect_error(
    .verify_yaml(foreign),
    regexp = "the following names not required or optional: 'foreign_input'"
  )

  sublist <- yaml::yaml.load_file(file.path("yaml-test", "verify-yaml-sublist.yaml"))
  expect_error(
    .verify_yaml(sublist),
    regexp = "model YAML cannot contain too depth lists in 'calling_modules'"
  )

})

test_that("can source yaml files", {

  example_model <- file.path("yaml-test", "example-model.yaml")

  expect_equal(src_yaml(example_model),
               structure(list(
                 list(
                   name = "server",
                   calling_modules = list(
                     list(items_tab_module_server = "items_tab_ui"),
                     list(customers_tab_module_server = "customers_tab_ui"),
                     list(transactions_tab_module_server = "transactions_tab_ui")
                   )
                 ),
                 list(
                   name = "items_tab_module_server",
                   input = c("items_list",
                             "is_fired"),
                   src = "inventory"
                 ),
                 list(
                   name = "customers_tab_module_server",
                   input = "customers_list",
                   output = c("paid_customers_table",
                              "free_customers_table"),
                   src = "sales"
                 ),
                 list(
                   name = "transactions_tab_module_server",
                   input = c("table", "button_clicked"),
                   output = "transactions_table",
                   return = "transactions_keys",
                   src = "sales"
                 )
               ), class = c("src_obj",
                            "src_yaml")))

  str_model <- "
  - name: childModuleA
    input: [input.data, reactive]
    calling_modules:
      - grandChildModule1:
        - grandChildModule1UI
  "

  expect_equal(src_yaml(text = str_model),
               structure(list(
                 list(
                   name = "childModuleA",
                   input = c("input.data",
                             "reactive"),
                   calling_modules = list(list(grandChildModule1 = "grandChildModule1UI"))
                 )
               ), class = c("src_obj",
                            "src_yaml")))


  expect_error(
    src_yaml(file = example_model, text = str_model),
    regexp = "Provide a file or text, not both."
  )

  expect_error(
    src_yaml(),
    regexp = "Provide a file or text."
  )

  test_src_unique_file_paths <- "
  - name: server
    calling_modules: [table, button]
    src: folder/proj/app.R
  - name: table
    src: folder/proj/sub-module/table.R
  - name: button
    src: folder/proj/sub-module/app.R
  "

  expect_equal(src_yaml(text = test_src_unique_file_paths),
               structure(list(
                 list(
                   name = "server",
                   calling_modules = c("table",
                                       "button"),
                   src = "folder/proj/app.R"
                 ),
                 list(name = "table", src = "folder/proj/sub-module/table.R"),
                 list(name = "button", src = "folder/proj/sub-module/app.R")
               ), class = c("src_obj",
                            "src_yaml")))

})

