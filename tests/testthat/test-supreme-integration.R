
context("test-supreme: Integration tests")

## src_file
module_output <- file.path("integration-data", "module-output.Rtest")
multiple_server_definition <- file.path("integration-data", "multiple-server-definition.Rtest")
server_exprs_elems <- file.path("integration-data", "server-exprs-elems.Rtest")
without_any_calling_module <- file.path("integration-data", "without-any-calling-module.Rtest")

## src_yaml
cycle_modules <- file.path("integration-data", "cycle-modules.yaml")

test_that("supreme with src_file", {

  expect_equal(supreme(src_file(module_output)),
               structure(list(
                 data = list(
                   list(
                     name = "linkedScatter",
                     input = c("data",
                               "left", "right"),
                     output = c("plot1", "plot2"),
                     return = "dataWithSelection",
                     src = "module-output.Rtest"
                   ),
                   list(
                     name = "server",
                     output = "summary",
                     calling_modules = list(list(linkedScatter = NULL)),
                     src = "module-output.Rtest"
                   )
                 ),
                 source_input = c("supreme_src_obj", "supreme_src_file")
               ), class = "supreme"))


  expect_error(
    supreme(src_file(multiple_server_definition)),
    regexp = "[supreme] duplicated module names in the source: 'server'",
    fixed = TRUE
  )

  expect_equal(supreme(src_file(server_exprs_elems)),
               structure(list(
                 data = list(
                   list(
                     name = "server",
                     calling_modules = list(
                       list(SomeTabServer = NULL),
                       list(BarPlotPanelServer = NULL),
                       list(CustomerListPanelServer = NULL),
                       list(ObservedPanelServer = NULL),
                       list(ConditionalItemsServer = NULL),
                       list(ConditionalConditionalItems1Server = NULL),
                       list(ConditionalConditionalItems2Server = NULL),
                       list(DetailsButtonServer = NULL)
                     ),
                     src = "server-exprs-elems.Rtest"
                   )
                 ),
                 source_input = c("supreme_src_obj",
                                  "supreme_src_file")
               ), class = "supreme"))


  expect_equal(supreme(src_file(without_any_calling_module)),
               structure(list(
                 data = list(
                   list(
                     name = "main_table_server",
                     input = c("data",
                               "tbl.pageLength", "tbl.selection"),
                     output = "tbl",
                     return = "rv",
                     src = "without-any-calling-module.Rtest"
                   )
                 ),
                 source_input = c("supreme_src_obj",
                                  "supreme_src_file")
               ), class = "supreme"))

})


test_that("supreme with src_yaml", {

  expect_equal(supreme(src_yaml(cycle_modules)),
               structure(list(
                 data = list(
                   list(
                     name = "server",
                     input = c("ax",
                               "by", "cz"),
                     output = c("O1", "O2"),
                     return = "rv",
                     calling_modules = list(list(reusableModule = NULL))
                   ),
                   list(
                     name = "reusableModule",
                     input = c("a", "b"),
                     output = c("OO1", "OO2", "OO3"),
                     return = c("RV1",
                                "RV2")
                   )
                 ),
                 source_input = c("supreme_src_obj", "supreme_src_yaml")
               ), class = "supreme"))

})


test_that("graph supreme with src_file (vdiffr)", {

})


test_that("graph supreme with src_yaml (vdiffr)", {

})


test_that("supreme print methods", {

  sp_yaml <- supreme(src_yaml(example_yaml()))
  sp_file <- supreme(src_file(example_app_path()))

  expect_equal(
    trimws(paste(utils::capture.output(sp_yaml), collapse = " ")),
    "A supreme model object 5 entities: server, customers_tab_module_server, items_tab_module_server, transactions_tab_module_server, ..."
  )

  expect_equal(
    trimws(paste(utils::capture.output(sp_file), collapse = " ")),
    "A supreme model object 5 entities: server, customers_tab_module_server, items_tab_module_server, transactions_tab_module_server, ..."
  )

  model1 <- '
  - name: displayImages
  '
  s1 <- supreme(src_yaml(text = model1))
  expect_equal(
    trimws(paste(utils::capture.output(s1), collapse = " ")),
    "A supreme model object 1 entity: displayImages"
  )

  model2 <- '
  - name: displayImages

  - name: checkInbox
  '
  s2 <- supreme(src_yaml(text = model2))
  expect_equal(
    trimws(paste(utils::capture.output(s2), collapse = " ")),
    "A supreme model object 2 entities: displayImages, checkInbox"
  )
})

