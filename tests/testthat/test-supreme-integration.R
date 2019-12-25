
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
                     calling_modules = list(list(linkedScatter = "scatters")),
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
                       list(SomeTabServer = "SomeTab"),
                       list(BarPlotPanelServer = "BarPlotPanel"),
                       list(CustomerListPanelServer = "CustomerListPanel"),
                       list(ObservedPanelServer = "ObservedPanel"),
                       list(ConditionalItemsServer = "ConditionalItems"),
                       list(ConditionalConditionalItems1Server = "ConditionalConditionalItems1"),
                       list(ConditionalConditionalItems2Server = "ConditionalConditionalItems2"),
                       list(DetailsButtonServer = "DetailsButton")
                     ),
                     src = "server-exprs-elems.Rtest"
                   )
                 ),
                 source_input = c("supreme_src_obj", "supreme_src_file")
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


test_that("graph supreme with src_file (test nomnoml code with hashing)", {
  {set.seed(2019); graph_module_output <- graph(supreme(src_file(module_output)))}
  expect_identical(
    digest::digest(graph_module_output[["x"]][["code"]]),
    "388797085036f53e161312556152a241"
  )
  {set.seed(2019); graph_server_exprs_elems <- graph(supreme(src_file(server_exprs_elems)))}
  expect_identical(
    digest::digest(graph_server_exprs_elems[["x"]][["code"]]),
    "0a58099ab0f9dc0d6d8de7803693e6a9"
  )
  {set.seed(2019); graph_without_any_calling_module <- graph(supreme(src_file(without_any_calling_module)))}
  expect_identical(
    digest::digest(graph_without_any_calling_module[["x"]][["code"]]),
    "292b57c14f07c439457a35716768be50"
  )
})


test_that("graph supreme with src_yaml (test nomnoml code with hashing)", {
  {set.seed(2019); graph_cycle_modules <- graph(supreme(src_yaml(cycle_modules)))}
  expect_identical(
    digest::digest(graph_cycle_modules[["x"]][["code"]]),
    "89d03e1bae867f8474bc76ebf1fbfe35"
  )
})


test_that("supreme error", {
  expect_error(
    supreme(1),
    "[supreme] the provided input cannot be turned into a supreme object",
    fixed = TRUE
  )
})

