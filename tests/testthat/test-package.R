
context("test-package")

test_that("can read package 'supreme.pkg.test'", {
  pkg <- example_package()

  sourced <- src_pkg(pkg)

  expect_equal(sourced,
               structure(
                 list(
                   list(
                     type = "module",
                     name = ".dotModule1_server",
                     calling_modules = NULL,
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = ".dotModule2_server",
                     calling_modules = NULL,
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = "a_module_for_table_server",
                     calling_modules = NULL,
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = "childModule1_server",
                     calling_modules = NULL,
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = "moduleA_server",
                     calling_modules = NULL,
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = "moduleB_server",
                     calling_modules = "childModule1_server",
                     src = "package:supreme.pkg.test"
                   ),
                   list(
                     type = "module",
                     name = "server",
                     calling_modules = c("moduleA_server",
                                         "moduleB_server"),
                     src = "package:supreme.pkg.test"
                   )
                 ),
                 class = c("src_obj",
                           "src_pkg")
               ))

})

