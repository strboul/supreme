
context("test-env")

test_that("can read environment objects", {

  env <- example_environment()

  expect_equal(src_env(env),
               structure(
                 list(
                   list(
                     type = "module",
                     name = "module1_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     type = "module",
                     name = "module2_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     type = "module",
                     name = "module3_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     type = "module",
                     name = "moduleA_server",
                     calling_modules = c("module1_server", "module2_server",
                                         "module3_server"),
                     src = "expression"
                   ),
                   list(
                     type = "module",
                     name = "server",
                     calling_modules = "moduleA_server",
                     src = "expression"
                   )
                 ),
                 class = c("src_obj", "src_env")
               ))

})

