
context("test-env")

test_that("can read environment objects", {

  env <- example_environment()

  expect_equal(src_env(env),
               structure(
                 list(
                   list(
                     name = "module1_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     name = "module2_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     name = "module3_server",
                     calling_modules = NULL,
                     src = "expression"
                   ),
                   list(
                     name = "moduleA_server",
                     calling_modules = c("module1_server", "module2_server",
                                         "module3_server"),
                     src = "expression"
                   ),
                   list(
                     name = "server",
                     calling_modules = "moduleA_server",
                     src = "expression"
                   )
                 ),
                 class = c("supreme_src_obj", "supreme_src_env")
               ))

})

