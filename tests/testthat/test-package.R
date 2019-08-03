
context("test-package")

test_that("can read package 'supreme.pkg.test'", {
  pkg.name <- "supreme.pkg.test"

  if (!supreme:::is_package_exist(pkg.name)) {
    pkg.path <- file.path("testpackages", pkg.name)
    answer <- menu(
      choices = c("Yes", "No"),
      title = paste(
        "\n",
        paste0(
          "The test package '",
          pkg.name,
          "' not found in the system."),
        "Do you want to install it to run the tests?",
        "",
        paste0(
          "The package placed under the directory: '",
          tools::file_path_as_absolute(pkg.path),
          "'"
        ),
        sep = "\n"
      )
    )
    if (identical(answer, 1L)) {
      devtools::install(pkg.path)
    }
  }

  sourced <- src_pkg(pkg.name)

  expect_equal(sourced,
               structure(list(
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
               ), class = "src_pkg"))

})
