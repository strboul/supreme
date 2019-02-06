
context("test-parse")

test_that("get_parsed_data", {
  par <- get_parsed_data("tests/testthat/test-app.R")

  expect_equal(head(par),
               structure(
                 list(
                   id = 14:19,
                   parent = c(16L, 22L, 22L, 19L, 22L,
                              22L),
                   token = c(
                     "SYMBOL_FUNCTION_CALL",
                     "'('",
                     "expr",
                     "STR_CONST",
                     "')'",
                     "expr"
                   ),
                   terminal = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                   text = c("source", "(", "", "\"test-long-module.R\"", ")",
                            "")
                 ),
                 row.names = c(NA, 6L),
                 class = "data.frame"
               ))

})
