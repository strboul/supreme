
context("test-example: the examples are the equivalents")

skip("till fixed")

supreme_file <- supreme(src_file(example_app_path()))
supreme_yaml <- supreme(src_yaml(example_yaml()))

#' Custom expectation for the examples
#'
#' Since the order of the list elements can be different, this custom expectation
#' checks the list elements where their names are the same.
#'
#' @references
#' Source: \url{https://testthat.r-lib.org/articles/custom-expectation.html}
#' @noRd
expect_example_equivalent <- function(object, expected) {
  expect_identical(length(object), length(expected))
  for (i in seq_along(object)) {
    obj <- object[[i]]
    obj_name <- obj[["name"]]
    exp <- unlist(lapply(expected, function(e) {
      if (e[["name"]] == obj_name) {
        e
      }
    }), recursive = FALSE)
    label_msg <- function(input, n_i) paste0(input, "[[", n_i, "]]")
    ## expect_equivalent because we don't check attributes (e.g. class)
    expect_equivalent(
      obj, exp,
      label = label_msg(deparse(substitute(object)), i),
      expected.label = label_msg(deparse(substitute(expected)), i)
    )
  }
}


test_that("example supreme$data outputs are equivalent", {
  expect_example_equivalent(supreme_file$data, supreme_yaml$data)
})


test_that("example as.data.frame outputs are equal", {
  expect_equal(as.data.frame(supreme_file), as.data.frame(supreme_yaml))
})


test_that("example supreme graphs are equal (vdiffr)", {

})

